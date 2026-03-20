;;; ================================================================
;;; GeoOffset.lsp v3.4
;;; Расчёт и оформление отклонений фактических точек
;;; от проектных линий и контуров
;;; AutoCAD 2013-2026 / BricsCAD Pro / Civil 3D
;;;
;;; v3.4 — FIX: атрибут VALUE всегда горизонтальный
;;;         (Rotation = 0 независимо от угла стрелки)
;;;         FIX: inside/outside для ЛЮБЫХ замкнутых контуров
;;;         (определение направления обхода CW/CCW через
;;;          знаковую площадь — ранее работало только для круга)
;;;         Отдельная логика для CIRCLE: направление по
;;;          радиальному вектору (центр↔точка), inside/outside
;;;          по сравнению distance с radius
;;; v3.0 — DCL: image_button + fill_image для цветов Хорошо/Плохо
;;;         FIX: допуск сравнивается ДО умножения на k
;;;         Масштаб блока: DIMSCALE или вручную
;;;         Отражение стрелок через динамический параметр блока
;;; ================================================================
(vl-load-com)

;;; ================================================================
;;; ГЛОБАЛЬНЫЕ ПЕРЕМЕННЫЕ
;;; ================================================================
(if (null *geo:side-closed*)   (setq *geo:side-closed*   "to_point"))
(if (null *geo:side-open*)     (setq *geo:side-open*     "to_point"))
(if (null *geo:coeff*)         (setq *geo:coeff*         1000.0))
(if (null *geo:precision*)    (setq *geo:precision*     0))
(if (null *geo:prefix*)        (setq *geo:prefix*        "+"))
(if (null *geo:layer*)         (setq *geo:layer*         "Geo_deviation"))
(if (null *geo:txtstyle*)      (setq *geo:txtstyle*      "Standard"))
(if (null *geo:txtht*)         (setq *geo:txtht*         1.8))
(setq *geo:tol-plan* 0.01)  ;; v3.4: всегда 0.01 при загрузке
(if (null *geo:tol-plan-on*)   (setq *geo:tol-plan-on*   "1"))

(if (null *geo:elev-mode*)     (setq *geo:elev-mode*     "manual"))
(if (null *geo:elev-val*)     (setq *geo:elev-val*      0.0))

(if (null *geo:color*)         (setq *geo:color*         80))
(if (null *geo:color-ok*)      (setq *geo:color-ok*      3))   ; зелёный = Хорошо
(if (null *geo:color-fail*)    (setq *geo:color-fail*    1))   ; красный = Плохо

(if (null *geo:proj-filter*)   (setq *geo:proj-filter*   nil))
(if (null *geo:fact-filter*)   (setq *geo:fact-filter*   nil))
(if (null *geo:proj-info*)     (setq *geo:proj-info*     "Не задан"))
(if (null *geo:fact-info*)     (setq *geo:fact-info*     "Не задан"))
(setq *geo:styles-list* nil)
(setq *geo:layers-list* nil)
(setq *geo:layers-plus* nil)

;;; ================================================================
;;; СОЗДАНИЕ БЛОКА Geo_Arrow
;;; ================================================================
(defun geo:create-arrow-block ( / as th )
  (if (not (tblsearch "BLOCK" "Geo_Arrow"))
    (progn
      (setq as 3.0 th 1.8)
      (entmake (list '(0 . "BLOCK") '(2 . "Geo_Arrow") '(70 . 2) '(10 0.0 0.0 0.0)))
      (entmake (list '(0 . "LINE") '(8 . "0") '(62 . 256)
                     (cons 10 (list 0.0 0.0 0.0))
                     (cons 11 (list 0.0 as 0.0))))
      (entmake (list '(0 . "SOLID") '(8 . "0") '(62 . 256)
                     (cons 10 (list (- (* as 0.25)) (* as 0.45) 0.0))
                     (cons 11 (list (* as 0.25) (* as 0.45) 0.0))
                     (cons 12 (list 0.0 0.0 0.0))
                     (cons 13 (list 0.0 0.0 0.0))))
      ;; ATTDEF VALUE — скрытый
      (entmake (list '(0 . "ATTDEF") '(100 . "AcDbEntity") '(8 . "0") '(62 . 256)
                     '(100 . "AcDbText")
                     (cons 10 (list (* as 1.5) 0.0 0.0))
                     (cons 40 th) '(1 . "0.0")
                     '(100 . "AcDbAttributeDefinition")
                     '(3 . "Offset Value") '(2 . "VALUE")
                     '(70 . 1) '(280 . 0)))
      ;; ATTDEF DISPLAY — видимый
      (entmake (list '(0 . "ATTDEF") '(100 . "AcDbEntity") '(8 . "0") '(62 . 256)
                     '(100 . "AcDbText")
                     (cons 10 (list (* as 1.5) (* as 0.5) 0.0))
                     (cons 11 (list (* as 1.5) (* as 0.5) 0.0))
                     (cons 40 th) '(1 . "0.0")
                     '(100 . "AcDbAttributeDefinition")
                     '(3 . "Display Text") '(2 . "DISPLAY")
                     '(70 . 0) '(72 . 0) '(74 . 2) '(280 . 0)))
      (entmake '((0 . "ENDBLK")))
      (princ "\n[GeoOffset] Блок Geo_Arrow создан.")
    )
  )
)

;;; ================================================================
;;; УТИЛИТЫ
;;; ================================================================
(defun geo:ensure-layer ( layName layColor / )
  (if (not (tblsearch "LAYER" layName))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord")
                   (cons 2 layName) '(70 . 0) (cons 62 (fix layColor)) '(6 . "Continuous")))))

(defun geo:get-text-styles ( / s r )
  (setq r '() s (tblnext "STYLE" T))
  (while s (setq r (append r (list (cdr (assoc 2 s)))) s (tblnext "STYLE"))) r)

(defun geo:get-layers ( / l r )
  (setq r '() l (tblnext "LAYER" T))
  (while l (setq r (append r (list (cdr (assoc 2 l)))) l (tblnext "LAYER"))) r)

(defun geo:get-obj-coords ( ent ) (cdr (assoc 10 (entget ent))))

;;; Безопасные обёртки для rtos/itoa (защита от nil > numberp)
(defun geo:safe-rtos ( val mode prec / )
  (if (numberp val) (rtos val mode prec) "0"))
(defun geo:safe-itoa ( val / )
  (if (and val (numberp val)) (itoa (fix val)) "0"))


(defun geo:flatten ( pt ) (list (car pt) (cadr pt) 0.0))

(defun geo:safe-closest-pt ( ent pt / r )
  (setq r (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list ent pt)))
  (if (vl-catch-all-error-p r) nil r))

(defun geo:safe-param ( ent pt / r )
  (setq r (vl-catch-all-apply 'vlax-curve-getParamAtPoint (list ent pt)))
  (if (vl-catch-all-error-p r) nil r))

(defun geo:safe-deriv1 ( ent param / r )
  (setq r (vl-catch-all-apply 'vlax-curve-getFirstDeriv (list ent param)))
  (if (vl-catch-all-error-p r) nil r))

(defun geo:find-nearest ( pt entsLst / bestE bestF bestD e f d )
  (setq bestD 1e308)
  (foreach e entsLst
    (setq f (geo:safe-closest-pt e pt))
    (if f (progn (setq d (distance (geo:flatten pt) (geo:flatten f)))
      (if (< d bestD) (setq bestD d bestE e bestF f)))))
  (if bestE (list bestE bestF bestD) nil))

(defun geo:get-tangent ( ent ptFoot / param tang dx dy len )
  (setq param (geo:safe-param ent ptFoot))
  (if param (setq tang (geo:safe-deriv1 ent param)))
  (if (null tang)
    (setq tang (list (- (car (vlax-curve-getEndPoint ent)) (car (vlax-curve-getStartPoint ent)))
                     (- (cadr (vlax-curve-getEndPoint ent)) (cadr (vlax-curve-getStartPoint ent))) 0.0)))
  (setq dx (car tang) dy (cadr tang) len (sqrt (+ (* dx dx) (* dy dy))))
  (if (> len 1e-10) (list (/ dx len) (/ dy len)) (list 1.0 0.0)))

(defun geo:cross-z ( ent ptFoot ptReal / tang dx dy cx cy )
  (setq tang (geo:get-tangent ent ptFoot) dx (car tang) dy (cadr tang)
        cx (- (car (geo:flatten ptReal)) (car (geo:flatten ptFoot)))
        cy (- (cadr (geo:flatten ptReal)) (cadr (geo:flatten ptFoot))))
  (- (* dx cy) (* dy cx)))

(defun geo:closed-p ( ent / ed typ )
  (setq ed (entget ent) typ (cdr (assoc 0 ed)))
  (cond
    ((= typ "CIRCLE") T)
    ((member typ '("LWPOLYLINE" "SPLINE" "POLYLINE"))
     (= 1 (logand 1 (cdr (assoc 70 ed)))))
    (T nil)))

;;; ================================================================
;;; ГЕОМЕТРИЯ КРУГА
;;; ================================================================
;;; Для круга не нужна касательная и cross-z.
;;; Внутри/снаружи определяется сравнением distance(точка, центр) с радиусом.
;;; Направление стрелки — радиальный вектор от центра через ptFoot.

;;; Получить центр круга (2D)
(defun geo:circle-center ( ent / ed )
  (setq ed (entget ent))
  (geo:flatten (cdr (assoc 10 ed))))

;;; Получить радиус круга
(defun geo:circle-radius ( ent / ed )
  (setq ed (entget ent))
  (cdr (assoc 40 ed)))

;;; Проверка: это круг?
(defun geo:circle-p ( ent )
  (= (cdr (assoc 0 (entget ent))) "CIRCLE"))

;;; Точка внутри круга? T = внутри или на круге, nil = снаружи
(defun geo:point-inside-circle ( ent pt / cen rad )
  (setq cen (geo:circle-center ent)
        rad (geo:circle-radius ent))
  (<= (distance (geo:flatten pt) cen) rad))

;;; Радиальный угол стрелки для круга
;;; inside:   стрелка от ptFoot к центру (внутрь)
;;; outside:  стрелка от ptFoot от центра (наружу)
;;; to_point: стрелка от ptFoot к ptReal
;;; Возвращает угол в радианах
(defun geo:circle-arrow-angle ( ent ptFoot ptReal mode
                                / cen dx dy len ang )
  (setq cen (geo:circle-center ent))
  (cond
    ;; Внутрь: вектор от центра к ptFoot (от центра наружу)
    ;; [v3.4] Инвертировано относительно остальных замкнутых фигур
    ((= mode "inside")
     (setq dx (- (car ptFoot) (car cen))
           dy (- (cadr ptFoot) (cadr cen))))
    ;; Наружу: вектор от ptFoot к центру (от контура внутрь)
    ;; [v3.4] Инвертировано относительно остальных замкнутых фигур
    ((= mode "outside")
     (setq dx (- (car cen) (car ptFoot))
           dy (- (cadr cen) (cadr ptFoot))))
    ;; К точке: вектор от ptFoot к ptReal
    (T
     (setq dx (- (car ptReal) (car ptFoot))
           dy (- (cadr ptReal) (cadr ptFoot)))))
  ;; Нормализация (защита от нулевого вектора)
  (setq len (sqrt (+ (* dx dx) (* dy dy))))
  (if (< len 1e-10)
    0.0
    (atan dy dx)))

;;; ================================================================
;;; ОПРЕДЕЛЕНИЕ НАПРАВЛЕНИЯ ОБХОДА (CW/CCW)
;;; ================================================================
;;; Знаковая площадь (Shoelace formula):
;;;   > 0 — обход CCW (против часовой)
;;;   < 0 — обход CW  (по часовой)
;;; Круг всегда CCW в AutoCAD.
;;; Возвращает T если CCW, nil если CW.
(defun geo:is-ccw ( ent / ed typ pts area i p1 p2
                          nSamp t0 tEnd dt par cp )
  (setq ed (entget ent) typ (cdr (assoc 0 ed)))
  (cond
    ;; Круг всегда CCW
    ((= typ "CIRCLE") T)
    ;; LWPOLYLINE — берём вершины напрямую из DXF
    ((= typ "LWPOLYLINE")
     (setq pts '())
     (foreach pair ed
       (if (= (car pair) 10)
         (setq pts (append pts (list (cdr pair))))))
     (setq area 0.0 i 0)
     (repeat (length pts)
       (setq p1 (nth i pts)
             p2 (nth (rem (1+ i) (length pts)) pts))
       (setq area (+ area
                    (- (* (car p1) (cadr p2))
                       (* (car p2) (cadr p1)))))
       (setq i (1+ i)))
     (> area 0.0))
    ;; Для остальных типов (SPLINE, POLYLINE, ELLIPSE)
    ;; семплируем точки по параметру кривой
    (T
     (setq nSamp 64
           t0   (vlax-curve-getStartParam ent)
           tEnd (vlax-curve-getEndParam ent))
     (if (or (null t0) (null tEnd)) T  ; фоллбэк — CCW
       (progn
         (setq dt (/ (- tEnd t0) (float nSamp))
               pts '() par t0)
         (repeat nSamp
           (setq cp (vlax-curve-getPointAtParam ent par))
           (if cp (setq pts (append pts (list cp))))
           (setq par (+ par dt)))
         (if (< (length pts) 3) T  ; недостаточно точек — CCW
           (progn
             (setq area 0.0 i 0)
             (repeat (length pts)
               (setq p1 (nth i pts)
                     p2 (nth (rem (1+ i) (length pts)) pts))
               (setq area (+ area
                            (- (* (car p1) (cadr p2))
                               (* (car p2) (cadr p1)))))
               (setq i (1+ i)))
             (> area 0.0))))))))

;;; ================================================================
;;; ЗНАК ОТКЛОНЕНИЯ
;;; ================================================================
;;; Знак отклонения
;;; cross > 0 = факт. точка СЛЕВА от направления кривой
;;; cross < 0 = факт. точка СПРАВА от направления кривой
;;; "left"  = стрелки СЛЕВА  > знак "+" если точка слева (cross>0), иначе "-"
;;; "right" = стрелки СПРАВА > знак "+" если точка справа (cross<0), иначе "-"
(defun geo:calc-sign ( ent ptFoot ptReal / cross mode )
  (setq mode (if (geo:closed-p ent) *geo:side-closed* *geo:side-open*))
  (cond
    ;; inside/outside/left/right: знак всегда "+"
    ;; направление задаётся нормалью, Flip корректирует визуал
    ((= mode "inside")   "+")
    ((= mode "outside")  "+")
    ((= mode "left")     "+")
    ((= mode "right")    "+")
    ;; К точке / по умолчанию
    (T
     (setq cross (geo:cross-z ent ptFoot ptReal))
     (if (> cross 0) "+" "-"))))


;;; ================================================================
;;; НУЖНО ЛИ ОТРАЖЕНИЕ (динамический параметр "Отражение")
;;; ================================================================
;;; Для открытых контуров:
;;;   mode = "left"  > стрелки СЛЕВА от направления кривой.
;;;   mode = "right" > стрелки СПРАВА.
;;; Если фактическая точка находится на противоположной стороне
;;; (по знаку cross-z), включаем отражение (Flip).
;;; [v3.4] Для inside/outside учитываем CW/CCW обход контура.
;;; Логика Flip: стрелка уже направлена нормалью внутрь/наружу.
;;; Если фактическая точка на противоположной стороне — Flip.
(defun geo:need-mirror ( ent ptFoot ptReal / cross mode ccw isInside )
  (setq mode (if (geo:closed-p ent) *geo:side-closed* *geo:side-open*))
  (cond
    ;; === КРУГ ===
    ;; Для круга inside/outside определяется по расстоянию до центра.
    ;; Стрелка уже направлена нужной нормалью (geo:circle-arrow-angle).
    ;; Flip нужен если точка на противоположной стороне.
    ((and (geo:circle-p ent) (member mode '("inside" "outside")))
     (setq isInside (geo:point-inside-circle ent ptReal))
     (cond
       ;; [v3.4] Инвертировано относительно остальных замкнутых фигур
       ;; inside: стрелка наружу, точка внутри > Flip
       ((and (= mode "inside")  isInside) T)
       ;; outside: стрелка внутрь, точка снаружи > Flip
       ((and (= mode "outside") (not isInside)) T)
       (T nil)))
    ;; === РАЗОМКНУТЫЙ КОНТУР ===
    ;; Стрелки слева, а точка справа > Flip
    ((and (= mode "left")  (progn (setq cross (geo:cross-z ent ptFoot ptReal)) (< cross 0))) T)
    ;; Стрелки справа, а точка слева > Flip
    ((and (= mode "right") (progn (if (null cross) (setq cross (geo:cross-z ent ptFoot ptReal))) (> cross 0))) T)
    ;; === ЗАМКНУТЫЙ КОНТУР (не круг) [v3.4] ===
    ;; inside/outside с учётом CW/CCW
    ((= mode "inside")
     (setq cross (geo:cross-z ent ptFoot ptReal)
           ccw (geo:is-ccw ent))
     (if ccw
       (> cross 0)    ; CCW: точка снаружи (cross>0) > Flip
       (< cross 0)))  ; CW:  точка снаружи (cross<0) > Flip
    ((= mode "outside")
     (setq cross (geo:cross-z ent ptFoot ptReal)
           ccw (geo:is-ccw ent))
     (if ccw
       (< cross 0)    ; CCW: точка внутри (cross<0) > Flip
       (> cross 0)))  ; CW:  точка внутри (cross>0) > Flip
    (T nil)))

;;; ================================================================
;;; LM:setdynpropvalue — установка значения динамического параметра
;;; (по мотивам Lee Mac). blk = VLA-объект, prp = имя параметра, val = значение
;;; ================================================================
(defun LM:setdynpropvalue ( blk prp val )
  (setq prp (strcase prp))
  (vl-some
    '(lambda ( x )
       (if (= prp (strcase (vla-get-propertyname x)))
         (progn
           (vla-put-value x
             (vlax-make-variant val
               (vlax-variant-type (vla-get-value x))))
           (cond (val) (T)))))
    (vlax-invoke blk 'getdynamicblockproperties)))

;;; ================================================================
;;; Применить отражение к динамическому блоку Geo_Arrow (если нужно)
;;; Использует LM:setdynpropvalue для установки Flip = 1
;;; Имя параметра в блоке: "Flip"
;;; ================================================================
(defun geo:mirror-block-if-needed ( blkObj ent ptFoot ptReal )
  (if (and blkObj
           (geo:need-mirror ent ptFoot ptReal)
           (= :vlax-true (vla-get-IsDynamicBlock blkObj)))
    (progn
      (LM:setdynpropvalue blkObj "Flip" 1)
      (vla-Update blkObj))))

;;; ================================================================
;;; УГОЛ СТРЕЛКИ
;;; ================================================================
;;; Угол стрелки — определяет направление нормали
;;; "left"  > нормаль = (-dy, dx)  ВСЕГДА (слева от направления кривой)
;;; "right" > нормаль = (dy, -dx)  ВСЕГДА (справа от направления кривой)
;;; "inside"/"outside" > нормаль с учётом CW/CCW обхода контура
;;; [v3.4] Для inside/outside определяем направление обхода.
;;;   CCW: внутрь = (dy,-dx), наружу = (-dy,dx)
;;;   CW:  внутрь = (-dy,dx), наружу = (dy,-dx)  (инверсия)
(defun geo:calc-angle ( ent ptFoot ptReal / tang dx dy cross mode ccw nx ny ang )
  (setq mode (if (geo:closed-p ent) *geo:side-closed* *geo:side-open*))
  ;; === КРУГ: отдельная логика ===
  ;; Для круга угол вычисляется через радиальный вектор (центр↔ptFoot),
  ;; а не через касательную + cross-z. Это гарантирует
  ;; корректное направление внутрь/наружу независимо от обхода.
  (if (geo:circle-p ent)
    (- (geo:circle-arrow-angle ent ptFoot ptReal mode) (/ pi 2))
    ;; === ОСТАЛЬНЫЕ ТИПЫ: касательная + нормаль ===
    (progn
      (setq tang (geo:get-tangent ent ptFoot) dx (car tang) dy (cadr tang))
      (cond
        ;; === РАЗОМКНУТЫЙ КОНТУР ===
        ;; Слева от направления кривой: нормаль = (-dy, dx) ВСЕГДА
        ((= mode "left")
         (setq nx (- dy) ny dx))
        ;; Справа от направления кривой: нормаль = (dy, -dx) ВСЕГДА
        ((= mode "right")
         (setq nx dy ny (- dx)))
        ;; === ЗАМКНУТЫЙ КОНТУР (не круг) ===
        ;; [v3.4] Определяем направление обхода для корректного inside/outside
        ((= mode "inside")
         (setq ccw (geo:is-ccw ent))
         (if ccw
           (setq nx dy ny (- dx))       ; CCW: вправо = внутрь
           (setq nx (- dy) ny dx)))     ; CW:  влево = внутрь
        ((= mode "outside")
         (setq ccw (geo:is-ccw ent))
         (if ccw
           (setq nx (- dy) ny dx)       ; CCW: влево = наружу
           (setq nx dy ny (- dx))))     ; CW:  вправо = наружу
        ;; === К ТОЧКЕ / ПО УМОЛЧАНИЮ ===
        ;; Нормаль в сторону фактической точки (по cross-z)
        (T
         (setq cross (geo:cross-z ent ptFoot ptReal))
         (if (> cross 0) (setq nx (- dy) ny dx) (setq nx dy ny (- dx)))))
      (setq ang (- (atan ny nx) (/ pi 2)))
      ang)))

;;; ================================================================
;;; МАСШТАБ БЛОКА
;;; Используем DIMSCALE (стандартный масштабный коэффициент размерных стилей).
;;; ================================================================
(defun geo:get-block-scale ( / ds )
  (setq ds (getvar "DIMSCALE"))
  (if (or (null ds) (<= ds 0)) 1.0 ds))

;;; ================================================================
;;; ENTSEL > ФИЛЬТР
;;; ================================================================
(defun geo:build-filter-from-sample ( promptMsg / sel ent ed etyp elay ecol filter info )
  (setq sel (entsel promptMsg))
  (if (null sel)
    (progn (princ "\nНичего не выбрано.") nil)
    (progn
      (setq ent  (car sel)
            ed   (entget ent)
            etyp (cdr (assoc 0 ed))
            elay (cdr (assoc 8 ed))
            ecol (cdr (assoc 62 ed)))
      (setq filter (list (cons 0 etyp) (cons 8 elay)))
      (if (and ecol (/= ecol 0))
        (setq filter (append filter (list (cons 62 ecol)))))
      (setq info (strcat "Тип: " etyp "  Слой: " elay
                         (if (and ecol (/= ecol 0))
                           (strcat "  Цвет: " (itoa ecol))
                           "  Цвет: ByLayer")))
      (princ (strcat "\nФильтр: " info))
      (list filter info)
    )
  )
)

;;; ================================================================
;;; SS > LIST
;;; ================================================================
(defun geo:ss->list ( ss / i lst )
  (if ss (progn (setq lst '() i 0)
    (repeat (sslength ss) (setq lst (append lst (list (ssname ss i))) i (1+ i))) lst) nil))

;;; ================================================================
;;; ЗАЛИВКА ЦВЕТОВОГО ИНДИКАТОРА (image_button)
;;; ================================================================
(defun geo:fill-color-tile ( tileKey aciColor / w h )
  (setq w (dimx_tile tileKey) h (dimy_tile tileKey))
  (start_image tileKey)
  (fill_image 0 0 w h (fix aciColor))
  (end_image)
)

;;; ================================================================
;;; ВСТАВКА БЛОКА
;;; [v3.0] Масштаб = DIMSCALE или ручной ввод
;;; ================================================================
(defun geo:insert-arrow ( ent ptFoot ptReal distStr rawValue layName
                          / mSpace blkObj rotAngle scale atts att tag
                            cosA sinA offX offY txtPt )
  (setq scale (geo:get-block-scale))
  (setq rotAngle (geo:calc-angle ent ptFoot ptReal))
  (setq mSpace (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (setq blkObj (vla-InsertBlock mSpace (vlax-3d-point (geo:flatten ptFoot))
                 "Geo_Arrow" scale scale scale rotAngle))
  (vla-put-Layer blkObj layName)
  ;; Вычисляем WCS-позицию для текста DISPLAY
  ;; Используем rotAngle стрелки: текст смещён по направлению стрелки на 4?scale
  ;; Ось Y блока при rotAngle: направление (cos(rot+?/2), sin(rot+?/2)) = (-sin(rot), cos(rot))
  (setq cosA (cos rotAngle) sinA (sin rotAngle))
  ;; Offset в определении блока DISPLAY: ~(4.5, 1.5) > в WCS при повороте:
  ;; xWCS = 4.5*cos - 1.5*sin, yWCS = 4.5*sin + 1.5*cos (всё ? scale)
  (setq offX (* scale (- (* 4.5 cosA) (* 1.5 sinA)))
        offY (* scale (+ (* 4.5 sinA) (* 1.5 cosA))))
  (setq txtPt (vlax-3d-point
                (list (+ (car ptFoot) offX) (+ (cadr ptFoot) offY) 0.0)))
  (if (= (vla-get-HasAttributes blkObj) :vlax-true)
    (progn (setq atts (vlax-invoke blkObj 'GetAttributes))
      (foreach att atts
        (setq tag (strcase (vla-get-TagString att)))
        (cond
          ((= tag "VALUE")
           (vla-put-TextString att rawValue)
           (vla-put-Rotation att 0.0))  ;; v3.4: VALUE всегда горизонтальный
          ((= tag "DISPLAY")
           (vla-put-TextString att distStr)
           (vla-put-Rotation att 0.0)
           (vla-put-InsertionPoint att txtPt)
           (vla-put-TextAlignmentPoint att txtPt)
           (if (tblsearch "STYLE" *geo:txtstyle*)
             (vla-put-StyleName att *geo:txtstyle*)))))))
  ;; Применяем динамическое отражение, если нужно
  (geo:mirror-block-if-needed blkObj ent ptFoot ptReal)
  blkObj)

;;; ================================================================
;;; DCL v3.0 — image_button с заливкой цвета, Хорошо/Плохо
;;; ================================================================
(defun geo:write-dcl ( / fn f )
  (setq fn (strcat (getvar "TEMPPREFIX") "GeoOffset.dcl"))
  (setq f (open fn "w"))

  (write-line "GeoOffset : dialog {" f)
  (write-line "  label = \"GeoOffset v3.4\";" f)

  ;; ========== ВЕРХНЯЯ ЧАСТЬ: 2 КОЛОНКИ ==========
  (write-line "  : row {" f)

  ;; ---------- ЛЕВАЯ КОЛОНКА ----------
  (write-line "    : column {" f)
  (write-line "      width = 36;" f)

  ;; < Допуски (ед. чертежа) >
  (write-line "      : boxed_column {" f)
  (write-line "        label = \"Допуски (ед. чертежа)\";" f)
  ;; В плане
  (write-line "        : row {" f)
  (write-line "          : toggle { label = \"В плане:\"; key = \"tol_plan_on\"; width = 14; }" f)
  (write-line "          : edit_box { key = \"tol_plan\"; edit_width = 8; fixed_width = true; }" f)
  (write-line "        }" f)

  ;; Цвета: надписи сверху, image_button снизу
  (write-line "        : row {" f)
  (write-line "          : column {" f)
  (write-line "            : text { label = \"Хорошо\"; alignment = centered; }" f)
  (write-line "            : image_button { key = \"img_col_ok\"; width = 6; height = 1.5; fixed_width = true; fixed_height = true; color = 3; }" f)
  (write-line "          }" f)
  (write-line "          : spacer { width = 2; }" f)
  (write-line "          : column {" f)
  (write-line "            : text { label = \"Плохо\"; alignment = centered; }" f)
  (write-line "            : image_button { key = \"img_col_fail\"; width = 6; height = 1.5; fixed_width = true; fixed_height = true; color = 1; }" f)
  (write-line "          }" f)
  (write-line "        }" f)
  (write-line "      }" f)

  ;; < Проектная отметка >
  (write-line "      : boxed_column {" f)
  (write-line "        label = \"Проектная отметка\";" f)
  (write-line "        : row {" f)
  (write-line "          : edit_box { key = \"elev_val\"; edit_width = 10; }" f)
  (write-line "          : spacer { width = 1; }" f)
  (write-line "          : toggle { label = \"Из объекта\"; key = \"elev_from_obj\"; }" f)
  (write-line "        }" f)
  (write-line "      }" f)

  ;; < Параметры >
  (write-line "      : boxed_column {" f)
  (write-line "        label = \"Параметры\";" f)
  (write-line "        : row {" f)
  (write-line "          : text { label = \"k=\"; width = 3; }" f)
  (write-line "          : edit_box { key = \"coeff\"; edit_width = 6; }" f)
  (write-line "          : popup_list { key = \"precision\"; width = 8; }" f)
  (write-line "        }" f)
  (write-line "      }" f)

  (write-line "    }" f)

  ;; ---------- ПРАВАЯ КОЛОНКА ----------
  (write-line "    : column {" f)
  (write-line "      width = 24;" f)

  ;; < Текст >
  (write-line "      : boxed_column {" f)
  (write-line "        label = \"Текст\";" f)
  (write-line "        : row {" f)
  (write-line "          : text { label = \"Высота:\"; width = 8; }" f)
  (write-line "          : edit_box { key = \"txt_height\"; edit_width = 6; }" f)
  (write-line "        }" f)
  (write-line "        : popup_list { key = \"txt_style\"; width = 18; }" f)
  (write-line "      }" f)

  ;; < Слой >
  (write-line "      : boxed_column {" f)
  (write-line "        label = \"Слой\";" f)
  (write-line "        : popup_list { key = \"layer_sel\"; width = 18; }" f)
  (write-line "        : edit_box { label = \"Новый:\"; key = \"layer_new\"; edit_width = 14; }" f)
  (write-line "        : row {" f)
  (write-line "          : text { label = \"Цвет:\"; width = 6; }" f)
  (write-line "          : image_button { key = \"img_color\"; width = 6; height = 1.5; fixed_width = true; fixed_height = true; color = 0; }" f)
  (write-line "        }" f)
  (write-line "      }" f)

  (write-line "    }" f)
  (write-line "  }" f)

  ;; ========== НИЖНЯЯ ЧАСТЬ ==========

  ;; < Положение стрелки >
  (write-line "  : boxed_column {" f)
  (write-line "    label = \"Положение стрелки\";" f)
  (write-line "    : row {" f)
  (write-line "      : column {" f)
  (write-line "        : text { label = \"Замкнутый контур:\"; }" f)
  (write-line "        : popup_list { key = \"side_closed\"; width = 18; }" f)
  (write-line "      }" f)
  (write-line "      : column {" f)
  (write-line "        : text { label = \"Разомкнутый контур:\"; }" f)
  (write-line "        : popup_list { key = \"side_open\"; width = 18; }" f)
  (write-line "      }" f)
  (write-line "    }" f)
  (write-line "  }" f)

  ;; Кнопки
  (write-line "  : row {" f)
  (write-line "    : button { label = \"Справка\"; key = \"btn_help\"; width = 12; fixed_width = true; }" f)
  (write-line "    : spacer { width = 2; }" f)
  (write-line "    ok_cancel;" f)
  (write-line "  }" f)

  (write-line "}" f)
  (close f)
  fn
)

;;; ================================================================
;;; ГЛАВНАЯ ФУНКЦИЯ
;;; ================================================================
(defun c:GeoOffset ( / dcl_fn dcl_id what_next
                       ssProj ssFact projEnts factEnts
                       selLayIdx lastLayIdx
                       old-osmode i pt result fResult
                       bestEnt bestFoot bestDist
                       dist sign distStr rawVal blkObj
                       totalPts errCount okCount
                       doProcess tmpCol tmpIdx )
  (vl-load-com)
  (geo:create-arrow-block)

  ;; === ПРИНУДИТЕЛЬНАЯ ВАЛИДАЦИЯ ВСЕХ ПЕРЕМЕННЫХ ===
  ;; Гарантирует правильный тип после обновления версии
  (if (not (numberp *geo:coeff*))       (setq *geo:coeff*       1000.0))
  (if (not (numberp *geo:precision*))   (setq *geo:precision*   0))
  (if (not (numberp *geo:txtht*))       (setq *geo:txtht*       1.8))
  (if (not (numberp *geo:tol-plan*))    (setq *geo:tol-plan*    0.01))
  (if (not (numberp *geo:elev-val*))    (setq *geo:elev-val*    0.0))
  (if (not (numberp *geo:color*))       (setq *geo:color*       80))
  (if (not (numberp *geo:color-ok*))    (setq *geo:color-ok*    3))
  (if (not (numberp *geo:color-fail*))  (setq *geo:color-fail*  1))
  (if (/= (type *geo:layer*) 'STR)       (setq *geo:layer*       "Geo_deviation"))
  (if (/= (type *geo:txtstyle*) 'STR)    (setq *geo:txtstyle*    "Standard"))
  (if (/= (type *geo:tol-plan-on*) 'STR) (setq *geo:tol-plan-on* "1"))
  (if (/= (type *geo:side-closed*) 'STR) (setq *geo:side-closed* "to_point"))
  (if (/= (type *geo:side-open*) 'STR)   (setq *geo:side-open*   "to_point"))
  (if (/= (type *geo:elev-mode*) 'STR)   (setq *geo:elev-mode*   "manual"))



  (setq *geo:styles-list* (geo:get-text-styles)
        *geo:layers-list* (geo:get-layers))
  (setq *geo:layers-plus* (append *geo:layers-list* (list ">> Создать новый <<")))
  (setq lastLayIdx (1- (length *geo:layers-plus*)))

  (setq dcl_fn (geo:write-dcl)
        dcl_id (load_dialog dcl_fn)
        ssProj nil ssFact nil what_next 5)

  ;; ======== ЦИКЛ ДИАЛОГА ========
  (while (>= what_next 2)
    (if (not (new_dialog "GeoOffset" dcl_id))
      (progn (princ "\n[GeoOffset] Ошибка загрузки диалога.") (setq what_next -1))
      (progn
        ;; --- Заполнение списков ---
        (start_list "txt_style") (mapcar 'add_list *geo:styles-list*) (end_list)
        (start_list "layer_sel") (mapcar 'add_list *geo:layers-plus*) (end_list)

        (start_list "precision")
        (mapcar 'add_list '("0" "0.0" "0.00" "0.000"))
        (end_list)
        (set_tile "precision" (geo:safe-itoa *geo:precision*))

        (start_list "side_closed")
        (mapcar 'add_list '("К точке" "Снаружи" "Внутри"))
        (end_list)
        (cond ((= *geo:side-closed* "to_point") (set_tile "side_closed" "0"))
              ((= *geo:side-closed* "outside")  (set_tile "side_closed" "1"))
              ((= *geo:side-closed* "inside")   (set_tile "side_closed" "2")))

        (start_list "side_open")
        (mapcar 'add_list '("К точке" "Слева" "Справа"))
        (end_list)
        (cond ((= *geo:side-open* "to_point") (set_tile "side_open" "0"))
              ((= *geo:side-open* "left")     (set_tile "side_open" "1"))
              ((= *geo:side-open* "right")    (set_tile "side_open" "2")))

        ;; --- Текущие значения ---
        (set_tile "tol_plan_on"  *geo:tol-plan-on*)
        (set_tile "tol_plan"     (geo:safe-rtos *geo:tol-plan* 2 4))
        (set_tile "elev_val"     (geo:safe-rtos *geo:elev-val* 2 3))
        (set_tile "elev_from_obj" (if (and *geo:elev-mode* (= *geo:elev-mode* "object")) "1" "0"))
        (set_tile "coeff"        (geo:safe-rtos *geo:coeff* 2 0))
        (set_tile "txt_height"   (geo:safe-rtos *geo:txtht* 2 1))

        ;; Стиль текста (vl-position может вернуть nil)
        (setq tmpIdx (vl-position *geo:txtstyle* *geo:styles-list*))
        (if tmpIdx
          (set_tile "txt_style" (itoa tmpIdx))
          (set_tile "txt_style" "0"))

        ;; Слой (vl-position может вернуть nil если слоя нет)
        (setq selLayIdx (vl-position *geo:layer* *geo:layers-list*))
        (if selLayIdx
          (progn
            (set_tile "layer_sel" (itoa selLayIdx))
            (mode_tile "layer_new" 1))
          (progn
            (set_tile "layer_sel" (itoa lastLayIdx))
            (set_tile "layer_new" *geo:layer*)
            (mode_tile "layer_new" 0)))



        ;; mode_tile: проектная отметка
        (if (= *geo:elev-mode* "object")
          (mode_tile "elev_val" 1) (mode_tile "elev_val" 0))

        ;; --- ЗАЛИВКА ЦВЕТОВЫХ ИНДИКАТОРОВ ---
        (geo:fill-color-tile "img_col_ok"   *geo:color-ok*)
        (geo:fill-color-tile "img_col_fail" *geo:color-fail*)
        (geo:fill-color-tile "img_color"    *geo:color*)

        ;; --- ACTION TILES ---
        (action_tile "elev_from_obj"
          "(if (= $value \"1\") (mode_tile \"elev_val\" 1) (mode_tile \"elev_val\" 0))")

        (action_tile "layer_sel"
          (strcat "(if (= (atoi $value) " (itoa lastLayIdx) ")"
                  "  (mode_tile \"layer_new\" 0)"
                  "  (mode_tile \"layer_new\" 1))"))

        ;; Цвет слоя — image_button, nil-safe
        (action_tile "img_color"
          "(progn (setq *geo:tmp-col* (acad_colordlg (fix *geo:color*))) (if *geo:tmp-col* (progn (setq *geo:color* *geo:tmp-col*) (geo:fill-color-tile \"img_color\" *geo:color*))))")

        ;; Цвет «Хорошо» — image_button, nil-safe
        (action_tile "img_col_ok"
          "(progn (setq *geo:tmp-col* (acad_colordlg (fix *geo:color-ok*))) (if *geo:tmp-col* (progn (setq *geo:color-ok* *geo:tmp-col*) (geo:fill-color-tile \"img_col_ok\" *geo:color-ok*))))")

        ;; Цвет «Плохо» — image_button, nil-safe
        (action_tile "img_col_fail"
          "(progn (setq *geo:tmp-col* (acad_colordlg (fix *geo:color-fail*))) (if *geo:tmp-col* (progn (setq *geo:color-fail* *geo:tmp-col*) (geo:fill-color-tile \"img_col_fail\" *geo:color-fail*))))")

        ;; Справка
        (action_tile "btn_help"
          (strcat
            "(alert \""
            "GeoOffset v3.4\\n\\n"
            "Порядок работы:\\n"
            "1. Настройте параметры\\n"
            "2. Нажмите Ok\\n"
            "3. Укажите образец проектного объекта\\n"
            "4. Выберите все проектные объекты\\n"
            "5. Укажите образец фактической точки\\n"
            "6. Выберите все фактические точки\\n\\n"
            "Масштаб блока: 0=DIMSCALE, или вручную.\\n"
            "Допуск в ед. чертежа (ДО умножения на k).\\n"
            "Хорошо = в допуске, Плохо = превышение.\""
            ")"))

        ;; OK
        (action_tile "accept"
          (strcat
            "(progn"
            "  (setq *geo:tol-plan-on* (get_tile \"tol_plan_on\"))"
            "  (setq *geo:tol-plan*    (atof (get_tile \"tol_plan\")))"
            "  (setq *geo:elev-val*    (atof (get_tile \"elev_val\")))"
            "  (setq *geo:elev-mode*   (if (= (get_tile \"elev_from_obj\") \"1\") \"object\" \"manual\"))"
            "  (setq *geo:coeff*       (atof (get_tile \"coeff\")))"
            "  (setq *geo:precision*   (atoi (get_tile \"precision\")))"
            "  (setq *geo:txtht*       (atof (get_tile \"txt_height\")))"
            "  (setq *geo:txtstyle*    (nth (atoi (get_tile \"txt_style\")) *geo:styles-list*))"
            "  (setq selLayIdx (atoi (get_tile \"layer_sel\")))"
            "  (if (= selLayIdx " (itoa lastLayIdx) ")"
            "    (setq *geo:layer* (get_tile \"layer_new\"))"
            "    (setq *geo:layer* (nth selLayIdx *geo:layers-list*)))"
            "  (setq *geo:side-closed* (nth (atoi (get_tile \"side_closed\")) (list \"to_point\" \"outside\" \"inside\")))"
            "  (setq *geo:side-open*   (nth (atoi (get_tile \"side_open\"))   (list \"to_point\" \"left\" \"right\")))"
            "  (done_dialog 1))"
          )
        )
        (action_tile "cancel" "(done_dialog 0)")

        (setq what_next (start_dialog))
        (if (= what_next 0) (princ "\n[GeoOffset] Отменено."))
      )
    )
  )
  (unload_dialog dcl_id)

  ;; ================================================================
  ;; ПОСЛЕ OK
  ;; ================================================================
  (setq doProcess T)

  (if (and (= what_next 1) doProcess)
    (progn
      (princ "\n\n======= ПРОЕКТНЫЕ ОБЪЕКТЫ =======")
      (setq fResult (geo:build-filter-from-sample "\nУкажите ОБРАЗЕЦ проектного объекта: "))
      (if (null fResult)
        (progn (princ "\n[GeoOffset] Образец не выбран. Отмена.") (setq doProcess nil)))))

  (if (and (= what_next 1) doProcess)
    (progn
      (setq *geo:proj-filter* (car fResult) *geo:proj-info* (cadr fResult))
      (princ (strcat "\nВыберите проектные объекты [" *geo:proj-info* "]: "))
      (setq ssProj (ssget "_:L" *geo:proj-filter*))
      (if (and (null ssProj) (> (length *geo:proj-filter*) 2))
        (progn (princ "\nПробую без цвета...")
          (setq ssProj (ssget "_:L" (list (car *geo:proj-filter*) (cadr *geo:proj-filter*))))))
      (if (null ssProj)
        (progn (princ "\n[GeoOffset] Проектные не выбраны.") (setq doProcess nil))
        (princ (strcat "\nВыбрано проектных: " (itoa (sslength ssProj)))))))

  (if (and (= what_next 1) doProcess)
    (progn
      (princ "\n\n======= ФАКТИЧЕСКИЕ ТОЧКИ =======")
      (setq fResult (geo:build-filter-from-sample "\nУкажите ОБРАЗЕЦ фактической точки: "))
      (if (null fResult)
        (progn (princ "\n[GeoOffset] Образец не выбран. Отмена.") (setq doProcess nil)))))

  (if (and (= what_next 1) doProcess)
    (progn
      (setq *geo:fact-filter* (car fResult) *geo:fact-info* (cadr fResult))
      (princ (strcat "\nВыберите фактические точки [" *geo:fact-info* "]: "))
      (setq ssFact (ssget "_:L" *geo:fact-filter*))
      (if (and (null ssFact) (> (length *geo:fact-filter*) 2))
        (progn (princ "\nПробую без цвета...")
          (setq ssFact (ssget "_:L" (list (car *geo:fact-filter*) (cadr *geo:fact-filter*))))))
      (if (null ssFact)
        (progn (princ "\n[GeoOffset] Точки не выбраны.") (setq doProcess nil))
        (princ (strcat "\nВыбрано фактических: " (itoa (sslength ssFact)))))))

  ;; ================================================================
  ;; ОБРАБОТКА
  ;; [FIX v3.0] Допуск: bestDist сравнивается с tol-plan ДО * k
  ;; ================================================================
  (if (and (= what_next 1) doProcess ssProj ssFact)
    (progn
      (geo:ensure-layer *geo:layer* *geo:color*)
      (setq projEnts  (geo:ss->list ssProj)
            factEnts  (geo:ss->list ssFact)
            totalPts  (length factEnts)
            errCount  0 okCount 0)
      (if (or (null *geo:coeff*) (<= *geo:coeff* 0))
        (progn (princ "\n[GeoOffset] k<=0, установлен k=1.0")
               (setq *geo:coeff* 1.0)))
      (setq old-osmode (getvar "OSMODE"))
      (setvar "OSMODE" 0)
      (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))

      (princ (strcat "\n\n  Масштаб блока: " (rtos (geo:get-block-scale) 2 2)
                     " (DIMSCALE=" (rtos (getvar "DIMSCALE") 2 1) ")"))
      (princ (strcat "\n  k= " (rtos *geo:coeff* 2 0)))
      (princ (strcat "\n  Допуск: " (rtos *geo:tol-plan* 2 4)
                     " ед. чертежа (сравнение ДО k)\n"))

      (setq i 0)
      (foreach factEnt factEnts
        (setq pt (geo:get-obj-coords factEnt))
        (if pt
          (progn
            (setq pt (geo:flatten pt))
            (setq result (geo:find-nearest pt projEnts))
            (if result
              (progn
                (setq bestEnt  (car result)
                      bestFoot (geo:flatten (cadr result))
                      bestDist (caddr result))
                ;; dist = значение в выходных единицах (для отображения)
                (setq dist (* bestDist *geo:coeff*))
                (setq sign (geo:calc-sign bestEnt bestFoot pt))
                (setq rawVal (rtos dist 2 *geo:precision*))
                (cond
                  ((= sign "+")
                   (if (= *geo:prefix* " ")
                     (setq distStr rawVal)
                     (setq distStr (strcat *geo:prefix* rawVal))))
                  (T (setq distStr (strcat sign rawVal))))
                ;; Вставка блока
                (setq blkObj (geo:insert-arrow bestEnt bestFoot pt distStr rawVal *geo:layer*))
                ;; [FIX v3.0] Цвет: допуск сравнивается ДО умножения на k
                ;; bestDist — расстояние в единицах чертежа
                ;; *geo:tol-plan* — допуск в единицах чертежа
                ;; Пример: чертёж в м, bestDist=0.03 м, допуск=0.05 м > 0.03 ? 0.05 > Хорошо
                (if (= *geo:tol-plan-on* "1")
                  (if (<= bestDist *geo:tol-plan*)
                    (vla-put-Color blkObj (fix *geo:color-ok*))     ; Хорошо
                    (vla-put-Color blkObj (fix *geo:color-fail*)))  ; Плохо
                  (vla-put-Color blkObj (fix *geo:color*)))
                (setq okCount (1+ okCount))
                (princ (strcat "\n  [" (itoa (1+ i)) "/" (itoa totalPts) "] "
                               distStr " (d=" (rtos bestDist 2 4) ")"
                               (if (= *geo:tol-plan-on* "1")
                                 (if (<= bestDist *geo:tol-plan*) " OK" " !!!")
                                 ""))))
              (progn (setq errCount (1+ errCount))
                (princ (strcat "\n  [" (itoa (1+ i)) "] ОШИБКА"))))))
        (setq i (1+ i)))

      (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setvar "OSMODE" old-osmode)

      (princ "\n\n========================================")
      (princ (strcat "\n  Обработано: " (itoa okCount)))
      (if (> errCount 0) (princ (strcat "\n  Ошибок: " (itoa errCount))))
      (princ "\n========================================")
    )
  )
  (princ)
)

(princ "\n[GeoOffset v3.4] Загружен. Команда: GeoOffset")
(princ)
