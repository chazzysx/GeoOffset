;;; ================================================================
;;; GeoOffset.lsp v3.4
;;; ������ � ���������� ���������� ����������� �����
;;; �� ��������� ����� � ��������
;;; AutoCAD 2013-2026 / BricsCAD Pro / Civil 3D
;;;
;;; v3.4 � FIX: ������� VALUE ������ ��������������
;;;         (Rotation = 0 ���������� ������ ��������)
;;; v3.0 � DCL: image_button + fill_image ��� ������ ������/�����
;;;         FIX: ������ ������������ �� ��������� �� k
;;;         ������� �����: DIMSCALE ��� �������
;;;         ��������� ������� ����� ������������ �������� �����
;;; ================================================================
(vl-load-com)

;;; ================================================================
;;; ���������� ����������
;;; ================================================================
(if (null *geo:side-closed*)   (setq *geo:side-closed*   "to_point"))
(if (null *geo:side-open*)     (setq *geo:side-open*     "to_point"))
(if (null *geo:coeff*)         (setq *geo:coeff*         1000.0))
(if (null *geo:precision*)    (setq *geo:precision*     0))
(if (null *geo:prefix*)        (setq *geo:prefix*        "+"))
(if (null *geo:layer*)         (setq *geo:layer*         "Geo_deviation"))
(if (null *geo:txtstyle*)      (setq *geo:txtstyle*      "Standard"))
(if (null *geo:txtht*)         (setq *geo:txtht*         1.8))
(if (null *geo:tol-plan*)      (setq *geo:tol-plan*      0.05))
(if (null *geo:tol-plan-on*)   (setq *geo:tol-plan-on*   "1"))
(if (null *geo:tol-elev-on*)   (setq *geo:tol-elev-on*   "0"))
(if (null *geo:tol-elev-lo*)  (setq *geo:tol-elev-lo*   0.0))
(if (null *geo:tol-elev-hi*)  (setq *geo:tol-elev-hi*   0.0))
(if (null *geo:elev-mode*)     (setq *geo:elev-mode*     "manual"))
(if (null *geo:elev-val*)     (setq *geo:elev-val*      0.0))
(if (null *geo:blk-scale*)    (setq *geo:blk-scale*     0.0))  ; 0 = ���� (DIMSCALE)
(if (null *geo:color*)         (setq *geo:color*         80))
(if (null *geo:color-ok*)      (setq *geo:color-ok*      3))   ; ������ = ������
(if (null *geo:color-fail*)    (setq *geo:color-fail*    1))   ; ������� = �����
(if (null *geo:opt-onepoint*)  (setq *geo:opt-onepoint*  "0"))
(if (null *geo:opt-short*)     (setq *geo:opt-short*     "0"))
(if (null *geo:opt-leader*)    (setq *geo:opt-leader*    "0"))
(if (null *geo:opt-fixpos*)    (setq *geo:opt-fixpos*    "1"))
(if (null *geo:proj-filter*)   (setq *geo:proj-filter*   nil))
(if (null *geo:fact-filter*)   (setq *geo:fact-filter*   nil))
(if (null *geo:proj-info*)     (setq *geo:proj-info*     "�� �����"))
(if (null *geo:fact-info*)     (setq *geo:fact-info*     "�� �����"))
(setq *geo:styles-list* nil)
(setq *geo:layers-list* nil)
(setq *geo:layers-plus* nil)

;;; ================================================================
;;; �������� ����� Geo_Arrow
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
      ;; ATTDEF VALUE � �������
      (entmake (list '(0 . "ATTDEF") '(100 . "AcDbEntity") '(8 . "0") '(62 . 256)
                     '(100 . "AcDbText")
                     (cons 10 (list (* as 1.5) 0.0 0.0))
                     (cons 40 th) '(1 . "0.0")
                     '(100 . "AcDbAttributeDefinition")
                     '(3 . "Offset Value") '(2 . "VALUE")
                     '(70 . 1) '(280 . 0)))
      ;; ATTDEF DISPLAY � �������
      (entmake (list '(0 . "ATTDEF") '(100 . "AcDbEntity") '(8 . "0") '(62 . 256)
                     '(100 . "AcDbText")
                     (cons 10 (list (* as 1.5) (* as 0.5) 0.0))
                     (cons 11 (list (* as 1.5) (* as 0.5) 0.0))
                     (cons 40 th) '(1 . "0.0")
                     '(100 . "AcDbAttributeDefinition")
                     '(3 . "Display Text") '(2 . "DISPLAY")
                     '(70 . 0) '(72 . 0) '(74 . 2) '(280 . 0)))
      (entmake '((0 . "ENDBLK")))
      (princ "\n[GeoOffset] ���� Geo_Arrow ������.")
    )
  )
)

;;; ================================================================
;;; �������
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

;;; ���������� ������ ��� rtos/itoa (������ �� nil > numberp)
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
;;; ���� ����������
;;; ================================================================
;;; ���� ����������
;;; cross > 0 = ����. ����� ����� �� ����������� ������
;;; cross < 0 = ����. ����� ������ �� ����������� ������
;;; "left"  = ������� �����  > ���� "+" ���� ����� ����� (cross>0), ����� "-"
;;; "right" = ������� ������ > ���� "+" ���� ����� ������ (cross<0), ����� "-"
(defun geo:calc-sign ( ent ptFoot ptReal / cross mode )
  (setq cross (geo:cross-z ent ptFoot ptReal)
        mode (if (geo:closed-p ent) *geo:side-closed* *geo:side-open*))
  (cond
    ((= mode "to_point") (if (> cross 0) "+" "-"))
    ;; inside/outside/left/right: ���� ������ "+"
    ;; ����������� ������� ��������, Flip ������������ ������
    ((= mode "inside")   "+")
    ((= mode "outside")  "+")
    ((= mode "left")     "+")
    ((= mode "right")    "+")
    (T                   (if (> cross 0) "+" "-"))))


;;; ================================================================
;;; ����� �� ��������� (������������ �������� "���������")
;;; ================================================================
;;; ��� �������� ��������:
;;;   mode = "left"  > ������� ����� �� ����������� ������.
;;;   mode = "right" > ������� ������.
;;; ���� ����������� ����� ��������� �� ��������������� �������
;;; (�� ����� cross-z), ������� ��� ������ ����� �������
;;; "���������������". ������ ������ ��������� �������� ���������.
(defun geo:need-mirror ( ent ptFoot ptReal / cross mode )
  (setq cross (geo:cross-z ent ptFoot ptReal)
        mode  (if (geo:closed-p ent) *geo:side-closed* *geo:side-open*))
  (cond
    ;; --- ����������� ������ ---
    ;; ������� �����, � ����� ������ > Flip
    ((and (= mode "left")  (< cross 0)) T)
    ;; ������� ������, � ����� ����� > Flip
    ((and (= mode "right") (> cross 0)) T)
    ;; --- ��������� ������ ---
    ;; ������� ������ (������� ������), � ����� ����� (cross>0) > Flip
    ((and (= mode "inside")  (> cross 0)) T)
    ;; ������� ������ (������� �����), � ����� ������ (cross<0) > Flip
    ((and (= mode "outside") (< cross 0)) T)
    (T nil)))

;;; ================================================================
;;; LM:setdynpropvalue � ��������� �������� ������������� ���������
;;; (�� ������� Lee Mac). blk = VLA-������, prp = ��� ���������, val = ��������
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
;;; ��������� ��������� � ������������� ����� Geo_Arrow (���� �����)
;;; ���������� LM:setdynpropvalue ��� ��������� Flip = 1
;;; ��� ��������� � �����: "Flip"
;;; ================================================================
(defun geo:mirror-block-if-needed ( blkObj ent ptFoot ptReal )
  (if (and blkObj
           (geo:need-mirror ent ptFoot ptReal)
           (= :vlax-true (vla-get-IsDynamicBlock blkObj)))
    (progn
      (LM:setdynpropvalue blkObj "Flip" 1)
      (vla-Update blkObj))))

;;; ================================================================
;;; ���� �������
;;; ================================================================
;;; ���� ������� � ���������� ����������� �������
;;; "left"  > ������� = (-dy, dx)  ������ (����� �� ����������� ������)
;;; "right" > ������� = (dy, -dx)  ������ (������ �� ����������� ������)
;;; "to_point"/"inside"/"outside" > ������� �� cross-z (�/�� ����. �����)
(defun geo:calc-angle ( ent ptFoot ptReal / tang dx dy cross mode nx ny ang )
  (setq tang (geo:get-tangent ent ptFoot) dx (car tang) dy (cadr tang))
  (setq mode (if (geo:closed-p ent) *geo:side-closed* *geo:side-open*))
  (cond
    ;; === ����������� ������ ===
    ;; ����� �� ����������� ������: ������� = (-dy, dx) ������
    ((= mode "left")
     (setq nx (- dy) ny dx))
    ;; ������ �� ����������� ������: ������� = (dy, -dx) ������
    ((= mode "right")
     (setq nx dy ny (- dx)))
    ;; === ��������� ������ ===
    ;; "inside" > ������� ������ ������ �� ����������� = (dy, -dx)
    ;; ��� ������������ ������ CCW ��� ������ �������.
    ;; ���� ����� ������� � Flip ��������� ����� geo:need-mirror.
    ((= mode "inside")
     (setq nx dy ny (- dx)))
    ;; "outside" > ������� ������ ����� �� ����������� = (-dy, dx)
    ;; ��� CCW ��� ������. ���� ����� ������ � Flip.
    ((= mode "outside")
     (setq nx (- dy) ny dx))
    ;; === � ����� / �� ��������� ===
    ;; ������� � ������� ����������� ����� (�� cross-z)
    (T
     (setq cross (geo:cross-z ent ptFoot ptReal))
     (if (> cross 0) (setq nx (- dy) ny dx) (setq nx dy ny (- dx)))))
  (setq ang (- (atan ny nx) (/ pi 2)))
  ang)

;;; ================================================================
;;; ������� �����
;;; ���� *geo:blk-scale* > 0 > ���������� ��� ��������.
;;; ���� = 0 > ���������� DIMSCALE (����������� ������).
;;; DIMSCALE = ���������� ����������� ��������� ������.
;;; ================================================================
(defun geo:get-block-scale ( / ds )
  (cond
    ((and (numberp *geo:blk-scale*) (> *geo:blk-scale* 0))
     *geo:blk-scale*)
    (T
     (setq ds (getvar "DIMSCALE"))
     (if (or (null ds) (<= ds 0)) 1.0 ds)))
)

;;; ================================================================
;;; ENTSEL > ������
;;; ================================================================
(defun geo:build-filter-from-sample ( promptMsg / sel ent ed etyp elay ecol filter info )
  (setq sel (entsel promptMsg))
  (if (null sel)
    (progn (princ "\n������ �� �������.") nil)
    (progn
      (setq ent  (car sel)
            ed   (entget ent)
            etyp (cdr (assoc 0 ed))
            elay (cdr (assoc 8 ed))
            ecol (cdr (assoc 62 ed)))
      (setq filter (list (cons 0 etyp) (cons 8 elay)))
      (if (and ecol (/= ecol 0))
        (setq filter (append filter (list (cons 62 ecol)))))
      (setq info (strcat "���: " etyp "  ����: " elay
                         (if (and ecol (/= ecol 0))
                           (strcat "  ����: " (itoa ecol))
                           "  ����: ByLayer")))
      (princ (strcat "\n������: " info))
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
;;; ������� ��������� ���������� (image_button)
;;; ================================================================
(defun geo:fill-color-tile ( tileKey aciColor / w h )
  (setq w (dimx_tile tileKey) h (dimy_tile tileKey))
  (start_image tileKey)
  (fill_image 0 0 w h (fix aciColor))
  (end_image)
)

;;; ================================================================
;;; ������� �����
;;; [v3.0] ������� = DIMSCALE ��� ������ ����
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
  ;; ��������� WCS-������� ��� ������ DISPLAY
  ;; ���������� rotAngle �������: ����� ������ �� ����������� ������� �� 4?scale
  ;; ��� Y ����� ��� rotAngle: ����������� (cos(rot+?/2), sin(rot+?/2)) = (-sin(rot), cos(rot))
  (setq cosA (cos rotAngle) sinA (sin rotAngle))
  ;; Offset � ����������� ����� DISPLAY: ~(4.5, 1.5) > � WCS ��� ��������:
  ;; xWCS = 4.5*cos - 1.5*sin, yWCS = 4.5*sin + 1.5*cos (�� ? scale)
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
           (vla-put-Rotation att 0.0))  ;; v3.4: VALUE ������ ��������������
          ((= tag "DISPLAY")
           (vla-put-TextString att distStr)
           (vla-put-Rotation att 0.0)
           (vla-put-InsertionPoint att txtPt)
           (vla-put-TextAlignmentPoint att txtPt)
           (if (tblsearch "STYLE" *geo:txtstyle*)
             (vla-put-StyleName att *geo:txtstyle*)))))))
  ;; ��������� ������������ ���������, ���� �����
  (geo:mirror-block-if-needed blkObj ent ptFoot ptReal)
  blkObj)

;;; ================================================================
;;; DCL v3.0 � image_button � �������� �����, ������/�����
;;; ================================================================
(defun geo:write-dcl ( / fn f )
  (setq fn (strcat (getvar "TEMPPREFIX") "GeoOffset.dcl"))
  (setq f (open fn "w"))

  (write-line "GeoOffset : dialog {" f)
  (write-line "  label = \"GeoOffset v2.9\";" f)

  ;; ========== ������� �����: 2 ������� ==========
  (write-line "  : row {" f)

  ;; ---------- ����� ������� ----------
  (write-line "    : column {" f)
  (write-line "      width = 36;" f)

  ;; < ������� (��. �������) >
  (write-line "      : boxed_column {" f)
  (write-line "        label = \"������� (��. �������)\";" f)
  ;; � �����
  (write-line "        : row {" f)
  (write-line "          : toggle { label = \"� �����:\"; key = \"tol_plan_on\"; width = 14; }" f)
  (write-line "          : edit_box { key = \"tol_plan\"; edit_width = 8; fixed_width = true; }" f)
  (write-line "        }" f)
  ;; �� ������
  (write-line "        : row {" f)
  (write-line "          : toggle { label = \"�� ������:\"; key = \"tol_elev_on\"; width = 14; }" f)
  (write-line "          : edit_box { key = \"tol_elev_lo\"; edit_width = 5; fixed_width = true; }" f)
  (write-line "          : text { label = \"-\"; width = 1; }" f)
  (write-line "          : edit_box { key = \"tol_elev_hi\"; edit_width = 5; fixed_width = true; }" f)
  (write-line "        }" f)
  ;; �����: ������� ������, image_button �����
  (write-line "        : row {" f)
  (write-line "          : column {" f)
  (write-line "            : text { label = \"������\"; alignment = centered; }" f)
  (write-line "            : image_button { key = \"img_col_ok\"; width = 6; height = 1.5; fixed_width = true; fixed_height = true; color = 3; }" f)
  (write-line "          }" f)
  (write-line "          : spacer { width = 2; }" f)
  (write-line "          : column {" f)
  (write-line "            : text { label = \"�����\"; alignment = centered; }" f)
  (write-line "            : image_button { key = \"img_col_fail\"; width = 6; height = 1.5; fixed_width = true; fixed_height = true; color = 1; }" f)
  (write-line "          }" f)
  (write-line "        }" f)
  (write-line "      }" f)

  ;; < ��������� ������� >
  (write-line "      : boxed_column {" f)
  (write-line "        label = \"��������� �������\";" f)
  (write-line "        : row {" f)
  (write-line "          : edit_box { key = \"elev_val\"; edit_width = 10; }" f)
  (write-line "          : spacer { width = 1; }" f)
  (write-line "          : toggle { label = \"�� �������\"; key = \"elev_from_obj\"; }" f)
  (write-line "        }" f)
  (write-line "      }" f)

  ;; < ��������� >
  (write-line "      : boxed_column {" f)
  (write-line "        label = \"���������\";" f)
  (write-line "        : row {" f)
  (write-line "          : text { label = \"k=\"; width = 3; }" f)
  (write-line "          : edit_box { key = \"coeff\"; edit_width = 6; }" f)
  (write-line "          : popup_list { key = \"precision\"; width = 8; }" f)
  (write-line "          : popup_list { key = \"prefix\"; width = 5; }" f)
  (write-line "        }" f)
  (write-line "        : row {" f)
  (write-line "          : text { label = \"������� ����� (0=DIMSCALE):\"; width = 26; }" f)
  (write-line "          : edit_box { key = \"blk_scale\"; edit_width = 8; fixed_width = true; }" f)
  (write-line "        }" f)
  (write-line "      }" f)

  (write-line "    }" f)

  ;; ---------- ������ ������� ----------
  (write-line "    : column {" f)
  (write-line "      width = 24;" f)

  ;; < ����� >
  (write-line "      : boxed_column {" f)
  (write-line "        label = \"�����\";" f)
  (write-line "        : row {" f)
  (write-line "          : text { label = \"������:\"; width = 8; }" f)
  (write-line "          : edit_box { key = \"txt_height\"; edit_width = 6; }" f)
  (write-line "        }" f)
  (write-line "        : popup_list { key = \"txt_style\"; width = 18; }" f)
  (write-line "      }" f)

  ;; < ���� >
  (write-line "      : boxed_column {" f)
  (write-line "        label = \"����\";" f)
  (write-line "        : popup_list { key = \"layer_sel\"; width = 18; }" f)
  (write-line "        : edit_box { label = \"�����:\"; key = \"layer_new\"; edit_width = 14; }" f)
  (write-line "        : row {" f)
  (write-line "          : text { label = \"����:\"; width = 6; }" f)
  (write-line "          : image_button { key = \"img_color\"; width = 6; height = 1.5; fixed_width = true; fixed_height = true; color = 0; }" f)
  (write-line "        }" f)
  (write-line "      }" f)

  (write-line "    }" f)
  (write-line "  }" f)

  ;; ========== ������ ����� ==========

  ;; < ��������� ������� >
  (write-line "  : boxed_column {" f)
  (write-line "    label = \"��������� �������\";" f)
  (write-line "    : row {" f)
  (write-line "      : column {" f)
  (write-line "        : text { label = \"��������� ������:\"; }" f)
  (write-line "        : popup_list { key = \"side_closed\"; width = 18; }" f)
  (write-line "      }" f)
  (write-line "      : column {" f)
  (write-line "        : text { label = \"����������� ������:\"; }" f)
  (write-line "        : popup_list { key = \"side_open\"; width = 18; }" f)
  (write-line "      }" f)
  (write-line "    }" f)
  (write-line "  }" f)

  ;; < ����� >
  (write-line "  : boxed_column {" f)
  (write-line "    label = \"�����\";" f)
  (write-line "    : toggle { label = \"���� ������� ����� ������\"; key = \"opt_onepoint\"; }" f)
  (write-line "    : toggle { label = \"�������� �������\";         key = \"opt_short\"; }" f)
  (write-line "    : toggle { label = \"������� �� �������\";       key = \"opt_leader\"; }" f)
  (write-line "    : toggle { label = \"������������� ��������� ��������\"; key = \"opt_fixpos\"; }" f)
  (write-line "  }" f)

  ;; ������
  (write-line "  : row {" f)
  (write-line "    : button { label = \"�������\"; key = \"btn_help\"; width = 12; fixed_width = true; }" f)
  (write-line "    : spacer { width = 2; }" f)
  (write-line "    ok_cancel;" f)
  (write-line "  }" f)

  (write-line "}" f)
  (close f)
  fn
)

;;; ================================================================
;;; ������� �������
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

  ;; === �������������� ��������� ���� ���������� ===
  ;; ����������� ���������� ��� ����� ���������� ������
  (if (not (numberp *geo:coeff*))       (setq *geo:coeff*       1000.0))
  (if (not (numberp *geo:precision*))   (setq *geo:precision*   0))
  (if (not (numberp *geo:txtht*))       (setq *geo:txtht*       1.8))
  (if (not (numberp *geo:tol-plan*))    (setq *geo:tol-plan*    0.05))
  (if (not (numberp *geo:tol-elev-lo*)) (setq *geo:tol-elev-lo* 0.0))
  (if (not (numberp *geo:tol-elev-hi*)) (setq *geo:tol-elev-hi* 0.0))
  (if (not (numberp *geo:elev-val*))    (setq *geo:elev-val*    0.0))
  (if (not (numberp *geo:blk-scale*))   (setq *geo:blk-scale*   0.0))
  (if (not (numberp *geo:color*))       (setq *geo:color*       80))
  (if (not (numberp *geo:color-ok*))    (setq *geo:color-ok*    3))
  (if (not (numberp *geo:color-fail*))  (setq *geo:color-fail*  1))
  (if (/= (type *geo:prefix*) 'STR)      (setq *geo:prefix*      "+"))
  (if (/= (type *geo:layer*) 'STR)       (setq *geo:layer*       "Geo_deviation"))
  (if (/= (type *geo:txtstyle*) 'STR)    (setq *geo:txtstyle*    "Standard"))
  (if (/= (type *geo:tol-plan-on*) 'STR) (setq *geo:tol-plan-on* "1"))
  (if (/= (type *geo:tol-elev-on*) 'STR) (setq *geo:tol-elev-on* "0"))
  (if (/= (type *geo:side-closed*) 'STR) (setq *geo:side-closed* "to_point"))
  (if (/= (type *geo:side-open*) 'STR)   (setq *geo:side-open*   "to_point"))
  (if (/= (type *geo:elev-mode*) 'STR)   (setq *geo:elev-mode*   "manual"))
  (if (/= (type *geo:opt-onepoint*) 'STR)(setq *geo:opt-onepoint* "0"))
  (if (/= (type *geo:opt-short*) 'STR)   (setq *geo:opt-short*   "0"))
  (if (/= (type *geo:opt-leader*) 'STR)  (setq *geo:opt-leader*  "0"))
  (if (/= (type *geo:opt-fixpos*) 'STR)  (setq *geo:opt-fixpos*  "1"))


  (setq *geo:styles-list* (geo:get-text-styles)
        *geo:layers-list* (geo:get-layers))
  (setq *geo:layers-plus* (append *geo:layers-list* (list ">> ������� ����� <<")))
  (setq lastLayIdx (1- (length *geo:layers-plus*)))

  (setq dcl_fn (geo:write-dcl)
        dcl_id (load_dialog dcl_fn)
        ssProj nil ssFact nil what_next 5)

  ;; ======== ���� ������� ========
  (while (>= what_next 2)
    (if (not (new_dialog "GeoOffset" dcl_id))
      (progn (princ "\n[GeoOffset] ������ �������� �������.") (setq what_next -1))
      (progn
        ;; --- ���������� ������� ---
        (start_list "txt_style") (mapcar 'add_list *geo:styles-list*) (end_list)
        (start_list "layer_sel") (mapcar 'add_list *geo:layers-plus*) (end_list)

        (start_list "precision")
        (mapcar 'add_list '("0" "0.0" "0.00" "0.000"))
        (end_list)
        (set_tile "precision" (geo:safe-itoa *geo:precision*))

        (start_list "prefix")
        (mapcar 'add_list '("+" "-" " "))
        (end_list)
        (cond ((= *geo:prefix* "+") (set_tile "prefix" "0"))
              ((= *geo:prefix* "-") (set_tile "prefix" "1"))
              (T                    (set_tile "prefix" "2")))

        (start_list "side_closed")
        (mapcar 'add_list '("� �����" "������" "�������"))
        (end_list)
        (cond ((= *geo:side-closed* "to_point") (set_tile "side_closed" "0"))
              ((= *geo:side-closed* "inside")   (set_tile "side_closed" "1"))
              ((= *geo:side-closed* "outside")  (set_tile "side_closed" "2")))

        (start_list "side_open")
        (mapcar 'add_list '("� �����" "�����" "������"))
        (end_list)
        (cond ((= *geo:side-open* "to_point") (set_tile "side_open" "0"))
              ((= *geo:side-open* "left")     (set_tile "side_open" "1"))
              ((= *geo:side-open* "right")    (set_tile "side_open" "2")))

        ;; --- ������� �������� ---
        (set_tile "tol_plan_on"  *geo:tol-plan-on*)
        (set_tile "tol_plan"     (geo:safe-rtos *geo:tol-plan* 2 4))
        (set_tile "tol_elev_on"  *geo:tol-elev-on*)
        (set_tile "tol_elev_lo"  (geo:safe-rtos *geo:tol-elev-lo* 2 2))
        (set_tile "tol_elev_hi"  (geo:safe-rtos *geo:tol-elev-hi* 2 2))
        (set_tile "elev_val"     (geo:safe-rtos *geo:elev-val* 2 3))
        (set_tile "elev_from_obj" (if (and *geo:elev-mode* (= *geo:elev-mode* "object")) "1" "0"))
        (set_tile "coeff"        (geo:safe-rtos *geo:coeff* 2 0))
        (set_tile "blk_scale"    (geo:safe-rtos *geo:blk-scale* 2 1))
        (set_tile "txt_height"   (geo:safe-rtos *geo:txtht* 2 1))

        ;; ����� ������ (vl-position ����� ������� nil)
        (setq tmpIdx (vl-position *geo:txtstyle* *geo:styles-list*))
        (if tmpIdx
          (set_tile "txt_style" (itoa tmpIdx))
          (set_tile "txt_style" "0"))

        ;; ���� (vl-position ����� ������� nil ���� ���� ���)
        (setq selLayIdx (vl-position *geo:layer* *geo:layers-list*))
        (if selLayIdx
          (progn
            (set_tile "layer_sel" (itoa selLayIdx))
            (mode_tile "layer_new" 1))
          (progn
            (set_tile "layer_sel" (itoa lastLayIdx))
            (set_tile "layer_new" *geo:layer*)
            (mode_tile "layer_new" 0)))

        ;; ������
        (set_tile "opt_onepoint" *geo:opt-onepoint*)
        (set_tile "opt_short"    *geo:opt-short*)
        (set_tile "opt_leader"   *geo:opt-leader*)
        (set_tile "opt_fixpos"   *geo:opt-fixpos*)

        ;; mode_tile: ��������� �������
        (if (= *geo:elev-mode* "object")
          (mode_tile "elev_val" 1) (mode_tile "elev_val" 0))

        ;; --- ������� �������� ����������� ---
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

        ;; ���� ���� � image_button, nil-safe
        (action_tile "img_color"
          "(progn (setq *geo:tmp-col* (acad_colordlg (fix *geo:color*))) (if *geo:tmp-col* (progn (setq *geo:color* *geo:tmp-col*) (geo:fill-color-tile \"img_color\" *geo:color*))))")

        ;; ���� ������� � image_button, nil-safe
        (action_tile "img_col_ok"
          "(progn (setq *geo:tmp-col* (acad_colordlg (fix *geo:color-ok*))) (if *geo:tmp-col* (progn (setq *geo:color-ok* *geo:tmp-col*) (geo:fill-color-tile \"img_col_ok\" *geo:color-ok*))))")

        ;; ���� ������ � image_button, nil-safe
        (action_tile "img_col_fail"
          "(progn (setq *geo:tmp-col* (acad_colordlg (fix *geo:color-fail*))) (if *geo:tmp-col* (progn (setq *geo:color-fail* *geo:tmp-col*) (geo:fill-color-tile \"img_col_fail\" *geo:color-fail*))))")

        ;; �������
        (action_tile "btn_help"
          (strcat
            "(alert \""
            "GeoOffset v2.9\\n\\n"
            "������� ������:\\n"
            "1. ��������� ���������\\n"
            "2. ������� Ok\\n"
            "3. ������� ������� ���������� �������\\n"
            "4. �������� ��� ��������� �������\\n"
            "5. ������� ������� ����������� �����\\n"
            "6. �������� ��� ����������� �����\\n\\n"
            "������� �����: 0=DIMSCALE, ��� �������.\\n"
            "������ � ��. ������� (�� ��������� �� k).\\n"
            "������ = � �������, ����� = ����������.\""
            ")"))

        ;; OK
        (action_tile "accept"
          (strcat
            "(progn"
            "  (setq *geo:tol-plan-on* (get_tile \"tol_plan_on\"))"
            "  (setq *geo:tol-plan*    (atof (get_tile \"tol_plan\")))"
            "  (setq *geo:tol-elev-on* (get_tile \"tol_elev_on\"))"
            "  (setq *geo:tol-elev-lo* (atof (get_tile \"tol_elev_lo\")))"
            "  (setq *geo:tol-elev-hi* (atof (get_tile \"tol_elev_hi\")))"
            "  (setq *geo:elev-val*    (atof (get_tile \"elev_val\")))"
            "  (setq *geo:elev-mode*   (if (= (get_tile \"elev_from_obj\") \"1\") \"object\" \"manual\"))"
            "  (setq *geo:coeff*       (atof (get_tile \"coeff\")))"
            "  (setq *geo:blk-scale*   (atof (get_tile \"blk_scale\")))"
            "  (setq *geo:precision*   (atoi (get_tile \"precision\")))"
            "  (setq *geo:prefix*      (nth (atoi (get_tile \"prefix\")) (list \"+\" \"-\" \" \")))"
            "  (setq *geo:txtht*       (atof (get_tile \"txt_height\")))"
            "  (setq *geo:txtstyle*    (nth (atoi (get_tile \"txt_style\")) *geo:styles-list*))"
            "  (setq selLayIdx (atoi (get_tile \"layer_sel\")))"
            "  (if (= selLayIdx " (itoa lastLayIdx) ")"
            "    (setq *geo:layer* (get_tile \"layer_new\"))"
            "    (setq *geo:layer* (nth selLayIdx *geo:layers-list*)))"
            "  (setq *geo:side-closed* (nth (atoi (get_tile \"side_closed\")) (list \"to_point\" \"inside\" \"outside\")))"
            "  (setq *geo:side-open*   (nth (atoi (get_tile \"side_open\"))   (list \"to_point\" \"left\" \"right\")))"
            "  (setq *geo:opt-onepoint* (get_tile \"opt_onepoint\"))"
            "  (setq *geo:opt-short*    (get_tile \"opt_short\"))"
            "  (setq *geo:opt-leader*   (get_tile \"opt_leader\"))"
            "  (setq *geo:opt-fixpos*   (get_tile \"opt_fixpos\"))"
            "  (done_dialog 1))"
          )
        )
        (action_tile "cancel" "(done_dialog 0)")

        (setq what_next (start_dialog))
        (if (= what_next 0) (princ "\n[GeoOffset] ��������."))
      )
    )
  )
  (unload_dialog dcl_id)

  ;; ================================================================
  ;; ����� OK
  ;; ================================================================
  (setq doProcess T)

  (if (and (= what_next 1) doProcess)
    (progn
      (princ "\n\n======= ��������� ������� =======")
      (setq fResult (geo:build-filter-from-sample "\n������� ������� ���������� �������: "))
      (if (null fResult)
        (progn (princ "\n[GeoOffset] ������� �� ������. ������.") (setq doProcess nil)))))

  (if (and (= what_next 1) doProcess)
    (progn
      (setq *geo:proj-filter* (car fResult) *geo:proj-info* (cadr fResult))
      (princ (strcat "\n�������� ��������� ������� [" *geo:proj-info* "]: "))
      (setq ssProj (ssget "_:L" *geo:proj-filter*))
      (if (and (null ssProj) (> (length *geo:proj-filter*) 2))
        (progn (princ "\n������ ��� �����...")
          (setq ssProj (ssget "_:L" (list (car *geo:proj-filter*) (cadr *geo:proj-filter*))))))
      (if (null ssProj)
        (progn (princ "\n[GeoOffset] ��������� �� �������.") (setq doProcess nil))
        (princ (strcat "\n������� ���������: " (itoa (sslength ssProj)))))))

  (if (and (= what_next 1) doProcess)
    (progn
      (princ "\n\n======= ����������� ����� =======")
      (setq fResult (geo:build-filter-from-sample "\n������� ������� ����������� �����: "))
      (if (null fResult)
        (progn (princ "\n[GeoOffset] ������� �� ������. ������.") (setq doProcess nil)))))

  (if (and (= what_next 1) doProcess)
    (progn
      (setq *geo:fact-filter* (car fResult) *geo:fact-info* (cadr fResult))
      (princ (strcat "\n�������� ����������� ����� [" *geo:fact-info* "]: "))
      (setq ssFact (ssget "_:L" *geo:fact-filter*))
      (if (and (null ssFact) (> (length *geo:fact-filter*) 2))
        (progn (princ "\n������ ��� �����...")
          (setq ssFact (ssget "_:L" (list (car *geo:fact-filter*) (cadr *geo:fact-filter*))))))
      (if (null ssFact)
        (progn (princ "\n[GeoOffset] ����� �� �������.") (setq doProcess nil))
        (princ (strcat "\n������� �����������: " (itoa (sslength ssFact)))))))

  ;; ================================================================
  ;; ���������
  ;; [FIX v3.0] ������: bestDist ������������ � tol-plan �� * k
  ;; ================================================================
  (if (and (= what_next 1) doProcess ssProj ssFact)
    (progn
      (geo:ensure-layer *geo:layer* *geo:color*)
      (setq projEnts  (geo:ss->list ssProj)
            factEnts  (geo:ss->list ssFact)
            totalPts  (length factEnts)
            errCount  0 okCount 0)
      (if (or (null *geo:coeff*) (<= *geo:coeff* 0))
        (progn (princ "\n[GeoOffset] k<=0, ���������� k=1.0")
               (setq *geo:coeff* 1.0)))
      (setq old-osmode (getvar "OSMODE"))
      (setvar "OSMODE" 0)
      (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))

      (princ (strcat "\n\n  ������� �����: " (rtos (geo:get-block-scale) 2 2)
                     " (DIMSCALE=" (rtos (getvar "DIMSCALE") 2 1) ")"))
      (princ (strcat "\n  k= " (rtos *geo:coeff* 2 0)))
      (princ (strcat "\n  ������: " (rtos *geo:tol-plan* 2 4)
                     " ��. ������� (��������� �� k)\n"))

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
                ;; dist = �������� � �������� �������� (��� �����������)
                (setq dist (* bestDist *geo:coeff*))
                (setq sign (geo:calc-sign bestEnt bestFoot pt))
                (setq rawVal (rtos dist 2 *geo:precision*))
                (cond
                  ((= sign "+")
                   (if (= *geo:prefix* " ")
                     (setq distStr rawVal)
                     (setq distStr (strcat *geo:prefix* rawVal))))
                  (T (setq distStr (strcat sign rawVal))))
                ;; ������� �����
                (setq blkObj (geo:insert-arrow bestEnt bestFoot pt distStr rawVal *geo:layer*))
                ;; [FIX v3.0] ����: ������ ������������ �� ��������� �� k
                ;; bestDist � ���������� � �������� �������
                ;; *geo:tol-plan* � ������ � �������� �������
                ;; ������: ����� � �, bestDist=0.03 �, ������=0.05 � > 0.03 ? 0.05 > ������
                (if (= *geo:tol-plan-on* "1")
                  (if (<= bestDist *geo:tol-plan*)
                    (vla-put-Color blkObj (fix *geo:color-ok*))     ; ������
                    (vla-put-Color blkObj (fix *geo:color-fail*)))  ; �����
                  (vla-put-Color blkObj (fix *geo:color*)))
                (setq okCount (1+ okCount))
                (princ (strcat "\n  [" (itoa (1+ i)) "/" (itoa totalPts) "] "
                               distStr " (d=" (rtos bestDist 2 4) ")"
                               (if (= *geo:tol-plan-on* "1")
                                 (if (<= bestDist *geo:tol-plan*) " OK" " !!!")
                                 ""))))
              (progn (setq errCount (1+ errCount))
                (princ (strcat "\n  [" (itoa (1+ i)) "] ������"))))))
        (setq i (1+ i)))

      (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setvar "OSMODE" old-osmode)

      (princ "\n\n========================================")
      (princ (strcat "\n  ����������: " (itoa okCount)))
      (if (> errCount 0) (princ (strcat "\n  ������: " (itoa errCount))))
      (princ "\n========================================")
    )
  )
  (princ)
)

(princ "\n[GeoOffset v3.4] ��������. �������: GeoOffset")
(princ)
