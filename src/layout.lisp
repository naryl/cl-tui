
(in-package cl-tui)

#|
Layout is a list of lists of lists ad infinitum.
Each list represents a horizontal/vertical row layout with odd levels being horizontal and
even levels being vertical. Leaves are frames.
Each element is a struct containing list/frame and layouting data (min-size, max-size and weight)
|#

(defstruct layout
  (rows nil)
  (cols nil)
  (cells nil))

(defstruct layout-row
  (min-size 0)
  (max-size 100500)
  (weight 1)
  (size 0))

(defstruct layout-col
  (min-size 0)
  (max-size 100500)
  (weight 1)
  (size 0))

(defstruct layout-cell
  frame left right top bottom)

(defmacro do-cells ((frame left right top bottom) layout &body body)
  (once-only (layout)
    (with-gensyms (cell)
      `(dolist (,cell (layout-cells ,layout))
         (let ((,frame (layout-cell-frame ,cell))
               (,left (layout-cell-left ,cell))
               (,right (layout-cell-right ,cell))
               (,top (layout-cell-top ,cell))
               (,bottom (layout-cell-bottom ,cell)))
           ,@body)))))

(defun layout-insert (layout frame-name)
  (let ((cell (make-layout-cell :frame frame-name
                                :left 0
                                :right 0
                                :top 0
                                :bottom 0)))
    (setf (layout-rows layout) (list (make-layout-row))
          (layout-cols layout) (list (make-layout-col))
          (layout-cells layout) (list cell))))

(defun layout-remove (layout frame)
  (setf (layout-rows layout) nil
        (layout-cols layout) nil
        (layout-cells layout) nil))

(defun recalculate-layout (layout width height)
  "Recalculate rows and cols min- and max-size and weight. Then their size."
  (when (and (layout-cols layout)
             (layout-rows layout))
    (setf (layout-col-size (first (layout-cols layout))) width
          (layout-row-size (first (layout-rows layout))) height)))

#|
(defun pack (frame place anchor &key (min-size 0) (max-size 100500) (weight 1))
  "Place frame in layout relative to anchor"
  (let ((anchor (find anchor (layout-cells *layout*) :key #'layout-cell-frame))
        (new-left (layout-cell-left anchor))
        (new-right (layout-cell-right anchor))
        (new-top (layout-cell-top anchor))
        (new-bottom (layout-cell-bottom anchor)))
    (case place
      (:left-of (setf new-left (layout-cell-left anchor))
                (setf new-right (layout-cell-left anchor)))
      (:right-of (setf new-left (1+ (layout-cell-right anchor)))
                 (setf new-right (1+ (layout-cell-right anchor))))
      (:above (setf new-top (layout-cell-top anchor))
              (setf new-bottom (layout-cell-top anchor)))
      (:below (setf new-top (1+ (layout-cell-bottom anchor)))
              (setf new-bottom (1+ (layout-cell-bottom anchor))))))
  ;; TODO: Fix rows, columns and other frames' placement here
  (push (make-layout-cell :frame frame
                          :left new-left
                          :right new-right
                          :top new-top
                          :bottom new-bottom
                          :min-size)
        (layout-cells *layout*)))

|#
