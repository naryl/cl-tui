
(in-package cl-tui)

;;; Container

(defun placement-width (placement)
  (getf placement :w nil))

(defun placement-height (placement)
  (getf placement :h nil))

(defun frame-free-p (placement)
  (not (or (getf placement :w nil)
           (getf placement :h nil))))

(defun calculate-split (children type max)
  (flet ((placement-size (placement)
           (case type
             (:vertical (placement-height placement))
             (:horizontal (placement-width placement))
             (t nil))))
    (let* ((free-frames-count (count-if (compose #'frame-free-p #'cdr)
                                        children))
           (reserved-size (reduce #'+ (mapcar (lambda (child)
                                                (or (placement-size (cdr child))
                                                    0))
                                              children)))
           (extra-space (- max reserved-size))
           (per-free (truncate extra-space free-frames-count))
           (extra-extra-space (- extra-space (* per-free free-frames-count)))
           (first-free-frame t))
      (loop :for i :from 0
         :for (child . placement) :in children
         :collect (acond ((placement-size placement)
                          it)
                         (first-free-frame
                          (setf first-free-frame nil)
                          (+ per-free extra-extra-space))
                         (t per-free))))))

(defun perform-container-layout (children ys xs hs ws)
  (loop :for child-h :in hs
     :for child-w :in ws
     :for child-x :in xs
     :for child-y :in ys
     :for (child . placement) in children
     :do (show-window child child-h child-w child-y child-x)
     :do (calculate-layout child)))

(defun running-sum (list)
  (let ((sums (list 0)))
    (loop :for item :in list
       :for sum := item :then (+ sum item)
       :do (push sum sums))
    (reverse (cdr sums))))

(defmethod calculate-layout ((frame container-frame))
  (with-slots (h w y x children split-type) frame
    (flet ((make-layout-list (element)
             (make-list (length children) :initial-element element)))
      (cond
        ((null children)
         nil)
        ((or (= 1 (length children))
             (eql split-type :none))
         (show-window (caar children) h w y x)
         (calculate-layout (caar children)))
        (t
         (let* ((limit (ecase split-type
                         (:horizontal w)
                         (:vertical h)))
                (splits (calculate-split children split-type limit))
                (fixeds (ecase split-type
                          (:horizontal (make-layout-list h))
                          (:vertical (make-layout-list w))))
                (xs (ecase split-type
                      (:horizontal (running-sum splits))
                      (:vertical (make-layout-list x))))
                (ys (ecase split-type
                      (:horizontal (make-layout-list y))
                      (:vertical (running-sum splits)))))
           (ecase split-type
             (:horizontal (perform-container-layout children
                                                    ys xs fixeds splits))
             (:vertical (perform-container-layout children
                                                  ys xs splits fixeds)))))))))
