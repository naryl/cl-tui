
(in-package cl-tui)

(defmacro defun/frame (name type (&rest args) &body body)
  (let ((frame-var (car args)))
    `(defun ,name ,args
       (setf ,frame-var (frame ,frame-var))
       (check-type ,frame-var ,type)
       ,@body)))

(defun/frame put-text canvas-frame (frame y x str &rest format-args)
  (with-slots (window) frame
    (ensure-ok (cl-charms:mvwaddstr window y x (apply #'format nil str format-args)))))

(defun/frame put-char canvas-frame (frame y x c)
  (with-slots (window) frame
    (ensure-ok (cl-charms:mvwaddch window y x (char-code c)))))

(defun/frame draw-box canvas-frame (frame)
  (with-slots (window) frame
    (ensure-ok (cl-charms:box window 0 0))))

;;; Log frame-specific

(defun/frame append-line log-frame (frame str &rest format-args)
  (with-slots (text) frame
    (let ((last-line (car text))
          (new-line (apply #'format nil str format-args)))
      (if (and last-line
               (string= new-line (log-line-text last-line)))
          (incf (log-line-count last-line))
          (push (make-log-line :text new-line :attrs *current-attributes*) text))
      nil)))

;;; Common stuff

(defun/frame clear frame (frame)
  (etypecase frame
    (log-frame (setf (slot-value frame 'text) nil))
    (canvas-frame (cl-charms:wclear (slot-value (slot-value frame 'window) 'window)))))
