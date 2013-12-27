
(in-package cl-tui)

(defmacro defun/frame (name type (&rest args) &body body)
  (let ((frame-var (car args)))
    `(defun ,name ,args
       (setf ,frame-var (frame ,frame-var))
       (check-type ,frame-var ,type)
       (ensure-window ,frame-var)
       ,@body)))

(defun/frame put-text canvas-frame (frame y x str)
  (with-slots (window) frame
    (cl-charms:mvwaddstr window y x str)))

(defun/frame put-char canvas-frame (frame x y c)
  (with-slots (window) frame
    (cl-charms:mvwaddstr window y x (string c))))

;;; Text frame-specific

;;; WRITE-LINE and APPEND-LINE should be the main interface
(defun/frame add-text text-frame (frame new-text)
  (with-slots (text) frame
    (setf text (concatenate 'string text new-text))))

(defun/frame clear frame (frame)
  (etypecase frame
    (text-frame (setf (slot-value frame 'text) ""))
    (canvas-frame (cl-charms:wclear (slot-value frame 'window)))))
