
(in-package cl-tui)

(defmacro defun/frame (name type (&rest args) &body body)
  (let ((frame-var (car args)))
    `(defun ,name ,args
       (setf ,frame-var (frame ,frame-var))
       (check-type ,frame-var ,type)
       (ensure-window ,frame-var)
       ,@body)))

(defun/frame putchar canvas-frame (frame x y c)
  (with-slots (window) frame
    (cl-charms:mvwaddstr window y x (string c))))

;;; Text frame-specific

(defun/frame add-text text-frame (frame new-text)
  (with-slots (text) frame
    (setf text (concatenate 'string text new-text))))

(defun/frame clear text-frame (frame)
  (setf (slot-value frame 'text) ""))
