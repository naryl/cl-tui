
(in-package cl-tui)

(defmacro defun/frame (name type (&rest args) &body body)
  (let ((frame-var (car args)))
    `(defun ,name ,args
       (setf ,frame-var (frame ,frame-var))
       (check-type ,frame-var ,type)
       ,@body)))

(defun/frame put-text canvas-frame (frame y x str)
  (with-slots (window) frame
    (cl-charms:mvwaddstr window y x str)))

(defun/frame put-char canvas-frame (frame x y c)
  (with-slots (window) frame
    (cl-charms:mvwaddstr window y x (string c))))

;;; Text frame-specific

(defun/frame append-line text-frame (frame new-line)
  (with-slots (text) frame
    (setf text (append text (list new-line)))))

(defun/frame append-text text-frame (frame new-text)
  (with-slots (text) frame
    (let ((last-line (lastcar text)))
      (setf (lastcar text)
            (concatenate 'string last-line new-text)))))

(defun/frame clear frame (frame)
  (etypecase frame
    (text-frame (setf (slot-value frame 'text) nil))
    (canvas-frame (cl-charms:wclear (slot-value frame 'window)))))
