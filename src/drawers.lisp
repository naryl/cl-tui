
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
    (canvas-frame (cl-charms:wclear (slot-value (slot-value frame 'window) 'window)))))

;;; Common stuff

(defun get-attribute-name-from-keyword (attribute)
  "Converts keyword to ncurses attribute."
  (cond ((keywordp attribute) ; Simple attribute
         (ecase attribute
           (:normal cl-charms:a_normal)
           (:standout cl-charms:a_standout)
           (:underline cl-charms:a_underline)
           (:reverse cl-charms:a_reverse)
           (:blink cl-charms:a_blink)
           (:dim cl-charms:a_dim)
           (:bold cl-charms:a_bold)
           (:protect cl-charms:a_protect)
           (:invis cl-charms:a_invis)
           (:altcharset cl-charms:a_altcharset)))
        ((and (listp attribute) ; Color
              (eq (first attribute) :color)
              (typep (second attribute) 'color-pair))
         (cl-charms:color-pair (ensure-color-pair (second attribute))))
        (t (error "Unknown attribute ~S" attribute))))

(defun attributes-to-code (&rest attributes)
  (let ((attribute-codes (mapcar #'get-attribute-name-from-keyword attributes)))
    (apply #'logior attribute-codes)))

(defmacro with-attributes ((&body attributes) frame &body body)
  "Enables given attributes, executes body and then ensures
they're disabled."
  (with-gensyms (attributes-code)
    `(let ((,attributes-code (attributes-to-code
                              ,@(mapcar (lambda (attr)
                                          (typecase attr
                                            (symbol attr)
                                            (list `(list ,@attr))))
                                        attributes))))
       (unwind-protect
            (progn
              (cl-charms:wattron (slot-value (frame ,frame) 'window) ,attributes-code)
              ,@body)
         (cl-charms:wattroff (slot-value (frame ,frame) 'window) ,attributes-code)))))

;;;; Colors

(defvar *used-color-pairs* -1 "ID of the last used color-pair -1 if none")
(defvar *used-colors* -1 "ID of the last used color -1 if none")

(defclass color ()
    ((r :initarg :r :type (integer 0 1000))
     (g :initarg :g :type (integer 0 1000))
     (b :initarg :b :type (integer 0 1000))
     (id :initform nil :type (or null fixnum))))

(defclass color-pair ()
  ((fg :initarg :fg :type fixnum)
   (bg :initarg :bg :type fixnum)
   (id :initform nil :type (or null fixnum))))

(defvar *color-pairs* (make-hash-table :test #'equal))
(defvar *colors* (make-hash-table :test #'equal))

(defun color (r g b)
  (or (gethash (list r g b) *colors*)
      (setf (gethash (list r g b) *colors*)
            (make-instance 'color :r r :g g :b b))))

(defun color-pair (fg bg)
  (or (gethash (list fg bg) *color-pairs*)
      (setf (gethash (list fg bg) *color-pairs*)
            (make-instance 'color-pair :fg fg :bg bg))))

(defun clear-colors ()
  (setf *used-color-pairs* -1
        *used-colors* -1)
  (dolist (obj (append *colors* *color-pairs*))
    (setf (slot-value obj 'id) nil)))

(defun ensure-color (color)
  (with-slots (id r g b) color
    (unless id
      (setf id (incf *used-colors*))
      (cl-charms:init-color id r g b))
    id))

(defun ensure-color-pair (pair)
  (with-slots (id fg bg) pair
    (unless id
      (setf id (incf *used-colors*))
      (let ((colors (mapcar #'ensure-color (list fg bg))))
        (apply #'cl-charms:init-pair id colors)))
    id))
