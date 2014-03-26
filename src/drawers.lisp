
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

;;; Common stuff

(defun get-attribute-name-from-keyword (attribute)
  "Converts keyword to ncurses attribute."
  (cond ((keywordp attribute)
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
        ((and (listp attribute)
              (eq (first attribute) :color)
              (keywordp (second attribute)))
         (aif (color-pair-by-name (second attribute))
              (cl-charms:color-pair it)
              (error "Unknown color ~S" (second attribute))))
        (t (error "Unknown attribute ~S" attribute))))

(defmacro with-attributes ((&body attributes) frame &body body)
  "Enables given attributes, executes body and then ensures
they're disabled."
  (let ((attribute-names (apply #'logior
                                (mapcar #'get-attribute-name-from-keyword attributes))))
    `(unwind-protect
          (progn
            (cl-charms:wattron (slot-value (frame ,frame) 'window),attribute-names)
            ,@body)
       (cl-charms:wattroff (slot-value (frame ,frame) 'window) ,attribute-names))))

(defvar *color-pairs* nil)

(defun color-pair-by-name (name)
  (1+ (position name *color-pairs* :key #'first)))

(defun color-from-keyword (color)
  (ecase color
    (:black cl-charms:COLOR_BLACK)
    (:red cl-charms:COLOR_RED)
    (:green cl-charms:COLOR_GREEN)
    (:yellow cl-charms:COLOR_YELLOW)
    (:blue cl-charms:COLOR_BLUE)
    (:magenta cl-charms:COLOR_MAGENTA)
    (:cyan cl-charms:COLOR_CYAN)
    (:white cl-charms:COLOR_WHITE)))

(defun defcolor (name fg bg)
  (let ((pair (assoc name *color-pairs*)))
    (if pair
        (setf (cdr pair) (list fg bg))
        (appendf *color-pairs* (list (list name fg bg)))))
  (when *running*
    (let ((id (color-pair-by-name name)))
      (let ((result (cl-charms:init-pair id (color-from-keyword fg)
                                         (color-from-keyword bg))))
        (unless (zerop result)
          (error "Error while initializing color pair: ~S~%Check if INIT-SCREEN is called with :colors attribute" result))))))

(defun init-color-pairs ()
  (loop
     :for i from 1
     :for color :in *color-pairs*
     :do (apply #'cl-charms:init-pair i
                (mapcar #'color-from-keyword (cdr color)))))
