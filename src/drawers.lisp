
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
              (integerp (second attribute)))
         (let ((color (second attribute)))
           (cond ((keywordp color) ; Default color
                  (ecase color
                    (:black cl-charms:COLOR_BLACK)
                    (:red cl-charms:COLOR_RED)
                    (:green cl-charms:COLOR_GREEN)
                    (:yellow cl-charms:COLOR_YELLOW)
                    (:blue cl-charms:COLOR_BLUE)
                    (:magenta cl-charms:COLOR_MAGENTA)
                    (:cyan cl-charms:COLOR_CYAN)
                    (:white cl-charms:COLOR_WHITE)))
                 ((integerp color) ; Custom color
                  (cl-charms:color-pair color)))))
        (t (error "Unknown attribute ~S" attribute))))

(defmacro with-attributes ((&body attributes) frame &body body)
  "Enables given attributes, executes body and then ensures
they're disabled."
  (with-gensyms (attribute-values attribute-codes attributes-sum)
    `(let* ((,attribute-values (list ,@(mapcar (lambda (attr)
                                                 (typecase attr
                                                   (symbol attr)
                                                   (list `(list ,@attr))))
                                               attributes)))
            (,attribute-codes (mapcar #'get-attribute-name-from-keyword ,attribute-values))
            (,attributes-sum (apply #'logior ,attribute-codes)))
       (unwind-protect
            (progn
              (cl-charms:wattron (slot-value (frame ,frame) 'window) ,attributes-sum)
              ,@body)
         (cl-charms:wattroff (slot-value (frame ,frame) 'window) ,attributes-sum)))))

(defvar *used-color-pairs* nil "Sorted list of used color pairs")
(defvar *used-colors* nil "Sorted list of used colors")

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro take-id (var)
    `(let ((id (loop
                  :for tail :on ,var
                  :do (when (null (cdr tail))
                        (return (1+ (first tail))))
                  :do (when (/= 1 (- (second tail) (first tail)))
                        (return (1+ (first tail)))))))
       (push id ,var)
       (setf ,var (sort ,var #'<))
       id)))

(defun make-color (r g b)
  (let ((free-id (take-id *used-colors*)))
    (when-running
      (unless (zerop (cl-charms:init-color free-id r g b))
        (error "Looks like you've reached the limit of ~S colors" free-id)))
    free-id))

(defun free-color (color)
  (if (< color 8)
      (error "Can't delete a default color")
      (deletef *used-colors* color))
  nil)

(defun make-color-pair (fg bg)
  (let ((free-id (take-id *used-color-pairs*)))
    (when-running
      (unless (zerop (cl-charms:init-pair free-id fg bg))
        (error "Looks like you've reached the limit of ~S color pairs" free-id)))
    free-id))

(defun free-color-pair (pair)
  (if (zerop pair)
      (error "Can't free the default color pair")
      (deletef *used-color-pairs* pair))
  nil)
