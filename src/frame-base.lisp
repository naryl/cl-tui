
(in-package cl-tui)

;;;; Frame

(defclass frame ()
  ((parent :type (or symbol null)
           :initarg :parent
           :initform nil)
   ;; Actual coordinates used by the layouter
   (h) (w) (y) (x)
   (window :initform nil
           :documentation "Ncurses window object. Created on demand.")))

(defmethod initialize-instance ((frame frame) &key &allow-other-keys)
  (call-next-method))

(defun hide-window (frame)
  (with-slots (window) (frame frame)
    (charms/ll:delwin window)
    (setf window nil)))

(defun show-window (frame nh nw ny nx)
  (unless (every #'non-negative-fixnum-p (list nh nw ny nx))
    (error "SHOW-WINDOW got negative size or position"))
  (let ((frame (frame frame)))
    (with-slots (h w y x window) frame
      (setf h nh
            w nw
            y ny
            x nx)
      (when (frame-drawable-p frame)
        (cond ((or (null window)
                   (cffi:null-pointer-p window))
               (setf window (charms/ll:newwin h w y x))
               (charms/ll:scrollok window 0))
              (t
               (ensure-ok (charms/ll:wresize window 1 1))
               (ensure-ok (charms/ll:mvwin window y x))
               (ensure-ok (charms/ll:wresize window h w))))))))

(defgeneric frame-drawable-p (frame)
  (:documentation "Returns whether instances of this frame can be drawed on. Otherwise no
  ncurses windows will be created")
  (:method ((frame frame))
    nil))

(defgeneric calculate-layout (frame)
  (:documentation "The frame should have actual X Y H and W when this generic is called.

  It sets the frame's children's coordinates and sizes to actual values and recursively
  calls itself on them.

  The default method does nothing and should be overloaded in container frames.")
  (:method ((name symbol))
    (calculate-layout (frame name)))
  (:method ((frame frame))
    nil))

(defun frame (name)
  (etypecase name
    (null nil)
    (frame name)
    (symbol (get name 'frame))))

(defsetf frame (name) (value)
  `(setf (get ,name 'frame) ,value))

(defmacro define-frame (name (type &rest frame-args)
                        &rest placement &key ((:on parent) nil)
                        &allow-other-keys)
  (remf placement :on)
  `(progn (setf (frame ',name)
                (make-instance ',type ,@frame-args
                               :name ',name
                               :parent ',parent))
          ,@(when parent
                  (list `(add-child ',parent ',name ,@placement)))
          (when (and *running*
                     (is-frame-displayed ',name))
            (resize))
          ',name))

(defmacro define-children (name () &body children)
  `(progn
     ,@(mapcar (lambda (child)
                 `(define-frame ,@child :on ,name))
               children)))

(defun destroy-frame (name)
  (let ((frame (frame name)))
    (awhen frame
      (with-slots (window children parent) it
        (when children
          (mapc #'destroy-frame children))
        (when window
          (hide-window name))
        (remprop name 'frame)
        t))))

(defgeneric add-child (parent child &rest placement)
  (:documentation "Add a child to a container frame")
  (:method ((parent symbol) child &rest placement)
    (unless child
      (error "Child is nil"))
    (apply #'add-child (frame parent) child placement)))

(defgeneric remove-child (parent child)
  (:documentation "Remove a child from a container frame")
  (:method ((parent symbol) child)
    (unless child
      (error "Child is nil"))
    (remove-child (frame parent) child)))

(defun get-screen-size ()
  "Returns size of terminal screen."
  (let (rows columns)
    (charms/ll:getmaxyx charms/ll:*stdscr* rows columns)
    (list rows columns)))

(defun frame-size (&optional frame)
  "Returns the frame (Y X) size in characters. Or NIL if it's unknown yet.
Default FRAME is the whole screen."
  (if (null frame)
    (get-screen-size)
    (list (slot-value (frame frame) 'h)
          (slot-value (frame frame) 'w))))

(defun is-frame-displayed (frame)
  (cond ((eq frame *display*)
         t)
        ((null frame)
         nil)
        (t
         (is-frame-displayed (slot-value (frame frame) 'parent)))))

(defun render-frame (frame)
  (render-self frame)
  (awhen (slot-value (frame frame) 'window)
    (charms/ll:wnoutrefresh it))
  (render-children frame))

(defun refresh (&optional (frame *display*))
  (when (not (is-frame-displayed frame))
    (cerror "Ignore" "Attempt to refresh a frame ~S which is not a child of current root ~S"
            frame *display*))
  (render-frame frame)
  (charms/ll:doupdate)
  (values))

(defgeneric render-self (frame)
  (:documentation "Displays the frame on screen.")
  (:method ((frame frame))
    nil)
  (:method ((name symbol))
    (render-self (frame name))))

(defgeneric render-children (frame)
  (:documentation "Render children for containers")
  (:method ((frame frame))
    nil)
  (:method ((name symbol))
    (render-children (frame name))))

(defun resize ()
  "Makes sure *DISPLAY* frame and all its children have proper place on the screen"
  (destructuring-bind (h w)
      (frame-size)
    (show-window *display* h w 0 0))
  (calculate-layout (frame *display*)))

;;; Common stuff

(defvar *current-attributes* nil)

(defun get-attribute-name-from-keyword (attribute)
  "Converts keyword to ncurses attribute."
  (cond ((keywordp attribute) ; Simple attribute
         (ecase attribute
           (:normal charms/ll:a_normal)
           (:standout charms/ll:a_standout)
           (:underline charms/ll:a_underline)
           (:reverse charms/ll:a_reverse)
           (:blink charms/ll:a_blink)
           (:dim charms/ll:a_dim)
           (:bold charms/ll:a_bold)
           (:protect charms/ll:a_protect)
           (:invis charms/ll:a_invis)
           (:altcharset charms/ll:a_altcharset)))
        ((and (listp attribute) ; Color
              (eq (first attribute) :color))
         (let ((pair (cond
                       ((and (= 2 (length attribute))
                             (typep (second attribute) 'color-pair))
                        (second attribute))
                       ((and (= 3 (length attribute))
                             (typep (second attribute) 'color)
                             (typep (third attribute) 'color))
                        (apply #'color-pair (cdr attribute))))))
           (charms/ll:color-pair (ensure-color-pair pair))))
        (t (error "Unknown attribute ~S" attribute))))

(defun attributes-to-code (attributes)
  (let ((attribute-codes (mapcar #'get-attribute-name-from-keyword attributes)))
    (apply #'logior attribute-codes)))

(defmacro with-attributes ((&body attributes) frame &body body)
  "Enables given attributes, executes body and then ensures
they're disabled."
  (with-gensyms (processed-attributes)
    `(let ((,processed-attributes
            (list ,@(mapcar (lambda (attr)
                              (typecase attr
                                (symbol attr)
                                (list `(list ,@attr))))
                            attributes))))
      (with-processed-attributes ,processed-attributes ,frame ,@body))))

(defmacro with-processed-attributes (processed-attributes frame &body body)
  (once-only (processed-attributes)
    (with-gensyms (attributes-code)
      `(let ((,attributes-code (attributes-to-code ,processed-attributes)))
         (unwind-protect
              (let ((*current-attributes* ,processed-attributes))
                (charms/ll:wattron (slot-value (frame ,frame) 'window) ,attributes-code)
                ,@body)
           (charms/ll:wattroff (slot-value (frame ,frame) 'window) ,attributes-code))))))

;;;; Colors

(defvar *used-color-pairs* 0 "ID of the last used color-pair 0 if none")
(defvar *used-colors* 0 "ID of the last used color 0 if none")

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
  (setf *used-color-pairs* 0
        *used-colors* 0)
  (dolist (obj (append (hash-table-values *colors*)
                       (hash-table-values *color-pairs*)))
    (setf (slot-value obj 'id) nil)))

(defun ensure-color (color)
  (with-slots (id r g b) color
    (unless id
      (setf id (incf *used-colors*))
      (charms/ll:init-color id r g b))
    id))

(defun ensure-color-pair (pair)
  (with-slots (id fg bg) pair
    (unless id
      (setf id (incf *used-colors*))
      (let ((colors (mapcar #'ensure-color (list fg bg))))
        (apply #'charms/ll:init-pair id colors)))
    id))
