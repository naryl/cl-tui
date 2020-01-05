
(in-package cl-tui)

;;; Common stuff

;;;; FRAME TYPES

;;; Dummy frame for holding other frames

(defclass container-frame (frame)
  ((children :initform nil
             :documentation "Alist of frame names and any placement arguments.")
   (split-type :type (member :none :vertical :horizontal)
               :initarg :split-type
               :initform :vertical)))

(defmethod add-child ((frame container-frame) child &rest placement)
  (with-slots (children) frame
    (deletef children child :key #'car)
    (appendf children (list (list* child placement)))))

(defmethod remove-child ((frame container-frame) child)
  (with-slots (children) frame
    (setf (slot-value (frame child) 'parent) nil)
    (deletef children child :key #'car)))

(defmethod render-children ((frame container-frame))
  (mapcar (compose #'render-frame #'car)
          (slot-value frame 'children)))

;;; Canvas frame superclass (for frames allowed to use simple drawing functions)

(defclass canvas-frame (frame) ())

(defmethod frame-drawable-p ((frame canvas-frame))
  t)

;;; Retained frame

(defclass retained-frame (canvas-frame)
  ())

;;; Callback frame

(defclass callback-frame (canvas-frame)
  ((render :type function
           :initform nil)))

(defmethod initialize-instance ((frame callback-frame) &key name render &allow-other-keys)
  (call-next-method)
  (when render
    (setf (slot-value frame 'render)
          (lambda ()
            (funcall render
                     :frame name
                     :h (charms/ll:getmaxy (slot-value frame 'window))
                     :w (charms/ll:getmaxx (slot-value frame 'window))
                     :allow-other-keys t)))))

(defmethod render-self ((frame callback-frame))
  (with-slots (render window) frame
    (charms/ll:werase window)
    (when render
      (funcall render))))

;;; Log frame

(defclass log-frame (frame)
  ((text :type list
         :initform nil)
   (line-render :type function
                :initarg :line-render
                :initform #'log-default-line-render)
   (deduplicate-lines :type boolean
                      :initarg :deduplicate-lines
                      :initform nil)))

(defmethod frame-drawable-p ((frame log-frame))
  t)

(defstruct log-line
  (text "" :type string)
  (ts (get-universal-time) :type integer)
  (count 1 :type fixnum)
  (attrs nil :type list))

(defun log-default-line-render (text &key ts count)
  (multiple-value-bind (sec min hour)
      (decode-universal-time ts)
    (format nil "~2,'0D:~2,'0D:~2,'0D ~A~A"
            hour min sec
            text
            (if (> count 1)
                (format nil " x~A" count)
                ""))))

(defun split-line (text width)
  (if (<= (length text) width)
      (list text)
      (let ((words (split-sequence #\Space text))
            (result nil)
            (current-line ""))
        (dolist (next-word words)
          (when (and (> (+ (length current-line) (length next-word) 1)
                        width)
                     (not (and (> (length next-word)) width
                               (string= current-line ""))))
            (push current-line result)
            (setf current-line ""))
          (setf current-line (concatenate 'string
                                          current-line
                                          (if (string= current-line "") "" " ")
                                          next-word)))
        (unless (string= current-line "")
          (push current-line result))
        (mapcan (lambda (line)
                  (if (> (length line) width)
                      (loop
                         :with length := (length line)
                         :for start :from 0 :by width :below length
                         :collecting (subseq line start (min length (+ start width))))
                      (list line)))
                (nreverse result)))))

(defun put-log-line (frame text line)
  (with-slots (w h line-render window) frame
    (let* ((rendered-text (funcall (slot-value frame 'line-render)
                                  (log-line-text text)
                                  :ts (log-line-ts text)
                                  :count (log-line-count text)
                                  :allow-other-keys t))
           (split-lines (split-line rendered-text w)))
      (with-processed-attributes (log-line-attrs text) frame
        (loop :for offset :from (length split-lines) :downto 1
           :for text-line :in split-lines
           :do (charms/ll:mvwaddstr window (- h line offset) 0 text-line)))
      (length split-lines))))

(defmethod render-self ((frame log-frame))
  (with-slots (window text h) frame
    (charms/ll:werase window)
    (let ((i 0))
      (dolist (line text)
        (incf i (put-log-line frame line i))
        (when (>= i h)
          (return-from render-self))))))

;;; Tabbed frame

(defclass tabbed-frame (container-frame)
  ((current-frame-position :initform 0
                           :initarg :initial-tab
                           :documentation "Position of currently drawn children."
                  ;; Position was chosed to simplify tab switch operation.
                  ;; Now it can be done via simple modular addition.
                  )
   (current-frame :initform nil
                  :documentation "Stores reference to currently displayed frame.")))

(defmethod render-children ((frame tabbed-frame))
  (unless (slot-value frame 'current-frame)
    (with-slots (children current-frame current-frame-position) frame
      (setf current-frame (car (nth (mod current-frame-position
                                         (max 1 (length children)))
                                    children)))))
  (awhen (slot-value frame 'current-frame)
    (render-frame it)))

(defmethod calculate-layout ((frame tabbed-frame))
  (dolist (child (slot-value frame 'children))
    (with-slots (x y w h) (frame (car child))
      (setf x (slot-value frame 'x)
            y (slot-value frame 'y)
            w (slot-value frame 'w)
            h (slot-value frame 'h))
      (show-window (car child) h w y x)
      (calculate-layout (car child)))))

;;; Edit frame

(defclass edit-frame (frame)
  ((prompt :initform "" :initarg :prompt :writer set-prompt)
   (vedit :initform (vedit:make-vedit))))

(defmethod frame-drawable-p ((frame edit-frame))
  t)

(defmethod render-self ((frame edit-frame))
  (with-slots (window prompt vedit w) frame
    (let* ((text (vedit:text vedit))
           (point (vedit:point vedit)))
      (ensure-ok (charms/ll:mvwaddstr window 0 0 (format nil "~A~A" prompt text)))
      
      (let* ((filler-size (- w (charms/ll:getcurx window) 1))
             (filler (make-string filler-size :initial-element #\Space)))
        (ensure-ok (charms/ll:waddstr window filler)))
      (ensure-ok (charms/ll:mvwchgat window 0 (+ (length prompt) point)
                                     1 charms/ll:a_reverse 0 (cffi:null-pointer))))))

