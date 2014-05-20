
(in-package cl-tui)

;;; Common stuff

;;;; FRAME TYPES

;;; Dummy frame for holding other frames

(defclass container-frame (frame)
  ((children :initform nil
             :documentation "Alist of frame names and any placement arguments.")
   (split-type :type (member :none :vertical :horizontal)
               :initform :vertical)))

(defmethod add-child ((frame container-frame) child &rest placement)
  (with-slots (children) frame
    (deletef children child :key #'car)
    (push (list* child placement) children)))

(defmethod remove-child ((frame container-frame) child)
  (with-slots (children) frame
    (setf (slot-value (frame child) 'parent) nil)
    (deletef children child :key #'car)))

(defmethod calculate-layout ((frame container-frame))
  (with-slots (h w y x children split-type) frame
    (cond ((null children)
           nil)
          ((= 1 (length children))
           (show-window (caar children) h w y x)
           (calculate-layout (caar children)))
          (t
           (let ((limit (ecase split-type
                          (:vertical h)
                          (:horizontal w))))
             (loop
                with step = (truncate limit (length children))
                for child in children
                for shift from 0 upto limit by step
                doing
                  (let ((x (+ x (case split-type
                                  (:vertical 0)
                                  (:horizontal shift))))
                        (y (+ y (case split-type
                                  (:vertical shift)
                                  (:horizontal 0))))
                        (h (case split-type
                             (:vertical step)
                             (:horizontal h)))
                        (w (case split-type
                             (:vertical w)
                             (:horizontal step))))
                    (show-window (car child) h w y x)
                    (calculate-layout (car child)))))))))

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
                     :h (cl-charms:getmaxy (slot-value frame 'window))
                     :w (cl-charms:getmaxx (slot-value frame 'window))
                     :allow-other-keys t)))))

(defmethod render-self ((frame callback-frame))
  (with-slots (render window) frame
    (cl-charms:werase window)
    (when render
      (funcall render))))

;;; Log frame

(defclass log-frame (frame)
  ((text :type list
         :initform nil)
   (line-render :type function
                :initarg :line-render
                :initform #'log-default-line-render)))

(defmethod frame-drawable-p ((frame log-frame))
  t)

(defstruct log-line
  (text "" :type string)
  (ts (get-universal-time) :type integer)
  (count 1 :type fixnum)
  (attrs nil :type list))

(defun log-default-line-render (text &key ts count)
  (let+ (((:values sec min hour) (decode-universal-time ts)))
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
           :do (cl-charms:mvwaddstr window (- h line offset) 0 text-line)))
      (length split-lines))))

(defmethod render-self ((frame log-frame))
  (with-slots (window text h) frame
    (cl-charms:werase window)
    (let ((i 0))
      (dolist (line text)
        (incf i (put-log-line frame line i))
        (when (>= i h)
          (return-from render-self))))))
