
(in-package cl-tui)

(defmacro defun/frame (name type (&rest args) &body body)
  (let ((frame-var (car args)))
    `(defun ,name ,args
       (setf ,frame-var (frame ,frame-var))
       (check-type ,frame-var ,type)
       ,@body)))

(defun/frame put-text canvas-frame (frame y x str &rest format-args)
  (with-slots (window) frame
    (ensure-ok (charms/ll:mvwaddstr window y x (apply #'format nil str format-args)))))

(defun/frame put-char canvas-frame (frame y x c)
  (with-slots (window) frame
    (ensure-ok (charms/ll:mvwaddch window y x (char-code c)))))

(defun/frame draw-box canvas-frame (frame)
  (with-slots (window) frame
    (ensure-ok (charms/ll:box window 0 0))))

;;; Container frame

(defun/frame set-split-type container-frame (frame split-type)
  (setf (slot-value frame 'split-type) split-type))

;;; Log frame

(defun/frame scroll-log log-frame (frame offset)
  (setf (slot-value frame 'offset)
        (alexandria:clamp (+ (slot-value frame 'offset) offset) 0 (1- (length (slot-value frame 'lines))))))

(defun/frame append-line log-frame (frame obj &rest init-args)
  (with-slots (lines deduplicate-lines) frame
    (let ((last-obj (car lines))
          (new-obj (if (stringp obj)
                       (apply 'format nil obj init-args)
                       (apply 'make-instance init-args))))
      (if (and last-obj
               deduplicate-lines
               (stringp obj)
               (string= new-obj (log-line-line last-obj)))
          (incf (log-line-count last-obj))
          (push (make-log-line :line new-obj) lines))
      new-obj)))

;;; Common stuff

(defun/frame clear frame (frame)
  (etypecase frame
    (log-frame (setf (slot-value frame 'lines) nil)
               (setf (slot-value frame 'offset) 0))
    (canvas-frame (charms/ll:wclear (slot-value frame 'window)))))

;;; Tabs manupulations

(defun/frame tabs-list tabbed-frame (frame)
  (mapcar #'car (slot-value frame 'children)))

(defun/frame tab-forward tabbed-frame (frame &optional (n 1))
  (with-slots (children current-frame current-frame-position) frame
    (setf current-frame-position (mod (+ current-frame-position n)
                                      (length children))
          current-frame          (car (nth current-frame-position
                                           children)))))

(defun/frame tab-backwards tabbed-frame (frame &optional (n 1))
  (tab-forward frame (- n)))

(defun/frame draw-tab-bar tabbed-frame (frame &key (left-padding 2)
                                                   (right-padding 2)
                                                   (top-padding 2)
                                                   (tab-padding 2))
  (let* ((tabs-list (tabs-list frame))
         (tabs-list-length (length tabs-list))
         (tab-bar-max-width (- (slot-value frame 'w)
                               left-padding
                               right-padding)))
    (labels
      ((nth-tab (nth)
         (nth (mod nth tabs-list-length)
              tabs-list))
       (get-neighbouring-tabs (tab-n)
         (let* ((shift 0)
                (tab (symbol-name (nth-tab (+ shift tab-n))))
                (length-acc (length tab))
                (i 0)
                result)
           (loop while (and (<= length-acc tab-bar-max-width)
                            (< i tabs-list-length))
                 finally (return (values result length-acc))
                 do
                 (incf length-acc (length tab))
                 (incf i)
                 (setf tab    (symbol-name (nth-tab (+ shift tab-n)))
                       result (if (negative-fixnum-p shift)
                                (list* tab result)
                                (append result (list tab)))
                       shift  (cond
                                ((zerop shift)
                                 1)
                                ((negative-fixnum-p shift)
                                 (1+ (abs shift)))
                                (t
                                 (- shift))))))))
      (multiple-value-bind (tabs length)
          (get-neighbouring-tabs (slot-value frame 'current-frame-position))
          (let ((current-frame (slot-value frame 'current-frame)))
        (with-slots (window) (frame current-frame)
          (charms/ll:wmove window
                           top-padding
                           (+ left-padding
                              (- (truncate (slot-value frame 'w) 2)
                                 (truncate length 2))))
          (with-attributes (:bold) current-frame
            (dolist (tab tabs)
              (if (string= tab current-frame)
                (with-attributes (:standout) current-frame
                  (charms/ll:wprintw window tab))
                (charms/ll:wprintw window tab))
              (charms/ll:wprintw window (format nil "~vT" tab-padding))))))))))

;;; Edit frame

(defun/frame get-text edit-frame (frame)
  (vedit:text (slot-value frame 'vedit)))

(defun/frame clear-text edit-frame (frame)
  (vedit:clear (slot-value frame 'vedit)))

(defun/frame handle-key edit-frame (frame key)
  (vedit:handle-key (slot-value frame 'vedit) key))
