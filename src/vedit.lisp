
(in-package vedit)

(defstruct (vedit (:constructor make-vedit%))
  point text completion-func handlers command)

(defmethod print-object ((obj vedit) out)
  (print-unreadable-object (obj out :type t)
    (format out "~S pt:~A len:~A" (text obj) (vedit-point obj) (size (vedit-text obj)))))

(defun make-vedit (&key
                     (point 0)
                     (text "")
                     (completion-func (lambda () nil))
                     (handlers *default-handlers*))
  (let ((vedit (make-vedit% :point point
                            :text text
                            :completion-func completion-func
                            :handlers handlers
                            :command (list))))
    (setf (text vedit) text)
    vedit))

(defun put-char (vedit char)
  (insert-item-at (vedit-text vedit) char (vedit-point vedit))
  (incf (vedit-point vedit))
  vedit)

(defun rem-char (vedit)
  (delete-item-at (vedit-text vedit) (vedit-point vedit))
  vedit)

(defun clear (vedit)
  (empty! (vedit-text vedit))
  (setf (vedit-point vedit) 0))

(defun point (vedit)
  (vedit-point vedit))

(defun current-char (vedit)
  (let ((point (vedit-point vedit)))
    (elt (subtext vedit point 1) 0)))

(defun subtext (vedit start count)
  (let ((ve (collect-elements (vedit-text vedit))))
    (coerce (subseq ve start (+ start count))
            'string)))

(defun text (vedit)
  (let* ((array (vedit-text vedit))
         (elements (collect-elements array)))
    (coerce elements 'string)))

(defun (setf text) (new-text vedit)
  (setf (vedit-text vedit)
        (make-container 'vector-container :initial-contents new-text)))

(defun move-char (vedit amount)
  (incf (vedit-point vedit) amount)
  (cond ((< (vedit-point vedit) 0)
         (setf (vedit-point vedit) 0))
        ((> (vedit-point vedit) (size (vedit-text vedit)))
         (setf (vedit-point vedit) (size (vedit-text vedit)))))
  (vedit-point vedit))

(defun move-word (vedit amount)
  (flet ((move-word-one (dir)
           (loop :do (move-char vedit dir)
              :while (not (member (item-at (vedit-text vedit)
                                        (vedit-point vedit))
                                  *delimit-chars*
                                  :test #'eql)))))
    (dotimes (_ (abs amount))
      (move-word-one (sign amount)))))

(defun move-full (vedit amount)
  (cond ((< amount 0)
         (setf (vedit-point vedit) 0))
        ((> amount 0)
         (setf (vedit-point vedit) (size (vedit-text vedit))))))

(defun complete (vedit)
  (when (vedit-completion-func vedit)
    (funcall (vedit-completion-func vedit)
             (subtext (vedit-text vedit)
                      0 (vedit-point vedit)))))

(defun handle-key (vedit char)
  (setf (vedit-command vedit)
        (append (vedit-command vedit) (list char)))
  (loop :for (seq func) :in (vedit-handlers vedit)
     :do (progn
           (cond ((equal seq (vedit-command vedit))
                  ;; Exact match
                  (funcall func vedit)
                  (setf (vedit-command vedit) nil)
                  (return-from handle-key))
                 ((seq-starts-with seq (vedit-command vedit))
                  ;; There's a longer command
                  (return-from handle-key)))))
  ;; No match. If the character is printable, put it into the string
  (loop :for c :in (vedit-command vedit)
     :if (and (typep c 'character)
              (graphic-char-p c))
     :do (put-char vedit c))
  (setf (vedit-command vedit) nil))

(defun backspace (vedit)
  (when (> (vedit-point vedit) 0)
    (move-char vedit -1)
    (rem-char vedit)))

(defun whitespacep (c)
  (member c '(#\Space #\Newline #\Tab)
          :test #'eql))

(defun del-word-back (vedit)
  (flet ((int ()
           (when (zerop (vedit-point vedit))
             (return-from del-word-back))))
    (int)
    (backspace vedit)
    (int)
    (move-char vedit -1)
    (loop :while (and (> (vedit-point vedit) 0)
                      (let ((c (current-char vedit)))
                        (and 
                         (alphanumericp c)
                         (not (whitespacep c)))))
       :do (rem-char vedit)
       :do (move-char vedit -1)))
  (let ((c (current-char vedit)))
    (cond ((and (zerop (vedit-point vedit))
                 (alphanumericp c)
                 (not (whitespacep c)))
           (rem-char vedit))
          (t
           (move-char vedit 1)))))

(defun complete-option (vedit text)
  (map nil (lambda (c) (put-char vedit c)) text))

(defmacro make-handlers ((vedit-var) &body handlers)
  `(list ,@(loop :for handler :in handlers
              :collect `(list ',(first handler)
                              (lambda (,vedit-var)
                                ,@(rest handler))))))

(defparameter *delimit-chars*
  '(#\Space #\. #\, #\; #\: #\' #\"
    #\( #\) #\_ #\- #\+ #\/ #\\ #\=
    #\! #\@ #\# #\$ #\% #\^ #\& #\*))

(defparameter *default-handlers*
  (make-handlers (vedit)
    ;; Ignore (from cl-tui)
    ((:key-down) nil)
    ((:key-up) nil)
    ;; ^A
    ((#\Soh)
     (move-full vedit -1))
    ;; ^E
    ((#\Enq)
     (move-full vedit 1))
    ;; Backspace
    ((:key-backspace)
     (backspace vedit))
    ((#\Rubout)
     (backspace vedit))
    ;; Ctrl+Backspace
    ((#\Backspace)
     (del-word-back vedit))
    ;; ^W
    ((#\Etb)
     (del-word-back vedit))
    ;; Delete
    ((#\Esc #\[ #\3 #\~)
     (rem-char vedit))
    ((:key-home)
     (move-full vedit -1))
    ((#\Esc #\[ #\H)
     (move-full vedit -1))
    ;; End
    ((:key-end)
     (move-full vedit 1))
    ((#\Esc #\[ #\F)
     (move-full vedit 1))
    ;; Right
    ((#\Esc #\[ #\C)
     (move-char vedit 1))
    ((:key-right)
     (move-char vedit 1))
    ;; Left
    ((#\Esc #\[ #\D)
     (move-char vedit -1))
    ((:key-left)
     (move-char vedit -1))
    ;; Tab
    ((#\Tab)
     (let ((completions (complete vedit)))
       (if (= 1 (length completions))
           (complete-option vedit (first completions))
           (values :complete completions))))))

;;; misc

(defun sign (int)
  (/ int (abs int)))

(defun seq-starts-with (seq1 seq2)
  (let ((p (search seq2 seq1)))
    (and p (= 0 p))))
