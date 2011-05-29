;;; drag-things.el --- drag various text objects around a buffer

(defun drag-word-right (&optional arg)
  "Drag current word right ARG words."
  (interactive "p")
  (if (looking-at "\\b\\w")
      (forward-word))
  (transpose-words arg))

(defun drag-word-left (&optional arg)
  "Drag current word left ARG words."
  (interactive "p")
  (if (looking-at "\\b\\w")
      (forward-word))
  (transpose-words (- arg)))

(defalias 'drag-sexp-right 'transpose-sexps)

(defun drag-sexp-left (&optional arg)
  "Drag sexp under point left ARG sexps."
  (interactive "p")
  (transpose-sexps (- arg)))

(defun strict-forward-line (arg)
  "Like forward line, but errors if point can't be left exactly
ARG lines from point."
  (let ((origin (point)))
    (unless (and (zerop (forward-line arg))
                 (zerop (current-column)))
      (goto-char origin)
      (error "Don't have two things to transpose."))))

(defun region-lines (beg end)
  "Return a cons of the extent of the lines in the region."
  (save-excursion
    (goto-char beg)
    (cons (line-beginning-position)
          (progn
            (goto-char end)
            (line-end-position)))))

(defun displacement-lines (region arg)
  "Return a cons of the extent of the lines that would be
displaced by moving REGION by ARG lines."
  (let ((beg (car region))
        (end (cdr region)))
    (save-excursion
      (cond ((minusp arg)
             (goto-char beg)
             (cons (save-excursion
                     (strict-forward-line arg)
                     (point))
                   (progn
                     (backward-char)
                     (point))))
            (t
             (goto-char end)
             (cons (save-excursion
                     (forward-char)
                     (point))
                   (progn
                     (strict-forward-line arg)
                     (line-end-position))))))))

(defun point-in-first-line-p ()
  (let ((col-row (posn-col-row (posn-at-point))))
    (zerop (cdr col-row))))

(defun swap-adjacent-regions (a b)
  (cond ((= (car a) (cdr a))
         (goto-char (car a))
         (let ((pos (if (> (car b) (car a))
                        (cdr b)
                      (car b))))
           (insert (delete-and-extract-region (car b) (cdr b)))
           (goto-char pos)))
        ((= (car b) (cdr b))
         (goto-char (car b))
         (let ((pos (if (> (car b) (car a))
                        (1+ (car a))
                      (point))))
           (insert (delete-and-extract-region (car a) (cdr a)))
           (goto-char pos)))
        (t
         (let ((ref (set-marker (make-marker) (car a))))
           (transpose-regions (car a) (cdr a) (car b) (cdr b))
           ;; if point is on the first line of the screen, transpose-regions
           ;; will scroll however many lines it takes to maintain point at the
           ;; top of the screen. This behaviour is inconsistent with the usual
           ;; emacs behaviour and can be amazingly annoying, thus this
           ;; gigantic ugly hack to avoid it. fixme - bit busted
           (when (point-in-first-line-p)
             (save-excursion
               (goto-char (point-min)) ;; hack
               (set-mark (point-min))  ;; hack
               (redisplay)))           ;; hack
           (goto-char ref)
           (set-marker ref nil)))))

(defun drag-region-lines (beg end arg)
  "Drag lines in the region down ARG lines."
  (unless (and arg (zerop arg))
    (let* ((arg (or arg 1))
           (a (region-lines beg end))
           (b (displacement-lines a arg))
           (region-offsets (cons (- beg (car a))
                                 (- end (car a)))))
      (swap-adjacent-regions a b)
      (let ((ref (point)))
        (goto-char (+ (car region-offsets) ref))
        (set-mark (+ (cdr region-offsets) ref))))))

(defun drag-line (arg)
  "Drag current line ARG lines down. Point is left in its place
relative to the line."
  (let ((col (current-column)))
    (drag-region-lines (point) (point) arg)))

(defvar drag-line-reindents nil
  "If true, dragging lines will reindent the dragged region.")

(defun drag-line-or-region (arg)
  "Drag the current line down ARG lines - if the mark is active,
drag all lines in the region.

Point is left in it's place relative to the moved lines. If the
mark is active, it will be left active."
  (interactive "*P")
  (cond ((or (consp arg)
             (and arg (zerop arg)))
         (let ((col (current-column)))
           (transpose-lines 0)
           (when drag-line-reindents
             (funcall indent-line-function))
           (move-to-column col)))

        ((region-active-p)
         (let (deactivate-mark)
           (when (and (bolp) ; don't move line if point is at its start
                      (> (point) (mark)))
             (backward-char))
           (drag-region-lines (region-beginning) (region-end) arg)

           (when drag-line-reindents
             (let ((lines (region-lines (region-beginning) (region-end))))
               (indent-region (car lines) (cdr lines)))))
         (setq mark-active t))

        (t
         (drag-line (or arg 1))
         (when drag-line-reindents
           (funcall indent-line-function))
         ;; fixme - this prevents the mark being turned on sometimes while
         ;; dragging empty lines around. Why is it necessary?
         (setq deactivate-mark t))))

(defun drag-line-or-region-up (arg)
  "Drag the current line up ARG lines - if the mark is active,
drag all lines in the region.

Point is left in it's place relative to the moved lines. If the
mark is active, it will be left active."
  (interactive "P")
  (drag-line-or-region
   (if (numberp arg) (- arg) (or arg -1))))

(provide 'drag-things)
