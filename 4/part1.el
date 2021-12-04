(defun aoc-get-bingo-numbers ()
  (beginning-of-buffer)
  (mapcar 'string-to-number (split-string (thing-at-point 'line nil) ",")))

(defun aoc-get-board-rows (start end)
  (mapcar
   (lambda (line) (mapcar 'string-to-number
                          (seq-filter (lambda (char) (not (equal char ""))) (split-string line "\s+"))))
   (split-string (buffer-substring-no-properties start end) "\n")))

(defun aoc-get-board-cols (rows cols)
  (if (eq rows nil)
      cols
    (aoc-get-board-cols
     (cdr rows)
     (mapcar* #'cons (car rows) cols))))

(defun aoc-get-bingo-boards ()
  (beginning-of-buffer)
  (forward-line 2)
  (let ((boards '()))
    (while (not (eobp))
      (let ((start (point)))
        (forward-line 4)
        (setq boards (cons (aoc-get-board-rows start (point-at-eol)) boards))
        (forward-line 2)))
    boards))

(defun aoc-complete-seq (lists)
  (seq-find 'aoc-check-row lists))

(defun aoc-check-row (row)
  (seq-every-p (lambda (item) (car (cdr item))) row))

(defun aoc-flatten-nums (lists)
  (mapcan (lambda (row) (mapcan (lambda (item) (car item)) row)) lists))

(defun aoc-init-state (lists)
  (mapcar (lambda (list) (mapcar (lambda (element) (list element nil)) list)) lists))

(defun aoc-update-state (lists num)
  (mapcar (lambda (l) (mapcar (lambda (element)
                                   (list (car element) (or (car (cdr element))
                                                           (eq (car element) num)))) l)) lists))

(defun aoc-play-bingo (rows cols numbers turn)
  (let ((updated-rows (aoc-update-state rows (car numbers)))
        (updated-cols (aoc-update-state cols (car numbers))))
    (message "number: %s\nrows: %s\ncols:%s\nturn:%s" (car numbers) updated-rows updated-cols turn)
    (let ((winning-seq (aoc-complete-seq (append updated-rows updated-cols))))
      (if winning-seq
          (list
           turn
           (car numbers)
           (seq-reduce #'+ (seq-difference (aoc-flatten-nums (append updated-rows updated-cols)) winning-seq)))
        (aoc-play-bingo updated-rows updated-cols (cdr numbers) (+ 1 turn))))))

(defun aoc-calc-bingo-win-conditions (numbers turn board)
  (let ((rows (aoc-init-state board))
        (cols (aoc-init-state (aoc-get-board-cols board '(() () () () ())))))
    (aoc-play-bingo rows cols numbers turn)))

(defun aoc-bingo ()
  (interactive)
  (let ((numbers (aoc-get-bingo-numbers))
        (boards (aoc-get-bingo-boards))
        (b (get-buffer-create "bingo-boards")))
    (select-window (split-window-vertically))
    (switch-to-buffer b)
    (insert (format "%s" (mapcar (apply-partially 'aoc-calc-bingo-win-conditions numbers 0) boards)))))
