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

(defun aoc-flatten-one (predicate list)
  (mapcar (lambda (item) (if (eq (car (cdr item)) predicate) (car item) 0)) list))

(defun aoc-flatten-many (predicate lists)
  (mapcan (apply-partially 'aoc-flatten-one predicate) lists))

(defun aoc-init-state (lists)
  (mapcar (lambda (list) (mapcar (lambda (element) (list element nil)) list)) lists))

(defun aoc-update-state (lists num)
  (mapcar (lambda (l) (mapcar (lambda (element)
                                   (list (car element) (or (car (cdr element))
                                                           (eq (car element) num)))) l)) lists))

(defun aoc-bingo-score (rows cols winning-seq)
  (let ((all-nums (aoc-flatten-many nil (append rows cols)))
        (winning-nums (aoc-flatten-one t winning-seq)))
    (message "all: %s" all-nums)
    (message "winning: %s" winning-nums)
    (message "diff: %s" (seq-uniq (seq-difference all-nums winning-nums)))
    (message "score: %s" (seq-reduce #'+ (seq-uniq (seq-difference all-nums winning-nums)) 0))
    (seq-reduce #'+ (seq-uniq (seq-difference all-nums winning-nums)) 0)))

(defun aoc-play-bingo (rows cols numbers turn)
  (let ((updated-rows (aoc-update-state rows (car numbers)))
        (updated-cols (aoc-update-state cols (car numbers))))
    (let ((winning-seq (aoc-complete-seq (append updated-rows updated-cols))))
      (cond (winning-seq (list
                          turn
                          (car numbers)
                          (aoc-bingo-score updated-rows updated-cols winning-seq)))
            ((eq 0 (length (cdr numbers))) '(nil nil nil))
            (t (aoc-play-bingo updated-rows updated-cols (cdr numbers) (+ 1 turn)))))))

(defun aoc-calc-bingo-win-conditions (numbers turn board)
  (let ((rows (aoc-init-state board))
        (cols (aoc-init-state (aoc-get-board-cols board '(() () () () ())))))
    (aoc-play-bingo rows cols numbers turn)))

(defun aoc-squid-bingo (sort-by buffer-name)
  (let ((numbers (aoc-get-bingo-numbers))
        (boards (aoc-get-bingo-boards))
        (b (get-buffer-create buffer-name)))
    (select-window (split-window-vertically))
    (switch-to-buffer b)
    (let ((winner (car (seq-sort-by #'car sort-by (mapcar (apply-partially 'aoc-calc-bingo-win-conditions numbers 0) boards)))))
      (insert (format "%s: %d" buffer-name (* (nth 1 winner) (nth 2 winner)))))))

;; it's always nice when the solution for part one ends up being just 1 small modification away from the solution for part 2 :)
;; in this case deciding to solve part 1 by computing win conditions for all boards ended up paying off

(defun aoc-squid-bingo-first ()
  (interactive)
  (aoc-squid-bingo #'< "first-winning-board"))

(defun aoc-squid-bingo-last ()
  (interactive)
  (aoc-squid-bingo #'> "last-winning-board"))
