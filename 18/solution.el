(require 'treepy)

(defun aoc-parse-snailfish-number (line)
  (treepy-vector-zip
   (car
    (read-from-string
     (replace-regexp-in-string "," " " line)))))

(defun aoc-test-num ()
  (interactive)
  (let* ((tr (aoc-parse-snailfish-number (thing-at-point 'line nil)))
         (pair (aoc-next-pair tr))
         (left (aoc-left-loc pair))
         (right (aoc-right-loc pair)))
    (message "%s" pair)
    (message "%s" (aoc-follow-path (treepy-path pair) (aoc-to-root pair)))))
 ;;   (message "%s" (aoc-explode pair))))


(defun aoc-to-root (tr)
  (let ((loc tr)
        (top nil))
    (while (not top)
      (if (treepy-up loc)
          (setq loc (treepy-up loc))
        (setq top t)))
    loc))

(defun aoc-next-pair (tr)
  (let ((loc tr))
    (while (not (and (treepy-branch-p loc) (seq-every-p 'numberp (treepy-children loc))))
      (setq loc (treepy-next loc)))
    loc))

(defun aoc-left-loc (tr)
  (let ((loc tr)
        (searching t)
        (result nil))
    (while searching
      (cond ((not loc) (setq searching nil))
            ((not (treepy-left loc)) (setq loc (treepy-up loc)))
            ((treepy-branch-p (treepy-left loc))
             (setq result (treepy-rightmost-descendant (treepy-left loc)))
             (setq searching nil))
            (t
             (setq result (treepy-left loc))
             (setq searching nil))))
    result))

(defun aoc-right-loc (tr)
  (let ((loc tr)
        (searching t)
        (result nil))
    (while searching
      (cond ((not loc) (setq searching nil))
            ((not (treepy-right loc)) (setq loc (treepy-up loc)))
            ((treepy-branch-p (treepy-right loc))
             (setq result (treepy-leftmost-descendant (treepy-right loc)))
             (setq searching nil))
            (t
             (setq result (treepy-right loc))
             (setq searching nil))))
    result))

(defun aoc-parse-snailfish-numbers ()
  (beginning-of-buffer)
  (let (nums '())
    (while (not (eobp))
      (setq nums (cons (aoc-parse-snailfish-number (thing-at-point 'line nil)) nums))
      (forward-line 1))
    (seq-reverse nums)))

(defun aoc-add-snailfish-numbers (n1 n2)
  (aoc-reduce-snailfish-number (treepy-list-zip (list (treepy-children n1) (treepy-children n2)))))

(defun aoc-reduce-snailfish-number (n)
  (let ((loc (aoc-next-pair n)))
    (while (not (treepy-end-p loc))
      (cond ((eq (length (treepy-path loc)) 4) (setq loc (aoc-explode loc)))
            ((seq-some (lambda (n) (>= 10 n)) (treepy-children loc)) (setq loc (aoc-split loc)))
            (t (setq loc (aoc-next-pair loc)))))
    (treepy-root loc)))

(defun aoc-follow-path (path loc)
  (message "follow path: %s %s" path loc)
  (let ((step (car path)))
    (message "%s" step)
    (if (equal step (treepy-node (treepy-down loc)))
        (aoc-follow-path (cdr path) (treepy-down loc))
      (aoc-follow-path (cdr path) (treepy-right loc)))))

(defun aoc-explode (loc)
  (let ((nums (treepy-children loc))
        (left (aoc-left-loc loc))
        (right (aoc-right-loc loc)))
    (if left (treepy-root (treepy-edit left (lambda (n v) (+ n v)) (nth 0 nums))))))
;;    (if right (treepy-edit right (lambda (n v) (+ n v)) (nth 1 nums)))
;;    (treepy-root loc)))

(defun aoc-split (loc))
  ;; do stuff

(defun aoc-eighteen-one ()
  (interactive)
  (let ((nums (aoc-parse-snailfish-numbers)))
    (message "%s" (aoc-add-snailfish-numbers (car nums) (car (cdr nums))))))
