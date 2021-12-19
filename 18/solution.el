(require 'treepy)

;; PARSING INPUTS

(defun aoc-parse-snailfish-number (line)
  (treepy-list-zip
   (car
    (read-from-string
     (replace-regexp-in-string
      "\\]"
      ")"
      (replace-regexp-in-string
       "\\["
       "("
       (replace-regexp-in-string "," " " line)))))))

(defun aoc-parse-snailfish-numbers ()
  (beginning-of-buffer)
  (let (nums '())
    (while (not (eobp))
      (setq nums (cons (aoc-parse-snailfish-number (thing-at-point 'line nil)) nums))
      (forward-line 1))
    (seq-reverse nums)))

;; TREE OPERATIONS

(defun aoc-to-root (tr)
  (let ((loc tr)
        (top nil))
    (while (not top)
      (if (treepy-up loc)
          (setq loc (treepy-up loc))
        (setq top t)))
    loc))

(defun aoc-relative-loc (starting-loc first-turn last-turn)
  (let ((loc starting-loc)
        (searching t)
        (result nil))
    (while searching
      (cond ((not loc) (setq searching nil))
            ((not (funcall first-turn loc)) (setq loc (treepy-up loc)))
            ((treepy-branch-p (funcall first-turn loc))
             (setq result (funcall last-turn (funcall first-turn loc)))
             (setq searching nil))
            (t
             (setq result (funcall first-turn loc))
             (setq searching nil))))
    result))

(defun aoc-rightmost-descendant (starting-loc)
  (let ((loc starting-loc))
    (while (treepy-branch-p loc)
      (setq loc (treepy-down loc))
      (if (treepy-right loc) (setq loc (treepy-right loc))))
  loc))

(defun aoc-left-loc (loc)
  (aoc-relative-loc loc 'treepy-left 'aoc-rightmost-descendant))

(defun aoc-right-loc (loc)
  (aoc-relative-loc loc 'treepy-right 'treepy-leftmost-descendant))

(defun aoc-deep-contains-p (seq elt)
  (or (seq-contains-p seq elt)
      (seq-some (lambda (s) (aoc-deep-contains-p s elt)) (seq-filter 'listp seq))))

(defun aoc-goto-node (node starting-loc)
  (let ((loc starting-loc))
    (while (not (and (equal (treepy-node loc) node)
                     (equal (length (treepy-path loc)) 4)))
      (setq loc (treepy-next loc)))
    loc))

(defun aoc-edit-and-return (start dest-func edit-func edit-args)
  (let ((return-to-node (treepy-node start))
        (dest-loc (funcall dest-func start)))
    (if (not dest-loc)
        start
      (aoc-goto-node
       return-to-node
       (aoc-to-root
        (treepy-edit dest-loc edit-func edit-args))))))

;; SNAILFISH NUMBER OPERATIONS

(defun aoc-add-snailfish-numbers (n1 n2)
  (let ((new-tree (treepy-list-zip (list (treepy-root n1) (treepy-root n2)))))
    (aoc-reduce-snailfish-number new-tree)))

(defun aoc-explode-pairs (starting-loc)
  (let ((loc starting-loc)
        (exploding t)
        (actions 0))
    (while exploding
      (cond ((and
              (treepy-branch-p loc)
              (seq-every-p 'numberp (treepy-children loc))
              (eq (length (treepy-path loc)) 4))
             (setq loc (aoc-explode loc))
             (setq actions (+ 1 actions)))
            ((treepy-end-p (treepy-next loc))
             (setq exploding nil))
            (t (setq loc (treepy-next loc)))))
    (list 'loc (aoc-to-root loc) 'actions actions)))

(defun aoc-split-bignums (starting-loc)
  (let ((loc starting-loc)
        (splitting t)
        (actions 0))
    (while splitting
      (cond ((and (numberp (treepy-node loc))
                  (>= (treepy-node loc) 10))
             (setq loc (aoc-split loc))
             (setq actions (+ 1 actions))
             (setq splitting nil))
            ((treepy-end-p (treepy-next loc))
             (setq splitting nil))
            (t (setq loc (treepy-next loc)))))
    (list 'loc (aoc-to-root loc) 'actions actions)))

(defun aoc-reduce-snailfish-number (starting-loc)
  (let* ((loc starting-loc)
         (reducing t))
    (while reducing
      (let* ((explode-results (aoc-explode-pairs loc))
             (exploded-loc (plist-get explode-results 'loc))
             (explode-count (plist-get explode-results 'actions))
             (split-results (aoc-split-bignums exploded-loc))
             (split-loc (plist-get split-results 'loc))
             (split-count (plist-get split-results 'actions)))
        (setq loc split-loc)
        (if (and (eq explode-count 0) (eq split-count 0))
            (setq reducing nil))))
    (aoc-to-root loc)))

(defun aoc-explode (loc)
  (let ((current-loc loc)
        (nums (treepy-children loc))
        (edit-func (lambda (n v) (+ n v))))
    (setq current-loc (aoc-edit-and-return current-loc 'aoc-left-loc edit-func (nth 0 nums)))
    (setq current-loc (aoc-edit-and-return current-loc 'aoc-right-loc edit-func (nth 1 nums)))
    (aoc-to-root (treepy-replace current-loc 0))))

(defun aoc-split (loc)
  (let* ((val (float (treepy-node loc)))
         (left (floor (/ val 2)))
         (right (ceiling (/ val 2))))
    (aoc-to-root (treepy-replace loc (list left right)))))

(defun aoc-magnitude (loc)
  (if (treepy-branch-p loc)
    (let* ((left (treepy-down loc))
           (right (treepy-right left)))
      (+ (* 3 (aoc-magnitude left)) (* 2 (aoc-magnitude right))))
    (treepy-node loc)))

(defun aoc-test-num ()
  (interactive)
  (let* ((n (aoc-parse-snailfish-number (thing-at-point 'line nil))))
    (message "%s" (aoc-reduce-snailfish-number n))))

(defun aoc-eighteen-one ()
  (interactive)
  (let ((nums (aoc-parse-snailfish-numbers)))
    (message "%s" (aoc-magnitude (seq-reduce 'aoc-add-snailfish-numbers (cdr nums) (car nums))))))

(defun aoc-eighteen-two ()
  (interactive)
  (let ((nums (aoc-parse-snailfish-numbers))
        (largest-magnitude 0))
    (while (> (length nums) 1)
      (let ((n1 (car nums)))
        (seq-do
         (lambda (n2)
           (setq largest-magnitude
                 (max largest-magnitude
                      (aoc-magnitude (aoc-add-snailfish-numbers n1 n2))
                      (aoc-magnitude (aoc-add-snailfish-numbers n2 n1)))))
         (cdr nums))
        (setq nums (cdr nums))))
    (message "%s" largest-magnitude)))
