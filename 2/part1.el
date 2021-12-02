(defun aoc-navigate-submarine ()
  "Follow instructions to navigate the sub, appends location info to end of line"
  (interactive)
  (goto-char (point-min))
  (let ((h-pos 0)
        (depth 0))
    (while (not (eobp))
      (let ((direction (car (split-string (thing-at-point 'line nil))))
            (amount (string-to-number (car (cdr (split-string (thing-at-point 'line nil)))))))
        (cond ((equal direction "forward") (setq h-pos (+ h-pos amount)))
              ((equal direction "up") (setq depth (- depth amount)))
              ((equal direction "down") (setq depth (+ depth amount))))
        (end-of-line)
        (insert (format " | Horizontal: %d, Depth: %d (%d)" h-pos depth (* h-pos depth)))
        (forward-line 1)
        (beginning-of-line)))
    ;; TODO - understand why the regexp string here isn't just "|"
    ;; https://stackoverflow.com/questions/14583702/align-regexp-from-emacs-lisp
    (align-regexp (point-min) (point-max-marker) "\\(\\s-*\\)|")))
