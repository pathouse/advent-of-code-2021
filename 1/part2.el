;; approach here is to try to make this work for part one as well by accepting an argument for sliding window size
;; where the size for part 1 was 1 and for part 2 is 3

(defun count-depth-measurement-increases-with-window ()
  "Count the number of times sonar depth measurement increases uses a sliding window"
  (interactive)
  (goto-char (point-min))
  (let ((window (list))
        (increases 0)
        (window-size (string-to-number (read-string "window size:"))))
    (progn
      (while (not (eobp))
        (setq window (cons (string-to-number (thing-at-point 'line nil)) window))
        (if
            (> (length window) window-size)
            (let ((window-one (seq-reduce #'+ (seq-take window window-size) 0))
                  (window-two (seq-reduce #'+ (rest window) 0)))
              (if (> window-one window-two) (setq increases (+ 1 increases)))
              (setq window (seq-take window window-size))))
        (forward-line 1))
      (message (number-to-string increases)))))
