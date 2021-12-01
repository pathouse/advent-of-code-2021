(defun count-depth-measurement-increases ()
  "Count the number of times sonar depth measurement increases"
  (interactive)
  (goto-char (point-min))
  (let ((previous-measurement (string-to-number (thing-at-point 'line nil)))
        (increases 0))
    (progn
      (while (not (eobp))
        (let ((current-measurement (string-to-number (thing-at-point 'line nil))))
          (if (> current-measurement previous-measurement) (setq increases (+ 1 increases)))
          (setq previous-measurement current-measurement)
          (forward-line 1)))
      (message (number-to-string increases)))))
