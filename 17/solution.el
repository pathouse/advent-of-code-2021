(defun aoc-parse-range (start end str)
  (let* ((start-pos (+ 2 (string-match start str)))
         (end-pos (string-match end str))
         (substr (seq-subseq str start-pos end-pos))
         (nums (seq-map 'string-to-number (split-string substr "\\.\\."))))
    (number-sequence (car nums) (car (cdr nums)))))

(defun aoc-parse-target-area ()
  (let* ((desc (thing-at-point 'line nil))
         (x-range (aoc-parse-range "x=" "," desc))
         (y-range (aoc-parse-range "y=" "\n" desc)))
    (list 'x x-range 'y y-range)))

(defun aoc-apply-drag (velocity)
  (let* ((x-velocity (aref velocity 0))
         (new-velocity (cond ((> x-velocity 0) (- x-velocity 1))
                             ((< x-velocity 0) (+ x-velocity 1))
                             (t x-velocity))))
    (aset velocity 0 new-velocity)))

(defun aoc-apply-gravity (velocity)
  (let* ((y-velocity (aref velocity 1))
         (new-velocity (- y-velocity 1)))
    (aset velocity 1 new-velocity)))

(defun aoc-update-position (position velocity)
  (let ((x (aref position 0))
        (x-velocity (aref velocity 0))
        (y (aref position 1))
        (y-velocity (aref velocity 1)))
    (aset position 0 (+ x x-velocity))
    (aset position 1 (+ y y-velocity))))

(defun aoc-run-step (position velocity)
  (aoc-update-position position velocity)
  (aoc-apply-drag velocity)
  (aoc-apply-gravity velocity)
  (list 'position (vector (aref position 0) (aref position 1))
        'velocity (vector (aref velocity 0) (aref velocity 1))))

(defun aoc-bad-trajectory (target position velocity)
  (or
   ;; x is not within range and
   ;; velocity has dropped to 0 so it will no longer change
   ;; or x will be beyond max x in the next step
   (or
    (and
     (eq (aref velocity 0) 0)
     (not (seq-contains-p (plist-get target 'x) (aref position 0))))
    (> (+ (aref position 0) (aref velocity 0)) (seq-max (plist-get target 'x))))
   ;; y is below range
   ;; or y will become below range in the next step
   (or
    (< (aref position 1) (seq-min (plist-get target 'y)))
    (< (+ (aref position 1) (aref velocity 1)) (seq-min (plist-get target 'y))))))

(defun aoc-on-target (target position)
  (and
   (seq-contains-p (plist-get target 'x) (aref position 0))
   (seq-contains-p (plist-get target 'y) (aref position 1))))

(defun aoc-seventeen ()
  (interactive)
  (let* ((target (aoc-parse-target-area))
         (x-velocity-range (number-sequence -200 200))
         (y-velocity-range (number-sequence -200 200))
         (good-velocities '()))
    (seq-do
     (lambda (x-v)
       (seq-do
        (lambda (y-v)
          (let ((position (vector 0 0))
                (velocity (vector x-v y-v))
                (path '())
                (result nil))
            (while (not result)
              (setq path (cons (aoc-run-step position velocity) path))
              (if (aoc-bad-trajectory target position velocity)
                  (setq result 'bad))
              (if (aoc-on-target target position)
                  (setq result 'good)))
            (if (eq result 'good)
                (setq good-velocities
                      (cons (list 'velocity (vector x-v y-v)
                                  'max-y (seq-max (seq-map (lambda (p) (aref (plist-get p 'position) 1)) path)))
                            good-velocities)))))
        y-velocity-range))
     x-velocity-range)
    (message "part one: %d" (seq-max (seq-map (lambda (v) (plist-get v 'max-y)) good-velocities)))
    (message "part two: %d" (length (seq-uniq (seq-map (lambda (v) (plist-get v 'velocity)) good-velocities) 'equal)))))
