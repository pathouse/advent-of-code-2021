(defconst segments (list "a" "b" "c" "d" "e" "f" "g"))
(defconst aoc-number-lookup
  (vector
   (vector "a" "b" "c" "e" "f" "g")     ;; 0
   (vector "c" "f")                     ;; 1
   (vector "a" "c" "d" "e" "g")         ;; 2
   (vector "a" "c" "d" "f" "g")         ;; 3
   (vector "b" "c" "d" "f")             ;; 4
   (vector "a" "b" "d" "f" "g")         ;; 5
   (vector "a" "b" "d" "e" "f" "g")     ;; 6
   (vector "a" "c" "f")                 ;; 7
   (vector "a" "b" "c" "d" "e" "f" "g") ;; 8
   (vector "a" "b" "c" "d" "f" "g")))   ;; 9

(defun aoc-segment-info (s)
  (cond ((equal s "a") (list 'pos 'h 'line 1 'y " aaaa " 'n " .... "))
        ((equal s "b") (list 'pos 'v 'line 2 'y "b" 'n "."))
        ((equal s "c") (list 'pos 'v 'line 2 'y "    c" 'n "    ."))
        ((equal s "d") (list 'pos 'h 'line 4 'y " dddd " 'n " .... "))
        ((equal s "e") (list 'pos 'v 'line 5 'y "e" 'n "."))
        ((equal s "f") (list 'pos 'v 'line 5 'y "    f" 'n "    ."))
        ((equal s "g") (list 'pos 'h 'line 7 'y " gggg " 'n " .... "))))

(defun aoc-render-digit (n index)
  (seq-do
   (lambda (segment)
     (let ((position (plist-get (aoc-segment-info segment) 'pos))
           (line (plist-get (aoc-segment-info segment) 'line))
           (padding (if (> index 0) "  " ""))
           (str (if (seq-contains-p (aref aoc-number-lookup n) segment)
                    (propertize (plist-get (aoc-segment-info segment) 'y) 'face '(:foreground "#65FF46"))
                  (propertize (plist-get (aoc-segment-info segment) 'n) 'face '(:foreground "#EBEBEB")))))
       (cond ((and (eq (line-number-at-pos) line)
                   (eq position 'h))
              (end-of-line)
              (insert (concat padding str))
              (if (eq (forward-line 1) 1) (newline)))
             ((and (eq (line-number-at-pos) line)
                   (eq position 'v))
              (dotimes (_n 2)
                (end-of-line)
                (insert (concat padding str))
                (if (eq (forward-line 1) 1) (newline))))
             (t (forward-line -2)
                (dotimes (_n 2)
                  (end-of-line)
                  (insert str)
                  (forward-line 1))))))
   (list "a" "b" "c" "d" "e" "f" "g")))

(defun aoc-render-seven-segment-number ()
  (interactive)
  (let ((buf (get-buffer-create "aoc-day-eight"))
        (digits
         (mapcar
          'string-to-number
          (seq-filter
           (lambda (s) (not (equal s "")))
           (split-string (read-string "number: ") "")))))
    (select-window (split-window-vertically))
    (switch-to-buffer buf)
    (erase-buffer)
    (seq-do-indexed
     (lambda (d idx)
       (beginning-of-buffer)
       (aoc-render-digit d idx))
     digits)))
