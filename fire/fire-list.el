;;; -*- lexical-binding: t; -*-

;; this seems to be a prototype using echo-server-client,
;; it does not work now

(setq fire-xsize 100
      fire-ysize 35
      fire-total-size (* fire-xsize fire-ysize)
      fire-table-size (* 256 5)
      table (make-vector fire-table-size 0)

      loop 0
      sloop 0
      height 0
      fire-limit (+ fire-total-size fire-xsize))

(defun gen-table ()
  (let ((minus (max (/ 800 fire-ysize) 1))
        (p2 0))
    (dotimes (i fire-table-size)
      (setf (aref table i)
            (if (<= i minus)
                0
              (setq p2 (/ (- i minus) 5)))))))

(setq fire-bitmap (make-vector (* fire-xsize (+ fire-ysize 3)) 0))

(defun fire-main ()
  (let ((idx 0))
    (while (< idx fire-total-size)
      (setf (aref fire-bitmap idx)
            (aref table (+ (aref fire-bitmap (+ idx fire-xsize -1))
                           (aref fire-bitmap (+ idx fire-xsize +1))
                           (aref fire-bitmap (+ idx fire-xsize))

                           (aref fire-bitmap (+ idx fire-xsize fire-xsize -1))
                           (aref fire-bitmap (+ idx fire-xsize fire-xsize +1)))))
      (setq idx (1+ idx)))))

(defun draw-fire ()
  (setq height (1+ height)
        loop   (1- loop))
  (when (< loop 0)
    (setq loop (random 3)
          sloop (1+ sloop)))
  (let ((i 0)
        (last1 0)
        (i1 1)
        (i2 (1+ (* fire-xsize 4)))
        (idx fire-total-size))
    (while (< idx fire-limit)
      (setq last1 (random (min i1 i2 height))
            i     (random 6))
      (while (and (< idx fire-limit)
                  (not (zerop i)))
        (setf (aref fire-bitmap idx) last1)
        (setq last1 (mod (+ last1 (random 6) -2) 256))
        (setf (aref fire-bitmap (+ idx fire-xsize)) last1)
        (setq last1 (mod (+ last1 (random 6) -2) 256))

        (setq idx (1+ idx)
              i   (1- i)
              i1  (+ i1 4)
              i2  (- i2 4)))

      (when (< idx fire-limit)
        (setf (aref fire-bitmap (+ idx fire-xsize fire-xsize)) last1)
        (setq last1 (mod (+ last1 (random 6) -2) 256))
        (setq idx (1+ idx)
              i1  (+ i1 4)
              i2  (- i2 4)))))
  (fire-main))

(gen-table)

;; (draw-fire)

(setq color-list '(232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 231))

(defun draw (color)
  (let ((idx (floor (* (min 0.99 (/ color 256.0))
                       (length color-list)))))
    (format "\x1b[48;5;%dm " (nth idx color-list))))

;; (defun display-fire ()
;;   (with-temp-buffer
;;     (insert "\r\n\e[0m\e[H")
;;     (let ((idx 0))
;;       (dotimes (i fire-ysize)
;;         (dotimes (j fire-xsize)
;;           (insert (draw (aref fire-bitmap idx)))
;;           (setq idx (1+ idx)))
;;         (insert "\r\n"))
;;       (process-send-region
;;        (caar echo-server-clients)
;;        (point-min) (point-max)))))

(setq fire-current-display "")

(defun display-fire ()
  (let ((list '("\r\n\e[0m\e[2J\e[H"))
        (idx 0))
    (dotimes (i fire-ysize)
      (dotimes (j fire-xsize)
        (push (draw (aref fire-bitmap idx)) list)
        (setq idx (1+ idx)))
      (push "\r\n" list))
    (setq fire-current-display
          (apply 'concat (nreverse list)))
    (process-send-string (caar echo-server-clients)
                         fire-current-display)))

(progn
  (draw-fire)
  (display-fire))

;; (setq timer (run-at-time 1 (/ 1.0 15)
;;                          (lambda ()
;;                            (interactive)
;;                            (draw-fire)
;;                            (display-fire))))

;; (cancel-timer timer)
