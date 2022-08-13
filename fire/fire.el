;;; fire generation code from aalib-1.4.0/src/aafire.c

(defvar fire-current-ascii ""
  "ASCII escaped representation of current fire frame.")

(defvar fire-clients nil
  "A list of clients connected to server.")

(defvar fire-frame-rate 14
  "Rate at which fire updates every second.")

(defvar fire-server-port 21814
  "Port fire server listens to.")

(defconst fire-xsize 79)
(defconst fire-ysize 27)
(defconst fire-total-size (* fire-xsize fire-ysize))
(defconst fire-limit (+ fire-total-size fire-xsize))
(defconst fire-table-size (* 256 5))

(defvar fire-table (make-vector fire-table-size 0))
(defvar fire-height 0)
(defvar fire-bitmap (make-vector
                     (* fire-xsize (+ fire-ysize 3)) 0))
(defvar fire-timer nil)

(defun fire-gen-table ()
  (let ((minus (max (/ 800 fire-ysize) 1)))
    (dotimes (i fire-table-size)
      (aset fire-table i
            (if (<= i minus)
                0
              (min (floor (/ (- i minus) 4.8)) 255))))))

(defun fire-one-frame ()
  "Draw one frame of fire onto `fire-bitmap'."
  (setq fire-height (1+ fire-height))
  (let ((i 0)
        (last1 0)
        (i1 1)
        (i2 (1+ (* fire-xsize 4)))
        (idx fire-total-size))
    (while (< idx fire-limit)
      (setq last1 (random (min i1 i2 fire-height))
            i     (random 6))
      (while (and (not (zerop i)) (< idx fire-limit))
        (aset fire-bitmap idx last1)
        (setq last1 (mod (+ last1 (random 6) -2) 256))
        (aset fire-bitmap (+ idx fire-xsize) last1)
        (setq last1 (mod (+ last1 (random 6) -2) 256))

        (setq idx (1+ idx)
              i   (1- i)
              i1  (+ i1 4)
              i2  (- i2 4)))

      (when (< idx fire-limit)
        (aset fire-bitmap (+ idx fire-xsize fire-xsize) last1)
        (setq last1 (mod (+ last1 (random 6) -2) 256)
              idx (1+ idx)
              i1  (+ i1 4)
              i2  (- i2 4)))))
  (let ((idx 0))
    (while (< idx fire-total-size)
      (aset fire-bitmap idx
            (aref fire-table
                  (+ (aref fire-bitmap (+ idx fire-xsize -1))
                     (aref fire-bitmap (+ idx fire-xsize +1))
                     (aref fire-bitmap (+ idx fire-xsize))

                     (aref fire-bitmap (+ idx fire-xsize fire-xsize -1))
                     (aref fire-bitmap (+ idx fire-xsize fire-xsize +1)))))
      (setq idx (1+ idx)))))

;;; black & white vector
;; (setq fire-color-vector [232 233 234 235 236 237 238 239 240
;;                              241 242 243 244 245 246 247 248 249
;;                              250 251 252 253 254 255 231])

(defconst fire-color-vector [;; background
                             232
                             ;; blue
                             17 18
                             ;; red
                             88 124 160 160 160 196 196 196 196 196
                             ;; yellow
                             208 214 220 220 220 226
                             226 226 226 226 226 226])
(defconst fire-color-length (length fire-color-vector))

(defun fire-pixel-ascii (color)
  "Convert COLOR to ASCII escape string."
  (format "\x1b[48;5;%dm "
          (aref fire-color-vector
                (floor (* (min 0.99 (/ color 160.0))
                             fire-color-length)))))

;;; black & white only
;; (defun draw (color)
;;   (format "\x1b[48;5;%dm "
;;           (+ 232 (floor (* 24 (min 0.99
;;                                    (/ color 160.0)))))))

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

(defun fire-bitmap-to-ascii ()
  (let ((list '("\r\n\e[0m\e[2J\e[H"))
        (idx (* 3 fire-xsize)))
    (dotimes (i (- fire-ysize 3))
      (dotimes (j fire-xsize)
        (push (fire-pixel-ascii
               (aref fire-bitmap idx))
              list)
        (setq idx (1+ idx)))
      (unless (= i (- fire-ysize 4))
        (push "\r\n" list)))
    (setq fire-current-ascii
          (apply 'concat (nreverse list)))))

(defun fire-one-frame-ascii ()
  (fire-one-frame)
  (fire-bitmap-to-ascii)
  ;; send current frame to every client
  (mapc (lambda (proc)
          (process-send-string proc fire-current-ascii))
        fire-clients))

(defun fire-stop-fire ()
  (interactive)
  (when (timerp fire-timer)
    (cancel-timer fire-timer)))

(defun fire-start-fire ()
  (interactive)
  (fire-stop-fire)
  (setq fire-timer
        (run-at-time nil (/ 1.0 fire-frame-rate)
                     'fire-one-frame-ascii)))

(defun fire-restart-fire ()
  (interactive)
  (fire-stop-fire)
  (setq fire-height 0
        fire-bitmap (make-vector
                     (* fire-xsize (+ fire-ysize 3)) 0))
  (fire-start-fire))

(defun fire-start-server ()
  "Start fire server.
This should only be called once."
  (interactive)
  (unless (process-status "fire-server")
    (make-network-process
     :name     "fire-server"
     :buffer   "*fire-server*"
     :family   'ipv4
     :service   fire-server-port
     :sentinel 'fire-server-sentinel
     :filter   'fire-server-filter
     :server   't
     :host     "0.0.0.0")
    (fire-gen-table)))

(defun fire-server-sentinel (proc msg)
  (message (concat "fire: " msg))
  (if (string-prefix-p "open from" msg)
      ;; new connections opened
      (progn
        (when (null fire-clients)       ; first client
          (fire-start-fire))
        (push proc fire-clients))       ; add proc to `fire-clients'
    ;; connection closed or deleted
    (setq fire-clients (remove proc fire-clients)) ; delete proc from clients
    (when (null fire-clients)           ; no live client
      (fire-stop-fire))))

(defun fire-server-filter (proc string)
  (delete-process proc))
