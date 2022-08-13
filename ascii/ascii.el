(defcustom ascii-server-port 21812
  "Port to listen to.")

(defcustom ascii-gallery-root
  (concat (file-name-directory (buffer-file-name))
          "gallery")
  "Directory to store all ASCII art.")

(defvar ascii-clients '()
  "AList containing each client, its timer, frame list.")

(defvar ascii-clear-screen "\r\n\e[2J\e[H"
  "Sequenct to clear screen and set cursor to top left.")

(defvar ascii-show-cursor "\e[?25h")

(defvar ascii-hide-cursor "\e[?25l")

(defun ascii-server-start ()
  "Start ASCII server."
  (interactive)
  (unless (process-status "ascii-server")
    (make-network-process
     :name     "ascii-server"
     :buffer   "*ascii-server*"
     :family   'ipv4
     :service   ascii-server-port
     :sentinel 'ascii-server-sentinel
     :filter   'ascii-server-filter
     :server   't
     :host     "0.0.0.0")
    (setq ascii-clients '())))

(defun ascii-gallery ()
  "Return a list of available artworks."
  (directory-files ascii-gallery-root
                   nil
                   (rx string-start
                       (any alnum ?- ?_)
                       (* (any alnum ?- ?_ ?.))
                       string-end)))

(defun ascii-welcome ()
  "Return welcome string."
  (concat "You've reached this small ASCII gallery, feel free to look around.\r\n"
          "Here are all the artworks stored here:\r\n"
          (mapconcat (lambda (file-name)
                       (format "  %s\r\n" file-name))
                     (ascii-gallery) "")
          "Enter a name to see the piece, or q to quit.\r\n"
          "Note: while admiring the artwork, press <enter> to quit.\r\n"
          "      Don't bother pressing Ctrl-C, I only see <enter>.\r\n"
          "> "))

;;; When the client enters C-c, telnet will send
;;; (255 244 255 253 6) --- IAC Interrupt Process, IAC DO TimingMark
;;; respond with IAC WONT TIMING-MARK
;;; (telnet-command-to-string '(IAC WONT TIMING-MARK))

(defvar telnet-command-alist
  '((ECHO . 1)
    (SUPPRESS-GO-AHEAD . 3)
    (TIMING-MARK . 6)
    (LINEMODE . 34)
    (SE   . 240)
    (SB   . 250)
    (WILL . 251)
    (WONT . 252)
    (DO   . 253)
    (DONT . 254)
    (IAC  . 255)))

(defun telnet-command-to-string (commands)
  "Return a list of telnet command as a string."
  (apply 'unibyte-string
         (mapcar (lambda (cmd)
                   (cdr (assoc cmd telnet-command-alist)))
                 commands)))

(defun ascii-server-filter (proc string)
  ;; (message (pp-to-string string))
  (setq string (string-as-unibyte string))
  (message (pp-to-string (string-to-list string)))
  (if (string-equal "\377\364\377\375" string)
    (process-send-string proc
                         (telnet-command-to-string
                          '(IAC WONT TIMING-MARK)))
    (let ((client (assoc proc ascii-clients)))
      (if (not (string-suffix-p "\n" string))
          ;; only part of one line
          (setf (nth 3 client) (concat (nth 3 client)
                                       string))
        ;; linebrak entered
        (setq string (string-trim (concat (nth 3 client) string)))
        (setf (nth 3 client) "")
        (if (null (cadr client))
            ;; at welcome screen
            (if (string-equal "q" string)
                ;; quit
                (progn
                  (process-send-string proc "byebye\r\n")
                  (delete-process proc))
              (if (member string (ascii-gallery))
                  (ascii-display client string)
                (process-send-string proc
                                     (concat
                                      "Sorry, I don't know this artwork, try again.\r\n"
                                      "> "))))
          ;; in display, then quit display
          (ascii-quit-display client)
          (process-send-string proc (concat ascii-clear-screen
                                            (ascii-welcome))))))))

(defun ascii-server-sentinel (proc msg)
  (message (concat "SENTINEL: " msg))
  (if (string-prefix-p "open from" msg)
      ;; new connections opened
      (progn
        (process-send-string proc (ascii-welcome))
        (setq ascii-clients
              (cons (list proc nil nil "")
                    ascii-clients)))
    ;; connection closed or deleted
    (let ((timer (caddr (assoc proc ascii-clients))))
      (when (timerp timer) (cancel-timer timer)))
    (setq ascii-clients (assq-delete-all proc ascii-clients))))

(defun ascii-display (client artwork)
  "Display artwork on CLIENT."
  (with-temp-buffer
    (insert-file-contents-literally
     (expand-file-name artwork ascii-gallery-root))
    (let* ((head (string-trim (thing-at-point 'line)))
           (type (car (last (split-string head)))))
      (if (string-equal "S" type)
          ;; still picture
          (ascii-display-still-picture client (current-buffer))
        ;; animation
        (ascii-display-animation client (current-buffer))))))

(defun ascii-quit-display (client)
  "Stop display and clear screen."
  (process-send-string (car client) ascii-show-cursor) ; show cursor
  (when (timerp (caddr client))
    (cancel-timer (caddr client)))
  (setf (cadr client) nil
        (caddr client) nil))

(defun ascii-display-still-picture (client buffer)
  (setf (cadr client) "S")
  (goto-char (point-min))
  (forward-line)
  (message (number-to-string (point)))
  (process-send-string (car client) ascii-clear-screen)
  (process-send-region (car client) (point) (point-max))
  ;; hide cursor, this only works after printing the picture
  (process-send-string (car client) ascii-hide-cursor))

(defun ascii-display-animation (client buffer)
  (let* ((frame-cons (ascii-get-frame-list buffer))
         (info       (car frame-cons))
         (frame-list (cdr frame-cons))
         (title  (nth 0 info))
         (width  (nth 1 info))
         (height (nth 2 info))
         (elapse (nth 3 info)))
    (process-send-string (car client)
                         (format (concat ascii-clear-screen ascii-hide-cursor
                                         "This is a %dx%d ASCIImation called\r\n"
                                         "  %s\r\n"
                                         "sit back and relax...")
                                 width height title))
    (setf (cadr  client) frame-list
          (caddr client) (run-with-timer 4 (/ elapse 1000)
                                         'ascii-one-frame proc))))

(defun ascii-get-frame-list (buffer)
  "Return a list of frames read from BUFFER."
  (with-current-buffer buffer
    (let (head title width height elapse frame-list)
      ;; get title
      (goto-char (point-min))
      (setq head (string-trim (thing-at-point 'line)))
      (setq title (string-trim-right
                   (string-remove-suffix "A" head)))
      ;; get width, height, and elapse between frames (in ms)
      (forward-line)
      (setq head (mapcar 'string-to-number
                    (split-string (string-trim (thing-at-point 'line)))))
      (setq width  (car head)
            height (cadr head)
            elapse (caddr head))
      ;; get frames
      (forward-line)
      (while (not (= (point) (point-max)))
        (let ((num (number-at-point))
              start end)
          (forward-line 1)              ; start of frame
          (setq start (point))
          (forward-line height)
          (setq end (point))            ; end of frame
          (setq frame-list
                (cons (cons num (ascii-prettify (buffer-substring start end)
                                                width height))
                      frame-list))))
      (cons (list title width height elapse)
            (nreverse frame-list)))))

(defun ascii-prettify (str width height)
  (concat
   (mapconcat (lambda (s)
                (concat s (make-string (- width (length s)) ? )))
              (let ((res (split-string str "\n")))
                (butlast res (- (length res) height)))
              "\r\n")
   "\r\n"))

(defun ascii-one-frame (proc)
  "Read one frame and send it to `proc'."
  (let* ((client (assoc proc ascii-clients))
         (frame-list (cadr client)))
    (if (null frame-list)
        ;; no more frames to display, cancel timer and close connection
        (progn
          (cancel-timer (caddr client))
          (delete-process proc)
          (setq ascii-clients
                (assq-delete-all proc ascii-clients)))
      (let ((count (caar frame-list))
            (frame (cdar frame-list)))
        ;; non-destructive way
        (setf (cadr client) (if (> count 1)
                                (cons (cons (- count 1) frame)
                                      (cdr frame-list))
                              (cdr frame-list)))
        ;; (process-send-string proc "\e[2J") ; clear screen
        ;; move cursor to top
        (process-send-string proc "\e[H")
        (process-send-string proc frame)))
    ))

(ascii-server-start)
