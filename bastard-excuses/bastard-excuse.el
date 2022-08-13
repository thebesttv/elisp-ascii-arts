;;; The Bastard Operator From Hell-Style Excuse Server
;;; http://pages.cs.wisc.edu/~ballard/bofh/

(defun bastard-excuse-list ()
  (let (excuses line)
    (with-temp-buffer
      (insert-file-contents "bofh-excuses")
      (while (< (point) (point-max))
        (setq line (string-trim (thing-at-point 'line)))
        (forward-line)
        (when (not (string-empty-p line))
          (setq excuses (cons line excuses))))
      (nreverse excuses))))

(setq bastard-excuse-list (bastard-excuse-list))

(defun random-sample (list)
  (nth (random (length list)) list))

(defun bastard-make-excuse ()
  (random-sample bastard-excuse-list))

