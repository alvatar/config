(import (string pregexp))

(if (null? (cdr (command-line)))
    (begin
      (display "usage: playpls [PLS file]\n")
      (exit))
    ((let recur ((lines (call-with-input-file (cadr (command-line))
                          (lambda (port)
                            (read-all port read-line)))))
       (if (not (null? lines))
           (let ((l (car lines)))
             (let ((match (pregexp-match "http://.+" l)))
               (if match
                   (shell-command (string-append "mplayer " (car match)))
                   (recur (cdr lines)))))
           (begin
             (display "Error playing PLS --- No address info\n")
             (exit))))))
