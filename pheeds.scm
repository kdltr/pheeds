(use irregex uri-common tcp6)

(define config-file (make-pathname (get-environment-variable "HOME") ".pheeds"))
(define email-from "pheeds@upyum.com")
(define email-to "kooda@upyum.com")

(include "gopher")

(define (notify line)
  (let ((res (read-menuline line)))
    (with-output-to-pipe (sprintf "sendmail -f ~A -t ~A" email-from email-to)
      (lambda ()
        (printf "Subject: [pheeds] ~A~%~%~A~%~%Original link: ~A~%"
                (substring (car (string-split line "\t")) 1)
                (with-input-from-request res read-string)
                (format-url res))))))

(define selector+date-regex
  (irregex
  '(: bos "0" (* any)
      (submatch-named year (= 4 numeric)) "-"
      (submatch-named month (= 2 numeric)) "-"
      (submatch-named day (= 2 numeric)) (+ any))))

(define (check-phlog url last-max-date)
  (let ((max-date last-max-date)
        (res (read-url url)))
    (with-input-from-request res
      (lambda ()
        (port-for-each
          (lambda (l)
            (and-let* ((match (irregex-match selector+date-regex l))
                       (date (string->number
                               (string-append
                                 (irregex-match-substring match 'year)
                                 (irregex-match-substring match 'month)
                                 (irregex-match-substring match 'day)))))
                 (when (> date last-max-date)
                   (notify l)
                   (set! max-date (max max-date date)))))
          read-line)))
  max-date))

(let ((updated-config (with-input-from-file config-file
                        (lambda ()
                          (port-map (lambda (args)
                                      (list (car args) (apply check-phlog args)))
                                    read)))))
  (with-output-to-file config-file
    (lambda ()
      (for-each write updated-config))))
