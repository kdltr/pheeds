(use irregex uri-common tcp6 simple-sha1 sql-de-lite)

(define email-from "pheeds@upyum.com")
(define email-to "kooda@upyum.com")

(define config-file (make-pathname (get-environment-variable "HOME") ".pheeds"))

(define *db* (open-database config-file))

(include "gopher.scm")


;; DATABASE

(define (create-database!)
  (exec (sql *db* "CREATE TABLE already_read (hash text primary key not null, unique(hash));"))
  (exec (sql *db* "CREATE TABLE phlogs (name text, url text, unique(name), unique(url));")))

(define (already-read? hash)
  (query fetch-value
         (sql *db* "SELECT * FROM already_read WHERE hash = ?")
         hash))

(define (mark-read! hash)
  (exec (sql *db* "INSERT INTO already_read VALUES (?);")
        hash))

(define (for-each-phlog proc)
  (for-each
    (lambda (r) (apply proc r))
    (query fetch-rows
           (sql *db* "SELECT * FROM phlogs;"))))

(define (add-phlog! name url)
  (exec (sql *db* "INSERT INTO phlogs VALUES (?, ?);")
        name url))

(define (del-phlog! name)
  (exec (sql *db* "DELETE FROM phlogs WHERE name = ?;")
        name))

(define (list-phlogs)
  (query fetch-rows
         (sql *db* "SELECT * FROM phlogs;")))


;; NETWORK

(define (notify! phlog-name res subject)
  (with-output-to-pipe
    (sprintf "sendmail -f ~A -t ~A" email-from email-to)
    (lambda ()
      (printf "Subject: [pheeds] ~A - ~A~%~%~A~%~%Original link: ~A~%"
              phlog-name
              subject
              (with-input-from-request res read-string)
              (format-url res)))))

(define (read-entries url)
  (let ((res (read-url url)))
    (with-input-from-request res
      (lambda ()
        (port-map (lambda (s)
                    (call-with-values
                      (lambda () (read-menuline s))
                      list))
                  read-gopher-line)))))

(define (text-link? res)
  (char=? (resource-type res) #\0))


(define (check-and-notify! phlog-name ents)
  (for-each
    (lambda (e)
      (let ((hash (string->sha1sum (format-url (car e)))))
        (when (and (text-link? (car e))
                   (not (already-read? hash)))
          (apply notify! phlog-name e)
          (mark-read! hash))))
    ents))

(define (check-all-phlogs!)
  (for-each-phlog
    (lambda (name url)
      (check-and-notify! name (read-entries url)))))


;; COMMAND LINE

(define (process-command-line args)
  (unless (null? args)
    (case (string->symbol (car args))
      ((add)
       (assert (>= (length args) 3))
       (add-phlog! (cadr args) (caddr args))
       (process-command-line (cdddr args)))
      ((del)
       (assert (>= (length args) 2))
       (del-phlog! (cadr args))
       (process-command-line (cddr args)))
      ((run)
       (check-all-phlogs!)
       (process-command-line (cdr args)))
      ((create)
       (create-database!)
       (process-command-line (cdr args)))
      )))

(cond-expand
  (compiling
   (process-command-line (command-line-arguments)))
  (else))
