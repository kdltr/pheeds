(define-record resource type host port selector)

(define (format-url res)
  (sprintf "gopher://~a:~a/~a~a"
           (resource-host res)
           (resource-port res)
           (resource-type res)
           (resource-selector res)))

(define (read-url str)
  (let* ((uri (uri-reference str))
         (_ (assert (eq? (uri-scheme uri) 'gopher)))
         (type+sel (string-intersperse (cdr (uri-path uri)) "/"))
         (type (if (equal? type+sel "") #\1 (string-ref type+sel 0)))
         (selector (if (equal? type+sel "") "" (substring type+sel 1))))
    (make-resource type
                   (uri-host uri)
                   (or (uri-port uri) 70)
                   selector)))

(define menuline-regex
  (irregex '(: (submatch-named type numeric)
               (submatch-named title (+ any)) "\t"
               (submatch-named selector (+ any)) "\t"
               (submatch-named host (+ any)) "\t"
               (submatch-named port (+ numeric)))))

(define (read-menuline str)
  (let ((match (irregex-match menuline-regex str)))
    (make-resource (irregex-match-substring match 'type)
                   (irregex-match-substring match 'host)
                   (irregex-match-substring match 'port)
                   (irregex-match-substring match 'selector))))

(define (request res)
  (let*-values (((in out) (tcp-connect (resource-host res) (resource-port res))))
    (fprintf out "~A\r\n" (resource-selector res))
    (close-output-port out)
    in))

(define (with-input-from-request resource thunk)
  (parameterize ((current-input-port (request resource)))
    (let ((res (thunk)))
      (close-input-port (current-input-port))
      res)))
