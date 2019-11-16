#lang racket

; A Wicci Shim in Racket
; Exploiting a subset of Racket's Web-Server's features
; Consider converting this to Typed Racket!!

;; * External Modules

(require db) ; to talk with PostgreSQL
(require xml) ; for xexpr->string

; (require web-server/http)
; (require net/url)

; These seem to come in with web-server/servlet
; (require web-server/http/request-structs)
; (require web-server/http/response-structs)

; We need this to receive http requests
; relay them to responder functions
; and return http responses to the correct client
(require web-server/servlet)
(require web-server/servlet-env)

;; * Macros

; Notice our lexical convention of ending macro names with a colon:

(define-syntax-rule (param: parameter-name initial-value)
  (define parameter-name (make-parameter initial-value)) )

(param: debug-mode #f)

; How can we include filename and line number??
(define-syntax-rule (debug: exp ...)
  (when (debug-mode)
    (printf "~a -> ~s~n" 'exp exp) ... ) )

;; * Parameters

; Global parameters, functions simulating dynamic (not lexical) bindings.
; If it weren't for wanting to run tests without having to restart everything
; we could use simple global variables with define and set!.  Overengineered??
; (param: debug-mode #f) ; moved into macro section above!
(param: echo-mode #f)
(param: repl-mode #f)
(param: test-name "")
(param: http-port 8080)
(param: db-init-func "wicci_ready")
(param: db-init-func-format "SELECT ~a()")
(param: db-init-func-cmd (format (db-init-func-format) (db-init-func)))
(param: db-func "wicci_serve")
(param: db-func-format "SELECT h,v,b FROM ~a($1,$2,'_body_bin') AS _(h,v,b)")
(param: db-func-cmd (format (db-func-format) (db-func)))
(param: db-user (getenv "USER"))
(param: db-name "wicci1")
; these are initialized by open-db below
(param: db #f)
(param: db-pool #f)
(param: db-stmt #f)

; It would really be nice to be able to show only non-default parameters
; but that would suggest I add another level of abstraction and get rid
; of the enormous amount of redundancy I'm currently having with parameters
; in this program which until recently was so beautifully concise -jgd!!
(define (show-parameters)
  (debug: (debug-mode) (echo-mode) (repl-mode)
          (test-name)   (http-port)   (db-init-func)
          (db-init-func-format)   (db-init-func-cmd)   (db-func)
          (db-func-format)   (db-func-cmd)   (db-user)
          (db-name) ) )

;; * Program

; if run from the command line, allow most parameters to be overridden
(command-line
 #:once-each
 [("-d" "--debug") "display what's going on" (debug-mode #t)]
 [("-e" "--echo") "echo requests rather than serving them" (echo-mode #t)]
 [("-i" "--interact") "start an interactive repl" (repl-mode #t)]
 [("-t" "--test") n "test name" (test-name n)]
 [("-p" "--http-port") p "http server port" (http-port p)]
 [("-I" "--db-init-func") f "database function to setup new db connection" (db-init-func f)]
 [("-F" "--db-func") f "database function to serve http requests" (db-func f)]
 [("-U" "--db-user") u "database user account" (db-user u)]
 [("-N" "--db-name") d "database name" (db-name d)] )

; This is a heuristic to determine whether we're still in a development repl
; as opposed to running a compiled Wicci Shim from its own executable.
; Racket development environments typically use the executables drracket or gracket.
; Our shim excutable shouldn't end in the suffix "racket".  Is there a better way??
(define (in-devel-repl)
  (regexp-match-positions #rx"racket$" (path->string (find-system-path 'run-file))) )

;; * Database

; check with db to ensure db functions exist with required interfaces!
; write this!!
(define (check-db-funcs)
  ; wicci-init-func :: () -> text
  ; wicci-query-func :: (bytea, bytea) -> TABLE(_name text, _text text, _binary bytea)
  ; $1 :: bytea :: latin-1 :: http header lines separated by (ending in???) CRLF pairs
  ; $2 :: bytea :: empty or as Content-Type, Content-Length :: body sent by browser
  #t )

; Sets up our database connections and a prepared statement in parameters:
; db-pool: the global connection pool, not for direct use
; db: a thread-specific database connection
; db-stmt: a prepared statement, automagically prepared in current db
(define (open-db)
  (db-pool (connection-pool
            (λ ()
              (let ([pgc (postgresql-connect #:user (db-user) #:database (db-name))])
                (let ([db-init-func (query-value pgc (db-init-func-cmd))])
                  (debug: db-init-func) )
                pgc ) ) ))
  (db (virtual-connection (db-pool)))
  (db-stmt (virtual-statement
            (λ (dbc)
              (case (dbsystem-name dbc)
                ((postgresql) (db-func-cmd))
                (else error "unknown database system") ) ) )) )

;; * Wicci Web Interface

;; ** Preparing Client Data for Wicci

; Recreate the hunk of bytes the client sent which got
; parsed apart by the Racket Web Server.  Octets other than
; US-ASCII have no official interpretation.  There is a mime
; encoding and a JSON encoding which fit within a US-ASCII
; character set.  Non-US-ASCII octets and encoded characters
; can be interpreted by the Wicci if it considers it
; meaningful to do so!
(define (headers->bytes method-bytes uri headers-list)
  (let ([out (open-output-bytes)])
    (write-bytes method-bytes out)
    (write-bytes #" " out)
    (write-bytes (string->bytes/latin-1 uri) out)
    (write-bytes #" HTTP/1.1\r\n") ; perhaps a fabrication!!
    (for ([hv headers-list])
      (write-bytes (header-field hv) out)
      (write-bytes #": " out)
      (write-bytes (header-value hv) out)
      (write-bytes #"\r\n" out) )
    (get-output-bytes out) ) )

;; ** Wicci Responder
;; *** Wicci Response Row Support

; Returns s as an exact integer or #f.
; Parsing a general number seems expensive!!
(define (string->integer s)
  (let ( [n (string->number s 10 'number-or-false)] )
    (and (integer? n) (inexact->exact n)) ) )

; Return the value of a non-body row as a string value.
; The Wicci should be giving us US-ASCII for header values
; which if provided as a byte-string will satisfy a latin-1
; encoding.  It's an error to call this on a body row!!
(define (wicci-row-string row)
    ; header-name text-value bytes-value
  (let ( [text-val (vector-ref row 1)]
         [bytes-val (vector-ref row 2)]
         [header  (vector-ref row 0)] )
    ; assert: (not (eq? (non-empty-string? text-val) (< 0 (bytes-length bytes-val))))
    (if (non-empty-string? text-val)
        text-val
        (if (string-prefix? header "_body")
            (begin
              (log-wicci-row row "wicci-row-string can't convert this!")
              "" )
            (bytes->string/latin-1 bytes-val) ) ) ) )

; Return the value of a body-row as a string if possible or
; log a failure and return "".
; Body byte-strings might be encoded any which way.
; Currently we only use this function for debugging and
; maybe error reports to console, so it should be moot.
; Is this still true??
(define (wicci-body-row-string row content-type)
  ; header-name text-value bytes-value
  (let ( [text-val (vector-ref row 1)]
         [bytes-val (vector-ref row 2)]
         [header (vector-ref row 0)]
         [log (λ (msg) (log-wicci-row row (format "~a: ~a" wicci-body-row-string msg)))] )
    ; assert: (not (eq? (non-empty-string? text-val) (< 0 (bytes-length bytes-val))))
    (cond [(non-empty-string? text-val) text-val]
          [(not content-type) (log "No content-type!") ""]
          [(not (string-prefix? header" _body")) (log "Not a body row!") ""]
          [(regexp-match-positions #rx"charset-latin-1$" content-type)
           (bytes->string/latin-1 bytes-val) ]
          [(regexp-match-positions #rx"charset-utf-8$" content-type)
           (bytes->string/utf-8 bytes-val) ]
          [else (log "can't convert this!") "" ] ) ) )

; Return the value of a wicci response row as a byte-string
(define (wicci-row-bytes row)
  ; header-name text-value bytes-value
  (let ( [text-val (vector-ref row 1)]
         [bytes-val (vector-ref row 2)] )
    ; assert: (not (eq? (non-empty-string? text-val) (< 0 (bytes-length bytes-val))))
    (if (non-empty-string? text-val) (string->bytes/utf-8 text-val) bytes-val) ) )

; Return the value of a wicci response row as an integer or #f.
; Expensive?? Possibly generating a temporary string!!
; Not currently used!!
(define (wicci-row-integer row)
  (string->integer (wicci-row-string row)) )

; Return the regular header rows from the Wicci as a list of header structures
(define (wicci-rows-headers rows)
  (let ([hdr-rows (filter (λ (row) (not (string-prefix? (vector-ref row 0) "_"))) rows)])
    (map (λ (row) (make-header (string->bytes/utf-8 (vector-ref row 0)) (wicci-row-bytes row))) hdr-rows) ) )

; Return Wicci header rows by their header name
; Can we count on header name case being normalized??
; To lower-cabob-case or to Camel-Kabob-Case??
; We are assuming the Wicci trims its row fields!!
(define (wicci-header-rows header rows)
  (filter (λ (row) (string-ci=? header (vector-ref row 0))) rows) )

; Return the first Wicci header row by its header name;
; logs any duplicates!
(define (wicci-header-row header all-rows)
  (let ([rows (wicci-header-rows header all-rows)])
    (begin
      (and (pair? rows)
             (when (pair? (cdr rows))
               (log-wicci-rows rows "wicci-header-row unexpected duplicate(s)!") )
             (car rows) ) ) ) )

; Return the list of Wicci body rows
(define (wicci-body-rows rows)
  (filter (λ (row) (string-prefix? (vector-ref row 0) "_body")) rows) )

; Translate a wicci body row to bytes suitable for respond/full
(define (wicci-body-row-bytes row)
  ; should we check Content-Type or Content-Length??
  ; code for fetching large object body needs to be added here!!
  (wicci-row-bytes row) )

; Fetch any wicci body rows, if any, in the appropriate form
(define (wicci-body rows)
  (map wicci-body-row-bytes (wicci-body-rows rows)) )
; Return a list of elements taken from items where they exist
; and are not #f or the corresponding items in defaults.
(define (list-or-defaults items defaults)
  (if (null? items)
      defaults
      (if (null? defaults)
          items
          (cons (or (car items) (car defaults))
                (list-or-defaults (cdr items) (cdr defaults)) ) ) ) )

; _status | HTTP/1.1 200 OK
; Return a list of the three parts or #f for any which don't exist.
; We don't check what they look like!
(define (wicci-status-row-parts rows)
  (let ( [defaults '(#f #f #f)]
         [row (wicci-header-row "_status" rows)] )
    (if (not row)
        defaults
        (let ( [wicci-status (wicci-row-string row)] )
          (if (not (non-empty-string? wicci-status))
              defaults
              (list-or-defaults (string-split wicci-status) defaults) ) ) ) ) )

;; *** Wicci Xexpr Client Feedback Support

; get rid of any extra levels of nesting
(define (xexpr-trim sexp)
  (if (and (pair? sexp) (not (symbol? (car sexp))) (null? (cdr sexp))) (xexpr-trim (car sexp)) sexp) )

; glue two trimmed xexpr forms together.
; make sure that they dont 
(define (xexpr-join #:tag [tag 'div] . xexprs)
  (let ([xexprs (map xexpr-trim xexprs)])
    (cons tag xexprs) ) )

; For debugging and error reporting
; convert client's request into xexpr html
; so we can show them what we got
(define (request->xexpr req)
  (let* ([method (bytes->string/utf-8 (request-method req))]
         [uri (url->string (request-uri req))]
         [post-data (request-post-data/raw req)] )
    `( (h2 "The Request We Received")
       (dl (dt "Method") (dd ,method)
           (dt "URI") (dd ,uri)
           ,@(foldr
              (λ (hv lst)
                (cons (list 'dt (bytes->string/utf-8 (header-field hv)))
                      (cons (list 'dd (bytes->string/utf-8 (header-value hv))) lst) ) )
              '() (request-headers/raw req) )
           ,@(if (not post-data) '() `((dt "Body:") (dd ,(bytes->string/utf-8 post-data)))) ) ) ) )

(define (wicci-header-rows->xexpr rows)
  (let ( [hdr-rows
          (filter (λ (row) (not (string-prefix? (vector-ref row 0) "_body"))) rows) ])
    `(table ,@(foldr
               (λ (row)
                 `(tr
                   (td ,(vector-ref row 0))
                   (td ,(vector-ref row 1))
                   (td ,(vector-ref row 2)) ) )
               hdr-rows ) ) ) )

; Convert any body rows into xexpr-encoded html.
; Only intended for debugging of simple responses in a case
; where the Content-Type is text/html. 
(define (wicci-body-rows->xexpr rows content-type-row)
  (if (not content-type-row)
      "No Content-Type Row!"
      (let ( [content-type (wicci-row-string content-type-row)] )
        (cond [(not (non-empty-string? content-type)) "No Content-Type!"]
              [(not (string-prefix? content-type "text/html")) "Not text/html!" ]
              [else
               (let ([hdr-rows (filter (λ (row) (string-prefix? (vector-ref row 0) "_body")) rows)])
                 `(dl ,@(foldr
                         (λ (row accum)
                           (cons `(dt ,(vector-ref row 0))
                                 (cons `(dd ,(wicci-body-row-string row content-type)) accum) ) )
                         '() hdr-rows ) ) ) ] ) ) ) )

(define (wicci-response->xexpr rows content-type-row)
  `( (h2 "The Response We Received From The Wicci Database")
     ,@(wicci-header-rows->xexpr rows)
     ,@(wicci-body-rows->xexpr rows content-type-row) ) )

;; *** Wicci Error Reporting

; When the Wicci can't process a request, it should have internally
; logged the problem and returned us an error page to send to the
; client.

; We need the Wicci to return a request-id for reference just in case
; things go south in an unexpected manner.  This will require some
; refactoring!!

; In the rare cases where the Wicci itself or the Racket Web Server
; response system seems to have failed, we should
; (1) Write an error message to the "console" (redirected to a log
;     file when the Shim is run as a command)
; (2) Write an error report to the database, if possible
; (3) Include the request-id with both, if available

(require racket/pretty)

; Log a bad request which failed 
(define (log-web-request req)
  ; How well will this work if some of it is binary?
  (displayln "Problematic request:")
  (pretty-print req)
)

(define (log-wicci-rows rows [message #f])
  ; These are only the problematic rows.
  ; We need to provide context.
  (displayln (or message "Something went wrong with these rows:"))
  (pretty-print rows)
)

(define (log-wicci-row row [message #f])
  ; We need to provide context.
  (displayln (or message "Something went wrong with this row:"))
  (pretty-print row)
)

; Return proper xexpr list with given tag applied to given content
; which may or may not already be a list.
(define (xexpr-cons tag content)
  ( (if (pair? content) cons list) tag content ) )

; Create content for an error page to be sent to the user.
; Use the Racket xexpr format for the html code
; Allow for optional title, h1 and head contents.
; Collect multiple body parts and assemble it all together.
; Return the list of byte-string format 
(define (xexpr->content #:title/h1 [title/h1 #f] #:head [head '()] . body)
  (let* ( ; make sure title/h1 is a list, if it exists
          [title/h1 (if (or (not title/h1) (pair? title/h1)) title/h1 (list title/h1))]
          ; prepend a title to the list of head items if we've got one
          [head (if (not title/h1) head (cons (cons 'title title/h1) head))]
          ; prepend 'head to the list of head items
          [head (cons 'head head)]
          ; make sure body is a list
          [body (if (pair? body) body (list body))]
          ; unwrap body if it's a singleton list of a list
          [body (if (and (null? (cdr body)) (pair? (car body))) (car body) body)]
          ; prepend an h1 to the list of body items if we've got one
          [body (if title/h1 (cons (xexpr-cons 'h1 title/h1) body) body)] )
    ; return the list of byte-strings required by respond/full for html content
    (list (string->bytes/utf-8 (xexpr->string `(html ,head (body ,@body))))) ) )

;; *** Wicci Responder and Program Start

; Process a Web Request by sending it to the Wicci and
; converting the resulting rows into a response/full.
; If anything goes wrong, and for some bizarre reason the
; Wicci doesn't handle it, send the user an appropriate
; response page and also log the error.
(define (wicci-responder req)
  (define missings '())
  (define xexpr-missings '())
  (define (try name item)
    (when (not item)
      (set! missings (cons name missings)) ; push name on missings
      (set! xexpr-missings                 ; push xexpr details on expr-missings
            (cons '(li name) xexpr-missings) ) )
    item )                              ; return the item in any case
  (let* ( [method-bytes (try "method" (request-method req))]
          [uri (try "url" (url->string (request-uri req)))]
          [request-headers
           (try "headers" (headers->bytes method-bytes uri (request-headers/raw req))) ]
          [request-body (or (request-post-data/raw req) #"")] ) ; OK if none at this point
    (if (not (null? missings)) ; something essential's missing
        (begin
          (printf "wicci-responder failed to get: ~a~n" missings)
          (log-web-request req)
          (response/full
           500 #"Request Error"
           (current-seconds) TEXT/HTML-MIME-TYPE
           '()
           (xexpr->content #:title/h1 "We didn't fully get your request"
                           '(p "Required elements we failed to get:")
                           `(ul ,xexpr-missings) ) ) )
        ; Should we check that Content-Length is compatible with the body??
        ; Maybe the Racket Web Server checks that already??
        ; OK, we got what we need from the request, send it to the database:
        (let ( [rows (query-rows (db) (db-stmt) request-headers request-body)] )
          (if (or (not rows) (null? rows))
              (begin
                (displayln "wicci-responder: database query failed!")
                (log-web-request req)
                (response/full
                 500 #"Database Error1"
                 (current-seconds) TEXT/HTML-MIME-TYPE
                 '()
                 (xexpr->content #:title/h1 "The Wicci Failed to Process Your Request"
                                 (request->xexpr req) ) ) )
              ; We got a response from the Wicci Database, let's check it
              (let* ( [response-status-parts (wicci-status-row-parts rows)]
                      [status-code (string->integer (second response-status-parts))]
                      [status-msg (third response-status-parts)]
                      [response-headers (wicci-rows-headers rows)]
                      [content-type-row (wicci-header-row "content-type" rows)] )
                (if (not (and status-code status-msg response-headers)) ; we need these!
                    (begin
                      (try "status-code" status-code)
                      (try "status-message" status-msg)
                      (try "response-headers" response-headers)
                      (printf "wicci-responser: failed to get ~a~n" missings)
                      (log-web-request req)
                      (log-wicci-rows rows "What the Wicci gave us:")
                      (response/full
                       500 #"Database Error2"
                       (current-seconds) TEXT/HTML-MIME-TYPE
                       '()
                       (xexpr->content #:title/h1 "Wicci Database Error"
                                       (xexpr-join
                                        (request->xexpr req)
                                        '(p "The database failed to give us back:")
                                        `(ul ,xexpr-missings) ) ) ) )
                    ; OK, everything looks fine, let's send our response
                    (begin
                      (when (debug-mode)
                        (printf "About to call response/full with:~n")
                        (debug: status-code status-msg
                                (current-seconds) TEXT/HTML-MIME-TYPE
                                response-headers
                                (wicci-body rows) ) )
                        (response/full
                         status-code (string->bytes/latin-1 status-msg)
                         (current-seconds) TEXT/HTML-MIME-TYPE
                         response-headers
                         (wicci-body rows) ) ) ) ) ) ) ) ) )

; Start a web service using the given responder
(define (httpd responder)
  (serve/servlet responder #:command-line? #t #:servlet-regexp #rx"") )

; If not running interactively and no other requests,
; ==> Start The Wicci!
(when (not (or (echo-mode) (repl-mode) (test-name) (in-devel-repl)))
  (open-db)
  (httpd wicci-responder) )

;; * Testing & Parameter Management - Ignore for Normal Operation

;; ** Web Interface Tests

(define (hello-responder req)
  (response/xexpr
   '(html (head (title "Hello world!"))
          (body (p "Hey out there!")) ) ) )

; Anomaly!!:
; When we explicitly specify the header
; Content-Type: text-html; charset=utf-8
; we get Â® instead of just a ® on the last line
; and Content-Length: 785
; When we DON'T explicitly specify the Content-Type header
; we still get that header, we don't get the character anomaly
; and Content-Length: 721
; My Guess: We're causing the Racket Web Server to think that
; the body is NOT is utf-8 and to convert it "again"
; Experiment: Convert to latin-1 and provide the header
; Result: we get the header, we don't get the character anomaly
; and Content-Length: 784

; Echo the user's request back to them as html.
; Use response/full, xexpr and quasiquotation explicitly.
(define (echo-responder req)
  (let ( [title/h1 "Your Request echoed back from the Shim"] )
    (response/full
     200 #"OK"
     (current-seconds) TEXT/HTML-MIME-TYPE
     (list ; (make-header #"Content-Type" #"text-html; charset=utf-8")
      (make-header #"server" #"Wicci-Shim-Racket") )
     (list
      (string->bytes/utf-8
       (xexpr->string
        `(html
          (head
           ,(xexpr-cons 'title title/h1)
           (body
            ,(xexpr-cons 'h1 title/h1)
            ,@(request->xexpr req)
            (p "The Wicci will be ready for you " (em "Real Soon Now®!")) ) ) ) ) ) ) ) ) )

; If not running interactively and echo-mode is requested
; ==> Start Echoing Requests!
(when (and (echo-mode) (not (in-devel-repl)))
  (httpd echo-responder) )

;; ** Parameter Management

; run-wicci is a bit exotic.  It can be used to
; - run tests which override parameters for the duration of the call
; - run alternative shim variations simultaneously
; Sincere apologies for mind-numbing repetitious coding pattern -jgd!!
(define (run-wicci
         [thunk #f] ; procedure to run, if present
         ; optional argument for each parameter defaults to its current value
         #:debug-mode [new-debug-mode (debug-mode)]
         #:echo-mode [new-echo-mode (echo-mode)]
         #:repl-mode [new-repl-mode (repl-mode)]
         #:test-name [new-test-name (test-name)]
         #:http-port [new-http-port (http-port)]
         #:db-init-func [new-db-init-func (db-init-func)]
         #:db-init-func-format [new-db-init-func-format (db-init-func-format)]
         #:db-init-func-cmd [new-db-init-func-cmd #f]
         #:db-func [new-db-func (db-func)]
         #:db-func-format [new-db-func-format (db-func-format)]
         #:db-func-cmd [new-db-func-cmd #f]
         #:db-user [new-db-user (db-user)]
         #:db-name [new-db-name (db-name)] )
  ; with each parameter overriddem or not as the call has specified
  (parameterize
      ( [debug-mode new-debug-mode]
        [echo-mode new-echo-mode]
        [repl-mode new-repl-mode]
        [test-name new-test-name]
        [http-port new-http-port]
        [db-init-func new-db-init-func]
        [db-init-func-format new-db-init-func-format]
        [db-init-func-cmd (or new-db-init-func-cmd
                              (format new-db-init-func-format new-db-init-func) )]
        [db-func new-db-func]
        [db-func-format new-db-func-format]
        [db-func-cmd (or new-db-func-cmd (format new-db-func-format new-db-func))]
        [db-user new-db-user]
        [db-name new-db-name]
        [db (db)] [db-pool (db-pool)] [db-stmt (db-stmt)] )
    (when (debug-mode) (show-parameters))
    (open-db)
    (if thunk (thunk)
        (read-eval-print-loop) ) ) )

;; ** Database Tests

; A convenience constructor for test headers to be sent to the wicci as bytes,
; allowing them to be expressed as association lists.
(define (headers-alist->bytes alist)
  (let ([out (open-output-bytes)])
    (for ([pair alist])
      (for ([x (list (car pair) #": " (cdr pair) #"\r\n")])
        ; what if x is NOT a latin-1 byte string?
        (write-bytes x out) ) )
    (get-output-bytes out) ) )

(define test-body "Hello Wicci!")

(define test-body-utf-8 (string->bytes/utf-8 test-body))

(define test-body-latin-1 (string->bytes/latin-1 test-body))

; use typed-racket to require this to be a proper list of pairs of http request headers!!
(define (make-test-headers #:body [body #f] #:content-type [content-type #f])
  (headers-alist->bytes
   `( (#"Accept" . #"text/html, application/xhtml+xml, application/xml;q=0.9, image/webp, */*;q=0.8")
      (#"Host" . #"localhost:8000")
      ,@(if (not body) '()
            (list (cons
                   #"Content-Length"
                   (string->bytes/latin-1 (format "~a" (bytes-length body))) )) )
      ,@(if (not content-type) '()
            (list (cons #"Content-Type" content-type)) ) ) ) )

; create a data structure to hold compatible headers & body together!
(define test-headers-utf-8
  (make-test-headers #:body test-body-latin-1 #:content-type #"text/plain; charset=utf-8") )

(define (test-db-query)
  (query-rows (db) (db-stmt) test-headers-utf-8 test-body-utf-8) )
; '_body_bin' is coming back as a hex-encoded string "\\x..."
; '_body' is coming back with embedded \n where I wanted a newline!

;'(#("_status" "HTTP/1.1 200 OK" #"")
;  #("Server" "Wicci" #"")
;  #("Content-Length" "410" #"")
;  #("Content-Type" "text/html; charset=utf-8" #"")
;  #("_body"
;    "\n<!DOCTYPE HTML>\n<html>\n<head>\n<title>\nWicci HTML Echo Test\n</title>\n</head>\n<body>\n    <dl>\n<dt>Accept</dt> <dd>text/html, application/xhtml+xml, application/xml;q=0.9, image/webp, */*;q=0.8</dd>\n<dt>Host</dt> <dd>localhost</dd>\n<dt>Content-Length</dt> <dd>12</dd>\n<dt>Content-Type</dt> <dd>text/plain; charset=utf-8</dd>\n<dt></dt> <dd></dd>\n<dt> _body </dt>\n<dd>\nHello Wicci!\n</dd>\n    </dl>\n</body>\n</html>\n"
;    #""))

;; ** Test Environments

; create a test repl using postgres db "greg"
(define (test-in-greg [thunk #f])
  (run-wicci thunk
   #:debug-mode #t
   #:repl-mode (not thunk)
   #:http-port 8000
   #:db-user "greg"
   #:db-name ""
   #:db-init-func-cmd "select 'New database connection initialized!'"
   #:db-func-cmd "SELECT h,v,b FROM wicci_serve_echo($1,$2,'_body') AS _(h,v,b)" ) )

(define tests
  (list
   (cons 'echo-server (λ () (httpd echo-responder)))
   (cons 'greg-db (λ () (test-in-greg test-db-query)))
   (cons 'greg-wicci (λ () (test-in-greg (λ () (httpd wicci-responder)) ))) ) )

(define (run name)
  (let ( [pair (assoc name tests)] )
    (if (not pair)
        (printf "Test ~a not found~n" name)
        ((cdr pair)) ) ) )

; If not running interactively and we have a test
; ==> Run The Test!

(when (and (test-name) (not (in-devel-repl)))
    (run (test-name)) )

; If not running interactively and repl-mode is requested
; ==> Start a REPL!
(when (and (repl-mode) (not (in-devel-repl)))
  (read-eval-print-loop) )
