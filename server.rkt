#lang typed/racket

(require racket/tcp)


(: PORT Natural)
(define PORT 1234)


(define-struct connection
  ([in : Input-Port]
   [out : Output-Port]
   [username : String]))

(: connections (Listof connection))
(define connections '())

(: add-connection! [-> connection Void])
(define add-connection!
  (λ (conn)
    (set! connections (append connections (list conn)))))

(: rem-connection! [-> connection Void])
(define rem-connection!
  (λ (conn)
    (set! connections (filter (λ ([x : connection]) (not (eq? x conn)))
                              connections))))

(: connection-close! [-> connection Thread])
(define connection-close!
  (λ (conn)
    (close-input-port  (connection-in conn))
    (close-output-port (connection-out conn))
    (rem-connection! conn)
    (broadcast* (string-append (connection-username conn) " has left."))))


(: broadcast [-> String connection Thread])
(define broadcast
  (λ (msg sender-conn)
    (: send-to [-> connection (U Thread Void)])
    (define send-to
      (λ (conn)
        (: exn-handler [-> exn:fail:network Thread])
        (define exn-handler
          (λ (exception)
            (displayln "network error")
            (displayln exception)
            (rem-connection! conn)
            (broadcast* (string-append (connection-username conn)
                                       " has left."))))

        (with-handlers ([exn:fail:network? exn-handler])
          (displayln (string-append (connection-username sender-conn)
                                    ": " msg)
                     (connection-out conn))
          (flush-output (connection-out conn)))))

    (thread
     (λ () (map send-to
                (filter (λ ([conn : connection])
                          (not (eq? conn sender-conn)))
                        connections))))))

(: broadcast* [->* (String) ((U connection Null)) Thread])
(define broadcast*
  (λ (msg (sender-conn '()))
    (: send-to [-> connection (U Thread Void)])
    (define send-to
      (λ (conn)
        (: exn-handler [-> exn:fail:network Thread])
        (define exn-handler
          (λ (exception)
            (displayln "network error")
            (displayln exception)
            (rem-connection! conn)
            (broadcast* (string-append (connection-username conn)
                                       " has left."))))

        (with-handlers ([exn:fail:network? exn-handler])
          (displayln msg (connection-out conn))
          (flush-output (connection-out conn)))))

    (if (null? sender-conn)
        (thread (λ () (map send-to connections)))
        (thread (λ () (map send-to
                           (filter (λ ([conn : connection])
                                     (not (eq? conn sender-conn)))
                                   connections)))))))


(: handle [-> connection (U Thread Void)])
(define handle
  (λ (connection)
    (: exn-handler [-> exn:fail:network Thread])
    (define exn-handler
      (λ (exception)
        (displayln "network error")
        (displayln exception)
        (rem-connection! connection)
        (broadcast* (string-append (connection-username connection)
                                   " has left."))))

    (with-handlers ([exn:fail:network? exn-handler])
      (define msg (read-line (connection-in connection)))

      (unless (eof-object? msg)
        (broadcast msg connection)
        (handle connection)))))

(: get-username [-> Input-Port Output-Port String])
(define get-username
  (λ (in out)
    (: valid-username? [-> Any Boolean :
                           #:+ (and String (! ""))
                           #:- (or "" (! String))])
    (define valid-username?
      (lambda (name)
        (define-predicate empty-string? "")

        (and (string? name)
             (not (empty-string? name)))))

    ;; TODO: actually do something useful here.
    (: exn-handler [-> exn:fail:network String])
    (define exn-handler
      (λ (exception)
        (displayln "network error")
        (displayln exception)

        ;; return a valid name ("").
        ""))

    (with-handlers ([exn:fail:network? exn-handler])
      (define name (read-line in))

      (cond [(valid-username? name)
             (write-byte 1 out) (flush-output out) ; tell the client it's name is valid.
             name]
            [else
             (write-byte 0 out) (flush-output out) ; tell the client it's name is invalid.
             (get-username in out)]))))

(: accept-and-handle [-> TCP-Listener Thread])
(define accept-and-handle
  (λ (listener)
    (define cust (make-custodian))

    (parameterize ([current-custodian cust])
      (define-values (in out) (tcp-accept listener))

      (define username (get-username in out))

      (thread
       (λ ()
         (define conn (connection in out username))

         (add-connection! conn)
         (broadcast* (string-append username " has joined.") conn)
         (handle conn)
         (connection-close! conn))))))


(: serve [-> Natural [-> Void]])
(define serve
  (λ (port)
    (define main-cust (make-custodian))

    (parameterize ([current-custodian main-cust])
      (define listener (tcp-listen port 5 #t))

      (: loop [-> Void])
      (define loop
        (λ ()
          (accept-and-handle listener)
          (loop)))

      (thread loop))

    (λ () (custodian-shutdown-all main-cust))))


(: stop [-> Void])
(define stop (serve PORT))
