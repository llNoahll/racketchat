#lang racket

(require racket/tcp)

(define PORT 1234)

(define connections '())
(define (add-connection! conn) (set! connections (append connections (list conn))))
(define (rem-connection! conn) (set! connections (filter (λ (x) (not (eq? x conn))) connections)))

(define-struct connection (in out username))
(define connection-close!
  (λ (conn)
    (close-input-port  (connection-in conn))
    (close-output-port (connection-out conn))
    (rem-connection! conn)
    (broadcast* (string-append (connection-username conn) " has left."))))

(define broadcast
  (λ (msg sender-conn)
    (define send-to
      (λ (conn)
        (define exn-handler
          (λ (exception)
            (displayln "network error")
            (displayln exception)
            (rem-connection! conn)
            (broadcast* (string-append (connection-username conn) " has left."))))

        (with-handlers ([exn:fail:network? exn-handler])
          (displayln (string-append (connection-username sender-conn) ": " msg) (connection-out conn))
          (flush-output (connection-out conn)))))

    (thread
     (λ () (map send-to
                (filter (λ (conn)
                          (not (eq? conn sender-conn)))
                        connections))))))

(define broadcast*
  (λ (msg (sender-conn '()))
    (define send-to
      (λ (conn)
        (define exn-handler
          (λ (exception)
            (displayln "network error")
            (displayln exception)
            (rem-connection! conn)
            (broadcast* (string-append (connection-username conn) " has left."))))

        (with-handlers ([exn:fail:network? exn-handler])
          (displayln msg (connection-out conn))
          (flush-output (connection-out conn)))))

    (displayln connections)
    (displayln (filter (λ (conn)
                         (not (eq? conn sender-conn)))
                       connections))

    (if (null? sender-conn)
        (thread (λ () (map send-to connections)))
        (thread (λ () (map send-to
                           (filter (λ (conn)
                                     (not (eq? conn sender-conn)))
                                   connections)))))))

(define handle
  (λ (connection)
    (define exn-handler
      (λ (exception)
        (displayln "network error")
        (displayln exception)
        (rem-connection! connection)
        (broadcast* (string-append (connection-username connection) " has left."))))

    (with-handlers ([exn:fail:network? exn-handler])
      (define msg (read-line (connection-in connection)))

      (unless (eof-object? msg)
        (broadcast msg connection)
        (handle connection)))))

(define get-username
  (λ (in out)
    (define valid-username?
      (λ (name)
        (and (string? name)
             (not (zero? (string-length name))))))

    ;; TODO: actually do something useful here.
    (define exn-handler
      (λ (exception)
        (displayln "network error")
        (displayln exception)))

    (with-handlers ([exn:fail:network? exn-handler])
      (define name (read-line in))

      (cond [(not (valid-username? name))
             (write-byte 0 out) (flush-output out) ; tell the client it's name is invalid.
             (get-username in out)]
            [else
             (write-byte 1 out) (flush-output out) ; tell the client it's name is valid.
             name]))))

(define accept-and-handle
  (λ (listener)
    (define cust (make-custodian))

    (parameterize ([current-custodian cust])
      (define-values (in out) (tcp-accept listener))
      (define username (get-username in out))

      (when (string? username)
        (thread
         (λ ()
           (define conn (connection in out username))

           (add-connection! conn)
           (broadcast* (string-append username " has joined.") conn)
           (handle conn)
           (connection-close! conn)))))))


(define serve
  (λ (port)
    (define main-cust (make-custodian))

    (parameterize ([current-custodian main-cust])
      (define listener (tcp-listen port 5 #t))
      (define (loop)
        (accept-and-handle listener)
        (loop))

      (thread loop))

    (λ () (custodian-shutdown-all main-cust))))


(define stop (serve PORT))
