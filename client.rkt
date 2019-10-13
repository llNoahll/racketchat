#lang typed/racket

(require racket/tcp typed/racket/gui typed/racket/draw)


(: SERVER-IP String)
(define SERVER-IP "127.0.0.1")

(: SERVER-PORT Natural)
(define SERVER-PORT 1234)


(define login-dialog (instantiate dialog% ("RacketChat")))
(define login-panel
  (new horizontal-panel%
       [parent login-dialog]
       [alignment '(center center)]))
(define username-field
  (new text-field%
       [parent login-dialog]
       [label "login or choose a username"]))
(define window (new frame% [label "RacketChat"]))
(define editor-canvas
  (new editor-canvas%
       [parent window]
       [label "Chat"]
       [min-height 300]
       [min-width 600]))
(define text (new text%))
(define hpanel (new horizontal-panel% [parent window]))
(define input-field (new text-field% [parent hpanel] [label "Input"]))


(: exn-handler [-> exn:fail:network Void])
(define exn-handler
  (λ (exception)
    (send text insert "Error sending data to server\n")))


(: setup-send-button [-> Input-Port Output-Port String Void])
(define setup-send-button
  (λ (in out username)
    (: send-btn-callback [-> (Instance Button%) (Instance Control-Event%)
                             (U Thread Void)])
    (define send-btn-callback
      (λ (button event)
        (with-handlers ([exn:fail:network? exn-handler])
          (thread
           (λ ()
             (unless (zero? (string-length (send input-field get-value)))
               (displayln (send input-field get-value) out)
               (flush-output out)
               (send text insert
                     (string-append username ": "
                                    (send input-field get-value) "\n"))
               (send input-field set-value "")))))))

    (define send-btn
      (new button%
           [parent hpanel]
           [label "Send"]
           [callback send-btn-callback]))

    (void)))


(: handle-bad-username [-> (U Input-Port Null) (U Output-Port Null) Any Void])
(define handle-bad-username
  (λ (in out username)
    ;; TODO: maybe we'll use the username in the future.
    (send username-field set-value "")
    (send username-field set-label "Username invalid")

    (unless (null? out)
      (close-output-port out))
    (unless (null? in)
      (close-input-port in))))

(: handle-post-login [-> Input-Port Output-Port String (U Thread Void)])
(define handle-post-login
  (λ (in out username)
    (setup-send-button in out username)
    (send login-dialog show #f)
    (send window show #t)
    (send text insert "Joined chat\n")

    (with-handlers ([exn:fail:network? exn-handler])
      (thread
       (thunk
        (let msg-checker ([port (sync/timeout 0 in)])
          (unless (eq? #f port)
            (define msg (read-line port))

            (unless (eof-object? msg)
              (send text insert
                    (string-append msg "\n"))))
          (msg-checker (sync/timeout 0 in))))))))


(: login [-> String String Natural (U Thread Void)])
(define login
  (λ (username ip port)
    (define login-cust (make-custodian))

    (parameterize ([current-custodian login-cust])
      (with-handlers ([exn:fail:network? exn-handler])
        (define-values (in out) (tcp-connect/enable-break SERVER-IP SERVER-PORT))

        (displayln username out)
        (flush-output out)
        (thread
         (λ ()
           (let ([flag (read-byte in)])
             (if (or (eof-object? flag)
                     (zero? flag))
                 (handle-bad-username in out username)
                 (handle-post-login   in out username)))))))))


(: login-callback [-> (Instance Button%) (Instance Event%) (U Thread Void)])
(define login-callback
  (λ (button event)
    (: valid-username? [-> Any Boolean : #:+ (and String (! ""))])
    (define valid-username?
      (λ (name)
        (define-predicate empty-string? "")

        (and (string? name)
             (not (empty-string? name)))))

    (define username (send username-field get-value))


    (if (valid-username? username)
        (login username SERVER-IP SERVER-PORT)
        (handle-bad-username '() '() username))))

(: cancel-callback [-> (Instance Button%) (Instance Event%) Nothing])
(define cancel-callback
  (λ (button event)
    (exit)))


(new button% [parent login-panel] [label "Login" ] [callback login-callback])
(new button% [parent login-panel] [label "Cancel"] [callback cancel-callback])


(: start [-> Void])
(define start
  (λ ()
    (send editor-canvas enable #f)
    (send editor-canvas set-editor text)
    (send login-dialog show #t)))


(start)