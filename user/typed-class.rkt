#lang typed/racket/no-check

;; (require racket/serialize)

(provide user%)


(define-serializable-class user%
  object%
  ;; (printable<%>)

  (: uid Natural)
  (define uid 0)

  (: account String)
  (define account "")

  (: passwd String)
  (define passwd "")

  (: name String)
  (define name "")

  (: sex Char)
  (define sex #\0)

  (: age Natural)
  (define age 0)

  (: birthday (List Natural Natural Natural))
  (define birthday (list 0000 00 00))

  (: email String)
  (define email "")

  (: phone-number (List Natural Natural Natural Natural))
  (define phone-number (list 00 000 0000 0000))

  (: nickname String)
  (define nickname "")

  (: signup-date (List Natural Natural Natural))
  (define signup-date (list 0000 00 00))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (: get-uid [-> Natural])
  (define/public (get-uid) uid)

  (: get-account [-> String])
  (define/public (get-account) account)

  (: get-passwd [-> String])
  (define/public (get-passwd) passwd)

  (: get-name [-> String])
  (define/public (get-name) name)

  (: get-sex [-> Char])
  (define/public (get-sex) sex)

  (: get-age [-> Natural])
  (define/public (get-age) age)

  (: get-birthday [-> (List Natural Natural Natural)])
  (define/public (get-birthday) birthday)

  (: get-email [-> String])
  (define/public (get-email) email)

  (: get-phone-number [-> (List Natural Natural Natural Natural)])
  (define/public (get-phone-number) phone-number)

  (: get-nickname [-> String])
  (define/public (get-nickname) nickname)

  (: get-signup-date [-> (List Natural Natural Natural)])
  (define/public (get-signup-date) signup-date)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (: set-uid! [-> Natural])
  (define/public set-uid!
    (λ (new-uid)
      (if (zero? uid)
          (set! uid new-uid)
          (error 'set-uid! "The uid has been set!"))))

  (: set-account! [-> String])
  (define/public set-account!
    (λ (new-account)
      (if (not (non-empty-string? account))
          (set! account new-account)
          (error 'set-account! "The account has been set!"))))

  (: set-passwd! [-> String])
  (define/public set-passwd!
    (λ (new-passwd)
      (if (not (non-empty-string? passwd))
          (set! passwd new-passwd)
          (error 'set-passwd! "The passwd has been set!"))))

  (: set-name! [-> String])
  (define/public set-name!
    (λ (new-name)
      (if (not (non-empty-string? name))
          (set! name new-name)
          (error 'set-name! "The name has been set!"))))

  (: set-sex! [-> Char])
  (define/public set-sex!
    (λ (new-sex)
      (if (eqv? sex #\0)
          (set! sex new-sex)
          (error 'set-sex! "The sex has been set!"))))

  (: set-age! [-> Natural])
  (define/public set-age!
    (λ (new-age)
      (if (zero? age)
          (set! age new-age)
          (error 'set-age! "The age has been set!"))))

  (: set-birthday! [-> (List Natural Natural Natural)])
  (define/public set-birthday!
    (λ (new-birthday)
      (if (equal? birthday '(0000 00 00))
          (set! birthday new-birthday)
          (error 'set-birthday! "The birthday has been set!"))))

  (: set-email! [-> String])
  (define/public set-email!
    (λ (new-email)
      (if (not (non-empty-string? email))
          (set! email new-email)
          (error 'set-email! "The email has been set!"))))

  (: set-phone-number! [-> (List Natural Natural Natural Natural)])
  (define/public set-phone-number!
    (λ (new-phone-number)
      (if (equal? phone-number '(00 000 0000 0000))
          (set! phone-number new-phone-number)
          (error 'set-phone-number! "The phone-number has been set!"))))

  (: set-nickname! [-> String])
  (define/public set-nickname!
    (λ (new-nickname)
      (if (not (non-empty-string? nickname))
          (set! nickname new-nickname)
          (error 'set-nickname! "The nickname has been set!"))))

  (: set-signup-date! [-> (List Natural Natural Natural)])
  (define/public set-signup-date!
    (λ (new-signup-date)
      (if (equal? signup-date '(0000 00 00))
          (set! signup-date new-signup-date)
          (error 'set-signup-date! "The signup-date has been set!"))))


  (super-new))

