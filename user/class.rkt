#lang racket

(provide user%)


(define-serializable-class user%
  object%
  ;; (printable<%>)


  (define uid 0)
  (define account "")
  (define passwd "")
  (define name "")
  (define sex #\0)
  (define age 0)
  (define birthday (list 0000 00 00))
  (define email "")
  (define phone-number (list 00 000 0000 0000))
  (define nickname "")
  (define signup-date (list 0000 00 00))

  (define/public (get-uid) uid)
  (define/public (get-account) account)
  (define/public (get-passwd) passwd)
  (define/public (get-name) uid)
  (define/public (get-sex) sex)
  (define/public (get-age) age)
  (define/public (get-birthday) birthday)
  (define/public (get-email) email)
  (define/public (get-phone-number) phone-number)
  (define/public (get-nickname) nickname)
  (define/public (get-signup-date) signup-date)

  (define/public set-uid!
    (λ (new-uid)
      (if (zero? uid)
          (set! uid new-uid)
          (error 'set-uid! "The uid has been set!"))))

  (define/public set-account!
    (λ (new-account)
      (if (not (non-empty-string? account))
          (set! account new-account)
          (error 'set-account! "The account has been set!"))))

  (define/public set-passwd!
    (λ (new-passwd)
      (if (not (non-empty-string? passwd))
          (set! passwd new-passwd)
          (error 'set-passwd! "The passwd has been set!"))))

  (define/public set-name!
    (λ (new-name)
      (if (not (non-empty-string? name))
          (set! name new-name)
          (error 'set-name! "The name has been set!"))))

  (define/public set-sex!
    (λ (new-sex)
      (if (eqv? sex #\0)
          (set! sex new-sex)
          (error 'set-sex! "The sex has been set!"))))

  (define/public set-age!
    (λ (new-age)
      (if (zero? age)
          (set! age new-age)
          (error 'set-age! "The age has been set!"))))

  (define/public set-birthday!
    (λ (new-birthday)
      (if (equal? birthday '(0000 00 00))
          (set! birthday new-birthday)
          (error 'set-birthday! "The birthday has been set!"))))

  (define/public set-email!
    (λ (new-email)
      (if (not (non-empty-string? email))
          (set! email new-email)
          (error 'set-email! "The email has been set!"))))

  (define/public set-phone-number!
    (λ (new-phone-number)
      (if (equal? phone-number '(00 000 0000 0000))
          (set! phone-number new-phone-number)
          (error 'set-phone-number! "The phone-number has been set!"))))

  (define/public set-nickname!
    (λ (new-nickname)
      (if (not (non-empty-string? nickname))
          (set! nickname new-nickname)
          (error 'set-nickname! "The nickname has been set!"))))

  (define/public set-signup-date!
    (λ (new-signup-date)
      (if (equal? signup-date '(0000 00 00))
          (set! signup-date new-signup-date)
          (error 'set-signup-date! "The signup-date has been set!"))))

  (super-new))

