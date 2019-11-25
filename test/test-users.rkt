#lang racket


(require datalog
         racket/serialize
         racket/fasl
         "../user/class.rkt"
         "../user/theory.rkt")


(define noah (new user%))


(define init-user!
  (λ (user key msg)
    ((get key) msg)
    (datalog users
             (! (key msg user)))))


(displayln (send noah get-uid))
(displayln (send noah get-account))
(displayln (send noah get-passwd))
(displayln (send noah get-name))
(displayln (send noah get-sex))
(displayln (send noah get-age))
(displayln (send noah get-birthday))
(displayln (send noah get-email))
(displayln (send noah get-phone-number))
(displayln (send noah get-nickname))
(displayln (send noah get-signup-date))


(send noah set-uid! 87264592)
(send noah set-account! "alifjq149709")
(send noah set-passwd! "qofijofeq")
(send noah set-name! "Noah WM")
(send noah set-sex! #\m)
(send noah set-age! 20)
(send noah set-birthday! '(2092 04 21))
(send noah set-email! "noahstorym@gmail.com")
(send noah set-phone-number! '(86 130 2417 0184))
(send noah set-nickname! "NoahStoryM")
(send noah set-signup-date! '(2020 02 20))


(displayln (send noah get-uid))
(displayln (send noah get-account))
(displayln (send noah get-passwd))
(displayln (send noah get-name))
(displayln (send noah get-sex))
(displayln (send noah get-age))
(displayln (send noah get-birthday))
(displayln (send noah get-email))
(displayln (send noah get-phone-number))
(displayln (send noah get-nickname))
(displayln (send noah get-signup-date))


;; (define users (make-theory))


(datalog users
         (! (normal-user noah))
         (! (uid (send noah get-uid) noah))
         (! (account (send noah get-account) noah))
         (! (passwd (send noah get-passwd) noah))
         (! (name (send noah get-name) noah))
         (! (sex (send noah get-sex) noah))
         (! (age (send noah get-age) noah))
         (! (birthday (send noah get-birthday) noah))
         (! (email (send noah get-email) noah))
         (! (phone-number (send noah get-phone-number) noah))
         (! (nickname (send noah get-nickname) noah))
         (! (signup-date (send noah get-signup-date) noah)))

(datalog users
         (? (normal-user USER)))

(define serilz-users (s-exp->fasl (serialize users)))

(call-with-output-file "../database/users.db" #:exists 'append
  (λ (out)
    (writeln serilz-users out)))