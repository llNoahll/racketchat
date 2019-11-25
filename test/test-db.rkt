#lang racket


(require datalog
         racket/serialize
         racket/fasl)

(define users
  (deserialize
   (fasl->s-exp
    (call-with-input-file "../database/users.db"
      (λ (in)
        (read in))))))

(define user-1
  (let ([in-users (datalog users
                           (? (normal-user USER)))])
    (hash-ref (if (list? in-users)
                  (car in-users)
                  in-users)
            'USER)))

(displayln (send user-1 get-uid))
(displayln (send user-1 get-account))
(displayln (send user-1 get-passwd))
(displayln (send user-1 get-name))
(displayln (send user-1 get-sex))
(displayln (send user-1 get-age))
(displayln (send user-1 get-birthday))
(displayln (send user-1 get-email))
(displayln (send user-1 get-phone-number))
(displayln (send user-1 get-nickname))
(displayln (send user-1 get-signup-date))

(datalog! users
          (? (uid 87264592 USER))
          (? (account "alifjq149709" USER))
          (? (passwd "qofijofeq" USER))
          (? (name "Noah WM" USER))
          (? (sex #\m USER))
          (? (age 20 USER))
          (? (birthday '(2092 04 21) USER))
          (? (email "noahstorym@gmail.com" USER))
          (? (phone-number '(86 130 2417 0184) USER))
          (? (nickname "NoahStoryM" USER))
          (? (signup-date '(2020 02 20) USER)))

(datalog users
         (? (name "Noah WM" USER)))
(datalog users
         (? (name NAME USER)))