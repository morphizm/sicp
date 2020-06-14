#lang racket/base

(define (serialized-exchange account1 account2)
  (let [(serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (id1 (account1 'id))
        (id2 (account2 'id))]
    (if (> id1 id2)
        ((serializer1 (serializer2 exhange))
        account1
        account2)
        ((serializer2 (serializer1 exchange))
        account2
        account1))))

