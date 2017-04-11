(use-modules (fibers))
(use-modules (web socket server))

;; Respond to text messages by reversing the message.  Respond to
;; binary messages with "hello".
(define (handler store data)
  (pk 'store store)
  (if (string? data)
      (string-reverse data)
      "hello"))

(run-fibers (run-server* handler))
