(use-modules (web socket server))

;; Respond to text messages by reversing the message.  Respond to
;; binary messages with "hello".
(define (handler data)
  (if (string? data)
      (string-reverse data)
      "hello"))

(run-server handler (make-server-socket #:port 9090))
