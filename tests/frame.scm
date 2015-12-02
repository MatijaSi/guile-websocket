(define-module (test-frame)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (web socket frame))

(define (hex-string->bytevector str)
  "Convert the hexadecimal encoded string STR to a bytevector."
  (define hex-char->int
    (match-lambda
     (#\0 0)
     (#\1 1)
     (#\2 2)
     (#\3 3)
     (#\4 4)
     (#\5 5)
     (#\6 6)
     (#\7 7)
     (#\8 8)
     (#\9 9)
     (#\a 10)
     (#\b 11)
     (#\c 12)
     (#\d 13)
     (#\e 14)
     (#\f 15)))

  (define (read-byte i)
    (let ((j (* 2 i)))
      (+ (hex-char->int (string-ref str (1+ j)))
         (* (hex-char->int (string-ref str j)) 16))))

  (let* ((len (/ (string-length str) 2))
         (bv  (make-bytevector len)))
    (let loop ((i 0))
      (if (= i len)
          bv
          (begin
            (bytevector-u8-set! bv i (read-byte i))
            (loop (1+ i)))))))

(define (bytevector->hex-string bv)
  (define int->hex-char
    (match-lambda
      (0 #\0)
      (1 #\1)
      (2 #\2)
      (3 #\3)
      (4 #\4)
      (5 #\5)
      (6 #\6)
      (7 #\7)
      (8 #\8)
      (9 #\9)
      (10 #\a)
      (11 #\b)
      (12 #\c)
      (13 #\d)
      (14 #\e)
      (15 #\f)))

  (list->string
   (append-map (lambda (x)
                 (let ((high (ash (logand x #xf0) -4))
                       (low  (logand x #x0f)))
                   (list (int->hex-char high)
                         (int->hex-char low))))
               (bytevector->u8-list bv))))

(define (call-with-input-bytevector bv proc)
  (let ((port (open-bytevector-input-port bv)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc port))
      (lambda ()
        (close-port port)))))

(define (frame->hex-string frame)
  (call-with-values open-bytevector-output-port
    (lambda (port get-bytevector)
      (write-frame frame port)
      (bytevector->hex-string (get-bytevector)))))

(define (bytevector->frame bv)
  (call-with-input-bytevector bv read-frame))

(define (hex-string->frame str)
  (bytevector->frame (hex-string->bytevector str)))

(test-begin "frame")

(test-equal "read unmasked text message"
  (hex-string->frame "810548656c6c6f")
  (make-text-frame "Hello"))

(test-equal "write unmasked text message"
  "810548656c6c6f"
  (frame->hex-string (make-text-frame "Hello")))

(test-equal "read masked text message"
  (hex-string->frame "818537fa213d7f9f4d5158")
  (make-text-frame "Hello" #vu8(#x37 #xfa #x21 #x3d)))

(test-equal "write masked text message"
  "818537fa213d7f9f4d5158"
  (frame->hex-string (make-text-frame "Hello" #vu8(#x37 #xfa #x21 #x3d))))

(test-equal "read fragmented umasked text message"
  (list (hex-string->frame "010348656c")
        (hex-string->frame "80026c6f"))
  (list (make-frame #f 'text #f (string->utf8 "Hel"))
        (make-frame #t 'continuation #f (string->utf8 "lo"))))

(test-equal "read unmasked ping frame"
  (hex-string->frame "890548656c6c6f")
  (make-ping-frame (string->utf8 "Hello")))

(test-equal "write unmasked ping frame"
  "890548656c6c6f"
  (frame->hex-string (make-ping-frame (string->utf8 "Hello"))))

(test-equal "read masked pong frame"
  (hex-string->frame "8a8537fa213d7f9f4d5158")
  (make-pong-frame (string->utf8 "Hello") #vu8(#x37 #xfa #x21 #x3d)))

(test-equal "write masked pong frame"
  "8a8537fa213d7f9f4d5158"
  (frame->hex-string
   (make-pong-frame (string->utf8 "Hello") #vu8(#x37 #xfa #x21 #x3d))))

(test-equal "read 256 bytes binary message in a single unmasked frame"
  (let* ((header (hex-string->bytevector "827e0100"))
         (len    (bytevector-length header))
         (frame  (make-bytevector (+ len 256) 170)))
    (bytevector-copy! header 0 frame 0 len)
    (bytevector->frame frame))
  (make-binary-frame (make-bytevector 256 170)))

(test-equal "read 64KiB binary message in a single unmasked frame"
  (let* ((header (hex-string->bytevector "827f0000000000010000"))
         (len    (bytevector-length header))
         (frame  (make-bytevector (+ len 65536) 170)))
    (bytevector-copy! header 0 frame 0 len)
    (bytevector->frame frame))
  (make-binary-frame (make-bytevector 65536 170)))

(test-equal "frame-length"
  10
  (frame-length (make-binary-frame (make-bytevector 10))))

(test-equal "frame-concatenate"
  #vu8(1 2 3 4 5)
  (frame-concatenate
   (list (make-binary-frame #vu8(1 2 3))
         (make-binary-frame #vu8(4 5)))))

(test-equal "text-frame->string"
  "Hello"
  (text-frame->string (make-text-frame "Hello")))

(test-equal "text-frames->string"
  "Hello"
  (text-frames->string
   (list (make-text-frame "Hel")
         (make-text-frame "lo"))))

(test-end "frame")


(exit (= (test-runner-fail-count (test-runner-current)) 0))
