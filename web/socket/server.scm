(define-module (web socket server)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (base64)
  #:use-module (sha-1)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (web socket frame)
  #:export (make-server-socket
            run-server))

;; See section 4.2 for explanation of the handshake.
(define (handshake client-key)
  "Translate the base64 encoded CLIENT-KEY string into a base64
encoded acceptance key."
  (base64-encode
   (sha-1->bytevector
    (sha-1
     (string->utf8
      (string-append (string-trim-both client-key)
                     "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))))))

(define (read-handshake-request client-socket)
  "Read HTTP request from CLIENT-SOCKET that should contain the
headers required for a WebSocket handshake."
  ;; See section 4.2.1.
  (read-request client-socket))

(define (make-handshake-response client-key)
  "Return an HTTP response object for upgrading to a WebSocket
connection for the client whose key is CLIENT-KEY, a base64 encoded
string."
  ;; See section 4.2.2.
  (let ((accept-key (handshake client-key)))
    (build-response #:code 101
                    #:headers `((upgrade . ("websocket"))
                                (connection . (upgrade))
                                (sec-websocket-accept . ,accept-key)))))

(define* (make-server-socket #:key
                             (host #f)
                             (addr (if host (inet-aton host) INADDR_LOOPBACK))
                             (port 8080))
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (bind sock AF_INET addr port)
    sock))

(define (accept-new-client server-socket)
  (match (accept server-socket)
    ((client-socket . _) client-socket)))

(define (serve-client client-socket handler)
  "Serve client connected via CLIENT-SOCKET by performing the HTTP
handshake and listening for control and data frames.  HANDLER is
called for each complete message that is received."
  (define (handle-data-frame type data)
    (let ((result (handler (match type
                            ('text   (utf8->string data))
                            ('binary data)))))
      (write-frame (cond
                    ((string? result)
                     (make-text-frame result))
                    ((bytevector? result)
                     (make-binary-frame result)))
                   client-socket)))

  ;; Perform the HTTP handshake and upgrade to WebSocket protocol.
  (let* ((request (read-handshake-request client-socket))
         (client-key (assoc-ref (request-headers request) 'sec-websocket-key))
         (response (make-handshake-response client-key)))
    (write-response response client-socket)
    (let loop ((fragments '())
               (type #f))
      (let ((frame (read-frame client-socket)))
        (cond
         ;; Per section 5.4, control frames may appear interspersed
         ;; along with a fragmented message.
         ((close-frame? frame)
          ;; Per section 5.5.1, echo the close frame back to the
          ;; client before closing the socket.
          (write-frame (make-close-frame (frame-data frame)) client-socket)
          (close-port client-socket))
         ((ping-frame? frame)
          ;; Per section 5.5.3, a pong frame must include the exact
          ;; same data as the ping frame.
          (write-frame (make-pong-frame (frame-data frame)) client-socket)
          (loop fragments type))
         ((pong-frame? frame) ; silently ignore pongs
          (loop fragments type))
         ((first-fragment-frame? frame) ; begin accumulating fragments
          (loop (list frame) (frame-type frame)))
         ((final-fragment-frame? frame) ; concatenate all fragments
          (handle-data-frame type (frame-concatenate (reverse fragments)))
          (loop '() #f))
         ((fragment-frame? frame) ; add a fragment
          (loop (cons frame fragments) type))
         ((data-frame? frame) ; unfragmented data frame
          (handle-data-frame (frame-type frame) (frame-data frame))
          (loop '() #f)))))))

(define* (run-server handler #:optional (server-socket (make-server-socket)))
  "Run WebSocket server on SERVER-SOCKET.  HANDLER, a procedure that
accepts a single argument, is called for each complete message that
the server receives from a client.  When the message is in text
format, HANDLER is passed a string.  When the message is in binary
format, HANDLER is passed a bytevector.  HANDLER must return either a
string, bytevector, or #f.  Strings and bytevectors are sent to the
client in response to their message, and #f indicates that nothing
should be sent back."
  ;; TODO: Handle multiple simultaneous clients.
  (listen server-socket 1)
  (let loop ()
    (serve-client (accept-new-client server-socket) handler)
    (loop)))
