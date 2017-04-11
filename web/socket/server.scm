;;; guile-websocket --- WebSocket client/server
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;;
;;; This file is part of guile-websocket.
;;;
;;; Guile-websocket is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation; either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; Guile-websocket is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with guile-websocket.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; WebSocket server.
;;
;;; Code:

(define-module (web socket server)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (fibers)  
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (web socket frame)
  #:use-module (web socket utils)
  #:export (make-server-socket
            run-server*))

;; See section 4.2 for explanation of the handshake.
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
  (let ((accept-key (make-accept-key (string-trim-both client-key))))
    (build-response #:code 101
                    #:headers `((upgrade . ("websocket"))
                                (connection . (upgrade))
                                (sec-websocket-accept . ,accept-key)))))

(define* (make-server-socket #:key
                             (host #f)
                             (addr (if host (inet-pton AF_INET host) INADDR_LOOPBACK))
                             (port 8080))
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (fcntl sock F_SETFD FD_CLOEXEC)
    (fcntl sock F_SETFL (logior O_NONBLOCK (fcntl sock F_GETFL)))
    (bind sock AF_INET addr port)
    sock))

(define (accept-new-client server-socket)
  (match (accept server-socket)
    ((client-socket . _) client-socket)))

(define (serve-client client-socket handler store)
  "Serve client connected via CLIENT-SOCKET by performing the HTTP
handshake and listening for control and data frames.  HANDLER is
called for each complete message that is received."
  (define (handle-data-frame type data)
    (let* ((result   (pk 'result (handler store (match type
                                          ('text   (utf8->string data))
                                          ('binary data)))))
           (response (cond
                      ((string? result)
                       (make-text-frame result))
                      ((bytevector? result)
                       (make-binary-frame result))
                      ((not result)
                       #f))))

      (when response
        (write-frame response client-socket))))

  (define (read-frame-maybe)
    (and (not (port-eof? client-socket))
         (read-frame client-socket)))

  ;; Perform the HTTP handshake and upgrade to WebSocket protocol.
  (let* ((request (read-handshake-request client-socket))
         (client-key (assoc-ref (request-headers request) 'sec-websocket-key))
         (response (make-handshake-response client-key)))
    (write-response response client-socket)
    (let loop ((fragments '())
               (type #f))
      (let ((frame (read-frame-maybe)))
        (cond
         ;; EOF - port is closed.
         ((not frame)
          (close-port client-socket))
         ;; Per section 5.4, control frames may appear interspersed
         ;; along with a fragmented message.
         ((close-frame? frame)
          ;; Per section 5.5.1, echo the close frame back to the
          ;; client before closing the socket.  The client may no
          ;; longer be listening.
          (false-if-exception
           (write-frame (make-close-frame (frame-data frame)) client-socket))
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

(define (client-loop port addr handler store)
  (setvbuf port 'block 1024)
  ;; Disable Nagle's algorithm.  We buffer ourselves.
  (setsockopt port IPPROTO_TCP TCP_NODELAY 1)
  (serve-client port handler store))
  
(define* (socket-loop handler server-socket store)
  (let loop ()
    (pk 'fuu)
    (match (pk 'client (accept server-socket SOCK_NONBLOCK))
      ((client . addr)
       (spawn-fiber (lambda () (pk (client-loop client addr handler store))))
       (loop)))))

(define (make-default-socket family addr port)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (fcntl sock F_SETFD FD_CLOEXEC)
    (fcntl sock F_SETFL (logior O_NONBLOCK (fcntl sock F_GETFL)))
    (bind sock family addr port)
    sock))

(define (run-server* handler)
  (lambda* (#:key (host #f)
                  (family AF_INET)
                  (addr (if host
                            (inet-pton family host)
                            INADDR_LOOPBACK))
                  (port 9090)
                  (socket (make-default-socket family addr port)))
    (listen socket 1024)
    (sigaction SIGPIPE SIG_IGN)
    (socket-loop handler socket (make-hash-table))))
