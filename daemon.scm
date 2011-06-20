(use-modules (scm schanjidic))
(use-modules (ice-9 rdelim))

(setlocale LC_ALL "")

;;(display "building dictironary…")
(let ((kanjidic (make-dict (cadr (command-line)))))
  ;;(display " done")
  ;;(newline)

  (let ((s (socket PF_INET SOCK_STREAM 0)))
    (setsockopt s SOL_SOCKET SO_REUSEADDR 1)
    (bind s AF_INET INADDR_ANY 47532)
    (listen s 5)
    
    ;;(simple-format #t "listening for clients in pid: ~S" (getpid))
    ;;(newline)
    
    (while #t
      (let* ((client-connection (accept s))
	     (client-details (cdr client-connection))
	     (client (car client-connection)))
	;;(simple-format #t "got new client connection: ~S"
	;;	       client-details)
	;;(newline)
	;;(simple-format #t "client address: ~S"
	;;	       (gethostbyaddr
	;;		(sockaddr:addr client-details)))
	;;(newline)
	  (let ((kanji (string-ref (read-line client) 0)))
	    ;;(display "looking for ")
	    ;;(write-char kanji)
	    ;;(newline)
	    (write (look-kanji kanji kanjidic)  client)
	    (close client))
	  (usleep 10000)))))