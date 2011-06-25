(use-modules (scm schanjidic))
(use-modules (ice-9 rdelim))
(setlocale LC_ALL "")

(define default_port 47532)

(let ((kanjidic (make-dict (cadr (command-line)))))

  (let ((s (socket PF_INET SOCK_STREAM 0)))
    (setsockopt s SOL_SOCKET SO_REUSEADDR 1)
    (bind s AF_INET INADDR_ANY default_port)
    (listen s 5)
    
    (while #t
      (let* ((client-connection (accept s))
	     (client-details (cdr client-connection))
	     (client (car client-connection)))
	(let ((kanji (substring (read-line client) 0 1)))
	  (write (look-kanji kanji kanjidic)  client)
	  (close client))
	(usleep 10000)))))