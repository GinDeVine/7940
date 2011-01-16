(defvar *goal* "Evolution?!")
(defvar *output-path* "/localhost/res/7940/wga-05-out.txt")
(defvar *cur-gen* '())
(defvar *num-of-fit* 20)
(defvar *max-rand* 4)
(defvar *chance-of-mutation* 90)
(defvar *best* (cons 0 ""))
(defvar *g* 0)
(defvar *start-time* (get-universal-time))

(defun cross-strings (s1 s2)
  (let ((r (random (min (length s1) (length s2)))))
    (concatenate 'string (subseq s1 0 r) (subseq s2 r))))

(defun random-char ()
  (code-char (random 200)))

(defun random-string ()
  (loop for i from 1 to (length (coerce *goal* 'list))
	with str = ""
	do (setq str (concatenate 'string str (coerce (list (random-char)) 'string)))
	finally (return str)))

(defun evaluate-string (s)
  (let ((s-list (coerce s 'list))
	(g-list (coerce *goal* 'list))
	(pac 0)
	(otc 0)
	(pac-val 0)
	(otc-val 0)
	(val 0))
    (loop for g-chr in g-list
	  for s-chr in s-list
	  while (and (characterp g-chr) (characterp s-chr))
	  do (if (equal g-chr s-chr)
		 (setq pac (+ pac 1))
	       (if (= (count s-chr *goal* :test #'equal)
		      (count s-chr s :test #'equal))
		   (setq otc (+ otc 1))))
	  finally (return))
    (if (not (= (length g-list) (length s-list)))
	(return-from evaluate-string 0))
    (setq pac-val (/ pac (length g-list)))
    (setq otc-val (/ otc (length g-list)))
    (setq val (+ pac-val (/ otc-val 2)))
    (if (> val (car *best*)) (log-to-file (format nil "A new best: ~a:  ~a" *g* (setq *best* (cons val s)))))
    val))  

(defun gen-by-fitness (gen)
  (let ((evd-list '()))
    (loop for str in gen
	  do (if (stringp str)
		 (push (cons (evaluate-string str) str) evd-list)))
    (sort evd-list #'> :key #'car)))

(defun create-first-gen ()
  (loop for i from 1 to *num-of-fit*
	with generation = '()
	do (push (random-string) generation)
	finally (return generation)))

(defun mutate-string (s)
  (setf (aref s (random (length s))) (random-char))
  s)

(Defun spawn-child (p1 p2)
  (let ((child (cross-strings (cdr p1) (cdr p2))))
    (if (< (random 100) *chance-of-mutation*)
	(setq child (mutate-string child)))
    child))
  

(defun get-new-gen (old-gen)
  (let ((old (gen-by-fitness old-gen))
	(fittest nil)
	(new nil))
    (setq fittest (subseq old 0 (min (length old) *num-of-fit*)))
    (loop for i from 0 to (random *max-rand*)
	  do (push (nth (random (length old)) old) fittest))
    (loop for s1 in fittest
	  do (loop for s2 in fittest
		   when (and (cons (not (equal s1 s2)) (and (listp s1) (listp s2))))
		   do (push (spawn-child s1 s2) new)))
    new))

(defun main ()
    (setq *cur-gen* (get-new-gen *cur-gen*)))

(setq *cur-gen* (create-first-gen))

(defun log-to-file (str)
  (with-open-file (*standard-output* *output-path* :direction :output
                                       :if-exists :append
                                       :if-does-not-exist :create)
      (write-line str *standard-output*))
  str)

(log-to-file (format nil "~% New run. Options are *n-o-f* ~a, *c-o-m* ~a *m-r* ~a." *num-of-fit* *chance-of-mutation* *max-rand*))
(log-to-file (format nil "The time is: ~a" *start-time*))
(log-to-file (format nil "Goal string is: ~% ~a ~%" *goal*))
(loop with x = 0
      while (< (car *best*) 1)
      while (< x 5E4)
      do (setq x (+ x 1))
      do (setq *g* x)
      do (main)
      finally (return (log-to-file (format nil "The best reached within ~a generations is ~% ~a ~% which had a fitness of ~a ~% Time of finish: ~a, which means, it used ~a to finish" *g* (cdr *best*) (car *best*) (get-universal-time) (- (get-universal-time) *start-time*)))))
