(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args)))
  )
(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
                (let ((rest (nthcdr n source)))
                  (if (consp rest)
                    (rec rest (cons
                               (subseq source 0 n)
                               acc))
                    (nreverse
                     (cons source acc))))))
          (if source (rec source nil) nil))
  )

(defun longer (x y)
  (labels ((compare (x y)
                    (and (consp x)
                         (or (null y)
                             (compare (cdr x) (cdr y))))))
          (if (and (listp x) (listp y))
            (compare x y)
            (> (length x) (length y)))))

(defun flatten (x)
  (labels ((rec (x acc)
               (cond ((null x) acc)
                     ((atom x) (cons x acc))
                     (t (rec
                         (car x)
                         (rec (cdr x) acc))))))
  (rec x nil)))

(defun flatten_SBCL (x)
  (labels ((flatten-recursively (x flattening-list)
                                (cond ((null x) flattening-list)
                                  ((eq (type-of x) 'SB-IMPL::COMMA) (flatten-recursively (sb-impl::comma-expr x) flattening-list))
                                  ((atom x) (cons x flattening-list))
                                  (t (flatten-recursively (car x) (flatten-recursively (cdr x) flattening-list))))))
          (flatten-recursively x nil)))

(defun g!-symbol-p (s)
            (and (symbolp s)
                 (> (length (symbol-name s)) 2)
                 (string= (symbol-name s)
                          "G!"
                          :start1 0
                          :end1 2))
            )

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten_SBCL body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                      `(,s (gensym ,(subseq
                                     (symbol-name s)
                                     2))))
              syms)
         ,@body))))

(defmacro/g! nif (expr pos zero neg)
  `(let ((,g!result ,expr))
     (cond ((plusp ,g!result) ,pos)
       ((zerop ,g!result) ,zero)
       (t ,neg))
     ))

;(nif -3 (princ "+") (princ "0")(princ "-"))
(defun fact (x)
  (if (= x 0)
      1
      (* x (fact (- x 1))))
  )
(defun choose (n r)
  (/ (fact n)
     (fact (- n r))
     (fact r))
  )

(defun tarai (x y z)
  (if (<= x y)
    y
    (tarai
     (tarai (1- x) y z)
     (tarai (1- y) z x)
     (tarai (1- z) x y))))



(defun primep (n)
  #+sbcl (sb-int:positive-primep n))

(defun cortz (n)
  (if (= n 1)
    1
    (if (evenp n)
        (cortz (/ n 2))
        (cortz (+ (* n 3) 1)))))

(defun dots(num)
  (let ((dot-string ""))
   (labels ((dot (n tmp)
                (if (= n 0)
                  tmp
                  (dot (- n 1) (concatenate 'string tmp ".")))))
          (if(= num 0)
           dot-string
           (dot num dot-string))))
  )

(defun dots2(num)
  (let ((result ""))
    (do ((i 0 (+ i 1)))
      ((> i (- num 1) )result)
      (setf result (concatenate 'string result ".")))))



(defmacro/g! ntimes (n &rest body)
  `(do ((x 0 (+ x 1)))
     ((>= x ,n))
     ,@body))

(defmacro/g! for (var start stop &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
       ((> ,var ,gstop))
       ,@body)))

;(setf path (make-pathname :name "myfile1"))

;(setf str (open path :direction :output :if-exists :supersede))

;(close str)
