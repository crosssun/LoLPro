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
