(defun n-elts (elt n)
  (if (> n 1)
    (list n elt)
    elt))

(defun compr (elt n lst)
  (if (null lst)
    (list (n-elts elt n))
    (let ((next (car lst)))
      (if (eql next elt)
        (compr elt (+ n 1) (cdr lst))
        (cons (n-elts elt n)
              (compr next 1 (cdr lst)))))))

(defun compress (x)
  (if (consp x)
    (compr (car x) 1 (cdr x))
    x))

(defun list-of (n elt)
  (if (zerop n)
    nil
    (cons elt (list-of (- n 1) elt))))

(defun uncompress (lst)
  (if (null lst)
    nil
    (let ((elt (car lst))
          (rest (uncompress (cdr lst))))
      (if (consp elt)
        (append (apply #'list-of elt)
                rest)
        (cons elt rest)))))

(compress '(1 1 1 0 1 0 0 0 0 1))
