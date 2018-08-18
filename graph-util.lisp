(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defun dot-label (exp)
  (let ((max-label-length 30))
    (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (> (length s) max-label-length)
          (concatenate 'string (subseq s 0 (- max-label-length 3))
                       "...")
          s))
      "")
    ))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
                (fresh-line)
                (princ (dot-name (car node)))
                (princ "[label=\"")
                (princ (dot-label node))
                (princ "\"];"))
        nodes))
(defun edges->dot (edges)
  (mapc (lambda (node)
                (mapc (lambda (edge)
                              (fresh-line)
                              (princ (dot-name (car node)))
                              (princ "->")
                              (princ (dot-name (car edge)))
                              (princ "[label=\"")
                              (princ (dot-label (cdr edge)))
                              (princ "\"];"))
                      (cdr node)))
        edges))

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}")
  )

(defun dot->png_osx (fname thunk)
  (with-open-file(*standard-output*
                  fname
                  :direction :output
                  :if-exists :supersede)
    (funcall thunk))
  (sb-ext:run-program "/usr/local/bin/dot" (list "-Tpng" "-O" fname)))

(defparameter *dotpath_osx* "/usr/local/bin/dot")

(defparameter *dotpath_win* "C:/Program Files (x86)/Graphviz2.38/bin/neato.exe")

      (defun dot->png (dotpath fname thunk)
        (with-open-file(*standard-output*
                        fname
                        :direction :output
                        :if-exists :supersede)
          (funcall thunk))
        (sb-ext:run-program dotpath (list "-Tpng" "-O" fname)))

(defun graph->png_osx (fname nodes edges)
  (dot->png_osx fname (lambda ()
                          (graph->dot nodes edges))))

(defun graph->png (dotpath fname nodes edges)
                            (dot->png dotpath fname (lambda ()
                                                    (graph->dot nodes edges))))

(defun uedges->dot (edges)
  (maplist (lambda (lst)
                   (mapc (lambda (edge)
                                 (unless (assoc (car edge)
                                                (cdr lst))
                                                (fresh-line)
                                                (princ (dot-name (caar lst)))
                                                (princ "--")
                                                (princ (dot-name (car edge)))
                                                (princ "[label=\"")
                                                (princ (dot-name (cdr edge)))
                                   (princ "\"];")))
                         (cdar lst)))
           edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png_osx (fname nodes edges)
  (dot->png_osx fname
            (lambda ()
                    (ugraph->dot nodes edges))))

  (defun ugraph->png (dotpath fname nodes edges)
                      (dot->png dotpath fname
                                (lambda ()
                                        (ugraph->dot nodes edges))))
