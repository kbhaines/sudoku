(defparameter *dim* 9)

(defun range(low high) (loop for n from low below (1+ high) collect n))

(defun take(n lst) (subseq lst 0 n ))

(defun istrue(n) (eq n T))

(defun mand(x y) (and x y))

(defun mapindex (f lst) 
  (let ((idx 0))
    (mapcar (lambda(e) (progn 
                          (setf idx (1+ idx)) 
                          (funcall f idx e))) lst)))

(defparameter *grid* 
    (mapcar (lambda(_)(make-list *dim* :initial-element '- )) (range 1 *dim*)))

(defun row(n) (nth n *grid*))

(defun col(n) 
  (flet ((c (row)(nth n row)))
    (mapcar #'c *grid*)))

(defun cell(r c) (nth c (row r)))

(defun 9group-ref(r c) 
  (let ((cblock (truncate (/ c 3)))
        (rblock (truncate (/ r 3))))
    (+ cblock (* 3 rblock))))

(defun 9group(n) 
  (let ((ri (* 3 (truncate (/ n 3))))
        (ci (* 3 (truncate (mod n 3)))))
    (apply #'append 
      (loop for n from ri to (+ 2 ri) collect 
          (subseq (row n) ci (+ 3 ci))))
    ))

(defun possibles(r c) 
  (cond ((eq '- (cell r c))
      (let ((poss-row (set-difference (range 1 *dim*) (row r)))
            (poss-col (set-difference (range 1 *dim*) (col c)))
            (poss-group (set-difference (range 1 *dim*) (9group (9group-ref r c)))))
        (intersection poss-row (intersection poss-col poss-group))))
        (t ())))

(defun report-possibles()
  (loop for r from 0 below *dim* collect 
        (loop for c from 0 below *dim* collect
              (length (possibles r c)))))

(defun fill-simple-possibles()
  (loop for r from 0 below *dim* collect 
        (loop for c from 0 below *dim* do 
              (let ((pos (possibles r c)))
                (cond ((eq 1 (length pos))
                       (set-cell r c (first pos)))
                      (t ()))))))

(defun iterate() (progn (fill-simple-possibles) (print-grid)))

(defun set-row(n lst)
  (setf (nth n *grid*) lst))

(defun set-col(c lst)
  (let ((row 0)) 
    (flet ((setc (a) 
                 (progn 
                   (setf (nth c (nth row *grid*)) a)
                   (setf row (1+ row)))))
    (mapcar #'setc lst))))

(defun set-cell(r c val)
  (setf (nth c (nth r *grid* )) val))

(defun valid-board() 
  (flet ((all-rows-valid() 
           (reduce #'mand (mapcar #'unique-set (loop for i from 0 below *dim* collect (row i)))))
         (all-cols-valid()
           (reduce #'mand (mapcar #'unique-set (loop for i from 0 below *dim* collect (col i)))))
         (all-groups-valid()
           (reduce #'mand (mapcar #'unique-set (loop for i from 0 below *dim* collect (9group i))))))

    (and (all-rows-valid) (all-cols-valid) (all-groups-valid))))

(defun unique-set (lst) 
  (labels ((count-occurences(lst) (count (car lst) (cdr lst))))
    (eq 0 (reduce #'+ (maplist #'count-occurences (remove '- lst))))))

(defun print-grid () 
  (progn
    (loop for r from 0 below *dim* do (print (row r)))
    (print (valid-board))))

(defun is-finished() 
  (not (find '- (apply #'append *grid*))))

(defun is-blocked()
  (not (find '1 (apply #'append (report-possibles)))))

(defun solve()
  (loop
    (iterate)
    (when (is-finished) (return t))
    (when (is-blocked) (progn (print "blocked!") (return nil)))))

(set-row 1 '(1 2 3 4 5 - 7 8 9))
(set-col 1 '(5 6 7 8 1 2 3 9))

(defparameter *board1*
      '((- - -  - 1 -  7 3 -)
        (8 - -  - - 9  - 1 2)
        (1 - 5  - 2 -  8 4 -)

        (5 7 9  - - -  1 - -)
        (- - 8  3 - -  - - -)
        (- 4 -  - - -  9 - -)
        
        (- - -  6 - -  3 7 -)
        (7 5 -  - - 4  - - -)
        (- 2 -  - - -  5 8 4)))
(setf *grid* *board1*)

(print-grid)


