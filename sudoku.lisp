(defparameter *dim* 9)

(defparameter *level* 0)

(defun range(low high) (loop for n from low below (1+ high) collect n))

(defparameter *grid* 
    (mapcar (lambda(_)(make-list *dim* :initial-element '- )) (range 1 *dim*)))

; deep copy: copy-tree
(defparameter *grid-stack* () )


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

(defparameter *board2*
  '((- - 5  - 2 3  4 - -)
    (2 3 -  4 - -  - - -)
    (- 1 -  - - 5  - 6 -)
    
    (- - -  - - 9  - - 8)
    (9 2 -  - - -  - - 1)
    (- 7 -  - 8 -  - - -)

    (5 - -  - - -  3 4 -)
    (6 - -  5 - -  - 9 -)
    (3 - -  1 4 2  - - -)))

; pg 74 bbos2
(defparameter *board3*
  '((- 9 -  - - 3  - 8 -)
    (- 6 -  - 5 -  - - -)
    (5 - -  - - -  - - 6)

    (- - -  - - -  7 4 1)
    (- - -  - 2 4  - - -)
    (- - 3  - - 5  - - -)

    (- - 5  - - -  8 - -)
    (- - -  6 - 9  1 - 7)
    (7 8 6  - 1 -  9 - -)
    ))

; pg 75 bbos2
(defparameter *board4*
  '((- - -  2 - -  - - -)
    (- 7 6  - - -  - - 4)
    (- - -  1 - 8  - 3 -)

    (9 - -  - - -  8 - -)
    (- - -  - - -  2 6 5)
    (1 - 3  - - -  7 - -)

    (- - -  3 - -  - 9 6)
    (- - -  7 4 -  - - -)
    (2 5 -  - - -  - - -)
    ))

; pg 77 bbos2
(defparameter *board5*
  '((- - -  - - -  - - -)
    (- - 4  - - -  8 5 -)
    (8 2 -  3 1 -  - - -)

    (3 - -  7 9 -  2 - -)
    (4 - 5  - - 3  9 8 -)
    (- - -  - - 8  - - 7)

    (- 8 2  - - 4  - 6 -)
    (5 1 -  9 - 2  - - -)
    (- - -  - 5 -  1 - -)
    ))

; pg 105 bbos2
(defparameter *board6*
  '((1 - 6  9 - 5  - - -)
    (- - -  - 4 -  - - -)
    (- - -  2 3 -  6 - 5)

    (- 6 1  5 - -  - 9 -)
    (5 - -  - - -  7 - -)
    (9 - -  - - -  5 - 4)

    (- - 8  4 - 9  - - -)
    (- 7 9  - - -  - 1 -)
    (- 4 -  - - -  - 8 3)
    ))

; pg 153 bbos2
(defparameter *board7*
  '((- - 8  - - 3  - - -)
    (9 3 -  - - -  - - 1)
    (- 7 -  2 - 6  - - 5)

    (3 2 -  - - -  - 1 -)
    (- - -  3 - -  - 2 -)
    (- - -  - - -  - 4 9)

    (- - -  7 9 -  - - 6)
    (- - 9  - 6 8  - - -)
    (5 - -  - - -  - - -)
    ))

; Bigissue 
(defparameter *board8*
  '((8 4 -  - - -  - - -)
    (- - 3  - 5 6  - - -)
    (- 6 -  8 4 -  - - -)

    (- - 9  4 - -  - 2 7)
    (- - 7  9 3 -  - 5 -)
    (- - -  - - -  - 8 -)

    (- - 4  3 - 5  9 - -)
    (- - -  - - -  5 - -)
    (6 - -  - 2 -  8 7 -)
    ))

; pg 160 bbos2
(defparameter *board9*
  '((7 - 9  - - -  - - 6)
    (4 2 -  5 - -  - - -)
    (- - -  4 3 -  - 7 -)

    (- 4 -  6 5 -  7 1 -)
    (1 - -  2 - -  - 9 -)
    (2 - 5  - - -  - 6 -)

    (- 6 -  - 7 -  8 - -)
    (- - 2  - - -  - - -)
    (- - -  8 - 6  - 3 -)
    ))

(defun matrix-transpose (matrix)
  (when matrix
    (apply #'mapcar #'list matrix)))


(defmacro lx (&body body) `(lambda(x) (,@body)))

(defun take(n lst) (subseq lst 0 n ))

(defun leave(n lst) (subseq lst 0 (- (length lst) n)))

(defun drop(n lst) (subseq lst n))

(defun istrue(n) (eq n T))

(defun mand(x y) (and x y))

(defun row(n) (nth n *grid*))

(defun col(n) (flet ((c (row)(nth n row))) (mapcar #'c *grid*)))

(defun cell(r c) (nth c (row r)))

(defun row-of-cell(cell) (first cell))
(defun col-of-cell(cell) (second cell))
(defun square-of-cell(cell) (9group-ref (first cell) (second cell)))
(defun cell-possibles (cell) (third cell))
(defun make-cell(row col possibles) (list row col possibles))

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

; lists possible values for cell at row r, col c.  Likely to be a superset of
; actual possible values, as it doesn't analyse the cell's row/col/group in
; depth.
(defun possibles(r c) 
  (cond ((eq '- (cell r c))
      (let ((poss-row (set-difference (range 1 *dim*) (row r)))
            (poss-col (set-difference (range 1 *dim*) (col c)))
            (poss-group (set-difference (range 1 *dim*) (9group (9group-ref r c)))))
        (intersection poss-row (intersection poss-col poss-group))))
        (t (list (cell r c)))))

(defun report-possibles()
    (apply #'append 
      (loop for r from 0 below *dim* collect 
            (loop for c from 0 below *dim* collect
                  (list r c (possibles r c))))))


(defun cell->rc(cell) 
  (list (truncate (/ cell *dim*)) (mod cell *dim*)))

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
    (if (not(valid-board)) (prin1 "BOARD FOOKED!"))))

(defun pr-grid(g)
  (loop for r in g do
        (print r)))

(defun is-finished() 
  (not (find '- (apply #'append *grid*))))

(setf *grid* *board4*)

(defun make() (ext:saveinitmem "exec" :init-function 'main :executable t :norc t))

(defun row-group(r possibles) (remove-if-not (lx eq r (first x)) possibles))

(defun col-group(c possibles) (remove-if-not (lx eq c (second x)) possibles))

(defun square-group(s possibles) (remove-if-not (lx eq s (square-of-cell x)) possibles))

(defun update-possible(cell possibles)
  (let ((cid (+ (col-of-cell cell) (* *dim* (row-of-cell cell)))))
    (setf (nth cid possibles) cell)))

(defun pair-combinations(lst)
  (apply #'append (maplist (lx mapcar (lambda(y)(list (first x) y)) (cdr x)) lst)))

(defun find-frequencies(grouping)
  (let ((freqs (sort (apply #'append (mapcar (lx cell-possibles x) (copy-tree grouping))) '< )))
        (mapcar (lx list x (count x freqs)) (remove-duplicates freqs))))

(defun hidden-singles(grouping)
  (let* ((freqs (find-frequencies grouping))
        (sgls (mapcar #'first (remove-if-not (lx eq 1 (second x)) freqs))))
    (loop for c in grouping 
            when (and (> (length (cell-possibles c)) 1) (intersection sgls (cell-possibles c))) collect (intersection sgls (cell-possibles c)))))


(defun hidden-pairs(group)
  (let* ((frqs (find-frequencies group))
         (perms (pair-combinations(mapcar #'first (remove-if (lx not(eq 2 (second x))) frqs)))))
    (loop for p in perms when (is-pair p group) collect p)))

(defun is-pair(pair grouping) 
  (let ((cs (remove-if-not (lx intersection pair (cell-possibles x)) grouping)))
    (eq 2 (length cs))))

(defun reduce-group(combo grouping)
    (loop for c in grouping
        when (intersection combo (cell-possibles c)) collect (make-cell (row-of-cell c) (col-of-cell c) combo)))

(defun reduce-possibles(filter possibles) 
  (let ((fps (funcall filter possibles)))
    (apply #'append (loop for chg in (append (hidden-singles fps) (hidden-pairs fps))
          collect (reduce-group chg fps)))))

; destructive update of possibles
(defun apply-reductions(rs possibles) 
  (mapcar (lx update-possible x possibles) rs))

(defun possibles->grid(poss)
  (let ((repr (mapcar (lx if(eq 1 (length(cell-possibles x)))(car(cell-possibles x)) '-) poss)))
    (mapcar (lx take *dim* (drop (* x *dim*) repr)) (range 0 8))))

(defun solve(grid) 
  (setf *grid* grid)
  (let ((ps (report-possibles)))
   (loop for n from 0 below *dim* do (apply-reductions (reduce-possibles (lx row-group n x) ps) ps))
   (loop for n from 0 below *dim* do (apply-reductions (reduce-possibles (lx col-group n x) ps) ps))
   (loop for n from 0 below *dim* do (apply-reductions (reduce-possibles (lx square-group n x) ps) ps))
   (cond ((is-finished)
          (if(not(valid-board))(progn (print "uh-oh; invalid board!!") (return-from solve nil)))
          (print "Hurrah!")
          (setf *grid* (possibles->grid ps))
          (print-grid)
          (return-from solve t))

         ((not(equal grid (possibles->grid ps)))
          (return-from solve (solve(possibles->grid ps)))))

  (print "BLOCKED!") nil ))

(defun test-boards()
  (mapcar #'solve-deep (list *board1* *board2* *board3* *board4* *board5* *board6* *board7* *board8* )))



(defun combos(lst)
  (if (< (length lst) 3) (return-from combos nil))
  (if (eq 3 (length lst)) (return-from combos (list lst)))
  (apply #'append (mapcar (lx list (car lst) (cadr lst) x) (cddr lst)) (list (combos (cdr lst)))))

(defun combos4(lst)
  (if (< (length lst) 4) (return-from combos4 nil))
  (if (eq 4 (length lst)) (return-from combos4 (list lst)))
  (apply #'append (mapcar (lx list (car lst) (cadr lst) (caddr lst) x) (cdddr lst)) (list (combos4 (cdr lst)))))


(defun group-combos(grouping)
  (let ((src (mapcar #'first (remove-if (lx < (second x) 2) (find-frequencies grouping)))))
    (combos src)))

(defun hidden-triples(grouping)
  (let ((cmbs (group-combos grouping))
        (ps (mapcar #'cell-possibles grouping)))
    (remove-if (lx not(eq 3 (second x))) (loop for c in cmbs collect (list c (length (remove nil (mapcar (lx intersection c x) ps))))))))


(defun solve-deep(grid)
  (let ((solved? (solve grid))
        (ps (report-possibles)))
    (setf grid *grid*)
    (if solved? (return-from solve-deep t)
      (loop for p in ps when (> (length(cell-possibles p)) 1) do
            (loop for pp in (cell-possibles p) do
                  (push (copy-tree grid) *grid-stack*)
                  (setf (nth (col-of-cell p) (nth (row-of-cell p) grid )) pp)
                  (format t "~%Going deep with supposition ~d,~d = ~d" (row-of-cell p) (col-of-cell p) pp)
                  (if (solve-deep grid) (return-from solve-deep t))
                  (setf grid (pop *grid-stack*))
                  (format t "~%Back from testing supposition ~d,~d = ~d" (row-of-cell p) (col-of-cell p) pp)
                  (pr-grid grid))))))

