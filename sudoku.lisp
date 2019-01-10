
(defvar *clear* (format nil "~c[2J" #\Escape))
(defvar *home* (format nil "~c[H" #\Escape))

(defparameter *dim* 9)

(defun range(low high) (loop for n from low below (1+ high) collect n))

(defparameter *grid* 
    (mapcar (lambda(_)(make-list *dim* :initial-element '- )) (range 1 *dim*)))

(defparameter *gg* '(
- - -  - 1 -  7 3 -
8 - -  - - 9  - 1 2
1 - 5  - 2 -  8 4 -

5 7 9  - - -  1 - -
- - 8  3 - -  - - -
- 4 -  - - -  9 - -

- - -  6 - -  3 7 -
7 5 -  - - 4  - - -
- 2 -  - - -  5 8 4
))

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

(defun make() (ext:saveinitmem "exec" :init-function 'main :executable t :norc t))

; fun little macro to save typing 'lambda(x)(...)'... just because
(defmacro lx (&body body) `(lambda(x) (,@body)))

(defun take(n lst) (subseq lst 0 n ))
(defun leave(n lst) (subseq lst 0 (- (length lst) n)))
(defun slice(n l lst) (subseq lst n (+ n l)))
(defun drop(n lst) (subseq lst n))
(defun mand(x y) (and x y))

; functions for manipulating grids and cells
(defun row(grid n) (nth n grid))
(defun col(grid n) (flet ((c (row)(nth n row))) (mapcar #'c grid)))
(defun cell(grid r c) (nth c (row grid r)))
(defun row-of-cell(cell) (first cell))
(defun col-of-cell(cell) (second cell))
(defun square-of-cell(cell) (square-ref (first cell) (second cell)))
(defun cell-possibles (cell) (third cell))
(defun make-cell(row col possibles) (list row col possibles))
(defun set-cell(grid r c val) (setf (nth c (nth r grid)) val))

(defun square-ref(r c) 
  (let ((cblock (truncate (/ c 3)))
        (rblock (truncate (/ r 3))))
    (+ cblock (* 3 rblock))))

(defun square(grid n) 
  (let ((ri (* 3 (truncate (/ n 3))))
        (ci (* 3 (truncate (mod n 3)))))
    (apply #'append 
      (loop for n from ri to (+ 2 ri) collect 
          (subseq (row grid n) ci (+ 3 ci))))))

; list all possible values for all cells in the given grid
(defun report-possibles(grid)
    (apply #'append 
      (loop for r below *dim* collect 
            (loop for c below *dim* collect
                  (list r c (possibles grid r c))))))

; lists possible values for grid cell at row r, col c.  Likely to be a superset of
; actual possible values, as it doesn't analyse the cell's row/col/group in
; depth.
(defun possibles(grid r c) 
  (cond ((eq '- (cell grid r c))
      (let ((poss-row (set-difference (range 1 *dim*) (row grid r)))
            (poss-col (set-difference (range 1 *dim*) (col grid c)))
            (poss-group (set-difference (range 1 *dim*) (square grid (square-ref r c)))))
        (intersection poss-row (intersection poss-col poss-group))))
        (t (list (cell grid r c)))))

(defun valid-board(grid) 
  (let ((all-rows-valid (reduce #'mand (mapcar #'unique-set (loop for i from 0 below *dim* collect (row grid i)))))
         (all-cols-valid (reduce #'mand (mapcar #'unique-set (loop for i from 0 below *dim* collect (col grid i)))))
         (all-groups-valid (reduce #'mand (mapcar #'unique-set (loop for i from 0 below *dim* collect (square grid i))))))

    (and all-rows-valid all-cols-valid all-groups-valid)))

(defun unique-set (lst) 
  (labels ((count-occurences(lst) (count (car lst) (cdr lst))))
    (eq 0 (reduce #'+ (maplist #'count-occurences (remove '- lst))))))

(defun print-grid (grid) 
  (progn
    (loop for r from 0 below *dim* do (print (row grid r)))
    (if (not(valid-board grid)) (prin1 "BOARD FOOKED!"))))

(defun pr-grid(g)
  (loop for r in g for rc below 9 do
        (let ((g1 (slice 0 3 r))
              (g2 (slice 3 3 r))
              (g3 (slice 6 3 r)))
          (if (or (eq 3 rc) (eq 6 rc)) (princ #\Newline))
          (format t "~{~a ~a ~a~}  ~{~a ~a ~a~}   ~{~a ~a ~a~}~%" g1 g2 g3))))

(defun is-finished(grid) (not (find '- (apply #'append grid))))

(defun solved(grid) (and (is-finished grid) (valid-board grid)))


; functions to filter possibles down to groups
(defun row-group(r possibles) (remove-if-not (lx eq r (first x)) possibles))
(defun col-group(c possibles) (remove-if-not (lx eq c (second x)) possibles))
(defun square-group(s possibles) (remove-if-not (lx eq s (square-of-cell x)) possibles))

(defun pair-combinations(lst)
  (apply #'append (maplist (lx mapcar (lambda(y)(list (first x) y)) (cdr x)) lst)))

(defun find-frequencies(grouping)
  (let ((freqs (apply #'append (mapcar (lx cell-possibles x) grouping))))
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

(defun solve(grid) 
  (labels ((solvex (grid ps)
   (loop for n from 0 below *dim* do (apply-reductions (reduce-possibles (row-group n ps)) ps))
   (loop for n from 0 below *dim* do (apply-reductions (reduce-possibles (col-group n ps)) ps))
   (loop for n from 0 below *dim* do (apply-reductions (reduce-possibles (square-group n ps)) ps))
   (let ((newgrid (possibles->grid ps)))
     (cond ((is-finished newgrid)
            (if(not(valid-board newgrid))
              (progn (print "uh-oh; invalid board!!") (return-from solvex newgrid)))
            (return-from solvex newgrid))

           ((not(equal newgrid grid ))
            (return-from solvex (solvex newgrid ps))))
     (print 'Blocked)
     newgrid)))
     (solvex grid (report-possibles grid))))

; destructive update of possibles
(defun apply-reductions(rs possibles) (mapcar (lx update-possible x possibles) rs))

(defun update-possible(cell possibles)
  (let ((cid (+ (col-of-cell cell) (* *dim* (row-of-cell cell)))))
    (setf (nth cid possibles) cell)))

(defun reduce-possibles(possibles) 
  ; hidden-triples testing slows the solving down overall; so not using it here.
  (apply #'append (loop for chg in (append (hidden-singles possibles) (hidden-pairs possibles) )
        collect (reduce-group chg possibles))))

; the grouping can be reduced by intersecting combo, which is assumed to be a
; hidden single/pair/triple any cell that has this combo as a subset can have
; its other options eliminated. Note  we put back the intersection so as not to
; increase the options in the cell
(defun reduce-group(combo grouping)
    (loop for c in grouping
        when (intersection combo (cell-possibles c)) collect (make-cell (row-of-cell c) (col-of-cell c) (intersection combo (cell-possibles c)))))

; make a grid from the possibles
(defun possibles->grid(poss)
  (let ((repr (mapcar (lx if(eq 1 (length(cell-possibles x)))(car(cell-possibles x)) '-) poss)))
    (mapcar (lx take *dim* (drop (* x *dim*) repr)) (range 0 8))))


(defun solve-deep(grid)
  (let ((newgrid (solve grid)))
    (if (solved newgrid) (return-from solve-deep newgrid))
    (loop for p in (report-possibles newgrid) when (> (length(cell-possibles p)) 1) do
          (loop for pp in (cell-possibles p) do
                (format t "~%Going deep with guess ~d,~d = ~d" (row-of-cell p) (col-of-cell p) pp)
                (set-cell newgrid (row-of-cell p) (col-of-cell p) pp)
                (let ((newgrid (solve-deep newgrid)))
                  (if (solved newgrid) (return-from solve-deep newgrid)))
                (format t "~%Back from testing guess ~d,~d = ~d" (row-of-cell p) (col-of-cell p) pp)))
    newgrid))


(defun test-boards()
  (mapcar (lx solved (solve-deep x)) (list *board1* *board2* *board3* *board4* *board5* *board6* *board7* *board8* *board9* )))

; return combinations of triples from the given list
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
    (mapcar #'first (remove-if (lx not(eq 3 (second x))) (loop for c in cmbs collect (list c (length (remove nil (mapcar (lx intersection c x) ps)))))))))

(defun read81()
  (let ((chars '(#\- #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
    (labels ((acc (listsofar)
                (if (> (length listsofar) 80) (return-from acc (take 81 listsofar)))
                (return-from acc (acc (append listsofar (remove-if (lx not(member x chars)) (coerce (read-line) 'list)))))))
      (acc '()))))

