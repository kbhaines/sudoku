
(defvar *clear* (format nil "~c[2J" #\Escape))
(defvar *home* (format nil "~c[H" #\Escape))
(defvar *up* (format nil "~c[A" #\Escape))

(defparameter *dim* 9)

(defun range(low high) (loop for n from low below (1+ high) collect n))


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
(defun set-cell(grid r c val) (setf (nth c (nth r grid)) val))

(defun cell-row(cell) (first cell))
(defun cell-col(cell) (second cell))
(defun cell-square(cell) (square-ref (first cell) (second cell)))
(defun cell-possibles (cell) (third cell))
(defun make-cell(row col possibles) (list row col possibles))

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

(defun valid-possibles(ps)
  (eq 0 (count nil (mapcar #'cell-possibles ps))))

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


(defun print-grid(g)
  (loop for r in g for rc below 9 do
        (let ((g1 (slice 0 3 r))
              (g2 (slice 3 3 r))
              (g3 (slice 6 3 r)))
          (if (or (eq 3 rc) (eq 6 rc)) (princ #\Newline))
          (format t "~{~a ~a ~a~}  ~{~a ~a ~a~}   ~{~a ~a ~a~}~%" g1 g2 g3))))

(defun overprint-grid(g)
    (format t "~c[11A" #\Escape)
    (print-grid g))

(defun is-finished(grid) (not (find '- (apply #'append grid))))

(defun solved(grid) (and (is-finished grid) (valid-board grid)))


; functions to filter possibles down to groups
(defun row-group(r possibles) (remove-if-not (lx eq r (first x)) possibles))
(defun col-group(c possibles) (remove-if-not (lx eq c (second x)) possibles))
(defun square-group(s possibles) (remove-if-not (lx eq s (cell-square x)) possibles))

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
  (let ((ps (report-possibles grid)))
   (loop for n from 0 below *dim* do (apply-reductions (reduce-possibles (row-group n ps)) ps))
   (loop for n from 0 below *dim* do (apply-reductions (reduce-possibles (col-group n ps)) ps))
   (loop for n from 0 below *dim* do (apply-reductions (reduce-possibles (square-group n ps)) ps))
   (let ((newgrid (possibles->grid ps)))
     (cond ((is-finished newgrid)
            (if(not(valid-board newgrid))
              ;(progn (print "uh-oh; invalid board!!") (return-from solve newgrid)))
              (progn  (return-from solve newgrid)))
            (return-from solve newgrid))

           ((not(equal newgrid grid ))
            (return-from solve (solve newgrid))))
    ;(print 'Blocked) 
    newgrid )))

; destructive update of possibles
(defun apply-reductions(rs possibles) (mapcar (lx update-possible x possibles) rs))

(defun update-possible(cell possibles)
  (let ((cid (+ (cell-col cell) (* *dim* (cell-row cell)))))
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
        when (intersection combo (cell-possibles c)) collect (make-cell (cell-row c) (cell-col c) (intersection combo (cell-possibles c)))))

; make a grid from the possibles
(defun possibles->grid(poss)
  (let ((repr (mapcar (lx if(eq 1 (length(cell-possibles x)))(car(cell-possibles x)) '-) poss)))
    (mapcar (lx take *dim* (drop (* x *dim*) repr)) (range 0 8))))


(defun solve-deep(grid &key callback)
  (let ((newgrid (solve grid)))
    (if callback (funcall callback newgrid))
    (if (solved newgrid) (return-from solve-deep newgrid))
    (let* ((ps (remove-if (lx eq 1 (length(cell-possibles x))) (report-possibles newgrid)))
           (p (first (sort ps (lambda(x y)(<(length (cell-possibles x)) (length (cell-possibles y))))))))
      (if (not(valid-possibles ps)) (return-from solve-deep newgrid))
      (loop for pp in (cell-possibles p) do
            (set-cell newgrid (cell-row p) (cell-col p) pp)
            (if callback (funcall callback newgrid))
            (let ((newgrid (solve-deep newgrid :callback callback)))
              (if (solved newgrid) (return-from solve-deep newgrid)))
            (set-cell newgrid (cell-row p) (cell-col p) '- )))
    grid))


(defun animated-solve(grid)
  (print-grid grid)
  (let ((solution (solve-deep grid :callback #'overprint-grid)))
    (format t "~%")
    (return-from animated-solve solution)))


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


(defun parse-string-grid(str)
  (flet ((parse-string(str) (mapcar (lx if (digit-char-p x) (- (char-code x) 48) '-) (coerce str 'list))))
    (loop for n below *dim* collect (subseq (parse-string str) (* 9 n) (+ (* 9 n) 9 )))))
