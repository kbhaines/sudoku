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

    (defun matrix-transpose (matrix)
  (when matrix
    (apply #'mapcar #'list matrix)))


(defun take(n lst) (subseq lst 0 n ))

(defun istrue(n) (eq n T))

(defun mand(x y) (and x y))

(defun mapindex (f lst) 
  (let ((idx 0))
    (mapcar (lambda(e) (progn 
                          (setf idx (1+ idx)) 
                          (funcall f idx e))) lst)))

(defun row(n) (nth n *grid*))

(defun col(n) 
  (flet ((c (row)(nth n row)))
    (mapcar #'c *grid*)))

(defun cell(r c) (nth c (row r)))

(defun row-of-cell(cell) (first cell))
(defun col-of-cell(cell) (second cell))
(defun square-of-cell(cell) (9group-ref (first cell) (second cell)))
(defun cell-possibles (cell) (third cell))

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
  (flet ((np (x) (eq nil (third x))))
    (remove-if #'np (apply #'append 
      (loop for r from 0 below *dim* collect 
            (loop for c from 0 below *dim* collect
                  (list r c (possibles r c))))))))

(defun multiple-possibilityp (x)
  (let ((len (length (third x))))
    (and (not(eq nil (third x))) ( > len 1))))

(defun single-possibilityp(x) 
  (let ((len (length (third x))))
    (eq 1 len)))

(defun sort-possibles(ps)
  (sort ps (lambda (x y)(< (length(third x)) (length(third y))))))

(defun fill-simple-possibles(possibles)
    (let ((simples (remove-if-not #'single-possibilityp possibles)))
      (flet 
        ((fill-it(with) 
           (let ((r (first with)) 
                 (c (second with)) 
                 (val (first(third with)))) 
             (set-cell r c val))))
        ;(print simples)
        (mapcar #'fill-it simples)
        (return-from fill-simple-possibles (> (length simples) 0)))))

(defun set-row(n lst)
  (setf (nth n *grid*) lst))

(defun set-col(c lst)
  (let ((row 0)) 
    (flet ((setc (a) 
                 (progn 
                   (setf (nth c (nth row *grid*)) a)
                   (setf row (1+ row)))))
    (mapcar #'setc lst))))

(defun get-search-space() 
  (flet ((srt (x y) (< (second x) (second y)))
         (fltr (x) (eq 0 (second x))))
    (remove-if #'fltr (sort (mapcar #'list  (range 0 80) (apply #'append (report-possibles))) #'srt))))

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

(defun is-finished() 
  (not (find '- (apply #'append *grid*))))


(defun iterate() 
  (return-from iterate (fill-simple-possibles(report-possibles)))
  (print-grid))

(defun solve()
  (loop
    (defvar cont)
    (setf cont (iterate))
    (when (is-finished) (progn (print-grid) (return t)))
    (when (not cont) (progn (print "blocked!") (return nil)))))

(defun deep-solve()
  (loop
    (loop until (not(fill-simple-possibles(report-possibles))))
    (when (not(valid-board)) (return-from deep-solve nil))
    (when (is-finished) (print-grid)(return-from deep-solve t))
    (mapcar #'go-deep (sort-possibles(report-possibles)))
    (when (not(is-finished)) (return-from deep-solve nil))
    (return t)))

(defun go-deep(cell)
    (incf *level*)
    (format t "Going to level ~d  ~C" *level* #\return)
    (print cell) (print-grid) (read)
    (flet ((try-possibles (v)
                (push (copy-tree *grid*) *grid-stack*)
                (set-cell (first cell) (second cell) v)
                (when (not(valid-board)) (print "bahhh!!")(break))
                (when (deep-solve) (print "solved!")(break)(setf *grid* (pop *grid-stack*))(return-from go-deep t))
                (unless (length *grid-stack*)(print "out of stack")(break))
                (setf *grid* (pop *grid-stack*))))

        (mapcar #'try-possibles (third cell))
        (decf *level*)
        (return-from go-deep nil)))


(setf *grid* *board4*)

(defun main() 
  (setf *grid* *board2*)
  (print-grid)
  (deep-solve))

(defun make() 
  (ext:saveinitmem "exec" :init-function 'main :executable t :norc t))

(defun row-needs(r) 
    (remove-duplicates(apply #'append (mapcar (lambda(c)(third c)) (poss-row r)))))

(defun row-missing-requirements(r)
    (set-difference(set-difference(range 1 9)(row r))(remove-duplicates(apply #'append (mapcar (lambda(c)(third c)) (poss-row r))))))


(defun filter-cell(pred possibles)
  (remove-if-not pred possibles))

(defun row-group(r possibles)
  (filter-cell (lambda(x) (eq r (first x))) possibles))

(defun col-group(c possibles) 
  (filter-cell (lambda(x) (eq c (second x))) possibles))

(defun square-group(s possibles)
  (filter-cell (lambda(x) (eq s (square-of-cell x))) possibles))

(defun potential-twins(poss)
  (let ((twins (filter-cell (lambda(x)(eq 2 (length(third x)))) poss)))
    (remove-if (lambda(x)(< (count (cell-possibles x) (mapcar #'cell-possibles twins) :test 'equal) 2)) twins)))

(defun poss-row(r) 
  (remove-if-not (lambda(x) (eq r (first x)))(report-possibles)))

(defun poss-col(r) 
  (remove-if-not (lambda(x) (eq r (second x)))(report-possibles)))

(defun find-twins(group)
 (remove-if-not (lambda(x)(eq 2 (length x))) (remove nil (maplist (lambda(x)(find (third(car x)) (mapcar #'third (cdr x)) :test 'equal)) group))))

(defun reduce-twins(twin group)
  (if (< (count twin (mapcar #'cell-possibles group) :test 'equal) 2)
    (return-from reduce-twins nil))
  (fill-simple-possibles (mapcar (lambda(x)(list (row-of-cell x)(col-of-cell x)(set-difference (cell-possibles x) twin))) group)))


(defun apply-twins(twins possibles) 
  (find t (apply #'append (list 
    (mapcar (lambda(x)(reduce-twins (cell-possibles x) (square-group (square-of-cell x) possibles))) twins)
    (mapcar (lambda(x)(reduce-twins (cell-possibles x) (row-group (row-of-cell x) possibles))) twins)
    (mapcar (lambda(x)(reduce-twins (cell-possibles x) (col-group (col-of-cell x) possibles))) twins)))))

(defun non-deep()
  (loop until (not(fill-simple-possibles(report-possibles))))
  (let ((twins (potential-twins (report-possibles))))
    (cond ((and (is-finished) (not twins))
           (print-grid)
           (return-from non-deep 'Done ))

          ((not twins)
           (return-from non-deep 'Blocked)))
    (loop until (not(apply-twins twins (report-possibles))))
    (if (not(is-finished)) (non-deep) (print-grid))))


(defun permutations-pair(lst)
  (apply #'append 
         (maplist (lambda(x)(mapcar (lambda(y)(list (first x) y)) (cdr x))) lst)))

(defun find-frequencies(grouping)
  (let ((freqs (sort (apply #'append (mapcar (lambda(x)(cell-possibles x)) (copy-tree grouping))) '< )))
        (mapcar (lambda(x)(list x (count x freqs))) (remove-duplicates freqs))))

(defun hidden-singles(freqs grouping)
  (let ((sgls (mapcar #'first (remove-if-not (lambda(x)(eq 1 (second x))) freqs))))
    (loop for i in sgls
     collect (let ((cell (find-if (lambda(x)(member i (cell-possibles x))) grouping)))
               (list (row-of-cell cell) (col-of-cell cell) (list i))))))

