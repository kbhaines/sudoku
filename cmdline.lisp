(load "sudoku.lisp")

(defun make() (ext:saveinitmem "exec" :init-function 'main :quiet t :executable t :norc t))

(defun main()
  (animated-solve (parse-string-grid (read-line)))
  (ext:exit))


;; while read x;do echo $x; echo $x|./exec;done < sudoku17.txt
