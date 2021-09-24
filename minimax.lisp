
; ****************************************************
; game
; begin
; ****************************************************

(defun connect-3-two-player ()
	(setq board (initBoard))
	(loop
		(setq p (player board))
		(printBoard board)
		(setq a (read))
		(setq board (result board p a))
		(when (terminal-test board)
			(printBoard board)
			(if (= p 1) (write-string "player 1 ") (write-string "player 2 "))
			(return (write-string "wins"))
		)
	)
)

(defun connect-3 ()
	(setq board (initBoard))
	(loop
		(setq p (player board))
		(printBoard board)
		(if (= p 1)
			(setq a (read))
			(setq a (result board p (minimax-reduced board)))
		)
		(setq board (result board p a))
		(when (terminal-test board)
			(printBoard board)
			(if (= p 1) (write-string "player 1 ") (write-string "player 2 "))
			(return (write-string "wins"))
		)
	)
)

; ****************************************************
; game
; end
; ****************************************************


; ****************************************************
; minimax
; begin
; ****************************************************

(defun minimax (board)
	; if terminal state, return utility
	; if max's turn, call minimax on all valid actions and return the highest value of those actions
	; if min's turn, call minimax on all valid actions and return the lowest value of those actions
)

(defun minimax-reduced (board)
	(if (terminal-test board) (utility board)
		nil   ; else, check call minimax on 
	)
)

(defun pick-action (board actions turn)
	(cond
		((null actions) nil)
		((= turn 1) nil)
		((= turn 2) nil)
	)
)


(defun indexOfMax (vals)

)

; ****************************************************
; minimax
; end
; ****************************************************

; ****************************************************
; initialize board
; begin
; ****************************************************

(defun initBoard ()
	'(0 0 0 0
	  0 0 0 0
	  0 0 0 0
	  0 0 0 0
	)
)

; ****************************************************
; initialize board
; end
; ****************************************************

; ****************************************************
; player and its helpers
; begin
; ****************************************************

(defun player (state)
	(getTurn board 0)
)

(defun getTurn (state turn)
	"Max goes first, 1 for max's turn, 2 for min's turn"
	(cond 
		((null state) (+ 1 turn))
		((= (car state) 1) (getTurn (cdr state) (+ turn 1)))
		((= (car state) 2) (getTurn (cdr state) (- turn 1)))
		(t (getTurn (cdr state) turn))
	)
)

(defun countMinAndMax (state min max)
	"counts the number of "
	(cond 
		((null state) (list min max))
		((= (car state) 1) (countMinAndMax (cdr state) (+ 1 min) max))
		((= (car state) 2) (countMinAndMax (cdr state) min (+ 1 max)))
		(t (countMinAndMax (cdr state) min max))
	)
)

; ****************************************************
; action and its helpers
; end
; ****************************************************


; ****************************************************
; action and its helpers
; begin
; ****************************************************

(defun action (state)
	"returns legal actions given the state as a list of indices to place a piece"
	(indexOfDeepest (deepestRowForColumnList state (- width 1)) 0)
)

(defun deepestRowForColumnList (state column)
	(if (> column 0)
		(append (deepestRowForColumnList state (- column 1)) (list (find-deepest state column 0))) 
		(cons (find-deepest state column 0) nil)
	)
)

(defun indexOfDeepest (rows column)
	"rows is a list of the deepest empty row for each column, column is the current column we are operating in"
	(cond
		((null rows) nil) ; when we run out of rows, return nil
		((null (car rows)) (indexOfDeepest (cdr rows) (+ column 1))) ; if car rows is nil, then this column is full
		; if car rows is not nil, then we add the index of the row and column to our returned list, and call indexOfDeepest on the rest of the list
		(t (append (list (+ (* width (car rows)) column)) (indexOfDeepest (cdr rows) (+ column 1)))) ; row * width + column
	)
)

(defun find-deepest (state column row)
	"Works from bottom up the stack trace, giving the first empty slot in a column"
	(if (> column 0) 
		(find-deepest (cdr state) (- column 1) row) ; if column = 0, adjust the alignment #1
		(if (equal state nil)
			nil 																		 ;#2
			(or (find-deepest state width (+ row 1)) (if (equal (car state) '0) row nil)) ;#3 or #4
		)
	)
)

; ****************************************************
; action and its helpers
; end
; ****************************************************


; ****************************************************
; result begin
; ****************************************************

(defun result (board piece index)
	"returns a new state with the performed action"
	(cond
		((null index) nil)
		((> index 0) (cons (car board) (result (cdr board) piece (- index 1))))
		(t (cons piece (cdr board)))
	)
)
; ****************************************************
; result end
; ****************************************************


; ****************************************************
; terminal-test and its helpers
; begin
; ****************************************************

(defun terminal-test (board)
	(or (test-horizontal board)
		(test-diagonal board)
		(test-vertical board)
	)
)

(defun test-horizontal (board)
	(cond 
		((null board) nil)
		(t (or (horizonCheck board) (test-horizontal (nthcdr 4 board))))
	)
)

(defun horizonCheck (board)
	(setq tmp (car board))
	(setq tmp1 (cadr board))
	(if (= tmp1 0)
		nil
		(if (= tmp tmp1) 		  ; if first = second
				(= tmp (caddr board)) ; then check if third = first
				(and (= tmp1 (caddr board)) (= tmp1 (cadddr board))) ; second = third AND second = fourth	
		)
	)
)

(defun test-diagonal (board)
	(or (diagonalCheck board)
		(diagonalCheck (nthcdr 4 board))
	)
)

(defun diagonalCheck (board)
	"checks diagonals on one row"
	(or (diagonalDownRight board)
		(diagonalDownRight (cdr board))
		(diagonalDownLeft (cddr board))
		(diagonalDownLeft (cdddr board))
	)
)

(defun diagonalDownRight (board)
	(setq tmp (car board))
	(and (not (= tmp 0)) (= tmp (car (nthcdr 5 board))) (= tmp (car (nthcdr 10 board))))
)

(defun diagonalDownLeft (board)
	(setq tmp (car board))
	(and (not (= tmp 0)) (= tmp (car (nthcdr 3 board))) (= tmp (car (nthcdr 6 board))))
)

(defun test-vertical (board)
	(or (verticalCheck board)
		(verticalCheck (nthcdr 4 board))
	)
)

(defun verticalCheck (board)
	(or (down board)
		(down (cdr board))
		(down (cddr board))
		(down (cdddr board))
	)
)

(defun down (board)
	(setq tmp (car board))
	(and (not (= tmp 0)) (= tmp (car (nthcdr 4 board))) (= tmp (car (nthcdr 8 board))))
)


; ****************************************************
; terminal-test and its helpers
; end
; ****************************************************

; ****************************************************
; utility and its helpers
; begin
; ****************************************************

; TODO: finish utility function
(defun utility (board player)

)

; (defun util-test-horizontal (board player)
; 	(cond 
; 		((null board) nil)
; 		(t (or (horizonCheck board) (test-horizontal (nthcdr 4 board))))
; 	)
; )

; (defun util-horizonCheck (board player)
; 	(setq tmp (car board))
; 	(setq tmp1 (cadr board))
; 	(if (= tmp1 0)
; 		nil
; 		(if (= tmp tmp1) 		  ; if first = second
; 				(= tmp (caddr board)) ; then check if third = first
; 				(and (= tmp1 (caddr board)) (= tmp1 (cadddr board))) ; second = third AND second = fourth	
; 		)
; 	)
; )

(defun util-test-diagonal (board player)
	(or (util-diagonalCheck board player)
		(util-diagonalCheck (nthcdr 4 board) player)
	)
)

(defun util-diagonalCheck (board player)
	"checks diagonals on one row"
	(or (util-diagonalDownRight board player)
		(util-diagonalDownRight (cdr board) player)
		(util-diagonalDownLeft (cddr board) player)
		(util-diagonalDownLeft (cdddr board) player)
	)
)

(defun util-diagonalDownRight (board player)
	(setq tmp (car board))
	(and (not (= tmp 0)) (= tmp (car (nthcdr 5 board))) (= tmp (car (nthcdr 10 board))))
)

(defun util-diagonalDownLeft (board player)
	(setq tmp (car board))
	(and (not (= tmp 0)) (= tmp (car (nthcdr 3 board))) (= tmp (car (nthcdr 6 board))))
)

(defun util-test-vertical (board player)
	(or (util-verticalCheck board player)
		(util-verticalCheck (nthcdr 4 board) player)
	)
)

(defun util-verticalCheck (board player)
	(or (util-down board player)
		(util-down (cdr board) player)
		(util-down (cddr board) player)
		(util-down (cdddr board) player)
	)
)

(defun util-down (board player)
	(and (= player (car board) (= player (car (nthcdr 4 board))) (= player (car (nthcdr 8 board)))))
)

; ****************************************************
; utility and its helpers
; end
; ****************************************************


; ****************************************************
; printBoard and its helpers
; begin
; ****************************************************

(defun printBoard (board)
	(setq tempBoard board)
	(setq i 0)
	(loop
		(setq j 0)
		(loop 
			(write (car tempBoard))
			(princ " ")
			(setq tempBoard (cdr tempBoard))
			(setq j (+ j 1))
			(when (= j 4) (return (terpri)))
		)
		(setq i (+ i 1))
		(when (= i 4) (return nil))
	)
)

(connect-3)