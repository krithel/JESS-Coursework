(deftemplate BoardSlot
	(slot row)
	(slot column)
	(slot content))

(deffacts Board
	(BoardSlot (row 1) (column 1) (content "-"))
	(BoardSlot (row 1) (column 2) (content "-"))
	(BoardSlot (row 1) (column 3) (content "-"))
	(BoardSlot (row 2) (column 1) (content "-"))
	(BoardSlot (row 2) (column 2) (content "-"))
	(BoardSlot (row 2) (column 3) (content "-"))
	(BoardSlot (row 3) (column 1) (content "-"))
	(BoardSlot (row 3) (column 2) (content "-"))
	(BoardSlot (row 3) (column 3) (content "-")))

(defrule SlotIsEmpty
	(BoardSlot {content == "-"})
	=>
	(printout t "The board slot is empty" crlf))

(defrule RowIsFull
	?slot1 <- (BoardSlot)
	?slot2 <- (BoardSlot {row == slot1.row && 

(defquery get-contents-of-slot
	(declare (variables ?col ?row))
	(BoardSlot (row ?row) (column ?col) (content ?content)))


(deffunction PrintBoard()
	(bind ?row 1)
	(bind ?col 1)
	(while (<= ?row 3)
	
		(while (<= ?col 3)
		
			(bind ?result (run-query* get-contents-of-slot ?col ?row))
			(while (?result next)
				(printout t (?result getString content)))
			(printout t " | ")
			(bind ?col (+ ?col 1))
		)
		(printout t crlf)
		(printout t "------------" crlf)
		(bind ?col 1)
		(bind ?row (+ ?row 1))
	)
)

(reset)
(PrintBoard())
