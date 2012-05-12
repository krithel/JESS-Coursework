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






(deffacts initial-phase
   (phase choose-player))



(deffunction ask-start-again ()
  (printout t "Play again? (y/n) ")
  (if (eq (read) y) then
    (assert (phase choose-player))))

(defrule player-select
   (phase choose-player)
   =>
   (printout t "Who moves first (Computer: c "
               "Human: h)? ")
   (assert (player-select (read))))

(defrule good-player-choice
   ?phase <- (phase choose-player)
   ?choice <- (player-select ?player&:(or (eq ?player c) (eq ?player h)))
   =>
   (retract ?phase ?choice)
   (assert (player-move ?player))
   (PrintBoard())
   (ask-start-again)) ;this is just for testing!!!

(defrule bad-player-choice 
   ?phase <- (phase choose-player)
   ?choice <- (player-select ?player&~c&~h)
   =>
   (retract ?phase ?choice)
   (assert (phase choose-player))
   (printout t "Choose c or h." crlf))

(deffunction boardFull()
	(bind ?row 1)
	(bind ?col 1)
	(bind ?full true)
	?full <- (while (<= ?row 3)
		
				(while (<= ?col 3)
		
					(bind ?result (run-query* get-contents-of-slot ?col ?row)
					(if (?result "-" content)
						(?full false)
					)
				)
			  )
	?full <-
	=>
	(printout t "It's a draw" crlf)
					
)			


(reset)

(run)