(deftemplate BoardSlot
	(slot row)
	(slot column)
	(slot content))

;=============Defining the facts===============
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

(deffacts initial-phase
   (phase choose-player))


;===============Defining the functions==========
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

(deffunction ask-start-again ()
  (printout t "Play again? (y/n) ")
  (if (eq (read) y) then
    (assert (phase choose-player))))


;================Defining the Rules==============

(defrule pickaslot
	(declare (salience 100))
	?slot <- (BoardSlot {row == 1 && column == 1})
	=> (bind ?slot.content "X"))



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
   
   )

(defrule bad-player-choice 
   ?phase <- (phase choose-player)
   ?choice <- (player-select ?player&~c&~h)
   =>
   (retract ?phase ?choice)
   (assert (phase choose-player))
   (printout t "Choose c or h." crlf))

(defrule RowIsFull
	?slot1 <- (BoardSlot {content != "-"})
	?slot2 <- (BoardSlot {row == slot1.row &&column != slot1.column && content == slot1.content})
	?slot3 <- (BoardSlot {row == slot2.row && column != slot1.column && column != slot2.column && content == slot2.content})
	=>(printout t "The player: " ?slot1.content " has won!" crlf))
	
(defrule ColIsFull
	?slot1 <- (BoardSlot {content != "-"})
	?slot2 <- (BoardSlot {column == slot1.column && row != slot1.row && content == slot1.content})
	?slot3 <- (BoardSlot {column == slot2.column && row != slot1.row && row != slot2.row && content == slot2.content})
	=>(printout t "The player: " ?slot1.content " has won!" crlf))

(defrule DiagIsFull
	?slot1 <- (BoardSlot {row == 1 && content != "-"})
	?slot2 <- (BoardSlot {row == 2 && column == 2 && content == slot1.content})
	?slot3 <- (BoardSlot {row == 3 && column != slot1.column && column != slot2.column && content == slot1.content})
	=>(printout t "The player: " ?slot1.content " has won!" crlf))

(defquery get-contents-of-slot
	(declare (variables ?col ?row))
	(BoardSlot (row ?row) (column ?col) (content ?content)))

(defrule get-human-move
   (player-move h)
   =>
   (printout t "Which column would you like to select?")
   (assert (human-col (read)))
   (printout t "Which row would you like to select?")
   (assert (human-row (read))))

(defrule good-human-move
   ?col <- (human-col ?choice)
   ?row <- (human-row ?choice)
   ?slot <- (BoardSlot {row == ?row && column == ?col && content == "-"})
   =>
   (printout t "good-human-move")
   (retract ?move ?whose-turn)
   (bind ?slot.content "X"))
   (PrintBoard())
   (assert (player-move c))

(reset)

(run)
