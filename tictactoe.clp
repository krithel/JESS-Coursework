(deftemplate BoardSlot
	(slot row)
	(slot column)
	(slot content))

(deftemplate State
 	(slot player)
	(slot mark))

(deftemplate Coords
 	(slot row)
	(slot col))

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
    (reset)))


(defquery get-contents-of-slot
	(declare (variables ?col ?row))
	(BoardSlot (row ?row) (column ?col) (content ?content)))

;================Defining the Rules==============

;================Player Piece Selecting============
(defrule player-select
   (phase choose-player)
   =>
   (printout t "Which piece do you want to be? (X or O): ")
   (assert (player-select (read))))

(defrule good-player-choice
   ?phase <- (phase choose-player)
   ?choice <- (player-select ?player&:(or (eq ?player O) (eq ?player X)))
   =>
   (retract ?phase ?choice)
   (assert (State (player Player) (mark ?player)))
   )

(defrule bad-player-choice 
   ?phase <- (phase choose-player)
   ?choice <- (player-select ?player&~O&~X)
   =>
   (retract ?phase ?choice)
   (assert (phase choose-player))
   (printout t "Choose X or O." crlf))


;=================Player Turn======================
(defrule get-human-move
	?player <- (State {player == Player})
	=>
	(PrintBoard())
   	(printout t "Which column would you like to select? (1-3): ")
	(bind ?colChoice (read))
   	(printout t "Which row would you like to select? (1-3): ")
	(bind ?rowChoice (read))
	(assert (Coords (row ?rowChoice) (col ?colChoice))))

(defrule good-human-move
 	?state <- (State {player == Player})
	?coord <- (Coords 	(row ?row	&:(integerp ?row)
									&:(> ?row 0) 
									&:(<= ?row 3))
						(col ?col	&:(integerp ?col)
						 			&:(> ?col 0)
									&:(<= ?col 3)))
	?slot <- (BoardSlot (content ?cont &: (eq ?cont "-"))
					{row == coord.row && column == coord.col})
	=>
	(printout t "good-human-move" crlf)
	(bind ?slot.content ?state.mark)
	(retract ?coord)
	(if (= ?state.mark O) then
	 	(modify ?state (mark X))
		else
		(modify ?state (mark O)))
	)

(defrule bad-human-move
 	?state <- (State {player == Player})
	?coord <- (Coords)
	=>
	(printout t "That is not a legitimate move. Please try again." crlf)
	(retract ?coord)
	(bind ?mark ?state.mark)
	(retract ?state)
	(assert (State (player Player) (mark ?mark)))
	)

(defrule winnerFound
 	?state <- (State {player == Winner})
	=>
	(PrintBoard)
	(printout t "The winner is " ?state.mark "!" crlf)
	(printout t "Congratulations!" crlf)
	(retract ?state)
	(ask-start-again)
	)

(defrule drawFound
 	?state <- (State {player == Draw})
	=>
	(PrintBoard)
	(printout t "The game was a draw!" crlf)
	(retract ?state)
	(ask-start-again)
	)


;====================Victory Condition Checkers=======================
(defrule board-is-full
 	(declare (salience 1))
 	(not (BoardSlot (content ?c &:(eq ?c "-"))))
	?state <- (State)
	=>
	(modify ?state (player Draw)))

(defrule RowIsFull
 	(declare (salience 2))
	?slot1 <- (BoardSlot {content != "-"})
	?slot2 <- (BoardSlot {row == slot1.row &&column != slot1.column && content == slot1.content})
	?slot3 <- (BoardSlot {row == slot2.row && column != slot1.column && column != slot2.column && content == slot2.content})
	?state <- (State)
	=>
	(modify ?state (player Winner) (mark ?slot1.content)))
	
(defrule ColIsFull
 	(declare (salience 2))
	?slot1 <- (BoardSlot {content != "-"})
	?slot2 <- (BoardSlot {column == slot1.column && row != slot1.row && content == slot1.content})
	?slot3 <- (BoardSlot {column == slot2.column && row != slot1.row && row != slot2.row && content == slot2.content})
	?state <- (State)
	=>
	(modify ?state (player Winner) (mark ?slot1.content)))

(defrule DiagIsFull
 	(declare (salience 2))
	?slot1 <- (BoardSlot {row == 1 && column != 2 && content != "-"})
	?slot2 <- (BoardSlot {row == 2 && column == 2 && content == slot1.content})
	?slot3 <- (BoardSlot {row == 3 && column != slot1.column && column != slot2.column && content == slot1.content})
	?state <- (State)
	=>
	(modify ?state (player Winner) (mark ?slot1.content)))


(reset)

(run)
