;===========Defining Templates===============
(deftemplate BoardSlot ;The template for each position on the board
	(slot row)			;The row of the slot
	(slot column)		;The column of the slot
	(slot content)		;The content of the slot
	(slot tag)			;Used to determine position easily
	(slot diag))		;Used to determine diagonals easily

(deftemplate State ;The template for the current state
 	(slot player)		;Who's turn it currently is
	(slot mark))		;What mark the current player is using

(deftemplate Coords ;The template for the player choosing coordinates
 	(slot row)		;Which row the player chose
	(slot col))		;Which column the player chose

;=============Defining the facts===============
(deffacts Board ;The original state of the board is empty
	(BoardSlot (row 1) (column 1) (content "-") (tag "Corner") (diag "Diag1"))
	(BoardSlot (row 1) (column 2) (content "-") (tag "Edge") (diag "None"))
	(BoardSlot (row 1) (column 3) (content "-") (tag "Corner") (diag "Diag2"))
	(BoardSlot (row 2) (column 1) (content "-") (tag "Edge") (diag "None"))
	(BoardSlot (row 2) (column 2) (content "-") (tag "Centre") (diag "Both"))
	(BoardSlot (row 2) (column 3) (content "-") (tag "Edge") (diag "None"))
	(BoardSlot (row 3) (column 1) (content "-") (tag "Corner") (diag "Diag2"))
	(BoardSlot (row 3) (column 2) (content "-") (tag "Edge") (diag "None"))
	(BoardSlot (row 3) (column 3) (content "-") (tag "Corner") (diag "Diag1")))

(deffacts initial-phase ;Setting the initial phase, so the player can choose a piece
   (phase choose-player))


;===============Defining the functions==========
(deffunction PrintBoard()
	;This function is used to print out the board on each turn.
	(bind ?row 1)
	(bind ?col 1)
	(printout t "    1   2   3" crlf)
	(printout t "   ------------" crlf)
	(while (<= ?row 3)
		(printout t ?row " | ")
		(while (<= ?col 3)
		
			(bind ?result (run-query* get-contents-of-slot ?col ?row))
			(while (?result next)
				(printout t (?result getString content)))
			(printout t " | ")
			(bind ?col (+ ?col 1))
		)
		(printout t crlf)
		(printout t "   ------------" crlf)
		(bind ?col 1)
		(bind ?row (+ ?row 1))
	)
)

(deffunction notequals (?i ?j ?k)
 	;This function returns true if the three values are not equal, false otherwise
 	(if (or (not (= ?i ?j)) (not (= ?i ?k)) (not (= ?j ?k)))
	 then (return TRUE) else (return FALSE)))

(deffunction equals (?i ?j ?k)
 	;This function returns true if the three values are equal, false otherwise
 	(if (and (= ?i ?j) (= ?i ?k) (= ?j ?k)) then (return TRUE) else (return FALSE)))

(deffunction in-a-row (?slot1 ?slot2 ?slot3)
 	;This function returns true if the three BoardSpaces are in a row, false otherwise
 	(if (and 	(equals ?slot1.row ?slot2.row ?slot3.row) 
		 		(notequals ?slot1.column ?slot2.column ?slot3.column))
	 	then (return TRUE)
		else 	(if 
					(and 	(equals ?slot1.column ?slot2.column ?slot3.column)
							(notequals ?slot1.row ?slot2.row ?slot3.row))
				then(return TRUE) 
				else (if 
						(and 
						 	(or 
							 	(and (eq ?slot1.diag ?slot2.diag) (eq ?slot2.diag "Both") (not (eq ?slot1.diag "None")))
								(and (eq ?slot2.diag ?slot3.diag) (eq ?slot2.diag "Both") (not (eq ?slot2.diag "None")))
								(and (eq ?slot1.diag ?slot3.diag) (eq ?slot2.diag "Both") (not (eq ?slot1.diag "None")))) 
							(notequals ?slot1.row ?slot2.row ?slot3.row) 
							(notequals ?slot1.column ?slot2.column ?slot3.column))
						then (return TRUE) else (return FALSE)))))


(deffunction ask-start-again ()
 	;This function checks if the player wants to play again, if so it resets the board.
  	(printout t "Play again? (y/n) ")
	(if (eq (read) y) then
    	(reset)))


(defquery get-contents-of-slot
 	;This query is used to find the value of the slot at the given coordinates.
	(declare (variables ?col ?row))
	(BoardSlot (row ?row) (column ?col) (content ?content)))

;================Defining the Rules==============

;=============Victory and Draw handling==========
(defrule winnerFound
 	;This rule triggers when there is a winner. It prints it out and asks if the user 
	;wants another game
	(declare (salience 25))
 	?state <- (State {player == Winner})
	=>
	(PrintBoard)
	(printout t "The winner is " ?state.mark "!" crlf)
	(retract ?state)
	(ask-start-again)
	)

(defrule drawFound
 	;This rule triggers when the game is a draw. It prints it out and asks if the user
	;wants another game
	(declare (salience 25))
 	?state <- (State {player == Draw})
	=>
	(PrintBoard)
	(printout t "The game was a draw!" crlf)
	(retract ?state)
	(ask-start-again)
	)
;====================Victory Condition Checkers=======================
(defrule SomeoneHasWon
 	;This rule is triggered when there is a row of 3. Then there is a winner and it changes
	;the state to the Winner state for the player.
 	(declare (salience 20))
 	?slot1 <- (BoardSlot {content != "-"})
 	?slot2 <- (BoardSlot {content == slot1.content && 
				(row != slot1.row || column != slot1.column)})
 	?slot3 <- (BoardSlot {content == slot1.content && 
				(row != slot1.row || column != slot1.column) && 
				(row != slot2.row || column != slot2.column)})
	(test (in-a-row ?slot1 ?slot2 ?slot3))
	?state <- (State)
	=>
	(modify ?state (player Winner) (mark ?slot1.content)))

(defrule board-is-full
 	;This rule is triggered when the board is full. If so, and there is no winner, then there
	;is a draw. The game state is then changed to a Draw.
 	(declare (salience 20))
 	(not (BoardSlot (content ?c &:(eq ?c "-"))))
	?state <- (State)
	=>
	(modify ?state (player Draw)))

;================Player Piece Selecting============
(defrule player-select
 	;This rule is triggered at the start in the choose-player phase, the player then 
	;selects which piece they are using
   (phase choose-player)
   =>
   (printout t "Which piece do you want to be? (X or O): ")
   (assert (player-select (read))))

(defrule good-player-choice
 	;This rule is triggered when the player has chosen a legitimate piece in the choose-player
	;phase. The state is the changed to the actual game for the player.
   ?phase <- (phase choose-player)
   ?choice <- (player-select ?player&:(or (eq ?player O) (eq ?player X)))
   =>
   (retract ?phase ?choice)
   (if (eq ?player X) then
		(assert (State (player Player) (mark ?player)))
		else
		(assert (State (player AI) (mark ?player))))
   )

(defrule bad-player-choice 
 	;This rule is triggered when the player has chosen a illegitimate piece in the
	;choose-player phase. The user is then prompted to retry
   ?phase <- (phase choose-player)
   ?choice <- (player-select ?player&~O&~X)
   =>
   (retract ?phase ?choice)
   (assert (phase choose-player))
   (printout t "Choose X or O." crlf))


;=================AI Turn==========================


(defrule AIRule1
 	;The AI will try and choose a square that gives it 3 in a row
	;RULE AIRule1
	;IF
	;	There are 2 Spaces filled with the AI Mark and a free space in a row
	;THEN
	;	The AI Will play in the free space.
	(declare (salience 8))
	?state <- (State {player == AI})
	?slot1 <- (BoardSlot {content == state.mark})
	?slot2 <- (BoardSlot {content == state.mark && (row != slot1.row || column != slot1.column)})
	?square <- (BoardSlot {content == "-"})
	(test (in-a-row ?slot1 ?slot2 ?square))
	=>
	(bind ?square.content ?state.mark)		 	
	(if (= ?state.mark O) then
	 	(modify ?state (player Player) (mark X))
		else
		(modify ?state (player Player) (mark O))))

(defrule AIRule2
; 	;The AI will try and choose a square that would give it's opponent 3 in a row
	;RULE AIRule2
	;IF
	;	There are 2 Spaces filled with the Player Mark and a free space in a row
	;THEN
	;	The AI will play in the free space.	
	(declare (salience 6))
	?state <- (State {player == AI})
	?slot1 <- (BoardSlot {content != state.mark && content != "-"})
	?slot2 <- (BoardSlot {content != state.mark && content != "-" && (row != slot1.row || column != slot1.column)})
	?square <- (BoardSlot {content == "-"})
	(test (in-a-row ?slot1 ?slot2 ?square))
	=>
	(bind ?square.content ?state.mark)
	(if (= ?state.mark O) then
	 	(modify ?state (player Player) (mark X))
		else
		(modify ?state (player Player) (mark O)))
)

(defrule AIRule3
 	;The AI will try and choose a square that would give then a 'Double row'
	;RULE AIRule3
	;IF
	;	There are 2 lines that contain an AI mark and 2 free spaces, and the two lines share
	; 	a free space.
	;THEN
	;	The AI Will play in the free space.
	(declare (salience 5))
	?state <- (State {player == AI})
	?slot11 <- (BoardSlot {content == state.mark})
	?slot12 <- (BoardSlot {content == "-"})
	?slot21 <- (BoardSlot {content == state.mark && (row != slot11.row || column != slot11.column)})
	?slot22 <- (BoardSlot {content == "-" && (row != slot12.row || column != slot12.column)})
	?square <- (BoardSlot {content == "-"})
	(test (in-a-row ?slot11 ?slot12 ?square))
	(test (in-a-row ?slot21 ?slot22 ?square))
	=>
	(bind ?square.content ?state.mark)
	(if (= ?state.mark O) then
	 	(modify ?state (player Player) (mark X))
		else
		(modify ?state (player Player) (mark O)))
)

(defrule AIRule4
;	;The AI will try and choose a square that would give the opponent a 'Double row'
	(declare (salience 4))
	?state <- (State {player == AI})
	?slot11 <- (BoardSlot {content != state.mark && content != "-"})
	?slot12 <- (BoardSlot {content == "-"})
	?slot21 <- (BoardSlot {content != state.mark && content != "-" && (row != slot11.row || column != slot11.column)})
	?slot22 <- (BoardSlot {content == "-" && (row != slot12.row || column != slot12.column)})
	?square <- (BoardSlot {content == "-"})
	=>
	(bind ?square.content ?state.mark)
	(if (= ?state.mark O) then
	 	(modify ?state (player Player) (mark X))
		else
		(modify ?state (player Player) (mark O)))
)

(defrule AIRule5
 	;The AI will try and choose the centre square
	;RULE AIRule5
	;IF
	;	The centre space is a free space.
	;THEN
	;	The AI Will play in the centre space.
	(declare (salience 3))
	?state <- (State {player == AI})
	?square <- (BoardSlot {content == "-" && tag == "Centre"})
	=>
	(bind ?square.content ?state.mark)
	(if (= ?state.mark O) then
	 	(modify ?state (player Player) (mark X))
		else
		(modify ?state (player Player) (mark O)))
)

(defrule AIRule6
 	;The AI will try and choose a corner square
	;RULE AIRule6
	;IF
	;	There is a free corner space.
	;THEN
	;	The AI Will play in the free corner space.
	(declare (salience 2))
	?state <- (State {player == AI})
	?square <- (BoardSlot {content == "-" && tag == "Corner"})
	=>
	(bind ?square.content ?state.mark)
	(if (= ?state.mark O) then
	 	(modify ?state (player Player) (mark X))
		else
		(modify ?state (player Player) (mark O)))
)

(defrule AIRule7
 	;The AI will choose any square 
	;RULE AIRule7
	;IF
	;	There is any free space.
	;THEN
	;	The AI Will play in the free space.
	(declare (salience 1))
	?state <- (State {player == AI})
	?square <- (BoardSlot {content == "-"})
	=>
	(bind ?square.content ?state.mark)
	(if (= ?state.mark O) then
	 	(modify ?state (player Player) (mark X))
		else
		(modify ?state (player Player) (mark O)))
)
;=================Player Turn======================
(defrule get-human-move
 	;This rule triggers when the State is for the Player Move. The player move is read in
	;and stored as a Coord.
	?player <- (State {player == Player})
	=>
	(PrintBoard())
   	(printout t "Which column would you like to select? (1-3): ")
	(bind ?colChoice (read))
   	(printout t "Which row would you like to select? (1-3): ")
	(bind ?rowChoice (read))
	(assert (Coords (row ?rowChoice) (col ?colChoice))))

(defrule good-human-move
 	;This rule triggers when the State is for the Player Move and they have chosen Coords that
	;are legitimate. That move is then made and the turn is changed to the AI.
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
	(bind ?slot.content ?state.mark)
	(retract ?coord)
	(if (= ?state.mark O) then
	 	(modify ?state (player AI) (mark X))
		else
		(modify ?state (player AI) (mark O)))
	)

(defrule bad-human-move
 	;This rule triggers when the State is for the Player Move and they have chosen Coords that
	;are not a legitimate move. They are then prompted to chose another place.
 	?state <- (State {player == Player})
	?coord <- (Coords)
	=>
	(printout t "That is not a legitimate move. Please try again." crlf)
	(retract ?coord)
	(bind ?mark ?state.mark)
	(retract ?state)
	(assert (State (player Player) (mark ?mark)))
	)

;======================Base Command=================
(reset)
(run)
