    PROGRAM HuntTheWumpus

      IMPLICIT NONE
      CHARACTER(1) :: user1, user, user3       
      INTEGER :: hunter, arrw, arrwnum, wmps, bat1, bat2, gameon
      INTEGER :: pit1, pit2 , inpt, inpt2, win, n, err, para
      INTEGER, DIMENSION(1:20,1:3) :: cave
      INTEGER, DIMENSION(1:3) :: now

      gameon=1
      CALL game(gameon, win)
      END program
      
*************************************************************
*	  Main frame of the game
*************************************************************      
      subroutine game(gameon, win)

      IMPLICIT NONE
      CHARACTER(1) :: user1, user, user3       
      INTEGER :: hunter, arrw, arrwnum, wmps, bat1, bat2, gameon
      INTEGER :: pit1, pit2 , inpt, win, inpt2, n, err, para
      INTEGER, DIMENSION(1:20,1:3) :: cave
      INTEGER, DIMENSION(1:3) :: now


*     player wins when w=1
      win=2
 
      WRITE(*, *) 'Welcome! Do you want to read the instruction (Y-N)? '
      READ (*,*) user 

      DO WHILE (.NOT.((user .EQ. 'Y') .OR. (user .EQ. 'y')
     -.OR.(user .EQ. 'N') .OR. (user .EQ. 'n')))
      WRITE(*,*) 'Invalid input. Please Choose between Y and N:'
      READ (*,*) user
      END DO 

      IF ((user .EQ. 'Y') .OR. (user .EQ. 'y')) THEN
            CALL Instructions
      ELSE IF ((user .EQ. 'N') .OR. (user .EQ. 'n')) THEN
            CONTINUE
      END IF      

      CALL initialize(hunter, bat1, bat2, pit1, pit2, wmps, cave, 
     - arrwnum)

      DO WHILE (gameon .EQ. 1)
		  CALL detect(bat1, bat2, pit1, pit2, hunter, wmps, cave)
		  WRITE(*,*) 'YOU ARE IN ROOM', hunter
		  WRITE(*,*) 'WHICH LEADS TO:', cave(hunter,:)
		  WRITE(*,*) 'Do you wanna move or shoot? (S-M)'
10        READ(*,*) user3

		  IF ((user3 .EQ. 's') .OR. (user3 .EQ. 'S')) THEN
			 CALL shoot(hunter, bat1, bat2, pit1, pit2, 
     - wmps, cave, arrw, arrwnum)
		  ELSE IF ((user3 .EQ. 'm') .OR. (user3 .EQ. 'M')) THEN
			 CALL move(hunter, bat1, bat2, pit1, pit2, wmps, cave) 
		  ELSE 
		     WRITE(*,*) 'Invalid input. Please Choose between S - M:'
		     GO TO 10
		  END IF
      END DO
      END
      
      
*************************************************************
*	  Init
*************************************************************

      SUBROUTINE initialize(hunter, bat1, bat2, pit1, pit2, wmps, cave,
     - arrwnum)
     
      IMPLICIT NONE
      CHARACTER(1) :: user1, user, user3       
      INTEGER :: hunter, arrw, arrwnum, wmps, bat1, bat2, gameon
      INTEGER :: pit1, pit2 , inpt, win, inpt2, n, err, para
      INTEGER, DIMENSION(1:20,1:3) :: cave
      INTEGER, DIMENSION(1:3) :: now
       
      cave(1, :) = (/2, 5, 8/)
      cave(2, :) = (/1, 3, 10/)
      cave(3, :) = (/2, 4, 12/)
      cave(4, :) = (/3, 5, 14/)
      cave(5, :) = (/1, 4, 6/)
      cave(6, :) = (/5, 7, 15/)
      cave(7, :) = (/6, 8, 17/)
      cave(8, :) = (/1, 7, 9/)
      cave(9, :) = (/8, 10, 18/)
      cave(10, :) = (/2, 9, 11/)
      cave(11, :) = (/10, 12, 19/)
      cave(12, :) = (/3, 11, 13/)
      cave(13, :) = (/12, 14, 20/)
      cave(14, :) = (/4, 13, 15/)
      cave(15, :) = (/6, 14, 16/)
      cave(16, :) = (/15, 17, 20/)
      cave(17, :) = (/7 ,16, 18/)
      cave(18, :) = (/9, 17, 19/)
      cave(19, :) = (/11, 18, 20/)
      cave(20, :) = (/13, 16, 19/)

      arrwnum=5

      CALL randnum(bat1)
      CALL randnum(bat2)
      CALL randnum(pit1)
      CALL randnum(pit2)
      CALL randnum(wmps)
      
      DO WHILE ((bat1 .EQ. bat2) .OR. (bat1 .EQ. pit1) 
     -    .OR. (bat1 .EQ. pit2)
     -    .OR. (bat2 .EQ. pit1) .OR. (bat2 .EQ. pit2)
     -    .OR. (pit1 .EQ. pit2) .OR. (wmps .EQ. bat1)
     -    .OR. (wmps .EQ. bat2) .OR. (wmps .EQ. pit1)
     -    .OR. (wmps .EQ. pit2)) 
      CALL randnum(bat1)
      CALL randnum(bat2)
      CALL randnum(pit1)
      CALL randnum(pit2)
      CALL randnum(wmps)      
      END DO
      
      CALL randnum(hunter)
      DO WHILE ((hunter .EQ. bat1).OR.(hunter .EQ. bat2)
     -   .OR.(hunter .EQ. pit1).OR.(hunter .EQ. pit2)
     -   .OR.(hunter .EQ. wmps)) 
      END DO

      END      

*************************************************************
*	  Random game plot generator
*************************************************************
      
      SUBROUTINE randnum(para)
      
      IMPLICIT NONE
      CHARACTER(1) :: user1, user, user3       
      INTEGER :: hunter, arrw, arrwnum, wmps, bat1, bat2, para
      INTEGER :: pit1, pit2 , inpt, win, inpt2, gameon, n, err
      INTEGER, DIMENSION(1:20,1:3) :: cave
      INTEGER, DIMENSION(1:3) :: now
         
         para = int(rand(0)*20)+1

      END      


*************************************************************
*	 End of game
*************************************************************

      SUBROUTINE gameover (win, now)

      IMPLICIT NONE
      CHARACTER(1) :: user1, user, user3    
      INTEGER :: pit1, pit2 , inpt, win, inpt2, n, gameon, err, para
      INTEGER, DIMENSION(1:20,1:3) :: cave
      INTEGER, DIMENSION(1:3) :: now

      IF (win .EQ. 1) THEN
         WRITE(*,*) 'CONGRATULATIONS! YOU WON!'
      ELSE
         WRITE(*,*) 'HAHAHA YOU LOST!'
      END IF

      gameon=0

      WRITE(*,*) 'GAME ENDED! ',
     - 'DO YOU WANT TO QUIT (Y-N)?'
20     READ (*, *) user1

      IF (.NOT. ((user1 .EQ. 'Y') .OR. (user1 .EQ. 'N') 
     -    .OR. (user1 .EQ. 'y').OR. (user1 .EQ. 'n'))) THEN
	       WRITE(*, *) 'INVALID INPUT. CHOOSE BETWEEN Y OR N:'
	       READ(*,*) user1
         GO TO 20        
      ELSE IF ((user1 .EQ. 'Y').OR.(user1 .EQ. 'y')) THEN
         WRITE(*,*) 'BYE BYE'
         STOP
      ELSE
         WRITE(*,*) 'SAME SET-UP (Y-N)?'
21       READ (*, *) user1
         IF (.NOT.((user1 .EQ. 'Y') .OR. (user1 .EQ. 'N') 
     -    .OR. (user1 .EQ. 'y').OR. (user1 .EQ. 'n'))) THEN
	       WRITE(*,*) 'INVALID INPUT. CHOOSE BETWEEN Y OR N'
         GO TO 21
         
         ELSE IF ((user1 .EQ. 'N') .OR. (user1 .EQ. 'n')) THEN
            gameon=1
            CALL itime(now) 
            CALL srand(now(1)+now(2)+now(3))
            CALL game(gameon, win)

         ELSE 
             CALL game(gameon, win)
         END IF

         
      END IF
      END 

      
*************************************************************
*	  Determines wumpus next move when disturbed
*************************************************************

      SUBROUTINE wump(arrw, wmps, cave, hunter)

      IMPLICIT NONE
      INTEGER :: hunter, arrw, arrwnum, wmps, bat1, bat2, para
      INTEGER :: pit1, pit2 , inpt, win, inpt2, gameon, n, err
      INTEGER, DIMENSION(1:20,1:3) :: cave
      INTEGER, DIMENSION(1:3) :: now
    
      IF (arrw .EQ. wmps) THEN
         IF (rand() .GT. 0.25) THEN
            wmps=cave(wmps,int(rand()*3)+1)
            WRITE(*,*) 'WUMPUS WAS DISTURBED AND MOVED TO OTHER ROOM'
         ELSE 
            WRITE(*,*) 'YOU KILLED THE WUMPUS!'
            win=1
            CALL gameover (win, now)
         END IF
       END IF  
        
      IF (hunter .EQ. wmps) THEN
         IF (rand() .GT. 0.25) THEN
            wmps=cave(wmps,int(rand()*3)+1)
         ELSE 
            win=0
            WRITE(*,*) 'YOU WERE EATEN BY THE WUMPUS SORRY'
            CALL gameover (win, now)
         END IF
       END IF  
       END 
                  
        
*************************************************************
*	  Hazard Detector for the player
*************************************************************

      SUBROUTINE detect(bat1, bat2, pit1, pit2, hunter, wmps, cave)
      
      IMPLICIT NONE
      CHARACTER(1) :: user1, user, user3       
      INTEGER :: hunter, arrw, arrwnum, wmps, bat1, bat2, para
      INTEGER :: pit1, pit2 , inpt, win, inpt2, n, gameon, err
      INTEGER, DIMENSION(1:20,1:3) :: cave
      INTEGER, DIMENSION(1:3) :: now
          
*     HAZARD 1. BATS
      IF ((bat1 .EQ. cave(hunter,1)) .OR.
     -    (bat2 .EQ. cave(hunter,1)) .OR.
     -    (bat1 .EQ. cave(hunter,2)) .OR.
     -    (bat2 .EQ. cave(hunter,2)) .OR.
     -    (bat1 .EQ. cave(hunter,2)) .OR.
     -    (bat2 .EQ. cave(hunter,3))) THEN
         WRITE(*,*) 'BATS NEAR YOU'
      END IF
      
*     HAZARD 2. PITS
      IF ((pit1 .EQ. cave(hunter,1)) .OR.
     -    (pit2 .EQ. cave(hunter,1)) .OR.
     -    (pit1 .EQ. cave(hunter,2)) .OR.
     -    (pit2 .EQ. cave(hunter,2)) .OR.
     -    (pit1 .EQ. cave(hunter,2)) .OR.
     -    (pit2 .EQ. cave(hunter,3))) THEN
         WRITE(*,*) 'I FEEL DRAFT..'
      END IF
      
*     HAZARD 3. WUMPUS
      IF ((wmps .EQ. cave(hunter,1)) .OR. 
     -    (wmps .EQ. cave(hunter,2)) .OR. 
     -    (wmps .EQ. cave(hunter,3))) THEN
         WRITE(*,*) 'I SMELL A WUMPUS..'
      END IF
      
      
      END 

*************************************************************
*	  When player move is to attack
*************************************************************

      SUBROUTINE shoot(hunter, bat1, bat2, pit1, pit2, 
     - wmps, cave, arrw, arrwnum)

      IMPLICIT NONE  
      INTEGER :: hunter, arrw, arrwnum, wmps, bat1, bat2, para
      INTEGER :: pit1, pit2 , inpt, win, inpt2, n, gameon, err
      INTEGER, DIMENSION(1:20,1:3) :: cave
      INTEGER, DIMENSION(1:3) :: now
      
      arrw=hunter      
      arrwnum=arrwnum-1

      WRITE(*,*) 'YOU CAN SHOOT UNTIL 5. HOW MANY:'
30    READ (*, *, IOSTAT = err) inpt
	    IF (err .NE. 0) THEN
	       WRITE(*, '(a$)')
     -	          'Please enter an integer input: '
           GO TO 30
      
        ELSE IF ((inpt .GT. 5) .OR. (inpt .LT. 1)) THEN
           WRITE(*,*) 'WRONG INPUT. CHOOSE FROM 1 TO 5. HOW MANY: '
           GO TO 30
        END IF

      DO n=1, inpt
         WRITE(*,*) 'WHERE TO?'
31       READ (*, *, IOSTAT = err) inpt2

		  IF (err .NE. 0) THEN
			 WRITE(*,*) 'PLEASE ENTER AN INTEGER VALUE:'
			 GO TO 31

		  ELSE IF ((inpt2 .GT. 20) .OR. (inpt2 .LT. 1)) THEN
			 WRITE(*,*) 'REMEMBER THERE ARE CAVES 1 TO 20. WHERE TO?'
			 GO TO 31    
		  END IF

      IF ((inpt2.EQ.cave(arrw,1)).OR.(inpt2.EQ.cave(arrw,2)) 
     - .OR. (inpt2.EQ.cave(arrw,3))) THEN
			 arrw=inpt2
			 CALL wump(arrw, wmps, cave, hunter)
			 CALL suicide(hunter, arrw, win, now)

 	  ELSE 
			 arrw=cave(arrw,int(rand(0)*3)+1)
			 WRITE(*,*) 'ARROW TO A RANDOM DIRECTION'

			 CALL wump(arrw, wmps, cave, hunter)
			 CALL suicide(hunter, arrw, win, now)
       END IF

       END DO

      WRITE(*,*) 'YOU MISSED!'

      IF (arrwnum .EQ. 0) THEN
         WRITE(*,*) 'YOU JUST USED UP ALL OF YOUR ARROWS. YOU LOST'
         win=0
         CALL gameover (win, now)
      END IF
      
      END 

*************************************************************
*	  Move player	 
*************************************************************

      SUBROUTINE move(hunter, bat1, bat2, pit1, pit2, wmps, cave)

      IMPLICIT NONE
      INTEGER :: hunter, arrw, arrwnum, wmps, bat1, bat2, para
      INTEGER :: pit1, pit2 , inpt, win, inpt2, room, gameon, err
      INTEGER, DIMENSION(1:20,1:3) :: cave
      INTEGER, DIMENSION(1:3) :: now
   
      WRITE(*,*) 'WHERE TO?'
40    READ (*,*, IOSTAT=err) room
	  
	  IF (err .NE. 0) THEN
		 WRITE(*,*) 'PLEASE ENTER AN INTEGER VALUE:'
		 GO TO 40
	  END IF
	  
      IF ((room .EQ. cave(hunter, 1)) .OR. (room .EQ. cave(hunter, 2)) 
     -    .OR. (room .EQ. cave(hunter, 3))) THEN
      hunter=room

      ELSE
         WRITE(*,*) 'YOU CAN ONLY GO TO ADJACENT ROOMS: '
         GO TO 40
      END IF   
      
*     HAZARD 1. BAT CHECK

         DO WHILE ((hunter .EQ. bat1) .OR. (hunter .EQ. bat2)) 
            CALL randnum(hunter)
            WRITE(*,*) 'ZAP--SUPER BAT SNATCH! ELSEWHEREVILLE FOR YOU!'
         END DO    

*     HAZARD 2. PIT CHECK	      
      IF ((hunter .EQ. pit1) .OR. (hunter .EQ. pit2)) THEN
           WRITE(*,*) 'YYIIIIEEEE . . . FELL IN A PIT'
           CALL gameover(win, now)
      END IF

*     HAZARD 3. WUMPUS CHECK
      CALL wump(arrw, wmps, cave, hunter)

      END


*************************************************************
*	  Check if the player shoot himself in the back
*************************************************************

      subroutine suicide(hunter, arrw, win, now)

      IMPLICIT NONE
      INTEGER :: hunter, arrw, win
      INTEGER, DIMENSION(1:3) :: now

		 IF (hunter .EQ. arrw) THEN
			WRITE(*,*) 'YOU SHOOT YOURSELF IN THE BACK. GAME OVER'
			win=0
			CALL gameover (win, now)
		 END IF
      END


***************************************************************
***************************************************************

      SUBROUTINE Instructions
         WRITE(*, '(/6a)')
     -	    'WELCOME TO HUNT THE WUMPUS!! ',
     -	    'THE WUMPUS LIVES IN A CAVE OF 20 ROOMS. ',
     -	    'EACH ROOM HAS 3 TUNNELS LEADNG TO OTHER ROOMS',
     -	    'LOOK AT A DODECAHEDRON TO SEE HOW IT WORKS- ',
     -	    'IF YOU DONNOT KNOW WHAT A DODECAHEDRON IS,',
     -	    'ASK SOMEONE '
         WRITE(*, '(/12a)')
     -	    'HAZARDS:  ',
     -	    'BOTTEMLESS PITS: TWO ROOMS HAVE BOTTOMLESS PITS',
     -	    'IF YOU GO THERE, YOU FALL INTO THE PIT AND LOSE ',
     -	    'SUPER BATS: TWO OTHER ROOMS HAVE SUPER BATS',
     -	    'IF YOU GO THERE, A BAT WILL MOVE YOU TO A RANDOM ROOM ',
     -	    'WUMPUS:',
     -	    'THE WUMPUS ONLY BOTHERED BY YOU ENTERING HIS ROOM OR',
     -	    'YOUR ARROW SHOOTS INTO HIS ROOM ',
     -	    'IF HE IS BOTHERED, HE MAY MOVE TO ANOTHER ROOM, ',
     -	    'OR CHOOSE TO STAY IN THE SAME ROOM ',
     -	    'IF YOU ARE IN THE SAME ROOM WITH THE WUMPUS, ',
     -	    'HE WILL EAT YOU UP, AND YOU LOSE '
     
        WRITE(*, '(/10a)')
     -	    'YOU: ',
     -	    'EACH TURN YOU CAN MOVE OR SHOOT A CROOKED ARROW.',
     -	    'MOVE: YOU CAN MOVE ONE ROOM (THROUGH ONE TUNNEL.) ',
     -	    'SHOOT: YOU HAVE 5 ARROWS. YOU LOSE WHEN YOU RUN OUT ',
     -	    'EACH ARROW CAN GO FROM 1 TO 5 ROOMS. YOU AIM BY TELLING ',
     -	    'THE COMPUTER THE ROOM #S YOU WANT THE ARROW TO GO TO. ',
     -	    'IF THE ARROW CANNOT GO THAT WAY,',
     -	    'IT MOVES AT RANDOM TO THE NEXT ROOM.',
     -	    'IF THE ARROW HITS THE WUMPUS, YOU WIN. ',
     -	    'IF THE ARROW HITS YOU, YOU LOSE.'
        WRITE(*, '(/6a)')     
     -	    'WARNINGS: ',
     -	    'WHEN YOU ARE ONE ROOM AWAY FROM WUMPUS OR HAZARD,',
     -	    'THE COMPUTER SAYS ',
     -	    'WUMPUS: I SMELL A WUMPUS',
     -	    'BAT: BATS NEARBY',
     -      'PIT: I FEEL A DRAFT'
        WRITE(*, '(/2a)')
     -      '*****',
     -      'HUNT THE WUMPUS'
*
      END
*

*******************************************************
*******************************************************
