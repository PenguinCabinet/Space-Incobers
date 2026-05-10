       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOBER-GAME.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CONSTANTS.
           05  GAME-WIDTH          PIC 99 VALUE 40.
           05  GAME-HEIGHT         PIC 99 VALUE 20.

       01  GAME-STATE.
           05  GAME-OVER-FLAG      PIC X VALUE 'N'.
           05  FRAME-COUNT         PIC 9(4) VALUE 0.
           05  GAME-SCORE          PIC 9(6) VALUE 0.

       01  PLAYER-STUFF.
           05  PLAYER-X            PIC 99 VALUE 20.
           05  PLAYER-Y            PIC 99 VALUE 19.
           05  PLAYER-CHAR         PIC X VALUE "@".

       01  BULLET-STUFF.
           05  BULLET-ACTIVE       PIC X VALUE 'N'.
           05  BULLET-X            PIC 99.
           05  BULLET-Y            PIC 99.

       01  INVADER-STUFF.
           05  INVADER-COUNT       PIC 99 VALUE 15.
           05  INVADER-DIRECTION   PIC S9 VALUE 1.
           05  INVADER-MOVE-SPEED  PIC 99 VALUE 5.
           05  INVADER-TABLE OCCURS 15 TIMES INDEXED BY I-IDX.
               10  INV-ACTIVE      PIC X.
               10  INV-X           PIC 99.
               10  INV-Y           PIC 99.

       01  BUNKER-STUFF.
           05  BUNKER-COUNT        PIC 99 VALUE 27.
           05  BUNKER-TABLE OCCURS 27 TIMES INDEXED BY B-IDX.
               10  BUNK-ACTIVE     PIC X.
               10  BUNK-X          PIC 99.
               10  BUNK-Y          PIC 99.

       01  WORK-VARS.
           05  INPUT-CHAR              PIC X.
           05  HIT-EDGE                PIC X VALUE 'N'.
           05  REMAINING-INV           PIC 99 VALUE 0.
           05  NANOSECONDS             PIC 9(18) COMP-5 VALUE 50000000.
           05  TIMEOUT-VAL             PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-GAME.
           PERFORM GAME-LOOP UNTIL GAME-OVER-FLAG = 'Y'.
           PERFORM FINALIZE-GAME.
           STOP RUN.

       INITIALIZE-GAME.
           MOVE 20 TO PLAYER-X.
           MOVE 19 TO PLAYER-Y.
           MOVE 'N' TO GAME-OVER-FLAG.
           MOVE 0 TO GAME-SCORE.
           MOVE 0 TO FRAME-COUNT.
           
           PERFORM VARYING I-IDX FROM 1 BY 1 UNTIL I-IDX > 15
               IF I-IDX <= 5
                   MOVE 2 TO INV-Y (I-IDX)
                   COMPUTE INV-X (I-IDX) = I-IDX * 6
               ELSE 
                   IF I-IDX <= 10
                       MOVE 4 TO INV-Y (I-IDX)
                       COMPUTE INV-X (I-IDX) = (I-IDX - 5) * 6
                   ELSE
                       MOVE 6 TO INV-Y (I-IDX)
                       COMPUTE INV-X (I-IDX) = (I-IDX - 10) * 6
                   END-IF
               END-IF
               MOVE 'Y' TO INV-ACTIVE (I-IDX)
           END-PERFORM.

           PERFORM VARYING B-IDX FROM 1 BY 1 UNTIL B-IDX > 27
               MOVE 'Y' TO BUNK-ACTIVE (B-IDX)
               
               IF B-IDX <= 9
                   COMPUTE BUNK-Y (B-IDX) = 14 + 
                     FUNCTION INTEGER-PART((B-IDX - 1) / 3)
                   COMPUTE BUNK-X (B-IDX) = 8 + 
                     FUNCTION MOD(B-IDX - 1, 3)
               ELSE
                   IF B-IDX <= 18
                       COMPUTE BUNK-Y (B-IDX) = 14 + 
                         FUNCTION INTEGER-PART((B-IDX - 10) / 3)
                       COMPUTE BUNK-X (B-IDX) = 18 + 
                         FUNCTION MOD(B-IDX - 10, 3)
                   ELSE
                       COMPUTE BUNK-Y (B-IDX) = 14 + 
                         FUNCTION INTEGER-PART((B-IDX - 19) / 3)
                       COMPUTE BUNK-X (B-IDX) = 28 + 
                         FUNCTION MOD(B-IDX - 19, 3)
                   END-IF
               END-IF
           END-PERFORM.

       GAME-LOOP.
           ADD 1 TO FRAME-COUNT.
           PERFORM DRAW-SCREEN.
           PERFORM GET-INPUT.
           PERFORM UPDATE-GAME.
           CALL "CBL_GC_NANOSLEEP" USING NANOSECONDS.

       GET-INPUT.
           MOVE SPACE TO INPUT-CHAR.
           ACCEPT INPUT-CHAR LINE 1 COLUMN 1 
                  WITH TIMEOUT TIMEOUT-VAL.
           
           EVALUATE INPUT-CHAR
               WHEN 'a'
                   IF PLAYER-X > 1 SUBTRACT 1 FROM PLAYER-X END-IF
               WHEN 'd'
                   IF PLAYER-X < GAME-WIDTH ADD 1 TO PLAYER-X END-IF
               WHEN 'w'
                   IF BULLET-ACTIVE = 'N'
                       MOVE 'Y' TO BULLET-ACTIVE
                       MOVE PLAYER-X TO BULLET-X
                       MOVE PLAYER-Y TO BULLET-Y
                   END-IF
               WHEN 'q'
                   MOVE 'Y' TO GAME-OVER-FLAG
           END-EVALUATE.

       UPDATE-GAME.
           IF BULLET-ACTIVE = 'Y'
               IF BULLET-Y > 1
                   SUBTRACT 1 FROM BULLET-Y
                   PERFORM CHECK-COLLISION
               ELSE
                   MOVE 'N' TO BULLET-ACTIVE
               END-IF
           END-IF.

           IF FUNCTION MOD(FRAME-COUNT, INVADER-MOVE-SPEED) = 0
               PERFORM MOVE-INVADERS
           END-IF.

       MOVE-INVADERS.
           MOVE 'N' TO HIT-EDGE.
           PERFORM VARYING I-IDX FROM 1 BY 1 UNTIL I-IDX > 15
               IF INV-ACTIVE (I-IDX) = 'Y'
                   ADD INVADER-DIRECTION TO INV-X (I-IDX)
                   IF (INV-X (I-IDX) >= GAME-WIDTH) OR 
                      (INV-X (I-IDX) <= 1)
                       MOVE 'Y' TO HIT-EDGE
                   END-IF
                   
                   PERFORM CHECK-INVADER-BUNKER-COLLISION

                   IF INV-Y (I-IDX) >= PLAYER-Y
                       MOVE 'Y' TO GAME-OVER-FLAG
                   END-IF
               END-IF
           END-PERFORM.

           IF HIT-EDGE = 'Y'
               MULTIPLY -1 BY INVADER-DIRECTION
               PERFORM VARYING I-IDX FROM 1 BY 1 UNTIL I-IDX > 15
                   IF INV-ACTIVE (I-IDX) = 'Y'
                       ADD 1 TO INV-Y (I-IDX)
                       PERFORM CHECK-INVADER-BUNKER-COLLISION
                   END-IF
               END-PERFORM
           END-IF.

       CHECK-INVADER-BUNKER-COLLISION.
           PERFORM VARYING B-IDX FROM 1 BY 1 UNTIL B-IDX > 27
               IF BUNK-ACTIVE (B-IDX) = 'Y'
                   IF INV-X (I-IDX) = BUNK-X (B-IDX) AND
                      INV-Y (I-IDX) = BUNK-Y (B-IDX)
                       MOVE 'N' TO BUNK-ACTIVE (B-IDX)
                   END-IF
               END-IF
           END-PERFORM.

       CHECK-COLLISION.
           PERFORM VARYING I-IDX FROM 1 BY 1 UNTIL I-IDX > 15
               IF INV-ACTIVE (I-IDX) = 'Y'
                   IF BULLET-X = INV-X (I-IDX)
                       IF BULLET-Y = INV-Y (I-IDX)
                           MOVE 'N' TO INV-ACTIVE (I-IDX)
                           MOVE 'N' TO BULLET-ACTIVE
                           ADD 100 TO GAME-SCORE
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

           IF BULLET-ACTIVE = 'Y'
               PERFORM VARYING B-IDX FROM 1 BY 1 UNTIL B-IDX > 27
                   IF BUNK-ACTIVE (B-IDX) = 'Y'
                       IF BULLET-X = BUNK-X (B-IDX)
                           IF BULLET-Y = BUNK-Y (B-IDX)
                               MOVE 'N' TO BUNK-ACTIVE (B-IDX)
                               MOVE 'N' TO BULLET-ACTIVE
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF.
           
           MOVE 0 TO REMAINING-INV.
           PERFORM VARYING I-IDX FROM 1 BY 1 UNTIL I-IDX > 15
               IF INV-ACTIVE (I-IDX) = 'Y' ADD 1 TO REMAINING-INV END-IF
           END-PERFORM.
           IF REMAINING-INV = 0
               MOVE 'Y' TO GAME-OVER-FLAG
           END-IF.

       DRAW-SCREEN.
           DISPLAY " " LINE 1 COLUMN 1 WITH BLANK SCREEN.
           DISPLAY "SCORE: " LINE 1 COLUMN 1.
           DISPLAY GAME-SCORE LINE 1 COLUMN 8.

           DISPLAY PLAYER-CHAR LINE PLAYER-Y COLUMN PLAYER-X.

           IF BULLET-ACTIVE = 'Y'
               DISPLAY "|" LINE BULLET-Y COLUMN BULLET-X
           END-IF.

           PERFORM VARYING I-IDX FROM 1 BY 1 UNTIL I-IDX > 15
               IF INV-ACTIVE (I-IDX) = 'Y'
                   DISPLAY "W" LINE INV-Y (I-IDX) COLUMN INV-X (I-IDX)
               END-IF
           END-PERFORM.

           PERFORM VARYING B-IDX FROM 1 BY 1 UNTIL B-IDX > 27
               IF BUNK-ACTIVE (B-IDX) = 'Y'
                   DISPLAY "#" LINE BUNK-Y (B-IDX) COLUMN BUNK-X (B-IDX)
               END-IF
           END-PERFORM.

       FINALIZE-GAME.
           DISPLAY " " LINE 1 COLUMN 1 WITH BLANK SCREEN.
           DISPLAY "GAME OVER" LINE 10 COLUMN 15.
           DISPLAY "FINAL SCORE: " LINE 11 COLUMN 15.
           DISPLAY GAME-SCORE LINE 11 COLUMN 28.
           DISPLAY "PRESS ANY KEY TO EXIT" LINE 13 COLUMN 15.
           ACCEPT INPUT-CHAR.
