       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARTVAR.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       01 WS-FINE          PIC X VALUE 'N'.

       01 WS-CODART        PIC 9(5).
       01 WS-DESART        PIC X(25).
       01 WS-PRZART        PIC 9(6).
       01 WS-IMPUNIT       PIC 9(4)V99.
       01 WS-QUANTART      PIC 9(6).

       01 WS-MSG           PIC X(60).

       COPY ARTVARM.

       PROCEDURE DIVISION.

       MAIN-PROCESS.

           PERFORM UNTIL WS-FINE = 'S'

              PERFORM LEGGI-ARTICOLO

           END-PERFORM

           EXEC CICS RETURN END-EXEC.

      *------------------------------------------------*
       LEGGI-ARTICOLO.
      *------------------------------------------------*

           MOVE SPACES TO MSGFO

           EXEC CICS
                SEND MAP('ARTVAR')
                MAPSET('ARTVARM')
                ERASE
           END-EXEC

           EXEC CICS
                RECEIVE MAP('ARTVAR')
                MAPSET('ARTVARM')
           END-EXEC

           IF EIBAID = DFHPF3
              MOVE 'S' TO WS-FINE
              EXIT PARAGRAPH
           END-IF

           MOVE CODARTFI TO WS-CODART

           EXEC SQL

              SELECT DESART,
                     PRZART,
                     IMPUNIT,
                     QUANTART
              INTO  :WS-DESART,
                    :WS-PRZART,
                    :WS-IMPUNIT,
                    :WS-QUANTART
              FROM ARTICOLI
              WHERE CODART = :WS-CODART

           END-EXEC

           IF SQLCODE = 100

              MOVE 'ARTICOLO NON TROVATO'
                TO MSGFO

              EXEC CICS
                   SEND MAP('ARTVAR')
                   MAPSET('ARTVARM')
                   CURSOR
              END-EXEC

              EXIT PARAGRAPH

           END-IF

           MOVE WS-DESART
             TO DESARTFO

           MOVE WS-PRZART
             TO PRZARTFO

           MOVE WS-IMPUNIT
             TO IMPUNTFO

           MOVE WS-QUANTART
             TO QUANTFO

           MOVE 'MODIFICARE I DATI E PREMERE ENTER'
             TO MSGFO

           EXEC CICS
                SEND MAP('ARTVAR')
                MAPSET('ARTVARM')
                CURSOR
           END-EXEC

           EXEC CICS
                RECEIVE MAP('ARTVAR')
                MAPSET('ARTVARM')
           END-EXEC

           IF EIBAID = DFHPF3
              MOVE 'S' TO WS-FINE
              EXIT PARAGRAPH
           END-IF

           MOVE DESARTFI TO WS-DESART
           MOVE PRZARTFI TO WS-PRZART
           MOVE IMPUNTFI TO WS-IMPUNIT
           MOVE QUANTFI  TO WS-QUANTART

           EXEC SQL

              UPDATE ARTICOLI
                 SET DESART   = :WS-DESART,
                     PRZART   = :WS-PRZART,
                     IMPUNIT  = :WS-IMPUNIT,
                     QUANTART = :WS-QUANTART
               WHERE CODART   = :WS-CODART

           END-EXEC

           IF SQLCODE = 0

              MOVE 'AGGIORNAMENTO ESEGUITO'
                TO MSGFO

           ELSE

              MOVE 'ERRORE DB2 IN UPDATE'
                TO MSGFO

           END-IF

           EXEC CICS
                SEND MAP('ARTVAR')
                MAPSET('ARTVARM')
                CURSOR
           END-EXEC.
