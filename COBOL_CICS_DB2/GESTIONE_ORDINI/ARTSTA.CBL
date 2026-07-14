       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARTSTA.

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       COPY DFHAID.

       COPY ARTSTAM.

       01 WS-CODART-DA       PIC 9(5).
       01 WS-CODART-A        PIC 9(5).

       01 WS-CODART          PIC 9(5).
       01 WS-DESART          PIC X(25).
       01 WS-PRZART          PIC 9(6).
       01 WS-IMPUNIT         PIC 9(4)V99.
       01 WS-QUANTART        PIC 9(6).

       01 WS-FINE            PIC X VALUE 'N'.

       01 WS-HEADER.
          05 FILLER PIC X(80)
             VALUE
             'CODICE DESCRIZIONE                IMPORTO   GIACENZA'.

       01 WS-RIGA.
          05 WS-CODART-X     PIC ZZZZ9.
          05 FILLER          PIC X VALUE SPACE.
          05 WS-DESART-X     PIC X(25).
          05 FILLER          PIC X VALUE SPACE.
          05 WS-IMPUNIT-X    PIC ZZZ9,99.
          05 FILLER          PIC X VALUE SPACE.
          05 WS-QTA-X        PIC ZZZZZ9.

      ********************************************************
      * CURSORE DB2
      ********************************************************

       EXEC SQL

          DECLARE C-ART CURSOR FOR

          SELECT CODART,
                 DESART,
                 PRZART,
                 IMPUNIT,
                 QUANTART

            FROM ARTICOLI

           WHERE CODART BETWEEN
                 :WS-CODART-DA
                 AND
                 :WS-CODART-A

           ORDER BY CODART

       END-EXEC.

       PROCEDURE DIVISION.

       MAIN-LOGIC.

      ********************************************************
      * VISUALIZZA MAPPA
      ********************************************************

           EXEC CICS
                SEND MAP('ARTSTA')
                MAPSET('ARTSTAM')
                ERASE
           END-EXEC

           EXEC CICS
                RECEIVE MAP('ARTSTA')
                MAPSET('ARTSTAM')
           END-EXEC

      ********************************************************
      * PF3 USCITA
      ********************************************************

           IF EIBAID = DFHPF3
              EXEC CICS
                   RETURN
              END-EXEC
           END-IF

      ********************************************************
      * PF12 MENU
      ********************************************************

           IF EIBAID = DFHPF12
              EXEC CICS
                   XCTL
                   PROGRAM('MENU')
              END-EXEC
           END-IF

      ********************************************************
      * PARAMETRI
      ********************************************************

           MOVE CODDAI TO WS-CODART-DA
           MOVE CODAI  TO WS-CODART-A

           IF WS-CODART-A = ZERO
              MOVE 99999 TO WS-CODART-A
           END-IF

      ********************************************************
      * INTESTAZIONE REPORT
      ********************************************************

           EXEC CICS
                WRITEQ TD
                QUEUE('RPTART')
                FROM(WS-HEADER)
                LENGTH(LENGTH OF WS-HEADER)
           END-EXEC

      ********************************************************
      * OPEN CURSOR
      ********************************************************

           EXEC SQL
                OPEN C-ART
           END-EXEC

           IF SQLCODE NOT = ZERO
              PERFORM ERRORE-DB2
           END-IF

      ********************************************************
      * FETCH LOOP
      ********************************************************

           PERFORM UNTIL WS-FINE = 'S'

              EXEC SQL

                   FETCH C-ART

                   INTO
                        :WS-CODART,
                        :WS-DESART,
                        :WS-PRZART,
                        :WS-IMPUNIT,
                        :WS-QUANTART

              END-EXEC

              EVALUATE SQLCODE

                 WHEN 0

                    MOVE WS-CODART
                      TO WS-CODART-X

                    MOVE WS-DESART
                      TO WS-DESART-X

                    MOVE WS-IMPUNIT
                      TO WS-IMPUNIT-X

                    MOVE WS-QUANTART
                      TO WS-QTA-X

                    EXEC CICS
                         WRITEQ TD
                         QUEUE('RPTART')
                         FROM(WS-RIGA)
                         LENGTH(LENGTH OF WS-RIGA)
                    END-EXEC

                 WHEN 100

                    MOVE 'S'
                      TO WS-FINE

                 WHEN OTHER

                    PERFORM ERRORE-DB2

              END-EVALUATE

           END-PERFORM

      ********************************************************
      * CLOSE CURSOR
      ********************************************************

           EXEC SQL
                CLOSE C-ART
           END-EXEC

      ********************************************************
      * MESSAGGIO FINALE
      ********************************************************

           MOVE
           'REPORT ARTICOLI GENERATO CORRETTAMENTE'
             TO MSGO

           EXEC CICS
                SEND MAP('ARTSTA')
                MAPSET('ARTSTAM')
           END-EXEC

           EXEC CICS
                RETURN
           END-EXEC.

      ********************************************************
      * ERRORE DB2
      ********************************************************

       ERRORE-DB2.

           MOVE
           'ERRORE DB2 DURANTE LA GENERAZIONE REPORT'
             TO MSGO

           EXEC CICS
                SEND MAP('ARTSTA')
                MAPSET('ARTSTAM')
           END-EXEC

           EXEC CICS
                RETURN
           END-EXEC.
