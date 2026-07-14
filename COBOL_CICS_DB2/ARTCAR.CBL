        IDENTIFICATION DIVISION.
       PROGRAM-ID. ARTCAR.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       01  WS-FINE            PIC X VALUE 'N'.

       01  WS-CODART          PIC 9(5).
       01  WS-DESART          PIC X(25).
       01  WS-PRZART          PIC 9(6).
       01  WS-IMPUNIT         PIC 9(4)V99.
       01  WS-QUANTART        PIC 9(6).

       COPY ARTMAP.

       PROCEDURE DIVISION.

       MAIN-LOGIC.

           PERFORM UNTIL WS-FINE = 'S'

              EXEC CICS
                   SEND MAP('ARTINS')
                   MAPSET('ARTMAP')
                   ERASE
              END-EXEC

              EXEC CICS
                   RECEIVE MAP('ARTINS')
                   MAPSET('ARTMAP')
              END-EXEC

              MOVE CODARTFI TO WS-CODART

              IF WS-CODART = 0

                 MOVE 'S' TO WS-FINE

              ELSE

                 MOVE DESARTFI TO WS-DESART
                 MOVE PRZARTFI TO WS-PRZART
                 MOVE IMPUNTFI TO WS-IMPUNIT
                 MOVE QUANTFI  TO WS-QUANTART

                 EXEC SQL

                    INSERT INTO ARTICOLI
                    (
                      CODART,
                      DESART,
                      PRZART,
                      IMPUNIT,
                      QUANTART
                    )
                    VALUES
                    (
                      :WS-CODART,
                      :WS-DESART,
                      :WS-PRZART,
                      :WS-IMPUNIT,
                      :WS-QUANTART
                    )

                 END-EXEC

                 IF SQLCODE NOT = 0

                    EXEC CICS
                       SEND TEXT
                       FROM('ERRORE DB2 INSERT')
                       LENGTH(18)
                    END-EXEC

                 END-IF

              END-IF

           END-PERFORM

           EXEC CICS
                RETURN
           END-EXEC.
