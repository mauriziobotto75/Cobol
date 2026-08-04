       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLIMGR.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT DSPFILE
                  ASSIGN TO WORKSTN.

       DATA DIVISION.

       FILE SECTION.

       FD DSPFILE.

       01 MENUCLI-R.
          05 SCELTA PIC X.
          05 MSG    PIC X(50).

       WORKING-STORAGE SECTION.

       01 FINE-PGM PIC X VALUE 'N'.

       PROCEDURE DIVISION.

       MAIN.

           PERFORM UNTIL FINE-PGM = 'S'

              MOVE SPACES TO MSG

              WRITE MENUCLI-R
              READ DSPFILE

              IF IN03
                 MOVE 'S' TO FINE-PGM
              ELSE

                 EVALUATE SCELTA

                    WHEN '1'
                       CALL 'CLIINS'

                    WHEN '2'
                       CALL 'CLIVAR'

                    WHEN '3'
                       CALL 'CLIRIC'

                    WHEN '4'
                       CALL 'CLICAN'

                    WHEN '9'
                       MOVE 'S' TO FINE-PGM

                    WHEN OTHER
                       MOVE
                       'SCELTA NON VALIDA'
                       TO MSG

                 END-EVALUATE

              END-IF

           END-PERFORM

           GOBACK.
