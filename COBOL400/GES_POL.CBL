       IDENTIFICATI*N DIVISION.
       PROGRAM-ID. GES*OL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

      FILE-CONTROL.

           SELECT*MENUFILE
                  ASSIGN *O WORKSTN.

       DATA DIVISION.
       FILE SECTION.

       FD ME*UFILE.

       01 MENUGES-R.
          05 SCELTA          PIC X.
          05 MSG             PIC X(50).*
       WORKING-STORAGE SECTION.

       01 WS-FINE            PIC X *ALUE 'N'.

          88 FINE-PROGR*MMA VALUE 'S'.

       01 WS-SCELTA         PIC X.

       PROCEDURE DIVISION.

       MAIN.

              PERFORM UNTIL FINE-PROGRAMMA

              MOVE SPACES TO MSG

              WRITE MENUGES-R
              READ MENUFILE

              IF 'IN03'

                 MOVE 'S'
                      TO WS-FINE

              ELSE

                 MOVE  SCELTA
                   TO WS-SCELTA

                 PERFORM ELABORA-SCELTA

              END-IF

           END-PERFORM

           ROLLBACK.

       ELABORA-SCELTA.

              EVALUATE WS-SCELTA

                 WHEN '1'

                   CALL 'INSPOL'

              WHEN*'2'

                   CALL 'VARP*L'

              WHEN '3'

                   CALL 'ANNPOL'

              WHEN '4'

                   CALL 'VISUPOL'

              WHEN*'5'

                   CALL 'POLS*A'

              WHEN OTHER

                   MOVE
                       'SCELTA NON VALIDA'
                   TO MSG

           END-EVAL*ATE.
