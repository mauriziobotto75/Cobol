       IDENTIFICATI*N DIVISION.
       PROGRAM-ID. GES_POLIZZE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

      FILE-CONTROL.

           SELECT MENUFILE
                  ASSIGN *O WORKSTN.

       DATA DIVISION.
       FILE SECTION.

       FD MENUFILE.

       01 MENUGES-R.
          05 SCELTA          PIC X.
          05 MSG             PIC X(50).
       WORKING-STORAGE SECTION.

       01 WS-FINE            PIC X VALUE 'N'.

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

          CALL 'INSPOL'

USING AREA-COMUNE
 
 
01 AREA-COMUNE.
 
05 UTENTE PIC X(10).
 
05 COD-POLIZZA PIC X(10).
 
05 MESSAGGIO PIC X(50).
 
WHEN '1'
 
CALL 'INSPOL'
 
USING AREA-COMUNE
 
WHEN '2'
 
CALL 'VARPOL'
 
USING AREA-COMUNE
 
WHEN '3'
 
CALL 'ANNPOL'
 
USING AREA-COMUNE
 
WHEN '4'
 
CALL 'VISUPOL'
 
USING AREA-COMUNE
 
WHEN '5'
 
CALL 'POLSTA'
 
USING AREA-COMUNE
