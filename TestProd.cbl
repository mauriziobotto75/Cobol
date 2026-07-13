       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROD.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT PRODOTTI
               ASSIGN TO DATABASE-PRODOTTI
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CODICE-PRODOTTO
               FILE STATUS IS FS-PRODOTTI.

       DATA DIVISION.

       FILE SECTION.

       FD PRODOTTI.

       01 RECORD-PRODOTTO.
          05 CODICE-PRODOTTO    PIC X(5).
          05 DESCRIZIONE        PIC X(30).
          05 PREZZO             PIC 9(6)V99.
          05 SCORTA-MINIMA      PIC 9(3).

       WORKING-STORAGE SECTION.

       01 FS-PRODOTTI           PIC XX.

       01 SW-FINE               PIC X VALUE 'N'.
          88 EOF-PRODOTTI       VALUE 'S'.

       01 WS-CODICE-RICERCA     PIC X(5).

      *********************************************************
       PROCEDURE DIVISION.
      *********************************************************

       MAIN.

      *-----------------------------------------
      * OPEN FILE
      *-----------------------------------------

           OPEN INPUT PRODOTTI

           IF FS-PRODOTTI NOT = '00'
              DISPLAY 'ERRORE OPEN FILE'
              DISPLAY 'FILE STATUS = ' FS-PRODOTTI
              STOP RUN
           END-IF

      *-----------------------------------------
      * LETTURA DIRETTA PER CHIAVE
      *-----------------------------------------

           DISPLAY 'INSERIRE CODICE PRODOTTO'
           ACCEPT WS-CODICE-RICERCA

           MOVE WS-CODICE-RICERCA
             TO CODICE-PRODOTTO

           READ PRODOTTI

                INVALID KEY

                   DISPLAY
                   'PRODOTTO NON TROVATO'

                   DISPLAY
                   'STATUS = '
                   FS-PRODOTTI

                NOT INVALID KEY

                   DISPLAY 'PRODOTTO TROVATO'
                   DISPLAY 'CODICE      : '
                           CODICE-PRODOTTO
                   DISPLAY 'DESCRIZIONE : '
                           DESCRIZIONE
                   DISPLAY 'PREZZO      : '
                           PREZZO
                   DISPLAY 'SCORTA MIN. : '
                           SCORTA-MINIMA

           END-READ

      *-----------------------------------------
      * LETTURA SEQUENZIALE DA UNA CHIAVE
      *-----------------------------------------

           MOVE WS-CODICE-RICERCA
             TO CODICE-PRODOTTO

           START PRODOTTI
                KEY IS NOT LESS THAN
                CODICE-PRODOTTO

                INVALID KEY

                   DISPLAY
                   'NESSUN RECORD SUCCESSIVO'

                   GO TO FINE-PROGRAMMA

           END-START

           MOVE 'N' TO SW-FINE

           PERFORM UNTIL EOF-PRODOTTI

               READ PRODOTTI NEXT RECORD

                    AT END
                       SET EOF-PRODOTTI TO TRUE

                    NOT AT END

                       DISPLAY
                       '--------------------'

                       DISPLAY
                       CODICE-PRODOTTO
                       ' '
                       DESCRIZIONE

               END-READ

           END-PERFORM

      *-----------------------------------------
      * CLOSE FILE
      *-----------------------------------------

       FINE-PROGRAMMA.

           CLOSE PRODOTTI

           IF FS-PRODOTTI NOT = '00'
              DISPLAY 'ERRORE CLOSE'
              DISPLAY 'STATUS = '
                      FS-PRODOTTI
           END-IF

           DISPLAY 'FINE ELABORAZIONE'

           STOP RUN.
