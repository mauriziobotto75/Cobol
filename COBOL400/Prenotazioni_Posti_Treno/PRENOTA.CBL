        IDENTIFICATION DIVISION.
       PROGRAM-ID. PRENOTA.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT FILE-PRENOTA ASSIGN TO PRENOTA
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

           SELECT FILE-LISTA ASSIGN TO LISTA
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

      *---------------------------------------
      * FILE PRENOTAZIONI
      *---------------------------------------
       FD FILE-PRENOTA.

       01 REC-PRENOTA.
          05 PR-TIPO           PIC X.
          05 PR-NPOSTI         PIC 9(4).
          05 PR-CODAG          PIC X(4).
          05 PR-DATAP          PIC 9(8).

      *---------------------------------------
      * FILE PRENOTAZIONI NON EVASE
      *---------------------------------------
       FD FILE-LISTA.

       01 REC-LISTA.
          05 LS-CODAG          PIC X(4).
          05 LS-NPOSTI         PIC ZZZ9.
          05 LS-DATAP          PIC 9(8).

       WORKING-STORAGE SECTION.

       77 FINE-FILE            PIC X VALUE 'N'.

       77 TOT-POSTI            PIC 9(5).
       77 QUOTA-POSTI          PIC 9(5).
       77 RESTO                PIC 9.

       77 DISP-FINESTRINO      PIC 9(5).
       77 DISP-CENTRO          PIC 9(5).
       77 DISP-CORRIDOIO       PIC 9(5).

      *---------------------------------------
      * TESTATA STAMPE
      *---------------------------------------

       01 MSG-ERRORE.
          05 FILLER PIC X(35)
             VALUE 'NUMERO POSTI NON MULTIPLO DI 3'.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

      *---------------------------------------
      * ACQUISIZIONE TOTALE POSTI
      *---------------------------------------

           PERFORM ACQUISISCI-POSTI

      *---------------------------------------
      * APERTURA FILE
      *---------------------------------------

           OPEN INPUT FILE-PRENOTA
                OUTPUT FILE-LISTA

      *---------------------------------------
      * PRIMA LETTURA
      *---------------------------------------

           PERFORM LEGGI-PRENOTA

      *---------------------------------------
      * ELABORAZIONE FILE
      *---------------------------------------

           PERFORM UNTIL FINE-FILE = 'S'

              EVALUATE PR-TIPO

                 WHEN 'P'

                    IF PR-NPOSTI <= DISP-FINESTRINO
                       SUBTRACT PR-NPOSTI
                           FROM DISP-FINESTRINO
                    ELSE
                       PERFORM SCRIVI-NON-EVASA
                    END-IF

                 WHEN 'N'

                    IF PR-NPOSTI <= DISP-CENTRO
                       SUBTRACT PR-NPOSTI
                           FROM DISP-CENTRO
                    ELSE
                       PERFORM SCRIVI-NON-EVASA
                    END-IF

                 WHEN 'C'

                    IF PR-NPOSTI <= DISP-CORRIDOIO
                       SUBTRACT PR-NPOSTI
                           FROM DISP-CORRIDOIO
                    ELSE
                       PERFORM SCRIVI-NON-EVASA
                    END-IF

                 WHEN OTHER

                    DISPLAY
                    'TIPO PRENOTAZIONE NON VALIDO'

              END-EVALUATE

              PERFORM LEGGI-PRENOTA

           END-PERFORM

      *---------------------------------------
      * CHIUSURA FILE
      *---------------------------------------

           CLOSE FILE-PRENOTA
                 FILE-LISTA

      *---------------------------------------
      * RIEPILOGO
      *---------------------------------------

           DISPLAY ' '
           DISPLAY 'POSTI RESIDUI'
           DISPLAY 'FINESTRINO : ' DISP-FINESTRINO
           DISPLAY 'CENTRO     : ' DISP-CENTRO
           DISPLAY 'CORRIDOIO  : ' DISP-CORRIDOIO

           STOP RUN.

      *=======================================
      * ACQUISIZIONE POSTI
      *=======================================

       ACQUISISCI-POSTI.

           MOVE 1 TO RESTO

           PERFORM UNTIL RESTO = 0

              DISPLAY 'INSERIRE TOTALE POSTI: '
              ACCEPT TOT-POSTI

              DIVIDE TOT-POSTI BY 3
                 GIVING QUOTA-POSTI
                 REMAINDER RESTO

              IF RESTO NOT = 0
                 DISPLAY MSG-ERRORE
              END-IF

           END-PERFORM

           MOVE QUOTA-POSTI TO DISP-FINESTRINO
           MOVE QUOTA-POSTI TO DISP-CENTRO
           MOVE QUOTA-POSTI TO DISP-CORRIDOIO.

      *=======================================
      * LETTURA FILE
      *=======================================

       LEGGI-PRENOTA.

           READ FILE-PRENOTA
               AT END
                  MOVE 'S' TO FINE-FILE
           END-READ.

      *=======================================
      * SCRITTURA RECORD NON EVASO
      *=======================================
        SCRIVI-NON-EVASA.

           MOVE SPACES     TO RIGA-DETTAGLIO

           MOVE PR-CODAG   TO D-CODAG
           MOVE PR-NPOSTI  TO D-NPOSTI
           MOVE PR-DATAP   TO D-DATAP

           WRITE REC-STAMPA FROM RIGA-DETTAGLIO

           DISPLAY
              'PRENOTAZIONE NON EVASA -> '
              D-CODAG SPACE
              D-NPOSTI SPACE
              D-DATAP.
 
