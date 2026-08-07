        IDENTIFICATION DIVISION.
       PROGRAM-ID. PRENOTA.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT FILE-PRENOTA ASSIGN TO PRENOTA
               ORGANIZATION IS SEQUENTIAL.

           SELECT FILE-LISTA ASSIGN TO LISTA
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

      *=========================================
      * FILE INPUT PRENOTAZIONI
      *=========================================
       FD FILE-PRENOTA.

       01 REC-PRENOTA.
          05 PR-TIPO          PIC X.
          05 PR-NPOSTI        PIC 9(4).
          05 PR-CODAG         PIC X(4).
          05 PR-DATAP         PIC 9(8).

      *=========================================
      * FILE OUTPUT REPORT
      *=========================================
       FD FILE-LISTA.

       01 REC-STAMPA          PIC X(50).

       WORKING-STORAGE SECTION.

      *=========================================
      * VARIABILI DI CONTROLLO
      *=========================================
       77 FINE-FILE           PIC X VALUE 'N'.

       77 TOT-POSTI           PIC 9(5).
       77 QUOTA-POSTI         PIC 9(5).
       77 RESTO               PIC 9.

       77 DISP-FINESTRINO     PIC 9(5).
       77 DISP-CENTRO         PIC 9(5).
       77 DISP-CORRIDOIO      PIC 9(5).

      *=========================================
      * RECORD DI DETTAGLIO STAMPA
      *=========================================
       01 RIGA-DETT.
          05 D-CODAG          PIC X(4).
          05 FILLER           PIC X(6) VALUE SPACES.
          05 D-NPOSTI         PIC ZZZ9.
          05 FILLER           PIC X(6) VALUE SPACES.
          05 D-DATA           PIC 9(8).

      *=========================================
      * TESTATE REPORT
      *=========================================
       01 TESTATA-1.
          05 FILLER PIC X(50)
             VALUE 'LISTA PRENOTAZIONI NON EVASE'.

       01 TESTATA-2.
          05 FILLER PIC X(50)
             VALUE
             'AGENZIA   POSTI   DATA PRENOTAZIONE'.

       01 TESTATA-3.
          05 FILLER PIC X(50)
             VALUE
             '-------   -----   -----------------'.

       PROCEDURE DIVISION.

      *=========================================
      * MAIN
      *=========================================
       MAIN.

           PERFORM ACQUISISCI-POSTI

           OPEN INPUT  FILE-PRENOTA
                OUTPUT FILE-LISTA

      * Scrittura intestazione report

           WRITE REC-STAMPA FROM TESTATA-1
           WRITE REC-STAMPA FROM SPACES
           WRITE REC-STAMPA FROM TESTATA-2
           WRITE REC-STAMPA FROM TESTATA-3

           PERFORM LEGGI-PRENOTA

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
                    'TIPO PRENOTAZIONE ERRATO : '
                    PR-TIPO

              END-EVALUATE

              PERFORM LEGGI-PRENOTA

           END-PERFORM

           CLOSE FILE-PRENOTA
                 FILE-LISTA

      * Riepilogo finale

           DISPLAY ' '
           DISPLAY 'POSTI RIMASTI DISPONIBILI'
           DISPLAY '-------------------------'
           DISPLAY 'FINESTRINO : ' DISP-FINESTRINO
           DISPLAY 'CENTRO     : ' DISP-CENTRO
           DISPLAY 'CORRIDOIO  : ' DISP-CORRIDOIO

           STOP RUN.

      *=========================================
      * ACQUISIZIONE POSTI TOTALI
      *=========================================
       ACQUISISCI-POSTI.

           MOVE 1 TO RESTO

           PERFORM UNTIL RESTO = 0

              DISPLAY 'INSERIRE NUMERO TOTALE POSTI : '
              ACCEPT TOT-POSTI

              DIVIDE TOT-POSTI BY 3
                 GIVING QUOTA-POSTI
                 REMAINDER RESTO

              IF RESTO NOT = 0
                 DISPLAY
                 'ERRORE: NUMERO NON MULTIPLO DI 3'
              END-IF

           END-PERFORM

           MOVE QUOTA-POSTI TO DISP-FINESTRINO
           MOVE QUOTA-POSTI TO DISP-CENTRO
           MOVE QUOTA-POSTI TO DISP-CORRIDOIO.

      *=========================================
      * LETTURA FILE INPUT
      *=========================================
       LEGGI-PRENOTA.

           READ FILE-PRENOTA
                AT END
                   MOVE 'S' TO FINE-FILE
           END-READ.

      *=========================================
      * SCRITTURA PRENOTAZIONE NON EVASA
      *=========================================
       SCRIVI-NON-EVASA.

           MOVE SPACES     TO RIGA-DETT

           MOVE PR-CODAG  TO D-CODAG
           MOVE PR-NPOSTI TO D-NPOSTI
           MOVE PR-DATAP  TO D-DATA

           WRITE REC-STAMPA FROM RIGA-DETT

           DISPLAY
             'NON EVASA -> '
              D-CODAG SPACE
              D-NPOSTI SPACE
              D-DATA.
 
