       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRENOTA.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT FILE-PRENOTAZIONI ASSIGN TO PRENOTA
               ORGANIZATION IS SEQUENTIAL.

           SELECT FILE-LISTA ASSIGN TO LISTA
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  FILE-PRENOTAZIONI.

       01  REC-PRENOTA.
           05 PR-TIPO          PIC X(1).
           05 PR-NPOSTI        PIC 9(4).
           05 PR-CODAG         PIC X(4).
           05 PR-DATAP         PIC 9(8).

       FD  FILE-LISTA.

       01  REC-LISTA.
           05 LS-CODAG         PIC X(4).
           05 LS-NPOSTI        PIC ZZZ9.
           05 LS-DATAP         PIC 9(8).

       WORKING-STORAGE SECTION.

       77  FINE-FILE           PIC X VALUE 'N'.

       77  TOT-POSTI           PIC 9(5).
       77  RESTO               PIC 9.

       77  DISP-FINESTRINO     PIC 9(5).
       77  DISP-CENTRO         PIC 9(5).
       77  DISP-CORRIDOIO      PIC 9(5).

       PROCEDURE DIVISION.

       MAIN.

      * Inserimento totale posti

           PERFORM UNTIL RESTO = 0

               DISPLAY 'TOTALE POSTI ? '
               ACCEPT TOT-POSTI

               DIVIDE TOT-POSTI BY 3
                   GIVING DISP-FINESTRINO
                   REMAINDER RESTO

               IF RESTO NOT = 0
                  DISPLAY 'IL NUMERO DEVE ESSERE MULTIPLO DI 3'
               END-IF

           END-PERFORM.

      * Inizializzazione disponibilita'

           MOVE DISP-FINESTRINO TO DISP-CENTRO
           MOVE DISP-FINESTRINO TO DISP-CORRIDOIO

           OPEN INPUT  FILE-PRENOTAZIONI
                OUTPUT FILE-LISTA

           PERFORM LETTURA

           PERFORM UNTIL FINE-FILE = 'S'

               EVALUATE PR-TIPO

                   WHEN 'P'

                       IF PR-NPOSTI <= DISP-FINESTRINO
                          SUBTRACT PR-NPOSTI
                             FROM DISP-FINESTRINO
                       ELSE
                          PERFORM SCRIVI-LISTA
                       END-IF

                   WHEN 'C'

                       IF PR-NPOSTI <= DISP-CORRIDOIO
                          SUBTRACT PR-NPOSTI
                             FROM DISP-CORRIDOIO
                       ELSE
                          PERFORM SCRIVI-LISTA
                       END-IF

                   WHEN 'N'

                       IF PR-NPOSTI <= DISP-CENTRO
                          SUBTRACT PR-NPOSTI
                             FROM DISP-CENTRO
                       ELSE
                          PERFORM SCRIVI-LISTA
                       END-IF

               END-EVALUATE

               PERFORM LETTURA

           END-PERFORM

           CLOSE FILE-PRENOTAZIONI
                 FILE-LISTA

           STOP RUN.

       LETTURA.

           READ FILE-PRENOTAZIONI
               AT END
                  MOVE 'S' TO FINE-FILE
           END-READ.

       SCRIVI-LISTA.

           MOVE PR-CODAG  TO LS-CODAG
           MOVE PR-NPOSTI TO LS-NPOSTI
           MOVE PR-DATAP  TO LS-DATAP

           WRITE REC-LISTA.
