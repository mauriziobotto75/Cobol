       IDENTIFICATION DIVISION.
       PROGRAM-ID. CNT001.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT CONTI
              ASSIGN TO DISK
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS CNT-NUMERO.

       DATA DIVISION.

       FILE SECTION.

       FD CONTI.

       01 REC-CONTO.
          05 CNT-NUMERO      PIC X(12).
          05 CLI-CODICE      PIC X(6).
          05 CNT-ABI         PIC 9(5).
          05 CNT-CAB         PIC 9(5).
          05 CNT-DATAAPR     PIC 9(8).
          05 CNT-SALDO       PIC S9(11)V99.
          05 CNT-FIDO        PIC S9(11)V99.
          05 CNT-STATO       PIC X.

       WORKING-STORAGE SECTION.

       01 FINE-PROG          PIC X VALUE "N".
          88 FINE            VALUE "S".

       01 SCELTA             PIC 9.

       SCREEN SECTION.

       01 MENU-CONTI.

          05 BLANK SCREEN.

          05 LINE 02 COLUMN 30
             VALUE "GESTIONE CONTI".

          05 LINE 05 COLUMN 10
             VALUE "1 APERTURA CONTO".

          05 LINE 06 COLUMN 10
             VALUE "2 MODIFICA CONTO".

          05 LINE 07 COLUMN 10
             VALUE "3 CHIUSURA CONTO".

          05 LINE 08 COLUMN 10
             VALUE "9 FINE".

          05 LINE 12 COLUMN 10
             VALUE "SCELTA : ".

          05 LINE 12 COLUMN 20
             PIC 9 USING SCELTA.

       PROCEDURE DIVISION.

       MAIN.

           OPEN I-O CONTI

           PERFORM UNTIL FINE

              DISPLAY MENU-CONTI

              ACCEPT MENU-CONTI

              EVALUATE SCELTA

                 WHEN 1
                      PERFORM APRI-CONTO

                 WHEN 2
                      PERFORM MODIFICA-CONTO

                 WHEN 3
                      PERFORM CHIUDI-CONTO

                 WHEN 9
                      SET FINE TO TRUE

              END-EVALUATE

           END-PERFORM

           CLOSE CONTI

           STOP RUN.

       APRI-CONTO.

           DISPLAY "NUMERO CONTO : "
           ACCEPT CNT-NUMERO

           DISPLAY "CODICE CLIENTE : "
           ACCEPT CLI-CODICE

           DISPLAY "ABI : "
           ACCEPT CNT-ABI

           DISPLAY "CAB : "
           ACCEPT CNT-CAB

           DISPLAY "DATA APERTURA : "
           ACCEPT CNT-DATAAPR

           MOVE 0 TO CNT-SALDO
           MOVE 0 TO CNT-FIDO

           MOVE "A" TO CNT-STATO

           WRITE REC-CONTO
              INVALID KEY
                 DISPLAY "CONTO GIA ESISTENTE"
           END-WRITE.

       MODIFICA-CONTO.

           DISPLAY "NUMERO CONTO : "
           ACCEPT CNT-NUMERO

           READ CONTI
              INVALID KEY
                 DISPLAY "CONTO INESISTENTE"
              NOT INVALID KEY

                 DISPLAY "FIDO : "
                 ACCEPT CNT-FIDO

                 REWRITE REC-CONTO

                    INVALID KEY
                       DISPLAY "ERRORE RISCRITTURA"

                 END-REWRITE

           END-READ.

       CHIUDI-CONTO.

           DISPLAY "NUMERO CONTO : "
           ACCEPT CNT-NUMERO

           READ CONTI
              INVALID KEY
                 DISPLAY "CONTO INESISTENTE"

              NOT INVALID KEY

                 MOVE "C"
                    TO CNT-STATO

                 REWRITE REC-CONTO

                    INVALID KEY
                       DISPLAY "ERRORE RISCRITTURA"

                 END-REWRITE

           END-READ.
