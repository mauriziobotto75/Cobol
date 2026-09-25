       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOV001.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT CONTI
                  ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CNT-NUMERO.

           SELECT MOVIMENTI
                  ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS MOV-KEY.

       DATA DIVISION.

       FILE SECTION.

       FD CONTI.

       01 REC-CONTO.
          05 CNT-NUMERO      PIC X(12).
          05 CLI-CODICE      PIC X(6).
          05 FIL-CODICE      PIC X(4).
          05 CNT-NOME        PIC X(40).
          05 CNT-INDIRIZZO   PIC X(40).
          05 CNT-SALDO       PIC S9(11)V99.
          05 CNT-FIDO        PIC S9(11)V99.
          05 CNT-STATO       PIC X.

       FD MOVIMENTI.

       01 REC-MOVIMENTO.
          05 MOV-KEY.
             10 MOV-NUMERO       PIC 9(8).
          05 CNT-NUMERO-MOV      PIC X(12).
          05 MOV-DATA            PIC 9(8).
          05 MOV-TIPO            PIC XX.
          05 MOV-CAUSALE         PIC X(40).
          05 MOV-IMPORTO         PIC S9(11)V99.
          05 MOV-VALUTA          PIC 9(8).

       WORKING-STORAGE SECTION.

       01 REC-CONTO-BKP.
          05 BKP-CNT-NUMERO      PIC X(12).
          05 BKP-CLI-CODICE      PIC X(6).
          05 BKP-FIL-CODICE      PIC X(4).
          05 BKP-CNT-NOME        PIC X(40).
          05 BKP-CNT-INDIRIZZO   PIC X(40).
          05 BKP-CNT-SALDO       PIC S9(11)V99.
          05 BKP-CNT-FIDO        PIC S9(11)V99.
          05 BKP-CNT-STATO       PIC X.

       01 DISPONIBILE            PIC S9(11)V99.
       01 ESITO                  PIC X VALUE "S".
       01 RISPOSTA               PIC X.

       PROCEDURE DIVISION.

       MAIN.

           OPEN I-O CONTI
           OPEN I-O MOVIMENTI

           PERFORM INSERISCI-MOVIMENTO

           CLOSE CONTI
           CLOSE MOVIMENTI

           STOP RUN.

       INSERISCI-MOVIMENTO.

           DISPLAY "NUMERO CONTO : "
           ACCEPT CNT-NUMERO-MOV

           MOVE CNT-NUMERO-MOV
             TO CNT-NUMERO

           READ CONTI
              INVALID KEY
                 DISPLAY "CONTO INESISTENTE"
                 EXIT PARAGRAPH
           END-READ

           IF CNT-STATO NOT = "A"

              DISPLAY "CONTO NON OPERATIVO"

              EXIT PARAGRAPH

           END-IF

           MOVE REC-CONTO
             TO REC-CONTO-BKP

           DISPLAY "TIPO MOVIMENTO (VE/PR/BO/PA) : "
           ACCEPT MOV-TIPO

           DISPLAY "IMPORTO : "
           ACCEPT MOV-IMPORTO

           DISPLAY "CAUSALE : "
           ACCEPT MOV-CAUSALE

           DISPLAY "DATA OPERAZIONE (AAAAMMGG): "
           ACCEPT MOV-DATA

           DISPLAY "DATA VALUTA (AAAAMMGG): "
           ACCEPT MOV-VALUTA

           PERFORM CONTROLLA-DISPONIBILITA

           IF ESITO = "S"

              DISPLAY
                 "CONFERMA OPERAZIONE (S/N)? "

              ACCEPT RISPOSTA

              IF RISPOSTA = "S"
                 OR RISPOSTA = "s"

                 WRITE REC-MOVIMENTO
                    INVALID KEY
                       DISPLAY "ERRORE SCRITTURA MOVIMENTO"
                       EXIT PARAGRAPH
                 END-WRITE

                 PERFORM AGGIORNA-SALDO

              END-IF

           END-IF.

       CONTROLLA-DISPONIBILITA.

           MOVE "S" TO ESITO

           EVALUATE MOV-TIPO

              WHEN "PR"
                   PERFORM VERIFICA-FONDI

              WHEN "BO"
                   PERFORM VERIFICA-FONDI

              WHEN "PA"
                   PERFORM VERIFICA-FONDI

              WHEN OTHER
                   CONTINUE

           END-EVALUATE.

       VERIFICA-FONDI.

           COMPUTE DISPONIBILE =
                   CNT-SALDO + CNT-FIDO

           IF MOV-IMPORTO > DISPONIBILE

              DISPLAY
                 "FONDI INSUFFICIENTI"

              MOVE "N"
                TO ESITO

           END-IF.

       AGGIORNA-SALDO.

           EVALUATE MOV-TIPO

              WHEN "VE"
                   ADD MOV-IMPORTO
                     TO CNT-SALDO

              WHEN "AC"
                   ADD MOV-IMPORTO
                     TO CNT-SALDO

              WHEN "PR"
                   SUBTRACT MOV-IMPORTO
                     FROM CNT-SALDO

              WHEN "BO"
                   SUBTRACT MOV-IMPORTO
                     FROM CNT-SALDO

              WHEN "PA"
                   SUBTRACT MOV-IMPORTO
                     FROM CNT-SALDO

              WHEN OTHER

                   DISPLAY
                      "TIPO MOVIMENTO ERRATO"

                   EXIT PARAGRAPH

           END-EVALUATE

           REWRITE REC-CONTO

               INVALID KEY

                  MOVE REC-CONTO-BKP
                     TO REC-CONTO

                  DISPLAY
                    "ROLLBACK ESEGUITO"

               NOT INVALID KEY

                  DISPLAY
                    "SALDO AGGIORNATO"

           END-REWRITE.
