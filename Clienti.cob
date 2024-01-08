       IDENTIFICATION DIVISION.

       PROGRAM-ID.CLIENTI.

       AUTHOR. BOTTO.

       DATE-WRITTEN. 24-08-2021.

       REMARKS.

           GLI ESTRATTI NON VENGONO INVIATI PER CONTI INATTIVI.
           UN ALTRO PROGRAMMA ELABORA I CONTI CON SALDO NEGATIVO.
           VENGONO STAMPATE A RIGHE PER ESTRATTO.

       ENVIRONMENT  DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER.
                       PC-HP.
       OBJECT-COMPUTER.
                       PC-HP.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILE-CLIENTI   ASSIGN TO DISK
                            ACCESS IS SEQUENTIAL
                            ORGANIZATION IS SEQUENTIAL.

           SELECT FILE-PROSPETTO ASSIGN TO PRINTER.

       DATA  DIVISION.

       FILE  SECTION.

       FD  FILE-CLIENTI
           BLOCK CONTAINS 5 RECORDS
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS RECORD-CLIENTE.

       01  RECORD-CLIENTE.
           02   IDENTIFICAZIONE-CONTO.
                03 TIPO-CONTO                                         PICTURE X.
                03 NUMERO-CONTO-CLIENTE.
                   04 NUMERO-MAGAZZINO                                PICTURE 999.
                   04 NUMERO-PROGRESSIVO                              PICTURE 9(4).
                03 CICLO-FATTURAZIONE                                 PICTURE 99.
                03 NOME-CLIENTE                                       PICTURE X(22).
                03 INDIRIZZO-CLIENTE                                  PICTURE X(22).
                03 CITTA-CLIENTE                                      PICTURE X(22).
           02   FID0.
                03 CODICE-FRAZIONE                                    PICTURE X.
                03 LIMITE-ACQUISTO                                    PICTURE 9(4).
           02   CRONISTORIA.
                03 ANNO-APERTURA                                      PICTURE 99.
                03 ULTIMO-ANNO-ATTIVO                                 PICTURE 99.
                03 SALDO-MASSIMO                                      PICTURE 9(6)v99.
           02   ANNO_PRECEDENTE.
                03 NUMERO-MESI-ATTIVI                                 PICTURE 99.
                03 NUMERO-MESI-OLTRE-90                               PICTURE 99.
                03 TOTALE-ACQUISTI-AP                                 PICTURE 9(7)v99.
                03 TOTALE-RESI-AP                                     PICTURE 9(7)v99.
           02   ANNO-IN-CORSO.
                03 NUMERO-MESI-ATTIVI                                 PICTURE 99.
                03 NUMERO-MESI-OLTRE90                                PICTURE 99.
                03 TOTALE-ACQUISTI-AC                                 PICTURE 9(7)v99.
                03 TOTALE-RESI-AC                                     PICTURE 9(7)v99.
           02   MESE-PRECEDENTE.
                03 NUMERO-OPERAZIONE-MP                               PICTURE 99.
                03 SALDO-A-RIPORTARE                                  PICTURE 9(6)v99.
           02   MESE-IN-CORSO.
                03 DATA-FATTURAZIONE-MC                               PICTURE 9(6).
                03 NUMERO-OPERAZIONE-MC                               PICTURE 99.
                03 SALDO-CORRENTE                                     PICTURE 9(6)v99.
                03   ACQUISTI.
                     04 NUMERO-ACQUISTI                               PICTURE 99.
                     04 IMPORTO-ACQUISTI                              PICTURE 9(6)v99.
                03   PAGAMENTI.
                     04 NUMERO-PAGAMENTI                              PICTURE 99.
                     04 IMPORTO-PAGAMENTI                             PICTURE 9(6)v99.
                03   CREDITI.
                     04 NUMERO-CREDITI                                PICTURE 99.
                     04 IMPORTO-CREDITI                               PICTURE 9(6)v99.
                03   RESI.
                     04 NUMERO-RESI                                   PICTURE 99.
                     04 IMPORTO-RESI                                  PICTURE 9(6)v99.
           02   CRONISTORIA_PAGAMENTI.
                03   SALDO-SCONTI.
                     04  30-GIORNI                                    PICTURE 9(6)v99.
                     04  60-GIORNI                                    PICTURE 9(6)v99.
                     04  90-GIORNI                                    PICTURE 9(6)v99.
                     04  120-GIORNI                                   PICTURE 9(6)v99.
                03   ULTIMO-PAGAMENTO.
                     04  DATA-PAG                                     PICTURE 9(6).
                     04  IMPORTO                                      PICTURE 9(6)v99.
                03   CODICE-SOLLECITO                                 PICTURE X.


       FD  FILE-PROSPETTO
           RECORD MODE F
           LABEL RECORD IS OMITTED
           DATA RECORDS ARE RIGA-1, RIGA-2, RIGA3, RIGA-4.

       01  RIGA-1.
           02 FILLER                                                  PICTURE XX.
           02 VECCHIO-SALDO                                           PICTURE $$$$.$$$.99.
           02 FILLER                                                  PICTURE X(5).
           02 ACQUISTI-1                                              PICTURE $$$$.$$$.99.
           02 FILLER                                                  PICTURE Xx.
           02 PAGAMENTI-1                                             PICTURE $$$$.$$$.99.
           02 FILLER                                                  PICTURE XX.
           02 CREDITI-1                                               PICTURE $$$$.$$$.99.
           02 FILLER                                                  PICTURE XX.
           02 NUMERO-CONTO-1                                          PICTURE 9(8).
           02 FILLER                                                  PICTURE X(5).
           02 DATA-FATTURAZIONE-1                                     PICTURE 9(8).
           02 FILLER                                                  PICTURE X(50).

       01  RIGA-2.
           02 FILLER                                                  PICTURE X(18).
           02 IMPORTO-DOVUTO                                          PICTURE $$$$.$$$.99.
           02 FILLER                                                  PICTURE X(11).
           02 NOME                                                    PICTURE X(22).
           02 FILLER                               02                 PICTURE X(71).

       01  RIGA-3.
           02 FILLER                                                  PICTURE X(40).
           02 INDIRIZZO                                               PICTURE X(22).
           02 FILLER                                                  PICTURE X(71).

       01  RIGA-4.
           02 FILLER                                                  PICTURE X(40).
           02 CITTA                                               PICTURE X(22).
           02 FILLER                                                  PICTURE X(71).

       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
       INIZIO-ELABORAZIONE.
           OPEN INPUT FILE-CLIENTI.
           OPEN OUTPUT FILE-PROSPETTO.
       LETTURA-CONTROLLO.
           READ FILE-CLIENTI AT END GO TO FINE-ELABORAZIONE.
           IF NUMERO-OPERAZIONE-MC IS EQUAL ZERO
                                    OR SALDO-CORRENTE IS NEGATIVE
                                    GO TO LETTURA-E-CONTROLLO.
       RIGA-STAMPA-1.
           MOVE SPACES TO RIGA-1.
           MOVE SALDO-A-RIPORTARE TO VECCHIO-SALDO.
           MOVE IMPORTO-ACQUISTI-MC TO ACQUISTI-1.
           MOVE IMPORTO-PAGAMENTI-MC TO PAGAMENTI-1.
           MOVE IMPORTO-CREDITI-MC TO CREDITI-1.
           MOVE NUMERO-CONTO-CLIENTE TO NUMERO-CONTO-1.
           MOVE DATA-FATTURAZIONE-MC TO DATA-FATTURAZIONE-1.
           WRITE RIGA-1 AFTER PAGE.

       RIGA-STAMPA-2.
           MOVE SPACES TO RIGA-2.
           MOVE SALDO-CORRENTE TO IMPORTO-DOVUTO.
           MOVE NOME-CLIENTE TO NOME.
           WRITE RIGA-2 AFTER 5 LINES.

       RIGA-STAMPA-3.
           MOVE SPACES TO RIGA-3.
           MOVE INDIRIZZO-CLIENTE TO INDIRIZZO.
           WRITE RIGA-3 AFTER 1 LINES.

       RIGA-STAMPA-4.
           MOVE CITTA-CLEINTE TO CITTA.

           WRITE RIGA-4 AFTER 1 LINES.

       NUOVO-RECORD.
           GO TO LETTURA-E-CONTROLLO.

       FINE-ELABORAZIONE.
           CLOSE FILE-CLIENTI.
           CLOSE FILE-PROSPETTO.
       STOP RUN.
