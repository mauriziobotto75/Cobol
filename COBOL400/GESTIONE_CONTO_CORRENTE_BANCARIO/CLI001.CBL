       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLI001.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT CLIENTI
               ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CLI-CODICE.
           SELECT CLIENTIFMT 
               ASSIGN TO WORKSTATION.

       DATA DIVISION.

       FILE SECTION.

       FD CLIENTI.

       01 REC-CLIENTE.
          05 CLI-CODICE        PIC X(6).
          05 CLI-NOME          PIC X(30).
          05 CLI-COGNOME       PIC X(30).
          05 CLI-INDIRIZZO     PIC X(20).
          05 CLI-CITTA         PIC X(20).
          05 CLI-TELEFONO      PIC X(15).
          05 CLI-CODFISCALE    PIC X(16).
          05 CLI-RESIDENZA     PIC X(15).

       WORKING-STORAGE SECTION.

       01 FINE-PROG            PIC X VALUE "N".
          88 FINE              VALUE "S".

       01 SCELTA               PIC 9 VALUE 0.

       SCREEN SECTION.

       01 MENU-CLIENTI.
          05 BLANK SCREEN.
          05 LINE 02 COLUMN 30
             VALUE "GESTIONE CLIENTI".

          05 LINE 05 COLUMN 10
             VALUE "1 INSERIMENTO CLIENTE".

          05 LINE 06 COLUMN 10
             VALUE "2 MODIFICA CLIENTE".

          05 LINE 07 COLUMN 10
             VALUE "3 CANCELLA CLIENTE".

          05 LINE 08 COLUMN 10
             VALUE "4 VISUALIZZA CLIENTE".

          05 LINE 10 COLUMN 10
             VALUE "9 FINE".

          05 LINE 12 COLUMN 10
             VALUE "SCELTA : ".

          05 LINE 12 COLUMN 20
             PIC 9 USING SCELTA.

       PROCEDURE DIVISION.

       MAIN.

           OPEN I-O CLIENTI

           PERFORM UNTIL FINE

               MOVE 0 TO SCELTA

               DISPLAY MENU-CLIENTI
               ACCEPT MENU-CLIENTI

               EVALUATE SCELTA

                   WHEN 1
                        PERFORM INSERISCI-CLIENTE

                   WHEN 2
                        PERFORM MODIFICA-CLIENTE

                   WHEN 3
                        PERFORM CANCELLA-CLIENTE

                   WHEN 4
                        PERFORM VISUALIZZA-CLIENTE

                   WHEN 9
                        SET FINE TO TRUE

                   WHEN OTHER
                        DISPLAY "SCELTA NON VALIDA"

               END-EVALUATE

           END-PERFORM

           CLOSE CLIENTI

           STOP RUN.

      *------------------------------------------------*
       INSERISCI-CLIENTE.
      *------------------------------------------------*

           DISPLAY "CODICE CLIENTE : "
           ACCEPT CLI-CODICE

           DISPLAY "NOME : "
           ACCEPT CLI-NOME

           DISPLAY "COGNOME : "
           ACCEPT CLI-COGNOME

           DISPLAY "INDIRIZZO : "
           ACCEPT CLI-INDIRIZZO

           DISPLAY "CITTA : "
           ACCEPT CLI-CITTA

           DISPLAY "TELEFONO : "
           ACCEPT CLI-TELEFONO

           DISPLAY "CODICE FISCALE : "
           ACCEPT CLI-CODFISCALE

           DISPLAY "RESIDENZA : "
           ACCEPT CLI-RESIDENZA

           WRITE REC-CLIENTE
               INVALID KEY
                   DISPLAY "CLIENTE GIA ESISTENTE"
               NOT INVALID KEY
                   DISPLAY "CLIENTE INSERITO"
           END-WRITE.

      *------------------------------------------------*
       MODIFICA-CLIENTE.
      *------------------------------------------------*

           DISPLAY "CODICE CLIENTE : "
           ACCEPT CLI-CODICE

           READ CLIENTI
              INVALID KEY
                 DISPLAY "CLIENTE INESISTENTE"

              NOT INVALID KEY

                 DISPLAY "NOME : "
                 ACCEPT CLI-NOME

                 DISPLAY "COGNOME : "
                 ACCEPT CLI-COGNOME

                 DISPLAY "INDIRIZZO : "
                 ACCEPT CLI-INDIRIZZO

                 DISPLAY "CITTA : "
                 ACCEPT CLI-CITTA

                 DISPLAY "TELEFONO : "
                 ACCEPT CLI-TELEFONO

                 DISPLAY "CODICE FISCALE : "
                 ACCEPT CLI-CODFISCALE

                 DISPLAY "RESIDENZA : "
                 ACCEPT CLI-RESIDENZA

                 REWRITE REC-CLIENTE
                    INVALID KEY
                       DISPLAY "ERRORE MODIFICA"
                    NOT INVALID KEY
                       DISPLAY "CLIENTE MODIFICATO"
                 END-REWRITE

           END-READ.

      *------------------------------------------------*
       CANCELLA-CLIENTE.
      *------------------------------------------------*

           DISPLAY "CODICE CLIENTE : "
           ACCEPT CLI-CODICE

           READ CLIENTI
              INVALID KEY
                 DISPLAY "CLIENTE INESISTENTE"

              NOT INVALID KEY

                 DELETE CLIENTI RECORD

                 DISPLAY "CLIENTE ELIMINATO"
