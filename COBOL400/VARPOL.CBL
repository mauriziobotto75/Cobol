        IDENTIFICATION DIVISION.
       PROGRAM-ID. VARPOL.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT DSPFILE
                  ASSIGN TO WORKSTN.

       DATA DIVISION.

       FILE SECTION.

       FD DSPFILE.

       01 VARPOLR.
          05 D-CODPOL        PIC X(10).
          05 D-NOMCLI        PIC X(40).
          05 D-INDIRIZ       PIC X(60).
          05 D-IMPORTO       PIC 9(7)V99.
          05 D-SCADENZA      PIC X(10).
          05 D-SIGAUTO       PIC X(10).
          05 D-NCAVALLI      PIC 9(3).
          05 D-CONFERMA      PIC X.
          05 D-MSG           PIC X(50).

       WORKING-STORAGE SECTION.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       01 WS-FINE            PIC X VALUE 'N'.

       01 HV-CODPOL          PIC X(10).
       01 HV-NOMCLI          PIC X(40).
       01 HV-INDIRIZ         PIC X(60).
       01 HV-IMPORTO         PIC S9(7)V99 COMP-3.
       01 HV-SCADENZA        PIC X(10).
       01 HV-SIGAUTO         PIC X(10).
       01 HV-NCAVALLI        PIC S9(4) COMP.

       PROCEDURE DIVISION.

       MAIN.

           PERFORM UNTIL WS-FINE = 'S'

               PERFORM 1000-INIZIALIZZA

               WRITE VARPOLR
               READ DSPFILE

               IF IN03
                  MOVE 'S'
                    TO WS-FINE
               ELSE
                  PERFORM 2000-RICERCA-POLIZZA
               END-IF

           END-PERFORM

           GOBACK.
      main-ex.
           exit.
      1000-INIZIALIZZA.

           MOVE SPACES
             TO D-MSG

           MOVE SPACES
             TO D-CONFERMA.

           MOVE ZEROES
             TO D-IMPORTO.

           MOVE ZEROES
             TO D-NCAVALLI.
       1000-inizializza-ex.

      2000-RICERCA-POLIZZA.

           MOVE D-CODPOL
             TO HV-CODPOL

           EXEC SQL

              SELECT
                     NOME_CLIENTE,
                     INDIRIZZO,
                     IMPORTO,
                     CHAR(SCADENZA),
                     SIGLA_AUTO,
                     NUM_CAVALLI

                INTO
                     :HV-NOMCLI,
                     :HV-INDIRIZ,
                     :HV-IMPORTO,
                     :HV-SCADENZA,
                     :HV-SIGAUTO,
                     :HV-NCAVALLI

                FROM POLIZZE

               WHERE COD_POLIZZA =
                     :HV-CODPOL

           END-EXEC

           EVALUATE SQLCODE

              WHEN 0

                   PERFORM 3000-CARICA-DATI

              WHEN 100

                   MOVE
                   'POLIZZA NON TROVATA'
                   TO D-MSG

              WHEN OTHER

                   PERFORM 9000-ERRORE-SQL

           END-EVALUATE.
       2000-ricerca-polizza-ex.
           exit.

       3000-CARICA-DATI.

           MOVE HV-NOMCLI
             TO D-NOMCLI

           MOVE HV-INDIRIZ
             TO D-INDIRIZ

           MOVE HV-IMPORTO
             TO D-IMPORTO

           MOVE HV-SCADENZA
             TO D-SCADENZA

           MOVE HV-SIGAUTO
             TO D-SIGAUTO

           MOVE HV-NCAVALLI
             TO D-NCAVALLI

           MOVE
           'MODIFICARE I DATI E CONFERMARE'
           TO D-MSG

           WRITE VARPOLR
           READ DSPFILE

           PERFORM 4000-CONFERMA.
      3000-carica-dati-ex.
          exit.

      4000-CONFERMA.

           IF D-CONFERMA = 'S'
           OR D-CONFERMA = 's'

              PERFORM 5000-UPDATE

           ELSE

              MOVE
              'AGGIORNAMENTO ANNULLATO'
              TO D-MSG

           END-IF.
       4000-CONFERMA-EX.
          EXIT.

      5000-UPDATE.

           EXEC SQL

              UPDATE POLIZZE

                 SET
                     NOME_CLIENTE = :D-NOMCLI,
                     INDIRIZZO    = :D-INDIRIZ,
                     IMPORTO      = :D-IMPORTO,
                     SCADENZA     = DATE(:D-SCADENZA),
                     SIGLA_AUTO   = :D-SIGAUTO,
                     NUM_CAVALLI  = :D-NCAVALLI

               WHERE COD_POLIZZA =
                     :HV-CODPOL

           END-EXEC

           PERFORM 6000-CHECK-UPDATE.
       5000-UPDATE-EX.
           EXIT.
       6000-CHECK-UPDATE.

           EVALUATE SQLCODE

              WHEN 0

                   EXEC SQL
                        COMMIT
                   END-EXEC

                   MOVE
                   'POLIZZA AGGIORNATA'
                   TO D-MSG

              WHEN 100

                   MOVE
                   'NESSUN RECORD AGGIORNATO'
                   TO D-MSG

              WHEN OTHER

                   EXEC SQL
                        ROLLBACK
                   END-EXEC

                   PERFORM 9000-ERRORE-SQL

           END-EVALUATE.
      6000-CHECK-UPDATE-EX.
         EXIT.
       9000-ERRORE-SQL.

           MOVE SPACES
             TO D-MSG

           STRING
              'SQLCODE='
              DELIMITED BY SIZE
              SQLCODE
              DELIMITED BY SIZE
              INTO D-MSG
           END-STRING.
      9000-ERRORE-SQL-EX.

              
