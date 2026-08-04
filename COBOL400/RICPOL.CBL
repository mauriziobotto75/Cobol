       IDENTIFICATION DIVISION.
       PROGRAM-ID. RICPOL.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT DSPFILE
                  ASSIGN TO WORKSTN.

       DATA DIVISION.

       FILE SECTION.

       FD  DSPFILE.

       01  RICPOLR.
           05 D-CODPOL          PIC X(10).
           05 D-NOMCLI          PIC X(40).
           05 D-INDIRIZ         PIC X(60).
           05 D-IMPORTO         PIC 9(7)V99.
           05 D-SCADENZA        PIC X(10).
           05 D-SIGAUTO         PIC X(10).
           05 D-NCAVALLI        PIC 9(3).
           05 D-STAPAG          PIC X(1).
           05 D-MSG             PIC X(50).

       WORKING-STORAGE SECTION.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       01 WS-FINE              PIC X VALUE 'N'.

       01 HV-CODPOL            PIC X(10).

       01 HV-NOMCLI            PIC X(40).
       01 HV-INDIRIZ           PIC X(60).
       01 HV-IMPORTO           PIC S9(7)V99 COMP-3.
       01 HV-SCADENZA          PIC X(10).
       01 HV-SIGAUTO           PIC X(10).
       01 HV-NCAVALLI          PIC S9(4) COMP.
       01 HV-STAPAG            PIC X(1).

       PROCEDURE DIVISION.

       MAIN.

           PERFORM UNTIL WS-FINE = 'S'

              PERFORM AZZERA-CAMPI

              WRITE RICPOLR
              READ DSPFILE

              IF IN03
                 MOVE 'S' TO WS-FINE
              ELSE
                 PERFORM RICERCA-POLIZZA
                 WRITE RICPOLR
                 READ DSPFILE
              END-IF

           END-PERFORM

           GOBACK.

       AZZERA-CAMPI.

           MOVE SPACES TO D-NOMCLI
           MOVE SPACES TO D-INDIRIZ
           MOVE SPACES TO D-SCADENZA
           MOVE SPACES TO D-SIGAUTO
           MOVE SPACES TO D-STAPAG
           MOVE SPACES TO D-MSG

           MOVE ZEROES TO D-IMPORTO
           MOVE ZEROES TO D-NCAVALLI.

       RICERCA-POLIZZA.

           MOVE D-CODPOL
             TO HV-CODPOL

           EXEC SQL

              SELECT
                     NOME_CLIENTE,
                     INDIRIZZO,
                     IMPORTO,
                     CHAR(SCADENZA),
                     SIGLA_AUTO,
                     NUM_CAVALLI,
                     STATO_PAGAMENTO

                INTO
                     :HV-NOMCLI,
                     :HV-INDIRIZ,
                     :HV-IMPORTO,
                     :HV-SCADENZA,
                     :HV-SIGAUTO,
                     :HV-NCAVALLI,
                     :HV-STAPAG

                FROM POLIZZE

               WHERE COD_POLIZZA =
                     :HV-CODPOL

           END-EXEC

           PERFORM CONTROLLA-SQLCODE.

       CONTROLLA-SQLCODE.

           EVALUATE SQLCODE

              WHEN 0

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

                   MOVE HV-STAPAG
                     TO D-STAPAG

                   MOVE
                   'POLIZZA TROVATA'
                     TO D-MSG

              WHEN 100

                   MOVE
                   'POLIZZA NON PRESENTE'
                     TO D-MSG

              WHEN OTHER

                   PERFORM ERRORE-SQL

           END-EVALUATE.

       ERRORE-SQL.

           MOVE SPACES TO D-MSG

           STRING
              'ERRORE DB2 SQLCODE='
              DELIMITED BY SIZE
              SQLCODE
              DELIMITED BY SIZE
              INTO D-MSG
           END-STRING.
