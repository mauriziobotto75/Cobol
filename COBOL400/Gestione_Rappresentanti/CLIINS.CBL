        IDENTIFICATION DIVISION.
       PROGRAM-ID. CLIINS.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       01 HV-CODCLI      PIC X(5).
       01 HV-RAGSOC      PIC X(40).
       01 HV-INDIRIZZO   PIC X(50).
       01 HV-CITTA       PIC X(30).
       01 HV-CAP         PIC X(5).
       01 HV-PIVA        PIC X(11).
       01 HV-RAPCLI      PIC X(5).

       PROCEDURE DIVISION.

       MAIN.

           DISPLAY 'CODICE CLIENTE'
           ACCEPT HV-CODCLI

           DISPLAY 'RAGIONE SOCIALE'
           ACCEPT HV-RAGSOC

           DISPLAY 'INDIRIZZO'
           ACCEPT HV-INDIRIZZO

           DISPLAY 'CITTA'
           ACCEPT HV-CITTA

           DISPLAY 'CAP'
           ACCEPT HV-CAP

           DISPLAY 'PARTITA IVA'
           ACCEPT HV-PIVA

           DISPLAY 'CODICE RAPPRESENTANTE'
           ACCEPT HV-RAPCLI

           EXEC SQL

              INSERT INTO CLIENTI
              (
                 CODCLI,
                 RAGSOC,
                 INDIRIZZO,
                 CITTA,
                 CAP,
                 PIVA,
                 RAPCLI
              )
              VALUES
              (
                 :HV-CODCLI,
                 :HV-RAGSOC,
                 :HV-INDIRIZZO,
                 :HV-CITTA,
                 :HV-CAP,
                 :HV-PIVA,
                 :HV-RAPCLI
              )

           END-EXEC

           EVALUATE SQLCODE

              WHEN 0

                   EXEC SQL
                        COMMIT
                   END-EXEC

                   DISPLAY
                   'CLIENTE INSERITO'

              WHEN -803

                   DISPLAY
                   'CODICE CLIENTE ESISTENTE'

              WHEN OTHER

                   EXEC SQL
                       ROLLBACK
                   END-EXEC

                   DISPLAY
                   'ERRORE SQLCODE '
                   SQLCODE

           END-EVALUATE

           GOBACK.
