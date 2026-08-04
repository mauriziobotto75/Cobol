       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLIVAR.

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

       01 WS-CONFERMA    PIC X.

       PROCEDURE DIVISION.

       MAIN.

           ACCEPT HV-CODCLI

           EXEC SQL

             SELECT
                    RAGSOC,
                    INDIRIZZO,
                    CITTA,
                    CAP,
                    PIVA,
                    RAPCLI

               INTO
                    :HV-RAGSOC,
                    :HV-INDIRIZZO,
                    :HV-CITTA,
                    :HV-CAP,
                    :HV-PIVA,
                    :HV-RAPCLI

               FROM CLIENTI

              WHERE CODCLI = :HV-CODCLI

           END-EXEC

           IF SQLCODE = 100

              DISPLAY 'CLIENTE NON TROVATO'
              GOBACK

           END-IF

           DISPLAY 'RAGIONE SOCIALE = '
                   HV-RAGSOC

           ACCEPT HV-RAGSOC
           ACCEPT HV-INDIRIZZO
           ACCEPT HV-CITTA

           DISPLAY 'CONFERMI (S/N)?'
           ACCEPT WS-CONFERMA

           IF WS-CONFERMA = 'S'

              EXEC SQL

                 UPDATE CLIENTI

                    SET
                       RAGSOC    = :HV-RAGSOC,
                       INDIRIZZO = :HV-INDIRIZZO,
                       CITTA     = :HV-CITTA,
                       CAP       = :HV-CAP,
                       PIVA      = :HV-PIVA,
                       RAPCLI    = :HV-RAPCLI

                  WHERE CODCLI =
                        :HV-CODCLI

              END-EXEC

              IF SQLCODE = 0

                 EXEC SQL
                      COMMIT
                 END-EXEC

                 DISPLAY
                 'CLIENTE AGGIORNATO'

              ELSE

                 EXEC SQL
                      ROLLBACK
                 END-EXEC

                 DISPLAY
                 'ERRORE UPDATE'

              END-IF

           END-IF

           GOBACK.
