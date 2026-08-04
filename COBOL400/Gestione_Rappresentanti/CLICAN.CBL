       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLICAN.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       01 HV-CODCLI      PIC X(5).
       01 HV-RAGSOC      PIC X(40).
       01 HV-CITTA       PIC X(30).

       01 WS-CONFERMA    PIC X.

       PROCEDURE DIVISION.

       MAIN.

           DISPLAY 'CODICE CLIENTE DA CANCELLARE : '
           ACCEPT HV-CODCLI

           EXEC SQL

              SELECT
                     RAGSOC,
                     CITTA
              INTO
                     :HV-RAGSOC,
                     :HV-CITTA
              FROM CLIENTI
              WHERE CODCLI = :HV-CODCLI

           END-EXEC

           IF SQLCODE = 100

              DISPLAY 'CLIENTE NON TROVATO'
              GOBACK

           END-IF

           DISPLAY '--------------------------------'
           DISPLAY 'CLIENTE TROVATO'
           DISPLAY 'CODICE : ' HV-CODCLI
           DISPLAY 'NOME   : ' HV-RAGSOC
           DISPLAY 'CITTA  : ' HV-CITTA
           DISPLAY '--------------------------------'

           DISPLAY 'CONFERMA CANCELLAZIONE (S/N) ?'
           ACCEPT WS-CONFERMA

           IF WS-CONFERMA = 'S'

              EXEC SQL

                  DELETE
                  FROM CLIENTI
                  WHERE CODCLI = :HV-CODCLI

              END-EXEC

              IF SQLCODE = 0

                 EXEC SQL
                      COMMIT
                 END-EXEC

                 DISPLAY 'CLIENTE ELIMINATO'

              ELSE

                 EXEC SQL
                      ROLLBACK
                 END-EXEC

                 DISPLAY 'ERRORE IN CANCELLAZIONE'

              END-IF

           ELSE

              DISPLAY 'CANCELLAZIONE ANNULLATA'

           END-IF

           GOBACK.
