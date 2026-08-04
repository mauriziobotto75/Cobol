       IDENTIFICATION DIVISION.
       PROGRAM-ID. RPTCLI02.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       01 HV-RAPCLI      PIC X(5).

       01 HV-CODCLI      PIC X(5).
       01 HV-RAGSOC      PIC X(40).
       01 HV-CITTA       PIC X(30).

       01 WS-TOTCLI      PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.

       MAIN.

           DISPLAY
           'CODICE RAPPRESENTANTE : '

           ACCEPT HV-RAPCLI

           EXEC SQL

              DECLARE CCLI CURSOR FOR

                  SELECT
                         CODCLI,
                         RAGSOC,
                         CITTA

                  FROM CLIENTI

                  WHERE RAPCLI = :HV-RAPCLI

                  ORDER BY RAGSOC

           END-EXEC

           EXEC SQL
                OPEN CCLI
           END-EXEC

           DISPLAY
           '==========================================='

           DISPLAY
           'CLIENTI DEL RAPPRESENTANTE '
           HV-RAPCLI

           DISPLAY
           '==========================================='

           PERFORM UNTIL SQLCODE = 100

              EXEC SQL

                   FETCH CCLI

                   INTO
                        :HV-CODCLI,
                        :HV-RAGSOC,
                        :HV-CITTA

              END-EXEC

              IF SQLCODE = 0

                 ADD 1 TO WS-TOTCLI

                 DISPLAY
                 HV-CODCLI SPACE
                 HV-RAGSOC SPACE
                 HV-CITTA

              END-IF

           END-PERFORM

           EXEC SQL
                CLOSE CCLI
           END-EXEC

           DISPLAY
           '-------------------------------------------'

           DISPLAY
           'TOTALE CLIENTI : '
           WS-TOTCLI

           DISPLAY
           '-------------------------------------------'

           GOBACK.
