       IDENTIFICATION DIVISION.
       PROGRAM-ID. RPTRIT.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       01 HV-CODPRE       PIC 9(6).
       01 HV-CODLIB       PIC X(6).
       01 HV-TITOLO       PIC X(60).
       01 HV-CODLET       PIC X(6).
       01 HV-DATAPRE      PIC X(10).

       01 WS-CONTEGGIO    PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.

           EXEC SQL

              DECLARE CRIT CURSOR FOR

                 SELECT
                        P.CODPRE,
                        L.CODLIB,
                        L.TITOLO,
                        P.CODLET,
                        CHAR(P.DATA_PREST)

                 FROM PRESTITI P,
                      LIBRI L

                 WHERE P.CODLIB = L.CODLIB
                 AND P.DATA_REST IS NULL

                 ORDER BY P.DATA_PREST

           END-EXEC

           EXEC SQL OPEN CRIT END-EXEC

           DISPLAY
           'ELENCO LIBRI NON RESTITUITI'

           PERFORM UNTIL SQLCODE = 100

              EXEC SQL

                 FETCH CRIT

                 INTO
                    :HV-CODPRE,
                    :HV-CODLIB,
                    :HV-TITOLO,
                    :HV-CODLET,
                    :HV-DATAPRE

              END-EXEC

              IF SQLCODE = 0

                 ADD 1 TO WS-CONTEGGIO

                 DISPLAY
                 HV-CODPRE SPACE
                 HV-CODLIB SPACE
                 HV-CODLET SPACE
                 HV-DATAPRE

                 DISPLAY
                 HV-TITOLO

              END-IF

           END-PERFORM

           EXEC SQL CLOSE CRIT END-EXEC

           DISPLAY
           'TOTALE PRESTITI APERTI : '
           WS-CONTEGGIO

           GOBACK.
