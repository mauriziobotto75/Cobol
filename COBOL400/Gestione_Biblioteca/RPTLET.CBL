       IDENTIFICATION DIVISION.
       PROGRAM-ID. RPTLET.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       01 HV-CODLET       PIC X(6).

       01 HV-CODPRE       PIC 9(6).
       01 HV-CODLIB       PIC X(6).
       01 HV-TITOLO       PIC X(60).
       01 HV-DTPRE        PIC X(10).
       01 HV-DTRES        PIC X(10).

       01 WS-TOTALE       PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.

           DISPLAY 'CODICE LETTORE : '
           ACCEPT HV-CODLET

           EXEC SQL

              DECLARE CLET CURSOR FOR

                 SELECT
                        P.CODPRE,
                        P.CODLIB,
                        L.TITOLO,
                        CHAR(P.DATA_PREST),
                        CHAR(COALESCE(P.DATA_REST,
                                      CURRENT DATE))

                 FROM PRESTITI P,
                      LIBRI L

                 WHERE P.CODLIB = L.CODLIB
                 AND P.CODLET = :HV-CODLET

                 ORDER BY P.DATA_PREST

           END-EXEC

           EXEC SQL OPEN CLET END-EXEC

           DISPLAY
           'ELENCO PRESTITI DEL LETTORE'

           PERFORM UNTIL SQLCODE = 100

              EXEC SQL
                   FETCH CLET
                   INTO
                        :HV-CODPRE,
                        :HV-CODLIB,
                        :HV-TITOLO,
                        :HV-DTPRE,
                        :HV-DTRES
              END-EXEC

              IF SQLCODE = 0

                 ADD 1 TO WS-TOTALE

                 DISPLAY
                 HV-CODPRE SPACE
                 HV-CODLIB SPACE
                 HV-TITOLO

              END-IF

           END-PERFORM

           EXEC SQL CLOSE CLET END-EXEC

           DISPLAY
           'TOTALE PRESTITI : '
           WS-TOTALE

           GOBACK.
