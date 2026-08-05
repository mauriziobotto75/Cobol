       IDENTIFICATION DIVISION.
       PROGRAM-ID. RPTANN.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       01 HV-ANNO         PIC 9(4).

       01 HV-CODLIB       PIC X(6).
       01 HV-TITOLO       PIC X(60).
       01 HV-AUTORE       PIC X(40).
       01 HV-EDITORE      PIC X(40).

       01 WS-TOTLIB       PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.

           DISPLAY
           'ANNO EDIZIONE : '

           ACCEPT HV-ANNO

           EXEC SQL

              DECLARE CANN CURSOR FOR

                 SELECT
                        CODLIB,
                        TITOLO,
                        AUTORE,
                        EDITORE

                 FROM LIBRI

                 WHERE ANNOED =
                       :HV-ANNO

                 ORDER BY TITOLO

           END-EXEC

           EXEC SQL OPEN CANN END-EXEC

           DISPLAY
           '======================================='

           DISPLAY
           'LIBRI ANNO '

           DISPLAY HV-ANNO

           DISPLAY
           '======================================='

           PERFORM UNTIL SQLCODE = 100

              EXEC SQL

                 FETCH CANN

                 INTO
                    :HV-CODLIB,
                    :HV-TITOLO,
                    :HV-AUTORE,
                    :HV-EDITORE

              END-EXEC

              IF SQLCODE = 0

                 ADD 1 TO WS-TOTLIB

                 DISPLAY
                 HV-CODLIB SPACE
                 HV-TITOLO

                 DISPLAY
                 HV-AUTORE SPACE
                 HV-EDITORE

              END-IF

           END-PERFORM

           EXEC SQL CLOSE CANN END-EXEC

           DISPLAY
           '---------------------------------------'

           DISPLAY
           'TOTALE LIBRI : '
           WS-TOTLIB

           DISPLAY
           '---------------------------------------'

           GOBACK.
