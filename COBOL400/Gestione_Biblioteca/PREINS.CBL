       IDENTIFICATION DIVISION.
       PROGRAM-ID. PREINS.

       WORKING-STORAGE SECTION.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       01 HV-CODPRE      PIC 9(6).
       01 HV-CODLIB      PIC X(6).
       01 HV-CODLET      PIC X(6).

       01 HV-DISP        PIC X.

       PROCEDURE DIVISION.

           ACCEPT HV-CODPRE
           ACCEPT HV-CODLIB
           ACCEPT HV-CODLET

           EXEC SQL

              SELECT DISPONIBILE

                INTO :HV-DISP

                FROM LIBRI

               WHERE CODLIB = :HV-CODLIB

           END-EXEC

           IF HV-DISP NOT = 'S'

              DISPLAY 'LIBRO NON DISPONIBILE'
              GOBACK

           END-IF

           EXEC SQL

              INSERT INTO PRESTITI

              (
                  CODPRE,
                  CODLIB,
                  CODLET,
                  DATA_PREST
              )

              VALUES

              (
                  :HV-CODPRE,
                  :HV-CODLIB,
                  :HV-CODLET,
                  CURRENT DATE
              )

           END-EXEC

           EXEC SQL

              UPDATE LIBRI

              SET DISPONIBILE = 'N'

              WHERE CODLIB = :HV-CODLIB

           END-EXEC

           EXEC SQL COMMIT END-EXEC

           DISPLAY 'PRESTITO REGISTRATO'

           GOBACK.
