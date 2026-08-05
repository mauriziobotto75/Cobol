       IDENTIFICATION DIVISION.
       PROGRAM-ID. RESTLIB.

       WORKING-STORAGE SECTION.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       01 HV-CODPRE PIC 9(6).
       01 HV-CODLIB PIC X(6).

       PROCEDURE DIVISION.

           ACCEPT HV-CODPRE

           EXEC SQL

              SELECT CODLIB

                INTO :HV-CODLIB

                FROM PRESTITI

               WHERE CODPRE = :HV-CODPRE

           END-EXEC

           EXEC SQL

              UPDATE PRESTITI

                 SET DATA_REST =
                     CURRENT DATE

               WHERE CODPRE =
                     :HV-CODPRE

           END-EXEC

           EXEC SQL

              UPDATE LIBRI

                 SET DISPONIBILE = 'S'

               WHERE CODLIB =
                     :HV-CODLIB

           END-EXEC

           EXEC SQL COMMIT END-EXEC

           DISPLAY 'LIBRO RESTITUITO'

           GOBACK.
