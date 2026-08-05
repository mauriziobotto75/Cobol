       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIBSTA.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       01 HV-AUTORE      PIC X(40).

       01 HV-CODLIB      PIC X(6).
       01 HV-TITOLO      PIC X(60).
       01 HV-EDITORE     PIC X(40).
       01 HV-ANNO        PIC 9(4).
       01 HV-GENERE      PIC X(20).

       PROCEDURE DIVISION.

           DISPLAY 'AUTORE DA STAMPARE : '
           ACCEPT HV-AUTORE

           EXEC SQL

              DECLARE CLIB CURSOR FOR

                  SELECT
                         CODLIB,
                         TITOLO,
                         EDITORE,
                         ANNOED,
                         GENERE

                  FROM LIBRI

                  WHERE AUTORE =
                        :HV-AUTORE

                  ORDER BY TITOLO

           END-EXEC

           EXEC SQL
                OPEN CLIB
           END-EXEC

           DISPLAY
           '========================================='

           DISPLAY
           'ELENCO LIBRI AUTORE'

           DISPLAY HV-AUTORE

           DISPLAY
           '========================================='

           PERFORM UNTIL SQLCODE = 100

              EXEC SQL

                 FETCH CLIB

                 INTO
                    :HV-CODLIB,
                    :HV-TITOLO,
                    :HV-EDITORE,
                    :HV-ANNO,
                    :HV-GENERE

              END-EXEC

              IF SQLCODE = 0

                 DISPLAY
                 HV-CODLIB SPACE
                 HV-TITOLO SPACE
                 HV-EDITORE

              END-IF

           END-PERFORM

           EXEC SQL
                CLOSE CLIB
           END-EXEC

           GOBACK.
