       IDENTIFICATION DIVISION.
       PROGRAM-ID. LETRIC.

       WORKING-STORAGE SECTION.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       01 HV-CODLET      PIC X(6).
       01 HV-COGNOME     PIC X(30).
       01 HV-NOME        PIC X(30).
       01 HV-INDIRIZZO   PIC X(50).
       01 HV-TELEFONO    PIC X(20).

       PROCEDURE DIVISION.

           DISPLAY 'CODICE LETTORE'
           ACCEPT HV-CODLET

           EXEC SQL

              SELECT
                     COGNOME,
                     NOME,
                     INDIRIZZO,
                     TELEFONO

              INTO
                     :HV-COGNOME,
                     :HV-NOME,
                     :HV-INDIRIZZO,
                     :HV-TELEFONO

              FROM LETTORI

              WHERE CODLET =
                    :HV-CODLET

           END-EXEC

           IF SQLCODE = 0

              DISPLAY 'COGNOME  : ' HV-COGNOME
              DISPLAY 'NOME     : ' HV-NOME
              DISPLAY 'INDIRIZZO: ' HV-INDIRIZZO
              DISPLAY 'TELEFONO : ' HV-TELEFONO

           ELSE

              DISPLAY 'LETTORE NON TROVATO'

           END-IF

           GOBACK.
