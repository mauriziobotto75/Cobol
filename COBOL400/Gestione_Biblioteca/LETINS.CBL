       IDENTIFICATION DIVISION.
       PROGRAM-ID. LETINS.

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

           DISPLAY 'COGNOME'
           ACCEPT HV-COGNOME

           DISPLAY 'NOME'
           ACCEPT HV-NOME

           DISPLAY 'INDIRIZZO'
           ACCEPT HV-INDIRIZZO

           DISPLAY 'TELEFONO'
           ACCEPT HV-TELEFONO

           EXEC SQL

              INSERT INTO LETTORI

              VALUES
              (
                 :HV-CODLET,
                 :HV-COGNOME,
                 :HV-NOME,
                 :HV-INDIRIZZO,
                 :HV-TELEFONO
              )

           END-EXEC

           EXEC SQL COMMIT END-EXEC

           GOBACK.
