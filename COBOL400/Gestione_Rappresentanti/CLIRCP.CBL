       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLIRICP.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       01 HV-CODCLI      PIC X(5).
       01 HV-RAGSOC      PIC X(40).
       01 HV-INDIRIZZO   PIC X(50).
       01 HV-CITTA       PIC X(30).
       01 HV-CAP         PIC X(5).
       01 HV-PIVA        PIC X(11).
       01 HV-RAPCLI      PIC X(5).

       PROCEDURE DIVISION.

       MAIN.

           DISPLAY 'CODICE CLIENTE : '
           ACCEPT HV-CODCLI

           EXEC SQL

              SELECT
                  RAGSOC,
                  INDIRIZZO,
                  CITTA,
                  CAP,
                  PIVA,
                  RAPCLI
              INTO
                  :HV-RAGSOC,
                  :HV-INDIRIZZO,
                  :HV-CITTA,
                  :HV-CAP,
                  :HV-PIVA,
                  :HV-RAPCLI
              FROM CLIENTI
              WHERE CODCLI = :HV-CODCLI

           END-EXEC

           IF SQLCODE = 0

              DISPLAY '================================='
              DISPLAY 'SCHEDA CLIENTE'
              DISPLAY '================================='

              DISPLAY 'CODICE        : ' HV-CODCLI
              DISPLAY 'RAGIONE SOC.  : ' HV-RAGSOC
              DISPLAY 'INDIRIZZO     : ' HV-INDIRIZZO
              DISPLAY 'CITTA         : ' HV-CITTA
              DISPLAY 'CAP           : ' HV-CAP
              DISPLAY 'P.IVA         : ' HV-PIVA
              DISPLAY 'RAPPRESENT.   : ' HV-RAPCLI

              DISPLAY '================================='

           ELSE

              DISPLAY 'CLIENTE NON TROVATO'

           END-IF

           GOBACK.
