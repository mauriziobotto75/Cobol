       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANN-POLIZZE.

       ENVIRONMENT *IVISION.

       INPUT-OUTPUT SECT*ON.

       FILE-CONTROL.

          SELECT POLIZZE
               ASSIGN TO DATABASE-POLIZZE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CODPOL.
         SELECT VIDEOFILE
              ASSIGN TO WORKSTN.

       DATA DIVISION.

       FILE SECTION.

       FD POLIZZE.

       01 POLREC.
          05 CODPOL         PIC X(10).
          05 NOMCLI         PIC X(40).
          05 INDIRIZZO     PIC X(60).
          05 IMPORTO        PIC 9(7)V99.
          05 SCADENZA       PIC X(10).
          05 SIGAUTO        PIC X(10).
          05 NCAVALLI       PIC 9(3). 
          05 STAPAG         PIC X.     
          05 DATPAG         PIC X(8).

       FD VIDEOFILE.

       01 ANNPOLR.
          05 V-CODPOL     PIC X(10).
          05 V-NOMCLI       PIC X(40).
*         05 V-IMPORTO      PIC 9(7*V99.
         *05 V-CONFERMA     PIC X.
         *05 V-MAG          PIC X(50).

       WORKING-STORAGE SECTION.

       O1 WS-FINE           PIC X VALUE 'N*.

       PROCEDURE DIVISION.

       MAIN.

           OPEN I-O POLI*ZE.

           PERFORM UNTIL WS-FINE = 'S'

              MOVE SPACES TO V-MSG
              MOVE SPACES TO V-CONFERMA
              MOVE SPACES TO V-NOMCLI
              MOVE ZEROES TO V-IMPORTO

              WRITE ANNPOLR
              READ VIDEOFILE

              IF IN03
                 MOVE 'S' TO WS-FINE
              ELSE
                 PERFO*M RICERCA-POLIZZA
              END-IF

           END-PERFORM.

           CLOSE POLIZZE

           GOBACK.
