       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSPOL.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

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
          05 INDIRIZ        PIC X(60).
          05 IMPORTO        PIC 9(7)V99.
          05 SCADENZA       PIC X(10).
          05 SIGAUTO        PIC X(10).
          05 NCAVALLI       PIC 9(3).
          05 STAPAG         PIC X.
          05 DATPAG         PIC X(10).

       FD VIDEOFILE.

       01 INSPOLR.
          05 V-CODPOL       PIC X(10).
          05 V-NOMCLI       PIC X(40).
          05 V-INDIRIZ      PIC X(60).
          05 V-IMPORTO      PIC 9(7)V99.
          05 V-SCADENZA     PIC X(10).
          05 V-SIGAUTO      PIC X(10).
          05 V-NCAVALLI     PIC 9(3).
          05 V-MSG          PIC X(50).

       WORKING-STORAGE SECTION.

       01 WS-FINE           PIC X VALUE 'N'.

       01 WS-ESISTE         PIC X VALUE 'N'.

       PROCEDURE DIVISION.

       MAIN.

           OPEN I-O POLIZZE.

           PERFORM UNTIL WS-FINE = 'S'

              MOVE SPACES TO V-MSG

              WRITE INSPOLR
              READ VIDEOFILE

              IF IN03

                 MOVE 'S'
                   TO WS-FINE

              ELSE

                 PERFORM CONTROLLA-CHIAVE

                 IF WS-ESISTE = 'S'

                    MOVE 'CODICE POLIZZA GIA PRESENTE'
                      TO V-MSG

                 ELSE

                    PERFORM INSERISCI-POLIZZA

                 END-IF

              END-IF

           END-PERFORM.

           CLOSE POLIZZE.

           GOBACK.
