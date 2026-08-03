       IDENTIFICATION DIVISION.
       PROGRAM-ID. VISU_POL.

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

       01 VISUR.
          05 V-CODPOL       PIC X(10).
          05 V-NOMCLI       PIC X(40).
          05 V-INDIRIZ      PIC X(60).
          05 V-IMPORTO      PIC 9(7)V99.
          05 V-SCADENZA     PIC X(10).
          05 V-MSG          PIC X(50).

       WORKING-STORAGE SECTION.

       01 WS-EXIT.
          05 WS-FINE PIC X VALUE 'N'.

       PROCEDURE DIVISION.

       MAIN.

           OPEN INPUT POLIZZE.

           PERFORM UNTIL WS-FINE = 'S'

              WRITE VISUR

              READ VIDEOFILE

              IF IN03

                 MOVE 'S'
                   TO WS-FINE

              ELSE

                 MOVE V-CODPOL
                   TO CODPOL

                 READ POLIZZE
                    INVALID KEY

                       MOVE 'POLIZZA NON TROVATA'
                         TO V-MSG

                    NOT INVALID KEY

                       MOVE NOMCLI
                         TO V-NOMCLI

                       MOVE INDIRIZ
                         TO V-INDIRIZ

                       MOVE IMPORTO
                         TO V-IMPORTO

                       MOVE SCADENZA
                         TO V-SCADENZA

                       MOVE 'POLIZZA TROVATA'
                         TO V-MSG

                 END-READ

              END-IF

           END-PERFORM.

           CLOSE POLIZZE.

           GOBACK.
