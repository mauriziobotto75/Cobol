       IDENTIFICATION DIVISION.
       PROGRAM-ID. INT002.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT MOVTIPO
               ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS KEY-MOVTIPO
               FILE STATUS IS FS-MOV.

       DATA DIVISION.

       FILE SECTION.

       FD MOVTIPO.

       01 REC-MOV.
          05 MOVTIPO       PIC XX.
          05 MOVDATA       PIC 9(8).
          05 MOVNUMERO     PIC 9(8).
          05 CNTNUMERO     PIC X(12).
          05 MOVCAUSALE    PIC X(40).
          05 MOVIMPORTO    PIC S9(11)V99.

       WORKING-STORAGE SECTION.

       01 KEY-MOVTIPO.
          05 K-TIPO        PIC XX.
          05 K-DATA        PIC 9(8).
          05 K-NUM         PIC 9(8).

       01 TIPO-RICH       PIC XX.
       01 FS-MOV          PIC XX.
       01 FINE-PROG       PIC X VALUE 'N'.

       PROCEDURE DIVISION.

       MAIN.

           DISPLAY "TIPO MOVIMENTO : "
           ACCEPT TIPO-RICH

           MOVE TIPO-RICH TO K-TIPO
           MOVE ZEROS     TO K-DATA K-NUM

           OPEN INPUT MOVTIPO

           START MOVTIPO
                KEY >= KEY-MOVTIPO
                INVALID KEY
                    MOVE 'S' TO FINE-PROG
           END-START

           PERFORM UNTIL FINE-PROG = 'S'

               READ MOVTIPO NEXT RECORD

                   AT END
                       MOVE 'S' TO FINE-PROG

                   NOT AT END

                       IF MOVTIPO = TIPO-RICH

                          DISPLAY CNTNUMERO
                          DISPLAY MOVDATA
                          DISPLAY MOVIMPORTO
                          DISPLAY MOVCAUSALE

                       ELSE

                          MOVE 'S' TO FINE-PROG

                       END-IF

               END-READ

           END-PERFORM

           CLOSE MOVTIPO

           STOP RUN.
