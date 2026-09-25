       IDENTIFICATION DIVISION.
       PROGRAM-ID. INT003.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT MOVDATA
               ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS KEY-DATA
               FILE STATUS IS FS-MOV.

       DATA DIVISION.

       FILE SECTION.

       FD MOVDATA.

       01 REC-MOV.
          05 MOVDATA       PIC 9(8).
          05 MOVNUMERO     PIC 9(8).
          05 CNTNUMERO     PIC X(12).
          05 MOVTIPO       PIC XX.
          05 MOVIMPORTO    PIC S9(11)V99.

       WORKING-STORAGE SECTION.

       01 KEY-DATA.
          05 K-DATA       PIC 9(8).
          05 K-NUM        PIC 9(8).

       01 DATA-DA         PIC 9(8).
       01 DATA-A          PIC 9(8).
       01 FS-MOV          PIC XX.
       01 FINE-PROG       PIC X VALUE 'N'.

       PROCEDURE DIVISION.

       MAIN.

           DISPLAY "DATA DA (AAAAMMGG): "
           ACCEPT DATA-DA

           DISPLAY "DATA A (AAAAMMGG): "
           ACCEPT DATA-A

           MOVE DATA-DA TO K-DATA
           MOVE 0       TO K-NUM

           OPEN INPUT MOVDATA

           START MOVDATA
               KEY >= KEY-DATA
               INVALID KEY
                  MOVE 'S' TO FINE-PROG
           END-START

           PERFORM UNTIL FINE-PROG = 'S'

               READ MOVDATA NEXT RECORD

                  AT END
                     MOVE 'S' TO FINE-PROG

                  NOT AT END

                     IF MOVDATA > DATA-A

                        MOVE 'S' TO FINE-PROG

                     ELSE

                        DISPLAY CNTNUMERO
                        DISPLAY MOVDATA
                        DISPLAY MOVTIPO
                        DISPLAY MOVIMPORTO

                     END-IF

               END-READ

           END-PERFORM

           CLOSE MOVDATA

       
