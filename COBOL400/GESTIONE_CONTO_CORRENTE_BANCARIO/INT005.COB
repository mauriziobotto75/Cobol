       IDENTIFICATION DIVISION.
       PROGRAM-ID. INT005.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT MOVCONTO
               ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS KEY-CONTO
               FILE STATUS IS FS-MOV.

       DATA DIVISION.

       FILE SECTION.

       FD MOVCONTO.

       01 REC-MOV.
          05 CNTNUMERO      PIC X(12).
          05 MOVDATA        PIC 9(8).
          05 MOVNUMERO      PIC 9(8).
          05 MOVTIPO        PIC XX.
          05 MOVCAUSALE     PIC X(40).
          05 MOVIMPORTO     PIC S9(11)V99.

       WORKING-STORAGE SECTION.

       01 KEY-CONTO.
          05 K-CONTO        PIC X(12).
          05 K-DATA         PIC 9(8).
          05 K-NUM          PIC 9(8).

       01 CONTO-RIC         PIC X(12).
       01 DATA-DA           PIC 9(8).
       01 DATA-A            PIC 9(8).

       01 FS-MOV            PIC XX.
       01 FINE-PROG         PIC X VALUE 'N'.

       PROCEDURE DIVISION.

       MAIN.

           DISPLAY "CONTO : "
           ACCEPT CONTO-RIC

           DISPLAY "DATA DA : "
           ACCEPT DATA-DA

           DISPLAY "DATA A : "
           ACCEPT DATA-A

           MOVE CONTO-RIC TO K-CONTO
           MOVE 0         TO K-DATA K-NUM

           OPEN INPUT MOVCONTO

           START MOVCONTO
                KEY >= KEY-CONTO
                INVALID KEY
                    MOVE 'S' TO FINE-PROG
           END-START

           PERFORM UNTIL FINE-PROG = 'S'

              READ MOVCONTO NEXT RECORD

                 AT END
                    MOVE 'S' TO FINE-PROG

                 NOT AT END

                    IF CNTNUMERO NOT = CONTO-RIC

                       MOVE 'S' TO FINE-PROG

                    ELSE

                       IF MOVDATA >= DATA-DA
                       AND MOVDATA <= DATA-A

                           DISPLAY MOVDATA
                           DISPLAY MOVTIPO
                           DISPLAY MOVCAUSALE
                           DISPLAY MOVIMPORTO

                       END-IF

                    END-IF

              END-READ

           END-PERFORM

           CLOSE MOVCONTO

           STOP RUN.
