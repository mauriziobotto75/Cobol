       IDENTIFICATION DIVISION.
       PROGRAM-ID. INT001.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT CONTISAL
               ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CNTNUMERO
               FILE STATUS IS FS-CONTI.

       DATA DIVISION.

       FILE SECTION.

       FD CONTISAL.

       01 REC-CONTO.
          05 CNTNUMERO      PIC X(12).
          05 CLICODICE      PIC X(6).
          05 FILCODICE      PIC X(4).
          05 CNTNOME        PIC X(40).
          05 CNTINDIR       PIC X(40).
          05 CNTSALDO       PIC S9(11)V99.
          05 CNTFIDO        PIC S9(11)V99.
          05 CNTSTATO       PIC X.

       WORKING-STORAGE SECTION.

       01 FS-CONTI         PIC XX.
       01 FINE-PROG        PIC X VALUE 'N'.

       PROCEDURE DIVISION.

       MAIN.

           OPEN INPUT CONTISAL

           START CONTISAL
               KEY >= LOW-VALUES
               INVALID KEY
                  MOVE 'S' TO FINE-PROG
           END-START

           PERFORM UNTIL FINE-PROG = 'S'

              READ CONTISAL NEXT RECORD

                 AT END
                    MOVE 'S' TO FINE-PROG

                 NOT AT END

                    IF CNTSALDO < 0

                       DISPLAY CNTNUMERO
                       DISPLAY CNTNOME
                       DISPLAY CNTSALDO

                    END-IF

              END-READ

           END-PERFORM

           CLOSE CONTISAL

           STOP RUN.
