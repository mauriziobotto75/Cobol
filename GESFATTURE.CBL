       IDENTIFICATION DIVISION.

       PROGRAM-ID. GESFATTURE.
       AUTHOR.  MB.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       SPECIAL NAMES. DECIMAL POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FCLIENTI ASSIGN TO FORMATFILE-FCLIENTI
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-FCLIENTI.

           SELECT FFATTURE ASSIGN TO DATABASE-FFATTURE
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS KEY-FFATTURE
           FILE STATUS IS FS-FATTURE.

           SELECT SITCLIEN ASSIGN TO FORMATFILE-SITCLIEN
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-SITCLIEN.

       DATA DIVISION.

       FILE SECTION.
         FD CLIENTI
            LABEL RECORD IS STANDARD
            RECORD CONTAINS 100 CHARACTERS
            DATA RECORD IS REC-CLIENTI.

       01 REC-CLIENTI    PIC X(100).

         FD FFATTURE
            LABEL RECORD IS STANDARD
            RECORD CONTAINS 50 CHARACTERS
            DATA RECORD IS REC-FATTURE.
       01 REC-FATTURE.
           05 KEY-FFATTURE.
             07 FFAT-COD-CLI  PIC 9(5).
             07 FFAT-NUM-FAT  PIC X(8).
           05 FILLER           PIC X(37).

       FD SITCLIEN
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 90 CHARACTERS
           DATA RECORD IS REC-SITCLIE.
       01 REC-SITCLIE        PIC X(90).

       WORKING-STORAGE SECTION.

       01 WS-REC-CLIENTI.
           05 WS-FCLI-COD-CLI PIC 9(5) VALUE ZEROES.
           05 WS-FCLI-INDIR   PIC X(20) VALUE SPACES.
           05 WS-FCLI-TEL     PIC X(10) VALUE ZEROES.
           05 WS-FCLI-CITTA   PIC X(10) VALUE SPACES.
           05 WS-FCLI-NOMINAT PIC X(25) VALUE SPACES.
           05 FILLER          PIC X(30) VALUE SPACES.

       01 WS-REC-FATTURE-OLD.
           05 WS-KEY-FFATTURE-OLD.
              07 WS-FFAT-COD-CLI-OLD PIC 9(5) VALUE ZEORES.
              07 WS-FFAT-NUM-FATT-OLD PIC X(8) VALUE SPACES.
              07 WS-FFAT-IMPONIB-OLD  PIC 9(14) COMP-3 VALUE ZEROES.
              07 WS-FFAT-IVA-OLD      PIC 9(14) COMP-3 VALUE ZEROES.
              07 WS-FFAT-TOT-FATT-OLD PIC 9(14) COMP-3 VALUE ZEROES.
           05 FILLER                  PIC X(13) VALUE SPACES.
       01 WS-REC-FATTURE-NEW.
           05 WS-KEY-FFATTURE-NEW.
              07 WS-FFAT-COD-CLI-NEW  PIC 9(5) VALUE ZEROES.
              07 WS-FFAT-NUM-FAT-NEW  PIC X(8) VALUE SPACES.
           05 WS-FFAT-IMPONIB-NEW     PIC 9(14) VALUES ZEROES.
           05 WS-FFAT-IVA-NEW         PIC 9(14) VALUES ZEROES.
           05 WS-FFAT-TOT-FATT-NEW    PIC 9(14) VALUES ZEROES.
           05 FILLER                  PIC X(13) VALUES SPACES.

       01 WS-REC-SITCLIE.
           05 WS-SIT-COD-CLI          PIC 9(5) VALUE ZEROES.
           05 WS-SIT-INDIR            PIC X(20) VALUE SPACES.
           05 WS-SIT-TEL              PIC 9(10) VALUE ZEROES.
           05 WS-SIT-NUM-TOT-FATT     PIC 9(8)  VALUE ZEROES.
           05 WS-SIT-FATT-DA          PIC X(8) VALUE  SPACES.
           05 WS-SIT-FATT-A           PIC X(8) VALUE SPACES.
           05 WS-SIT-TOT-IMPONIB      PIC 9(15) COMP-3 VALUE ZEROES.
           05 WS-SIT-TOT-IVA          PIC 9(15) COMP-3 VALUE ZEROES.
           05 WS-SIT-TOT-FATTURA      PIC 9(15) COMP-3 VALUE ZEROES.
           05 FILLER                  PIC X(7) VALUES SPACES.

       01 WS-CAMPI-CONTATORI.
           05 WS-CTR-REC-LET-FCLIENTI  PIC 9(3) VALUE ZEROES.
           05 WS-CTR-REC-LET-FFATTURE  PIC 9(3) VALUE ZEROES.
           05 WS-CTR-REC-SCR-SITCLIEN  PIC 9(3) VALUE ZEROES.

       01 WS-CAMPI-COMODO.
           05 WS-TOT-COM-IMPONIB      PIC 9(15) COMP-3 VALUE ZEROES.
           05 WS-TOT-COM-IVA          PIC 9(15) COMP-3 VALUE ZEROES.
           05 WS-TOT-COM-FATTURA      PIC 9(15) COMP-3 VALUE ZEROES.
           05 WS-TOT-COM-N-FAT        PIC 9(8)  VALUES ZEROES.
           05 WS-COM-FAT-DA           PIC X(8)  VALUES SPACES.
       01 SW-FINE-CLI                PIC 9(1) VALUES ZEROES.
           88 EOF-CLI                 VALUE 1.
       01 SW-FINE-FAT                PIC 9(1) VALUES ZEROES.
           88 EOF-FAT                 VALUE 1.

 *======================================================================================================================================================*
 *                                        DEFINIZIONE CAMPI DI FILE STATUS                                                                              *
 *============================================================================ ==========================================================================*

       01 FS-FCLIENTI                PIC 9(2) VALUE ZEROES.
       01 FS-FATTURE                 PIC 9(2) VALUE ZEROES.
       01 FS-SITCLIEN                PIC 9(2) VALUE ZEROES.

  *======================================================================================================================================================*
       PROCEDURE DIVISION.
  *======================================================================================================================================================*

       INIZIO.
          PERFORM OP-INIZIALI THRU OP-INIZIALI-EX.
          PERFORM ELABORA THRU ELABORA-EX UNTIL EOF-CLI.
          PERFORM OP-FINALI   THRU OP-FINALI-EX.
          STOP RUN.

       OP-INIZIALI.
          OPEN INPUT FCLIENTI, FFATTURE, SITCLIEN.
          READ FCLIENTI INTO WS-REC-CLIENTI AT END MOVE 1 TO SW-FINE-CLI.
          ADD 1 TO WS-CTR-REC-LET-FCLIENTI.
       OP-INIZIALI-EX.
          EXIT.

       ELABORA.
          MOVE WS-FCLI-COD-CLI TO FFAT-COD-CLI
          MOVE SPACES TO FFAT-NUM-FATT.
          START FFATTURE KEY IS GREATER THAN KEY-FFATTURE
                INVALID KEY
                GOTO B010.
          PERFORM LETTURA-IND THRU LETTURA-IND-EX.
          PERFORM ELAB-FATTURA THRU ELAB-FATTURA-EX UNTIL EOF-FAT
                  OR WS-FCLI-CO-CLI NOT EQUAL WS-FFATT-COD-CLI-NEW.
       B010.
          PERFORM SCRITTURA THRU SCRITTURA-EX.
          PERFORM LETTURA-SEQ THRU LETTURA-SEQ-EX.
       ELABORA-EX.
          EXIT.
       LETTURA-IND.
          READ FFATTURE NEXT AT END MOVE 1 TO SW-FINE-FAT
                        GO TO LETTURA-IND-EX.
          ADD 1 TO WS-CTR-REC-LET-FFATTURE.
          MOVE REC-FATTURE TO WS-REC-FATTURE-NEW.
       LETTURA-IND-EX.
          EXIT.

       ELAB-FATTURA.
          MOVE WS-REC-FATTURE-NEW TO  WS-REC-FATTURE-OLD.
          ADD  WS-FFAT-IMPONIB-OLD TO WS-TOT-COM-IMPONIB
          ADD  WS-FFAT-IVA-OLD  TO WS-TOT-COM-IVA
          ADD  WS-FFAT-TOT-FATT-OLD TO WS-TOT-COM-FATTURA
          ADD 1 TO WS-TOT-COM-N-FAT.
          IF WS-TOT-COM-N-FAT = 1
             MOVE WS-FFAT-NUM-FATT-NEW TO WS-COM-FATT-DA.
             LETTURA-IND THRU LETTURA-IND-EX.
       ELAB-FATTURA-EX.
          EXIT.


       SCRITTURA.
          MOVE WS-FCLI-COD-CLI TO WS-SIT-COD-CLI
          MOVE WS-FCLI-INDIR   TO WS-SIT-INDIR
          MOVE WS-FCLI-TEL     TO WS-SIT-TEL
          MOVE WS-TOT-COM-N-FAT TO WS-SIT-TOT-NUM-FATT
          MOVE WS-COM-FATT-DA   TO WS-SIT-FATT-DA
          MOVE WS-FFAT-NUM-FAT-OLD TO WS-SIT-FATT-A
          MOVE WS-TOT-COM-IMPONIB TO WS-SIT-TOT-IMPONIB
          MOVE WS-TOT-COM-IVA     TO WS-SIT-TOT-IVA
          MOVE WS-TOT-COM-FATTURA TO WS-SIT-TOT-FATTURA.
          WRITE REC-SITCLIE TO WS-REC-SCR-SITCLIE.
          ADD 1 TO WS-CTR-REC-SCR-SITCLIEN.
          MOVE ZEROES TO WS-TOT-COM-N-FAT
          MOVE ZEROES TO WS-TOT-COM-IMPONIB
          MOVE ZEROES TO WS-TOT-COM-IVA
          MOVE ZEROES TO WS-TOT-COM-FATTURA
          MOVE SPACES TO WS-COM-FATT-DA
          MOVE SPACES TO WS-FFAT-NUM-FATT-OLD.
       SCRITTURA-EX.
          EXIT.

       LETTURA-SEQ.
          READ FCLIENTI INTO WS-REC-CLIENTI
               AT END MOVE 1 TO SW-FINE-CLI
                  GO TO LETTURA-SEQ-EX.
          ADD 1 TO WS-CTR-REC-LET-FCLIENTI.
       LETTURA-SEQ-EX.
          EXIT.
       OP-FINALI.
         CLOSE FCLIENTI, FFATTURE, SITCLIEN.
       OP-FINALI-EX.
         EXIT.
