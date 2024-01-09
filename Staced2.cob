 ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. Staced.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       SOURCE-COMPUTER. PC-HP.
       OBJECT-COMPUTER. PC-HP.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
      *-----------------------
       FILE-CONTROL.
           SELECT CARTOR ASSIGN TO DISK.
           SELECT CEDOL  ASSIGN TO DISK.
           SELECT ANOPIS
                  ASSIGN TO DISK
           RECORD KEY IS CHIAVE.

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       FD  CARDFILE
           LABEL RECORDS ARE OMITTED.
       01  STUDENT-RECORD.
           02 ID-CARD PIC X.
           88 FIRST-LAVEL VALUE '1'.
           88 SECOND-LAVEL VALUE '2'.
           02 NAME PIC X(24).
           02 STREET PIC X(20).
           02 CITY   PIC X(20).
           02 STATE  PIC X(10).
           02 ZIP    PIC X(5).

       FD  PRINTED-REPORT
           LABEL RECORDS ARE OMITTED.
       01  PRINT-RECORD PIC X(132).


       WORKING-STORAGE SECTION.
      *-----------------------
       77  PAGE-CTR PIC 9(4) COMP-3 VALUE ZERO.

       01  HEADING-RECORD-1 .
           02 FILLER PIC X(37) VALUE IS SPACES.
           02 TITLE  PIC X(25) VALUE IS 'STUDENT LISTING SEMI 1978'.
           02 FILLER PIC X(44) VALUE IS SPACES.
           02 FILLER PIC X(7) VALUE 'PAGE     '.
           02 PAGE-NUMBER PIC Z,ZZZ.
           02 FILLER PIC X(13) VALUE SPACE.

       01  HEADING-RECORD-2.
           02 FILLER PIC X(18) VALUE IS SPACES.
           02 HEADING-2  PIC X(4)  VALUE IS 'NAME'.
           02 FILLER PIC X(23) VALUE IS SPACES.
           02 HEADING-3 PIC X(6) VALUE IS 'STREET'.
           02 FILLER PIC X(21) VALUE IS SPACES.
           02 HEADING-4 PIC X(4) VALUE IS 'cITY'.
           02 FILLER PIC X(11) VALUE IS SPACES.
           02 HEADING-5 PIC X(5) VALUE IS 'STATE'.
           02 FILLER PIC X(5) VALUE IS SPACES.
           02 HEADING-6 PIC X(3) VALUE IS 'ZIP'.
           02 FILLER PIC X(36) VALUE IS SPACES.

       01  DETAIL-RECORD.
           02 FILLER PIC X(10) VALUE IS SPACES.
           02 NAME   PIC X(24).
           02 FILLER PIC X(5) VALUE IS SPACES.
           02 STREET  PIC X(20).
           02 FILLER PIC X(5) VALUE IS SPACES.
           02 CITY  PIC X(20).
           02 FILLER PIC X VALUE IS SPACES.
           02 STATE PIC X(10).
           02 FILLER PIC X VALUE IS SPACES.
           02 ZIP PIC X(5).
           02 FILLER PIC X(31) VALUE IS SPACES.
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PREPARATION-ROUTINE.
           OPEN INPUT CARDFILE
                OUTPUT PRINTED-REPORT.
       HEADING-ROUTINE.
           ADD 1 TO PAGE-CTR.
           MOVE PAGE-CTR TO PAGE-NUMBER.
           MOVE HEADING-RECORD-1 TO PRINT-RECORD.
           WRITE PRINT-RECORD AFTER POSITIONING ZERO.
           MOVE HEADING-RECORD-2 TO PRINT-RECORD.
           WRITE PRINT-RECORD AFTER POSITIONING 2.
       MAIN-SEQUENCE.
           READ CARDFILE AT END GO TO FINISH.
           IF FIRST-LEVEL
               GO TO MAIN-SEQUENCE.
           MOVE CORRESPONDING STUDENT-RECORD TO DETAIL-RECORD.
           MOVE DETAIL-RECORD TO PRINT-RECORD.
           WRITE PRINT-RECORD AFTER POSITIONING 2
                 AT END OF PAGE GO TO HEADING-ROUTINE.
           GO TO MAIN-SEQUENCE.
       FINISH.
           CLOSE CARDFILE PRINTED-REPORT.
           STOP RUN.

      **
      * The main procedure of the program
      **

      ** add other procedures here
       END PROGRAM STACED.
