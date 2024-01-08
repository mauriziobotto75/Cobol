      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. LISTING.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

       CONFIGURATION SECTION.
      *-----------------------
       SOURCE-COMPUTER. PC-DOS.
       OBJECT-COMPUTER. PC-DOS.
       INPUT-OUTPUT SECTION.
      *-----------------------
       FILE-CONTROL.
      *   SELECT CARDFILE ASSIGN TO sys005-ur-2540-s-infile.
      *    SELECT CARDFILE
      *     ASSIGN TO
      *    'C:\Users\MBOTTO\OneDrive - Capgemini\ Desktop\Programmazione COBOL\CardFile.txt'".
          SELECT CARDFILE ASSIGN TO DISK.
          SELECT PRINTED-REPORT ASSIGN TO SYS006-UR-1403-S-OUTFILE.
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
           FD CARDFILE
           LABEL RECORDS ARE OMITTED.
       01  STUDENT-RECORD.
           02 ID-CARD    PIC X.
             88 FIRST-LAVEL VALUE '1'.
             88 SECOND-LAVEL VALUE '2'.
           02 NAME        PIC X(24).
           02 STREET      PIC X(20).
           02 CITY        PIC X(20).
           02 STATE       PIC X(10).
           02 ZIP         PIC X(5).

           FD PRINTED-REPORT
           LABEL RECORDS ARE OMITTED.
       01  PRINT-RECORD  PIC X(132).


       WORKING-STORAGE SECTION.
      *-----------------------
       01  PAGE-CTR      PIC 9(3) VALUE ZERO.
       01  HEADING-RECORD-1.
           02  FILLER    PIC X(37) VALUE IS SPACES.
           02  TITLES    PIC X(24) VALUE IS
           "STUDENT LISTING SEMINARIO 1978".
           02  FILLER    PIC X(44) VALUE IS SPACES.
           02  FILLER    PIC X(7)  VALUE 'PAGE    '.
           02  PAGE-NUMBER PIC Z.ZZZ.
           02  FILLER    PIC X(13) VALUE IS SPACES.
       01  HEADING-RECORD-2.
           02  FILLER    PIC X(18) VALUE IS SPACES.
           02  HEADING-2  PIC X(4) VALUE IS 'NAME'.
           02  FILLER    PIC X(23) VALUE IS SPACES.
           02  HEADING-3  PIC X(6) VALUE IS 'STREET'.
           02  FILLER    PIC X(21) VALUE IS SPACES.
           02  HEADING-4  PIC X(4) VALUE IS 'CITY'.
           02  FILLER    PIC X(11) VALUE IS SPACES.
           02  HEADING-5 PIC X(5)  VALUE IS 'STATE'.
           02  FILLER    PIC X(5)  VALUE IS SPACES.
           02  HEADING-6 PIC X(3)  VALUE IS 'ZIP'.
           02  FILLER    PIC X(36) VALUE IS SPACES.
       01  DETAIL-RECORD.
           02  FILLER    PIC X(10) VALUE IS SPACES.
           02  NOME      PIC X(24).
           02  FILLER    PIC X(5)  VALUE IS SPACES.
           02  STREET    PIC X(20).
           02  FILLER    PIC X(5)  VALUE IS SPACES.
           02  CITY      PIC X(20).
           02  FILLER    PIC X  VALUE IS SPACES.
           02  STATE     PIC X(10).
           02  FILLER    PIC X  VALUE IS SPACES.
           02  ZIP       PIC X(5).
           02  FILLER    PIC X(31)  VALUE IS SPACES.

       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PREPARATION-ROUTINE.
           OPEN INPUT CARDFILE
                OUTPUT PRINTED-REPORT.
       HEADING-ROUTINE.
           ADD 1 TO PAGE-CTR.
           MOVE PAGE-CTR TO PAGE-NUMBER.
           MOVE HEADING-RECORD-1 TO PRINT-RECORD.
           WRITE PRINT-RECORD AFTER 1.
           MOVE HEADING-RECORD-2 TO PRINT-RECORD.
           WRITE PRINT-RECORD AFTER 2.
       HEADING-ROUTINE-EX.
           EXIT.

       MAIN-SEQUENCE.
           READ CARDFILE AT END GO TO FINISH.
           IF FIRST-LaVEL  GO TO
                          MAIN-SEQUENCE.
           MOVE CORReSPONDING STUDENT-RECORD TO DETAIL-RECORD.
           MOVE DETAIL-RECORD TO PRINT-RECORD.
           WRITE PRINT-RECORD  AFTER   2
                 AT END-OF-PAGE  GO TO HEADING-ROUTINE.
           GO TO MAIN-SEQUENCE.
       FINISH.
           CLOSE CARDFILE
           CLOSE PRINTED-REPORT.
           STOP RUN.





      **
      * The main procedure of the program
      **

      ** add other procedures here
       END PROGRAM LISTING.
