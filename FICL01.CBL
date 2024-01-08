IDENTIFICATION DIVISION.
PROGRAM-ID.
             FICL01.
************************************************************************************************************************************************************************
*            PROGRAMMA DI CARICAMENTO, ANNULLAMENTO E VARIAZIONE FIDO SU CLIENTI BY B.M.
************************************************************************************************************************************************************************
CONFIGURATION SECTION.
SPECIAL NAMES.
     DECIMAL-POINT IS COMMA.
SOURCE-COMPUTER.       ALPHA.
OBJECT-COMPUTER.       ALPHA.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
*
*
        COPY "FIDOCL.FCL".
        COPY "ANACLI.FCL".
*
************************************************************************************************************************************************************************
DATA DIVISION.
FILE SECTION.
*
     COPY  "FIDOCL.CPY"
     COPY  "ANACLI.CPY"
*
************************************************************************************************************************************************************************
WORKING-STORAGE SECTION.
*
    COPY "ANOMAL.CPY".
    COPY "FIDOIM.CPY".
    COPY "FMSAREA.CPY".
*
01  INDICE  PIC 99 VALUE ZERO.
01  GIRA-DATA.
    05 DT-GG    PIC 99.
    05 DT-MM    PIC 99.
    05 DT-AA    PIC 99.
01  XGEN-SI     PIC XX    VALUE "SI".
01  XGEN-NO     PIC XX    VALUE "NO".
01  RTC-CONTROLLI  PIC XX.
01  RTC-TROVATO    PIC XX.
*
01 END-N200        PIC XX.
01 LIB-MAPPE       PIC X(10) VALUE "FIDOCL.FLB"
01 NOME-MAPPA      PIC X(6)  VALUE "FICL01".
01 NOME-PROGRAMMA.
   05 NOME-ASS     PIC X(6)  VALUE "PROC0:".
   05 NOME-PGM     PIC X(6).
*
01 WW-IMPORTO-FIDO     PIC 9(12).
01 WI-BUFFER.
   05 WI-DATA.
      10 WI-GG         PIC 9(2).
      10 WI-MM         PIC 9(2).
      10 WI-AA         PIC 9(4).
   05 WI-CODCLI        PIC 9(4).
   05 WI-DESCLI        PIC X(30).
   05 WI-CITCLI        PIC X(7).
   05 WI-NAZCLI        PIC X(7).
   05 WI-IMPORTO-FIDO  PIC 9(10).
   05 WI-CONFER        PIC X.
*
01 CAMPI-MASCHERA.
   05 CODCLI           PIC X(6) VALUE "CODCLI".
   05 DESCLI           PIC X(6) VALUE "DESCLI".
   05 CITCLI           PIC X(6) VALUE "CITCLI".
   05 NAZCLI           PIC X(6) VALUE "NAZCLI".
   05 CONFER           PIC X(6) VALUE "CONFER".
*
01 XX-MESS-MESS.
   05 XX-MESS          OCCURS 10.
      10 XX-MESS1      PIC X(10).
      10 XX-MESS-RIGA  PIC X(2).
      10 XX-MESS2      PIC X(48).
*
***********************************************************************************************************************************************************************
PROCEDURE DIVISION.
***********************************************************************************************************************************************************************
DECLARATIVES.
IO-ERROR1 SECTION.
   USE AFTER ERROR PROCEDURE ON FIDOCL.
IO-INI1.
   IF BUCKET-LOCKED 
      CALL "LIB$WAIT" USING BY REFERENCE IM-DELAY
   ELSE 
      MOVE RTC TO QQ-RTC
      MOVE "FIDOCL" TO QQ-FILE
      MOVE ANOMAL TO NOME-PGM
      MOVE PUT_MODE TO IM-MODE
      MOVE QQ-BUFFER TO IM-RESTO
      CALL "XCGBUFF" USING IM-BUFFER 
      CALL "LIB$RUN_PROGRAM" USING BY DESCRIPTOR NOME-PROGRAMMA.
*
IO-ERROR2 SECTION.
        USE AFTER ERROR PROCEDURE ON ANACLI.
IO-INI2.
        IF BUCKET-LOCKED 
           CALL "LIB$WAIT" USING BY REFERENCE IM-DELAY 
        ELSE
            MOVE RTC TO QQ-RTC
            MOVE "ANACLI" TO QQ-FILE
            MOVE ANOMAL TO NOME-PGM
            MOVE PUT_MODE TO IM-MODE
            MOVE QQ-BUFFER TO IM-RESTO
            CALL "XCGBUFF" USING IM-BUFFER 
            CALL "LIB$RUN_PROGRAM" USING BY DESCRIPTOR NOME-PROGRAMMA.
END-DECLARATIVES.
*************************************************************************************
MAIN-SECTION.
MAIN-010.
        PERFORM N010-INIZIO.
        PERFORM N200-ELABORA 
                UNTIL  END-N200 = HIGH-VALUE.
        PERFORM N400-FINE.
MAIN-999.
        EXIT.
*************************************************************************************
N010-INIZIO SECTION.
N010-010.
        ACCEPT GIRA-DATA FROM DATE.
        MOVE DT-GG TO WI-AA.
        MOVE DT-MM TO WI-MM.
        MOVE DT-AA TO WI-GG.
        CALL "FDV$ATERM USING BY DESCRIPTOR TERM_CONTROL_AREA
                              BY REFERENCE  TERM_CONTROL_AREA
                              BY DESCRIPTOR LOGICAL_UNIT_TT.
        CALL "FDV$AWKSP USING BY DESCRIPTOR WORKSPACE 
                              BY REFERENCE WORKSPACE_SIZE.
        CALL "FDV$LOPEN" USING BY DESCRIPTOR LIB-MAPPE
                              BY REFERENCE LOGICAL_UNIT_TT.
        MOVE SPACES      TO XX-MESS-MESS.
        MOVE "C O N F E R M A R E    " TO XX-MESS(1).
        MOVE "CODICE CLIENTE INESISTENTE " TO XX-MESS(2).
        MOVE "FIDO ERRATO                " TO XX-MESS(3).

        MOVE LOW-VALUE TO END-N200.
        MOVE SPACES TO WI-BUFFER.
        INITIALIZE WI-BUFFER REPLACING NUMERIC BY ZERO.

QUI.
    CALL "FDV$CLRSH" USING BY DESCRIPTOR NOME-MAPPA.
    PERFORM N799-OPEN.
    PERFORM N230-LEGGI-VIDEO.
N010-999.
    EXIT.


