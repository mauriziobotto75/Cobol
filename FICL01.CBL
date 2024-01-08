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
 


