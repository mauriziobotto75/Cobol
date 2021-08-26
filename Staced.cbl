       identification division.
       program-id. STACED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370-HI45.
       OBJECT-COMPUTER. IBM-370-HI45.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CARTOR ASSIGN SYS005-UR-2540R-S.
           SELECT CEDOL  ASSIGN SYS006-UR-I403-S.
           SELECT ANOPIS ASSIGN SYS007-DA-3340-I-ANOPIS.
           RECORD KEY IS CHIAVE.
       input-output CONTROL.
           APPLY WRITE-VERIFY ON ANOPIS.
       DATA DIVISION.
       FILE SECTION.
       FD  CARTOR,
           LABEL RECORD OMITTED,    
           RECORDING MODE F,
           DATA RECORD CARTELLINO-OROLOGIO.
       01  CARTELLINO-OROLOGIO.
           02 I-DIPENDENZA              PIC X.
           02 I-NUM-CARTELLINO          PIC 9(5).
           02 CODICI-RICERCA.
              03 I-CODICE-REPARTO       PIC X(4).
              03 I-CODICE-PERSONALE     PIC X(4).
           02 I-DATA.
              03 I-MM                   PIC 99.
              03 I-AA                   PIC 99.
           02 I-QUALIFICA               PIC XX.
           02 I-ORE-LAVORATE.
              03 I-ORE-ORDINARIE        PIC 9(3)v9(2).
              03 I-ORE-ST-DIURNE        PIC 9(3)v9(2).
              03 I-ORE-ST-NOTTURNE      PIC 9(3)V9(2).
           02 ASSENZE-RETRIBUITE.
              03 I-ORE-MALATTIA         PIC 9(3)v9(2).
              03 I-ORE-FERIE            PIC 9(3).
           02 ASSENZE-NON-RETRIBUITE.
              03 I-ORE-SCIOPERO         PIC 9(3)v9(2).
              03 I-ORE-FERM-NCN-RET     PIC 9(3)V9(2).

       FD  CEDOL,
           LABEL RECORD OMITTED,
           DATA RECORD RIGA.
       01  RIGA.
           02 FILLER                    PIC X(11).
           02 TESTATA1                  PIC X(122).
           02 TESTATA2  REDEFINES  TESTATA1.
              03  FILLER                PIC X(13).
              03  O-NUM-PROG            PIC ZZZ9.
              03  FILLER                PIC X(6).
              03  O-NOMINATIVO          PIC X(25).
              03  FILLER                PIC X(7).
              03  0-CODICE-PERSONALE    PIC X(4).
              03  FILLER                PIC X(8).
              03  O-MESE                PIC A(9).
              03  FILLER                PIC XXX.
              03  O-ANNO.
                  04 SECOLO             PIC 99.
                  04 ANNO               PIC 99.
              03  FILLER                PIC X(39).
           02 TESTATA4 REDEFINES TESTATA2.
              03  FILLER                PIC X(44).
              03  O-PAGA-ORARIA         PIC Z,ZZ9.
              03  FILLER                PIC X(8).
              03  O-DIPENDENZA          PIC X.
              03  FILLER                PIC X(9).
              03  O-CODICE-REPARTO      PIC X(4).
              03  FILLER                PIC X(8).
              03  O-QUALIFICA           PIC XX.
              03  FILLER                PIC X(41).
           02 GENERICA REDEFINES TESTATA4.
              03  FILLER                PIC X(24).
              03  O-TRATTENUTE          PIC ZZ,ZZZ.
              03  FILLER                PIC X(10).
              03  O-COMPETENZE          PIC ZZZ,ZZZ.
              03  FILLER                PIC X(7).
              03  O-DESCRIZIONE         PIC X(26).
              03  FILLER                PIC X(42).
       
       FD  ANOPIS,
           LABEL RECORD STANDARD,
           RECORDING MODE F,
           BLOCK CONTAINS 21 RECORDS,
           DATA RECORD ANAG-OPERAI.

       01  ANAG-OPERAI.
           02  SITUAZIONE                PIC X.
           02  CHIAVE.
               03 CODICE-REPARTO         PIC X(4).
               03 CODICE-PERSONALE       PIC X(4).
           02  NOMINATIVO                PIC X(25).
           02  INDIRIZZO                 PIC X(26).
           02  DATA-NASCITA              PIC 9(8).    
           02  DATA-ASSUNZIONE           PIC 9(8).
           02  QUALIFICA                 PIC XX.
           02  DIPENDENZA                PIC X.
           02  FERIE                     PIC 9(6).
           02  CARICHI-FAMILIARI.
               03 FIGLI                  PIC 99.
               03 ASCENDENTI             PIC 9.
               03 STATO-CIVILE           PIC 9.
           02  MINIMO-CATEGORIA          PIC 9(4) COMP-3.
           02  CONTINGENZA               PIC 9(5) COMP-3.
           02  INDENNITA-MENSA           PIC 9(4) COMP-3.
           02  PAGA-ORARIA               PIC 9(4) COMP-3.
           02  PROGRESSIVO-RETRIB        PIC 9(8) COMP-3.
           02  FILLER                    PIC X(8).
       working-storage SECTION.
       77  CARATTERE-CONTROLLO           PIC X.
       77  NUM-CED                       PIC 99 VALUE 0.
       77  INDICE                        PIC 9 COMP VALUE 1.
       77  NUM-PROG                      PIC 9(3) COMP VALUE 1.
       77  NOMI-MSFI                     PIC X(108) VALUE 
                                         'GENNAIO FEBBRAIO MARZO APRILE 
                                         MAGGIO GIUGNO LUGLIO AGOSTO 
                                         SETTEMBRE OTTOBRE NOVEMBRE 
                                        DICEMBRE '.
       77  MM                            PIC 99.
       01  IND                           PIC 9 VALUE 1 SYNC.
       01  TABELLA-MESI.
           02 ELEM-TABELLA               OCCURS 12 TIMES.
              03 MESE                    PIC X(9).
       01  RIGHE-CEDOLINO SYNC.
           02 RIGA-CEDOL                 OCCURS 10 TIMES.
              03 TRATTENUTE              PIC 9(6).
              03 COMPETENZE              PIC 9(6).
              03 DESCRIZIONE             PIC X(26).

      
       PROCEDURE DIVISION.
           READY TRACE.
       INIZIO-ELABORAZIONE.
           OPEN INPUT CARTOR, OUTPUT CEDOL, I-O ANOPIS.
           MOVE NOMI-MESI TO TABELLA-MESI.
       LETTURA-CARTOR.
           READ CARTOR AT END GO TO FINE-CARTOR.
           IF NUM-CED = 10 GO TO LETTURA-CARTOR.
              MOVE I-MM TO MM.
       LETTURA-ANOPIS.
       A3. READ ANOPIS AT END GO TO FINE-ANOPIS.
       A4. IF SITUAZIONE = 'C' GO TO LETTURA-ANOPIS.
       A1. IF CHIAVE NOT = CODICI-RICERCA GO TO MESSAGGIO-ERRORE. 
       A2. CALL 'CALLCRET',
                USING CARTELLINO-OROLOGIO, ANAG-OPERAI, RIGHE-CEDOLINO, IND.
           REWRITE ANAG-OPERAI INVALID KEY STOP RUN.
       STAMPA-TESTATA.
           MOVE SPACE TO RIGA.
           MOVE '           N.PROGR.     COGNOME E NOME    PERSON   MESE ANNO'  TO  TESTATA1.
           WRITE RIGA AFTER POSITIONING ZERO.
           MOVE SPACES TO RIGA.
           MOVE NUM-PROG TO O-NUM-PROG.
           MOVE NOMINATIVO TO O-NOMINATIVO.
           MOVE CODICE-PERSONALE TO O-CODICE-PERSONALE.    
           MOVE MESE (MM) TO O-MESE.
           MOVE 20 SECOLO.
           MOVE I-AA TO ANAG.
           WRITE RIGA AFTER POSITIONING 2.
           MOVE SPACE TO RIGA.
           MOVE '  PAGA ORARIA    DIP.  COD. REP. QUALIF. '  TO TESTATA1.
           WRITE RIGA AFTER POSITIONING 2.
           MOVE SPACE TO RIGA.
           MOVE PAGA-ORARIA TO O-PAGA-ORARIA.
           MOVE DIPENDENZA TO O-DIPENDENZA.
           MOVE CODICE-REPARTO TO O-CODICE-REPARTO.
           MOVE QUALIFICA TO O-QUALIFICA.
           WRITE RIGA AFTER POSITIONING 2.
           MOVE SPACES TO RIGA.
           MOVE '   TRATTENUTE   COMPETENZE   DESCRIZIONE ' TO TESTATA1.
           MOVE '-' TO CARATTERE-CONTROLLO.
           WRITE RIGA AFTER POSITIONING 2.
           MOVE SPACES TO RIGA.
       
       STAMPA-RECORDS.
           MOVE TRATTENURE(INDICE) TO C-TRATTENUTE.
           MOVE COMPETENZE(INDICE) TO C-COMPETENZE.
           MOVE DESCRIZIONE(INDICE) TO C-DESCRIZIONE.
           WRITE RIGA AFTER POSITIONING 2.
           MOVE SPACE TO RIGA.
           IF INDICE = IND GO TO FINE-MODULO.
           ADD 1 TO INDICE.
           GO TO STAMPA-RECORDS.
       FINE-MODULO.
           MOVE 1 TO IND, INDICE.
           ADD 1 TO NUM-CED, NUM-PROG.
           GO TO LETTURA-CARTOR.
       FINE-CARTOR.
           GO TO FINE-LAVORO.
       FINE-ANOPIS.
           DISPLAY 'ERRORE NEI CODICI' UPON CONSOLE.
           GO TO FINE-LAVORO.
       FINE-LAVORO.
           CLOSE CEDOL, CARTOR, ANOPIS.
           STOP RUN.
