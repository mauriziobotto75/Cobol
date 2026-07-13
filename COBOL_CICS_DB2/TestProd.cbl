       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROD.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       01 SW-FINE               PIC X VALUE 'N'.
          88 EOF-PRODOTTI       VALUE 'S'.

       01 WS-CODICE-RICERCA     PIC X(5).

       01 WS-PRODOTTO.
          05 WS-CODICE-PRODOTTO PIC X(5).
          05 WS-DESCRIZIONE     PIC X(30).
          05 WS-PREZZO          PIC S9(6)V99 COMP-3.
          05 WS-SCORTA-MINIMA   PIC S9(3) COMP-3.

      ********************************************************
      * CURSORE
      ********************************************************

       EXEC SQL
            DECLARE C-PRODOTTI CURSOR FOR
            SELECT CODICE_PRODOTTO,
                   DESCRIZIONE,
                   PREZZO,
                   SCORTA_MINIMA
              FROM PRODOTTI
             WHERE CODICE_PRODOTTO >= :WS-CODICE-RICERCA
             ORDER BY CODICE_PRODOTTO
       END-EXEC.

       PROCEDURE DIVISION.

       MAIN.

      ********************************************************
      * IN CICS NORMALMENTE IL CODICE ARRIVA DA UNA MAPPA BMS
      ********************************************************

           MOVE 'A0001'
             TO WS-CODICE-RICERCA

      ********************************************************
      * LETTURA SINGOLO PRODOTTO
      ********************************************************

           EXEC SQL
                SELECT CODICE_PRODOTTO,
                       DESCRIZIONE,
                       PREZZO,
                       SCORTA_MINIMA
                  INTO :WS-CODICE-PRODOTTO,
                       :WS-DESCRIZIONE,
                       :WS-PREZZO,
                       :WS-SCORTA-MINIMA
                  FROM PRODOTTI
                 WHERE CODICE_PRODOTTO =
                       :WS-CODICE-RICERCA
           END-EXEC

           EVALUATE SQLCODE

              WHEN 0

                   DISPLAY 'PRODOTTO TROVATO'

                   DISPLAY 'CODICE      : '
                           WS-CODICE-PRODOTTO

                   DISPLAY 'DESCRIZIONE : '
                           WS-DESCRIZIONE

                   DISPLAY 'PREZZO      : '
                           WS-PREZZO

                   DISPLAY 'SCORTA MIN. : '
                           WS-SCORTA-MINIMA

              WHEN +100

                   DISPLAY 'PRODOTTO NON TROVATO'

              WHEN OTHER

                   DISPLAY 'ERRORE DB2'
                   DISPLAY 'SQLCODE = '
                           SQLCODE

           END-EVALUATE

      ********************************************************
      * LETTURA SEQUENZIALE
      ********************************************************

           EXEC SQL
                OPEN C-PRODOTTI
           END-EXEC

           IF SQLCODE NOT = 0
              DISPLAY 'ERRORE OPEN CURSORE'
              DISPLAY SQLCODE
              GO TO FINE-PROGRAMMA
           END-IF

           MOVE 'N' TO SW-FINE

           PERFORM UNTIL EOF-PRODOTTI

              EXEC SQL
                   FETCH C-PRODOTTI
                     INTO :WS-CODICE-PRODOTTO,
                          :WS-DESCRIZIONE,
                          :WS-PREZZO,
                          :WS-SCORTA-MINIMA
              END-EXEC

              EVALUATE SQLCODE

                 WHEN 0

                    DISPLAY
                    '----------------------'

                    DISPLAY
                    WS-CODICE-PRODOTTO
                    ' '
                    WS-DESCRIZIONE

                 WHEN +100

                    SET EOF-PRODOTTI
                        TO TRUE

                 WHEN OTHER

                    DISPLAY
                    'ERRORE FETCH'

                    DISPLAY
                    SQLCODE

                    SET EOF-PRODOTTI
                        TO TRUE

              END-EVALUATE

           END-PERFORM

           EXEC SQL
                CLOSE C-PRODOTTI
           END-EXEC.

       FINE-PROGRAMMA.

           EXEC CICS RETURN
           END-EXEC.
