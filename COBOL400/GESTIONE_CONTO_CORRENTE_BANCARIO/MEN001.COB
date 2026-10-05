       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEN001.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 WS-FINE              PIC X VALUE 'N'.

       01 SCELTA               PIC 9 VALUE 0.

       01 FUNZ                 PIC 99 VALUE 0.

       SCREEN SECTION.

       01 MENUFMT.
          05 BLANK SCREEN.
          05 LINE 02 COLUMN 30
             VALUE 'BANCA - MENU PRINCIPALE'.

          05 LINE 05 COLUMN 10
             VALUE '1 - Gestione Clienti'.

          05 LINE 06 COLUMN 10
             VALUE '2 - Gestione Conti'.

          05 LINE 07 COLUMN 10
             VALUE '3 - Gestione Movimenti'.

          05 LINE 09 COLUMN 10
             VALUE '4 - Conti con saldo negativo'.

          05 LINE 10 COLUMN 10
             VALUE '5 - Movimenti per tipo'.

          05 LINE 11 COLUMN 10
             VALUE '6 - Movimenti tra date'.

          05 LINE 12 COLUMN 10
             VALUE '7 - Movimenti di un conto'.

          05 LINE 13 COLUMN 10
             VALUE '8 - Movimenti conto/data'.

          05 LINE 15 COLUMN 10
             VALUE '9 - Fine'.

          05 LINE 18 COLUMN 10
             VALUE 'Scelta : '.

          05 LINE 18 COLUMN 20
             PIC 9
             USING SCELTA.

          05 LINE 24 COLUMN 02
             VALUE 'F3=Fine'.

       PROCEDURE DIVISION.

       MAIN.

           PERFORM UNTIL WS-FINE = 'S'

              MOVE 0 TO SCELTA

              DISPLAY MENUFMT

              ACCEPT MENUFMT

              ACCEPT FUNZ FROM ESCAPE KEY

              IF FUNZ = 3
                 MOVE 'S' TO WS-FINE
              ELSE

                 EVALUATE SCELTA

                    WHEN 1

                        CALL 'CLI001'

                    WHEN 2

                        CALL 'CNT001'

                    WHEN 3

                        CALL 'MOV001'

                    WHEN 4

                        CALL 'INT001'

                    WHEN 5

                        CALL 'INT002'

                    WHEN 6

                        CALL 'INT003'

                    WHEN 7

                        CALL 'INT004'

                    WHEN 8

                        CALL 'INT005'

                    WHEN 9

                        MOVE 'S'
                           TO WS-FINE

                    WHEN OTHER

                        DISPLAY
                           'SCELTA NON VALIDA'

                 END-EVALUATE

              END-IF

           END-PERFORM

           STOP RUN.
