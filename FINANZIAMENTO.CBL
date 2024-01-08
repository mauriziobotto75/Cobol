       IDENTIFICATION DIVISION.

       PROGRAM-ID.
           FINANZIAMENTO.

       AUTHOR.
           BOTTO.

       DATE-WRITTEN.
           27-11-2023.

       REMARKS.



       ENVIRONMENT  DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER.
           PC-HP.
       OBJECT-COMPUTER.
           PC-HP.

       INPUT-OUTPUT SECTION.
           FILE-CONTROL.



       DATA  DIVISION.

           FILE  SECTION.



       WORKING-STORAGE SECTION.

       01  WS-FINANCIAMENTO.
           03 WS-CLIENTE                   PIC X(20).
           03 WS-OBJETO                    PIC X(20).
           03 WS-VALOR                     PIC 9(6)V99.
           03 WS-PARCELAS                  PIC 9(05)V99 OCCURS 12 TIMES.

       01  WS-VARIAVEIS.
           03 WS-VR-PARCELAS               PIC 9(5)V99.
           03 WS-IND                       PIC 99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           INITIALIZE                WS-VARIAVEIS.
           DISPLAY 'INFORME O NOME DO CLIENTE:    '.
           ACCEPT  WS-CLIENTE.
           DISPLAY 'INFORME O OBJETO FINANCIADO:  '.
           ACCEPT WS-OBJETO.
           DISPLAY 'INFORME O VALOR DO OBJETO:    '.
           ACCEPT WS-VALOR.

           COMPUTE WS-VR-PARCELAS = WS-VALOR / 12.
           PERFORM UNTIL WS-IND EQUAL 12
               ADD 1 TO WS-IND
               MOVE WS-VR-PARCELAS TO WS-PARCELAS(WS-IND)
           END-PERFORM.

           STOP RUN.
       END PROGRAM FINANZIAMENTO.
