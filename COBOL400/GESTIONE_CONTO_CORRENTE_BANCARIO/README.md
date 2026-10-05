# Gestione conto corrente bancario

Gli otto sorgenti COBOL sono mantenuti con estensione `.COB`; gli eseguibili locali vengono prodotti in `build/` e sono esclusi dal repository.

In questo ambiente, `Compile.bat` puntava a `C:\Programmi\GnuCobol 3.2\bin`, che non è installato. La compilazione è stata eseguita con una copia temporanea del batch configurata per l'OpenCOBOL 1.1.0 effettivamente installato. Su questo compilatore `MEN001.COB` segnala che `ACCEPT ... FROM ESCAPE KEY` non è implementato: il file viene compilato, ma quel comportamento non è supportato a runtime.

`INT002.COB` e `INT003.COB` distinguono ora i nomi degli archivi indicizzati dai campi omonimi dei record; `INT003.COB` termina inoltre correttamente con `STOP RUN.`
