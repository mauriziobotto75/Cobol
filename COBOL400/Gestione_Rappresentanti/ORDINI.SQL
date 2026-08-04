CREATE TABLE ORDINI
(
    NUMORD       INTEGER      NOT NULL,
    DATAORD      DATE,
    CODCLI       CHAR(5),
    CODRAP       CHAR(5),
    STATO        CHAR(1),

    PRIMARY KEY (NUMORD)
);
