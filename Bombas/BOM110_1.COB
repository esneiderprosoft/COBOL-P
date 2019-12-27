000012* CREA JSON PARA GENERAR LISTA DE COMBUSTIBLES
000020 IDENTIFICATION DIVISION.
000021* PROGRAM-ID. "BOM110_1".
000022
000023 PROGRAM-ID. "HttpExtensionProc".
000024 ENVIRONMENT DIVISION.
000025 CONFIGURATION SECTION.
000026 INPUT-OUTPUT SECTION.
000027 FILE-CONTROL.
000080 COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
000150 COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".


           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-USUAR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-COMBUSTIBLES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-COMBU-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-COMB
                  ALTERNATE RECORD KEY IS FECHA-COMB WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


000349     SELECT MAESTRO-ARTICULOS LOCK MODE IS AUTOMATIC
000350            ASSIGN NOM-ART-W
000351            ORGANIZATION INDEXED;
000352            ACCESS MODE  DYNAMIC;
000353            RECORD KEY COD-ART
000354            ALTERNATE RECORD KEY CLASE-ART       WITH DUPLICATES
000355            ALTERNATE RECORD KEY COD-BARRAS-ART  WITH DUPLICATES
000356            ALTERNATE RECORD KEY DESCRIP-ART     WITH DUPLICATES
000357            ALTERNATE RECORD KEY USO-ART         WITH DUPLICATES
000358            ALTERNATE RECORD KEY CL-ROTAC-ART    WITH DUPLICATES
000359            ALTERNATE RECORD KEY UBICAC-ART      WITH DUPLICATES
000360            ALTERNATE RECORD KEY FECHA-VENCE-ART WITH DUPLICATES
000361            FILE STATUS IS OTR-STAT.


           SELECT MOVIMIENTO-INVENT LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-INVENT-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-INV
                  ALTERNATE RECORD KEY COD-ART-INV   WITH DUPLICATES
                  ALTERNATE RECORD KEY SECUENCIA-INV
                  ALTERNATE RECORD KEY NIT-INV           WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-INV         WITH DUPLICATES
                  ALTERNATE RECORD KEY LLAVE-DOC-CTL-INV WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-SALDOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-SALDOS-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-SAL
                  ALTERNATE RECORD KEY COD-ART-SAL  WITH DUPLICATES
                  ALTERNATE RECORD KEY COD-LOTE-SAL WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.



005030     SELECT ARCHIVO-JSON
005040            ASSIGN NOM-PLANO-W
005050            ORGANIZATION IS LINE SEQUENTIAL
005060            FILE STATUS IS OTR-STAT.



000400 DATA DIVISION.
000410 FILE SECTION.
000420 COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
000421 COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
000450 COPY "P:\PROG\PROYECT\FUENTES\FD-USUAR.CBL".
000450 COPY "P:\PROG\PROYECT\FUENTES\FD-COMBU.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-ARTIC.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-INVEN.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SALDO.CBL".

005140 FD  ARCHIVO-JSON
005150     LABEL RECORD STANDARD.
005160 01  REG-PLANO.
005170     02 DATOS-PLANO     PIC X(500).

000540 WORKING-STORAGE  SECTION.
000550 COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
000560 COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".


000570 77 NOM-USUAR-W            PIC X(70).
       77 NOM-COMBU-W            PIC X(70).
       77 NOM-PLANO-W            PIC X(90).
       77 NOM-ART-W              PIC X(70).
       77 NOM-INVENT-W           PIC X(70).
       77 NOM-SALDOS-W           PIC X(70).
       77 GALONAJE-W             PIC ZZZZZZZZZZ.ZZZ.
       77 DESCTO-W               PIC ZZZZZZZZZZZZ.ZZZ.
       77 FECHA-SEC              PIC 9(6).
       77 FECHA-FIN              PIC 9(6).
       77 CONT                   PIC 99.
       77 SW-ITEM                PIC 9.
       77 SW-TAB                 PIC 9.
       77 MES-ACT-W              PIC X(3).
       77 DIRECTORIO-W           PIC X(50).
       77 MENSAJE-LNK            PIC X(30).
       77 INVALID-INV            PIC 99. 
       77  SDO-ACT-W             PIC S9(12)V999.

       01  FECHA-SIG.
           02 ANO-SIG        PIC 9(4).
           02 MES-SIG        PIC 99.
           02 DIA-SIG        PIC 99.


       01  LLAVE-W.
           02  LOCAL-W      PIC X(5).
           02  CTA-W.
               05 TIPO-W    PIC 9.
               05 GRUPO-W   PIC XX.
               05 NUMERO-W  PIC X(13).
               05 CLASE-W   PIC XX.
           02  COD-LOTE-W   PIC 9(9).



       01 VARIABLES.
          02 NUMER-W             PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ.
          02 CANT-W              PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZZ-.
          02 VALOR-W             PIC ZZZ,ZZZ,ZZZ,ZZZ.
          02 CANT-TOT            PIC S9(13)V999.
          02 VENTA-TOT           PIC S9(13).
          02 TASA-TOT            PIC S9(13).
          02 GLOB-TOT            PIC S9(12).
          02 TOTAL-TOT           PIC S9(13).
          02 TOTAL-CHEQUE        PIC S9(13).
          02 TOTAL-W             PIC S9(13).
          02 TOTAL-VAL           PIC S9(13).
          02 TOTAL-FIN           PIC S9(13).
          02 TOTAL-RBO           PIC S9(13).
          02 TABLA-W.
             03 ACUM-ART-W OCCURS 5.
                05 COD-ART-TAB.
                   07 GRP-ART-TAB    PIC XX.
                   07 NRO-ART-TAB    PIC X(16).
                05 CANT-ART-TAB      PIC S9(12)V999.
                05 VLR-ART-TAB       PIC S9(12).
          02 TOTAL-CANT-W   PIC S9(13)V999.
          02 TOTAL-VENT-W   PIC S9(13)V999.
          02 TOTAL-SALD-W   PIC S9(13)V999.

       01 VAL-ART-W              PIC Z,ZZZ,ZZZ,ZZZ,ZZZ.ZZZ.
       01 ANO-FIN                PIC 99.


       01 FECHA-W.
          02   ANO-W             PIC 99.
          02   MES-W             PIC 99.
          02   DIA-W             PIC 99.


       01 FECHA-EDIT             PIC X(10).

       01 VALOR-GLOBAL-W         PIC ZZZZZZZZZZZZ.ZZ.

       01 LINEA-LLEGAD-W             PIC X(100).

       01 DATO-LLEGADA-W.
          02 SESION-LLEGAD-W        PIC X(29).
          02 DIRECTORIO-LLEGAD-W    PIC X(30).
          02 CARPTA-LLEGAD-W        PIC X(9).
          02 FECHA-INI-LLEGAD-W.
             03 ANO-INI-W              PIC XX.
             03 MES-INI-W              PIC XX.
             03 DIA-INI-W              PIC XX.
          02 FECHA-FIN-LLEGAD-W.
             03 ANO-FIN-W              PIC XX.
             03 MES-FIN-W              PIC XX.
             03 DIA-FIN-W              PIC XX.
          02 TURNO-LLEGAD-W            PIC X.
          02 SUC-LLEGAD-W              PIC X.


       01 LIN-1.
          02 FILLER                  PIC X(7)   VALUE "{*NRO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LLAVE-COMB-J            PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*OPER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OPER-COMB-J             PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*FECHA-REST*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FEC-REST-COMB-J         PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(8)   VALUE "*FECHA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-COMB-J            PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(8)   VALUE "*SURTI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SURTIDOR-COMB-J         PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(8)   VALUE "*ARTIC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ARTICULO-COMB-J         PIC X(18).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*CANTID*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CANTIDAD-COMB-J         PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*VALOR*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VALOR-COMB-J            PIC X(16).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*SOBRET*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SOBRET-COMB-J           PIC X(16).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*GLOBAL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 GLOBAL-COMB-J           PIC X(13).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*NUM-ANT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NUM-ANT-COMB-J          PIC X(18).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*NUM-ACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NUM-ACT-COMB-J          PIC X(18).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*TOTAL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TOTAL-J                 PIC X(20).
003710    02 FILLER                  PIC XX     VALUE "*}".
005040    02 CIERRE-LIN-1            PIC X.



       01 LIN-2.
          02 FILLER                  PIC X(7)   VALUE "{*ART*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-ART-J               PIC X(18).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-ART-J           PIC X(50).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*CANT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CANT-ART-TAB-J          PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*VLR*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VLR-ART-TAB-J           PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*TOTAL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TOTAL-SALD-J            PIC X(20).
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-LIN-2            PIC X.



       01 DATOS-ENVIO.
          02 DAT0-ENV            PIC XX.
          02 FILLER              PIC X VALUE "|".
          02 CANT-VENT-ENV       PIC X(20).
          02 FILLER              PIC X VALUE "|".
          02 VALOR-VENT-ENV      PIC X(20).
          02 FILLER              PIC X VALUE "|".
          02 SOBRET-ENV          PIC X(20).
          02 FILLER              PIC X VALUE "|".
          02 GLOBAL-ENV          PIC X(20).
          02 FILLER              PIC X VALUE "|".
          02 SUBTOTAL-ENV        PIC X(20).
          02 FILLER              PIC X VALUE "|".
          02 TOTAL-VALES         PIC X(20).
          02 FILLER              PIC X VALUE "|".
          02 VALOR-FINANC-ENV    PIC X(20).
          02 FILLER              PIC X VALUE "|".
          02 NETO-RECAUDO-ENV    PIC X(20).
          02 FILLER              PIC X VALUE "|".
          02 TOTAL-RBO-ENV       PIC X(20).
          02 FILLER              PIC X VALUE "|".
          02 TTAL-CANT-ART-ENV   PIC X(21).
          02 FILLER              PIC X VALUE "|".
          02 TOTAL-ART-ENV       PIC X(21).
          02 FILLER              PIC X VALUE "|".
          02 TTAL-SALD-ENV       PIC X(21).


000791 LINKAGE SECTION.
000792 COPY "P:\PROG\PROYECT\FUENTES\ISAPICTX.CBL".
000793
000794 PROCEDURE DIVISION WITH stdcall LINKAGE USING ISAPI-CTX-CNT.
000795
000810 DECLARATIVES.
000811 I-O-TEST SECTION.
000812     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUNET.         
000813 ESCR-EXCEPTIONES.
000814     IF OTR-STAT = "00"
000815        CONTINUE 
000816     ELSE
000817        MOVE OTR-STAT                TO MSJ1-HTML
000818        MOVE NOM-USU-W               TO MSJ2-HTML
000819        MOVE "BOM110_1"              TO MSJ3-HTML
000820        GO TO ENVIAR2-ERROR
000821     END-IF.
001030
001040 I-O-TEST SECTION.
001050     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.         
001060 ESCR-EXCEPTIONES.
001070     IF OTR-STAT = "00"
001080        CONTINUE
001090     ELSE
001100        MOVE OTR-STAT                TO MSJ1-HTML
001101        MOVE NOM-SESION-W            TO MSJ2-HTML
001102        MOVE "BOM110_1"              TO MSJ3-HTML
001103        GO TO ENVIAR2-ERROR
001104     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" 
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-USUAR-W      TO MSJ2-HTML
006680        MOVE "BOM110_1"       TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-COMBUSTIBLES.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" 
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-COMBU-W      TO MSJ2-HTML
006680        MOVE "BOM110_1"       TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

002694 I-O-TEST SECTION.
002700     USE AFTER EXCEPTION PROCEDURE ON MAESTRO-ARTICULOS.
002710 ESCR-EXCEPTIONES.
002720     IF OTR-STAT = "00"
002730        CONTINUE 
002740     ELSE
002750        MOVE OTR-STAT         TO MSJ1-HTML
002760        MOVE NOM-ART-W        TO MSJ2-HTML
002770        MOVE "BOM110_1"       TO MSJ3-HTML
002780        GO TO MOSTRAR-ERROR
002790     END-IF.

002694 I-O-TEST SECTION.
002700     USE AFTER EXCEPTION PROCEDURE ON MOVIMIENTO-INVENT.
002710 ESCR-EXCEPTIONES.
002720     IF OTR-STAT = "00"
002730        CONTINUE 
002740     ELSE
002750        MOVE OTR-STAT         TO MSJ1-HTML
002760        MOVE NOM-INVENT-W     TO MSJ2-HTML
002770        MOVE "BOM110_1"       TO MSJ3-HTML
002780        GO TO MOSTRAR-ERROR
002790     END-IF.

002694 I-O-TEST SECTION.
002700     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SALDOS.
002710 ESCR-EXCEPTIONES.
002720     IF OTR-STAT = "00"
002730        CONTINUE 
002740     ELSE
002750        MOVE OTR-STAT         TO MSJ1-HTML
002760        MOVE NOM-SALDOS-W     TO MSJ2-HTML
002770        MOVE "BOM110_1"       TO MSJ3-HTML
002780        GO TO MOSTRAR-ERROR
002790     END-IF.


006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" OR "35"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-PLANO-W      TO MSJ2-HTML
006680        MOVE "BOM110_1"       TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

001150 END DECLARATIVES.
001160
001161 INICIAR-IIS.
001162     MOVE LOW-VALUE TO COBW3.
001163     MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
001164     CALL "COBW3_INIT" USING COBW3.
001220
001221 LEER-DATO-HTML.
001852     MOVE "datosh" TO COBW3-SEARCH-DATA.
001853     CALL "COBW3_GET_VALUE" USING COBW3.
001854     MOVE COBW3-GET-DATA    TO LINEA-LLEGAD-W.

           INITIALIZE DATO-LLEGADA-W

           UNSTRING LINEA-LLEGAD-W DELIMITED BY "|"
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W, FECHA-INI-LLEGAD-W, FECHA-FIN-LLEGAD-W, TURNO-LLEGAD-W, SUC-LLEGAD-W
           END-UNSTRING.

001855     MOVE SESION-LLEGAD-W    TO LLAVE-SESION-W.

001238 INICIAR-SESION.
001245     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
001246
000820     MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
000830     ACCEPT HORA-TOTAL FROM TIME.
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.

001303 ABRIR-USUARIO.
001304     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
001305
001306     OPEN INPUT ARCHIVO-USUNET
001307     EVALUATE OTR-STAT
001308       WHEN "00"  CONTINUE
001309       WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
001310                  CLOSE       ARCHIVO-USUNET
001311                  OPEN I-O    ARCHIVO-USUNET
001312                  INITIALIZE REG-USUNET
001314                  WRITE REG-USUNET
001315                  CLOSE       ARCHIVO-USUNET
001316                  MOVE "Validacion de usuarios"      TO MSJ1-HTML
001317                  MOVE "Falta configurar usuario"    TO MSJ2-HTML
001318                  MOVE "Sc"                          TO MSJ3-HTML
001319                  GO TO ENVIAR2-ERROR
001320       WHEN OTHER GO TO CERRAR-SESION
001321     END-EVALUATE.
001322
001332     INITIALIZE REG-USUNET.

007110 LEER-USUARIO.
007120     READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
007130
007140     CLOSE ARCHIVO-USUNET.
007200     
007210     IF NOMBRE-USUNET = SPACES
007220     OR NIT-USUNET = ZEROS
007230        MOVE "Validacion de Sesion"        TO MSJ1-HTML
007240        MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007250        MOVE "Sc"                          TO MSJ3-HTML
007260        GO TO ENVIAR2-ERROR
007270     END-IF.

       INICIAR-RESTRICION.

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
001614     INITIALIZE NOM-USUAR-W NOM-COMBU-W NOM-PLANO-W NOM-ART-W
001615     MOVE "\\" TO NOM-USUAR-W
001616
001617     INSPECT NOM-USUAR-W REPLACING FIRST "                    "
001618                                    BY IP-DATOS-USUNET
001621     INSPECT NOM-USUAR-W REPLACING FIRST " "
001624                                     BY "\"
007292     INSPECT NOM-USUAR-W REPLACING FIRST "                              "
007293                                     BY DIRECTORIO-LLEGAD-W

           MOVE NOM-USUAR-W  TO NOM-COMBU-W NOM-ART-W DIRECTORIO-W

           INSPECT NOM-USUAR-W  REPLACING FIRST "                       "
                                             BY "\CONTROL\SC-ARCHUSU.DAT".

           INSPECT NOM-COMBU-W  REPLACING FIRST "                       "
                                             BY "\CONTROL\SC-ARCOMBU.DAT".

           INSPECT NOM-ART-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-MAESART.DAT".


           INSPECT NOM-INVENT-W REPLACING FIRST "         "
                                             BY CARPTA-LLEGAD-W.

           MOVE NOM-INVENT-W    TO NOM-SALDOS-W

           INSPECT NOM-INVENT-W REPLACING FIRST "             "
                                             BY "SC-MOVINV.DAT".


           INSPECT NOM-SALDOS-W REPLACING FIRST "            "
                                             BY "SC-SALDO.DAT".

           MOVE "C:\PROSOFT\TEMP\SC-LISTCOMB-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON" TO  NOM-PLANO-W

           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                             BY SESION-LLEGAD-W.

       LEER-USUARIO.

           OPEN INPUT ARCHIVO-USUARIOS
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.


       LEER-COMBUSTIBLES.
           OPEN INPUT ARCHIVO-COMBUSTIBLES.

           MOVE FECHA-INI-LLEGAD-W     TO FECHA-COMB

           START ARCHIVO-COMBUSTIBLES KEY >= FECHA-COMB
                 INVALID KEY 
                         MOVE "99"       TO MSJ1-HTML
                         MOVE "8"        TO MSJ2-HTML
                         MOVE "BOM110_1" TO MSJ3-HTML
                         GO TO ENVIAR2-ERROR
           END-START.


       ABRIR-PLANO.

           OPEN OUTPUT ARCHIVO-JSON.

           INITIALIZE REG-PLANO VARIABLES.
           MOVE "{*LISTADO*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.


       LLENAR-PLANO.

           READ ARCHIVO-COMBUSTIBLES NEXT WITH NO LOCK AT END GO TO TOTAL-LISTADO.

           IF SUCURSAL-COMB IS NOT NUMERIC
              MOVE 1 TO SUCURSAL-COMB
           END-IF

           IF SUCURSAL-COMB = SUC-LLEGAD-W
              CONTINUE
           ELSE
              GO TO LLENAR-PLANO
           END-IF

           IF FECHA-COMB > FECHA-FIN-LLEGAD-W GO TO TOTAL-LISTADO.

           IF  TURNO-LLEGAD-W = "*"
               CONTINUE
           ELSE
               IF TURNO-LLEGAD-W = TURNO-COMB
                  CONTINUE
               ELSE
                 GO TO LLENAR-PLANO
               END-IF
           END-IF.


       RUTINA-MOVIMIENTO.

           MOVE 0 TO SW-ITEM
           PERFORM LEER-TABLA VARYING I FROM 1 BY 1
                              UNTIL I > 15.

           PERFORM SUMAR-VALES VARYING I FROM 1 BY 1
                               UNTIL I > 200.

           ADD VALOR-FINANC-COMB TO TOTAL-FIN.
           ADD VALOR-CHEQUE      TO TOTAL-CHEQUE.

           GO TO LLENAR-PLANO.

       TOTAL-LISTADO.

           INITIALIZE CIERRE-LIN-1 DATOS-ENVIO
008890     MOVE LIN-1 TO DATOS-PLANO
008900     WRITE REG-PLANO.
008910
008920     MOVE "]" TO DATOS-PLANO
008930     WRITE REG-PLANO.


           MOVE 00           TO DAT0-ENV
           MOVE CANT-TOT     TO CANT-W
           MOVE CANT-W       TO CANT-VENT-ENV

           MOVE VENTA-TOT    TO VALOR-W
           MOVE VALOR-W      TO VALOR-VENT-ENV

           MOVE TASA-TOT     TO VALOR-W
           MOVE VALOR-W      TO SOBRET-ENV

           MOVE GLOB-TOT     TO VALOR-W
           MOVE VALOR-W      TO GLOBAL-ENV

           MOVE TOTAL-TOT    TO VALOR-W
           MOVE VALOR-W      TO SUBTOTAL-ENV.

       LINEA-VALES.

           MOVE TOTAL-VAL     TO VALOR-W
           MOVE VALOR-W       TO TOTAL-VALES.

       LINEA-FINANC.

           MOVE TOTAL-FIN     TO VALOR-W
           MOVE VALOR-W       TO VALOR-FINANC-ENV.

       LINEA-RBO.

           MOVE TOTAL-CHEQUE  TO VALOR-W
           MOVE VALOR-W       TO NETO-RECAUDO-ENV


           COMPUTE TOTAL-RBO =
                   TOTAL-TOT - TOTAL-VAL + TOTAL-FIN - TOTAL-CHEQUE.


           MOVE TOTAL-RBO     TO VALOR-W
           MOVE VALOR-W       TO TOTAL-RBO-ENV.

           INITIALIZE REG-PLANO I.
           MOVE ",*LINEA-ART*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.
           OPEN INPUT MAESTRO-ARTICULOS.

           PERFORM LINEA-ART VARYING I FROM 1 BY 1
                                    UNTIL   I > 5.

           MOVE TOTAL-CANT-W  TO VAL-ART-W
           MOVE VAL-ART-W     TO TTAL-CANT-ART-ENV

           MOVE TOTAL-VENT-W  TO VAL-ART-W
           MOVE VAL-ART-W     TO TOTAL-ART-ENV

           MOVE TOTAL-SALD-W  TO VAL-ART-W
           MOVE VAL-ART-W     TO TTAL-SALD-ENV.




       CERRAR-ARCHIVO.
           INITIALIZE CIERRE-LIN-2
008890     MOVE LIN-2 TO DATOS-PLANO
008900     WRITE REG-PLANO.
008910
008920     MOVE "]}" TO DATOS-PLANO
008930     WRITE REG-PLANO.

           CLOSE ARCHIVO-COMBUSTIBLES
                 MAESTRO-ARTICULOS
                 ARCHIVO-JSON.


002147 ENVIO-DATOS.
004378     MOVE "datosrecibidos"  TO COBW3-CNV-NAME
004393     MOVE DATOS-ENVIO       TO COBW3-CNV-VALUE
004403     CALL "COBW3_SET_CNV"   USING COBW3
004413     MOVE "../PAGINAS/RECIBIDOS.ASPX"  TO SALIDA-HTML
004430     PERFORM ABRIR-HTML.

002545 CERRAR-SESION.
002546     CALL "COBW3_FREE" USING COBW3.
002547     MOVE 1 TO PROGRAM-STATUS.
002548     EXIT PROGRAM.
002549

       LEER-TABLA.

           IF VALOR-COMB (I) > 0
              MOVE LLAVE-COMB           TO LLAVE-COMB-J
              MOVE SURTIDOR-COMB (I)    TO SURTIDOR-COMB-J

              IF SW-ITEM IS ZERO
                 MOVE 1 TO SW-ITEM
                 MOVE OPER-COMB         TO OPER-COMB-J
                 MOVE FEC-REST-COMB     TO FEC-REST-COMB-J
              ELSE
                 INITIALIZE OPER-COMB-J FEC-REST-COMB-J
              END-IF

              PERFORM EDITAR-FECHA

              MOVE FECHA-EDIT           TO FECHA-COMB-J
              MOVE ARTICULO-COMB (I)    TO ARTICULO-COMB-J

              MOVE NUM-ANT-COMB (I)     TO NUMER-W
              MOVE NUMER-W              TO NUM-ANT-COMB-J

              MOVE NUM-ACT-COMB (I)     TO NUMER-W
              MOVE NUMER-W              TO NUM-ACT-COMB-J

              MOVE CANTIDAD-COMB (I)    TO CANT-W
              MOVE CANT-W               TO CANTIDAD-COMB-J

              MOVE VALOR-COMB (I)       TO VALOR-W
              MOVE VALOR-W              TO VALOR-COMB-J

              MOVE SOBRET-COMB (I)      TO VALOR-W
              MOVE VALOR-W              TO SOBRET-COMB-J

              MOVE GLOBAL-COMB (I)      TO VALOR-W
              MOVE VALOR-W              TO GLOBAL-COMB-J


              COMPUTE TOTAL-W ROUNDED = VALOR-COMB  (I)
                                       + SOBRET-COMB (I)
                                       + GLOBAL-COMB (I)


              MOVE TOTAL-W     TO VALOR-W
              MOVE VALOR-W     TO TOTAL-J

              MOVE ","               TO CIERRE-LIN-1
              INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA

              MOVE LIN-1 TO DATOS-PLANO
              WRITE REG-PLANO END-WRITE

              ADD CANTIDAD-COMB (I) TO CANT-TOT
              ADD VALOR-COMB    (I) TO VENTA-TOT
              ADD SOBRET-COMB   (I) TO TASA-TOT
              ADD GLOBAL-COMB   (I) TO GLOB-TOT
              ADD TOTAL-W           TO TOTAL-TOT

              MOVE 0 TO SW-TAB
              PERFORM UBICAR-ARTICULO VARYING J FROM 1 BY 1
                                      UNTIL J > 5 OR SW-TAB = 1
              IF SW-TAB = 0
                 PERFORM ASIGNAR-ARTICULO VARYING J FROM 1 BY 1
                                          UNTIL J > 5 OR SW-TAB = 1
              END-IF
           END-IF.

       UBICAR-ARTICULO.
              IF ARTICULO-COMB (I) = COD-ART-TAB (J)
                 ADD CANTIDAD-COMB (I) TO CANT-ART-TAB (J)
                 ADD TOTAL-W           TO VLR-ART-TAB  (J)
                 MOVE 1 TO SW-TAB
              END-IF.


       ASIGNAR-ARTICULO.
              IF COD-ART-TAB (J) = SPACES 
                 MOVE ARTICULO-COMB (I) TO COD-ART-TAB  (J)
                 MOVE CANTIDAD-COMB (I) TO CANT-ART-TAB (J)
                 MOVE TOTAL-W           TO VLR-ART-TAB  (J)
                 MOVE 1 TO SW-TAB
              END-IF.

       SUMAR-VALES.
              ADD VLR-DEU-COMB (I) TO TOTAL-VAL.


       LINEA-ART.
           IF COD-ART-TAB (I) = SPACES OR LOW-VALUES
              CONTINUE
           ELSE
              PERFORM LLENAR-LIN-15
           END-IF.

       LLENAR-LIN-15.

           MOVE 0               TO TIPO-ART.
           MOVE GRP-ART-TAB (I) TO GRUPO-ART.
           MOVE NRO-ART-TAB (I) TO NUMERO-ART.
           MOVE SPACES          TO CLASE-ART.
           READ MAESTRO-ARTICULOS WITH NO LOCK
                INVALID KEY 
                        MOVE COD-ART TO DESCRIP-ART
           END-READ

           MOVE COD-ART            TO COD-ART-J
           MOVE DESCRIP-ART        TO DESCRIP-ART-J

           MOVE CANT-ART-TAB (I)   TO CANT-W
           MOVE CANT-W             TO CANT-ART-TAB-J


           MOVE VLR-ART-TAB (I)    TO VALOR-W
           MOVE VALOR-W            TO VLR-ART-TAB-J

           MOVE ANO-FIN-W          TO ANO-FIN
           MOVE "ALM01"            TO COD-ALM-SAL.
           MOVE COD-ART            TO COD-ART-SAL.
           COMPUTE ANO-SIG = ANO-FIN + 2000.
           MOVE MES-FIN-W           TO MES-SIG.
           MOVE DIA-FIN-W           TO DIA-SIG.
           MOVE ZEROS               TO COD-LOTE-W.

           CALL "INV808F" USING LLAVE-SAL SDO-ACT-W FECHA-SIG DIRECTORIO-W MENSAJE-LNK INVALID-INV

           IF INVALID-INV > 0
              MOVE INVALID-INV     TO MSJ1-HTML
              MOVE MENSAJE-LNK     TO MSJ2-HTML
              MOVE "BOM110_1"      TO MSJ3-HTML 
              GO TO ENVIAR2-ERROR
           END-IF

           MOVE SDO-ACT-W          TO CANT-W
           MOVE CANT-W             TO TOTAL-SALD-J


           ADD CANT-ART-TAB(I) TO TOTAL-CANT-W
           ADD VLR-ART-TAB(I)  TO TOTAL-VENT-W.
           ADD SDO-ACT-W       TO TOTAL-SALD-W.


           MOVE ","               TO CIERRE-LIN-2
           INSPECT LIN-2 REPLACING ALL "*" BY CARAC-COMILLA

           MOVE LIN-2 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE.



       EDITAR-FECHA.
           EVALUATE MES-COMB
           WHEN 01 MOVE "ENE DD/AA" TO FECHA-EDIT
           WHEN 02 MOVE "FEB DD/AA" TO FECHA-EDIT
           WHEN 03 MOVE "MAR DD/AA" TO FECHA-EDIT
           WHEN 04 MOVE "ABR DD/AA" TO FECHA-EDIT
           WHEN 05 MOVE "MAY DD/AA" TO FECHA-EDIT
           WHEN 06 MOVE "JUN DD/AA" TO FECHA-EDIT
           WHEN 07 MOVE "JUL DD/AA" TO FECHA-EDIT
           WHEN 08 MOVE "AGT DD/AA" TO FECHA-EDIT
           WHEN 09 MOVE "SEP DD/AA" TO FECHA-EDIT
           WHEN 10 MOVE "OCT DD/AA" TO FECHA-EDIT
           WHEN 11 MOVE "NOV DD/AA" TO FECHA-EDIT
           WHEN 12 MOVE "DIC DD/AA" TO FECHA-EDIT.
           INSPECT FECHA-EDIT REPLACING FIRST "DD" BY DIA-COMB.
           INSPECT FECHA-EDIT REPLACING FIRST "AA" BY ANO-COMB.


002566 COPY "P:\PROG\PROYECT\FUENTES\SC-WEB.CBL".
002567
