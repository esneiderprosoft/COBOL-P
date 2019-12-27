000012* CREA JSON PARA GENERAR LISTADO DE VENTAS DE COMBUSTIBLES
000020 IDENTIFICATION DIVISION.
000021* PROGRAM-ID. "BOM111".
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


           SELECT ARCHIVO-MAESTROS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-MAE-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-MAE
                  ALTERNATE RECORD KEY IS NOMBRE-MAE    WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CTA-INTER-MAE WITH DUPLICATES
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


           SELECT ARCHIVO-TERCEROS  LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TER-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-TERCERO
                  ALTERNATE RECORD KEY DESCRIP-TER WITH DUPLICATES
                  ALTERNATE RECORD KEY ACT-TER     WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT MOVIMIENTO-DIARIO LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-MOV-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-MOV
                  ALTERNATE RECORD KEY SECUENCIA-MOV
                  ALTERNATE RECORD KEY LLAVE-FEC
                  ALTERNATE RECORD KEY LLAVE-DOC-MOV   WITH DUPLICATES
                  ALTERNATE RECORD KEY COSTO-MOV       WITH DUPLICATES
                  ALTERNATE RECORD KEY NRO-ORD-MOV     WITH DUPLICATES
                  ALTERNATE RECORD KEY LLAVE-LIBRE-MOV WITH DUPLICATES
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
       COPY "P:\PROG\PROYECT\FUENTES\FD-MAEST.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-ARTIC.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-TERCE.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-MOVIM.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-PLANO.
           02 DATOS-PLANO     PIC X(500).

000540 WORKING-STORAGE  SECTION.
000550 COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
000560 COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".


       77 NOM-USUAR-W            PIC X(70).
       77 NOM-COMBU-W            PIC X(70).
       77 NOM-MAE-W              PIC X(70).
       77 NOM-ART-W              PIC X(70).
       77 NOM-TER-W              PIC X(70).
       77 NOM-MOV-W              PIC X(70).
       77 NOM-PLANO-W            PIC X(90).
       77 CONT                   PIC 999.


       77 FECHA-SEC              PIC 9(6).
       77 FECHA-FIN              PIC 9(6).
       77 MES-ACT-W              PIC X(3).
       77 BANDERA-W              PIC 9.
       77 IND-W                  PIC 9(3).
       77 REF-W                  PIC 9(6).

       01  FECHA-SIG.
           02 ANO-SIG        PIC 9(4).
           02 MES-SIG        PIC 99.
           02 DIA-SIG        PIC 99.

       01 LLAVE-W                PIC X(8).

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
          02 TOT-DEUDA           PIC S9(13).
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

       01 FECHA-EDIT.
          02 MES-EDIT              PIC X(8).
          02 FILLER                PIC X.
          02 DIA-EDIT              PIC ZZ.
          02 FILLER                PIC X VALUE "/".
          02 ANO-EDIT              PIC 99.


       01 VALOR-GLOBAL-W         PIC ZZZZZZZZZZZZ.ZZ.


       01 LINEA-LLEGAD-W             PIC X(100).

       01 DATO-LLEGADA-W.
          02 SESION-LLEGAD-W        PIC X(29).
          02 DIRECTORIO-LLEGAD-W    PIC X(30).
          02 CARPTA-LLEGAD-W        PIC X(9).
          02 NUM-INI-COMB-W         PIC X(6).
          02 NUM-FIN-COMB-W         PIC X(6).
          02 OPC-IMP-W              PIC X.



       01 LIN-1.
          02 FILLER                  PIC X(9)   VALUE "{*SURTI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SURTIDOR-J              PIC XX.
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
          02 FILLER                  PIC X(8)   VALUE "*DEUDA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CANT-DEU-COMB-J         PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*PLACA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PLACA-COMB-J            PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*SEC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SECUENCIA-MOV-J         PIC X(11).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*CUENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CUENTA-MOV-J            PIC X(11).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*NIT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NIT-J                   PIC X(12).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*REFER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 REFER-MOV-J             PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*NOMB*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOMBRE-MAE-J            PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*DEBITO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VALOR-DEB-J             PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*CREDIT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VALOR-CRED-J            PIC X(20).
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
          02 VALOR-FINANC-ENV    PIC X(20).
          02 FILLER              PIC X VALUE "|".
          02 TOTAL-VALES         PIC X(20).
          02 FILLER              PIC X VALUE "|".
          02 NETO-RECAUDO-ENV    PIC X(20).
          02 FILLER              PIC X VALUE "|".
          02 TOTAL-RBO-ENV       PIC X(20).
          02 FILLER              PIC X VALUE "|".
          02 FECHA-ENV           PIC X(14).
          02 FILLER              PIC X VALUE "|".
          02 TURNO-ENV           PIC X.
          02 FILLER              PIC X VALUE "|".
          02 DETALLE-ENV         PIC X(20).
          02 FILLER              PIC X VALUE "|".
          02 VEND-ENV            PIC X(5).

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
000819        MOVE "BOM111"                TO MSJ3-HTML
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
001102        MOVE "BOM111"                TO MSJ3-HTML
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
006680        MOVE "BOM111"         TO MSJ3-HTML
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
006680        MOVE "BOM111"         TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

002694 I-O-TEST SECTION.
002700     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-MAESTROS.
002710 ESCR-EXCEPTIONES.
002720     IF OTR-STAT = "00"
002730        CONTINUE 
002740     ELSE
002750        MOVE OTR-STAT         TO MSJ1-HTML
002760        MOVE NOM-MAE-W        TO MSJ2-HTML
002770        MOVE "BOM111"         TO MSJ3-HTML
002780        GO TO ENVIAR2-ERROR
002790     END-IF.

002694 I-O-TEST SECTION.
002700     USE AFTER EXCEPTION PROCEDURE ON MAESTRO-ARTICULOS.
002710 ESCR-EXCEPTIONES.
002720     IF OTR-STAT = "00"
002730        CONTINUE 
002740     ELSE
002750        MOVE OTR-STAT         TO MSJ1-HTML
002760        MOVE NOM-ART-W        TO MSJ2-HTML
002770        MOVE "BOM111"         TO MSJ3-HTML
002780        GO TO ENVIAR2-ERROR
002790     END-IF.

002694 I-O-TEST SECTION.
002700     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TERCEROS.
002710 ESCR-EXCEPTIONES.
002720     IF OTR-STAT = "00"
002730        CONTINUE 
002740     ELSE
002750        MOVE OTR-STAT         TO MSJ1-HTML
002760        MOVE NOM-TER-W        TO MSJ2-HTML
002770        MOVE "BOM111"         TO MSJ3-HTML
002780        GO TO ENVIAR2-ERROR
002790     END-IF.

002694 I-O-TEST SECTION.
002700     USE AFTER EXCEPTION PROCEDURE ON MOVIMIENTO-DIARIO.
002710 ESCR-EXCEPTIONES.
002720     IF OTR-STAT = "00"
002730        CONTINUE 
002740     ELSE
002750        MOVE OTR-STAT         TO MSJ1-HTML
002760        MOVE NOM-MOV-W        TO MSJ2-HTML
002770        MOVE "BOM111"         TO MSJ3-HTML
002780        GO TO ENVIAR2-ERROR
002790     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" OR "35"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-PLANO-W      TO MSJ2-HTML
006680        MOVE "BOM111"         TO MSJ3-HTML
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
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W, NUM-INI-COMB-W, NUM-FIN-COMB-W, OPC-IMP-W
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

           MOVE NOM-USUAR-W  TO NOM-COMBU-W NOM-ART-W NOM-MAE-W NOM-TER-W NOM-MOV-W

           INSPECT NOM-USUAR-W  REPLACING FIRST "                       "
                                             BY "\CONTROL\SC-ARCHUSU.DAT".

           INSPECT NOM-COMBU-W  REPLACING FIRST "                       "
                                             BY "\CONTROL\SC-ARCOMBU.DAT".


           INSPECT NOM-MAE-W    REPLACING FIRST "                       "
                                             BY "\CONTROL\SC-ARCHMAE.DAT".

           INSPECT NOM-ART-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-MAESART.DAT".

           INSPECT NOM-TER-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-ARCHTER.DAT".

           MOVE "C:\PROSOFT\TEMP\SC-LISTVENT-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON" TO  NOM-PLANO-W

           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                             BY SESION-LLEGAD-W.

       LEER-USUARIO.

           OPEN INPUT ARCHIVO-USUARIOS
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.


       LEER-COMBUSTIBLES.
           OPEN INPUT ARCHIVO-COMBUSTIBLES.

           MOVE NUM-INI-COMB-W     TO NRO-COMB

           READ ARCHIVO-COMBUSTIBLES WITH NO LOCK
                 INVALID KEY 
                         MOVE "99"       TO MSJ1-HTML
                         MOVE NRO-COMB   TO MSJ2-HTML
                         MOVE "BOM111"   TO MSJ3-HTML
                         GO TO ENVIAR2-ERROR
           END-READ.

       ABRIR-PLANO.

           OPEN OUTPUT ARCHIVO-JSON.

           INITIALIZE REG-PLANO VARIABLES DATOS-ENVIO I CONT.
           MOVE "{*LISTADO*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.

           PERFORM LEER-TABLA VARYING I FROM 1 BY 1
                                       UNTIL I > 15.


       RUTINA-SUBTOTAL.

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
           MOVE VALOR-W      TO SUBTOTAL-ENV
                             
           MOVE VALOR-FINANC-COMB TO VALOR-W
           MOVE VALOR-W           TO VALOR-FINANC-ENV

           COMPUTE TOT-DEUDA ROUNDED = TOTAL-TOT + VALOR-FINANC-COMB - VALOR-NETO-COMB - VALOR-CHEQUE

           MOVE TOT-DEUDA    TO VALOR-W
           MOVE VALOR-W      TO TOTAL-VALES

           MOVE VALOR-CHEQUE TO VALOR-W
           MOVE VALOR-W      TO NETO-RECAUDO-ENV

           MOVE VALOR-NETO-COMB TO VALOR-W
           MOVE VALOR-W         TO TOTAL-RBO-ENV.

           PERFORM RUTINA-FECHA.

           MOVE FECHA-EDIT       TO FECHA-ENV
           MOVE TURNO-COMB       TO TURNO-ENV
           MOVE DETALLE-COMB     TO DETALLE-ENV
           MOVE VEND-COMB        TO VEND-ENV.


       ABRIR-MOVIMIENTO.
           INITIALIZE LIN-1 CARPTA-LLEGAD-W
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA

           MOVE LIN-1 TO DATOS-PLANO
           WRITE REG-PLANO
           MOVE "]" TO DATOS-PLANO
           WRITE REG-PLANO

           EVALUATE MES-COMB
               WHEN 01 MOVE "\ENE\" TO CARPTA-LLEGAD-W
               WHEN 02 MOVE "\FEB\" TO CARPTA-LLEGAD-W
               WHEN 03 MOVE "\MAR\" TO CARPTA-LLEGAD-W
               WHEN 04 MOVE "\ABR\" TO CARPTA-LLEGAD-W
               WHEN 05 MOVE "\MAY\" TO CARPTA-LLEGAD-W
               WHEN 06 MOVE "\JUN\" TO CARPTA-LLEGAD-W
               WHEN 07 MOVE "\JUL\" TO CARPTA-LLEGAD-W
               WHEN 08 MOVE "\AGT\" TO CARPTA-LLEGAD-W
               WHEN 09 MOVE "\SEP\" TO CARPTA-LLEGAD-W
               WHEN 10 MOVE "\OCT\" TO CARPTA-LLEGAD-W
               WHEN 11 MOVE "\NOV\" TO CARPTA-LLEGAD-W
               WHEN 12 MOVE "\DIC\" TO CARPTA-LLEGAD-W
           END-EVALUATE

           INSPECT NOM-MOV-W REPLACING FIRST "         "
                                          BY CARPTA-LLEGAD-W

           INSPECT NOM-MOV-W REPLACING FIRST "              "
                                          BY "SC-ARCHMOV.DAT".


           OPEN INPUT MOVIMIENTO-DIARIO.

           MOVE "1B"        TO LOTE-MOV.
           MOVE NRO-COMB    TO COMPROB-MOV.
           MOVE 0           TO COMP1-MOV.

           START MOVIMIENTO-DIARIO KEY = LLAVE-COMP-MOV
                 INVALID KEY 
                         MOVE "}" TO DATOS-PLANO
                         WRITE REG-PLANO
                         GO TO CERRAR-ARCHIVO
           END-START.

           MOVE LLAVE-COMP-MOV     TO LLAVE-W.


           INITIALIZE REG-PLANO.
           MOVE ",*LINEA-ART*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.
           OPEN INPUT MAESTRO-ARTICULOS
                      ARCHIVO-MAESTROS
                      ARCHIVO-TERCEROS.

       LEER-MOVIMIENTO.

           READ MOVIMIENTO-DIARIO NEXT WITH NO LOCK AT END GO TO CERRAR-MOVIMIENTO.

           IF LLAVE-W = LLAVE-COMP-MOV
              CONTINUE
           ELSE
              GO TO CERRAR-MOVIMIENTO
           END-IF

           MOVE 0 TO BANDERA-W IND-W

           PERFORM IMPRIMIR-DATOS VARYING IND-W FROM 1 BY 1
                                            UNTIL IND-W > 200

           IF BANDERA-W = 0
              INITIALIZE COD-ART-J DESCRIP-ART-J PLACA-COMB-J CANT-DEU-COMB-J 
           END-IF

           MOVE SECUENCIA-MOV      TO SECUENCIA-MOV-J
           MOVE CUENTA-MOV         TO CUENTA-MOV-J
           MOVE ID-MOV             TO NIT-J
           
           IF NRO-DOC-MOV > 0
               MOVE NRO-DOC-MOV    TO REFER-MOV
           END-IF

           MOVE REFER-MOV          TO REFER-MOV-J
           
           IF ID-MOV = ZEROS 
               MOVE CUENTA-MOV TO CTA-MAE
               MOVE 4          TO TIPO-MAE
               READ ARCHIVO-MAESTROS WITH NO LOCK
                    INVALID KEY
                            MOVE ALL "_" TO NOMBRE-MAE
               END-READ
            ELSE
               MOVE ID-MOV TO COD-TER
               READ ARCHIVO-TERCEROS WITH NO LOCK
                    INVALID KEY
                            MOVE ALL "*" TO DESCRIP-TER
               END-READ
               MOVE DESCRIP-TER  TO NOMBRE-MAE
           END-IF

           MOVE NOMBRE-MAE       TO NOMBRE-MAE-J

           IF VALOR-MOV > 0
              MOVE VALOR-MOV     TO NUMER-W
              MOVE NUMER-W       TO VALOR-DEB-J
              INITIALIZE VALOR-CRED-J
           ELSE
              MOVE VALOR-MOV     TO NUMER-W
              MOVE NUMER-W       TO VALOR-CRED-J
              INITIALIZE VALOR-DEB-J
           END-IF

           MOVE ","               TO CIERRE-LIN-2
           INSPECT LIN-2 REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT LIN-2 REPLACING ALL LOW-VALUES BY SPACES

           MOVE LIN-2 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           GO TO LEER-MOVIMIENTO.


       CERRAR-MOVIMIENTO.

           INITIALIZE LIN-2
           MOVE LIN-2 TO DATOS-PLANO
           WRITE REG-PLANO
           MOVE "]}" TO DATOS-PLANO
           WRITE REG-PLANO

           CLOSE MAESTRO-ARTICULOS
                 ARCHIVO-TERCEROS
                 MOVIMIENTO-DIARIO.

       CERRAR-ARCHIVO.

           CLOSE ARCHIVO-COMBUSTIBLES.
      *           MAESTRO-ARTICULOS
      *           ARCHIVO-JSON.


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
              ADD 1 TO CONT
              MOVE SURTIDOR-COMB (I)    TO SURTIDOR-J
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
              INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES

              MOVE LIN-1 TO DATOS-PLANO
              WRITE REG-PLANO END-WRITE

              ADD CANTIDAD-COMB (I) TO CANT-TOT
              ADD VALOR-COMB    (I) TO VENTA-TOT
              ADD SOBRET-COMB   (I) TO TASA-TOT
              ADD GLOBAL-COMB   (I) TO GLOB-TOT
              ADD TOTAL-W           TO TOTAL-TOT
           END-IF.

       IMPRIMIR-DATOS.
           MOVE NRO-DOC-MOV TO REF-W.

           IF VLR-DEU-COMB (IND-W) = VALOR-MOV AND DOC-DEU-COMB (IND-W) = REF-W
              IF ART-DEU-COMB (IND-W) = SPACES 
                 MOVE SPACES TO DESCRIP-ART
              ELSE
                 MOVE 0                    TO TIPO-ART
                 MOVE GRP-DEU-COMB (IND-W) TO GRUPO-ART
                 MOVE NUM-DEU-COMB (IND-W) TO NUMERO-ART
                 MOVE SPACES               TO CLASE-ART

                 READ MAESTRO-ARTICULOS WITH NO LOCK 
                      INVALID KEY 
                              MOVE ALL "*" TO DESCRIP-ART
                 END-READ
              END-IF

              MOVE COD-ART              TO COD-ART-J
              MOVE DESCRIP-ART          TO DESCRIP-ART-J

              MOVE CANT-DEU-COMB(IND-W) TO NUMER-W
              MOVE NUMER-W              TO CANT-DEU-COMB-J

              MOVE PLACA-COMB (IND-W)   TO PLACA-COMB-J
              MOVE 1 TO BANDERA-W
           END-IF.


       RUTINA-FECHA.
           EVALUATE MES-COMB
           WHEN 01 MOVE "   ENERO" TO MES-EDIT
           WHEN 02 MOVE " FEBRERO" TO MES-EDIT
           WHEN 03 MOVE "   MARZO" TO MES-EDIT
           WHEN 04 MOVE "   ABRIL" TO MES-EDIT
           WHEN 05 MOVE "    MAYO" TO MES-EDIT
           WHEN 06 MOVE "   JUNIO" TO MES-EDIT
           WHEN 07 MOVE "   JULIO" TO MES-EDIT
           WHEN 08 MOVE "  AGOSTO" TO MES-EDIT
           WHEN 09 MOVE "SEPTIEMB" TO MES-EDIT
           WHEN 10 MOVE " OCTUBRE" TO MES-EDIT
           WHEN 11 MOVE "NOVIEMBR" TO MES-EDIT
           WHEN 12 MOVE "DICIEMBR" TO MES-EDIT.
           MOVE DIA-COMB TO DIA-EDIT.
           MOVE ANO-COMB TO ANO-EDIT.


002566 COPY "P:\PROG\PROYECT\FUENTES\SC-WEB.CBL".
002567
