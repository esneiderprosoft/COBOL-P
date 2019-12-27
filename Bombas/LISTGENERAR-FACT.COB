000012* SE GUARDA FACTURACION A PARTIR DE LOS COMPROBANTES DE COMBUSTIBLES.
000020 IDENTIFICATION DIVISION.
000021* PROGRAM-ID. "BOMB11_1".
000022
000023 PROGRAM-ID. "HttpExtensionProc".    
000024 ENVIRONMENT DIVISION.
000025 CONFIGURATION SECTION.
000026 INPUT-OUTPUT SECTION.
000027 FILE-CONTROL.
000080 COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
000150 COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FS-FACTURA2.CBL".


           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-USUAR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


    	   SELECT ARCHIVO-PREFIJOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PREF-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS LLAVE-PREF
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-FACTURAS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-FACT-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-FAC
                  ALTERNATE RECORD KEY IS FECHA-FAC        WITH DUPLICATES
                  ALTERNATE RECORD KEY IS NIT-FAC          WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LLAVE-VEND-FAC   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS VENDEDOR-FAC     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PLACA-FAC        WITH DUPLICATES
                  ALTERNATE RECORD KEY IS TIQ-BASC-FAC     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ORD-PEDI-FAC     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CIUDAD-FAC       WITH DUPLICATES                  
                  ALTERNATE RECORD KEY IS NRO-FACT-DEV-FAC WITH DUPLICATES
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


           SELECT MAESTRO-ARTICULOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-ART-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY COD-ART
                  ALTERNATE RECORD KEY CLASE-ART       WITH DUPLICATES
                  ALTERNATE RECORD KEY COD-BARRAS-ART  WITH DUPLICATES
                  ALTERNATE RECORD KEY DESCRIP-ART     WITH DUPLICATES
                  ALTERNATE RECORD KEY USO-ART         WITH DUPLICATES
                  ALTERNATE RECORD KEY CL-ROTAC-ART    WITH DUPLICATES
                  ALTERNATE RECORD KEY UBICAC-ART      WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-VENCE-ART WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-TERCEROS  LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TER-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-TERCERO
                  ALTERNATE RECORD KEY DESCRIP-TER WITH DUPLICATES
                  ALTERNATE RECORD KEY ACT-TER     WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-TEMPORAL
                  ASSIGN NOM-TMP-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-TEMP
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
       COPY "P:\PROG\PROYECT\FUENTES\FD-PREFI.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-FACTURA.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-MOVIM.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-ARTIC.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-TERCE.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-FACTURA2.CBL".


       FD ARCHIVO-TEMPORAL
             LABEL RECORD STANDARD.
       01 REG-TEMP.
          02 LLAVE-TEMP.
             03 NIT-2           PIC 9(10).
             03 C-COSTO-2       PIC X(4).
             03 FECHA-2         PIC X(6).
          02 DATOS-TEMP.
             03 TABLA-TEMP OCCURS 5.
                05 ART-2.
                   07 GRUPO-2   PIC XX.
                   07 COD-ART-2 PIC X(13).
                05 CANT-2       PIC 9(6)V99.
                05 VALOR-2      PIC S9(12)V99.
             03 TABLA-MOV-TEMP OCCURS 1000.
                05 NOM-MOV-TEMP PIC X(20).
                05 SECUENCIA-TEMP.
                   07 LOTE-TEMP PIC XX.
                   07 COMP-TEMP PIC X(6).
                   07 SECU-TEM  PIC X(3).


       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-PLANO.
           02 DATOS-PLANO     PIC X(500).


000540 WORKING-STORAGE  SECTION.
000550 COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
000560 COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".

       77 NOM-FACT2-LNK          PIC X(70).
       77 NOM-USUAR-W            PIC X(70).
       77 NOM-PREF-W             PIC X(70).
       77 NOM-FACT-W             PIC X(70).
       77 NOM-MOV-W              PIC X(70).
       77 NOM-ART-W              PIC X(70).
       77 NOM-TER-W              PIC X(70).
       77 NOM-TMP-W              PIC X(70).
       77 NOM-PLANO-W            PIC X(70).
       77 DIRECTORIO-EDIT        PIC X(70).
       77 NOM-FAC-EDIT           PIC X(20).
       77 NOM-FAC2-EDIT          PIC X(20).
       77 NOM-MOV-EDIT           PIC X(20).
       77 VALOR-EDIT             PIC Z(12).ZZ-.
       77 CANTIDAD-EDIT          PIC Z(6).ZZZ.
       77 VALOR-TOTAL-W          PIC S9(12)V99.


       77 NIT-W                  PIC 9(10).
       77 SALDO-W                PIC X.
       77 SW-MES                 PIC 9.
       77 SW-LOTE                PIC 9.
       77 MES-CTL                PIC 99.
       77 MES-MAX                PIC 99.
       77 TIPO-W                 PIC 9.
       77 NRO-FACT-W             PIC 9(6).
       77 SW9                    PIC 9.
       77 SW-ITEM                PIC 9.
       77 LOTE-X                 PIC XX.
       77 FECHA-EDIT             PIC X(9).
       77 DETALLE-FACT-W         PIC X(50).
       77 SUBTOT-W               PIC S9(11)V99.
       77 CANT-W                 PIC 9(12)V999.
       77 SALDO-CARTERA          PIC S9(11).
       77 CUPO-TER-W             PIC 9(10).
       77 MORA-W                 PIC S9(5).
       77 SUC-W                  PIC 9.
       77 DEF-PREF-W             PIC X.


       01 CTA-DEUD.
          02 MAY-DEUD            PIC X(4).
          02 SCTA-DEUD           PIC XX.
          02 AUXIL-W.
             03 AUX1-W           PIC X(4).
             03 SUCURSAL-W       PIC X.

       01 DETALLE-W.
          02 SURT-W              PIC XX.
          02 FILLER              PIC X.
          02 ART-DEU-W.
             05 GRP-DEU-W        PIC XX.
             05 NUM-DEU-W        PIC X(13).
          02 FILLER              PIC X.
          02 CANT-DET            PIC Z(11)9.999.
          02 FILLER              PIC X.
          02 OTROS-DET           PIC X(17).

       01 FECHA-INI.
          02 ANO-INI             PIC 99.
          02 MES-INI             PIC 99.
          02 DIA-INI             PIC 99.

       01 FECHA-FIN.
          02 ANO-FIN             PIC 99.
          02 MES-FIN             PIC 99.
          02 DIA-FIN             PIC 99.

       01 LINEA-LLEGAD-W             PIC X(200).

       01 DATO-LLEGADA-W.
          02 SESION-LLEGAD-W        PIC X(29).
          02 DIRECTORIO-LLEGAD-W    PIC X(30).
          02 CARPTA-LLEGAD-W        PIC X(9).



       01 NOMBRE-LNK             PIC X(30).
       01 DIRECTORIO-W           PIC X(35).
       01 INVALID-007            PIC 99.
       01 MENSAJE-W              PIC X(50).
       01 DATO-ULT-MOV-LNK.
          02 LOTE-ULT-MOV-LNK.
             03 LOTE1-ULT-MOV-LNK PIC X.
             03 LOTE2-ULT-MOV-LNK PIC X.
             03 LOTE3-ULT-MOV-LNK PIC X.
          02 NRO-ULT-COMP-LNK.
             03 NRO1-ULT-COMP-LNK PIC 9(3).
             03 NRO2-ULT-COMP-LNK PIC 9(6).
          02 FECHA-ULT-MOV-LNK.
             03 ANO-ULT-MOV-LNK  PIC 99.
             03 MES-ULT-MOV-LNK  PIC 99.
             03 DIA-ULT-MOV-LNK  PIC 99.

       01 LIN-1.
          02 FILLER                  PIC X(7)   VALUE "{*NIT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NIT-J                   PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(8)   VALUE "*COSTO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 C-COSTO-J               PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(8)   VALUE "*FEHCA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-J                 PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*ART*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-ART-J               PIC X(18).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*CANTID*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CANTIDAD-J              PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*VALOR*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VALOR-COMB-J            PIC X(16).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*TOTAL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TOTAL-J                 PIC X(20).
003710    02 FILLER                  PIC XX     VALUE "*}".
005040    02 CIERRE-LIN-1            PIC X.


       01 LIN-2.
          02 FILLER                  PIC X(7)   VALUE "{*NOM*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOM-ARCH-J              PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(07)  VALUE "*LOTE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LOTE-J                  PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*COMP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COMP-J                  PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*SECU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SECU-J                  PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*CUENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CUENTA-MOV-J            PIC X(11).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*NIT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NIT2-J                  PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*REFER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 REFER-MOV-J             PIC X(6).
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-LIN-2            PIC X.



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
000819        MOVE "BOMB11_1"              TO MSJ3-HTML
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
001102        MOVE "BOMB11_1"              TO MSJ3-HTML
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
006680        MOVE "BOMB11_1"         TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PREFIJOS.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" 
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-PREF-W       TO MSJ2-HTML
006680        MOVE "BOMB11_1"       TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-FACTURAS.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" 
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-FACT-W       TO MSJ2-HTML
006680        MOVE "BOMB11_1"       TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-FACTURAS2.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" 
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-FACT2-LNK    TO MSJ2-HTML
006680        MOVE "BOMB11_1"       TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON MOVIMIENTO-DIARIO.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" 
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-MOV-W        TO MSJ2-HTML
006680        MOVE "BOMB11_1"       TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON MAESTRO-ARTICULOS.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" 
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-ART-W        TO MSJ2-HTML
006680        MOVE "BOMB11_1"       TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TERCEROS.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" 
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-TER-W        TO MSJ2-HTML
006680        MOVE "BOMB11_1"       TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TEMPORAL.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" OR "35"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-TMP-W        TO MSJ2-HTML
006680        MOVE "BOMB11_1"       TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" OR "35"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-PLANO-W      TO MSJ2-HTML
006680        MOVE "BOMB11_1"       TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

006710
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
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W, ADMIN-W, FECHA-INI, FECHA-FIN, NIT-W, NRO-FACT-W, DEF-PREF-W
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
001614     INITIALIZE NOM-USUAR-W  NOM-PREF-W NOM-TER-W NOM-FACT-W DIRECTORIO-EDIT NOM-ART-W 
001615     MOVE "\\" TO NOM-USUAR-W 
001616
001617     INSPECT NOM-USUAR-W REPLACING FIRST "                    "
001618                                    BY IP-DATOS-USUNET
001621     INSPECT NOM-USUAR-W REPLACING FIRST " "
001624                                     BY "\"
007292     INSPECT NOM-USUAR-W REPLACING FIRST "                              "
007293                                     BY DIRECTORIO-LLEGAD-W

           MOVE NOM-USUAR-W  TO NOM-PREF-W NOM-TER-W NOM-FACT-W DIRECTORIO-EDIT NOM-ART-W

           INSPECT NOM-USUAR-W  REPLACING FIRST "                       "
                                             BY "\CONTROL\SC-ARCHUSU.DAT".

           INSPECT NOM-PREF-W REPLACING FIRST "                        "
                                           BY "\CONTROL\SC-ARCHPREF.DAT".

           INSPECT NOM-TER-W    REPLACING FIRST "                       "
                                             BY "\CONTROL\SC-ARCHTER.DAT".

           INSPECT NOM-ART-W    REPLACING FIRST "                       "
                                             BY "\CONTROL\SC-MAESART.DAT".

           INSPECT NOM-FACT-W   REPLACING FIRST "         "
                                             BY "\CONTROL\".

           MOVE NOM-FACT-W      TO NOM-FACT2-LNK.


       LEER-USUARIO.

           OPEN INPUT ARCHIVO-USUARIOS
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.

           EVALUATE PUC-USU
               WHEN 1 MOVE "13050500001" TO CTA-DEUD
               WHEN 2 MOVE "15050500001" TO CTA-DEUD
               WHEN 4 MOVE "14700500001" TO CTA-DEUD
           END-EVALUATE.

       ABRIR-FACTURAS.
           IF DEF-PREF-W > 1
              MOVE 7 TO SUC-W
              MOVE "SC-ARCHFACT-07.DAT"   TO NOM-FAC-EDIT
              MOVE "SC-ARCHEXFACT-07.DAT" TO NOM-FAC2-EDIT
           ELSE
              MOVE 8 TO SUC-W
              MOVE "SC-ARCHFACT-08.DAT"   TO NOM-FAC-EDIT
              MOVE "SC-ARCHEXFACT-08.DAT" TO NOM-FAC2-EDIT
           END-IF.

           INSPECT NOM-FACT-W   REPLACING FIRST "                    "
                                             BY NOM-FAC-EDIT.

           INSPECT NOM-FACT2-LNK REPLACING FIRST "                    "
                                              BY NOM-FAC2-EDIT.

           OPEN I-O ARCHIVO-FACTURAS.
           EVALUATE OTR-STAT
               WHEN "00"  CONTINUE
               WHEN "35"  OPEN OUTPUT ARCHIVO-FACTURAS
                          CLOSE ARCHIVO-FACTURAS
                          GO TO ABRIR-FACTURAS
               WHEN OTHER GO TO CERRAR-SESION
           END-EVALUATE.

           IF DEF-PREF > 1
              MOVE 7      TO LOTE3-ULT-MOV-LNK
           ELSE
              MOVE 8      TO LOTE3-ULT-MOV-LNK
           END-IF.
           MOVE "F"       TO LOTE1-ULT-MOV-LNK
           MOVE "0"       TO LOTE2-ULT-MOV-LNK.

       COMPROBAR-FACTURAS.
           MOVE NRO-FACT-W TO NRO-FAC
           READ ARCHIVO-FACTURAS WITH NO LOCK 
                INVALID KEY 
                        CLOSE ARCHIVO-FACTURAS
                NOT INVALID KEY
                        ADD 1 TO NRO-FACT-W
                        GO TO COMPROBAR-FACTURAS
           END-READ.


       EDITAR-DETALLE.
           MOVE "DURANTE EL PERIODO F1F1F1F1F HASTA F2F2F2F2F"
                                TO DETALLE-FACT-W.

           EVALUATE MES-INI
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
               WHEN 12 MOVE "DIC DD/AA" TO FECHA-EDIT
           END-EVALUATE.

           INSPECT FECHA-EDIT REPLACING FIRST "DD" BY DIA-INI.
           INSPECT FECHA-EDIT REPLACING FIRST "AA" BY ANO-INI.
           INSPECT DETALLE-FACT-W REPLACING FIRST "F1F1F1F1F"
                                            BY FECHA-EDIT.

           EVALUATE MES-FIN
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
               WHEN 12 MOVE "DIC DD/AA" TO FECHA-EDIT
           END-EVALUATE.

           INSPECT FECHA-EDIT REPLACING FIRST "DD" BY DIA-FIN.
           INSPECT FECHA-EDIT REPLACING FIRST "AA" BY ANO-FIN.
           INSPECT DETALLE-FACT-W REPLACING FIRST "F2F2F2F2F"
                                            BY FECHA-EDIT.

       DATO-TERCERO.

           IF NIT-W = 99
              MOVE "PROCESO TOTAL" TO DESCRIP-TER
           ELSE
              PERFORM LEER-TERCERO
           END-IF.

       CREAR-TEMPORAL.

           ACCEPT HORA-SIST FROM TIME
           MOVE "C:\PROSOFT\TEMP\AAAAT1T1T1.L" TO NOM-TMP-W
           INSPECT NOM-TMP-W  REPLACING FIRST "AAAA" BY ADMIN-W
           INSPECT NOM-TMP-W  REPLACING FIRST "T1T1T1" BY HORA-SIST.

           OPEN OUTPUT ARCHIVO-TEMPORAL
           CLOSE ARCHIVO-TEMPORAL
           OPEN I-O ARCHIVO-TEMPORAL.


       ABRIR-MOVIMIENTO.
           ADD 1 TO SW-MES.
           EVALUATE SW-MES
               WHEN 1 MOVE MES-INI TO MES-CTL
               WHEN 2 MOVE MES-FIN TO MES-CTL
               WHEN OTHER CLOSE ARCHIVO-TEMPORAL
                    GO TO CERRAR-SESION
           END-EVALUATE

           MOVE DIRECTORIO-EDIT  TO NOM-MOV-W

           EVALUATE MES-CTL
               WHEN 01 MOVE "\ENE\SC-ARCHMOV.DAT" TO NOM-MOV-EDIT
               WHEN 02 MOVE "\FEB\SC-ARCHMOV.DAT" TO NOM-MOV-EDIT
               WHEN 03 MOVE "\MAR\SC-ARCHMOV.DAT" TO NOM-MOV-EDIT
               WHEN 04 MOVE "\ABR\SC-ARCHMOV.DAT" TO NOM-MOV-EDIT
               WHEN 05 MOVE "\MAY\SC-ARCHMOV.DAT" TO NOM-MOV-EDIT
               WHEN 06 MOVE "\JUN\SC-ARCHMOV.DAT" TO NOM-MOV-EDIT
               WHEN 07 MOVE "\JUL\SC-ARCHMOV.DAT" TO NOM-MOV-EDIT
               WHEN 08 MOVE "\AGT\SC-ARCHMOV.DAT" TO NOM-MOV-EDIT
               WHEN 09 MOVE "\SEP\SC-ARCHMOV.DAT" TO NOM-MOV-EDIT
               WHEN 10 MOVE "\OCT\SC-ARCHMOV.DAT" TO NOM-MOV-EDIT
               WHEN 11 MOVE "\NOV\SC-ARCHMOV.DAT" TO NOM-MOV-EDIT
               WHEN 12 MOVE "\DIC\SC-ARCHMOV.DAT" TO NOM-MOV-EDIT
           END-EVALUATE.

           INSPECT NOM-MOV-W REPLACING FIRST "                    "
                                          BY NOM-MOV-EDIT.

           OPEN I-O MOVIMIENTO-DIARIO.
           IF OTR-STAT  = "00"
              CONTINUE
           ELSE
              GO TO ABRIR-MOVIMIENTO
           END-IF.

           MOVE 0 TO SW-LOTE.

       LEER-INDICE.
           ADD 1 TO SW-LOTE.
           EVALUATE SW-LOTE
               WHEN 1 MOVE "1B" TO LOTE-X
               WHEN 2 IF NIT-USU = 41329247
                      OR 900006128
                         MOVE "1F" TO LOTE-X
                      ELSE
                         GO TO RE-INICIAR
                      END-IF
               WHEN 3 IF NIT-USU = 41329247
                      OR 900006128
                         MOVE "2F" TO LOTE-X
                      ELSE
                         GO TO RE-INICIAR
                      END-IF
               WHEN OTHER GO TO RE-INICIAR
           END-EVALUATE.


           MOVE LOTE-X TO LOTE-MOV.
           START MOVIMIENTO-DIARIO KEY = LOTE-MOV
                 INVALID KEY GO TO LEER-INDICE
           END-START.

       LEER-MOV.
           READ MOVIMIENTO-DIARIO NEXT WITH NO LOCK AT END GO TO RE-INICIAR.

           IF LOTE-MOV = LOTE-X
              CONTINUE
           ELSE
              GO TO LEER-INDICE
           END-IF

           IF  NIT-W = 99
               CONTINUE
           ELSE
               IF ID-MOV = NIT-W
                  CONTINUE
               ELSE
                  GO TO LEER-MOV
               END-IF
           END-IF

           IF  CUENTA-MOV = CTA-DEUD AND VALOR-MOV  > 0
               CONTINUE
           ELSE
               GO TO LEER-MOV
           END-IF

           IF FECHA-MOV < FECHA-INI OR FECHA-MOV > FECHA-FIN
              GO TO LEER-MOV
           END-IF

           IF FLUJO1-MOV = "*"
              GO TO LEER-MOV
           END-IF.

       RUTINA-MOVIMIENTO.
           MOVE ID-MOV      TO NIT-2.
           MOVE COSTO-MOV   TO C-COSTO-2.
           MOVE FECHA-MOV   TO FECHA-2
           READ ARCHIVO-TEMPORAL
                INVALID KEY INITIALIZE DATOS-TEMP
                            WRITE REG-TEMP
           END-READ


           IF SW-LOTE = 1
              MOVE DETALLE-MOV    TO DETALLE-W
              MOVE CANT-DET TO CANT-W
              IF CANT-W IS NOT NUMERIC OR (CANT-W IS ZERO)
                 MOVE "99COMB" TO ART-DEU-W
                 MOVE 1        TO CANT-W
              END-IF
           ELSE
              MOVE "99ALMAC"   TO ART-DEU-W
              MOVE 1           TO CANT-W
           END-IF

           MOVE 0 TO SW-ITEM.
           PERFORM UBICAR-ART VARYING I FROM 1 BY 1
                                UNTIL I  > 5 OR SW-ITEM = 1.

           IF SW-ITEM = 0
              PERFORM LLENAR-ART VARYING I FROM 1 BY 1
                                   UNTIL I > 5 OR SW-ITEM = 1
           END-IF.


           MOVE 0 TO SW-BUSCAR.
           PERFORM ASIGNAR-SECUENCIA VARYING I FROM 1 BY 1
                                     UNTIL I > 998 OR SW-BUSCAR = 1.


           REWRITE REG-TEMP END-REWRITE
           GO TO LEER-MOV.

       UBICAR-ART.
           IF ART-DEU-W = ART-2 (I)
              ADD CANT-W    TO CANT-2  (I)
              ADD VALOR-MOV TO VALOR-2 (I)
              MOVE 1 TO SW-ITEM
           END-IF.

       LLENAR-ART.
           IF ART-2 (I) = SPACES
              MOVE ART-DEU-W TO ART-2   (I)
              MOVE CANT-W    TO CANT-2  (I)
              MOVE VALOR-MOV TO VALOR-2 (I)
              MOVE 1 TO SW-ITEM
           END-IF.

       ASIGNAR-SECUENCIA.
           IF LOTE-TEMP (I) = SPACES
              MOVE NOM-MOV-EDIT  TO NOM-MOV-TEMP   (I)
              MOVE SECUENCIA-MOV TO SECUENCIA-TEMP (I)
              MOVE 1 TO SW-BUSCAR
           END-IF.


       RE-INICIAR.
           CLOSE MOVIMIENTO-DIARIO.

           IF MES-INI IS NOT = MES-FIN AND SW-MES = 1
              GO TO ABRIR-MOVIMIENTO
           END-IF.

       PROCESO-FACTURACION.
            MOVE NOM-TMP-W    TO NOM-PLANO-W
            INSPECT NOM-PLANO-W REPLACING FIRST ".L   " BY ".JSON"
            CLOSE ARCHIVO-TEMPORAL
            OPEN OUTPUT ARCHIVO-JSON.
            OPEN INPUT  ARCHIVO-TERCEROS
                        ARCHIVO-TEMPORAL.

           INITIALIZE REG-PLANO.
           MOVE "{*LISTADO*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.
           MOVE 0  TO SW9.

       LEER-TEMPORAL.
           READ ARCHIVO-TEMPORAL NEXT WITH NO LOCK AT END GO TO CERRAR-ARCHIVOS.

           IF SW9 = 0
              MOVE 1 TO SW9
           ELSE
              ADD 1 TO NRO-FACT-W
           END-IF

           COMPUTE SUBTOT-W =  VALOR-2 (1)
                             + VALOR-2 (2)
                             + VALOR-2 (3)
                             + VALOR-2 (4)
                             + VALOR-2 (5)
           IF SUBTOT-W = ZERO
               GO TO LEER-TEMPORAL
           END-IF.

           INITIALIZE REG-FAC.

           MOVE NRO-FACT-W TO NRO-FAC
           COMPUTE ANO-FAC = ANO-FIN + 2000.
           MOVE MES-FIN        TO MES-FAC
           MOVE DIA-FIN        TO DIA-FAC
           MOVE FECHA-FAC      TO FECHA-VENCE-FAC (1)
           MOVE NIT-2          TO NIT-FAC
           MOVE C-COSTO-2      TO C-COSTO-FAC
           MOVE 02             TO FORMA-PAGO-FAC
           MOVE "ALM01"        TO VENDEDOR-FAC
           MOVE DETALLE-FACT-W TO DETALLE-FAC


           PERFORM LLENAR-TABLA-FACT VARYING I FROM  1 BY 1
                                               UNTIL I  > 5

           INITIALIZE LIN-1 CARPTA-LLEGAD-W

           STRING LIN-1, "],*LINEA-ART*:[" DELIMITED BY SIZE
                  INTO DATOS-PLANO
           END-STRING

           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA

           WRITE REG-PLANO

           MOVE 0 TO SW-BUSCAR.
           PERFORM ACTUALIZAR-NUMERO VARYING I FROM 1 BY 1
                            UNTIL I > 998 OR SW-BUSCAR = 1.


           GO TO LEER-TEMPORAL.

       CERRAR-ARCHIVOS.
           MOVE "{}]}"  TO DATOS-PLANO
           WRITE REG-PLANO
           CLOSE ARCHIVO-JSON
                 ARCHIVO-TERCEROS.


002147 ENVIO-DATOS.
004378     MOVE "datosrecibidos"  TO COBW3-CNV-NAME
004393     MOVE 00        TO COBW3-CNV-VALUE
004403     CALL "COBW3_SET_CNV"   USING COBW3
004413     MOVE "../PAGINAS/RECIBIDOS.ASPX"  TO SALIDA-HTML
004430     PERFORM ABRIR-HTML.

002545 CERRAR-SESION.
002546     CALL "COBW3_FREE" USING COBW3.
002547     MOVE 1 TO PROGRAM-STATUS.
002548     EXIT PROGRAM.

       LEER-TERCERO.

           OPEN INPUT ARCHIVO-TERCEROS.
           MOVE NIT-W   TO COD-TER
           READ ARCHIVO-TERCEROS  WITH NO LOCK
                INVALID KEY
                        MOVE SPACES   TO DESCRIP-TER
           END-READ.


           CLOSE ARCHIVO-TERCEROS.


       LLENAR-TABLA-FACT.

           IF VALOR-2 (I) > 0
              MOVE NIT-2     TO NIT-J
              MOVE C-COSTO-2 TO C-COSTO-J
              MOVE FECHA-2   TO FECHA-J
              MOVE ART-2 (I) TO COD-ART-J

              MOVE CANT-2    (I) TO CANTIDAD-EDIT
              MOVE CANTIDAD-EDIT TO CANTIDAD-J

              MOVE VALOR-2   (I) TO VALOR-EDIT
              MOVE VALOR-EDIT    TO VALOR-COMB-J

              ADD  VALOR-2   (I) TO VALOR-TOTAL-W
              MOVE VALOR-TOTAL-W TO VALOR-EDIT
              MOVE VALOR-EDIT    TO TOTAL-J

              MOVE ","               TO CIERRE-LIN-1
              INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
              INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES

              MOVE LIN-1 TO DATOS-PLANO
              WRITE REG-PLANO END-WRITE

           END-IF.

       ACTUALIZAR-NUMERO.
           IF LOTE-TEMP (I) = SPACES
              MOVE 1 TO SW-BUSCAR
           ELSE
      *        MOVE NOM-MOV-TEMP (I)   TO NOM-ARCH-J
              MOVE LOTE-TEMP    (I)   TO LOTE-J
              MOVE COMP-TEMP    (I)   TO COMP-J
              MOVE SECU-TEM     (I)   TO SECU-J

              MOVE ","               TO CIERRE-LIN-2
              INSPECT LIN-2 REPLACING ALL "*" BY CARAC-COMILLA
              INSPECT LIN-2 REPLACING ALL LOW-VALUES BY SPACES

              MOVE LIN-2 TO DATOS-PLANO
              WRITE REG-PLANO END-WRITE
           END-IF.

002566 COPY "P:\PROG\PROYECT\FUENTES\SC-WEB.CBL".
002567
