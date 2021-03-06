000012* BOMBAS - GENERA FACTURA DE VENTAS DE COMBUSTIBLES.
000020 IDENTIFICATION DIVISION.
000021* PROGRAM-ID. "BOM105".
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


           SELECT ARCHIVO-SURTIDORES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-SURT-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY COD-SURTI
                  FILE STATUS  OTR-STAT.


           SELECT ARCHIVO-COMBUSTIBLES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-COMBU-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-COMB
                  ALTERNATE RECORD KEY IS FECHA-COMB WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-TERCEROS  LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TER-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-TERCERO
                  ALTERNATE RECORD KEY DESCRIP-TER WITH DUPLICATES
                  ALTERNATE RECORD KEY ACT-TER     WITH DUPLICATES
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


           SELECT ARCHIVO-SOBRETASAS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-SOBRE-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY ART-SOBRE
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-JSON
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.

000400 DATA DIVISION.
000410 FILE SECTION.
000420 COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
000421 COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
000450 COPY "P:\PROG\PROYECT\FUENTES\FD-USUAR.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SURTI.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-COMBU.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-ARTIC.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-TERCE.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SOBRE.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-PLANO.
           02 DATOS-PLANO     PIC X(500).

000540 WORKING-STORAGE  SECTION.
000550 COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
000560 COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".

000570 01 NOM-USUAR-W           PIC X(70).
       01 NOM-SURT-W            PIC X(70).
       01 NOM-COMBU-W           PIC X(70).
       01 NOM-PLANO-W           PIC X(90).
       01 NOM-TER-W             PIC X(70).
       01 NOM-ART-W             PIC X(70).
       01 NOM-SOBRE-W           PIC X(70).
       01 VARIABLES.
          02 CANT2-EDIT         PIC ZZZZZZZZZZZZ.ZZ.
          02 CANT-EDIT          PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZZ.
          02 CANTIDAD-T         PIC 9(13)V999.
          02 VENTA-W            PIC 9(13)V999.
          02 VALOR-EDIT         PIC Z,ZZZ,ZZZ,ZZZ,ZZZ.ZZZ.
          02 VALOR-EDIT2        PIC ZZZ,ZZZ,ZZZ,ZZZ.
          02 VLR-TOT-VENTA      PIC S9(13)V999.
          02 VLR-TOT-DEUD       PIC 9(13)V999.
          02 VALOR-TEMP         PIC S9(13)V99.
       01 NIT-EDIT-J            PIC ZZ,ZZZ,ZZZ,ZZZ.
       77 COMP-EDIT             PIC ZZZZZZ.

       01 ART-W.
          02 TIPO-ART-W            PIC 9.
          02 COD-ART-W.
             03 GRUPO-W            PIC XX.
             03 NUM-ART-W          PIC X(13).
          02 CLASE-W               PIC XX.



       01 LINEA-LLEGAD-W             PIC X(100).

       01 DATO-LLEGADA-W.
          02 SESION-LLEGAD-W        PIC X(29).
          02 DIRECTORIO-LLEGAD-W    PIC X(30).
          02 CARPTA-LLEGAD-W        PIC X(9).
          02 COMP-W                 PIC X(6).



       01 DATOS-ENVIO.
          02 DAT0-ENV           PIC XX.
          02 FILLER             PIC X VALUE "|".
          02 NRO-ENV            PIC X(6).
          02 FILLER             PIC X VALUE "|".
          02 FECHA-ENV          PIC X(6).
          02 FILLER             PIC X VALUE "|".
          02 TURNO-ENV          PIC X.
          02 FILLER             PIC X VALUE "|".
          02 DETALLE-ENV        PIC X(20).
          02 FILLER             PIC X VALUE "|".
          02 OPER-ENV           PIC X(4).
          02 FILLER             PIC X VALUE "|".
          02 FEC-REST-ENV       PIC X(6).
          02 FILLER             PIC X VALUE "|".
          02 CANTIDAD-ENV       PIC X(20).
          02 FILLER             PIC X VALUE "|".
          02 VALOR-TOT-ENV      PIC X(20).
          02 FILLER             PIC X VALUE "|".
          02 VALOR-FIN-ENV      PIC X(20).
          02 FILLER             PIC X VALUE "|".
          02 VALOR-TOT-DEU-ENV  PIC X(20).
          02 FILLER             PIC X VALUE "|".
          02 VALOR-CHEQUE-ENV   PIC X(20).
          02 FILLER             PIC X VALUE "|".
          02 VALOR-NETO-ENV     PIC X(20).
          02 FILLER             PIC X VALUE "|".
          02 VEND-ENV           PIC X(5).


       01 LIN-1.
          02 FILLER                  PIC X(9)   VALUE "{*SURTI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SURTIDOR-J              PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*ARTICU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-ART-J               PIC X(18).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*SOBRETA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SOBRET-J                PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*GLB-SOBRETA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 GLB-SOBRET-J            PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*VLR-VENT-ART*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VR-VENTA-ART-J          PIC X(21).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*CANTID*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CANTIDAD-COMB-J         PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*NUM-ANT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NUM-ANT-COMB-J          PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*VALOR*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VALOR-COMB-J            PIC X(21).
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-LIN-1            PIC X.
 
       01 LIN-2.
          02 FILLER                  PIC X(7)   VALUE "{*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-DEUD-J              PIC X(11).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*NIT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NIT-J                   PIC X(15).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*DOCUM*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DOCUM-DEUD-J            PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*VLR*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VALOR-DEUD-J            PIC X(15).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIPT-TER-J          PIC X(50).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*ART*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ART-DEUD-J              PIC X(18).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*CANT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CANT-DEUD-J             PIC X(15).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*PLACA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PLACA-DEUD-J            PIC X(6).
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
000819        MOVE "BOM105"                TO MSJ3-HTML
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
001102        MOVE "BOM105"                TO MSJ3-HTML
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
006680        MOVE "BOM105"         TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.


006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SURTIDORES.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" 
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-SURT-W       TO MSJ2-HTML
006680        MOVE "BOM105"         TO MSJ3-HTML
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
006680        MOVE "BOM105"         TO MSJ3-HTML
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
006680        MOVE "BOM105"         TO MSJ3-HTML
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
006680        MOVE "BOM105"         TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SOBRETASAS.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" 
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-SOBRE-W      TO MSJ2-HTML
006680        MOVE "BOM105"         TO MSJ3-HTML
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
006680        MOVE "BOM105"         TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.
001105
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
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W, COMP-W
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
001614     INITIALIZE NOM-USUAR-W NOM-SURT-W NOM-COMBU-W NOM-PLANO-W NOM-TER-W
001615     MOVE "\\" TO NOM-USUAR-W
001616
001617     INSPECT NOM-USUAR-W REPLACING FIRST "                    "
001618                                    BY IP-DATOS-USUNET
001621     INSPECT NOM-USUAR-W REPLACING FIRST " "
001624                                     BY "\"
007292     INSPECT NOM-USUAR-W REPLACING FIRST "                              "
007293                                     BY DIRECTORIO-LLEGAD-W

           MOVE NOM-USUAR-W  TO  NOM-SURT-W NOM-COMBU-W NOM-TER-W NOM-ART-W NOM-SOBRE-W

           INSPECT NOM-SURT-W REPLACING FIRST "                       "
                                           BY "\CONTROL\SC-ARCHSUR.DAT".

007190     INSPECT NOM-USUAR-W  REPLACING FIRST "                       "
007200                                       BY "\CONTROL\SC-ARCHUSU.DAT".

           INSPECT NOM-COMBU-W REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCOMBU.DAT".


           INSPECT NOM-TER-W   REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHTER.DAT".

           INSPECT NOM-ART-W   REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-MAESART.DAT".

           INSPECT NOM-SOBRE-W  REPLACING FIRST "                       "
                                             BY "\CONTROL\SC-ARCHSOB.DAT".

           MOVE "C:\PROSOFT\TEMP\SC-LISTCOMB-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON" TO  NOM-PLANO-W

           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                             BY SESION-LLEGAD-W.

       LEER-USUARIO.

           OPEN INPUT ARCHIVO-USUARIOS
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.

       ABRIR-COMBUSTIBLES.
           OPEN INPUT ARCHIVO-COMBUSTIBLES.
           MOVE COMP-W       TO NRO-COMB
           READ ARCHIVO-COMBUSTIBLES WITH NO LOCK
                INVALID KEY 
                        MOVE "99"          TO MSJ1-HTML
                        MOVE "01"          TO MSJ2-HTML
                        MOVE "BOM105"      TO MSJ3-HTML
                        GO TO ENVIAR2-ERROR
           END-READ.

       LEER-COMBUSTIBLES.

           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT ARCHIVO-TERCEROS
                      ARCHIVO-SOBRETASAS
                      ARCHIVO-SURTIDORES
                      MAESTRO-ARTICULOS.

           INITIALIZE REG-PLANO VARIABLES I.
           MOVE "{*SURTIDORES*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.

           PERFORM SUMAR-VALORES VARYING I FROM 1 BY 1
                                          UNTIL I > 15.

           INITIALIZE J.

           PERFORM SUMAR-DEUDORES VARYING J FROM 1 BY 1
                                           UNTIL J > 200.


           MOVE "],*TBLA-DEUD*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.
           INITIALIZE I



           PERFORM MOSTRAR-TABLA-DEUD VARYING I FROM 1 BY 1
                                               UNTIL I > 200.


           MOVE "]}" TO DATOS-PLANO
           WRITE REG-PLANO.


           CLOSE  ARCHIVO-JSON
                  ARCHIVO-SOBRETASAS
                  ARCHIVO-SURTIDORES
                  ARCHIVO-TERCEROS
                  MAESTRO-ARTICULOS

           MOVE 00            TO DAT0-ENV
           MOVE NRO-COMB      TO NRO-ENV
           MOVE FECHA-COMB    TO FECHA-ENV
           MOVE TURNO-COMB    TO TURNO-ENV
           MOVE DETALLE-COMB  TO DETALLE-ENV
           MOVE OPER-COMB     TO OPER-ENV
           MOVE FEC-REST-COMB TO FEC-REST-ENV

           MOVE CANTIDAD-T    TO CANT-EDIT
           MOVE CANT-EDIT     TO CANTIDAD-ENV

           MOVE VLR-TOT-VENTA TO VALOR-EDIT   
           MOVE VALOR-EDIT    TO VALOR-TOT-ENV

           MOVE VALOR-FINANC-COMB TO VALOR-EDIT
           MOVE VALOR-EDIT        TO VALOR-FIN-ENV

           MOVE VLR-TOT-DEUD  TO VALOR-EDIT
           MOVE VALOR-EDIT    TO VALOR-TOT-DEU-ENV

           MOVE VALOR-CHEQUE  TO VALOR-EDIT
           MOVE VALOR-EDIT    TO VALOR-CHEQUE-ENV

           MOVE VALOR-NETO-COMB TO VALOR-EDIT
           MOVE VALOR-EDIT      TO VALOR-NETO-ENV

           MOVE VEND-COMB     TO VEND-ENV.


002147 ENVIO-DATOS.
004378     MOVE "datosrecibidos"  TO COBW3-CNV-NAME
004393     MOVE DATOS-ENVIO  TO COBW3-CNV-VALUE
004403     CALL "COBW3_SET_CNV"   USING COBW3
004413     MOVE "../PAGINAS/RECIBIDOS.ASPX"  TO SALIDA-HTML
004430     PERFORM ABRIR-HTML.

002545 CERRAR-SESION.
002546     CALL "COBW3_FREE" USING COBW3.
002547     MOVE 1 TO PROGRAM-STATUS.
002548     EXIT PROGRAM.


       SUMAR-VALORES.

           MOVE ","               TO CIERRE-LIN-1
           IF I = 15 MOVE SPACE   TO CIERRE-LIN-1.

           MOVE CANTIDAD-COMB (I) TO CANT-EDIT
           MOVE CANT-EDIT         TO CANTIDAD-COMB-J

           MOVE NUM-ANT-COMB (I)  TO CANT-EDIT
           MOVE CANT-EDIT         TO NUM-ANT-COMB-J


           ADD  CANTIDAD-COMB (I) TO CANTIDAD-T

           COMPUTE VENTA-W = VALOR-COMB  (I) + SOBRET-COMB (I) + GLOBAL-COMB (I)
           MOVE VENTA-W           TO VALOR-EDIT
           MOVE VALOR-EDIT        TO VALOR-COMB-J

           ADD VENTA-W            TO VLR-TOT-VENTA

           MOVE SURTIDOR-COMB (I) TO SURTIDOR-J

           MOVE SURTIDOR-J        TO COD-SURTI
           READ ARCHIVO-SURTIDORES WITH NO LOCK
                 INVALID KEY CONTINUE
           END-READ

           INITIALIZE ART-W

           MOVE PRODUCTO-SURTI TO COD-ART-W
           MOVE ART-W          TO COD-ART

           READ MAESTRO-ARTICULOS WITH NO LOCK
                INVALID KEY MOVE ZEROS TO VR-VENTA1-ART (1) COD-ART
           END-READ

           MOVE PRODUCTO-SURTI     TO COD-ART-J

           MOVE VR-VENTA1-ART (1)  TO CANT2-EDIT
           MOVE CANT2-EDIT         TO VR-VENTA-ART-J

           MOVE PRODUCTO-SURTI TO ART-SOBRE

           READ ARCHIVO-SOBRETASAS WITH NO LOCK
                INVALID KEY
                        MOVE 0 TO VALOR-SOBRE VALOR-GLOBAL-SOBRE
           END-READ

           MOVE VALOR-SOBRE         TO CANT2-EDIT
           MOVE CANT2-EDIT          TO SOBRET-J

           MOVE VALOR-GLOBAL-SOBRE  TO CANT2-EDIT
           MOVE CANT2-EDIT          TO GLB-SOBRET-J

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA

           MOVE LIN-1 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE.

       SUMAR-DEUDORES.

           IF COD-DEU-COMB (J) = SPACES OR LOW-VALUES
              CONTINUE
           ELSE
              ADD VLR-DEU-COMB (J)   TO VLR-TOT-DEUD
           END-IF.

       MOSTRAR-TABLA-DEUD.

           MOVE ","               TO CIERRE-LIN-2
           IF I = 200 MOVE SPACE  TO CIERRE-LIN-2.

           MOVE COD-DEU-COMB (I)  TO COD-DEUD-J
           MOVE NIT-DEU-COMB (I)  TO NIT-EDIT-J COD-TER.
           MOVE NIT-EDIT-J        TO NIT-J
           MOVE DOC-DEU-COMB (I)  TO DOCUM-DEUD-J

           MOVE ART-DEU-COMB (I)  TO ART-DEUD-J

           MOVE PLACA-COMB (I)    TO PLACA-DEUD-J
           MOVE CANT-DEU-COMB (I) TO VALOR-EDIT2
           MOVE VALOR-EDIT2       TO CANT-DEUD-J

           MOVE VLR-DEU-COMB (I)  TO VALOR-EDIT2
           MOVE VALOR-EDIT2       TO VALOR-DEUD-J
           INSPECT VALOR-DEUD-J   REPLACING ALL ";" BY "0"


           IF COD-TER IS ZERO
                MOVE SPACES TO DESCRIP-TER
           ELSE
                READ ARCHIVO-TERCEROS WITH NO LOCK
                     INVALID KEY
                             MOVE SPACES TO DESCRIP-TER
                END-READ
           END-IF

           MOVE DESCRIP-TER       TO DESCRIPT-TER-J

           INSPECT LIN-2 REPLACING ALL "*" BY CARAC-COMILLA

           MOVE LIN-2 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE.


002566 COPY "P:\PROG\PROYECT\FUENTES\SC-WEB.CBL".
002567
