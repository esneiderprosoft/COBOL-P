004820*=================================================================
004830* GUARDADO, CONSULTA Y RETIRO DE TABLA DE MACROS PARA ACTUALIZACION
      * DE CONTROLES
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
004920     COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-MACRO-EVOL LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-MACRO-W;
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-MACROEVOL
                  ALTERNATE RECORD KEY DETALLE-MACROEVOL WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-PLANO
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION LINE SEQUENTIAL.


005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
005110 COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-MACREV.CBL".	   

       FD  ARCHIVO-PLANO
           LABEL RECORD STANDARD.
       01  REG-PLANO.
           02 DATOS-PLANO      PIC X(29000).

005190 WORKING-STORAGE SECTION.
005200 COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
005210 COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".

       01 LINEA-LLEGADA-W               PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W             PIC X(15).
             03 FECHA-LLEGAD-W          PIC X(8).
             03 HORA-LLEGAD-W           PIC X(6).
          02 DIR-CONTAB-LLEGADA-W       PIC X(30).
          02 MES-CONTAB-LLEGADA-W       PIC XX.
          02 OPERADOR-LLEGADA-W         PIC X(4).
          02 PASO-W                     PIC X.
          02 NOVEDAD-LLEGADA-W          PIC X.
          02 LLAVE-MACRO-LLEGADA-W      PIC X(7).
          02 DETALLE-LLEGADA-W          PIC X(40).
          02 FORMATO-CONSEN-LLEGADA-W   PIC X(5).
          02 NOM-PLANO-LLEGADA-W        PIC X(28).

       77 MES-SAL-W                     PIC X(4).
       77 NOM-MACRO-W                   PIC X(70).
       77 NOM-PLANO-W                   PIC X(70).
       77 TABLA-LLEGADA-W               PIC X(28500).
       77 TABLA-VIAS-LLEGADA-W          PIC X(12).
       77 TABLA-FORM-LLEGADA-W          PIC X(6).
       77 TABLA-COMANDOS-LLEGADA-W      PIC X(150).

       01 REG-W.
          02 LLAVE-W.             
             03 CL-W                    PIC X.
             03 CODIGO-W                PIC 9(6).
          02 DETALLE-W                  PIC X(40).
          02 TABLA-W.
             03 RENGLON-W               PIC X(95) OCCURS 300.
          02 TABLA-VIAS-W.
             03 VIAS-W                  PIC X(2) OCCURS 6.
          02 FILLER                     PIC X(47).
          02 FORMATO-CONSEN-W           PIC X(5).
          02 TABLA-FORM-W.
             03 COD-FORM-W              PIC XX OCCURS 3.
          02 COMANDOS-W.
             03 PROG-W                  PIC X(10) OCCURS 15.
          02 OPER-W                     PIC X(4).
          02 FECHA-ACT-W                PIC X(6).

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV             PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 DETALLE-MACROEVOL-ENV      PIC X(40).
          02 FILLER                     PIC X VALUE "|".
          02 VIAS1-MACROEVOL-ENV        PIC X(2).
          02 FILLER                     PIC X VALUE "|".
          02 VIAS2-MACROEVOL-ENV        PIC X(2).
          02 FILLER                     PIC X VALUE "|".
          02 VIAS3-MACROEVOL-ENV        PIC X(2).
          02 FILLER                     PIC X VALUE "|".
          02 VIAS4-MACROEVOL-ENV        PIC X(2).
          02 FILLER                     PIC X VALUE "|".
          02 VIAS5-MACROEVOL-ENV        PIC X(2).
          02 FILLER                     PIC X VALUE "|".
          02 VIAS6-MACROEVOL-ENV        PIC X(2).
          02 FILLER                     PIC X VALUE "|".
          02 FORMATO-CONSEN-ENV         PIC X(5).
          02 FILLER                     PIC X VALUE "|".
          02 COD1-FORM-MACROEVOL-ENV    PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 COD2-FORM-MACROEVOL-ENV    PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 COD3-FORM-MACROEVOL-ENV    PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 PROG1-MACROEVOL-ENV        PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 PROG2-MACROEVOL-ENV        PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 PROG3-MACROEVOL-ENV        PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 PROG4-MACROEVOL-ENV        PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 PROG5-MACROEVOL-ENV        PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 PROG6-MACROEVOL-ENV        PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 PROG7-MACROEVOL-ENV        PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 PROG8-MACROEVOL-ENV        PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 PROG9-MACROEVOL-ENV        PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 PROG10-MACROEVOL-ENV       PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 PROG11-MACROEVOL-ENV       PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 PROG12-MACROEVOL-ENV       PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 PROG13-MACROEVOL-ENV       PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 PROG14-MACROEVOL-ENV       PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 PROG15-MACROEVOL-ENV       PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 OPER-MACROEVOL-ENV         PIC X(4).
          02 FILLER                     PIC X VALUE "|".
          02 FECH-OPER-MACROEVOL-ENV    PIC X(8).
          02 FILLER                     PIC X VALUE "|".
          02 NOMBRE-TXT-ENV             PIC X(28).

006310 LINKAGE SECTION.
006320 COPY "P:\PROG\PROYECT\FUENTES\ISAPICTX.CBL".
006330 PROCEDURE DIVISION WITH stdcall LINKAGE USING ISAPI-CTX-CNT.
006340
006350 DECLARATIVES.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUNET.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USU-W                TO MSJ2-HTML
              MOVE "HC107"                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.         
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-SESION-W             TO MSJ2-HTML
              MOVE "HC107"                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-MACRO-EVOL.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-MACRO-W              TO MSJ2-HTML
              MOVE "HC107"                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PLANO.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PLANO-W              TO MSJ2-HTML
              MOVE "HC107"                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

006720  END DECLARATIVES.
006730
006740 INICIAR-IIS.
006750     MOVE LOW-VALUE TO COBW3.
006760     MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
006770     CALL "COBW3_INIT" USING COBW3.
006780
006790 LEER-DATO-HTML.
006800     MOVE "datosh" TO COBW3-SEARCH-DATA.
006810     CALL "COBW3_GET_VALUE" USING COBW3.
006820     MOVE COBW3-GET-DATA    TO LINEA-LLEGADA-W.

           UNSTRING LINEA-LLEGADA-W DELIMITED BY "|"
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W,
                    OPERADOR-LLEGADA-W, PASO-W, NOVEDAD-LLEGADA-W, LLAVE-MACRO-LLEGADA-W,
                    DETALLE-LLEGADA-W, FORMATO-CONSEN-LLEGADA-W, NOM-PLANO-LLEGADA-W
           END-UNSTRING.

001855     MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.

000797 INICIAR-SESION.
000806     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.
006840
006900 ABRIR-USUARIO.
006910     INITIALIZE OTR-STAT.
006920     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
006930
006940     OPEN INPUT ARCHIVO-USUNET
006950     EVALUATE OTR-STAT
006960       WHEN "00"  CONTINUE
006970       WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
006980                  INITIALIZE REG-USUNET
006990                  WRITE REG-USUNET
007000                  CLOSE ARCHIVO-USUNET
007010                  MOVE "Validacion de Usuario"       TO MSJ1-HTML
007020                  MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007030                  MOVE "Sc"                          TO MSJ3-HTML
007040                  GO TO ENVIAR2-ERROR
007050       WHEN OTHER GO TO CERRAR-SESION
007060     END-EVALUATE.
007070
007080     INITIALIZE LLAVE-USUNET.

007100
007110 LEER-USUARIO.
007120     READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
007140     CLOSE ARCHIVO-USUNET.
007200     
007210     IF NOMBRE-USUNET = SPACES
007230        MOVE "Validacion de Sesion"        TO MSJ1-HTML
007240        MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007250        MOVE "Sc"                          TO MsJ3-HTML
007260        GO TO ENVIAR2-ERROR
007270     END-IF.
007280 
007350

        ASIGNAR-NOMBRES.
           EVALUATE MES-CONTAB-LLEGADA-W
             WHEN 01 MOVE "\ENE" TO MES-SAL-W 
             WHEN 02 MOVE "\FEB" TO MES-SAL-W 
             WHEN 03 MOVE "\MAR" TO MES-SAL-W 
             WHEN 04 MOVE "\ABR" TO MES-SAL-W 
             WHEN 05 MOVE "\MAY" TO MES-SAL-W 
             WHEN 06 MOVE "\JUN" TO MES-SAL-W 
             WHEN 07 MOVE "\JUL" TO MES-SAL-W 
             WHEN 08 MOVE "\AGT" TO MES-SAL-W 
             WHEN 09 MOVE "\SEP" TO MES-SAL-W 
             WHEN 10 MOVE "\OCT" TO MES-SAL-W 
             WHEN 11 MOVE "\NOV" TO MES-SAL-W 
             WHEN 12 MOVE "\DIC" TO MES-SAL-W 
           END-EVALUATE
           
    

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

001614     INITIALIZE NOM-MACRO-W

           MOVE "\\" TO NOM-MACRO-W

           INSPECT NOM-MACRO-W REPLACING FIRST "                    "
                                           BY IP-DATOS-USUNET

007190     INSPECT NOM-MACRO-W REPLACING FIRST "                           "
                                            BY "\PROG\DATOS\SC-MACRO-EV.DAT".

        EVALUAR-PASO.
           EVALUATE PASO-W
             WHEN 1 GO TO MODIFICAR-ARCHIVO
             WHEN 2 GO TO CONSULTAR-ARCHIVO
           END-EVALUATE.

        MODIFICAR-ARCHIVO.
           MOVE NOM-PLANO-JSON-W TO NOM-PLANO-W

           INSPECT NOM-PLANO-W REPLACING FIRST "                            "
                                            BY NOM-PLANO-LLEGADA-W.

           INSPECT NOM-PLANO-W REPLACING FIRST "    "
                                            BY ".TXT".

           OPEN INPUT ARCHIVO-MACRO-EVOL
                      ARCHIVO-PLANO.

        LEER-PLANO.
           READ ARCHIVO-PLANO WITH NO LOCK
           END-READ.

           UNSTRING REG-PLANO DELIMITED BY "|" 
            INTO TABLA-W, TABLA-VIAS-W, TABLA-FORM-W, COMANDOS-W
           END-UNSTRING.

        BUSCAR-ARCHIVOS.
           MOVE LLAVE-MACRO-LLEGADA-W   TO LLAVE-MACROEVOL
           READ ARCHIVO-MACRO-EVOL WITH NO LOCK
                 INVALID KEY
                   MOVE "01"  TO SW-INVALID-ENV
                 NOT INVALID KEY
                   MOVE "00"  TO SW-INVALID-ENV
           END-READ

           IF NOVEDAD-LLEGADA-W = 7 AND SW-INVALID-ENV = "00" GO TO CERRAR-ARCHIVOS.
           IF NOVEDAD-LLEGADA-W = 7 AND SW-INVALID-ENV = "01" GO TO NUEVO.
           IF NOVEDAD-LLEGADA-W = 8 AND SW-INVALID-ENV = "00" GO TO CAMBIO.
           IF NOVEDAD-LLEGADA-W = 8 AND SW-INVALID-ENV = "01" GO TO CERRAR-ARCHIVOS.
           IF NOVEDAD-LLEGADA-W = 9 AND SW-INVALID-ENV = "00" GO TO RETIRO.
           IF NOVEDAD-LLEGADA-W = 9 AND SW-INVALID-ENV = "01" GO TO CERRAR-ARCHIVOS.


        NUEVO.
           INITIALIZE REG-W.

           MOVE OPERADOR-LLEGADA-W       TO OPER-W
           ACCEPT FECHA-ACT-W            FROM DATE
           MOVE LLAVE-MACRO-LLEGADA-W    TO LLAVE-W
           MOVE DETALLE-LLEGADA-W        TO DETALLE-W
           MOVE FORMATO-CONSEN-LLEGADA-W TO FORMATO-CONSEN-W.

        GRABAR.
           MOVE REG-W TO REG-MACROEVOL.
           CLOSE    ARCHIVO-MACRO-EVOL.
           OPEN I-O ARCHIVO-MACRO-EVOL.

           WRITE    REG-MACROEVOL END-WRITE

           CLOSE    ARCHIVO-MACRO-EVOL.
           GO TO    PAGINA-CONFIG.

        CAMBIO.
           MOVE OPERADOR-LLEGADA-W       TO OPER-W
           ACCEPT FECHA-ACT-W            FROM DATE
           MOVE LLAVE-MACROEVOL          TO LLAVE-W
           MOVE DETALLE-MACROEVOL        TO DETALLE-W
           MOVE FORMATO-CONSEN-MACROEVOL TO FORMATO-CONSEN-W
           MOVE REG-W                    TO REG-MACROEVOL.
           CLOSE    ARCHIVO-MACRO-EVOL.

           OPEN I-O ARCHIVO-MACRO-EVOL.

           REWRITE  REG-MACROEVOL

           CLOSE    ARCHIVO-MACRO-EVOL.

           GO TO    PAGINA-CONFIG.

        RETIRO.
           CLOSE    ARCHIVO-MACRO-EVOL.

           OPEN I-O ARCHIVO-MACRO-EVOL.
           
           DELETE   ARCHIVO-MACRO-EVOL.

           CLOSE    ARCHIVO-MACRO-EVOL.

           GO TO    PAGINA-CONFIG.

        CONSULTAR-ARCHIVO.
           OPEN INPUT ARCHIVO-MACRO-EVOL.        

           MOVE LLAVE-MACRO-LLEGADA-W   TO LLAVE-MACROEVOL
           READ ARCHIVO-MACRO-EVOL WITH NO LOCK
                 INVALID KEY
                   INITIALIZE DATOS-ENVIO
                   MOVE "01"  TO SW-INVALID-ENV
                   CLOSE ARCHIVO-MACRO-EVOL
                   GO TO PAGINA-CONFIG
                 NOT INVALID KEY
                   MOVE "00"  TO SW-INVALID-ENV
           END-READ

           MOVE DETALLE-MACROEVOL        TO DETALLE-MACROEVOL-ENV
           MOVE VIAS-MACROEVOL (1)       TO VIAS1-MACROEVOL-ENV
           MOVE VIAS-MACROEVOL (2)       TO VIAS2-MACROEVOL-ENV
           MOVE VIAS-MACROEVOL (3)       TO VIAS3-MACROEVOL-ENV
           MOVE VIAS-MACROEVOL (4)       TO VIAS4-MACROEVOL-ENV
           MOVE VIAS-MACROEVOL (5)       TO VIAS5-MACROEVOL-ENV
           MOVE VIAS-MACROEVOL (6)       TO VIAS6-MACROEVOL-ENV
           MOVE FORMATO-CONSEN-MACROEVOL TO FORMATO-CONSEN-ENV
           MOVE COD-FORM-MACROEVOL (1)   TO COD1-FORM-MACROEVOL-ENV
           MOVE COD-FORM-MACROEVOL (2)   TO COD2-FORM-MACROEVOL-ENV
           MOVE COD-FORM-MACROEVOL (3)   TO COD3-FORM-MACROEVOL-ENV
           MOVE PROG-MACROEVOL (1)       TO PROG1-MACROEVOL-ENV
           MOVE PROG-MACROEVOL (2)       TO PROG2-MACROEVOL-ENV
           MOVE PROG-MACROEVOL (3)       TO PROG3-MACROEVOL-ENV
           MOVE PROG-MACROEVOL (4)       TO PROG4-MACROEVOL-ENV
           MOVE PROG-MACROEVOL (5)       TO PROG5-MACROEVOL-ENV
           MOVE PROG-MACROEVOL (6)       TO PROG6-MACROEVOL-ENV
           MOVE PROG-MACROEVOL (7)       TO PROG7-MACROEVOL-ENV
           MOVE PROG-MACROEVOL (8)       TO PROG8-MACROEVOL-ENV
           MOVE PROG-MACROEVOL (9)       TO PROG9-MACROEVOL-ENV
           MOVE PROG-MACROEVOL (10)      TO PROG10-MACROEVOL-ENV
           MOVE PROG-MACROEVOL (11)      TO PROG11-MACROEVOL-ENV
           MOVE PROG-MACROEVOL (12)      TO PROG12-MACROEVOL-ENV
           MOVE PROG-MACROEVOL (13)      TO PROG13-MACROEVOL-ENV
           MOVE PROG-MACROEVOL (14)      TO PROG14-MACROEVOL-ENV
           MOVE PROG-MACROEVOL (15)      TO PROG15-MACROEVOL-ENV
           MOVE OPER-MACROEVOL           TO OPER-MACROEVOL-ENV
           MOVE FECH-OPER-MACROEVOL      TO FECH-OPER-MACROEVOL-ENV.

        ESCRIBIR-PLANO.
           OPEN OUTPUT ARCHIVO-PLANO
           MOVE TABLA-MACROEVOL         TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE
           CLOSE ARCHIVO-PLANO

           MOVE NOM-JSON-W TO NOMBRE-TXT-ENV

           INSPECT NOMBRE-TXT-ENV REPLACING FIRST ".JSON"
                                               BY ".TXT ".
        CERRAR-ARCHIVOS.
           CLOSE ARCHIVO-MACRO-EVOL
                 ARCHIVO-PLANO.

010700  PAGINA-CONFIG.
010720     MOVE "datosrecibidos"    TO COBW3-CNV-NAME
010730     MOVE DATOS-ENVIO         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3
010750
010760     MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
010770     PERFORM ABRIR-HTML.
007980   
007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.

008030
008040 COPY "P:\PROG\PROYECT\FUENTES\SC-WEB.CBL".
