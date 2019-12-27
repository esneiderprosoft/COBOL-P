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

       01 LINEA-LLEGADA-W               PIC X(200).

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
          02 LLAVE-MACRO-LLEGADA-W.
             03 CL-MACRO-LLEGADA-W      PIC X.
             03 CODIGO-MACRO-LLEGADA-W  PIC X(6).
          02 DETALLE-LLEGADA-W          PIC X(40).
          02 FORMATO-CONSEN-LLEGADA-W   PIC X(5).
          02 NOM-PLANO-LLEGADA-W        PIC X(28).

       77 MES-SAL-W                     PIC X(4).
       77 NOM-MACRO-W                   PIC X(70).
       77 NOM-PLANO-W                   PIC X(70).
       77 LINEA-PLANO-W                 PIC X(28500).
       77 OPC-PLANO-W                   PIC X(17).
       77 TABLATXT-VIAS                 PIC X(12).
       77 TABLATXT-COMANDOS             PIC X(150).
       77 TABLATXT-FORM                 PIC X(6).
       77 DATOS-PLANO-W                 PIC X(1024).

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

       01 TABLA-MACROEVOL-W.
          02 RENGLONES-W                PIC X(1000) OCCURS 29.

       01 LIN-1.
          02 FILLER                     PIC X(17) VALUE "{*DETALLE_MACRO*:".
          02 FILLER                     PIC X     VALUE "*".
          02 DETALLE-MACROEVOL-ENV      PIC X(40).
          02 FILLER                     PIC XX    VALUE "*,".
          02 FILLER                     PIC X(17) VALUE "*FORMATO_CONSEN*:".
          02 FILLER                     PIC X     VALUE "*".
          02 FORMATO-CONSEN-ENV         PIC X(5).
          02 FILLER                     PIC XX    VALUE "*,".
          02 FILLER                     PIC X(13) VALUE "*OPER_MACRO*:".
          02 FILLER                     PIC X     VALUE "*".
          02 OPER-MACROEVOL-ENV         PIC X(4).
          02 FILLER                     PIC XX    VALUE "*,".
          02 FILLER                     PIC X(14) VALUE "*FECHA_MACRO*:".
          02 FILLER                     PIC X     VALUE "*".
          02 FECH-OPER-MACROEVOL-ENV    PIC X(8).
          02 FILLER                     PIC XX    VALUE "*,".
          02 FILLER                     PIC X(15) VALUE "*VIAS_MACRO*:".
          02 FILLER                     PIC X     VALUE "[".

       01 LIN-2.
          02 FILLER                     PIC X(8)  VALUE "{*VIAS*:".
          02 FILLER                     PIC X     VALUE "*".
          02 VIAS-MACROEVOL-ENV         PIC X(2).
          02 CIERRE-LIN-2               PIC X(3)  VALUE "*},".
      *    02 FILLER                     PIC X(2)  VALUE "],".


       01 LIN-3.
      *    02 FILLER                     PIC X(11) VALUE "*COD_FORM*:".
      *    02 FILLER                     PIC X     VALUE "[".
          02 FILLER                     PIC X(7)  VALUE "{*COD*:".
          02 FILLER                     PIC X     VALUE "*".
          02 COD-FORM-MACROEVOL-ENV     PIC XX.
          02 CIERRE-LIN-3               PIC X(3)  VALUE "*},".
      *    02 FILLER                     PIC X(2)  VALUE "],".

       01 LIN-4.
      *    02 FILLER                     PIC X(11) VALUE "*COMANDOS*:".
      *    02 FILLER                     PIC X     VALUE "[".
          02 FILLER                     PIC X(8)  VALUE "{*PROG*:".
          02 FILLER                     PIC X     VALUE "*".
          02 PROG-MACROEVOL-ENV         PIC X(10).
          02 CIERRE-LIN-4               PIC X(3)  VALUE "*},".
      *    02 FILLER                     PIC X(2)  VALUE "],".

       01 LIN-5.
      *    02 FILLER                     PIC X(14) VALUE "*TABLA_MACRO*:".
      *    02 FILLER                     PIC X     VALUE "[".
          02 FILLER                     PIC X(9)  VALUE "{*RENG1*:".
          02 FILLER                     PIC X     VALUE "*".
          02 RENG-MACRO-ENV             PIC X(1000).
          02 CIERRE-LIN-5               PIC X(3)  VALUE "*},".
      *    02 FILLER                     PIC XX    VALUE "]}".
      *    02 CIERRE-LIN-5               PIC X.


       01 LIN-6.
          02 FILLER                         PIC X(17) VALUE "{*DETALLE_MACRO*:".
          02 FILLER                         PIC X     VALUE "*".
          02 DETALLE-MACROEVOL-J            PIC X(40).
          02 FILLER                         PIC XX    VALUE "*}".
          02 CIERRE-LIN-6                   PIC X.



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
             WHEN 3 GO TO CONSULTAR-MACROEVOL
           END-EVALUATE.

        MODIFICAR-ARCHIVO.
           INITIALIZE REG-W.

           MOVE NOM-PLANO-JSON-W TO NOM-PLANO-W

           INSPECT NOM-PLANO-W REPLACING FIRST "                            "
                                            BY NOM-PLANO-LLEGADA-W.

           INSPECT NOM-PLANO-W REPLACING FIRST "    "
                                            BY ".TXT".

           OPEN INPUT ARCHIVO-MACRO-EVOL
                      ARCHIVO-PLANO.

        LEER-PLANO.
           READ ARCHIVO-PLANO NEXT WITH NO LOCK AT END GO TO BUSCAR-ARCHIVOS.

           UNSTRING DATOS-PLANO DELIMITED BY "$|" 
                    INTO OPC-PLANO-W, LINEA-PLANO-W
           END-UNSTRING

           EVALUATE OPC-PLANO-W
               WHEN "RENG-MACROEVOL"
                    MOVE LINEA-PLANO-W   TO TABLA-W
               WHEN "TABLAS-MACROEVOL"
                    UNSTRING LINEA-PLANO-W DELIMITED BY "|"
                             INTO TABLATXT-FORM, TABLATXT-COMANDOS, TABLATXT-VIAS
                    END-UNSTRING
                    IF TABLATXT-FORM = SPACES OR LOW-VALUES
                       CONTINUE
                    ELSE
                       UNSTRING TABLATXT-FORM DELIMITED BY ","
                         INTO  COD-FORM-W (1), COD-FORM-W (2), COD-FORM-W (3)
                       END-UNSTRING
                    END-IF
                    IF TABLATXT-COMANDOS = SPACES OR LOW-VALUES
                       CONTINUE
                    ELSE
                       UNSTRING TABLATXT-COMANDOS DELIMITED BY ","
                         INTO  PROG-W (1), PROG-W (2), PROG-W (3), PROG-W (4),
                               PROG-W (5), PROG-W (6), PROG-W (7), PROG-W (8),
                               PROG-W (9), PROG-W (10), PROG-W (11), PROG-W (12),
                               PROG-W (13), PROG-W (14), PROG-W (15)
                       END-UNSTRING
                    END-IF
                    IF TABLATXT-VIAS = SPACES OR LOW-VALUES
                       CONTINUE
                    ELSE
                       UNSTRING TABLATXT-VIAS DELIMITED BY ","
                         INTO  VIAS-W (1), VIAS-W (2), VIAS-W (3), VIAS-W (4),
                               VIAS-W (6)
                       END-UNSTRING
                    END-IF
           END-EVALUATE

           GO TO LEER-PLANO.

        BUSCAR-ARCHIVOS.
           MOVE LLAVE-MACRO-LLEGADA-W   TO LLAVE-MACROEVOL
           READ ARCHIVO-MACRO-EVOL WITH NO LOCK
                 INVALID KEY
                   MOVE "01"  TO SW-INVALID
                 NOT INVALID KEY
                   MOVE "00"  TO SW-INVALID
           END-READ

           IF NOVEDAD-LLEGADA-W = 7 AND SW-INVALID = "00" GO TO ENVIAR-ERROR.
           IF NOVEDAD-LLEGADA-W = 7 AND SW-INVALID = "01" GO TO NUEVO.
           IF NOVEDAD-LLEGADA-W = 8 AND SW-INVALID = "00" GO TO CAMBIO.
           IF NOVEDAD-LLEGADA-W = 8 AND SW-INVALID = "01" GO TO ENVIAR-ERROR.
           IF NOVEDAD-LLEGADA-W = 9 AND SW-INVALID = "00" GO TO RETIRO.
           IF NOVEDAD-LLEGADA-W = 9 AND SW-INVALID = "01" GO TO ENVIAR-ERROR.

        ENVIAR-ERROR.
           CLOSE ARCHIVO-MACRO-EVOL
           MOVE "SC-1"          TO MSJ1-HTML
           MOVE "01"            TO MSJ2-HTML
           MOVE "RX424"         TO MSJ3-HTML
           GO TO ENVIAR2-ERROR.

        NUEVO.
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
                   CLOSE ARCHIVO-MACRO-EVOL
                   MOVE "SC-1"          TO MSJ1-HTML
                   MOVE "01"            TO MSJ2-HTML
                   MOVE "RX424"         TO MSJ3-HTML
                   GO TO ENVIAR2-ERROR
           END-READ

           MOVE "{*MACROEVOL*:["         TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO

           MOVE DETALLE-MACROEVOL        TO DETALLE-MACROEVOL-ENV
           MOVE FORMATO-CONSEN-MACROEVOL TO FORMATO-CONSEN-ENV
           MOVE OPER-MACROEVOL           TO OPER-MACROEVOL-ENV
           MOVE FECH-OPER-MACROEVOL      TO FECH-OPER-MACROEVOL-ENV
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           MOVE TABLA-MACROEVOL          TO TABLA-MACROEVOL-W
           MOVE TABLA-FORM-MACROEVOL     TO TABLA-FORM-W
           MOVE TABLA-VIAS-MACROEVOL     TO TABLA-VIAS-W
           MOVE COMANDOS-MACROEVOL       TO COMANDOS-W

           PERFORM MONTAR-VIAS VARYING I FROM 1 BY 1
                                         UNTIL I > 6

           MOVE "]," TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           MOVE "*COD_FORM*:[" TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO

           PERFORM MONTAR-FORM VARYING I FROM 1 BY 1
                                         UNTIL I > 3

           MOVE "]," TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           MOVE "*COMANDOS*:[" TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO

           PERFORM MONTAR-COMANDOS VARYING I FROM 1 BY 1
                                            UNTIL I > 15
           MOVE "]," TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           MOVE "*TABLA_MACRO*:[" TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO

           PERFORM MONTAR-MACROEVOL VARYING I FROM 1 BY 1
                                             UNTIL I > 29
           MOVE "]}," TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           INITIALIZE LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           INITIALIZE LIN-2
           MOVE "*} "          TO CIERRE-LIN-2
           INSPECT LIN-2 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-2 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           MOVE "]," TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           MOVE "*COD_FORM*:[" TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO

           INITIALIZE LIN-3
           MOVE "*} "          TO CIERRE-LIN-3
           INSPECT LIN-3 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-3 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           MOVE "]," TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO


           MOVE "*COMANDOS*:[" TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO
           INITIALIZE LIN-4
           MOVE "*} "          TO CIERRE-LIN-4
           INSPECT LIN-4 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-4 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           MOVE "]," TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           MOVE "*TABLA_MACRO*:[" TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO
           INITIALIZE LIN-5
           MOVE "*} "             TO CIERRE-LIN-5
           INSPECT LIN-5 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-5 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           MOVE "]}]}" TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           CLOSE ARCHIVO-MACRO-EVOL
           GO TO PAGINA-CONFIG.

        CONSULTAR-MACROEVOL.
           MOVE LLAVE-MACRO-LLEGADA-W   TO LLAVE-MACROEVOL
           READ ARCHIVO-MACRO-EVOL WITH NO LOCK
                 INVALID KEY
                   CLOSE ARCHIVO-MACRO-EVOL
                   MOVE "SC-1"          TO MSJ1-HTML
                   MOVE "01"            TO MSJ2-HTML
                   MOVE "RX424"         TO MSJ3-HTML
                   GO TO ENVIAR2-ERROR
           END-READ

           MOVE "{*MACROEVOL*:["       TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO

           MOVE DETALLE-MACROEVOL      TO DETALLE-MACROEVOL-J
           MOVE "," TO CIERRE-LIN-6
           INSPECT LIN-6 REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT LIN-6 REPLACING ALL LOW-VALUES BY SPACES
           MOVE LIN-6 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO.

           INITIALIZE LIN-6
           INSPECT LIN-6 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-6 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           MOVE "]}" TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO.

        PAGINA-CONFIG.
           MOVE "statuscode" TO COBW3-CNV-NAME
           MOVE "00"         TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3

           MOVE "programa-id" TO COBW3-CNV-NAME
           MOVE "RX424"       TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3

           MOVE "..\PAGINAS\RECIBIDOSLISTADO.ASPX" TO SALIDA-HTML
           PERFORM ABRIR-HTML.

       DATOS-ENVIO.
           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE DATOS-PLANO-W    TO COBW3-CNV-VALUE
           CALL "COBW3_SET_REPEAT" USING COBW3.
         
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.

       MONTAR-VIAS.
           MOVE VIAS-W (I)      TO VIAS-MACROEVOL-ENV
           IF I = 6
              MOVE "*} "        TO CIERRE-LIN-2
           END-IF
           INSPECT LIN-2 REPLACING ALL "*"        BY CARAC-COMILLA
           INSPECT LIN-2 REPLACING ALL LOW-VALUES BY SPACES
           MOVE LIN-2 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO.

       MONTAR-FORM.
           MOVE COD-FORM-W (I)  TO COD-FORM-MACROEVOL-ENV
           IF I = 3
              MOVE "*} "        TO CIERRE-LIN-3
           END-IF
           INSPECT LIN-3 REPLACING ALL "*"        BY CARAC-COMILLA
           INSPECT LIN-3 REPLACING ALL LOW-VALUES BY SPACES
           MOVE LIN-3 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO.

       MONTAR-COMANDOS.
           MOVE PROG-W (I)      TO PROG-MACROEVOL-ENV
           IF I = 15
              MOVE "*} "        TO CIERRE-LIN-4
           END-IF
           INSPECT LIN-4 REPLACING ALL "*"        BY CARAC-COMILLA
           INSPECT LIN-4 REPLACING ALL LOW-VALUES BY SPACES
           MOVE LIN-4 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO.

       MONTAR-MACROEVOL.
           MOVE RENGLONES-W (I) TO  RENG-MACRO-ENV
           IF I = 29
              MOVE "*} "        TO CIERRE-LIN-5
           END-IF
           INSPECT LIN-5 REPLACING ALL "*"        BY CARAC-COMILLA
           INSPECT LIN-5 REPLACING ALL "\.br\"    BY "-    "
           INSPECT LIN-5 REPLACING ALL LOW-VALUES BY SPACES
           MOVE LIN-5 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO.
      
       COPY "P:\PROG\PROYECT\FUENTES\SC-WEB19.CBL".
