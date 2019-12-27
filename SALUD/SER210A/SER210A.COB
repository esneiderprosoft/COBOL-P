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
004910     COPY "..\..\FUENTES\FS-USUNET.CBL".
004920     COPY "..\..\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-MACRO-EVOL LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-MACRO-W;
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-MACROEVOL
                  ALTERNATE RECORD KEY DETALLE-MACROEVOL WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-MACREV.CBL".

005190 WORKING-STORAGE SECTION.
005200 COPY "..\..\FUENTES\COBW3.CBL".
005210 COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LINEA-LLEGADA-W               PIC X(200).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W             PIC X(15).
             03 FECHA-LLEGAD-W          PIC X(8).
             03 HORA-LLEGAD-W           PIC X(6).
          02 DIR-CONTAB-LLEGADA-W       PIC X(30).
          02 MES-CONTAB-LLEGADA-W       PIC XX.
          02 LLAVE-MACRO-LLEGADA-W.
             03 CL-MACRO-LLEGADA-W      PIC X.
             03 CODIGO-MACRO-LLEGADA-W  PIC X(6).
          02 DESCRIP-LLEGADA-W          PIC X(40).
          02 DESCRIP2-LLEGADA-W         PIC X(60).
          02 TIPO-LLEGADA-W             PIC X.

       77 MES-SAL-W                     PIC X(4).
       77 NOM-MACRO-W                   PIC X(70).
       77 DATOS-PLANO-W                 PIC X(1024).

       01 TABLA-MACROEVOL-W.
          02 RENGLONES-W                PIC X(1000) OCCURS 29.

       01 LIN-1.
          02 FILLER                     PIC X(16) VALUE "{*NOMBRE_PACI*:".
          02 FILLER                     PIC X     VALUE "*".
          02 DESCRIP-MACROEVOL-ENV      PIC X(40).
          02 FILLER                     PIC XX    VALUE "*,".
          02 FILLER                     PIC X(13) VALUE "*NOMBRE_EXA*:".
          02 FILLER                     PIC X     VALUE "*".
          02 DESCRIP2-MACROENV-ENV      PIC X(60).
          02 FILLER                     PIC XX    VALUE "*,".
          02 FILLER                     PIC X(14) VALUE "*LLAVE_MACRO*:".
          02 FILLER                     PIC X     VALUE "*".
          02 LLAVE-MACRO-ENV            PIC X(7).
          02 FILLER                     PIC XX    VALUE "*,".
          02 FILLER                     PIC X(13) VALUE "*TABL_MACRO*:".
          02 FILLER                     PIC X     VALUE "[".

       01 LIN-5.
          02 FILLER                     PIC X(8)  VALUE "{*RENG*:".
          02 FILLER                     PIC X     VALUE "*".
          02 RENG-MACRO-ENV             PIC X(1000).
          02 CIERRE-LIN-5               PIC X(3)  VALUE "*},".


006310 LINKAGE SECTION.
006320 COPY "..\..\FUENTES\ISAPICTX.CBL".
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
                    LLAVE-MACRO-LLEGADA-W,

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

           INSPECT NOM-MACRO-W REPLACING FIRST "                           "
                                            BY "\PROG\DATOS\SC-MACRO-EV.DAT".

        CONSULTAR-ARCHIVO.
           OPEN INPUT ARCHIVO-MACRO-EVOL.

           MOVE LLAVE-MACRO-LLEGADA-W   TO LLAVE-MACROEVOL
           READ ARCHIVO-MACRO-EVOL WITH NO LOCK
                 INVALID KEY
                   INITIALIZE  LLAVE-MACRO-LLEGADA-W
                               DESCRIP-LLEGADA-W
                               DESCRIP2-LLEGADA-W
                               TIPO-LLEGADA-W
           END-READ

           MOVE LLAVE-MACROEVOL     TO LLAVE-MACRO-ENV
           MOVE DESCRIP-LLEGADA-W   TO DESCRIP-MACROEVOL-ENV
           MOVE DESCRIP2-LLEGADA-W  TO DESCRIP2-MACROENV-ENV


           MOVE "{*MACROEVOL*:["         TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           MOVE TABLA-MACROEVOL          TO TABLA-MACROEVOL-W

           PERFORM MONTAR-MACROEVOL VARYING I FROM 1 BY 1
                                             UNTIL I > 29
           MOVE "]}," TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           INITIALIZE LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           INITIALIZE LIN-5
           MOVE "*} "             TO CIERRE-LIN-5
           INSPECT LIN-5 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-5 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           MOVE "]}]}" TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           CLOSE ARCHIVO-MACRO-EVOL.

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
      
       COPY "..\..\FUENTES\SC-WEB19.CBL".
