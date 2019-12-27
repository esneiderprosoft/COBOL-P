004820*=================================================================
004830* CONSULTA DE NUMERO DE FACTURA Y ORDEN DE SALIDA.
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "..\..\FUENTES\FS-USUNET.CBL".
004920     COPY "..\..\FUENTES\FS-SESION.CBL".

            SELECT ARCHIVO-NUMERACION LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-NUMER-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-NUM
                  ALTERNATE RECORD KEY NIT-NUM         WITH DUPLICATES
                  ALTERNATE RECORD KEY DESCRIP-NUM     WITH DUPLICATES
                  ALTERNATE RECORD KEY NOMBRE-PAC-NUM  WITH DUPLICATES
                  ALTERNATE RECORD KEY HAB-NUM         WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-ING-NUM   WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-RET-NUM   WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

      *      SELECT ARCHIVO-ORDEN-SALIDA LOCK MODE IS AUTOMATIC
      *            ASSIGN NOM-SALID-W
      *            ORGANIZATION IS INDEXED;
      *            ACCESS MODE  IS DYNAMIC
      *            RECORD KEY   IS LLAVE-NUM
      *            ALTERNATE RECORD KEY NIT-NUM         WITH DUPLICATES
      *            ALTERNATE RECORD KEY DESCRIP-NUM     WITH DUPLICATES
      *            ALTERNATE RECORD KEY NOMBRE-PAC-NUM  WITH DUPLICATES
      *            ALTERNATE RECORD KEY HAB-NUM         WITH DUPLICATES
      *            ALTERNATE RECORD KEY FECHA-ING-NUM   WITH DUPLICATES
      *            ALTERNATE RECORD KEY FECHA-RET-NUM   WITH DUPLICATES
      *            FILE STATUS IS OTR-STAT.

005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-NUMER.CBL".
      * COPY "..\..\FUENTES\FD-SALID.CBL". 

005190 WORKING-STORAGE SECTION.
005200 COPY "..\..\FUENTES\COBW3.CBL".
005210 COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LINEA-LLEGADA-W               PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W             PIC X(15).
             03 FECHA-LLEGAD-W          PIC X(8).
             03 HORA-LLEGAD-W           PIC X(6).
          02 DIR-CONTAB-LLEGADA-W       PIC X(30).
          02 MES-CONTAB-LLEGADA-W       PIC XX.
          02 OPERADOR-LLEGADA-W         PIC X(4).
          02 LLAVE-LLEGADA-W.
             05 PREFIJO-LLEGADA-W       PIC X.
             05 NRO-LLEGADA-W           PIC X(6).

       77 MES-SAL-W                     PIC X(4).
       77 NOM-NUMER-W                   PIC X(70).
       77 NOM-SALID-W                   PIC X(70).

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV              PIC XX.
          02 FILLER                      PIC X      VALUE "|".
          02 LLAVE-SALID-NUM-ENV         PIC 9(10).
          02 FILLER                      PIC X      VALUE "|".
          02 CONTROL-X-SERV-NUM-ENV      PIC X.
          02 FILLER                      PIC X      VALUE "|".
          02 CONTROL-CL-0-NUM-ENV        PIC X.
          02 FILLER                      PIC X      VALUE "|".
          02 CONTROL-CL-1-NUM-ENV        PIC X.
          02 FILLER                      PIC X      VALUE "|".
          02 CONTROL-CL-2-NUM-ENV        PIC X.
          02 FILLER                      PIC X      VALUE "|".
          02 CONTROL-CL-3-NUM-ENV        PIC X.
          02 FILLER                      PIC X      VALUE "|".
          02 CONTROL-CL-4-NUM-ENV        PIC X.
          02 FILLER                      PIC X      VALUE "|".
          02 CONTROL-CL-5-NUM-ENV        PIC X.
          02 FILLER                      PIC X      VALUE "|".
          02 CONTROL-CL-6-NUM-ENV        PIC X.
          02 FILLER                      PIC X      VALUE "|".
          02 CONTROL-CL-7-NUM-ENV        PIC X.
          02 FILLER                      PIC X      VALUE "|".
          02 FECHA-ING-NUM-ENV           PIC 9(8).
          02 FILLER                      PIC X      VALUE "|".
          02 ESTADO-NUM-ENV              PIC X.
          02 FILLER                      PIC X      VALUE "|".
          02 NIT-NUM-ENV                 PIC 9(10).
          02 FILLER                      PIC X      VALUE "|".
          02 ID-PAC-NUM-ENV              PIC X(15).
          02 FILLER                      PIC X      VALUE "|".
          02 CONTROL-CAP-NUM-ENV         PIC 9(4).
          02 FILLER                      PIC X      VALUE "|".
          02 CTL-NROPACI-NUM-ENV         PIC X.
          02 FILLER                      PIC X      VALUE "|".
          02 FECHA-FARMA-NUM-ENV         PIC 9(8).
          02 FILLER                      PIC X      VALUE "|".
          02 CONVENIO-NUM-ENV            PIC XX.
          02 FILLER                      PIC X      VALUE "|".
          02 LLAVE-NUM-ENV.
             03 PREFIJO-NUM-ENV          PIC X.
             03 NRO-NUM-ENV              PIC 9(6).
          02 FILLER                      PIC X      VALUE "|".
          02 FECHA-RET-NUM-ENV           PIC 9(8).
          02 FILLER                      PIC X      VALUE "|".
          02 DESCRIP-NUM-ENV             PIC X(40).
          02 FILLER                      PIC X      VALUE "|".
          02 HORA-ING-NUM-ENV            PIC 9(4).
          02 FILLER                      PIC X      VALUE "|".
          02 FACT-CAPIT-NUM-ENV.
             03 PRE-CAPIT-NUM-ENV        PIC X.
             03 NRO-CAPIT-NUM-ENV        PIC 9(6).
          02 FILLER                      PIC X      VALUE "|".
          02 MYT-NUM-ENV                 PIC X.
          02 FILLER                      PIC X      VALUE "|".
          02 FECHA-SALID-ENV             PIC 9(8).

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
              MOVE "INV401-02"              TO MSJ3-HTML
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
              MOVE "INV401-02"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-NUMERACION.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-NUMER-W              TO MSJ2-HTML
              MOVE "INV401-02"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      *  I-O-TEST SECTION.
      *     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ORDEN-SALIDA.
      * ESCR-EXCEPTIONES.                                                                                                                                
      *     IF OTR-STAT = "00"
      *        CONTINUE
      *     ELSE
      *        MOVE OTR-STAT                 TO MSJ1-HTML
      *        MOVE NOM-NUMER-W              TO MSJ2-HTML
      *        MOVE "INV401-02"              TO MSJ3-HTML
      *        GO TO ENVIAR2-ERROR
      *     END-IF.

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
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W, LLAVE-LLEGADA-W
           END-UNSTRING.

001855     MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.

000797 INICIAR-SESION.
000806     MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.
006840
006900 ABRIR-USUARIO.
006910     INITIALIZE OTR-STAT.
006920     MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
006930
006940     OPEN INPUT ARCHIVO-USUNET
006950     EVALUATE OTR-STAT
006960       WHEN "00"  CONTINUE
006970       WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
006980                  INITIALIZE REG-USUNET
006990                  WRITE REG-USUNET
007000                  CLOSE ARCHIVO-USUNET
007010                  MOVE "Validacion de Usuario"       TO MENSAJE1-HTML
007020                  MOVE "Falta configurar usuarios"   TO MENSAJE2-HTML
007030                  MOVE "Sc"                          TO MENSAJE3-HTML
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
007230        MOVE "Validacion de Sesion"        TO MENSAJE1-HTML
007240        MOVE "Falta configurar usuarios"   TO MENSAJE2-HTML
007250        MOVE "Sc"                          TO MENSAJE3-HTML
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

001614     INITIALIZE NOM-NUMER-W
                      NOM-SALID-W
           MOVE "\\" TO NOM-NUMER-W
                        NOM-SALID-W
           INSPECT NOM-NUMER-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET
001621     INSPECT NOM-NUMER-W REPLACING FIRST " "
001624                                       BY "\"
007190     INSPECT NOM-NUMER-W REPLACING FIRST "                              "
                                            BY DIR-CONTAB-LLEGADA-W
           INSPECT NOM-NUMER-W REPLACING FIRST "                     "
007300                                      BY "\CONTROL\SC-NUMER.DAT".
           MOVE NOM-NUMER-W TO NOM-SALID-W.
      *     INSPECT NOM-NUMER-W REPLACING FIRST "\CONTROL\SC-NUMER.DAT"
007300*                                      BY "\CONTROL\SC-NUMER.DAT".
       ABRIR-NUMERACION.
           OPEN INPUT ARCHIVO-NUMERACION.

       BUSCAR-FACTURA.
           MOVE LLAVE-LLEGADA-W   TO LLAVE-NUM
           READ ARCHIVO-NUMERACION INVALID KEY
                                   MOVE "01"  TO SW-INVALID-ENV
                                   NOT INVALID KEY
                                   MOVE "00"  TO SW-INVALID-ENV
           END-READ.

       ENVIAR-DATOS-NUMER.
           MOVE LLAVE-SALID-NUM        TO LLAVE-SALID-NUM-ENV
           MOVE CONTROL-X-SERV-NUM     TO CONTROL-X-SERV-NUM-ENV
           MOVE CONTROL-CL-0-NUM       TO CONTROL-CL-0-NUM-ENV
           MOVE CONTROL-CL-1-NUM       TO CONTROL-CL-1-NUM-ENV
           MOVE CONTROL-CL-2-NUM       TO CONTROL-CL-2-NUM-ENV
           MOVE CONTROL-CL-3-NUM       TO CONTROL-CL-3-NUM-ENV
           MOVE CONTROL-CL-4-NUM       TO CONTROL-CL-4-NUM-ENV
           MOVE CONTROL-CL-5-NUM       TO CONTROL-CL-5-NUM-ENV
           MOVE CONTROL-CL-6-NUM       TO CONTROL-CL-6-NUM-ENV
           MOVE CONTROL-CL-7-NUM       TO CONTROL-CL-7-NUM-ENV
           MOVE FECHA-ING-NUM          TO FECHA-ING-NUM-ENV
           MOVE ESTADO-NUM             TO ESTADO-NUM-ENV
           MOVE NIT-NUM                TO NIT-NUM-ENV
           MOVE ID-PAC-NUM             TO ID-PAC-NUM-ENV
           MOVE CONTROL-CAP-NUM        TO CONTROL-CAP-NUM-ENV
           MOVE CTL-NROPACI-NUM        TO CTL-NROPACI-NUM-ENV
           MOVE FECHA-FARMA-NUM        TO FECHA-FARMA-NUM-ENV
           MOVE CONVENIO-NUM           TO CONVENIO-NUM-ENV
           MOVE FECHA-RET-NUM          TO FECHA-RET-NUM-ENV
           MOVE DESCRIP-NUM            TO DESCRIP-NUM-ENV
           MOVE HORA-ING-NUM           TO HORA-ING-NUM-ENV
           MOVE FACT-CAPIT-NUM         TO FACT-CAPIT-NUM-ENV
           MOVE MYT-NUM                TO MYT-NUM-ENV.

      * ABRIR-ORDEN-SALIDA.
      *     OPEN INPUT ARCHIVO-ORDEN-SALIDA.

      * BUSCAR-ORDEN.
      *     MOVE LLAVE-SALID-NUM  TO LLAVE-SALID
      *     READ ARCHIVO-ORDEN-SALIDA INVALID KEY
      *                               MOVE "01"  TO SW-INVALID-ENV
      *                               NOT INVALID KEY
      *                               MOVE "00"  TO SW-INVALID-ENV
      *     END-READ.

      * ENVIAR-DATOS-SALID.
      *     MOVE FECHA-SALID   TO FECHA-SALID-ENV.    

       CERRAR-ARCHIVOS.
           CLOSE ARCHIVO-NUMERACION.

010700 PAGINA-CONFIG.
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
008040 COPY "..\..\FUENTES\SC-WEB.CBL".
005210* COPY "..\..\FUENTES\WS-HCEVO.CBL".
