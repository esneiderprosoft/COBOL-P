004820*=================================================================
004830* CONSULTA DE ARCHIVO MACRO HIS.
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "..\..\FUENTES\FS-USUNET.CBL".
004920     COPY "..\..\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-MACRO-HIS LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-MACRO-W
      *"\PROG\HC\DATOS\SC-MACDR.DAT"
                   ORGANIZATION IS INDEXED;
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS LLAVE-MACROH
                   ALTERNATE RECORD KEY IS NOMBRE-MACROH WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.

            SELECT ARCHIVO-JSON LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.

005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-MACR1.CBL".	   

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON                PIC X(500).

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
          02 LLAVE-MACRO-LLEGADA-W      PIC X(3).
          02 OPERADOR-LLEGADA-W         PIC X(4).

       77 MES-SAL-W                     PIC X(4).
       77 NOM-MACRO-W                   PIC X(70).
       77 NOM-PLANO-W                   PIC X(70).

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV             PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 NOMBRE-MACROH-ENV          PIC X(40).
          02 FILLER                     PIC X VALUE "|".
          02 DATO-J                     PIC X(28).

       01 LIN-1.
          02 FILLER                     PIC X(14)  VALUE "{*ART_MACROH*:".
          02 FILLER                     PIC X      VALUE "*".
          02 ART-MACROH-J               PIC X(15).
          02 FILLER                     PIC XX     VALUE "*}".    
          02 CIERRE-LIN-1               PIC X. 

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
              MOVE "INV401-11"              TO MSJ3-HTML
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
              MOVE "INV401-11"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-MACRO-HIS.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-MACRO-W              TO MSJ2-HTML
              MOVE "INV401-11"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PLANO-W              TO MSJ2-HTML
              MOVE "INV401-11"              TO MSJ3-HTML
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
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, LLAVE-MACRO-LLEGADA-W, OPERADOR-LLEGADA-W        
            END-UNSTRING.

001855     MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.
           MOVE FUNCTION CURRENT-DATE TO FECHA-JSON-W.
           ACCEPT HORA-JSON-W FROM TIME.
           MOVE OPERADOR-LLEGADA-W TO OPER-JSON-W.

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
           END-EVALUATE.
           
        ASIGNAR-NOMBRES. 
           INITIALIZE NOM-MACRO-W 

           MOVE "\\" TO NOM-MACRO-W
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".
           INSPECT NOM-MACRO-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET
           INSPECT NOM-MACRO-W REPLACING FIRST " "
                                            BY "\"
           INSPECT NOM-MACRO-W REPLACING FIRST "                          "
                                            BY "PROG\HC\DATOS\SC-MACDR.DAT".

       ABRIR-MACRO.
           OPEN INPUT ARCHIVO-MACRO-HIS.

       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-JSON.

           MOVE "{*MACRO*:[" TO  DATOS-JSON
           INSPECT DATOS-JSON  REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON  END-WRITE.

       LEER-MACRO.
           MOVE LLAVE-MACRO-LLEGADA-W   TO LLAVE-MACROH
           READ ARCHIVO-MACRO-HIS WITH NO LOCK
                INVALID KEY 
                  MOVE "01" TO SW-INVALID-ENV               
                NOT INVALID KEY
                  MOVE "00" TO SW-INVALID-ENV
           END-READ.
           MOVE NOMBRE-MACROH      TO NOMBRE-MACROH-ENV
           PERFORM MOVER-ART VARYING I FROM 1 BY 1
                             UNTIL I > 20.
        CERRAR-JSON.
           INITIALIZE LIN-1
           INITIALIZE ART-MACROH-J
                                                                 
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-1 TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE          
           MOVE "]}" TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE.

        CERRAR-TERCEROS.
           CLOSE ARCHIVO-MACRO-HIS
                 ARCHIVO-JSON.

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
008040 COPY "..\..\FUENTES\SC-WEB.CBL".

       MOVER-ART.
           INITIALIZE LIN-1
           MOVE ART-MACROH (I)      TO ART-MACROH-J
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT LIN-1 REPLACING ALL LOW-VALUE BY " "
           MOVE "," TO CIERRE-LIN-1
           MOVE LIN-1 TO DATOS-JSON
           WRITE REG-JSON END-WRITE.
005210* COPY "..\..\FUENTES\WS-HCEVO.CBL".
