004820*=================================================================
004830* CONSULTA DE ARCHIVO TABLA, CUPS Y ESCUPS.
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "..\..\FUENTES\FS-USUNET.CBL".
004920     COPY "..\..\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-TABLAS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TABLA-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS LLAVE-TAB
                  ALTERNATE RECORD KEY IS DESCRIP-TAB   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS COD-RIPS-TAB  WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-CUPS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-CUPS-W
      *"/PROG/DATOS/SC-ARCHCUPS.DAT"
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS LLAVE-CUP
                  ALTERNATE RECORD KEY IS DESCRIP-CUP WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LLAVE-ALT-CUP WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-ESPEC-CUP LOCK MODE IS AUTOMATIC
                       ASSIGN NOM-ESCUP-W
                       ORGANIZATION IS INDEXED;
                       ACCESS MODE  IS DYNAMIC
                       RECORD KEY   IS COD-ESP-CUP
                       FILE STATUS IS OTR-STAT.


      *     SELECT TABLA-MEDICAMENTOS LOCK MODE IS AUTOMATIC
      *                 ASSIGN NOM-TABME-W
      *"../control/SC-TABMED.DAT"
      *                 ORGANIZATION IS INDEXED;
      *                 ACCESS MODE DYNAMIC;
      *                 RECORD KEY  IS LLAVE-TABME
      *                 ALTERNATE RECORD KEY IS ART-TABME WITH DUPLICATES
      *                 FILE STATUS IS OTR-STAT.

005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-TABLA19.CBL".	   
       COPY "..\..\FUENTES\FD-CUPS19.CBL".	   
       COPY "..\..\FUENTES\FD-ESCUP19.CBL".
      * COPY "..\..\FUENTES\FD-TABME.CBL".	   

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
          02 COD-TABLA-LLEGADA-W        PIC X(15).
          02 NIT-USU-LLEGADA-W          PIC X(10).
          02 CONVENIO-NUM               PIC XX.
          02 LLAVE-CUP-LLEGADA-W        PIC X(12).
          

       77 MES-SAL-W                     PIC X(4).
       77 NOM-TABLA-W                   PIC X(70).
       77 NOM-CUPS-W                    PIC X(70).
       77 NOM-ESCUP-W                   PIC X(70).
       01 MONTO-TAB-EDIT                PIC ZZZZZZZZZZZ.ZZ.
       01 MONTO-TABME-EDIT              PIC ZZZZZZZZZZZ.ZZ.

       01 DATOS-ENVIO.
          02 SW-INVALID1-ENV            PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 SW-INVALID2-ENV            PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 DESCRIP-TAB-ENV            PIC X(30).
          02 FILLER                     PIC X VALUE "|".
          02 GR-SER-TAB-ENV             PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 MONTO-TAB-ENV              PIC X(14).
          02 FILLER                     PIC X VALUE "|".
          02 FORMA-LIQ-TAB-ENV          PIC 9.
          02 FILLER                     PIC X VALUE "|".
          02 NIVEL-CUP-ENV              PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CIS-CUP-ENV                PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 OCULTAR-CUP-ENV            PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 SEXO-CUP-ENV               PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 UNID-EDAD-CUP-ENV          PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 EDAD-MIN-CUP-ENV           PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 EDAD-MAX-CUP-ENV           PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 COD-TAB-ENV                PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 CD-SER-TAB-ENV             PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 LLAVE-TIPO-TAB-ENV         PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 COD-PAQ-INT-TAB-ENV        PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 DATOS-SEP-CUP-ENV          PIC X(12).
          02 FILLER                     PIC X VALUE "|".
          02 MONTO-TABME-ENV            PIC X(17).
          02 FILLER                     PIC X VALUE "|".
          02 INCREM-TAB-ENV             PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 DIV2-CUP-ENV               PIC X(2).
          02 FILLER                     PIC X VALUE "|".
          02 DIVISION-CUP-ENV           PIC X(2).
          02 FILLER                     PIC X VALUE "|".
          02 DIAGN-CUP-ENV              PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 ATIENDE-ESP-CUP-ENV        PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 SW-INVALID3-ENV            PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 DESCRIP-CUP-ENV            PIC X(80).
          02 FILLER                     PIC X VALUE "|".
          02 COD-SER-TAB-ENV            PIC X(12).
          02 FILLER                     PIC X VALUE "|".
          02 TIPO-PROC-CUP-ENV          PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 DETALLE-PROF-ENV           PIC X(10).


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
              MOVE "SAL41-07"               TO MSJ3-HTML
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
              MOVE "SAL41-07"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TABLAS.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-TABLA-W              TO MSJ2-HTML
              MOVE "SAL41-07"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CUPS.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-CUPS-W               TO MSJ2-HTML
              MOVE "SAL41-07"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ESPEC-CUP.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-ESCUP-W              TO MSJ2-HTML
              MOVE "SAL41-07"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

      *  I-O-TEST SECTION.
      *     USE AFTER EXCEPTION PROCEDURE ON TABLA-MEDICAMENTOS.
      * ESCR-EXCEPTIONES.                                                                                                                                
      *     IF OTR-STAT = "00"
      *        CONTINUE
      *     ELSE
      *        MOVE OTR-STAT                 TO MSJ1-HTML
      *        MOVE NOM-TABME-W              TO MSJ2-HTML
      *        MOVE "SAL41-07"               TO MSJ3-HTML
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
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W, COD-TABLA-LLEGADA-W, 
                    NIT-USU-LLEGADA-W, CONVENIO-NUM, LLAVE-CUP-LLEGADA-W
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
           INITIALIZE NOM-TABLA-W 

           MOVE "\\" TO NOM-TABLA-W 
                        NOM-CUPS-W
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".
           INSPECT NOM-TABLA-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET
           INSPECT NOM-TABLA-W REPLACING FIRST " "
                                            BY "\"
           MOVE NOM-TABLA-W   TO NOM-CUPS-W
                                 NOM-ESCUP-W

           INSPECT NOM-TABLA-W REPLACING FIRST "                              "
                                            BY DIR-CONTAB-LLEGADA-W
           INSPECT NOM-TABLA-W REPLACING FIRST "                     "
                                            BY "\CONTROL\SC-TABLA.DAT".
           INSPECT NOM-CUPS-W REPLACING FIRST "                          "
                                           BY "PROG\DATOS\SC-ARCHCUPS.DAT".
           INSPECT NOM-ESCUP-W REPLACING FIRST "                         "
                                            BY "PROG\DATOS\SC-ESP-CUP.DAT".

       ABRIR-TABLA.
           OPEN INPUT ARCHIVO-TABLAS
                      ARCHIVO-CUPS
                      ARCHIVO-ESPEC-CUP.

       LEER-TABLA.
           MOVE COD-TABLA-LLEGADA-W   TO LLAVE-TAB
           READ ARCHIVO-TABLAS WITH NO LOCK
                INVALID KEY 
                  MOVE "01" TO SW-INVALID1-ENV               
                NOT INVALID KEY
                  MOVE "00" TO SW-INVALID1-ENV
           END-READ.

           MOVE DESCRIP-TAB      TO DESCRIP-TAB-ENV
           MOVE GR-SER-TAB       TO GR-SER-TAB-ENV
           MOVE MONTO-TAB        TO MONTO-TAB-EDIT
           MOVE MONTO-TAB-EDIT   TO MONTO-TAB-ENV
           MOVE CD-SER-TAB       TO CD-SER-TAB-ENV
           MOVE LLAVE-TIPO-TAB   TO LLAVE-TIPO-TAB-ENV
           MOVE COD-PAQ-INT-TAB  TO COD-PAQ-INT-TAB-ENV
           MOVE INCREM-TAB       TO INCREM-TAB-ENV
           MOVE COD-SER-TAB      TO COD-SER-TAB-ENV
           MOVE FORMA-LIQ-TAB    TO FORMA-LIQ-TAB-ENV.

        LEER-CUPS.
           MOVE LLAVE-CUP-LLEGADA-W  TO LLAVE-CUP
           IF NIT-USU-LLEGADA-W = "0800162035" OR "0900030814"
                CONTINUE
           ELSE
                IF (GR-SER-TAB = "XX" OR "F5" OR "F6" OR "F7" OR "E8" OR "E9" OR "D8" OR "D9")
                   CONTINUE
                ELSE
                   MOVE SPACES TO NRO3-CUP
                END-IF
           END-IF

           READ ARCHIVO-CUPS WITH NO LOCK
                INVALID KEY 
                  MOVE "9R" TO SW-INVALID2-ENV
                NOT INVALID KEY
                  MOVE "00" TO SW-INVALID2-ENV
           END-READ.

           MOVE NIVEL-CUP     TO NIVEL-CUP-ENV
           MOVE CIS-CUP       TO CIS-CUP-ENV
           MOVE OCULTAR-CUP   TO OCULTAR-CUP-ENV
           MOVE DESCRIP-CUP   TO DESCRIP-CUP-ENV
           MOVE SEXO-CUP      TO SEXO-CUP-ENV
           MOVE UNID-EDAD-CUP TO UNID-EDAD-CUP-ENV
           MOVE EDAD-MIN-CUP  TO EDAD-MIN-CUP-ENV
      *    MOVE DATOS-SEP-CUP TO DATOS-SEP-CUP-ENV
           MOVE EDAD-MAX-CUP  TO EDAD-MAX-CUP-ENV.
        
        LEER-ESCUP.
           MOVE LLAVE-CUP-LLEGADA-W  TO COD-ESP-CUP

           READ ARCHIVO-ESPEC-CUP WITH NO LOCK
                                  INVALID KEY
                                    MOVE "01"  TO SW-INVALID3-ENV
                                  NOT INVALID KEY
                                    MOVE "00"  TO SW-INVALID3-ENV
           END-READ.

           MOVE TIPO-PROC-CUP      TO TIPO-PROC-CUP-ENV
           IF ATIENDE-ESP-CUP = LOW-VALUES
               INITIALIZE ATIENDE-ESP-CUP
           END-IF
           MOVE ATIENDE-ESP-CUP    TO ATIENDE-ESP-CUP-ENV.

        CERRAR-ARCHIVOS.
           CLOSE ARCHIVO-TABLAS
                 ARCHIVO-CUPS
                 ARCHIVO-ESPEC-CUP.

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
005210* COPY "..\..\FUENTES\WS-HCEVO.CBL".
