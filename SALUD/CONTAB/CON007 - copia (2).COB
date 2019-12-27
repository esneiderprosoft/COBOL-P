      *=================================================================
      * CONTABILIDAD - BUSCA EL ULTIMO NUMERO DE UNA SECUENCIA.
      *=================================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PROSOFT.
       OBJECT-COMPUTER. PROSOFT.
       INPUT-OUTPUT SECTION.            
       FILE-CONTROL.
            COPY "..\..\FUENTES\FS-USUNET.CBL".
            COPY "..\..\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-LOTES LOCK MODE IS AUTOMATIC
      	          ASSIGN NOM-LOTE-LNK
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-LOTE
                  ALTERNATE RECORD KEY IS NOMBRE-LOTE WITH DUPLICATES
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

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-LOTES.CBL".
       COPY "..\..\FUENTES\FD-MOVIM.CBL".
       
       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".
       77 NOM-MOV-W                 PIC X(70).
       77 NOM-LOTES-W                  PIC X(70).
       77 MES-SAL-W                    PIC X(4).
       77 NRO-ULT-COMP-W               PIC 9(6).
       77 SW9                          PIC 9.
       77 SEC-W                        PIC X(50).
       77 ULT-NRO-W                    PIC 9(9).

       01 SECUENC-EDIT.
          02 SEC1-EDIT                 PIC 9.
          02 SEC2-EDIT                 PIC XX.

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(8).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 DATO-ULT-MOV-LNK.
             03 LOTE-ULT-MOV-LNK.
                05 LOTE1-ULT-MOV-LNK   PIC X.
                05 LOTE2-ULT-MOV-LNK   PIC X.
             03 NRO-ULT-COMP-LNK.
                05 NRO1-ULT-COMP-LNK   PIC 9(3).
                05 NRO2-ULT-COMP-LNK   PIC 9(6).
             03 FECHA-ULT-MOV-LNK.
                05 ANO-ULT-MOV-LNK     PIC 99.
                05 MES-ULT-MOV-LNK     PIC 99.
                05 DIA-ULT-MOV-LNK     PIC 99.

       01 NOMBRE-OPER-W.
          02 NOMBRE1-OPER-W            PIC X(28).
          02 SUC-OPER-W                PIC X(2).
       01 IDENT-OPER-W                 PIC 9(10).

       01 LINEA-LLEGADA-W              PIC X(100).

       01 DATOS-ENVIO.
          02 ESTADO-ENVIO              PIC X(2).
          02 FILLER                    PIC X VALUE "|".
          02 MENSAJE-ENVIO             PIC X(8).
          02 FILLER                    PIC X VALUE "|".
          02 SECU-ENV                  PIC X(2).
          02 FILLER                    PIC X VALUE "|".
          02 ULT-FECHA-ENV.
             03 ANO-ULT-MOV-ENV        PIC 99.
             03 MES-ULT-MOV-ENV        PIC 99.
             03 DIA-ULT-MOV-ENV        PIC 99.
          02 FILLER                    PIC X VALUE "|".
          02 NRO-ULT-COMP-ENV          PIC 9(9).
          

       LINKAGE SECTION.

       COPY "..\..\FUENTES\ISAPICTX.CBL".

       PROCEDURE DIVISION WITH stdcall LINKAGE USING ISAPI-CTX-CNT.

       DECLARATIVES.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUNET.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USU-W                TO MSJ2-HTML
              MOVE "CON007"                 TO MSJ3-HTML
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
              MOVE "CON007"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-LOTES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-LOTE-LNK              TO MSJ2-HTML
              MOVE "CON007"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MOVIMIENTO-DIARIO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-MOV-W                  TO MSJ2-HTML
              MOVE "CON007"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       END DECLARATIVES.

       INICIAR-IIS.
           MOVE LOW-VALUE TO COBW3.
           MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
           CALL "COBW3_INIT" USING COBW3.
      
       LEER-DATO-HTML.
           MOVE "datosh" TO COBW3-SEARCH-DATA.
           CALL "COBW3_GET_VALUE" USING COBW3.
           MOVE COBW3-GET-DATA    TO LINEA-LLEGADA-W.

           UNSTRING LINEA-LLEGADA-W DELIMITED BY "|"
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, DATO-ULT-MOV-LNK
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.


       INICIAR-SESION.
           MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W
           GO TO VALIDAR-SESION.
      
       FIN-VALIDAR-SESION.
      
       ABRIR-USUARIO.
           INITIALIZE OTR-STAT.
           MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
      
           OPEN INPUT ARCHIVO-USUNET
           EVALUATE OTR-STAT
             WHEN "00"  CONTINUE
             WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
                        INITIALIZE REG-USUNET
                        WRITE REG-USUNET
                        CLOSE ARCHIVO-USUNET
                        MOVE "Validacion de Usuario"       TO MSJ1-HTML
                        MOVE "Falta configurar usuarios"   TO MSJ2-HTML
                        MOVE "Sc"                          TO MSJ3-HTML
                        GO TO ENVIAR2-ERROR
             WHEN OTHER GO TO CERRAR-SESION
           END-EVALUATE.
      
           INITIALIZE LLAVE-USUNET.

      
       LEER-USUARIO.
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.     
           CLOSE ARCHIVO-USUNET.
           
           IF NOMBRE-USUNET = SPACES
              MOVE "Validacion de Sesion"        TO MSJ1-HTML
              MOVE "Falta configurar usuarios"   TO MSJ2-HTML
              MOVE "Sc"                          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

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

           INITIALIZE NOM-LOTE-LNK NOM-MOV.

           MOVE "\\" TO NOM-LOTE-LNK

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

           INSPECT NOM-LOTE-LNK REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET
           INSPECT NOM-LOTE-LNK REPLACING FIRST " "
                                            BY "\"
           INSPECT NOM-LOTE-LNK REPLACING FIRST "                          "
                                            BY "PROG\DATOS\SC-ARCHLOTE.DAT".

       ABRIR-ARCHIVO-LOTES.
           OPEN INPUT ARCHIVO-LOTES
           MOVE LOTE-ULT-MOV-LNK  TO COD-LOTE
           READ ARCHIVO-LOTES WITH NO LOCK
                INVALID KEY
                  IF LOTE2-ULT-MOV-LNK = "Q"
                     MOVE 0 TO CONSEC-LOTE
                  ELSE
                     MOVE 1 TO CONSEC-LOTE
                  END-IF
           END-READ.

           CLOSE ARCHIVO-LOTES.

           IF CONSEC-LOTE = 1
              MOVE "\\"  TO NOM-MOV-W
              INSPECT NOM-MOV-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

              INSPECT NOM-MOV-W REPLACING FIRST " "
                                             BY "\"

              INSPECT NOM-MOV-W REPLACING FIRST "        "
                                             BY DIR-CONTAB-LLEGADA-W

              INSPECT NOM-MOV-W REPLACING FIRST "                       "
                                             BY "\CONTROL\SC-ARCHMOV.DAT"
           ELSE
              MOVE "\\"  TO NOM-MOV-W
              INSPECT NOM-MOV-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

              INSPECT NOM-MOV-W REPLACING FIRST " "
                                             BY "\"

              INSPECT NOM-MOV-W REPLACING FIRST "        "
                                             BY DIR-CONTAB-LLEGADA-W

              INSPECT NOM-MOV-W REPLACING FIRST "    "
                                             BY MES-SAL-W

              INSPECT NOM-MOV-W REPLACING FIRST "               "
                                             BY "\SC-ARCHMOV.DAT"
           END-IF.

       ABRIR-MOVIMIENTO.
           OPEN INPUT MOVIMIENTO-DIARIO.

       LEER-NUMERACION.
           INITIALIZE MOV-DIARIO.
           MOVE "99999999"             TO LLAVE-COMP-MOV.
           MOVE 0                      TO SEC1-EDIT
           MOVE LOTE-ULT-MOV-LNK       TO SEC2-EDIT
           MOVE SECUENC-EDIT           TO SECU-MOV.
                             
           READ MOVIMIENTO-DIARIO RECORD KEY IS SECUENCIA-MOV
                INVALID KEY  CLOSE    MOVIMIENTO-DIARIO
                             OPEN I-O MOVIMIENTO-DIARIO
                             INITIALIZE ULT-NRO-MOV CONSEC-LTF-MOV            
                             MOVE "ULTIMO COMPROBANTE"  TO DETALLE-MOV
                             WRITE MOV-DIARIO END-WRITE
           END-READ.

           IF ULT-NRO-MOV IS NOT NUMERIC
              INITIALIZE ULT-NRO-MOV
           END-IF.
      
           IF CONSEC-LTF-MOV  IS NOT NUMERIC
              INITIALIZE CONSEC-LTF-MOV 
           END-IF.
      
           MOVE FECHA-VENCE-MOV   TO FECHA-ULT-MOV-LNK.
       
           MOVE ULT-NRO-MOV    TO NRO2-ULT-COMP-LNK
           ADD  1              TO NRO2-ULT-COMP-LNK
      
           IF LOTE-ULT-MOV-LNK = "LT"
              MOVE CONSEC-LTF-MOV TO NRO-ULT-COMP-LNK
              ADD  1              TO NRO2-ULT-COMP-LNK
           END-IF.

           MOVE "00"              TO ESTADO-ENVIO
           MOVE CONSEC-LOTE       TO MENSAJE-ENVIO
           MOVE LOTE-ULT-MOV-LNK  TO SECU-ENV
           MOVE NRO-ULT-COMP-LNK  TO NRO-ULT-COMP-ENV     
           MOVE FECHA-ULT-MOV-LNK TO ULT-FECHA-ENV.
    
       CERRAR-ARCHIVOS.
            CLOSE MOVIMIENTO-DIARIO. 


       PAGINA-CONFIG.
010720     MOVE "datosrecibidos"    TO COBW3-CNV-NAME
010730     MOVE  DATOS-ENVIO        TO COBW3-CNV-VALUE
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
