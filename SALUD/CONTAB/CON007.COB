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

       01 LINEA-LLEGADA-W              PIC X(100).

       77 NOM-MOV-W                    PIC X(70).
       77 NOM-LOTES-W                  PIC X(70).
       77 MES-SAL-W                    PIC X(4).
       77 SECU-ANT                     PIC XX.
       77 COMP-ANT                     PIC 9(6).
       77 COMP-W                       PIC 9(6).

       01 SECU-EDIT.
          02 SEC1-EDIT                 PIC 9.
          02 SEC2-EDIT                 PIC XX.

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(30).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 SECU-W.
             03 SECU1-W                PIC X.
             03 SECU2-W                PIC X.
          02 OPERADOR-LLEGADA-W        PIC X(4).
       
       01 DATOS-ENVIO.
          02 SW-INVALID-ENV            PIC X(2).
          02 FILLER                    PIC X VALUE "|".
          02 SECU-ENV                  PIC X(2).
          02 FILLER                    PIC X VALUE "|".
          02 ULT-FECHA-ENV.
             03 ANO-ULT-MOV-ENV        PIC 99.
             03 MES-ULT-MOV-ENV        PIC 99.
             03 DIA-ULT-MOV-ENV        PIC 99.
          02 FILLER                    PIC X VALUE "|".
          02 COMP-ENV                  PIC 9(6).
          

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
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, SECU-W, OPERADOR-LLEGADA-W 
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.


       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
           GO TO VALIDAR-SESION.
      
       FIN-VALIDAR-SESION.
      
       ABRIR-USUARIO.
           INITIALIZE OTR-STAT.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
      
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
           MOVE SECU-W  TO COD-LOTE
           READ ARCHIVO-LOTES WITH NO LOCK
                INVALID KEY
                   MOVE 1 TO CONSEC-LOTE
           END-READ
           CLOSE ARCHIVO-LOTES.

    
           IF CONSEC-LOTE = 1
              MOVE "\\"  TO NOM-MOV-W
              INSPECT NOM-MOV-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

              INSPECT NOM-MOV-W REPLACING FIRST " "
                                             BY "\"

              INSPECT NOM-MOV-W REPLACING FIRST "                              "
                                             BY DIR-CONTAB-LLEGADA-W

              INSPECT NOM-MOV-W REPLACING FIRST "                       "
                                             BY "\CONTROL\SC-ARCHMOV.DAT"
           ELSE
              MOVE "\\"  TO NOM-MOV-W
              INSPECT NOM-MOV-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

              INSPECT NOM-MOV-W REPLACING FIRST " "
                                             BY "\"

              INSPECT NOM-MOV-W REPLACING FIRST "                              "
                                             BY DIR-CONTAB-LLEGADA-W

              INSPECT NOM-MOV-W REPLACING FIRST "    "
                                             BY MES-SAL-W

              INSPECT NOM-MOV-W REPLACING FIRST "               "
                                             BY "\SC-ARCHMOV.DAT"
           END-IF.

       ABRIR-MOVIMIENTO.
           OPEN INPUT MOVIMIENTO-DIARIO.
           MOVE "00"   TO SW-INVALID-ENV.

       LEER-NUMERACION.
           INITIALIZE MOV-DIARIO.
           MOVE "99999999"             TO LLAVE-COMP-MOV.
           MOVE 0                      TO SEC1-EDIT
           MOVE SECU-W                 TO SEC2-EDIT
           MOVE SECU-EDIT              TO SECU-MOV

           READ MOVIMIENTO-DIARIO RECORD KEY IS SECUENCIA-MOV 
                INVALID KEY CLOSE MOVIMIENTO-DIARIO
                            OPEN I-O MOVIMIENTO-DIARIO 
                            INITIALIZE COMP-ANT 
                            MOVE "ULTIMO COMPROBANTE"      TO DETALLE-MOV
                            MOVE COMP-ANT                  TO REFER-MOV
                            WRITE MOV-DIARIO END-WRITE
                            MOVE "01"                      TO SW-INVALID-ENV
           END-READ.

           IF REF1-MOV IS NOT NUMERIC
              MOVE 0 TO REF1-MOV
           INSPECT REFER-MOV REPLACING ALL " " BY "0".
           
           MOVE FECHA-VENCE-MOV  TO ULT-FECHA-ENV
           MOVE REFER-MOV        TO COMP-W 
           ADD 1                 TO COMP-W
           MOVE COMP-W           TO COMP-ENV.        
 
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