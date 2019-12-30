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
            COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
            COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".
            COPY "P:\PROG\PROYECT\FUENTES\FS-MOVIM.CBL".

           SELECT ARCHIVO-LOTES LOCK MODE IS AUTOMATIC
      	          ASSIGN NOM-LOTE-LNK
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-LOTE
                  ALTERNATE RECORD KEY IS NOMBRE-LOTE WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-MOVIM.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-LOTES.CBL".
       
       WORKING-STORAGE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC19.CBL".

       77 ULT-NRO-W                       PIC 9(9).

       01 SECU-EDIT.
          02 SEC1-EDIT                 PIC 9.
          02 SEC2-EDIT                 PIC XX.

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(30).
          02 MES-CONTAB-LLEGADA-W      PIC X(5).

       01 DATO-ULT-MOV-W.
          02 LOTE-ULT-MOV-W.
             03 LOTE1-ULT-MOV-W        PIC X.
             03 LOTE2-ULT-MOV-W        PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 NRO-ULT-COMP-W.
             03 NRO1-ULT-COMP-W        PIC 9(3).
             03 NRO2-ULT-COMP-W        PIC 9(6).
          02 FILLER                    PIC X VALUE "|".
          02 FECHA-ULT-MOV-W.
             03 ANO-ULT-MOV-W          PIC 99.
             03 MES-ULT-MOV-W          PIC 99.
             03 DIA-ULT-MOV-W          PIC 99.

       01 DATOS-ENVIO                  PIC X(100).

       LINKAGE SECTION.

       COPY "P:\PROG\PROYECT\FUENTES\ISAPICTX.CBL".

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
              MOVE NOM-LOTE-LNK             TO MSJ2-HTML
              MOVE "CON007"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MOVIMIENTO-DIARIO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-MOV                  TO MSJ2-HTML
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
           MOVE COBW3-GET-DATA    TO LINEA-LLEGAD-W.

           INITIALIZE DATO-ULT-MOV-W.

           UNSTRING LINEA-LLEGAD-W DELIMITED BY "|"
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, LOTE-ULT-MOV-W
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
007010                  MOVE "SC-2"                        TO MSJ1-HTML
007020                  MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007030                  MOVE "CON007"                      TO MSJ3-HTML
007040                  GO TO ENVIAR2-ERROR
             WHEN OTHER GO TO CERRAR-SESION
           END-EVALUATE.
      
           INITIALIZE LLAVE-USUNET.

      
       LEER-USUARIO.
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.     
           CLOSE ARCHIVO-USUNET.
           
           IF NOMBRE-USUNET = SPACES
007010        MOVE "SC-2"                        TO MSJ1-HTML
007020        MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007030        MOVE "CON007"                      TO MSJ3-HTML
007040        GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.

           INITIALIZE NOM-LOTE-LNK NOM-MOV.

           MOVE "\\" TO NOM-LOTE-LNK

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

           INSPECT NOM-LOTE-LNK REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET
           INSPECT NOM-LOTE-LNK REPLACING FIRST " "
                                            BY "\"

           MOVE NOM-LOTE-LNK   TO NOM-MOV

           INSPECT NOM-MOV REPLACING FIRST "                              "
                                        BY DIR-CONTAB-LLEGADA-W.

           INSPECT NOM-LOTE-LNK REPLACING FIRST "                          "
                                            BY "PROG\DATOS\SC-ARCHLOTE.DAT".


       ABRIR-ARCHIVO-LOTES.
           OPEN INPUT ARCHIVO-LOTES
           MOVE LOTE-ULT-MOV-W  TO COD-LOTE
           READ ARCHIVO-LOTES WITH NO LOCK
                INVALID KEY
                   MOVE 1 TO CONSEC-LOTE
           END-READ
           CLOSE ARCHIVO-LOTES.

           IF CONSEC-LOTE = 1
              INSPECT NOM-MOV REPLACING FIRST "                       "
                                             BY "\CONTROL\SC-ARCHMOV.DAT"
           ELSE
              INSPECT NOM-MOV REPLACING FIRST "     "
                                           BY MES-CONTAB-LLEGADA-W
              INSPECT NOM-MOV REPLACING FIRST "              "
                                           BY "SC-ARCHMOV.DAT"
           END-IF.

       ABRIR-MOVIMIENTO.
           OPEN INPUT MOVIMIENTO-DIARIO.


       LEER-NUMERACION.
           INITIALIZE MOV-DIARIO.
           MOVE "99999999"             TO LLAVE-COMP-MOV.
           MOVE 0                      TO SEC1-EDIT
           MOVE LOTE-ULT-MOV-W         TO SEC2-EDIT
           MOVE SECU-EDIT              TO SECU-MOV


           READ MOVIMIENTO-DIARIO RECORD KEY IS SECUENCIA-MOV 
                INVALID KEY 
                        CLOSE MOVIMIENTO-DIARIO
                        OPEN I-O MOVIMIENTO-DIARIO 
                        INITIALIZE ULT-NRO-MOV CONSEC-LTF-MOV
                        MOVE "ULTIMO COMPROBANTE" TO DETALLE-MOV
                        WRITE MOV-DIARIO END-WRITE
           END-READ.
                                  

           IF ULT-NRO-MOV IS NOT NUMERIC
              INITIALIZE ULT-NRO-MOV
           END-IF.

           IF CONSEC-LTF-MOV  IS NOT NUMERIC
              INITIALIZE CONSEC-LTF-MOV 
           END-IF.

           MOVE FECHA-VENCE-MOV  TO FECHA-ULT-MOV-W.

           IF LOTE2-ULT-MOV-W = "Q"
              MOVE ULT-NROPQ-MOV TO ULT-NRO-W
              ADD  1             TO ULT-NRO-W
              MOVE ULT-NRO-W     TO NRO-ULT-COMP-W
           ELSE
              INITIALIZE            NRO-ULT-COMP-W
              MOVE ULT-NRO-MOV   TO NRO2-ULT-COMP-W
              ADD  1             TO NRO2-ULT-COMP-W
           END-IF.

           IF LOTE-ULT-MOV-W = "LT"
              MOVE CONSEC-LTF-MOV TO NRO-ULT-COMP-W
              ADD  1              TO NRO2-ULT-COMP-W
           END-IF.
 
       CERRAR-ARCHIVOS.
            CLOSE MOVIMIENTO-DIARIO. 
            MOVE DATO-ULT-MOV-W  TO DATOS-ENVIO.

010700 PAGINA-CONFIG.  
010720     MOVE "statuscode" TO COBW3-CNV-NAME
010730     MOVE "00"         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

000570	   MOVE "datosrecibidos" TO COBW3-CNV-NAME
000580     MOVE DATOS-ENVIO      TO COBW3-CNV-VALUE
000590	   CALL "COBW3_SET_CNV"  USING COBW3

010720     MOVE "programa-id" TO COBW3-CNV-NAME
010730     MOVE "CON007"      TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010760     MOVE "..\PAGINAS\RECIBIDOSPLANO.ASPX" TO SALIDA-HTML
010770     PERFORM ABRIR-HTML.

007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.

008030
008040 COPY "P:\PROG\PROYECT\FUENTES\SC-WEB19.CBL".
