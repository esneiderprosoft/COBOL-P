      *=================================================================
      *GRABAR ARCHIVO-NOMBRE-TAR CON110I-02  SALUD
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".                                                         
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".
                                                                                     
      * "\PROG\DATOS\SC-ARCHLOTE.DAT";

           SELECT ARCHIVO-LOTES LOCK MODE IS AUTOMATIC
	               ASSIGN LOTES-W;
                    ORGANIZATION IS INDEXED;
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS COD-LOTE
                    ALTERNATE RECORD KEY IS NOMBRE-LOTE WITH DUPLICATES
                    FILE STATUS IS OTR-STAT.

      
       DATA DIVISION.
       FILE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-LOTES.CBL".

      
       WORKING-STORAGE  SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC19.CBL".

       77 LOTES-W                   PIC X(60).

       01  DATO-LLEGADA-W.
           02 NOV-LLEGADA-W            PIC X.
           02 LOTE-LLEGADA-W           PIC X(2).
           02 NOMBRE-LLEGADA-W         PIC X(30).
           02 CONSECUTIVO-LLEGADA-W    PIC X.
           02 PREFIJO-LLEGADA-W        PIC XX.
           02 PRESUP-LLEGADA-W         PIC X.
           02 SINSIT-LLEGADA-W         PIC X.
           02 CONTRATO-LLEGADA-W       PIC X.
           02 SIMPLIF-LLEGADA-W        PIC X.

       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.
          
       01 MENSAJE-ENVIO                PIC X(20).

      
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
              MOVE "CON110I-02"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-LOTES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT           TO MSJ1-HTML
              MOVE LOTES-W            TO MSJ2-HTML
              MOVE "CON110I-02"          TO MSJ3-HTML
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
              MOVE "CON110I-02"                TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO LINEA-LLEGAD-W.

           UNSTRING LINEA-LLEGAD-W DELIMITED BY "|"
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W, 
              NOV-LLEGADA-W, LOTE-LLEGADA-W, NOMBRE-LLEGADA-W, CONSECUTIVO-LLEGADA-W, 
              PREFIJO-LLEGADA-W, PRESUP-LLEGADA-W, SINSIT-LLEGADA-W, CONTRATO-LLEGADA-W,
              SIMPLIF-LLEGADA-W
           END-UNSTRING.

           MOVE SESION-LLEGAD-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.
           
       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
           GO TO VALIDAR-SESION.


       FIN-VALIDAR-SESION.

       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
      
           OPEN INPUT ARCHIVO-USUNET
           EVALUATE OTR-STAT
             WHEN "00"  CONTINUE
             WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
                        CLOSE       ARCHIVO-USUNET
                        OPEN I-O    ARCHIVO-USUNET
                        INITIALIZE REG-USUNET
                        MOVE "." TO NOMBRE-USUNET
                        WRITE REG-USUNET
                        CLOSE       ARCHIVO-USUNET
                        MOVE "SC-2"      TO MSJ1-HTML
                        MOVE "Falta configurar usuario"    TO MSJ2-HTML
                        MOVE "CON110I-02"                          TO MSJ3-HTML                        
                        GO TO ENVIAR2-ERROR
             WHEN OTHER GO TO CERRAR-SESION
           END-EVALUATE.
      
           INITIALIZE LLAVE-USUNET.

       LEER-USUARIO.
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
           CLOSE ARCHIVO-USUNET.
      
           IF NOMBRE-USUNET = SPACES
              MOVE "SC-2"      TO MSJ1-HTML
              MOVE "Falta configurar usuario"    TO MSJ2-HTML
              MOVE "CON110I-02"                          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.
           INITIALIZE LOTES-W

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           
           MOVE "\\" TO LOTES-W

           INSPECT LOTES-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET
                                          
           INSPECT LOTES-W   REPLACING FIRST "                           "
                                               BY "\PROG\DATOS\SC-ARCHLOTE.DAT".

       ABRIR-GRUPO-SER.
           OPEN INPUT ARCHIVO-LOTES.  

           
       LEER-GRUPO-SER.
           MOVE LOTE-LLEGADA-W TO COD-LOTE 
           READ ARCHIVO-LOTES  WITH NO LOCK
                INVALID KEY 
                  MOVE 1 TO SW-INVALID
           END-READ.

           IF NOV-LLEGADA-W = 7 AND SW-INVALID = 1 GO TO NUEVO.
           IF NOV-LLEGADA-W = 7 AND SW-INVALID = 0 GO TO ERROR-1.
           IF NOV-LLEGADA-W = 8 AND SW-INVALID = 0 GO TO CAMBIO.
           IF NOV-LLEGADA-W = 8 AND SW-INVALID = 1 GO TO ERROR-1.
           IF NOV-LLEGADA-W = 9 AND SW-INVALID = 0 GO TO RETIRO.
           IF NOV-LLEGADA-W = 9 AND SW-INVALID = 1 GO TO ERROR-1.

       NUEVO.
           INITIALIZE REG-LOTE.

           MOVE LOTE-LLEGADA-W        TO COD-LOTE
           MOVE NOMBRE-LLEGADA-W      TO NOMBRE-LOTE
           MOVE CONSECUTIVO-LLEGADA-W TO CONSEC-LOTE
           MOVE PREFIJO-LLEGADA-W     TO PREFIJO-LOTE
           MOVE PRESUP-LLEGADA-W      TO PRESUP-LOTE 
           MOVE SINSIT-LLEGADA-W      TO SINSIT-FOND-LOTE
           MOVE CONTRATO-LLEGADA-W    TO CONTRATO-LOTE
           MOVE SIMPLIF-LLEGADA-W     TO SIMPLIF-LOTE
           CLOSE ARCHIVO-LOTES.

           OPEN I-O ARCHIVO-LOTES.


           WRITE REG-LOTE END-WRITE
           CLOSE ARCHIVO-LOTES.

           MOVE "REGISTRO GRABADO" TO MENSAJE-ENVIO.

           GO TO ENVIO-DATOS.           

       CAMBIO.

           CLOSE ARCHIVO-LOTES.
           OPEN I-O ARCHIVO-LOTES.

           MOVE LOTE-LLEGADA-W        TO COD-LOTE
           MOVE NOMBRE-LLEGADA-W      TO NOMBRE-LOTE
           MOVE CONSECUTIVO-LLEGADA-W TO CONSEC-LOTE
           MOVE PREFIJO-LLEGADA-W     TO PREFIJO-LOTE
           MOVE PRESUP-LLEGADA-W      TO PRESUP-LOTE 
           MOVE SINSIT-LLEGADA-W      TO SINSIT-FOND-LOTE
           MOVE CONTRATO-LLEGADA-W    TO CONTRATO-LOTE
           MOVE SIMPLIF-LLEGADA-W     TO SIMPLIF-LOTE

           REWRITE REG-LOTE END-REWRITE

           CLOSE ARCHIVO-LOTES. 
           MOVE "REGISTRO MODIFICADO" TO MENSAJE-ENVIO.

           GO TO ENVIO-DATOS.            

       RETIRO.
           CLOSE ARCHIVO-LOTES.
           OPEN I-O ARCHIVO-LOTES.
     
           DELETE ARCHIVO-LOTES END-DELETE
           CLOSE ARCHIVO-LOTES.

           MOVE "REGISTRO BORRADO" TO MENSAJE-ENVIO.

           GO TO ENVIO-DATOS.

           
       ERROR-1.
           MOVE "ERROR AL GUARDAR" TO MENSAJE-ENVIO.

           GO TO ENVIO-DATOS.   


       ENVIO-DATOS.  
            MOVE "statuscode" TO COBW3-CNV-NAME
            MOVE "00"         TO COBW3-CNV-VALUE
            CALL "COBW3_SET_CNV" USING COBW3

           MOVE "datosrecibidos" TO COBW3-CNV-NAME
010730     MOVE MENSAJE-ENVIO  TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010720     MOVE "programa-id" TO COBW3-CNV-NAME
010730     MOVE "RX-421W-02"      TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

           MOVE "..\PAGINAS\RECIBIDOSPLANO.ASPX" TO SALIDA-HTML
           PERFORM ABRIR-HTML.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.
      
       COPY "..\..\FUENTES\SC-WEB19.CBL".
      

