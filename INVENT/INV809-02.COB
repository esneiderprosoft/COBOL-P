      *=================================================================
      *GRABAR ARCHIVO-DIVISION INV809-02  SALUD
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".
       

           SELECT ARCHIVO-DIVISION
                  ASSIGN NOM-DIV-LNK
                  ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS LLAVE-DIV
                  ALTERNATE RECORD KEY IS DESCRIP-DIV WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

      
       DATA DIVISION.
       FILE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-DIVIS.CBL".

      
       WORKING-STORAGE  SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".

       77 NOM-PLANO-W               pic x(90).
       77 NOM-DIV-LNK               PIC X(60).
       77 MES-SAL-W                 PIC X(4).

       01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 IP-SERVER-W               PIC X(13).
           02 DIR-CONTAB-LLEGADA-W      PIC X(6).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 NOV-LLEGADA-W             PIC X.
           02 COD-DIV-LLEGADA-W         PIC X(2).
           02 DESCRIP-LLEGADA-W         PIC X(20).

       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.
          02 FILLER                    PIC X VALUE "|".
          02 DAT1-ENV                  PIC X(30).
          02 FILLER                    PIC X VALUE "|".
          02 DAT2-ENV                  PIC X(20).

      
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
              MOVE "INV809"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-DIVISION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE                    
           ELSE
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-DIV-LNK               TO MSJ2-HTML
              MOVE "INV809"                  TO MSJ3-HTML
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
              MOVE "INV809"                 TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO DATO-LLEGADA-W.

           MOVE LLAVESESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.

       INICIAR-SESION.
           MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W

           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.

       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
       
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
                        MOVE "Validacion de usuarios"      TO MSJ1-HTML
                        MOVE "Falta configurar usuario"    TO MSJ2-HTML
                        MOVE "Sc"                          TO MSJ3-HTML
                        GO TO ENVIAR2-ERROR
             WHEN OTHER GO TO CERRAR-SESION
           END-EVALUATE.
       
           INITIALIZE LLAVE-USUNET.
      
       LEER-USUARIO.
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
           CLOSE ARCHIVO-USUNET.
        
           IF NOMBRE-USUNET = SPACES
              MOVE "Validacion de usuarios"      TO MSJ1-HTML
              MOVE "Falta configurar usuario"    TO MSJ2-HTML
              MOVE "Sc"                          TO MSJ3-HTML
              GO TO ENVIAR-ERROR
           END-IF.

       ASIGNAR-NOMBRES.
           INITIALIZE NOM-DIV-LNK

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

      *     INSPECT IP-SERVER-W REPLACING FIRST "/" BY "\"
           INITIALIZE NOM-DIV-LNK
           MOVE "\\" TO NOM-DIV-LNK
           INSPECT NOM-DIV-LNK REPLACING FIRST "             "
                                          BY IP-SERVER-W

           INSPECT NOM-DIV-LNK REPLACING FIRST "     "
                                          BY    "\PSC$"

           INSPECT NOM-DIV-LNK REPLACING FIRST " "
                                          BY "\"

           INSPECT NOM-DIV-LNK   REPLACING FIRST "      "
                                            BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT NOM-DIV-LNK   REPLACING FIRST "                     "
                                             BY  "\CONTROL\SC-DIVIS.DAT".

       ABRIR-DIV.
           OPEN INPUT ARCHIVO-DIVISION

           EVALUATE OTR-STAT
             WHEN "00" CONTINUE
             WHEN "35" OPEN OUTPUT ARCHIVO-DIVISION
                       CLOSE       ARCHIVO-DIVISION
                       GO TO ABRIR-DIV
           END-EVALUATE.
 

       LEER-DIV.
           MOVE 00 TO SW-INVALID
           MOVE COD-DIV-LLEGADA-W TO COD-DIV
           READ ARCHIVO-DIVISION  WITH NO LOCK
                INVALID KEY 
                  MOVE 01 TO SW-INVALID
           END-READ.

           IF NOV-LLEGADA-W = 7 AND SW-INVALID = 01 GO TO NUEVO.
           IF NOV-LLEGADA-W = 7 AND SW-INVALID = 00 GO TO ERROR-1.
           IF NOV-LLEGADA-W = 8 AND SW-INVALID = 00 GO TO CAMBIO.
           IF NOV-LLEGADA-W = 8 AND SW-INVALID = 01 GO TO ERROR-1.
           IF NOV-LLEGADA-W = 9 AND SW-INVALID = 00 GO TO RETIRO.
           IF NOV-LLEGADA-W = 9 AND SW-INVALID = 01 GO TO ERROR-1.

       NUEVO.
           INITIALIZE REG-DIV

           MOVE COD-DIV-LLEGADA-W  TO COD-DIV
           MOVE DESCRIP-LLEGADA-W  TO DESCRIP-DIV 

           CLOSE ARCHIVO-DIVISION

           OPEN I-O ARCHIVO-DIVISION

           WRITE REG-DIV END-WRITE

           CLOSE ARCHIVO-DIVISION

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO GRABADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       CAMBIO.
           IF  COD-DIV-LLEGADA-W = COD-DIV
           AND DESCRIP-LLEGADA-W = DESCRIP-DIV
               GO TO ENVIO-DATOS
           END-IF

           CLOSE ARCHIVO-DIVISION

           OPEN I-O ARCHIVO-DIVISION

           MOVE COD-DIV-LLEGADA-W  TO COD-DIV
           MOVE DESCRIP-LLEGADA-W  TO DESCRIP-DIV 

           REWRITE REG-DIV END-REWRITE

           CLOSE ARCHIVO-DIVISION 
           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO MODIFICADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       RETIRO.
           CLOSE ARCHIVO-DIVISION

           OPEN I-O ARCHIVO-DIVISION

           DELETE ARCHIVO-DIVISION END-DELETE

           CLOSE ARCHIVO-DIVISION

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO BORRADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       ERROR-1.
           EVALUATE NOV-LLEGADA-W
             WHEN 7 MOVE "88"                           TO DAT0-ENV
                    MOVE "Error - Dato ya Esta Creado"  TO DAT1-ENV
                    MOVE "SER106-02"                    TO DAT2-ENV
             WHEN 8 MOVE "88"                           TO DAT0-ENV
                    MOVE "Error - Dato NO Esta Creado"  TO DAT1-ENV
                    MOVE "SER106-02"                    TO DAT2-ENV
             WHEN 9 MOVE "88"                           TO DAT0-ENV
                    MOVE "Error - Dato NO Esta Creado"  TO DAT1-ENV
                    MOVE "SER106-02"                    TO DAT2-ENV
           END-EVALUATE

           GO TO ENVIO-DATOS.   


       ENVIO-DATOS.
           MOVE "datosrecibidos"  TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO       TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"   USING COBW3

           MOVE "..\..\FRAMEWORKS\PAGINAS\RECIBIDOS.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.
        
       COPY "P:\PROG\PROYECT\FUENTES\SC-WEB19.CBL".
      
