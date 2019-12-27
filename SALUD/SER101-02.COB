      *=================================================================
      *GRABAR ARCHIVO-NOMBRE-TAR SER101-02  SALUD
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
       
      * "../control/SC-GRUPOTAR.DAT"

           SELECT ARCHIVO-GR-SER LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-GRUSER-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CODIGO-GRSER
                  FILE STATUS IS OTR-STAT.

      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-GRSER.CBL".

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-GRUSER-W              PIC X(60).
       77 MES-SAL-W                 PIC X(4).
       01 LINEA-LLEGAD-W            PIC X(150).
       01 INGRES-TERC-EDIT.
          02 NUM-TERC-EDIT          PIC XX.
          02 NUM2-TERC-EDIT         PIC X.

       01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 DIR-CONTAB-LLEGADA-W      PIC X(8).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 NOV-LLEGADA-W             PIC X.
           02 COD-GRUP-LLEGADA-W        PIC X(2).
           02 DESCRIP-LLEGADA-W         PIC X(30).
           02 INGRESO-CLIN-W            PIC 9(3).
           02 INGRES-TERCE-W            PIC 99V9.
           02 CODIGO-CONTAB-W           PIC X(11).

       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.
          02 FILLER                    PIC X VALUE "|".
          02 DAT1-ENV                  PIC X(25).
          02 FILLER                    PIC X VALUE "|".


      
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
              MOVE "SER101"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-GR-SER.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-GRUSER-W         TO MSJ2-HTML
              MOVE "SER101-02"          TO MSJ3-HTML
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
              MOVE "SER101"                 TO MSJ3-HTML
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
              INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, 
              NOV-LLEGADA-W, COD-GRUP-LLEGADA-W, DESCRIP-LLEGADA-W, INGRESO-CLIN-W, 
              INGRES-TERC-EDIT, CODIGO-CONTAB-W
           END-UNSTRING.

           MOVE INGRES-TERC-EDIT TO INGRES-TERCE-W

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
           INITIALIZE NOM-GRUSER-W

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           
           MOVE "\\" TO NOM-GRUSER-W

           INSPECT NOM-GRUSER-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET
                                          
           INSPECT NOM-GRUSER-W REPLACING FIRST " "
                                          BY    "\"

           INSPECT NOM-GRUSER-W REPLACING FIRST "        "
                                             BY DIR-CONTAB-LLEGADA-W 

           INSPECT NOM-GRUSER-W   REPLACING FIRST "                        "
                                               BY "\CONTROL\SC-GRUPOSER.DAT".

       ABRIR-GRUPO-SER.
           OPEN INPUT ARCHIVO-GR-SER.  

           
       LEER-GRUPO-SER.
           MOVE COD-GRUP-LLEGADA-W TO CODIGO-GRSER 
           READ ARCHIVO-GR-SER  WITH NO LOCK
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
           INITIALIZE REG-GRSER.

           MOVE COD-GRUP-LLEGADA-W TO CODIGO-GRSER
           MOVE DESCRIP-LLEGADA-W  TO DESCRIP-GRSER
           MOVE INGRESO-CLIN-W     TO PORC-CL-GRSER
           MOVE INGRES-TERCE-W     TO PORC-OTR-GRSER 
           MOVE CODIGO-CONTAB-W    TO OTR-CTA-GRSER
           CLOSE ARCHIVO-GR-SER.

           OPEN I-O ARCHIVO-GR-SER.


           WRITE REG-GRSER END-WRITE
           CLOSE ARCHIVO-GR-SER.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO GRABADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.           

       CAMBIO.

           CLOSE ARCHIVO-GR-SER.
           OPEN I-O ARCHIVO-GR-SER.

           MOVE COD-GRUP-LLEGADA-W TO CODIGO-GRSER
           MOVE DESCRIP-LLEGADA-W  TO DESCRIP-GRSER
           MOVE INGRESO-CLIN-W     TO PORC-CL-GRSER
           MOVE INGRES-TERCE-W     TO PORC-OTR-GRSER 
           MOVE CODIGO-CONTAB-W    TO OTR-CTA-GRSER

           REWRITE REG-GRSER END-REWRITE

           CLOSE ARCHIVO-GR-SER. 
           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO MODIFICADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.            

       RETIRO.
           CLOSE ARCHIVO-GR-SER.
           OPEN I-O ARCHIVO-GR-SER.
     
           DELETE ARCHIVO-GR-SER END-DELETE
           CLOSE ARCHIVO-GR-SER.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO BORRADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

           
       ERROR-1.
           MOVE 99 TO DAT0-ENV
           MOVE "ERROR" TO DAT1-ENV.

           GO TO ENVIO-DATOS.   


       ENVIO-DATOS.
           MOVE 00 TO DAT0-ENV
           MOVE "CORRECTO" TO DAT1-ENV


           MOVE "datosrecibidos"  TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO       TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"   USING COBW3

           MOVE "..\..\FRAMEWORKS\PAGINAS\RECIBIDOS.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.
      
       COPY "..\..\FUENTES\SC-WEB19.CBL".
      

