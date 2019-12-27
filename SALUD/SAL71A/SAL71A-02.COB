      *============================================================
      * GRABAR DATOS  SER11A-02  SALUD
      *============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.                 
       CONFIGURATION SECTION.           
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL". 

           SELECT ARCHIVO-ESPEC-CUP LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-ESPCUPS-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-ESP-CUP
                  FILE STATUS IS OTR-STAT.


       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-ESCUP.CBL".


       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-ESPCUPS-W                 PIC X(60).
 

       01 LLEGADA-W                     PIC X(200).
       01 FECHA-W                       PIC X(8).
       01 OPER-W                        PIC X(4).

       77 MES-SAL-W                     PIC X(4).

       01 REG-LLEGADA-W.
          02 LLAVESESION-LLEGA-W       PIC X(29).
          02 DIR-CONTAB-LLEGADA-W      PIC X(8).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 NOVEDAD-LLEGADA-W         PIC X.
      *    02 COD-LLEGADA-W             PIC X(12).

       01 REG-ESP-CUP-W.
          02 COD-ESP-CUP-W.
             05 GRP-ESP-CUP-W           PIC XX.
             05 NRO-ESP-CUP-W           PIC X(10).
          02 DATOS-ESP-CUP-W.   
             05 ESP-CUP-W               PIC X(3) OCCURS 50.
             05 OPER-ESP-CUP-W          PIC X(4).
             05 FECHA-ESP-CUP-W         PIC X(8).
             05 SEXO-ESP-CUP-W          PIC X.
             05 ATIENDE-ESP-CUP-W       PIC X.
             05 PYP-CUP-W               PIC X.
             05 TIPO-PROC-CUP-W         PIC 9.
             05 FINALID-CUP-W           PIC 99.
             05 FILLER                  PIC X(2000).


       01 DATOS-ENVIO.
          02 DAT0-ENV                         PIC XX.
          02 FILLER                           PIC X VALUE "|".
          02 DAT1-ENV                         PIC X(30).


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
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-USU-W                 TO MSJ2-HTML
              MOVE "SER11A-02"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-SESION-W              TO MSJ2-HTML
              MOVE "SER11A-02"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.


       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ESPEC-CUP.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE NOM-ESPCUPS-W              TO MSJ2-HTML
              MOVE "SER11A-02"                TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO LLEGADA-W.
           
           INITIALIZE REG-ESP-CUP-W

           UNSTRING LLEGADA-W DELIMITED BY "|"
               INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, NOVEDAD-LLEGADA-W, COD-ESP-CUP-W,
                    ESP-CUP-W (1), ESP-CUP-W (2), ESP-CUP-W (3), ESP-CUP-W (4), ESP-CUP-W (5), ESP-CUP-W (6),
                    ESP-CUP-W (7), ESP-CUP-W (8), ESP-CUP-W (9), ESP-CUP-W (10), ESP-CUP-W (11), ESP-CUP-W (12),
                    ESP-CUP-W (13), ESP-CUP-W (14), ESP-CUP-W (15), ESP-CUP-W (16), ESP-CUP-W (17), ESP-CUP-W (18),
                    ESP-CUP-W (19), ESP-CUP-W (20), ESP-CUP-W (21), ESP-CUP-W (22), ESP-CUP-W (23), ESP-CUP-W (24),
                    ESP-CUP-W (25), ESP-CUP-W (26), ESP-CUP-W (27), ESP-CUP-W (28), ESP-CUP-W (29), OPER-ESP-CUP-W,
                    FECHA-ESP-CUP-W, SEXO-ESP-CUP-W, ATIENDE-ESP-CUP-W, PYP-CUP-W, TIPO-PROC-CUP-W, FINALID-CUP-W                       
           END-UNSTRING.

           MOVE LLAVESESION-LLEGA-W    TO LLAVE-SESION-W.
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
           INITIALIZE NOM-ESPCUPS-W.
		   
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".
		   
           MOVE "\\" TO NOM-ESPCUPS-W
		   
           INSPECT NOM-ESPCUPS-W REPLACING FIRST "                    "
                                              BY IP-DATOS-USUNET

           INSPECT NOM-ESPCUPS-W REPLACING FIRST " "
                                              BY "\"

           INSPECT NOM-ESPCUPS-W REPLACING FIRST "                         "
                                              BY "PROG\DATOS\SC-ESP-CUP.DAT".

         
       ABRIR-ARCHIVOS.
           OPEN INPUT ARCHIVO-ESPEC-CUP.
                     

       NOVEDAD.
           INITIALIZE SW-INVALID 

           MOVE COD-ESP-CUP-W TO COD-ESP-CUP
           READ ARCHIVO-ESPEC-CUP WITH NO LOCK
                  INVALID KEY
                     MOVE 01 TO SW-INVALID
           END-READ.

           IF NOVEDAD-LLEGADA-W = 7 AND SW-INVALID = 01 GO TO NUEVO.
           IF NOVEDAD-LLEGADA-W = 7 AND SW-INVALID = 00 GO TO ERROR-1.
           IF NOVEDAD-LLEGADA-W = 8 AND SW-INVALID = 00 GO TO CAMBIO.
           IF NOVEDAD-LLEGADA-W = 8 AND SW-INVALID = 01 GO TO ERROR-1.
           IF NOVEDAD-LLEGADA-W = 9 AND SW-INVALID = 00 GO TO RETIRO.
           IF NOVEDAD-LLEGADA-W = 9 AND SW-INVALID = 01 GO TO ERROR-1.


        NUEVO.    
      *     ACCEPT FECHA-W FROM DATE
      *     MOVE FECHA-W TO FECHA-ESP-CUP-W 

           MOVE REG-ESP-CUP-W TO REG-ESP-CUP
      
           CLOSE      ARCHIVO-ESPEC-CUP
           OPEN I-O   ARCHIVO-ESPEC-CUP

           WRITE REG-ESP-CUP END-WRITE       
           CLOSE      ARCHIVO-ESPEC-CUP.
           
           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO CREADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       CAMBIO.
           CLOSE      ARCHIVO-ESPEC-CUP
           OPEN I-O   ARCHIVO-ESPEC-CUP

           MOVE REG-ESP-CUP-W  TO REG-ESP-CUP

           REWRITE REG-ESP-CUP END-REWRITE. 
           CLOSE      ARCHIVO-ESPEC-CUP.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO MODIFICADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       RETIRO.
           CLOSE      ARCHIVO-ESPEC-CUP
           OPEN I-O   ARCHIVO-ESPEC-CUP

           DELETE     ARCHIVO-ESPEC-CUP END-DELETE
           CLOSE      ARCHIVO-ESPEC-CUP.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO ELIMINADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       ERROR-1.
           MOVE 99 TO DAT0-ENV
           MOVE SW-INVALID TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       ENVIO-DATOS.
           MOVE "datosrecibidos"  TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO       TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"   USING COBW3
           MOVE "../PAGINAS/RECIBIDOS.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML.

       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.

       COPY "..\..\FUENTES\SC-WEB.CBL".
