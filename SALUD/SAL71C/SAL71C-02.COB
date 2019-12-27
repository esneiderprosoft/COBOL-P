      *=================================================================
      *GRABAR ARCHIVO-GR-CAP SAL71C-01  SALUD
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

           SELECT ARCHIVO-GR-CAP LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-GRPCAP-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CODIGO-GRCAP
                  FILE STATUS IS OTR-STAT.

      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-GRCAP.CBL".

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-GRPCAP-W              PIC X(60).
       77 MES-SAL-W                 PIC X(4).

       01 LINEA-LLEGAD-W            PIC X(150).


       01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 DIR-CONTAB-LLEGADA-W      PIC X(8).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 NOV-LLEGADA-W             PIC X.         


       01 REG-GRCAP-W.
           02 CODIGO-GRCAP-W.
              05 COD1-GRCAP-W           PIC X.
              05 COD2-GRCAP-W           PIC X.
           02 DESCRIP-GRCAP-W           PIC X(25).
           02 FILLER                    PIC X(30).
           02 TABLA-GRCAP-W.
              05 CLASE-GRCAP-W          PIC X OCCURS 9.
           02 FILLER                    PIC X(2000).



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
              MOVE "SAL71C-02"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-GR-CAP.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"         
              CONTINUE            
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE "SAL71C-02"                TO MSJ2-HTML
              MOVE NOM-GRPCAP-W               TO MSJ3-HTML
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
              MOVE "SAL71C-02"              TO MSJ3-HTML
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
              INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, NOV-LLEGADA-W, 
                   CODIGO-GRCAP-W, DESCRIP-GRCAP-W, CLASE-GRCAP-W(1), CLASE-GRCAP-W(2), CLASE-GRCAP-W(3),
                   CLASE-GRCAP-W(4), CLASE-GRCAP-W(5), CLASE-GRCAP-W(6), CLASE-GRCAP-W(7), CLASE-GRCAP-W(8)
           END-UNSTRING.

           MOVE LLAVESESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.



      *     MOVE "MM"                     TO MSJ1-HTML
      *     MOVE "SAL71C-02"              TO MSJ2-HTML
      *     MOVE LINEA-LLEGAD-W           TO MSJ3-HTML
      *     GO TO ENVIAR2-ERROR.

      * OTR.

           
           
       INICIAR-SESION.
      *     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
           MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W
           GO TO VALIDAR-SESION.


       FIN-VALIDAR-SESION.

       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
      *     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
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
           INITIALIZE NOM-GRPCAP-W

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           
           MOVE "\\" TO NOM-GRPCAP-W

           INSPECT NOM-GRPCAP-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET
                                          
           INSPECT NOM-GRPCAP-W REPLACING FIRST " "
                                          BY    "\"

           INSPECT NOM-GRPCAP-W REPLACING FIRST "        "
                                             BY DIR-CONTAB-LLEGADA-W 

           INSPECT NOM-GRPCAP-W   REPLACING FIRST "                        "
                                               BY "\CONTROL\SC-GRUPOCAP.DAT".

       ABRIR-GRUPO-SER.
           OPEN INPUT ARCHIVO-GR-CAP.  

           
       LEER-GRUPO-SER.
           MOVE CODIGO-GRCAP-W TO CODIGO-GRCAP
           READ ARCHIVO-GR-CAP  WITH NO LOCK
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
      *     INITIALIZE REG-GRCAP.

           MOVE REG-GRCAP-W    TO REG-GRCAP

      *     MOVE CODIGO-GRCAP-W    TO CODIGO-GRCAP
      *     MOVE DESCRIP-GRCAP-W   TO DESCRIP-GRCAP
      *     MOVE CLASE-GRCAP-W (1) TO CLASE-GRCAP(I)
      *     MOVE CLASE-GRCAP-W (2) TO CLASE-GRCAP(I)
      *     MOVE CLASE-GRCAP-W (3) TO CLASE-GRCAP(I)
      *     MOVE CLASE-GRCAP-W (4) TO CLASE-GRCAP(I)
      *     MOVE CLASE-GRCAP-W (5) TO CLASE-GRCAP(I)
      *     MOVE CLASE-GRCAP-W (6) TO CLASE-GRCAP(I)
      *     MOVE CLASE-GRCAP-W (7) TO CLASE-GRCAP(I)
      *     MOVE CLASE-GRCAP-W (8) TO CLASE-GRCAP(I)

           CLOSE ARCHIVO-GR-CAP.

           OPEN I-O ARCHIVO-GR-CAP.


           WRITE REG-GRCAP END-WRITE
           CLOSE ARCHIVO-GR-CAP.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO GRABADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.           

       CAMBIO.

           CLOSE ARCHIVO-GR-CAP.
           OPEN I-O ARCHIVO-GR-CAP.

           MOVE REG-GRCAP-W    TO REG-GRCAP

      *     MOVE CODIGO-GRCAP-W    TO CODIGO-GRCAP
      *     MOVE DESCRIP-GRCAP-W   TO DESCRIP-GRCAP
      *     MOVE CLASE-GRCAP-W (1) TO CLASE-GRCAP(I)
      *     MOVE CLASE-GRCAP-W (2) TO CLASE-GRCAP(I)
      *     MOVE CLASE-GRCAP-W (3) TO CLASE-GRCAP(I)
      *     MOVE CLASE-GRCAP-W (4) TO CLASE-GRCAP(I)
      *     MOVE CLASE-GRCAP-W (5) TO CLASE-GRCAP(I)
      *     MOVE CLASE-GRCAP-W (6) TO CLASE-GRCAP(I)
      *     MOVE CLASE-GRCAP-W (7) TO CLASE-GRCAP(I)
      *     MOVE CLASE-GRCAP-W (8) TO CLASE-GRCAP(I)

           REWRITE REG-GRCAP END-REWRITE

           CLOSE ARCHIVO-GR-CAP. 
           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO MODIFICADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.            

       RETIRO.
           CLOSE ARCHIVO-GR-CAP.
           OPEN I-O ARCHIVO-GR-CAP.
     
           DELETE ARCHIVO-GR-CAP END-DELETE
           CLOSE ARCHIVO-GR-CAP.

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