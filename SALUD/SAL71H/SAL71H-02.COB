      *=================================================================
      *GRABAR ARCHIVO-NOMBRE-COMU SAL71H-02  SALUD
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.                   
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
       

           SELECT ARCHIVO-NOMBRE-COMU
                  ASSIGN NOM-COMU-W 
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-NOM-CU
                  FILE STATUS IS OTR-STAT.

      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-NOMCOMU.CBL".

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-PLANO-W               pic x(90).
       77 NOM-COMU-W                PIC X(60).
       77 MES-SAL-W                 PIC X(4).


       01 LINEA-LLEGADA-W           PIC X(500).

       01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGADA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 DIR-CONTAB-LLEGADA-W      PIC X(8).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 NOV-LLEGADA-W             PIC X.

   
       01 REG-NOMCU-W.
          02 LLAVE-NOM-CUM-W.
             03 CODIGO-NOMCU-W          PIC 9(3).
          02 DESCRIP-NOMCU-W            PIC X(30).
          02 FILLER                     PIC X(2000).



       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.
          02 FILLER                    PIC X VALUE "|".
          02 DAT1-ENV                  PIC X(30).
          02 FILLER                    PIC X VALUE "|".
          02 DAT2-ENV                  PIC X(20).

      
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
              MOVE "SAL71H-02"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-NOMBRE-COMU.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE                    
           ELSE
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-COMU-W                TO MSJ2-HTML
              MOVE "SAL71H-02"               TO MSJ3-HTML
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
              MOVE "SAL71H-02"              TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO LINEA-LLEGADA-W.

           INITIALIZE REG-NOMCU-W.

           UNSTRING LINEA-LLEGADA-W  DELIMITED BY "|"
               INTO LLAVESESION-LLEGADA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, NOV-LLEGADA-W, CODIGO-NOMCU-W, DESCRIP-NOMCU-W
           END-UNSTRING.

       INICIAR-SESION.
           MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W

           MOVE LLAVESESION-LLEGADA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.


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
           INITIALIZE NOM-COMU-W.

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

           MOVE "\\" TO  NOM-COMU-W

           INSPECT NOM-COMU-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

           INSPECT NOM-COMU-W REPLACING FIRST " "
                                             BY "\"

           INSPECT NOM-COMU-W   REPLACING FIRST  "        "
                                               BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT NOM-COMU-W   REPLACING FIRST "                         "
                                             BY "\CONTROL\SC-GRUPOCOMU.DAT".



       ABRIR-NOMBRE-COMU.
           OPEN INPUT ARCHIVO-NOMBRE-COMU

           EVALUATE OTR-STAT
             WHEN "00" CONTINUE
             WHEN "35" OPEN OUTPUT ARCHIVO-NOMBRE-COMU
                       CLOSE       ARCHIVO-NOMBRE-COMU
                       GO TO ABRIR-NOMBRE-COMU
           END-EVALUATE.
 

       LEER-NOMBRE-COMU.
           MOVE 00 TO SW-INVALID
           MOVE CODIGO-NOMCU-W TO CODIGO-NOMCU
           READ ARCHIVO-NOMBRE-COMU  WITH NO LOCK
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
           MOVE REG-NOMCU-W TO REG-NOMCU

           CLOSE ARCHIVO-NOMBRE-COMU.
           OPEN I-O ARCHIVO-NOMBRE-COMU.

           WRITE REG-NOMCU END-WRITE

           CLOSE ARCHIVO-NOMBRE-COMU.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO GRABADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       CAMBIO.
           CLOSE ARCHIVO-NOMBRE-COMU.
           OPEN I-O ARCHIVO-NOMBRE-COMU.

           MOVE REG-NOMCU-W TO REG-NOMCU

           REWRITE REG-NOMCU END-REWRITE
           CLOSE ARCHIVO-NOMBRE-COMU. 

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO MODIFICADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       RETIRO.
           CLOSE ARCHIVO-NOMBRE-COMU.

           OPEN I-O ARCHIVO-NOMBRE-COMU.

           DELETE ARCHIVO-NOMBRE-COMU END-DELETE

           CLOSE ARCHIVO-NOMBRE-COMU.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO BORRADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       ERROR-1.
           MOVE 99 TO DAT0-ENV
           MOVE "ERROR" TO DAT1-ENV.

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
        
       COPY "..\..\FUENTES\SC-WEB19.CBL".
      
