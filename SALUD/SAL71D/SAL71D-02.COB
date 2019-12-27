      *=================================================================
      *GRABAR ARCHIVO-NOMBRE-TAR SAL71D-02  SALUD
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".

      * "\PROG\DATOS\SC-COLEG.DAT"
           SELECT ARCHIVO-COLEGIOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-COLEG-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY LLAVE-COLEG
                  ALTERNATE RECORD KEY IS DESCRIP-COLEG WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-COLEG.CBL".

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-COLEG-W               PIC X(60).
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
           02 DIR-CONTAB-LLEGADA-W      PIC X(30).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 NOVED-LLEGADA-W           PIC X.

       01  REG-COLEG-W.
           02 LLAVE-COLEG-W.
              05 TIPO-COLEG-W           PIC X.
              05 COD-COLEG-W.
                 07 COD-CIU-COLEG-W     PIC 9(5).
                 07 SECU-COLEG-W        PIC 9(6).
           02 DESCRIP-COLEG-W           PIC X(50).
           02 NUCLEO-COLEG-W            PIC X(10).
           02 CIUDAD-COLEG-W.
              05 DPTO-COLEG-W           PIC 99.
              05 CIU-COLEG-W            PIC 9(3).
           02 ZONA-COLEG-W              PIC 9.
           02 DIRECCION-COLEG-W         PIC X(30).
           02 TELEFONO-COLEG-W          PIC X(10).



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
              MOVE "SAL71D-02"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-COLEGIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"         
              CONTINUE            
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE "SAL71D-02"                TO MSJ2-HTML
              MOVE NOM-COLEG-W                TO MSJ3-HTML
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
              MOVE "SAL71D-02"              TO MSJ3-HTML
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

           INITIALIZE REG-COLEG-W.

           UNSTRING LINEA-LLEGAD-W DELIMITED BY "|"
              INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, 
                   NOVED-LLEGADA-W, LLAVE-COLEG-W, DESCRIP-COLEG-W, NUCLEO-COLEG-W, CIUDAD-COLEG-W,
                   ZONA-COLEG-W, DIRECCION-COLEG-W, TELEFONO-COLEG-W
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
              GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.
           INITIALIZE NOM-COLEG-W

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           
           MOVE "\\" TO NOM-COLEG-W

           INSPECT NOM-COLEG-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET
                                          
           INSPECT NOM-COLEG-W REPLACING FIRST "                        "
                                            BY "\PROG\DATOS\SC-COLEG.DAT".



       ABRIR-GRUPO-SER.
           OPEN INPUT ARCHIVO-COLEGIOS.  

           
       LEER-GRUPO-SER.
           MOVE LLAVE-COLEG-W TO LLAVE-COLEG 
           READ ARCHIVO-COLEGIOS  WITH NO LOCK
                INVALID KEY 
                  MOVE 1 TO SW-INVALID
           END-READ.

           IF NOVED-LLEGADA-W = 7 AND SW-INVALID = 1 GO TO NUEVO.
           IF NOVED-LLEGADA-W = 7 AND SW-INVALID = 0 GO TO ERROR-1.
           IF NOVED-LLEGADA-W = 8 AND SW-INVALID = 0 GO TO CAMBIO.
           IF NOVED-LLEGADA-W = 8 AND SW-INVALID = 1 GO TO ERROR-1.
           IF NOVED-LLEGADA-W = 9 AND SW-INVALID = 0 GO TO RETIRO.
           IF NOVED-LLEGADA-W = 9 AND SW-INVALID = 1 GO TO ERROR-1.

       NUEVO.
           INITIALIZE REG-COLEG.

           MOVE REG-COLEG-W TO REG-COLEG

           CLOSE ARCHIVO-COLEGIOS.

           OPEN I-O ARCHIVO-COLEGIOS.


           WRITE REG-COLEG END-WRITE
           CLOSE ARCHIVO-COLEGIOS.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO GRABADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.           

       CAMBIO.

           CLOSE ARCHIVO-COLEGIOS.
           OPEN I-O ARCHIVO-COLEGIOS.

           MOVE REG-COLEG-W TO REG-COLEG

           REWRITE REG-COLEG END-REWRITE

           CLOSE ARCHIVO-COLEGIOS. 
           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO MODIFICADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.            

       RETIRO.
           CLOSE ARCHIVO-COLEGIOS.
           OPEN I-O ARCHIVO-COLEGIOS.
     
           DELETE ARCHIVO-COLEGIOS END-DELETE
           CLOSE ARCHIVO-COLEGIOS.

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