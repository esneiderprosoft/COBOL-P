      *=================================================================
      *GRABAR ARCHIVO-NOMBRE-TAR SAL713-02  SALUD
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.                   
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
       
           SELECT ARCHIVO-TABLAS 
                  ASSIGN NOM-TABLA-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS LLAVE-TAB
                  ALTERNATE RECORD KEY IS DESCRIP-TAB WITH DUPLICATES
                  ALTERNATE RECORD KEY IS COD-RIPS-TAB WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL". 
       COPY "..\..\FUENTES\FD-TABLA19.CBL".

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-PLANO-W               PIC X(90).
       77 NOM-TABLA-W               PIC X(60).      
       77 MES-SAL-W                 PIC X(4).


       01 LINEA-LLEGADA-W           PIC X(500).

       01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGADA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 DIR-CONTAB-LLEGADA-W      PIC X(30).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
      *     02 OPERADOR-LLEGADA-W        PIC X(4).
           02 NOV-LLEGADA-W             PIC X.

   
       01 REG-TAB-W.
          02 LLAVE-TAB-W.
             03 LLAVE-TIPO-TAB-W.
                05 COD-TAB-W.
                   07 COD1-TAB-W        PIC X.
                   07 COD2-TAB-W        PIC X.
                05 TIPO-TAB-W           PIC 9.
             03 COD-SER-TAB-W.
                05 GR-SER-TAB-W         PIC XX.
                05 CD-SER-TAB-W.     
                   07 CD-SER1-TAB-W     PIC XX.
                   07 CD-SER2-TAB-W     PIC X(8).
          02 DESCRIP-TAB-W.
             03 INI-TAB-W.
                05 DESCRIP1-TAB-W       PIC X.
                05 DESCRIP2-TAB-W       PIC XX.
             03 DESCRIP3-TAB-W          PIC X(27).
          02 FORMA-LIQ-TAB-W            PIC 9.
          02 MONTO-TAB-W                PIC 9(12)V99.
          02 CTA-CONTAB-TAB-W.
             05 MAY-CONTAB-TAB-W        PIC X(4).
             05 COD-SCTA-TAB-W.
                07 SCT-CONTAB-TAB-W     PIC XX.
                07 AUX-CONTAB-TAB-W     PIC X(5).
          02 COD-PAQ-INT-TAB-W          PIC X(10).
          02 COD-RIPS-TAB-W.
             05 GRP-RIPS-TAB-W.
                07 COD1-RIPS-TAB-W      PIC X.
                07 COD2-RIPS-TAB-W      PIC X.
             05 COD3-RIPS-TAB-W         PIC X(09).
          02 PAQUETE-TAB-W              PIC X.
          02 FILLER                     PIC X(12).
          02 CANT-DIA-AUTORI-TAB-W      PIC 9(3).
          02 INCREM-TAB-W               PIC 9.
          02 DIV-TAB-W                  PIC XX.
          02 EXTENSION-TAB-W.       
             03 RENGLON1-TAB1-W         PIC X(30).
             03 RENGLON2-TAB1-W         PIC X(30).
             03 RENGLON3-TAB1-W         PIC X(30).
             03 RENGLON4-TAB1-W         PIC X(30).
          02 FILLER                     PIC X(100).
          02 OPER-TAB-W                 PIC X(4).
          02 FEC-ACT-TAB-W.
             03 ANO-ACT-TAB-W           PIC 9(4).
             03 MES-ACT-TAB-W           PIC 99.
             03 DIA-ACT-TAB-W           PIC 99.





       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.
          02 FILLER                    PIC X VALUE "|".
          02 DAT1-ENV                  PIC X(30).

      
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
              MOVE "SAL713-02"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TABLAS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE                    
           ELSE
              MOVE OTR-STAT                       TO MSJ1-HTML
              MOVE "SAL713-02-ARCHIVO-TABLAS"     TO MSJ2-HTML
              MOVE NOM-TABLA-W                    TO MSJ3-HTML
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
              MOVE "SAL713-02"              TO MSJ3-HTML
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

           INITIALIZE REG-TAB-W.

           UNSTRING LINEA-LLEGADA-W  DELIMITED BY "|"
               INTO LLAVESESION-LLEGADA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, NOV-LLEGADA-W, LLAVE-TAB-W, DESCRIP-TAB-W,
                    FORMA-LIQ-TAB-W, MONTO-TAB-W, CTA-CONTAB-TAB-W, COD-PAQ-INT-TAB-W, COD-RIPS-TAB-W, PAQUETE-TAB-W, CANT-DIA-AUTORI-TAB-W, INCREM-TAB-W,
                    DIV-TAB-W, EXTENSION-TAB-W, OPER-TAB-W, FEC-ACT-TAB-W 
           END-UNSTRING.


      *     MOVE "01"                 TO MSJ1-HTML
      *     MOVE LLAVE-TAB-W             TO MSJ2-HTML
      *     MOVE "SAL713-02"              TO MSJ3-HTML
      *     GO TO ENVIAR2-ERROR.

      * OTR.

           MOVE LLAVESESION-LLEGADA-W    TO LLAVE-SESION-W.
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
           INITIALIZE NOM-TABLA-W

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

           MOVE "\\" TO  NOM-TABLA-W

           INSPECT NOM-TABLA-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-TABLA-W REPLACING FIRST " "
                                             BY "\"

           INSPECT NOM-TABLA-W   REPLACING FIRST  "                              "
                                              BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT NOM-TABLA-W   REPLACING FIRST "                     "
                                             BY  "\CONTROL\SC-TABLA.DAT".


       ABRIR-NOMBRE-TAR.
           OPEN INPUT ARCHIVO-TABLAS.

           EVALUATE OTR-STAT
             WHEN "00" CONTINUE
             WHEN "35" OPEN OUTPUT ARCHIVO-TABLAS
                       CLOSE       ARCHIVO-TABLAS
                       GO TO ABRIR-NOMBRE-TAR
           END-EVALUATE.
 

       LEER-NOMBRE-TAR.
           MOVE 00 TO SW-INVALID
           MOVE LLAVE-TAB-W TO LLAVE-TAB
           READ ARCHIVO-TABLAS  WITH NO LOCK
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
           INITIALIZE REG-TAB
           MOVE REG-TAB-W TO REG-TAB

           CLOSE ARCHIVO-TABLAS.
           OPEN I-O ARCHIVO-TABLAS.

           WRITE REG-TAB END-WRITE
           CLOSE ARCHIVO-TABLAS.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO GRABADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       CAMBIO.
           CLOSE ARCHIVO-TABLAS.
           OPEN I-O ARCHIVO-TABLAS.

           MOVE REG-TAB-W TO REG-TAB

           REWRITE REG-TAB END-REWRITE
           CLOSE ARCHIVO-TABLAS. 

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO MODIFICADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       RETIRO.
           CLOSE ARCHIVO-TABLAS.

           OPEN I-O ARCHIVO-TABLAS.

           DELETE ARCHIVO-TABLAS END-DELETE

           CLOSE ARCHIVO-TABLAS.

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