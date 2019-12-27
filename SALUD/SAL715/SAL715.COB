      *=================================================================
      *GRABAR PORCENTAJE SER105 --- SAL715
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
       COPY "..\..\FUENTES\FS-TABLA19.CBL".       

      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-TABLA19.CBL".

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77  NOM-TAB-LNK                 PIC X(60).

       77  PORC-EDIT                   PIC ZZ.ZZ.
       77  CLAVE-W                     PIC X(6).
       77  GRUPO-W                     PIC XX.

       01  VARIABLES.
           02  INVALID-KEY             PIC 9.
           02  COD-W                   PIC XX.
           02  COD-A                   PIC XX.
           02  COD-TR                  PIC XX.
           02  INCREM-W                PIC 99V99.
	   02  VALOR-W                 PIC 9(9).
           02  FINAL-W                 PIC 9.
           02  DIF-W                   PIC 9.
           02  SW-APROX                PIC 9(4).


       01 LINEA-LLEGAD-W            PIC X(150).

       01 INGRES-TERC-EDIT.
          02 NUM-TERC-EDIT          PIC XX.
          02 NUM2-TERC-EDIT         PIC X.

       01 DATO-LLEGADA-W.
          02 LLAVESESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(30).
          02 MES-CONTAB-LLEGADA-W      PIC XX.


       01 REG-TAB-W.
          02 LLAVE-TAB-W.
             03 LLAVE-TIPO-TAB-W.
                05 COD-TAB-W.
                   07 COD1-TAB-W      PIC X.
                   07 COD2-TAB-W      PIC X.
                05 TIPO-TAB-W         PIC 9.
             03 COD-SER-TAB-W.
                05 GR-SER-TAB-W       PIC XX.
                05 CD-SER-TAB-W.
                   07 CD-SER1-TAB-W   PIC XX.
                   07 CD-SER2-TAB-W   PIC X(8).
          02 DESCRIP-TAB-W.
             03 INI-TAB-W.
                05 DESCRIP1-TAB-W     PIC X.
                05 DESCRIP2-TAB-W     PIC XX.
             03 DESCRIP3-TAB-W        PIC X(27).
          02 FORMA-LIQ-TAB-W          PIC 9.
          02 MONTO-TAB-W              PIC 9(12)V99.
          02 CTA-CONTAB-TAB-W.
             05 MAY-CONTAB-TAB-W      PIC X(4).
             05 COD-SCTA-TAB-W.
                07 SCT-CONTAB-TAB-W   PIC XX.
                07 AUX-CONTAB-TAB-W   PIC X(5).
          02 COD-PAQ-INT-TAB-W        PIC X(10).
          02 COD-RIPS-TAB-W.
             05 GRP-RIPS-TAB-W.
                07 COD1-RIPS-TAB-W    PIC X.
                07 COD2-RIPS-TAB-W    PIC X.
             05 COD3-RIPS-TAB-W       PIC X(09).
          02 PAQUETE-TAB-W            PIC X.
          02 FILLER                   PIC X(12).
          02 CANT-DIA-AUTORI-TAB-W    PIC 9(3).
          02 INCREM-TAB-W             PIC 9.
          02 DIV-TAB-W                PIC XX.
          02 EXTENSION-TAB-W.
             03 RENGLON1-TAB1-W       PIC X(30).
             03 RENGLON2-TAB1-W       PIC X(30).
             03 RENGLON3-TAB1-W       PIC X(30).
             03 RENGLON4-TAB1-W       PIC X(30).
          02 FILLER                   PIC X(100).
          02 OPER-TAB-W               PIC X(4).
          02 FEC-ACT-TAB-W.
             03 ANO-ACT-TAB-W         PIC 9(4).
             03 MES-ACT-TAB-W         PIC 99.
             03 DIA-ACT-TAB-W         PIC 99.
          02 FILLER                   PIC X(2000).
                                     

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
              MOVE "SAL715"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TABLAS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT            TO MSJ1-HTML
              MOVE NOM-TAB-LNK         TO MSJ2-HTML
              MOVE "SAL715"            TO MSJ3-HTML
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
              MOVE "SAL715"                 TO MSJ3-HTML
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
              INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, COD-TAB-W, INCREM-TAB-W
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
           INITIALIZE NOM-TAB-LNK

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           
           MOVE "\\" TO NOM-TAB-LNK

           INSPECT NOM-TAB-LNK REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET
                                          
           INSPECT NOM-TAB-LNK REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-TAB-LNK REPLACING FIRST "                              "
                                            BY DIR-CONTAB-LLEGADA-W 

           INSPECT NOM-TAB-LNK   REPLACING FIRST "                     "
                                              BY "\CONTROL\SC-TABLA.DAT".


       ABRIR-TABLAS.
           OPEN INPUT ARCHIVO-TABLAS.  

           
       LEER-TABLAS.
           IF COD-TAB-W = "IS"
              MOVE 1 TO SW-APROX
           ELSE
              MOVE 100 TO SW-APROX
           END-IF

           IF FORMA-LIQ-TAB = 3
              IF SW-APROX > 1
                 COMPUTE VALOR-W ROUNDED =  MONTO-TAB * ( 100 + INCREM-W ) / (SW-APROX * 100)
              ELSE
                 COMPUTE VALOR-W = MONTO-TAB * ( 100 + INCREM-W ) / (SW-APROX * 100)
                 MOVE VALOR-W TO FINAL-W
                 IF FINAL-W < 4
                    SUBTRACT FINAL-W FROM VALOR-W
                 ELSE
                    IF FINAL-W > 3 AND < 8
                       COMPUTE DIF-W = 5 - FINAL-W
                       ADD DIF-W TO VALOR-W
                    ELSE
                       COMPUTE DIF-W = 10 - FINAL-W
                       ADD DIF-W TO VALOR-W
                    END-IF
                 END-IF
              END-IF

              COMPUTE MONTO-TAB = VALOR-W * SW-APROX
              REWRITE REG-TAB
           END-IF


           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO GRABADO" TO DAT1-ENV.

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