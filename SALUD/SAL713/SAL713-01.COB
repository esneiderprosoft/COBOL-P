      *============================================================
      * CREACION DE ARCHIVO JSON SAL713-01  SALUD
      *============================================================
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

       01 TABLA-LLEGADA-W.
          02 DATO-LLEGADA-W PIC X OCCURS 200.
       
       01 LLEGADA-W         PIC X(100).

       01 TABLA-TEMP.
          02 CARAC-TEMP     PIC X OCCURS 50.

       77 NOM-TABLA-W               PIC X(60).
       77 NOM-CUPS-W                PIC X(60).

       01 MONTO-TAB-EDIT                PIC ZZZZZZZZZZZ.ZZ.
       01 MONTO-TABME-EDIT              PIC ZZZZZZZZZZZ.ZZ.

       77 MES-SAL-W                 PIC X(4).
       77 CONT                      PIC 9999.
       
       01  VAR-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 DIR-CONTAB-LLEGADA-W      PIC X(30).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 OPERADOR-LLEGADA-W        PIC X(4).
           02 COD-TABLA-LLEGADA-W       PIC X(15).

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV             PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 DESCRIP-TAB-ENV            PIC X(30).
          02 FILLER                     PIC X VALUE "|".
          02 COD-TAB-ENV                PIC X(15).
          02 FILLER                     PIC X VALUE "|".
          02 FORMA-LIQ-TAB-ENV          PIC 9.
          02 FILLER                     PIC X VALUE "|".
          02 MONTO-TAB-ENV              PIC X(12).
          02 FILLER                     PIC X VALUE "|".
          02 INCREM-TAB-ENV             PIC 9.
          02 FILLER                     PIC X VALUE "|".
          02 CTA-CONTB-ENV              PIC X(11).
          02 FILLER                     PIC X VALUE "|".
          02 COD-PAQ-TAB-ENV            PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 COD-RIPS-TAB-ENV           PIC X(11).
          02 FILLER                     PIC X VALUE "|".
          02 PAQUETE-TAB-ENV            PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CANT-DIA-ENV               PIC 9(3).
          02 FILLER                     PIC X VALUE "|".
          02 DIV-TAB-ENV                PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 EXTENSION-TAB-ENV          PIC X(120).
          
           

      
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
              MOVE "ARCHIVO-USUNET"         TO MSJ2-HTML
              MOVE "SAL713"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
          
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "ARCHIVO-SESION"         TO MSJ2-HTML
              MOVE "SAL713"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.                                                                                      

        I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TABLAS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE                    
           ELSE
              MOVE OTR-STAT                       TO MSJ1-HTML
              MOVE "SAL713-01-ARCHIVO-TABLAS"     TO MSJ2-HTML
              MOVE NOM-TABLA-W                    TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO LLEGADA-W


           UNSTRING LLEGADA-W DELIMITED BY "|"
              INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W, 
                   COD-TABLA-LLEGADA-W
           END-UNSTRING.

           MOVE LLAVESESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.

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
           INITIALIZE  NOM-TABLA-W   
           
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\". 

           MOVE "\\" TO  NOM-TABLA-W  

           INSPECT NOM-TABLA-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

           INSPECT NOM-TABLA-W REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-TABLA-W   REPLACING FIRST "                              "
                                               BY DIR-CONTAB-LLEGADA-W.

           INSPECT NOM-TABLA-W   REPLACING FIRST "                     "
                                             BY  "\CONTROL\SC-TABLA.DAT".
       

       ABRIR-NOMBRE-TAR.
           OPEN INPUT ARCHIVO-TABLAS.


       LEER-TABLAS.
           MOVE COD-TABLA-LLEGADA-W   TO LLAVE-TAB
           READ ARCHIVO-TABLAS WITH NO LOCK
                INVALID KEY 
                  MOVE "01" TO SW-INVALID-ENV               
                NOT INVALID KEY
                  MOVE "00" TO SW-INVALID-ENV
           END-READ.

           MOVE DESCRIP-TAB      TO DESCRIP-TAB-ENV           
           MOVE LLAVE-TAB        TO COD-TAB-ENV
           MOVE FORMA-LIQ-TAB    TO FORMA-LIQ-TAB-ENV
           MOVE MONTO-TAB        TO MONTO-TAB-EDIT
           MOVE MONTO-TAB-EDIT   TO MONTO-TAB-ENV
           MOVE INCREM-TAB       TO INCREM-TAB-ENV
           MOVE CTA-CONTAB-TAB   TO CTA-CONTB-ENV
           MOVE COD-RIPS-TAB     TO COD-RIPS-TAB-ENV 
           MOVE PAQUETE-TAB      TO PAQUETE-TAB-ENV
           MOVE COD-PAQ-INT-TAB  TO COD-PAQ-TAB-ENV
           MOVE DIV-TAB          TO DIV-TAB-ENV
           MOVE EXTENSION-TAB    TO EXTENSION-TAB-ENV.                      
           

       CERRAR-ARCHIVOS.        
           CLOSE ARCHIVO-TABLAS.


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