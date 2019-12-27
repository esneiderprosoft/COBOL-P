      *============================================================
      * CREACION DE ARCHIVO JSON SAL71A-C  SALUD
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


           SELECT ARCHIVO-CUPS LOCK MODE IS AUTOMATIC
                  ASSIGN ARCHCUPS-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS LLAVE-CUP
                  ALTERNATE RECORD KEY IS DESCRIP-CUP WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LLAVE-ALT-CUP WITH DUPLICATES
                  FILE STATUS IS OTR-STAT. 
                                                          
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-CUPS19.CBL".
       COPY "..\..\FUENTES\FD-ESCUP.CBL".

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 TABLA-LLEGADA-W.
          02 DATO-LLEGADA-W PIC X OCCURS 200.
       
       01 LLEGADA-W         PIC X(100).

       01 TABLA-TEMP.
          02 CARAC-TEMP     PIC X OCCURS 50.


       77 NOM-ESPCUPS-W                 PIC X(60).
       77 ARCHCUPS-W                    PIC X(60).  
       77 MES-SAL-W                 PIC X(4).
       77 CONT                      PIC 9999.
       
       01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 DIR-CONTAB-LLEGADA-W      PIC X(30).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 OPERADOR-LLEGADA-W        PIC X(4).
           02 COD-CUPS-LLEGADA-W        PIC X(15).
             


       01 DATOS-ENVIO.
          02 SWINVALID-ENV              PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 SWINVALID1-ENV             PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 COD-ESP-CUPS-ENV           PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 DESCRIP-CUPS-ENV           PIC X(50).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA1-CUP-ENV       PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA2-CUP-ENV       PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA3-CUP-ENV       PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA4-CUP-ENV       PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA5-CUP-ENV       PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA6-CUP-ENV       PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA7-CUP-ENV       PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA8-CUP-ENV       PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA9-CUP-ENV       PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA10-CUP-ENV      PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA11-CUP-ENV      PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA12-CUP-ENV      PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA13-CUP-ENV      PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA14-CUP-ENV      PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA15-CUP-ENV      PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA16-CUP-ENV      PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA17-CUP-ENV      PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA18-CUP-ENV      PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA19-CUP-ENV      PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA20-CUP-ENV      PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA21-CUP-ENV      PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA22-CUP-ENV      PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA23-CUP-ENV      PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA24-CUP-ENV      PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA25-CUP-ENV      PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA26-CUP-ENV      PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA27-CUP-ENV      PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA28-CUP-ENV      PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECIALIDA29-CUP-ENV      PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 OPER-ESP-CUP-ENV           PIC X(4).
          02 FILLER                     PIC X VALUE "|".
          02 FECHA-ESP-CUP-ENV          PIC X(8).
          02 FILLER                     PIC X VALUE "|".
          02 SEXO-ESP-CUP-ENV           PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 ATIENDE-ESP-CUP-ENV        PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 PYP-CUP-ENV                PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 TIPO-CUP-ENV               PIC 9.
          02 FILLER                     PIC X VALUE "|".
          02 FINALID-CUP-ENV            PIC 99.


           
      
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
              MOVE "SER134-01"              TO MSJ3-HTML
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
              MOVE "SAL71A-C"               TO MSJ3-HTML
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
              MOVE "SAL71A-C"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CUPS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE ARCHCUPS-W                 TO MSJ2-HTML
              MOVE "SAL71A-C-CUPS"            TO MSJ3-HTML
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
              INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W, COD-CUPS-LLEGADA-W
           END-UNSTRING.

           MOVE LLAVESESION-LLEGA-W   TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-JSON-W.
           ACCEPT HORA-JSON-W FROM TIME.
           MOVE OPERADOR-LLEGADA-W TO OPER-JSON-W.

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
           INITIALIZE  ARCHCUPS-W NOM-ESPCUPS-W.
		   
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".
		   
           MOVE "\\" TO  ARCHCUPS-W NOM-ESPCUPS-W
		   
           INSPECT ARCHCUPS-W REPLACING FIRST "                    "
                                              BY IP-DATOS-USUNET

           INSPECT ARCHCUPS-W REPLACING FIRST " "
                                              BY "\"

           MOVE ARCHCUPS-W TO NOM-ESPCUPS-W
           INSPECT NOM-ESPCUPS-W REPLACING FIRST "                         "
                                              BY "PROG\DATOS\SC-ESP-CUP.DAT".

           INSPECT ARCHCUPS-W REPLACING FIRST "                          "
                                           BY "PROG\DATOS\SC-ARCHCUPS.DAT".



       ABRIR-USUARIOS.
           OPEN INPUT ARCHIVO-CUPS
                      ARCHIVO-ESPEC-CUP.


       LEER-SISVAN.
           MOVE COD-CUPS-LLEGADA-W TO COD-ESP-CUP
           READ ARCHIVO-ESPEC-CUP WITH NO LOCK
                INVALID KEY 
                  MOVE "01" TO SWINVALID-ENV               
                NOT INVALID KEY
                  MOVE "00" TO SWINVALID-ENV
           END-READ.
                
           MOVE COD-ESP-CUP           TO COD-ESP-CUPS-ENV.


       LEER-PACIENTE.
           MOVE COD-ESP-CUP TO LLAVE-CUP
           READ ARCHIVO-CUPS WITH NO LOCK
                INVALID KEY 
                  MOVE "01" TO SWINVALID1-ENV               
                NOT INVALID KEY
                  MOVE "00" TO SWINVALID1-ENV
           END-READ.

           MOVE DESCRIP-CUP     TO DESCRIP-CUPS-ENV

           MOVE ESP-CUP(1)      TO ESPECIALIDA1-CUP-ENV
           MOVE ESP-CUP(2)      TO ESPECIALIDA2-CUP-ENV
           MOVE ESP-CUP(3)      TO ESPECIALIDA3-CUP-ENV
           MOVE ESP-CUP(4)      TO ESPECIALIDA4-CUP-ENV
           MOVE ESP-CUP(5)      TO ESPECIALIDA5-CUP-ENV
           MOVE ESP-CUP(6)      TO ESPECIALIDA6-CUP-ENV
           MOVE ESP-CUP(7)      TO ESPECIALIDA7-CUP-ENV
           MOVE ESP-CUP(8)      TO ESPECIALIDA8-CUP-ENV
           MOVE ESP-CUP(9)      TO ESPECIALIDA9-CUP-ENV
           MOVE ESP-CUP(10)     TO ESPECIALIDA10-CUP-ENV
           MOVE ESP-CUP(11)     TO ESPECIALIDA11-CUP-ENV
           MOVE ESP-CUP(12)     TO ESPECIALIDA12-CUP-ENV
           MOVE ESP-CUP(13)     TO ESPECIALIDA13-CUP-ENV
           MOVE ESP-CUP(14)     TO ESPECIALIDA14-CUP-ENV
           MOVE ESP-CUP(15)     TO ESPECIALIDA15-CUP-ENV
           MOVE ESP-CUP(16)     TO ESPECIALIDA16-CUP-ENV
           MOVE ESP-CUP(17)     TO ESPECIALIDA17-CUP-ENV
           MOVE ESP-CUP(18)     TO ESPECIALIDA18-CUP-ENV
           MOVE ESP-CUP(19)     TO ESPECIALIDA19-CUP-ENV
           MOVE ESP-CUP(20)     TO ESPECIALIDA20-CUP-ENV  
           MOVE ESP-CUP(21)     TO ESPECIALIDA21-CUP-ENV  
           MOVE ESP-CUP(22)     TO ESPECIALIDA22-CUP-ENV  
           MOVE ESP-CUP(23)     TO ESPECIALIDA23-CUP-ENV  
           MOVE ESP-CUP(24)     TO ESPECIALIDA24-CUP-ENV  
           MOVE ESP-CUP(25)     TO ESPECIALIDA25-CUP-ENV  
           MOVE ESP-CUP(26)     TO ESPECIALIDA26-CUP-ENV  
           MOVE ESP-CUP(27)     TO ESPECIALIDA27-CUP-ENV  
           MOVE ESP-CUP(28)     TO ESPECIALIDA28-CUP-ENV  
           MOVE ESP-CUP(29)     TO ESPECIALIDA29-CUP-ENV  
           MOVE OPER-ESP-CUP    TO OPER-ESP-CUP-ENV
           MOVE FECHA-ESP-CUP   TO FECHA-ESP-CUP-ENV
           MOVE SEXO-ESP-CUP    TO SEXO-ESP-CUP-ENV 
           MOVE ATIENDE-ESP-CUP TO ATIENDE-ESP-CUP-ENV
           MOVE PYP-CUP         TO PYP-CUP-ENV
           MOVE TIPO-PROC-CUP   TO TIPO-CUP-ENV
           MOVE FINALID-CUP     TO FINALID-CUP-ENV.  

       CERRAR-ARCHIVOS.                
           CLOSE ARCHIVO-CUPS
                 ARCHIVO-ESPEC-CUP.   


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