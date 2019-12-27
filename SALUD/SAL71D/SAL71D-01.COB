      *============================================================
      * CREACION DE ARCHIVO JSON SAL71D-01  SALUD
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  


           SELECT ARCHIVO-COLEGIOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-COLEG-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY LLAVE-COLEG
                  ALTERNATE RECORD KEY IS DESCRIP-COLEG WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-CIUDAD  LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-CIUD-W 
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-CIU
                  ALTERNATE RECORD KEY IS NOMBRE-CIU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.
                                                      
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-COLEG.CBL".
       COPY "..\..\FUENTES\FD-CIUDA.CBL".

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

          
       01 LLEGADA-W         PIC X(100).
       77 NOM-CIUD-W                PIC X(60).
       77 NOM-COLEG-W               PIC X(60). 
         
       01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 DIR-CONTAB-LLEGADA-W      PIC X(30).
           02 MES-CONTAB-LLEGADA-W      PIC XX.            
           02 OPERADOR-LLEGADA-W        PIC X(4).
           02 COD-LLEGADA-W             PIC X(12).


       01 DATOS-ENVIO.
          02 SWINVALID-ENV              PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 SWINVALID1-ENV             PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 LLAVE-COLEG-ENV            PIC X(12).
          02 FILLER                     PIC X VALUE "|".
          02 NOMBRE-COLEG-ENV           PIC X(50).
          02 FILLER                     PIC X VALUE "|".
          02 NUCLEO-COLEG-ENV           PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 ZONA-COLEG-ENV             PIC X(1).
          02 FILLER                     PIC X VALUE "|".
          02 DIRECC-COLEG-ENV           PIC X(30).
          02 FILLER                     PIC X VALUE "|".
          02 CIUDAD-ENV.
             05 DPTO-COLEG-ENV          PIC 99.
             05 CIU-COLEG-ENV           PIC 9(3).
          02 FILLER                     PIC X VALUE "|".
          02 NOMBRE-CIU-ENV.
             05 NOMBRE1-CIU-ENV         PIC X(16).
             05 NOMBRE2-CIU-ENV         PIC X(4).
          02 FILLER                     PIC X VALUE "|".
          02 TELFONO-COLEG-ENV          PIC X(10).
                                                                     

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
              MOVE "SER101"                 TO MSJ3-HTML
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
              MOVE "SAL71D-01"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-COLEGIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"         
              CONTINUE            
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE "SAL71D-01"                TO MSJ2-HTML
              MOVE NOM-COLEG-W                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CIUDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"         
              CONTINUE            
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE "SAL71D-01-CD"             TO MSJ2-HTML
              MOVE NOM-CIUD-W                 TO MSJ3-HTML
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
              INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W, COD-LLEGADA-W
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
           INITIALIZE  NOM-COLEG-W 

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           MOVE "\\" TO NOM-COLEG-W  

           INSPECT NOM-COLEG-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

           INSPECT NOM-COLEG-W REPLACING FIRST " "
                                            BY "\"
           
           INSPECT NOM-COLEG-W REPLACING FIRST "                       "
                                            BY "PROG\DATOS\SC-COLEG.DAT".   



           INITIALIZE  NOM-CIUD-W 

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           MOVE "\\" TO NOM-CIUD-W  

           INSPECT NOM-CIUD-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

           INSPECT NOM-CIUD-W REPLACING FIRST " "
                                            BY "\"          

           INSPECT NOM-CIUD-W REPLACING FIRST "                          "
                                           BY "PROG\DATOS\SC-ARCHCIUD.DAT". 



       ABRIR-ARCHIVOS-COL.
           OPEN INPUT ARCHIVO-COLEGIOS.

       LEER-COLEG.
           MOVE COD-LLEGADA-W TO LLAVE-COLEG
           MOVE "00"          TO SWINVALID-ENV

           READ ARCHIVO-COLEGIOS WITH NO LOCK
                INVALID KEY 
                  MOVE "01" TO SWINVALID-ENV               
           END-READ.
 
           MOVE LLAVE-COLEG      TO LLAVE-COLEG-ENV
           MOVE DESCRIP-COLEG    TO NOMBRE-COLEG-ENV
           MOVE NUCLEO-COLEG     TO NUCLEO-COLEG-ENV
           MOVE ZONA-COLEG       TO ZONA-COLEG-ENV
           MOVE DIRECCION-COLEG  TO DIRECC-COLEG-ENV
           MOVE CIUDAD-COLEG     TO CIUDAD-ENV
           MOVE TELEFONO-COLEG   TO TELFONO-COLEG-ENV. 


       CERRAR-COLG.               
           CLOSE  ARCHIVO-COLEGIOS.



       ABRIR-ARCHIVOS-CIUD.
           OPEN INPUT ARCHIVO-CIUDAD.

       LEER-COLEG.
           MOVE CIUDAD-COLEG  TO COD-CIU
           MOVE "00"          TO SWINVALID-ENV

           READ ARCHIVO-CIUDAD WITH NO LOCK
                INVALID KEY 
                  MOVE "01" TO SWINVALID-ENV               
           END-READ.
 
           MOVE COD-CIU          TO CIUDAD-ENV
           MOVE NOMBRE-CIU       TO NOMBRE-CIU-ENV. 


       CERRAR-CIUD.               
           CLOSE  ARCHIVO-CIUDAD.


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