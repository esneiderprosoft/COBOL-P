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
      * COPY "..\..\FUENTES\FS-CUPS19.CBL".
       COPY "..\..\FUENTES\FS-COSTO.ECBL".

             SELECT ARCHIVO-CUPS LOCK MODE IS AUTOMATIC
                       ASSIGN ARCHCUPS-W
                       ORGANIZATION IS INDEXED;
                       ACCESS MODE DYNAMIC;
                       RECORD KEY  IS LLAVE-CUP
                       ALTERNATE RECORD KEY IS DESCRIP-CUP WITH DUPLICATES
                       ALTERNATE RECORD KEY IS LLAVE-ALT-CUP WITH DUPLICATES
                       FILE STATUS IS OTR-STAT.


             SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                    ASSIGN NOM-USUA-W
                    ORGANIZATION IS INDEXED
                    ACCESS MODE IS DYNAMIC;
                    RECORD KEY IS NOMBRE-USU
                    ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                    ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                    ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                    FILE STATUS IS OTR-STAT.


      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-CUPS19.CBL".
       COPY "..\..\FUENTES\FD-COSTO.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 TABLA-LLEGADA-W.
          02 DATO-LLEGADA-W PIC X OCCURS 200.
       
       01 LLEGADA-W         PIC X(100).

       01 TABLA-TEMP.
          02 CARAC-TEMP     PIC X OCCURS 50.


       77 NOM-COSTO-ECBL-LNK        PIC X(60).
       77 ARCHCUPS-W                PIC X(60).
       77 NOM-USUA-W                PIC X(60).

       77 MES-SAL-W                 PIC X(4).
       77 CONT                      PIC 9999.
       77 COSTO-EDIT-W              PIC X(4).

       
       01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 DIR-CONTAB-LLEGADA-W      PIC X(30).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 OPERADOR-LLEGADA-W        PIC X(4).
           02 LLAVE-CUPS-LLEGADA-W      PIC X(12).
             


       01 DATOS-ENVIO.
          02 SWINVALID-ENV              PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 SWINVALID1-ENV             PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 DESCRIP-ENV                PIC X(80).
          02 FILLER                     PIC X VALUE "|".
          02 TIPO-SERV-ENV              PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 COD-ABREV-ENV              PIC X(5).
          02 FILLER                     PIC X VALUE "|".
          02 DURACION-ENV               PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 NIVEL-ENV                  PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 COPAGO-ENV                 PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 NOPOS-ENV                  PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CIS-ENV                    PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 EDAD-MIN-ENV               PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 EDAD-MAX-ENV               PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 UNIDAD-MED-ENV             PIC  X.
          02 FILLER                     PIC X VALUE "|".
          02 SEXO-ENV                   PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 DIAGN-RIPS-ENV             PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 COSTO-ENV                  PIC X(4).
          02 FILLER                     PIC X VALUE "|".
          02 NOMBRE-COSTO-ENV           PIC X(20).
          02 FILLER                     PIC X VALUE "|".
          02 100-MED-ENV                PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 PORC-CLIN-ENV              PIC 9(3).
          02 FILLER                     PIC X VALUE "|".
          02 PORC-OTR-ENV               PIC 9V99.
          02 FILLER                     PIC X VALUE "|".
          02 CTA-OTR-ENV                PIC X(11).
          02 FILLER                     PIC X VALUE "|".
          02 NIT-OTR-ENV                PIC 9(10).
          02 FILLER                     PIC X VALUE "|".
          02 CTA-CONTAB-ENV.
             05 CTA-CONTAB1-ENV         PIC X(11).      
             05 FILLER                  PIC X VALUE "|".
             05 CTA-CONTAB2-ENV         PIC X(11).      
             05 FILLER                  PIC X VALUE "|".
             05 CTA-CONTAB3-ENV         PIC X(11).      
          02 FILLER                     PIC X VALUE "|".
          02 DIVISION-ENV               PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 DIVISION2-ENV              PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 OPER-ELAB-ENV              PIC X(4).
          02 FILLER                     PIC X VALUE "|".
          02 FECHA-ELAB-ENV             PIC X(8).
          02 FILLER                     PIC X VALUE "|".
          02 PUC-USU-ENV                PIC X.



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
              MOVE "SAL718C"                TO MSJ3-HTML
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
              MOVE "SAL718C"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-COSTO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE NOM-COSTO-ECBL-LNK         TO MSJ2-HTML
              MOVE "SAL718C"                  TO MSJ3-HTML
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
              MOVE "SAL718C-CUPS"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  		   


       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE NOM-USUA-W                 TO MSJ2-HTML
              MOVE "SAL718C"                  TO MSJ3-HTML
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
              INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W, LLAVE-CUPS-LLEGADA-W
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
           INITIALIZE  ARCHCUPS-W NOM-USUA-W
		   
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".
		   
           MOVE "\\" TO  ARCHCUPS-W NOM-USUA-W
		   
           INSPECT ARCHCUPS-W REPLACING FIRST "                    "
                                              BY IP-DATOS-USUNET

           INSPECT ARCHCUPS-W REPLACING FIRST " "
                                           BY "\"

           MOVE ARCHCUPS-W TO NOM-COSTO-ECBL-LNK  
           INSPECT NOM-COSTO-ECBL-LNK REPLACING FIRST "                              "
                                               BY DIR-CONTAB-LLEGADA-W.

           MOVE NOM-COSTO-ECBL-LNK TO  nom-usua-w
           INSPECT NOM-COSTO-ECBL-LNK REPLACING FIRST "                       "
                                                   BY "\CONTROL\SC-ARCHCOS.DAT".
        
           INSPECT NOM-USUA-W REPLACING FIRST "                       "
                                           BY "\CONTROL\SC-ARCHUSU.DAT".


           INSPECT ARCHCUPS-W REPLACING FIRST "                          "
                                           BY "PROG\DATOS\SC-ARCHCUPS.DAT".



       ABRIR-USUARIOS.

           OPEN INPUT ARCHIVO-CUPS
                      ARCHIVO-COSTO.


       LEER-USUARIO.
           OPEN INPUT ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT AT END MOVE 0 TO OPCION-1.
           CLOSE ARCHIVO-USUARIOS.


           IF (PUC-USU = 1 OR 2 OR 3 OR 4 OR 5)
              MOVE PUC-USU  TO PUC-USU-ENV
           END-IF.




       LEER-CUPS.
           MOVE LLAVE-CUPS-LLEGADA-W TO LLAVE-CUP
           READ ARCHIVO-CUPS WITH NO LOCK
                INVALID KEY 
                  MOVE "01" TO SWINVALID-ENV               
                NOT INVALID KEY
                  MOVE "00" TO SWINVALID-ENV
           END-READ.

           MOVE DESCRIP-CUP        TO DESCRIP-ENV
           MOVE TIPO-CUP           TO TIPO-SERV-ENV
           MOVE ABREV-CUP          TO COD-ABREV-ENV
           MOVE DURACION-CUP       TO DURACION-ENV
           MOVE NIVEL-CUP          TO NIVEL-ENV 
           MOVE COPAG-MOD-CUP      TO COPAGO-ENV
           MOVE NOPOS-CUP          TO NOPOS-ENV
           MOVE CIS-CUP            TO CIS-ENV
           MOVE EDAD-MIN-CUP       TO EDAD-MIN-ENV 
           MOVE EDAD-MAX-CUP       TO EDAD-MAX-ENV
           MOVE UNID-EDAD-CUP      TO UNIDAD-MED-ENV
           MOVE SEXO-CUP           TO SEXO-ENV
           MOVE DIAGN-CUP          TO DIAGN-RIPS-ENV
           MOVE COSTO-CUP          TO COSTO-EDIT-W
           MOVE 100-MED-CUP        TO 100-MED-ENV
           MOVE PORC-CL-CUP        TO PORC-CLIN-ENV
           MOVE PORC-OTR-CUP       TO PORC-OTR-ENV
           MOVE CTA-OTR-CUP        TO CTA-OTR-ENV
           MOVE NIT-OTR-CUP        TO NIT-OTR-ENV
           MOVE CTA-CONTAB-CUP(1)  TO CTA-CONTAB1-ENV
           MOVE CTA-CONTAB-CUP (2) TO CTA-CONTAB2-ENV
           MOVE CTA-CONTAB-CUP(3)  TO CTA-CONTAB2-ENV
           MOVE DIVISION-CUP       TO DIVISION-ENV
           MOVE DIV2-CUP           TO DIVISION2-ENV
           MOVE OPER-ELAB-CUP      TO OPER-ELAB-ENV
           MOVE FECHA-ELAB-CUP     TO FECHA-ELAB-ENV.


       LEER-COSTO.
           MOVE COSTO-EDIT-W TO COD-COSTO
           READ ARCHIVO-COSTO WITH NO LOCK
                INVALID KEY 
                  MOVE "01" TO SWINVALID1-ENV               
                NOT INVALID KEY
                  MOVE "00" TO SWINVALID1-ENV
           END-READ.
                
           MOVE COD-COSTO           TO COSTO-ENV
           MOVE NOMBRE-COSTO        TO NOMBRE-COSTO-ENV.



       CERRAR-ARCHIVOS.                
           CLOSE ARCHIVO-CUPS
                 ARCHIVO-COSTO.


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