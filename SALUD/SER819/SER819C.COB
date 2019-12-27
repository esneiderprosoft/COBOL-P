      *============================================================
      * CREACION DE ARCHIVO JSON SER819C  SALUD
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  
       COPY "..\..\FUENTES\FS-PROFE19.CBL".
     

      
      *     SELECT ARCHIVO-PROFESIONALES LOCK MODE IS AUTOMATIC                            
      *            ASSIGN PROFESIONALES-W
      *            ORGANIZATION IS INDEXED
      *            ACCESS MODE  IS DYNAMIC
      *            RECORD KEY   IS COD-PROF
      *            ALTERNATE RECORD KEY DESCRIP-PROF WITH DUPLICATES
      *            FILE STATUS IS OTR-STAT.                                                        


           SELECT ARCHIVO-MAESTROS LOCK MODE IS AUTOMATIC
                  ASSIGN MAESTROS-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-MAE
                  ALTERNATE RECORD KEY IS NOMBRE-MAE    WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CTA-INTER-MAE WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

            SELECT ARCHIVO-DIVISION LOCK MODE IS AUTOMATIC
                   ASSIGN DIVISION-W
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC;
                   RECORD KEY IS LLAVE-DIV
                   ALTERNATE RECORD KEY IS DESCRIP-DIV WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.

            SELECT ARCHIVO-SUCUR LOCK MODE IS AUTOMATIC
                   ASSIGN SUCURSAL-W
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC;
                   RECORD KEY IS COD-SUCUR
                   FILE STATUS IS OTR-STAT.


            SELECT ARCHIVO-ESPECIALIDAD LOCK MODE IS AUTOMATIC
                   ASSIGN ESPECIALIDAD-W
                   ORGANIZATION IS INDEXED
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS COD-ESP
                   ALTERNATE RECORD KEY IS NOMBRE-ESP WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL". 
       COPY "..\..\FUENTES\FD-PROFE19.CBL".
       COPY "..\..\FUENTES\FD-MAEST.CBL".
       COPY "..\..\FUENTES\FD-DIVIS.CBL".
       COPY "..\..\FUENTES\FD-SUCUR.CBL".
       COPY "..\..\FUENTES\FD-ESPEC.CBL".

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".


       77 PROFESIONALES-W           PIC X(60).
       77 MAESTROS-W                PIC X(60).
       77 DIVISION-W                PIC X(60).
       77 SUCURSAL-W                PIC X(60). 
       77 ESPECIALIDAD-W            PIC X(60).



       01 TABLA-LLEGADA-W.
          02 DATO-LLEGADA-W PIC X OCCURS 200.
       
       01 LLEGADA-W         PIC X(1000).

       01 TABLA-TEMP.
          02 CARAC-TEMP     PIC X OCCURS 50.

 
       77 ESP-W                     PIC X(3).
       77 NOMBRE-ESP-W              PIC X(70).



       01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 DIR-CONTAB-LLEGADA-W      PIC X(30).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 OPERADOR-LLEGADA-W        PIC X(4).
           02 COD-PROF-W                PIC X(10).
             


       01 DATOS-ENVIO.
          02 SWINVALID-ENV              PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 NOMBRE-PROF-ENV            PIC X(30).
          02 FILLER                     PIC X VALUE "|".
          02 IDENTIF-PROF-ENV           PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 DESCRIP-PROF-ENV           PIC X(30).
          02 FILLER                     PIC X VALUE "|".
          02 REGIS-PROF-ENV             PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 ATIENDE-PROF-ENV           PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CONTRATO-PROF-ENV          PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 PORCENT-PROF-ENV           PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESTADO-PROF-ENV            PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CTA-RET-PROF-ENV           PIC X(4).
          02 FILLER                     PIC X VALUE "|".
          02 DESCRIP-CTA-ENV            PIC X(30).
          02 FILLER                     PIC X VALUE "|".
          02 DIV-PROF-ENV               PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 DESCRP-DIV-PROF-ENV        PIC X(20).
          02 FILLER                     PIC X VALUE "|".
          02 SUCURSAL-PROF-ENV          PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 DESCRP-SUCUR-ENV           PIC X(20).
          02 FILLER                     PIC X VALUE "|".
          02 CL1-PROF-ENV               PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CL2-PROF-ENV               PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CL3-PROF-ENV               PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CL4-PROF-ENV               PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CL5-PROF-ENV               PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CL6-PROF-ENV               PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CL7-PROF-ENV               PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 ESPECI1-PROF-ENV           PIC X(3).
          02 FILLER                     PIC X VALUE "|".  
          02 DESCESP1-PROF-ENV          PIC X(70).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECI2-PROF-ENV           PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 DESCESP2-PROF-ENV          PIC X(70).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECI3-PROF-ENV           PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 DESCESP3-PROF-ENV          PIC X(70).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECI4-PROF-ENV           PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 DESCESP4-PROF-ENV          PIC X(70).
          02 FILLER                     PIC X VALUE "|".
          02 ESPECI5-PROF-ENV           PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 DESCESP5-PROF-ENV          PIC X(70).
          02 FILLER                     PIC X VALUE "|".
          02 OPER-AUT-PROF-ENV          PIC X(4).
          02 FILLER                     PIC X VALUE "|".
          02 OPER-AUT-CIR-PROF-ENV      PIC X(4).
          02 FILLER                     PIC X VALUE "|".
          02 OPER-AUT-OTRO-PROF-ENV     PIC X(4).
          02 FILLER                     PIC X VALUE "|".
          02 OPER-AUT-4-PROF-ENV        PIC X(4).
          02 FILLER                     PIC X VALUE "|".
          02 OPER-AUT-5-PROF-ENV        PIC X(4).
          02 FILLER                     PIC X VALUE "|".
          02 INT-MIN-PROF-ENV           PIC 99.
          02 FILLER                     PIC X VALUE "|".
          02 CANTD-CITAS-PROF-ENV       PIC 9(3).
          02 FILLER                     PIC X VALUE "|".
          02 FORMA-AGE-PROF-ENV         PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 SOBREAGE-PROF-ENV          PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 OPERAD-PROF-ENV            PIC X(4).
          02 FILLER                     PIC X VALUE "|".           
          02 FECHA-PROF-ENV.
             05 ANO-PROF-ENV            PIC 9(4).
             05 MES-PROF-ENV            PIC 99.
             05 DIA-PROF-ENV            PIC 99.
          02 FILLER                     PIC X VALUE "|".
          02 FECHA-INI-DES-PROF-ENV     PIC X(8).
          02 FILLER                     PIC X VALUE "|".
          02 HORA-INI-DES-PROF-ENV      PIC X(4).
          02 FILLER                     PIC X VALUE "|".
          02 FECHA-FIN-DES-PROF-ENV     PIC X(8).
          02 FILLER                     PIC X VALUE "|".
          02 HORA-FIN-DES-PROF-ENV      PIC X(4).
          02 FILLER                     PIC X VALUE "|".
          02 IMP-DVD-PROF-ENV           PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 IMP-BIRAD-PROF-ENV         PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 IMP-NORM-PROF-ENV          PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 ASOC-PROF-ENV              PIC X.
          

 
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
              MOVE "SER819C"                TO MSJ3-HTML
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
              MOVE "SER819C"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PROFESIONALES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT              TO MSJ1-HTML
              MOVE NOM-PROF-LNK       TO MSJ2-HTML
              MOVE "SER819C"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF. 


       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-MAESTROS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE MAESTROS-W       TO MSJ2-HTML
              MOVE "SER819C"        TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF. 


       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-DIVISION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE DIVISION-W           TO MSJ2-HTML
              MOVE "SER819C"            TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SUCUR.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE SUCURSAL-W           TO MSJ2-HTML
              MOVE "SER819C"            TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ESPECIALIDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE ESPECIALIDAD-W       TO MSJ2-HTML
              MOVE "SER819C"            TO MSJ3-HTML
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
              INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W, COD-PROF-W
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
           INITIALIZE NOM-PROF-LNK MAESTROS-W DIVISION-W SUCURSAL-W ESPECIALIDAD-W .

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           MOVE "\\" TO NOM-PROF-LNK MAESTROS-W DIVISION-W SUCURSAL-W ESPECIALIDAD-W .

           INSPECT NOM-PROF-LNK REPLACING FIRST "                    "
                                        BY IP-DATOS-USUNET

           INSPECT NOM-PROF-LNK REPLACING FIRST " "
                                        BY "\"

           MOVE NOM-PROF-LNK TO SUCURSAL-W ESPECIALIDAD-W 

           INSPECT SUCURSAL-W REPLACING FIRST "                       "
                                           BY "PROG\DATOS\SC-SUCUR.DAT".

           INSPECT ESPECIALIDAD-W REPLACING FIRST "                         " 
                                               BY "PROG\DATOS\SC-ARCHESP.DAT".



           INSPECT NOM-PROF-LNK REPLACING FIRST "                              "          
                                                BY DIR-CONTAB-LLEGADA-W.

           INSPECT NOM-PROF-LNK REPLACING FIRST "         "
                                               BY "\CONTROL\".

           MOVE NOM-PROF-LNK TO MAESTROS-W DIVISION-W

           INSPECT NOM-PROF-LNK REPLACING FIRST "               "
                                             BY "SC-ARCHPROF.DAT".

           INSPECT MAESTROS-W REPLACING FIRST "              "
                                           BY "SC-ARCHMAE.DAT".

           INSPECT DIVISION-W REPLACING FIRST "            "
                                           BY "SC-DIVIS.DAT".



       ABRIR-USUARIOS.

           OPEN INPUT  ARCHIVO-PROFESIONALES
                       ARCHIVO-MAESTROS
                       ARCHIVO-DIVISION
                       ARCHIVO-SUCUR
                       ARCHIVO-ESPECIALIDAD.


       LEER-SISVAN.
           MOVE COD-PROF-W TO COD-PROF
           READ ARCHIVO-PROFESIONALES WITH NO LOCK
                INVALID KEY 
                  MOVE "01" TO SWINVALID-ENV               
                NOT INVALID KEY
                  MOVE "00" TO SWINVALID-ENV
           END-READ.
                
           MOVE DESCRIP-PROF          TO NOMBRE-PROF-ENV
           MOVE COD-PROF              TO IDENTIF-PROF-ENV              
           MOVE DETALLE-PROF          TO DESCRIP-PROF-ENV
           MOVE REG-MED-PROF          TO REGIS-PROF-ENV 
           MOVE ATIENDE-PROF          TO ATIENDE-PROF-ENV
           MOVE CONTRATO-P            TO CONTRATO-PROF-ENV
           MOVE PORCENT-P             TO PORCENT-PROF-ENV
           MOVE ESTADO-PROF           TO ESTADO-PROF-ENV. 
           MOVE CTA-RET-PROF          TO CTA-RET-PROF-ENV


           IF CTA-RET-PROF-ENV = SPACES
             MOVE SPACES TO DESCRIP-CTA-ENV
           ELSE
             MOVE CTA-RET-PROF-ENV TO CTA-MAE
             MOVE 4                TO TIPO-MAE
             READ ARCHIVO-MAESTROS WITH NO LOCK
                  INVALID KEY
                    MOVE SPACES TO DESCRIP-CTA-ENV
             END-READ
             MOVE NOMBRE-MAE   TO DESCRIP-CTA-ENV
           END-IF.

           MOVE DIV-PROF        TO DIV-PROF-ENV
           IF DIV-PROF-ENV = SPACES  
              MOVE SPACES TO DIV-PROF-ENV
           ELSE  
              MOVE DIV-PROF-ENV TO COD-DIV
              READ ARCHIVO-DIVISION WITH NO LOCK
                  INVALID KEY MOVE SPACES TO DESCRP-DIV-PROF-ENV
              END-READ
              MOVE DESCRIP-DIV TO DESCRP-DIV-PROF-ENV
           END-IF.


           MOVE SUCURSAL-PROF TO SUCURSAL-PROF-ENV
           IF SUCURSAL-PROF-ENV = SPACES  
             MOVE SPACES TO DESCRP-SUCUR-ENV
           ELSE  
             MOVE SUCURSAL-PROF-ENV TO COD-SUCUR
             READ ARCHIVO-SUCUR WITH NO LOCK
                 INVALID KEY MOVE SPACES TO DESCRP-SUCUR-ENV
             END-READ
             MOVE DESCRIP-SUCUR TO DESCRP-SUCUR-ENV
           END-IF.


           MOVE CL-PROF(1) TO CL1-PROF-ENV
           MOVE CL-PROF(2) TO CL2-PROF-ENV
           MOVE CL-PROF(3) TO CL3-PROF-ENV
           MOVE CL-PROF(4) TO CL4-PROF-ENV
           MOVE CL-PROF(5) TO CL5-PROF-ENV
           MOVE CL-PROF(6) TO CL6-PROF-ENV
           MOVE CL-PROF(7) TO CL7-PROF-ENV.


           MOVE ESP-PROF(1) TO ESP-W
           PERFORM MOSTRAR-ESP
           MOVE ESP-W TO ESPECI1-PROF-ENV
           MOVE NOMBRE-ESP-W TO DESCESP1-PROF-ENV
          
           MOVE ESP-PROF(2) TO ESP-W
           PERFORM MOSTRAR-ESP
           MOVE ESP-W TO ESPECI2-PROF-ENV
           MOVE NOMBRE-ESP-W TO DESCESP2-PROF-ENV

           MOVE ESP-PROF(3) TO ESP-W
           PERFORM MOSTRAR-ESP
           MOVE ESP-W TO ESPECI3-PROF-ENV
           MOVE NOMBRE-ESP-W TO DESCESP3-PROF-ENV

           MOVE ESP-PROF(4) TO ESP-W
           PERFORM MOSTRAR-ESP
           MOVE ESP-W TO ESPECI4-PROF-ENV
           MOVE NOMBRE-ESP-W TO DESCESP4-PROF-ENV

           MOVE ESP-PROF(5) TO ESP-W
           PERFORM MOSTRAR-ESP
           MOVE ESP-W TO ESPECI5-PROF-ENV
           MOVE NOMBRE-ESP-W TO DESCESP5-PROF-ENV.


           IF OPER-AUT-PROF = SPACES 
              MOVE "XXXX" TO OPER-AUT-PROF-ENV
           ELSE
              MOVE OPER-AUT-PROF TO OPER-AUT-PROF-ENV
           END-IF.

           IF  OPER-AUT-CIRU-PROF = SPACES  
              MOVE "XXXX" TO OPER-AUT-CIR-PROF-ENV
           ELSE   
              MOVE OPER-AUT-CIRU-PROF TO OPER-AUT-CIR-PROF-ENV
           END-IF.
      
           IF OPER-AUT-OTRO-PROF = SPACES  
              MOVE "XXXX" TO OPER-AUT-OTRO-PROF-ENV
           ELSE
              MOVE OPER-AUT-OTRO-PROF TO OPER-AUT-OTRO-PROF-ENV
           END-IF.
      
           IF OPER-AUT-4-PROF = SPACES  
              MOVE "XXXX" TO OPER-AUT-4-PROF-ENV 
           ELSE
              MOVE OPER-AUT-4-PROF TO OPER-AUT-4-PROF-ENV 
           END-IF.

           IF OPER-AUT-5-PROF = SPACES  
              MOVE "XXXX" TO OPER-AUT-5-PROF-ENV 
           ELSE
              MOVE OPER-AUT-5-PROF TO OPER-AUT-5-PROF-ENV 
           END-IF.

           INSPECT OPER-AUT-PROF-ENV      REPLACING ALL "*" BY "X".
           INSPECT OPER-AUT-CIR-PROF-ENV  REPLACING ALL "*" BY "X".
           INSPECT OPER-AUT-OTRO-PROF-ENV REPLACING ALL "*" BY "X".
           INSPECT OPER-AUT-4-PROF-ENV    REPLACING ALL "*" BY "X".
           INSPECT OPER-AUT-5-PROF-ENV    REPLACING ALL "*" BY "X".


           MOVE INT-MIN-P TO INT-MIN-PROF-ENV
           MOVE CANTD-CITAS-PROF TO CANTD-CITAS-PROF-ENV


           MOVE FORMA-AGE-PROF TO FORMA-AGE-PROF-ENV
           IF FORMA-AGE-PROF-ENV = "S"
              CONTINUE
           ELSE
              MOVE "N" TO FORMA-AGE-PROF-ENV
           END-IF.

           MOVE SOBREAGE-PROF      TO SOBREAGE-PROF-ENV
           MOVE OPER-PROF          TO OPERAD-PROF-ENV
           MOVE FECHA-PROF         TO FECHA-PROF-ENV.
           MOVE FECHA-INI-DES-PROF TO FECHA-INI-DES-PROF-ENV
           MOVE HORA-INI-DES-PROF  TO HORA-INI-DES-PROF-ENV
           MOVE FECHA-FIN-DES-PROF TO FECHA-FIN-DES-PROF-ENV
           MOVE HORA-FIN-DES-PROF  TO HORA-FIN-DES-PROF-ENV
           MOVE IMP-DVD-PROF       TO IMP-DVD-PROF-ENV
           MOVE IMP-BIRAD-PROF     TO IMP-BIRAD-PROF-ENV
           MOVE IMP-NORM-PROF      TO IMP-NORM-PROF-ENV
           MOVE ASOC-PROF          TO ASOC-PROF-ENV.
  



       MOSTRAR-ESP.
              MOVE ESP-W TO COD-ESP
              READ ARCHIVO-ESPECIALIDAD WITH NO LOCK
                   INVALID KEY MOVE SPACES TO NOMBRE-ESP
              END-READ.


       CERRAR-ARCHIVOS.                
           CLOSE ARCHIVO-PROFESIONALES
                 ARCHIVO-MAESTROS
                 ARCHIVO-DIVISION
                 ARCHIVO-SUCUR
                 ARCHIVO-ESPECIALIDAD.



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