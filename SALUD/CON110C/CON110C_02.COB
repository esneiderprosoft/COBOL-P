      *=================================================================
      * CONSULTA DE ARCHIVO TERCEROS- CON110C_01 -MOSTRAR DATOS 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

            SELECT ARCHIVO-TERCEROS  LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-TER-W
                   ORGANIZATION IS INDEXED;
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS COD-TERCERO
                   ALTERNATE RECORD KEY DESCRIP-TER WITH DUPLICATES
                   ALTERNATE RECORD KEY ACT-TER     WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.
            SELECT ARCHIVO-ACTIVIDAD LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-ACT-W
      *"/PROG/DATOS/SCARCHACT.DAT"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY  IS COD-ACT
                   FILE STATUS IS OTR-STAT. 

            SELECT ARCHIVO-CIUDAD  LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-CIUD-W
      *"P:\PROG\DATOS\SC-ARCHCIUD.DAT";
                   ORGANIZATION IS INDEXED;
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS COD-CIU
                   ALTERNATE RECORD KEY IS NOMBRE-CIU WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.

            SELECT ARCHIVO-ENTIDADES LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-ENTI-W
      *"\PROG\DATOS\SC-ARCHENT.DAT";
                   ORGANIZATION IS INDEXED;
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS COD-ENT
                   ALTERNATE RECORD KEY IS NOMBRE-ENT WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.

            SELECT ARCHIVO-ZONAS LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-ZON-W
      *"../CONTROL/SC-ARCHZONA.DAT"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC;
                   RECORD KEY IS LLAVE-ZONA
                   ALTERNATE RECORD KEY IS DESCRIP-ZONA WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.

            SELECT ARCHIVO-GRADO-NEG LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-GRADNEG-W
      *"\PROG\DATOS\SC-GRNEG.DAT"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC;
                   RECORD KEY IS LLAVE-GRAD
                   ALTERNATE RECORD KEY IS DESCRIP-GRAD WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.

            SELECT ARCHIVO-VENDEDOR LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-VEND-W
      *"../CONTROL/SC-ARCHVEN.DAT"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS COD-VEN
                   FILE STATUS IS OTR-STAT.
            

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-TERCE.CBL".
       COPY "..\..\FUENTES\FD-ACTIV.CBL".
       COPY "..\..\FUENTES\FD-CIUDA.CBL".
       COPY "..\..\FUENTES\FD-ENTID.CBL".
       COPY "..\..\FUENTES\FD-ZONAS.CBL".
       COPY "..\..\FUENTES\FD-GRADO.CBL".
       COPY "..\..\FUENTES\FD-VENDE.CBL".

     
       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LLEGADA-W                    PIC X(100).
       77 NOM-TER-W                    PIC X(70).
       77 NOM-ACT-W                    PIC X(70).
       77 NOM-CIUD-W                   PIC X(70).
       77 NOM-ENTI-W                   PIC X(70).
       77 NOM-ZON-W                    PIC X(70).
       77 NOM-GRADNEG-W                PIC X(70).
       77 NOM-VEND-W                   PIC X(70).

       01 FACTOR-TER-EDIT              PIC ZZZ.ZZ.
       01 PORC-ICA-TER2-EDIT           PIC ZZ.ZZZ.
       01 PORC-RET-TER-EDIT            PIC ZZ.Z.     

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(30).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
	  02 OPERADOR-LLEGADA-W        PIC X(4).
          02 COD-TERCERO-LLEGADA-W.
             03 COD-TER                PIC 9(10).
          

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV            PIC X(2).
          02 FILLER                    PIC X VALUE "|".
          02 COD-TERCERO-ENV.
             03 COD-TER                PIC 9(10). 
          02 FILLER                    PIC X VALUE "|".
          02 DV-TER-ENV                PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 FECHA-CUMP-VEH-ENV.
             03 ANO-NAC-TER2           PIC 9(4).
             03 MES-CUMP-VEH           PIC 99.
             03 DIA-CUMP-VEH           PIC 99. 
          02 FILLER                    PIC X VALUE "|".        
          02 APEL1-TER2-ENV            PIC X(20).
          02 FILLER                    PIC X VALUE "|".
          02 APEL2-TER2-ENV            PIC X(20).
          02 FILLER                    PIC X VALUE "|".
          02 NOMB1-TER2-ENV            PIC X(30).                   
          02 FILLER                    PIC X VALUE "|".
          02 DIRECC-TER-ENV.
             03 DIRECC1-TER            PIC X(25).
             03 DIRECC2-TER            PIC X(20).
          02 FILLER                    PIC X VALUE "|".
          02 COD-CIU-TER-ENV.
             03 DPTO-CIU-TER           PIC 99.
             03 CIUD-CIU-TER           PIC 9(3).
          02 FILLER                    PIC X VALUE "|".
          02 NOMBRE-CIU-TER-ENV.
             05 NOMBRE1-CIU            PIC X(16).
             05 NOMBRE2-CIU            PIC X(4).
          02 FILLER                    PIC X VALUE "|".
          02 INDICATIVO-TER-ENV        PIC X(3).
          02 FILLER                    PIC X VALUE "|".
          02 TELEFONO-TER-ENV          PIC 9(12).
          02 FILLER                    PIC X VALUE "|".
          02 NIT-TER-ENV               PIC 9(10).
          02 FILLER                    PIC X VALUE "|".
          02 TIPO-ID-TER-ENV           PIC X(2).
          02 FILLER                    PIC X VALUE "|".
          02 ENTIDAD-TER-ENV           PIC X(6).
          02 FILLER                    PIC X VALUE "|".
          02 NOMBRE-ENT-TER-ENV.
             03 NOMBRE1-ENT            PIC X(30).
             03 NOMBRE2-ENT            PIC X(40).
          02 FILLER                    PIC X VALUE "|".
          02 ACT-TER-ENV-ENV           PIC 99.
          02 FILLER                    PIC X VALUE "|".
          02 NOMBRE-ACT-TER-ENV        PIC X(25).
          02 FILLER                    PIC X VALUE "|".
          02 CONVENIO-TER-ENV.
             03 CONVENIO1-TER          PIC X.
             03 CONVENIO2-TER          PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 RUT-TER-ENV               PIC X.    
          02 FILLER                    PIC X VALUE "|".
          02 NOM-COMER-TER-ENV         PIC X(50).
          02 FILLER                    PIC X VALUE "|".
          02 REFER1-TER-ENV            PIC X(40).
          02 FILLER                    PIC X VALUE "|".
          02 REFER2-TER-ENV            PIC X(40).
          02 FILLER                    PIC X VALUE "|".
          02 REFER3-TER-ENV            PIC X(20).
          02 FILLER                    PIC X VALUE "|".
          02 CARGO-TER-ENV             PIC X(10).
          02 FILLER                    PIC X VALUE "|".
          02 E-MAIL-TER-ENV            PIC X(60).
          02 FILLER                    PIC X VALUE "|".
          02 ASESOR-TER-ENV            PIC X(2).
          02 FILLER                    PIC X VALUE "|".
          02 TIPO-CUPO-TER2-ENV        PIC 9.
          02 FILLER                    PIC X VALUE "|".
          02 FECHA-CRE-TER-ENV.
             07 ANO-CRE-TER            PIC 9(4).
             07 MES-CRE-TER            PIC 99.
             07 DIA-CRE-TER            PIC 99.
          02 FILLER                    PIC X VALUE "|".
          02 ADMI-CRE-TER-ENV          PIC X(4).
          02 FILLER                    PIC X VALUE "|".
          02 FECHA-MOD-TER-ENV.                   
             03 ANO-MOD-TER            PIC 9(4).
             03 MES-MOD-TER            PIC 99.
             03 DIA-MOD-TER            PIC 99.
          02 FILLER                    PIC X VALUE "|".
          02 ADMI-MOD-TER-ENV          PIC X(4).                    
          02 FILLER                    PIC X VALUE "|".
          02 FACTOR-TER-ENV            PIC X(6).
          02 FILLER                    PIC X VALUE "|".
          02 CUPO-TER-ENV              PIC 9(11).
          02 FILLER                    PIC X VALUE "|".
          02 VENDEDOR-TER-ENV          PIC X(5).
          02 FILLER                    PIC X VALUE "|".
          02 PAGO-TER-ENV              PIC 99.
          02 FILLER                    PIC X VALUE "|".
          02 PLAZO-TER-ENV             PIC 9(3).
          02 FILLER                    PIC X VALUE "|".
          02 ZONA-TER-ENV.
             03 ZONA1-TER              PIC X.
             03 ZONA2-TER              PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 DESCRIP-ZONA-ENV          PIC X(20).
          02 FILLER                    PIC X VALUE "|".
          02 RUTA-TER-ENV              PIC XX.
          02 FILLER                    PIC X VALUE "|".
          02 DESCRIP-RUTA-ENV          PIC X(20).
          02 FILLER                    PIC X VALUE "|".
          02 ORDEN-TER-ENV             PIC 9(3).
          02 FILLER                    PIC X VALUE "|".
          02 ACTIV-ICA-ENV             PIC 9(3).
          02 FILLER                    PIC X VALUE "|".
          02 PORC-ICA-TER2-ENV         PIC X(6).
          02 FILLER                    PIC X VALUE "|".
          02 PORC-RET-TER-ENV          PIC X(4).
          02 FILLER                    PIC X VALUE "|".
          02 GRADO-TER-ENV             PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 DESCRIP-GRAD-TER-ENV      PIC X(15).
          02 FILLER                    PIC X VALUE "|".
          02 REG-IVA-TER-ENV           PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 CALIFIC-TER-ENV           PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 GRAN-CONTRIB-TER-ENV      PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 RETENEDOR-TER-ENV         PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 VLR-BASE-RET-TER-ENV      PIC 9(12).
          02 FILLER                    PIC X VALUE "|".
          02 RET-IVA-COMPRA-ENV        PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 RET-IVA-TER-ENV           PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 EXENT-RET-TER-ENV         PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 SEGURO-TER-ENV            PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 DATACRE-TER-ENV           PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 ACUE-PAGO-TER2-ENV        PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 CAPITADO-TER2-ENV         PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 NIT1-CLI-TER-ENV          PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 RET-ICAV-TER-ENV          PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 BLOQ-TER-ENV              PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 EXIVA-TER-ENV             PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 MARCA-VEH-ENV             PIC X(10).
          02 FILLER                    PIC X VALUE "|".
          02 EMPRESA-VEH-ENV           PIC X(10).
          02 FILLER                    PIC X VALUE "|".
          02 NRO-VEH-ENV               PIC X(4).
                             

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
              MOVE "CON110C_01"             TO MSJ3-HTML
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
              MOVE "CON110C_01"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TERCEROS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-TER-W                TO MSJ2-HTML
              MOVE "CON110C_01"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ENTIDADES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-ENTI-W               TO MSJ2-HTML
              MOVE "CON110C_02"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CIUDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-CIUD-W                TO MSJ2-HTML
              MOVE "CON110C_02"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ACTIVIDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-ACT-W               TO MSJ2-HTML
              MOVE "CON110C_02"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ZONAS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-ZON-W               TO MSJ2-HTML
              MOVE "CON110C_02"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-GRADO-NEG.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-GRADNEG-W            TO MSJ2-HTML
              MOVE "CON110C_02"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-VENDEDOR.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-VEND-W               TO MSJ2-HTML
              MOVE "CON110C_02"             TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA    TO LLEGADA-W.

           UNSTRING LLEGADA-W DELIMITED BY "|"
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W,
                   COD-TERCERO-LLEGADA-W
                    
           END-UNSTRING.
           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.


       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W

      *     MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W

          
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.

       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W

      *     MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W

      
           OPEN INPUT ARCHIVO-USUNET
      
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
         
           INITIALIZE NOM-TER-W
                      NOM-CIUD-W
                      NOM-ENTI-W
                      NOM-ACT-W
                      NOM-ZON-W
                      NOM-GRADNEG-W
                      NOM-VEND-W
                    
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
 
           MOVE "\\" TO NOM-TER-W
                        NOM-CIUD-W
                        NOM-ENTI-W
                        NOM-ACT-W
                        NOM-ZON-W
                        NOM-GRADNEG-W
                        NOM-VEND-W

                       
           INSPECT NOM-TER-W REPLACING FIRST "                    "
                                           BY IP-DATOS-USUNET 
           INSPECT NOM-TER-W REPLACING FIRST " "
                                           BY "\"
           INSPECT NOM-TER-W REPLACING FIRST "                              "
                                            BY DIR-CONTAB-LLEGADA-W. 
           MOVE NOM-TER-W TO NOM-ZON-W
                             NOM-VEND-W

           INSPECT NOM-TER-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-ARCHTER.DAT".

           INSPECT NOM-ZON-W REPLACING FIRST "                        "
                                          BY "\CONTROL\SC-ARCHZONA.DAT".

           INSPECT NOM-VEND-W REPLACING FIRST "                       "
                                           BY "\CONTROL\SC-ARCHVEN.DAT".

           INSPECT NOM-CIUD-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET
           INSPECT NOM-CIUD-W REPLACING FIRST " "
                                          BY "\"
           MOVE NOM-CIUD-W TO NOM-ENTI-W
                              NOM-ACT-W
                              NOM-GRADNEG-W
                                      
           INSPECT NOM-CIUD-W REPLACING FIRST "                          "
                                           BY "PROG\DATOS\SC-ARCHCIUD.DAT".

           INSPECT NOM-ENTI-W REPLACING FIRST "                         "
                                           BY "PROG\DATOS\SC-ARCHENT.DAT".

           INSPECT NOM-ACT-W REPLACING FIRST "                        "
                                          BY "PROG\DATOS\SCARCHACT.DAT".

           INSPECT NOM-GRADNEG-W REPLACING FIRST "                       "
                                              BY "PROG\DATOS\SC-GRNEG.DAT".

          
       ABRIR-ARCHIVO.

           OPEN INPUT ARCHIVO-TERCEROS
                      ARCHIVO-CIUDAD 
                      ARCHIVO-ENTIDADES 
                      ARCHIVO-ACTIVIDAD
                      ARCHIVO-ZONAS
                      ARCHIVO-GRADO-NEG
                      ARCHIVO-VENDEDOR.
                     

           MOVE "00"  TO SW-INVALID.

       LEER-ARCHIVO.
           MOVE COD-TERCERO-LLEGADA-W  TO COD-TERCERO

           READ ARCHIVO-TERCEROS WITH NO LOCK 
                INVALID KEY
                  MOVE "01"  TO SW-INVALID
           END-READ.

           MOVE SW-INVALID        TO SW-INVALID-ENV           
           MOVE COD-TERCERO       TO COD-TERCERO-ENV
           MOVE DV-TER            TO DV-TER-ENV               
           MOVE FECHA-CUMP-VEH    TO FECHA-CUMP-VEH-ENV
           MOVE APEL1-TER2        TO APEL1-TER2-ENV
           MOVE APEL2-TER2        TO APEL2-TER2-ENV
           MOVE NOMB1-TER2        TO NOMB1-TER2-ENV                       
           MOVE DIRECC-TER        TO DIRECC-TER-ENV
           MOVE COD-CIU-TER       TO COD-CIU-TER-ENV
           MOVE COD-CIU-TER-ENV   TO COD-CIU
           READ ARCHIVO-CIUDAD WITH NO LOCK
                INVALID KEY 
                   MOVE COD-CIU   TO NOMBRE-CIU
           END-READ
           MOVE NOMBRE-CIU        TO NOMBRE-CIU-TER-ENV         
           MOVE INDICATIVO-TER    TO INDICATIVO-TER-ENV        
           MOVE TELEFONO-TER      TO TELEFONO-TER-ENV                  
           MOVE NIT-TER           TO NIT-TER-ENV                             
           MOVE TIPO-ID-TER       TO TIPO-ID-TER-ENV                     
           MOVE ENTIDAD-TER       TO ENTIDAD-TER-ENV
           MOVE ENTIDAD-TER-ENV   TO COD-ENT
           READ ARCHIVO-ENTIDADES WITH NO LOCK
                INVALID KEY 
                   MOVE COD-ENT   TO NOMBRE-ENT
           END-READ
           MOVE NOMBRE-ENT        TO NOMBRE-ENT-TER-ENV                    
           MOVE ACT-TER           TO ACT-TER-ENV-ENV 
           MOVE ACT-TER-ENV-ENV   TO COD-ACT
           READ ARCHIVO-ACTIVIDAD WITH NO LOCK
                INVALID KEY 
                   MOVE COD-ACT   TO NOMBRE-ACT
           END-READ
           MOVE NOMBRE-ACT        TO NOMBRE-ACT-TER-ENV                       
           MOVE CONVENIO-TER      TO CONVENIO-TER-ENV                     
           MOVE RUT-TER           TO RUT-TER-ENV                                 
           MOVE NOM-COMER-TER     TO NOM-COMER-TER-ENV                       
           MOVE REFER1-TER        TO REFER1-TER-ENV                          
           MOVE REFER2-TER        TO REFER2-TER-ENV                          
           MOVE REFER3-TER        TO REFER3-TER-ENV                          
           MOVE CARGO-TER         TO CARGO-TER-ENV                          
           MOVE E-MAIL-TER        TO E-MAIL-TER-ENV                            
           MOVE ASESOR-TER        TO ASESOR-TER-ENV                                  
           MOVE TIPO-CUPO-TER2    TO TIPO-CUPO-TER2-ENV                       
           MOVE FECHA-CRE-TER     TO FECHA-CRE-TER-ENV                    
           MOVE ADMI-CRE-TER      TO ADMI-CRE-TER-ENV                        
           MOVE FECHA-MOD-TER     TO FECHA-MOD-TER-ENV           
           MOVE ADMI-MOD-TER      TO ADMI-MOD-TER-ENV                                           
           MOVE FACTOR-TER        TO FACTOR-TER-EDIT
           MOVE FACTOR-TER-EDIT   TO FACTOR-TER-ENV                            
           MOVE CUPO-TER          TO CUPO-TER-ENV                    
           MOVE VENDEDOR-TER      TO VENDEDOR-TER-ENV                        
           MOVE PAGO-TER          TO PAGO-TER-ENV                        
           MOVE PLAZO-TER         TO PLAZO-TER-ENV

           MOVE ZONA-TER          TO ZONA-TER-ENV
           MOVE 1                 TO TIPO-ZONA
           MOVE ZONA-TER-ENV      TO COD-ZONA 
           READ ARCHIVO-ZONAS WITH NO LOCK
                INVALID KEY 
                   MOVE LLAVE-ZONA TO DESCRIP-ZONA   
           END-READ
           MOVE DESCRIP-ZONA      TO DESCRIP-ZONA-ENV
                 
           MOVE RUTA-TER          TO RUTA-TER-ENV 
           MOVE 2                 TO TIPO-ZONA
           MOVE RUTA-TER-ENV      TO COD-ZONA 
           READ ARCHIVO-ZONAS WITH NO LOCK
                INVALID KEY 
                   MOVE LLAVE-ZONA TO DESCRIP-ZONA   
           END-READ
           MOVE DESCRIP-ZONA      TO DESCRIP-RUTA-ENV
                          
           MOVE ORDEN-TER         TO ORDEN-TER-ENV                          
           MOVE ACTIV-ICA         TO ACTIV-ICA-ENV
           MOVE PORC-ICA-TER2     TO PORC-ICA-TER2-EDIT
           MOVE PORC-ICA-TER2-EDIT TO PORC-ICA-TER2-ENV                     
           MOVE PORC-RET-TER      TO PORC-RET-TER-EDIT
           MOVE PORC-RET-TER-EDIT TO PORC-RET-TER-ENV                       
           MOVE GRADO-TER         TO GRADO-TER-ENV

           MOVE GRADO-TER-ENV     TO LLAVE-GRAD 
           READ ARCHIVO-GRADO-NEG WITH NO LOCK
                INVALID KEY 
                  MOVE LLAVE-GRAD TO DESCRIP-GRAD
           END-READ
           MOVE DESCRIP-GRAD      TO DESCRIP-GRAD-TER-ENV
                           
           MOVE REG-IVA-TER       TO REG-IVA-TER-ENV                        
           MOVE CALIFIC-TER       TO CALIFIC-TER-ENV                       
           MOVE GRAN-CONTRIB-TER  TO GRAN-CONTRIB-TER-ENV               
           MOVE RETENEDOR-TER     TO RETENEDOR-TER-ENV                      
           MOVE VLR-BASE-RET-TER  TO VLR-BASE-RET-TER-ENV                   
           MOVE RET-IVA-COMPRA    TO RET-IVA-COMPRA-ENV                     
           MOVE RET-IVA-TER       TO RET-IVA-TER-ENV                        
           MOVE EXENT-RET-TER     TO EXENT-RET-TER-ENV                      
           MOVE SEGURO-TER        TO SEGURO-TER-ENV                         
           MOVE DATACRE-TER       TO DATACRE-TER-ENV                       
           MOVE ACUE-PAGO-TER2    TO ACUE-PAGO-TER2-ENV                     
           MOVE CAPITADO-TER2     TO CAPITADO-TER2-ENV                     
           MOVE NIT1-CLI-TER      TO NIT1-CLI-TER-ENV                     
           MOVE RET-ICAV-TER      TO RET-ICAV-TER-ENV             
           MOVE BLOQ-TER          TO BLOQ-TER-ENV                          
           MOVE EXIVA-TER         TO EXIVA-TER-ENV                         
           MOVE MARCA-VEH         TO MARCA-VEH-ENV                         
           MOVE EMPRESA-VEH       TO EMPRESA-VEH-ENV                     
           MOVE NRO-VEH           TO NRO-VEH-ENV.                           
                                       


       CERRAR-ARCHIVO.
        
           CLOSE ARCHIVO-TERCEROS
                 ARCHIVO-CIUDAD 
                 ARCHIVO-ENTIDADES 
                 ARCHIVO-ACTIVIDAD
                 ARCHIVO-ZONAS
                 ARCHIVO-GRADO-NEG
                 ARCHIVO-VENDEDOR.
                     
       PAGINA-CONFIG.	 
           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO      TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3
         
           MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
           PERFORM ABRIR-HTML.

       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.

              
       COPY "..\..\FUENTES\SC-WEB19.CBL".
