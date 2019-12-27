      *=================================================================
      * BUSCA SI HAY CITAS DE ESE DIA - SER891A 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

           SELECT ARCHIVO-CITAS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-CITAS-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-CIT
		  ALTERNATE RECORD KEY IS FECHA-CIT        WITH DUPLICATES
		  ALTERNATE RECORD KEY IS MEDICO-CIT       WITH DUPLICATES
		  ALTERNATE RECORD KEY IS PACI-CIT         WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LLAVE-ALT-CIT    WITH DUPLICATES
		  ALTERNATE RECORD KEY IS ATIENDE-PROF-CIT WITH DUPLICATES				  
                  ALTERNATE RECORD KEY IS FECHA-FACTU-CIT  WITH DUPLICATES
		  ALTERNATE RECORD KEY IS FECHA-SOLIC-CIT  WITH DUPLICATES				  
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-USUAR-W
      * "../CONTROL/SC-ARCHUSU.DAT"
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
       COPY "..\..\FUENTES\FD-CITAS.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
     

       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 CONT-W                       PIC 99.
                                       
       01 LLEGADA-W                    PIC X(100).
                               
       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(21).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 FECHA-FACT-LLEGA          PIC X(6).
          02 MED-OTR-FACT-LLEGA        PIC X(10).
          02 ID-HC-LLEGA               PIC X(15).
          02 CL-FACT-LLEGA             PIC X.
          02 PUERTA-ESTAD-LLEGA        PIC X.

       01 LLAVE-CITAS-W.
          02 FECHA-CITA-W             PIC X(6).
          02 MED-CITA-W               PIC X(10).
          02 ID-HC-CITA-W             PIC X(15).
          02 CL-CITA-W                PIC X.
        
       77 NOM-CITAS-W                  PIC X(70).
       77 NOM-USUAR-W                  PIC X(70).
       77 SW-INVALID-W                 PIC XX.
              
       
       01 DATOS-ENVIO.
          02 ESTADO-ENV                PIC XX.           
          02 FILLER                    PIC X VALUE "|".
          02 INVALID-ENV               PIC XX. 
          02 FILLER                    PIC X VALUE "|".
          02 FECHA-CITA-ENV            PIC X(6).
          02 FILLER                    PIC X VALUE "|".
          02 MED-CITA-ENV              PIC X(10).
          02 FILLER                    PIC X VALUE "|".
          02 ID-HC-CITA-ENV            PIC X(15).
          02 FILLER                    PIC X VALUE "|".
          02 CL-CITA-ENV               PIC X.
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
              MOVE "SER891AD"               TO MSJ3-HTML
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
              MOVE "SER891AD"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CITAS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-CITAS-W              TO MSJ2-HTML
              MOVE "SER891AD"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
   
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USUAR-W              TO MSJ2-HTML
              MOVE "SER891AD"               TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, FECHA-FACT-LLEGA, MED-OTR-FACT-LLEGA,
                   ID-HC-LLEGA, CL-FACT-LLEGA, PUERTA-ESTAD-LLEGA
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.


       INICIAR-SESION.
           MOVE "D:\PROGELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
          
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.

       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\PROGELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
      
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
         
           INITIALIZE NOM-USUAR-W NOM-CITAS-W 

           MOVE "\\" TO NOM-USUAR-W NOM-CITAS-W

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-USUAR-W REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-USUAR-W REPLACING FIRST "                     "
                                            BY  DIR-CONTAB-LLEGADA-W.

           INSPECT NOM-USUAR-W REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHUSU.DAT".

           INSPECT NOM-CITAS-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-CITAS-W REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-CITAS-W REPLACING FIRST "                          "
                                            BY "PROG\HC\DATOS\SC-AGEND.DAT".

       LEER-USUARIO.
           OPEN INPUT  ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT AT END MOVE 0 TO OPCION-1
           END-READ
           CLOSE ARCHIVO-USUARIOS.

           IF PREFIJO-USU = "  "
              MOVE "00"  TO PREFIJO-USU. 


           IF ID-HC-LLEGA = MED-CITA-W
           OR PUERTA-ESTAD-LLEGA = 1
              GO TO CERRAR-SESION
           END-IF.

       ABRIR-CITAS.
           OPEN I-O   ARCHIVO-CITAS.

           INITIALIZE LLAVE-CIT CONT-W
           MOVE  FECHA-FACT-LLEGA      TO FECHA-CITA-W
           MOVE  MED-OTR-FACT-LLEGA    TO MED-CITA-W
           MOVE  ID-HC-LLEGA           TO ID-HC-CITA-W
           MOVE  CL-FACT-LLEGA         TO CL-CITA-W
           MOVE  LLAVE-CITAS-W         TO LLAVE-PACI-CIT

           START ARCHIVO-CITAS KEY >= LLAVE-CIT
                 INVALID KEY
                     GO TO CERRAR-ARCHIVOS
           END-START.

       LEER-CITAS.
           READ ARCHIVO-CITAS NEXT WITH NO LOCK AT END 
                GO TO CERRAR-ARCHIVOS
           END-READ

           IF LLAVE-CITAS-W  = LLAVE-PACI-CIT
              CONTINUE
           ELSE
              GO TO CERRAR-ARCHIVOS
           END-IF

           ADD 1 TO CONT-W
           GO TO LEER-CITAS.

       CERRAR-ARCHIVOS.
           IF CONT-W = ZEROS
               MOVE "9F" TO SW-INVALID-W
           END-IF
           CLOSE ARCHIVO-CITAS. 
       ENVIO-DATOS.

           MOVE "00"          TO ESTADO-ENV
           MOVE SW-INVALID-W  TO INVALID-ENV
           MOVE FECHA-CITA-W  TO FECHA-CITA-ENV
           MOVE MED-CITA-W    TO MED-CITA-ENV
           MOVE ID-HC-CITA-W  TO ID-HC-CITA-ENV
           MOVE CL-CITA-W     TO CL-CITA-ENV.
                 
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
