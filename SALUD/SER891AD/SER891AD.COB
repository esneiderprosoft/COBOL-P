      *=================================================================
      * DEVUELVE LA HORA DE LA CITA DEL DIA FACTURADO - SER891AD 
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

       01 LLEGADA-W                    PIC X(100).
                               
       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(21).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 FECHA-FACT-W              PIC X(6).
          02 MED-OTR-FACT-W            PIC 9(10).
          02 ID-HISTORIA-FACT-W        PIC X(15).
          02 CL-FACT-W                 PIC 9.
          02 PUERTA-ESTAD-W            PIC 9.
          02 HORA-CIT-LNK-W.
             03 HR-CIT-LNK             PIC 99.
             03 MN-CIT-LNK             PIC 99.
        
       77 NOM-CITAS-W                  PIC X(70).
       77 NOM-USUAR-W                  PIC X(70).
              
       
       01 DATOS-ENVIO.
          02 ESTADO-ENV                PIC XX.           
          02 FILLER                    PIC X VALUE "|".
          02 HORA-CIT-ENV              PIC X(4). 

                  
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, FECHA-FACT-W, MED-OTR-FACT-W,
                   ID-HISTORIA-FACT-W, CL-FACT-W, PUERTA-ESTAD-W, HORA-CIT-LNK-W   
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

       ABRIR-CITAS.
            OPEN I-O   ARCHIVO-CITAS.
          
       BUSCAR-CITAS.
           MOVE  ID-HISTORIA-FACT-W TO PACI-CIT.

           START ARCHIVO-CITAS KEY = PACI-CIT
                 INVALID KEY
                   INITIALIZE HORA-CIT-LNK-W
                   GO TO VALIDAR-HORA
           END-START.

       LEER-CITAS.
           READ ARCHIVO-CITAS NEXT WITH NO LOCK AT END 
                GO TO CERRAR-ARCHIVOS
           END-READ

           IF PACI-CIT = ID-HISTORIA-FACT-W
              IF FECHA-CIT = FECHA-FACT-W
                 MOVE HORA-CIT TO HORA-CIT-LNK-W
                 GO TO VALIDAR-HORA
              ELSE
                 GO TO LEER-CITAS
              END-IF
           ELSE
              GO TO CERRAR-ARCHIVOS
           END-IF.

       VALIDAR-HORA.
           IF  HR-CIT-LNK = 00
           AND HR-CIT-LNK = 00
               ACCEPT HORA-CIT-LNK-W FROM TIME
           END-IF.

       CERRAR-ARCHIVOS.

           CLOSE ARCHIVO-CITAS. 
      
       ENVIO-DATOS.

           MOVE "00"           TO ESTADO-ENV
           MOVE HORA-CIT-LNK-W TO HORA-CIT-ENV.
       
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
