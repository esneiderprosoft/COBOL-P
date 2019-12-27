      *=================================================================
      * BUSQUEDA Y CONSULTA DE ARCHIVO PACIENTES 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
           COPY "..\..\FUENTES\FS-USUNET.CBL".
           COPY "..\..\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                 ASSIGN NOM-USUAR-W
                 ORGANIZATION IS INDEXED
                 ACCESS MODE IS DYNAMIC;
                 RECORD KEY IS NOMBRE-USU
                 ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                 ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                 ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                 FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-PACIENTES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PACIE-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PACI
                  ALTERNATE RECORD KEY  DESCRIP-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  EPS-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  ID-COTIZ-PACI WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.



       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-PACIE.CBL".
         

       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-USUAR-W                            PIC X(70).
       77 NOM-PACIE-W                           PIC X(70).
       01 LLEGADA-W                              PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W                      PIC X(15).
             03 FECHA-LLEGAD-W                   PIC X(8).
             03 HORA-LLEGAD-W                    PIC X(6).
          02 DIR-CONTAB-LLEGADA-W                PIC X(8).
          02 MES-CONTAB-LLEGADA-W                PIC XX.
          02 COD-PACI-LLEGADA-W                  PIC X(15). 

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV                      PIC XX.
          02 FILLER                              PIC X     VALUE "|".
          02 COD-PACI-ENV                        PIC X(15).
          02 FILLER                              PIC X     VALUE "|".
          02 NOM-COMUNIDAD-PACI-ENV              PIC 9(3).
          02 FILLER                              PIC X     VALUE "|".
          02 NOM-RESGUARDO-PACI-ENV              PIC 9(3).
          02 FILLER                              PIC X     VALUE "|".
          02 E-MAIL-PACI-ENV                     PIC X(80).
          02 FILLER                              PIC X     VALUE "|".
          02 DATO-ACT-PACI-ENV.
             03 OPER-CREA-PACI                   PIC X(4).
             03 FECHA-CREA-PACI.
                05 ANO-CREA-PACI                 PIC 9(4).
                05 MES-CREA-PACI                 PIC 99.
                05 DIA-CREA-PACI                 PIC 99.
             03 HORA-CREA-PACI.
                05 HR-CREA-PACI                  PIC 99.
                05 MN-CREA-PACI                  PIC 99.
             03 OPER-CORR-PACI                   PIC X(4).
             03 FECHA-CORR-PACI.
                05 ANO-CORR-PACI                 PIC 9(4).
                05 MES-CORR-PACI                 PIC 99.
                05 DIA-CORR-PACI                 PIC 99.
             03 HORA-CORR-PACI.
                05 HR-CORR-PACI                  PIC 99.
                05 MN-CORR-PACI                  PIC 99. 
          02 FILLER                              PIC X     VALUE "|".
                      
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
              MOVE "INV103-15"              TO MSJ3-HTML
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
              MOVE "SER110C_03_1"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-USUAR-W          TO MSJ2-HTML
              MOVE "SER110C_03_1"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.


       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-PACIE-W               TO MSJ2-HTML
              MOVE "SER110C_03_1"              TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, COD-PACI-LLEGADA-W     
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
         
           INITIALIZE NOM-USUAR-W 
                      NOM-PACIE-W
                                     
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
 
           MOVE "\\" TO NOM-USUAR-W 
                        NOM-PACIE-W                                              

           INSPECT NOM-PACIE-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET
           INSPECT NOM-PACIE-W REPLACING FIRST " "
                                          BY "\"          
           INSPECT NOM-PACIE-W REPLACING FIRST "        "
                                          BY DIR-CONTAB-LLEGADA-W 
           MOVE NOM-PACIE-W TO NOM-USUAR-W

           INSPECT NOM-PACIE-W REPLACING FIRST "                     "
                                            BY "\CONTROL\SC-PACIE.DAT".
           INSPECT NOM-USUAR-W REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHUSU.DAT".
       ABRIR-ARCHIVOS.
         
           OPEN INPUT ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.

           OPEN INPUT ARCHIVO-PACIENTES.
                   
           MOVE COD-PACI-LLEGADA-W  TO COD-PACI
           MOVE "00"               TO SW-INVALID

           READ ARCHIVO-PACIENTES WITH NO LOCK 
                INVALID KEY
                  MOVE "01" TO SW-INVALID                  
           END-READ.

           MOVE SW-INVALID              TO SW-INVALID-ENV
           MOVE COD-PACI                TO COD-PACI-ENV         
           MOVE NOM-COMUNIDAD-PACI      TO NOM-COMUNIDAD-PACI-ENV
           MOVE NOM-RESGUARDO-PACI      TO NOM-RESGUARDO-PACI-ENV 
           MOVE E-MAIL-PACI             TO E-MAIL-PACI-ENV
           MOVE DATO-ACT-PACI           TO DATO-ACT-PACI-ENV.           

       CERRAR-MAESTRO-ARTICULOS.           
           CLOSE ARCHIVO-PACIENTES.
               
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