      *=================================================================
      * BUSQUEDA Y CONSULTA DE MAESTRO ARTICULOS 
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

           SELECT MAESTRO-ARTICULOS LOCK MODE IS AUTOMATIC
                 ASSIGN NOM-ART-W
                 ORGANIZATION INDEXED;
                 ACCESS MODE  DYNAMIC;
                 RECORD KEY COD-ART
                 ALTERNATE RECORD KEY CLASE-ART       WITH DUPLICATES
                 ALTERNATE RECORD KEY COD-BARRAS-ART  WITH DUPLICATES
                 ALTERNATE RECORD KEY DESCRIP-ART     WITH DUPLICATES
                 ALTERNATE RECORD KEY USO-ART         WITH DUPLICATES
                 ALTERNATE RECORD KEY CL-ROTAC-ART    WITH DUPLICATES
                 ALTERNATE RECORD KEY UBICAC-ART      WITH DUPLICATES
                 ALTERNATE RECORD KEY FECHA-VENCE-ART WITH DUPLICATES
                 FILE STATUS IS OTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-ARTIC.CBL".

       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-USUAR-W                            PIC X(70).
       77 NOM-ART-W                              PIC X(70).
       

       01 LLEGADA-W                              PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W                      PIC X(15).
             03 FECHA-LLEGAD-W                   PIC X(8).
             03 HORA-LLEGAD-W                    PIC X(6).
          02 DIR-CONTAB-LLEGADA-W                PIC X(30).
          02 MES-CONTAB-LLEGADA-W                PIC XX.
          02 COD-ART-LLEGADA-W.
             03 LLAVE-NRO-ART-LLEGADA-W.
                05 LLAVE-GRUPO-ART-LLEGADA-W.
                   07 TIPO-ART-LLEGADA-W         PIC X.
                   07 GRUPO-ART-LLEGADA-W.
                      09 GRUPO1-ART-LLEGADA-W    PIC X.
                      09 GRUPO2-ART-LLEGADA-W    PIC X.
                05 NUMERO-ART-LLEGADA-W.
                   07 NRO1-ART-LLEGADA-W        PIC XX.
                   07 NRO2-ART-LLEGADA-W        PIC X(11).
             03 CLASE-ART-LLEGADA-W             PIC XX.
          02 OPERADOR-LLEGADA-W                  PIC X(4).

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV                     PIC XX.
          02 FILLER                             PIC X     VALUE "|".
          02 DESCRIP-ART-ENV                    PIC X(50). 

                       
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
              MOVE "INV103_05"              TO MSJ3-HTML
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
              MOVE "INV103_05"              TO MSJ3-HTML
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
              MOVE "INV103-05"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.


       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MAESTRO-ARTICULOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-ART-W                TO MSJ2-HTML
              MOVE "INV103_05"              TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, COD-ART-LLEGADA-W, OPERADOR-LLEGADA-W
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
         
           INITIALIZE NOM-USUAR-W NOM-ART-W 
 
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
 
           MOVE "\\" TO NOM-USUAR-W NOM-ART-W

           INSPECT NOM-ART-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET
           INSPECT NOM-ART-W REPLACING FIRST " "
                                          BY "\"          
           INSPECT NOM-ART-W REPLACING FIRST "                              "
                                          BY DIR-CONTAB-LLEGADA-W 
           MOVE NOM-ART-W TO NOM-USUAR-W         
          
           INSPECT NOM-ART-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-MAESART.DAT".
           INSPECT NOM-USUAR-W REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHUSU.DAT".

       ABRIR-ARCHIVOS.

           OPEN INPUT ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.

           OPEN INPUT MAESTRO-ARTICULOS.

           MOVE COD-ART-LLEGADA-W  TO COD-ART
           MOVE "00"               TO SW-INVALID

           READ MAESTRO-ARTICULOS WITH NO LOCK 
                INVALID KEY
                  MOVE "01" TO SW-INVALID                  
           END-READ.

       DATOS.

           MOVE SW-INVALID        TO SW-INVALID-ENV
           MOVE DESCRIP-ART       TO DESCRIP-ART-ENV.

       CERRAR-MAESTRO-ARTICULOS.  
          
           CLOSE MAESTRO-ARTICULOS.

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