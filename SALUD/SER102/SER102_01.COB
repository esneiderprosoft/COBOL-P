      *=================================================================
      * CONSULTA DE ARCHIVO GRSER
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

            SELECT ARCHIVO-GR-SER LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-GRSER-W
      * "../control/SC-GRUPOSER.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CODIGO-GRSER
                  FILE STATUS IS OTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-GRSER.CBL".

       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-GRSER-W                  PIC X(70).
       
       01 LLEGADA-W                    PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(8).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 CODIGO-GRSER-LLEGADA-W    PIC X(15).
        
       01 DATOS-ENVIO.
          02 SW-INVALID-ENV            PIC X(2).
               
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
              MOVE "SER102_01"              TO MSJ3-HTML
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
              MOVE "SER102_01"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-GR-SER.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MENSAJE1-HTML
              MOVE NOM-GRSER-W                TO MENSAJE2-HTML
              MOVE "SER102_01"              TO MENSAJE3-HTML
              GO TO ENVIAR-ERROR
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, CODIGO-GRSER-LLEGADA-W
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.


       INICIAR-SESION.
           MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W
          
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.

       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
      
           OPEN INPUT ARCHIVO-USUNET
      
           INITIALIZE LLAVE-USUNET.

       LEER-USUARIO.           
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
           CLOSE ARCHIVO-USUNET.
      
           IF NOMBRE-USUNET = SPACES
              MOVE "Validacion de usuarios"      TO MENSAJE1-HTML
              MOVE "Falta configurar usuario"    TO MENSAJE2-HTML
              MOVE "Sc"                          TO MENSAJE3-HTML
              GO TO ENVIAR-ERROR
           END-IF.
 
       ASIGNAR-NOMBRES.                  
         
           INITIALIZE NOM-GRSER-W 

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
 
           MOVE "\\" TO NOM-GRSER-W

           INSPECT NOM-GRSER-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET
           INSPECT NOM-GRSER-W REPLACING FIRST " "
                                          BY "\"          
           INSPECT NOM-GRSER-W REPLACING FIRST "        "
                                          BY DIR-CONTAB-LLEGADA-W          
           INSPECT NOM-GRSER-W REPLACING FIRST " "
                                            BY "\"
           INSPECT NOM-GRSER-W REPLACING FIRST "                       "
                                            BY "CONTROL\SC-GRUPOSER.DAT".


       ABRIR-ARCHIVO-GR-SER.
           OPEN INPUT ARCHIVO-GR-SER.
           MOVE "00"  TO SW-INVALID-ENV.

       LEER-ARCHIVO-GR-SER.
           MOVE CODIGO-GRSER-LLEGADA-W  TO CODIGO-GRSER
           READ ARCHIVO-GR-SER WITH NO LOCK KEY IS CODIGO-GRSER
                INVALID KEY
                  MOVE "01"  TO SW-INVALID-ENV
           END-READ.

       CERRAR-ARCHIVO-GR-SER.            
           CLOSE ARCHIVO-GR-SER.

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