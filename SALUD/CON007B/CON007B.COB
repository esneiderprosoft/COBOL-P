      *=================================================================
      * BUSCA SI ESTA BLOQUEADO EL MES - CON007B 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

           SELECT MOVIMIENTO-DIARIO LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-MOV-W  
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-MOV
                  ALTERNATE RECORD KEY SECUENCIA-MOV
                  ALTERNATE RECORD KEY LLAVE-FEC
                  ALTERNATE RECORD KEY LLAVE-DOC-MOV   WITH DUPLICATES
                  ALTERNATE RECORD KEY COSTO-MOV       WITH DUPLICATES
                  ALTERNATE RECORD KEY NRO-ORD-MOV     WITH DUPLICATES
                  ALTERNATE RECORD KEY LLAVE-LIBRE-MOV WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-MOVIM.CBL".
      


       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-MOV-W                    PIC X(70).
       77 OPC-SEGU                     PIC X(6).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(20).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 ADMIN-LLEGADA-W           PIC X(4).

       01 LLEGADA-W                    PIC X(100).
   
       01 BLOQ-W                       PIC X.                   

       01 DATOS-ENVIO.
          02 ESTADO-ENVIO              PIC XX.
          02 FILLER                    PIC X VALUE "|".
          02 BLOQ-ENVIAR               PIC X. 
               
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
              MOVE "CON007B"                TO MSJ3-HTML
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
              MOVE "CON007B"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MOVIMIENTO-DIARIO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MENSAJE1-HTML
              MOVE NOM-MOV-W                TO MENSAJE2-HTML
              MOVE "CON007B"                TO MENSAJE3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, ADMIN-LLEGADA-W
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
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
         
           INITIALIZE NOM-MOV-W  

           MOVE "\\" TO NOM-MOV-W 
 
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

           INSPECT NOM-MOV-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET
           INSPECT NOM-MOV-W REPLACING FIRST " "
                                          BY "\"
           INSPECT NOM-MOV-W REPLACING FIRST "                    "
                                          BY DIR-CONTAB-LLEGADA-W.
           INSPECT NOM-MOV-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-ARCHMOV.DAT".

           MOVE ADMIN-LLEGADA-W TO ADMIN-W.


       LEER-MOVIMIENTO.    
           OPEN INPUT MOVIMIENTO-DIARIO.

           IF OTR-STAT IS NOT = 00
               MOVE 1 TO BLOQ-W
               GO TO CERRAR-SESION
            END-IF.

            MOVE "999999999" TO SECUENCIA-MOV.
            READ MOVIMIENTO-DIARIO RECORD KEY IS SECUENCIA-MOV
                 INVALID KEY MOVE 0 TO COSTO1-MOV.
            CLOSE MOVIMIENTO-DIARIO.

            MOVE COSTO1-MOV TO BLOQ-W.

       ENVIO-DATOS.
            MOVE "00"   TO ESTADO-ENVIO
            MOVE BLOQ-W TO BLOQ-ENVIAR.

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