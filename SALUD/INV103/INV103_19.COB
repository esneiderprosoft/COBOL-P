      *=================================================================
      * CONSULTA DE ARCHIVO PREMED- INV103 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

             SELECT ARCHIVO-PREMED LOCK MODE IS AUTOMATIC
                    ASSIGN NOM-PREMED-W
      *"../control/SC-ARCHPRES.DAT"
                    ORGANIZATION IS INDEXED
                    ACCESS MODE IS DYNAMIC;
                    RECORD KEY IS LLAVE-PREME
                    ALTERNATE RECORD KEY IS DESCRIP-PREME WITH DUPLICATES
                    FILE STATUS IS OTR-STAT.


       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-PREMED.CBL".

       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-PREMED-W                  PIC X(70).
       
       01 LLEGADA-W                    PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(30).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 LLAVE-PREME-LLEGADA-W.
	     05 TIPO-PREME-W           PIC X.
	     05 COD-PREME-W.
                07 COD-PREME1-W        PIC X.
                07 COD-PREME2-W        PIC X. 
          02 OPERADOR-LLEGADA-W        PIC X(4).

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV            PIC X(2).
          02 FILLER                    PIC X VALUE "|".
          02 DESCRIP-PREME-ENV         PIC X(20).
               
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
              MOVE "INV103_19"              TO MSJ3-HTML
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
              MOVE "INV103_19"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PREMED.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PREMED-W              TO MSJ2-HTML
              MOVE "INV103_19"              TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, LLAVE-PREME-LLEGADA-W,
                   OPERADOR-LLEGADA-W
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
         
           INITIALIZE NOM-PREMED-W 
 
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
 
           MOVE "\\" TO NOM-PREMED-W

           INSPECT NOM-PREMED-W REPLACING FIRST "                    "
                                           BY IP-DATOS-USUNET
           INSPECT NOM-PREMED-W REPLACING FIRST " "
                                           BY "\"          
           INSPECT NOM-PREMED-W REPLACING FIRST "                              "
                                           BY DIR-CONTAB-LLEGADA-W          
           INSPECT NOM-PREMED-W REPLACING FIRST " "
                                           BY "\"
           INSPECT NOM-PREMED-W REPLACING FIRST "                       "
                                             BY "CONTROL\SC-ARCHPRES.DAT".


       ABRIR-ARCHIVO-PREMED.
           OPEN INPUT ARCHIVO-PREMED.
           MOVE "00"  TO SW-INVALID-ENV.

       LEER-ARCHIVO-PREMED.
           MOVE LLAVE-PREME-LLEGADA-W  TO LLAVE-PREME
           READ ARCHIVO-PREMED WITH NO LOCK 
                INVALID KEY
                  MOVE "01"  TO SW-INVALID-ENV
           END-READ.

       CERRAR-ARCHIVO-PREMED. 
           
           MOVE DESCRIP-PREME  TO DESCRIP-PREME-ENV

           CLOSE ARCHIVO-PREMED.

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