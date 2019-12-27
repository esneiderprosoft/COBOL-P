      *=================================================================
      * DEVUELVE LA DEPENDENCIA DEL OPERADOR - CON003A
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

           SELECT ARCHIVO-RESTRICCION LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-REST-W
      * "\PROG\DATOS\SC-ARCHREST.SC"
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY LLAVE-REST
                  ALTERNATE RECORD KEY NOMBRE-REST WITH DUPLICATES
                  FILE STATUS OTR-STAT.


           SELECT ARCHIVO-DEPENDENCIAS  LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-DEPEND-W
      *"\PROG\DATOS\SC-DEPEN.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS LLAVE-DEPEN
                  ALTERNATE RECORD KEY IS
                  DESCRIP1-DEPEN WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-RESTR.CBL".
       COPY "..\..\FUENTES\FD-DEPEN.CBL".


       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

      * 77 NOM-REST-W                   PIC X(70).
       77 NOM-DEPEND-W                 PIC X(70).
       
       01 LLEGADA-W                    PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(20).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 OPER-W                    PIC X(4).
          02 DEPEN-W.
             03 DEPEN1-W               PIC X.
             03 DEPEN2-W               PIC XX.

         

       01 DATOS-ENVIO.
          02 COD-ENVIAR                PIC X(2).
          02 FILLER                    PIC X VALUE "|".
          02 DEPEN-ENVIAR              PIC X. 
               
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
              MOVE "CON003A"                TO MSJ3-HTML
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
              MOVE "CON003A"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-RESTRICCION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MENSAJE1-HTML
              MOVE NOM-REST-W               TO MENSAJE2-HTML
              MOVE "CON003A"                TO MENSAJE3-HTML
              GO TO ENVIAR-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-DEPENDENCIAS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MENSAJE1-HTML
              MOVE NOM-DEPEND-W             TO MENSAJE2-HTML
              MOVE "CON003A"                TO MENSAJE3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPER-W, DEPEN-W 
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
         
           INITIALIZE NOM-REST-W NOM-DEPEND-W 
 
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
 
           MOVE "\\" TO NOM-REST-W NOM-DEPEND-W

           INSPECT NOM-REST-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET
           INSPECT NOM-REST-W REPLACING FIRST " "
                                          BY "\"

           MOVE NOM-REST-W TO NOM-DEPEND-W
          
           INSPECT NOM-REST-W REPLACING FIRST "                         "
                                          BY "PROG\DATOS\SC-ARCHREST.SC".

           INSPECT NOM-REST-W REPLACING FIRST "                       "
                                          BY  "PROG\DATOS\SC-DEPEN.DAT".     

       LEER-OPERADOR.
           OPEN INPUT ARCHIVO-RESTRICCION.

           IF OTR-STAT IS NOT = "00"
              MOVE 000 TO DEPEN-W
              GO TO CERRAR-SESION.
      
           INITIALIZE REG-REST.
           IF OPER-W = SPACES
              MOVE 000 TO DEPEN-REST
           ELSE
              MOVE OPER-W TO COD-REST
              READ ARCHIVO-RESTRICCION WITH NO LOCK
                   INVALID KEY
                     MOVE 000 TO DEPEN-REST
              END-READ
           END-IF.

           CLOSE ARCHIVO-RESTRICCION.
      

           IF DEPEN-REST IS NOT NUMERIC
              MOVE 000 TO DEPEN-REST.
      
      
       ABRIR-DEPENDENCIA.
           OPEN INPUT ARCHIVO-DEPENDENCIAS.

           EVALUATE OTR-STAT
           WHEN "00"  CONTINUE
           WHEN "35"  OPEN OUTPUT ARCHIVO-DEPENDENCIAS
                      MOVE 000               TO COD-DEPEN
                      MOVE "SIN DEPENDENCIA" TO DESCRIP-DEPEN
                      MOVE 00                TO CONF-IMPRES-DEPEN
                      WRITE REG-DEPEN
                      CLOSE       ARCHIVO-DEPENDENCIAS
                      OPEN INPUT  ARCHIVO-DEPENDENCIAS
           WHEN OTHER MOVE 000 TO DEPEN-W
                      GO TO CERRAR-SESION.
      
           MOVE DEPEN-REST          TO COD-DEPEN.
           READ ARCHIVO-DEPENDENCIAS WITH NO LOCK
                INVALID KEY
                  MOVE 00 TO CONF-IMPRES-DEPEN
           END-READ.

           MOVE 0                   TO DEPEN1-W.
           MOVE CONF-IMPRES-DEPEN   TO DEPEN2-W.

           CLOSE ARCHIVO-DEPENDENCIAS.


       ENVIO-DATOS.            
            MOVE "00"    TO COD-ENVIAR.
            MOVE DEPEN-W TO DEPEN-ENVIAR.

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