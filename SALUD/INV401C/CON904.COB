      *=================================================================
      *  SISTEMA DE SEGURIDAD - BUSCA SI HAY RESTRICIONES    CON904S
      *=================================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PROSOFT.
       OBJECT-COMPUTER. PROSOFT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            COPY "..\..\FUENTES\FS-USUNET.CBL".
            COPY "..\..\FUENTES\FS-SESION.CBL".

            SELECT ARCHIVO-RESTRICCION LOCK MODE IS AUTOMATIC
                   ASSIGN TO NOM-RESTR-W 
                   ORGANIZATION INDEXED;
                   ACCESS MODE DYNAMIC;
                   RECORD KEY LLAVE-REST
                   ALTERNATE RECORD KEY NOMBRE-REST WITH DUPLICATES
                   FILE STATUS OTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-RESTR.CBL".
       
       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".
       
       77 NOM-RESTR-W                     PIC X(70).
       77 NOMBRE-OPER                     PIC X(30).
       77 NOMBRE-BMP                      PIC X(40).
       
       01 DATO-LLEGADA-W.
          02 LLAVESESION-LLEGA-W.
             03 ID-LLEGAD-W               PIC X(15).
             03 FECHA-LLEGAD-W            PIC X(8).
             03 HORA-LLEGAD-W             PIC X(6).
          02 DIR-CONTAB-LLEGADA-W         PIC X(8).
          02 MES-CONTAB-LLEGADA-W         PIC XX.
          02 ADMIN-LLEGADA-W              PIC X(4).
          02 OPC-SEGU-LLEGADA-W           PIC X(6).


       01 LLEGADA-W                       PIC X(100).

       01  COD-W                          PIC X(6).
       01  INVALID-W                      PIC 99.
       

       01 DATOS-ENVIO.
          02 COD-W-ENV                    PIC X(6).
          02 FILLER                       PIC X VALUE "|".
          02 NOMBRE-OPER-ENV              PIC X(30).
          02 FILLER                       PIC X VALUE "|".
          02 COD-REST-ENV                 PIC X(4).
          02 FILLER                       PIC X VALUE "|".
          02 INVALID-ENV                  PIC 99.

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
              MOVE "CON904"                TO MSJ3-HTML
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
              MOVE "CON904"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-RESTRICCION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-RESTR-W              TO MSJ2-HTML
              MOVE "CON904"                TO MSJ3-HTML
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
              INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, ADMIN-LLEGADA-W, OPC-SEGU-LLEGADA-W
           END-UNSTRING.

           MOVE LLAVESESION-LLEGA-W    TO LLAVE-SESION-W.
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
           INITIALIZE NOM-RESTR-W

           MOVE "\\" TO NOM-RESTR-W
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           INSPECT NOM-RESTR-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET
           INSPECT NOM-RESTR-W REPLACING FIRST " "
                                            BY "\"
           INSPECT NOM-RESTR-W REPLACING FIRST "                         "
                                            BY "PROG\DATOS\SC-ARCHREST.SC".

           MOVE ADMIN-LLEGADA-W    TO ADMIN-W.
           MOVE OPC-SEGU-LLEGADA-W TO COD-W.
     
       REVISAR-DATO.
           IF ADMIN-W = "GEBC"
              MOVE 00                TO INVALID-W
              MOVE "ACCESO ESPECIAL" TO NOMBRE-OPER
              GO TO ENVIAR-DATOS
           END-IF.

           OPEN INPUT ARCHIVO-RESTRICCION.

           MOVE ADMIN-W            TO COD-REST.
           MOVE SPACES             TO OPC-REST.
           MOVE 00                 TO INVALID-W.
                         
           READ ARCHIVO-RESTRICCION 
                INVALID KEY     MOVE 01                    TO INVALID-W
                                MOVE "OPERARIO NO EXISTE"  TO NOMBRE-OPER
                                GO                         TO ENVIAR-DATOS
                NOT INVALID KEY MOVE NOMBRE-REST           TO NOMBRE-OPER
           
           MOVE ADMIN-W            TO COD-REST.
           MOVE COD-W              TO OPC-REST.
           MOVE 00                 TO INVALID-W.

           READ ARCHIVO-RESTRICCION 
                INVALID KEY     GO      TO ENVIAR-DATOS
                NOT INVALID KEY MOVE 01 TO INVALID-W
           END-READ.

       ENVIAR-DATOS.
           MOVE NOMBRE-OPER        TO NOMBRE-OPER-ENV
           MOVE COD-REST           TO COD-REST-ENV
           MOVE COD-W              TO COD-W-ENV
           MOVE INVALID-W          TO INVALID-ENV.

       PAGINA-CONFIG.
010720     MOVE "datosrecibidos"   TO COBW3-CNV-NAME
010730     MOVE DATOS-ENVIO        TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3
010750
010760     MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
010770     PERFORM ABRIR-HTML.
007980   
007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.

008030
008040 COPY "..\..\FUENTES\SC-WEB.CBL".
