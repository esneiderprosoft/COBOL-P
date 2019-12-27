      *=================================================================
      *  SISTEMA DE SEGURIDAD - BUSCA SI HAY RESTRICIONES    CON904
      *=================================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PROSOFT.
       OBJECT-COMPUTER. PROSOFT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
            COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-RESTRICCION LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-REST-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY LLAVE-REST  
                  ALTERNATE RECORD KEY NOMBRE-REST WITH DUPLICATES
                  FILE STATUS OTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-RESTR.CBL".
       
       WORKING-STORAGE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".

       01 NOM-USUAR-W                     PIC X(70).
       01 NOMBRE-OPER                     PIC X(30).
      * 01 NOMBRE-BMP                      PIC X(40).       
       
       01 LINEA-LLEGAD-W                  PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVESESION-LLEGA-W.
             03 ID-LLEGAD-W               PIC X(15).
             03 FECHA-LLEGAD-W            PIC X(8).
             03 HORA-LLEGAD-W             PIC X(6).
          02 DIR-CONTAB-LLEGADA-W         PIC X(8).
          02 MES-CONTAB-LLEGADA-W         PIC XX.
          02 ADMIN-LLEGADA-W              PIC X(4).
          02 OPC-SEGU-LLEGADA-W           PIC X(6).

       01 INVALID-W                       PIC 99.
       01 COD-W                           PIC X(6).

       01 DATOS-ENVIO.
          02 INVALID-ENV                  PIC 99.
          02 FILLER                       PIC X VALUE "|".
          02 COD-W-ENV                    PIC X(6).
          02 FILLER                       PIC X VALUE "|".
          02 NOMBRE-OPER-ENV              PIC X(30).
          02 FILLER                       PIC X VALUE "|".
          02 COD-REST-ENV                 PIC X(4).

       LINKAGE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\ISAPICTX.CBL".

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
              MOVE "CON904"                 TO MSJ3-HTML
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
              MOVE "CON904"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-RESTRICCION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                TO MSJ1-HTML
              MOVE NOM-REST-W              TO MSJ2-HTML
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
           MOVE COBW3-GET-DATA    TO LINEA-LLEGAD-W.

           UNSTRING LINEA-LLEGAD-W DELIMITED BY "|"
              INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, ADMIN-LLEGADA-W, OPC-SEGU-LLEGADA-W
           END-UNSTRING.

           MOVE LLAVESESION-LLEGA-W    TO LLAVE-SESION-W.
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
           INITIALIZE NOM-REST-W

           MOVE "\\" TO NOM-REST-W
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

           INSPECT NOM-REST-W REPLACING FIRST "                    "
                                        BY IP-DATOS-USUNET

           INSPECT NOM-REST-W REPLACING FIRST " "
                                        BY "\"

           INSPECT NOM-REST-W REPLACING FIRST "                         "
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
                         
           READ ARCHIVO-RESTRICCION WITH NO LOCK
                INVALID KEY     
                  MOVE 01                    TO MSJ1-HTML
                  MOVE "OPERARIO NO EXISTE"  TO MSJ2-HTML
                  MOVE "CON904"              TO MSJ3-HTML
                  GO TO ENVIAR2-ERROR
                NOT INVALID KEY 
                  MOVE NOMBRE-REST           TO NOMBRE-OPER
           END-READ.
           
           MOVE ADMIN-W            TO COD-REST.
           MOVE COD-W              TO OPC-REST.
           MOVE 00                 TO INVALID-W.

           READ ARCHIVO-RESTRICCION WITH NO LOCK
                INVALID KEY
                  GO TO ENVIAR-DATOS
                NOT INVALID KEY
                  MOVE 01        TO INVALID-W
           END-READ.

       ENVIAR-DATOS.
           MOVE INVALID-W          TO INVALID-ENV
           MOVE NOMBRE-OPER        TO NOMBRE-OPER-ENV
           MOVE COD-REST           TO COD-REST-ENV
           MOVE COD-W              TO COD-W-ENV.

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
008040 COPY "P:\PROG\PROYECT\FUENTES\SC-WEB.CBL".
