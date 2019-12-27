000001*=================================================================
      * MUESTRA EL NOMBRE DEL OPERADOR
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
      *"\PROG\DATOS\SC-ARCHREST.SC"
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
       
       77 NOM-RESTR-W                  PIC X(70).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(8).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 ADMIN-LLEGADA-W           PIC X(5).

       01 NOMBRE-OPER-W.
          02 NOMBRE1-OPER-W            PIC X(28).
          02 SUC-OPER-W                PIC X(2).
       01 IDENT-OPER-W                 PIC 9(10).

       01 LINEA-LLEGADA-W              PIC X(100).

       01 OPER-W                       PIC X(4).

       01 DATOS-ENVIO.
          02 NOMBRE-OPER-ENV.
             05 NOMBRE1-OPER-ENV       PIC X(28).
             05 SUC-OPER-ENV           PIC X(2).
          02 FILLER                    PIC X VALUE "|".
          02 IDENT-OPER-ENV            PIC X(10).

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
              MOVE "CON003"                 TO MSJ3-HTML
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
              MOVE "CON003"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-RESTRICCION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-RESTR-W                TO MSJ2-HTML
              MOVE "CON003"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       END DECLARATIVES.

       INICIAR-IIS.
006750     MOVE LOW-VALUE TO COBW3.
006760     MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
006770     CALL "COBW3_INIT" USING COBW3.
006780
006790 LEER-DATO-HTML.
006800     MOVE "datosh" TO COBW3-SEARCH-DATA.
006810     CALL "COBW3_GET_VALUE" USING COBW3.
006820     MOVE COBW3-GET-DATA    TO LINEA-LLEGADA-W.

           UNSTRING LINEA-LLEGADA-W DELIMITED BY "|"
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, ADMIN-LLEGADA-W
           END-UNSTRING.

001855     MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.

000797 INICIAR-SESION.
000806     MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.
006840
006900 ABRIR-USUARIO.
006910     INITIALIZE OTR-STAT.
006920     MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
006930
006940     OPEN INPUT ARCHIVO-USUNET
006950     EVALUATE OTR-STAT
006960       WHEN "00"  CONTINUE
006970       WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
006980                  INITIALIZE REG-USUNET
006990                  WRITE REG-USUNET
007000                  CLOSE ARCHIVO-USUNET
007010                  MOVE "Validacion de Usuario"       TO MENSAJE1-HTML
007020                  MOVE "Falta configurar usuarios"   TO MENSAJE2-HTML
007030                  MOVE "Sc"                          TO MENSAJE3-HTML
007040                  GO TO ENVIAR2-ERROR
007050       WHEN OTHER GO TO CERRAR-SESION
007060     END-EVALUATE.
007070
007080     INITIALIZE LLAVE-USUNET.

007100
007110 LEER-USUARIO.
007120     READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
007140     CLOSE ARCHIVO-USUNET.
007200     
007210     IF NOMBRE-USUNET = SPACES
007230        MOVE "Validacion de Sesion"        TO MENSAJE1-HTML
007240        MOVE "Falta configurar usuarios"   TO MENSAJE2-HTML
007250        MOVE "Sc"                          TO MENSAJE3-HTML
007260        GO TO ENVIAR2-ERROR
007270     END-IF.


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

           MOVE ADMIN-LLEGADA-W TO OPER-W.

       LEER-OPERADOR.

           OPEN INPUT ARCHIVO-RESTRICCION.

           INITIALIZE REG-REST.
           IF OPER-W = "GEBC"
              MOVE "Prosoft E.A.T"  TO NOMBRE-REST
              MOVE 0017329215       TO ID-REST
            ELSE
              IF OPER-W = SPACES
                 MOVE SPACES        TO NOMBRE-REST
                 MOVE 0             TO ID-REST
              ELSE
                 MOVE OPER-W        TO COD-REST
                 READ ARCHIVO-RESTRICCION INVALID KEY
                 MOVE COD-REST      TO NOMBRE-REST
              END-READ
           END-IF.

      *     MOVE NOMBRE-REST        TO 3.
           INSPECT ID-REST REPLACING ALL " " BY "0".
           MOVE ID-REST             TO IDENT-OPER-W.
           CLOSE ARCHIVO-RESTRICCION.

       ENVIAR-DATOS.
           IF NOMBRE-OPER-W = SPACES OR LOW-VALUES
                 MOVE SPACES        TO NOMBRE-OPER-ENV
                 MOVE SPACES        TO SUC-OPER-ENV
                 MOVE IDENT-OPER-W  TO IDENT-OPER-ENV
           ELSE  
              MOVE NOMBRE-OPER-W    TO NOMBRE-OPER-ENV
              MOVE IDENT-OPER-W     TO IDENT-OPER-ENV
           END-IF.

       PAGINA-CONFIG.
010720     MOVE "datosrecibidos"    TO COBW3-CNV-NAME
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