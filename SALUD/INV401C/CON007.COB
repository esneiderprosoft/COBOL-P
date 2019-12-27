000001*=================================================================
      * CONTABILIDAD - BUSCA EL ULTIMO NUMERO DE UNA SECUENCIA.
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

            SELECT ARCHIVO-LOTES LOCK MODE IS AUTOMATIC
                    ASSIGN NOM-LOTES-W
      * "\PROG\DATOS\SC-ARCHLOTE.DAT";
                    ORGANIZATION IS INDEXED;
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS COD-LOTE
                    ALTERNATE RECORD KEY IS NOMBRE-LOTE WITH DUPLICATES
                    FILE STATUS IS OTR-STAT.

             SELECT MOVIMIENTO-DIARIO LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-MOV
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
       COPY "..\..\FUENTES\FD-LOTES.CBL".
       COPY "..\..\FUENTES\FD-MOVIM.CBL".
       
       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".
       
       77 NOM-LOTES-W                  PIC X(70).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(8).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 SECU-LLEGADA-W            PIC XX.
          02 COMP-LLEGADA-W            PIC X(6).

       01 NOMBRE-OPER-W.
          02 NOMBRE1-OPER-W            PIC X(28).
          02 SUC-OPER-W                PIC X(2).
       01 IDENT-OPER-W                 PIC 9(10).

       01 LINEA-LLEGADA-W              PIC X(100).

       01 SECU-W                       PIC X(4).
       01 COMP-W                       PIC 9(6).
       01 COMP-ANT                     PIC X(6).
       01 SECU-ANT                     PIC XX.

       01 ULT-FECHA-W.
          02 ULT-ANO-W                 PIC 99.
          02 ULT-MES-W                 PIC 99.
          02 ULT-DIA-W                 PIC 99.

       01 DATOS-ENVIO.
          02 OTR-STAT-ENV              PIC X(2).
          02 FILLER                    PIC X VALUE "|".
          02 COMP-ENV                  PIC X(6).
          02 FILLER                    PIC X VALUE "|".
          02 SECU-ENV                  PIC X(2).
          02 FILLER                    PIC X VALUE "|".
          02 ULT-FECHA-ENV             PIC X(6).

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
              MOVE "CON007"                 TO MSJ3-HTML
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
              MOVE "CON007"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-LOTES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-LOTES-W              TO MSJ2-HTML
              MOVE "CON007"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MOVIMIENTO-DIARIO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-MOV                  TO MSJ2-HTML
              MOVE "CON007"                 TO MSJ3-HTML
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
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, SECU-LLEGADA-W, COMP-LLEGADA-W
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
           INITIALIZE NOM-LOTES-W NOM-MOV.

           MOVE "\\" TO NOM-LOTES-W

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

           INSPECT NOM-LOTES-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET
           INSPECT NOM-LOTES-W REPLACING FIRST " "
                                            BY "\"
           INSPECT NOM-LOTES-W REPLACING FIRST "                          "
                                            BY "PROG\DATOS\SC-ARCHLOTE.DAT".
           MOVE SECU-LLEGADA-W   TO SECU-W
           MOVE COMP-LLEGADA-W   TO COMP-W.

       ABRIR-ARCHIVO-LOTES.
           OPEN INPUT ARCHIVO-LOTES
           MOVE SECU-W  TO COD-LOTE
           READ ARCHIVO-LOTES INVALID KEY
                MOVE 1 TO CONSEC-LOTE
           END-READ
           CLOSE ARCHIVO-LOTES.

           IF CONSEC-LOTE = 1
              MOVE NOM-LOTES-W               TO NOM-MOV
              INSPECT NOM-LOTES-W REPLACING FIRST "SC-ARCHLOTE.DAT"
                                               BY "ARCHMOV.DAT    "
           ELSE
              MOVE "ARCHMOV.DAT"             TO NOM-MOV
           END-IF.

           OPEN INPUT MOVIMIENTO-DIARIO.

       LEER-NUMERACION.
           INITIALIZE MOV-DIARIO.
           MOVE "9999999" TO LLAVE-COMP-MOV.
           MOVE SECU-W    TO SECU-MOV.
           READ MOVIMIENTO-DIARIO RECORD KEY IS SECUENCIA-MOV
                INVALID KEY  CLOSE    MOVIMIENTO-DIARIO
                             OPEN I-O MOVIMIENTO-DIARIO
                             INITIALIZE COMP-ANT
                             MOVE "ULTIMO COMPROBANTE"
                                                  TO DETALLE-MOV
                             MOVE COMP-ANT        TO REFER-MOV
                             WRITE MOV-DIARIO END-WRITE.

           IF REF1-MOV IS NOT NUMERIC MOVE 0 TO REF1-MOV.
           INSPECT REFER-MOV REPLACING ALL " " BY "0".
           MOVE FECHA-VENCE-MOV TO ULT-FECHA-W.
           MOVE REFER-MOV       TO COMP-W.
           ADD  1               TO COMP-W.

           MOVE COMP-W          TO COMP-ENV
           MOVE ULT-FECHA-W     TO ULT-FECHA-ENV
           MOVE SECU-MOV        TO SECU-ENV
           MOVE "00"            TO OTR-STAT-ENV.

       CERRAR-ARCHIVOS.
            CLOSE MOVIMIENTO-DIARIO. 


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