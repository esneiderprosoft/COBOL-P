004820*=================================================================
004830* FACTURACION - GENERA FACTURAS MENU PRINCIPAL.
      * ALMACEN SIN99 NO CONTABILIZA NI DESCARGA INVENTARIOS.
      * 17355476  FERRETERIA ALEXANDER SAN MARTIN
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "..\..\FUENTES\FS-USUNET.CBL".
004920     COPY "..\..\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-TARIFAS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TARIF-W 
      * "../CONTROL/SC-ARCHTAR.DAT"
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS COD-TAR.

           SELECT ARCHIVO-CONT-CAP LOCK MODE IS AUTOMATIC   
                  ASSIGN NOM-CONT-W
      * "../control/SC-CONTR.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CONT-CNCAP
                  ALTERNATE RECORD KEY IS NIT-CNCAP WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.
       
           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-USUAR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-TARIF.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-CNCAP.CBL".	   

005190 WORKING-STORAGE SECTION.
005200 COPY "..\..\FUENTES\COBW3.CBL".
005210 COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-USUAR-W                   PIC X(70).
       77 NOM-TARIF-W                   PIC X(70).
       77 MES-SAL-W                     PIC X(4).
       77 NOM-CONT-W                    PIC X(70).

       01 VARIABLES.
          02  MONTO-CONT-W              PIC S9(10)V99.
          02  PORCENT-CONT-W            PIC S9(5)V9(4).
          02  SALDO-CONT-W              PIC S9(11)V99.

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W             PIC X(15).
             03 FECHA-LLEGAD-W          PIC X(8).
             03 HORA-LLEGAD-W           PIC X(6).
          02 DIR-CONTAB-LLEGADA-W       PIC X(8).
          02 MES-CONTAB-LLEGADA-W       PIC XX.
                                      
       01 LINEA-LLEGADA-W               PIC X(100).
       
       01 DATOS-ENVIO.
          02 ESTADO-ENV                 PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 MENSAJE-ENV                PIC X(7).
          02 FILLER                     PIC X VALUE "|".
          02 COD-TAR-ENV                PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 PORCENT-CONT-ENV           PIC S9(5)V9(4).


006310 LINKAGE SECTION.
006320 COPY "..\..\FUENTES\ISAPICTX.CBL".
006330 PROCEDURE DIVISION WITH stdcall LINKAGE USING ISAPI-CTX-CNT.
006340
006350 DECLARATIVES.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUNET.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USU-W                TO MSJ2-HTML
              MOVE "INV401"                 TO MSJ3-HTML
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
              MOVE "INV401"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TARIFAS.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-TARIF-W              TO MSJ2-HTML
              MOVE "INV401"                 TO MSJ3-HTML
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
              MOVE "INV401"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CONT-CAP.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-CONT-W           TO MSJ2-HTML
              MOVE "INV401"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.


006720  END DECLARATIVES.
006730
006740 INICIAR-IIS.
006750     MOVE LOW-VALUE TO COBW3.
006760     MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
006770     CALL "COBW3_INIT" USING COBW3.
006780
006790 LEER-DATO-HTML.
006800     MOVE "datosh" TO COBW3-SEARCH-DATA.
006810     CALL "COBW3_GET_VALUE" USING COBW3.
006820     MOVE COBW3-GET-DATA    TO LINEA-LLEGADA-W.

           UNSTRING LINEA-LLEGADA-W DELIMITED BY "|"
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W
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
007280 
007350

        ASIGNAR-NOMBRES.
           EVALUATE MES-CONTAB-LLEGADA-W
             WHEN 01 MOVE "\ENE" TO MES-SAL-W 
             WHEN 02 MOVE "\FEB" TO MES-SAL-W 
             WHEN 03 MOVE "\MAR" TO MES-SAL-W 
             WHEN 04 MOVE "\ABR" TO MES-SAL-W 
             WHEN 05 MOVE "\MAY" TO MES-SAL-W 
             WHEN 06 MOVE "\JUN" TO MES-SAL-W 
             WHEN 07 MOVE "\JUL" TO MES-SAL-W 
             WHEN 08 MOVE "\AGT" TO MES-SAL-W 
             WHEN 09 MOVE "\SEP" TO MES-SAL-W 
             WHEN 10 MOVE "\OCT" TO MES-SAL-W 
             WHEN 11 MOVE "\NOV" TO MES-SAL-W 
             WHEN 12 MOVE "\DIC" TO MES-SAL-W 
           END-EVALUATE
           
    

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

001614     INITIALIZE NOM-TARIF-W
                      NOM-USUAR-W
                      NOM-CONT-W

           MOVE "\\" TO NOM-USUAR-W

           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

001621     INSPECT NOM-USUAR-W REPLACING FIRST " "
001624                                       BY "\"

007190     INSPECT NOM-USUAR-W REPLACING FIRST "        "
                                            BY DIR-CONTAB-LLEGADA-W

           MOVE NOM-USUAR-W TO NOM-TARIF-W NOM-CONT-W 

           INSPECT NOM-USUAR-W REPLACING FIRST "                       "
007200                                      BY "\CONTROL\SC-ARCHUSU.DAT".

           INSPECT NOM-TARIF-W REPLACING FIRST "                       "
007200                                       BY "\CONTROL\SC-ARCHTAR.DAT".

           INSPECT NOM-CONT-W REPLACING FIRST "                     "
007300                                       BY "\CONTROL\SC-CONTR.DAT".

      
        ABRIR-ARCHIVOS.
           OPEN INPUT ARCHIVO-TARIFAS.
           OPEN INPUT ARCHIVO-CONT-CAP.


        PROCESO.
           MOVE "CL"                TO COD-TAR
           READ ARCHIVO-TARIFAS INVALID KEY
                                MOVE "SO" TO COD-TAR
           END-READ

           READ ARCHIVO-CONT-CAP INVALID KEY
                                 MOVE 0 TO PORCENT-CONT-W
                                 NOT INVALID KEY
                                 IF PORC-AVISO-CNCAP IS ZERO
                                    MOVE 75     TO PORC-AVISO-CNCAP
                                 END-IF
                                 PERFORM SUMAR-CONTRATO
                                 COMPUTE MONTO-CONT-W = MONTO-CNCAP
                                                        + VLR-ADIC-CNCAP (1)
                                                        + VLR-ADIC-CNCAP (2)
                                                        + VLR-ADIC-CNCAP (3)
                                                        + VLR-ADIC-CNCAP (4)
                                 COMPUTE PORCENT-CONT-W = (SALDO-CONT-W / MONTO-CONT-W) * 300
           END-READ
           CLOSE ARCHIVO-CONT-CAP.


        SUMAR-CONTRATO.
           COMPUTE VLR-FAC-CNCAP (25) = VLR-FAC-CNCAP  (1)
                                       + VLR-FAC-CNCAP  (2)
                                       + VLR-FAC-CNCAP  (3)
                                       + VLR-FAC-CNCAP  (4)
                                       + VLR-FAC-CNCAP  (5)
                                       + VLR-FAC-CNCAP  (6)
                                       + VLR-FAC-CNCAP  (7)
                                       + VLR-FAC-CNCAP  (8)
                                       + VLR-FAC-CNCAP  (9)
                                       + VLR-FAC-CNCAP  (10)
                                       + VLR-FAC-CNCAP  (11)
                                       + VLR-FAC-CNCAP  (12)
                                       + VLR-FAC-CNCAP  (13)
                                       + VLR-FAC-CNCAP  (14)
                                       + VLR-FAC-CNCAP  (15)
                                       + VLR-FAC-CNCAP  (16)
                                       + VLR-FAC-CNCAP  (17)
                                       + VLR-FAC-CNCAP  (18)
                                       + VLR-FAC-CNCAP  (19)
                                       + VLR-FAC-CNCAP  (20)
                                       + VLR-FAC-CNCAP  (21)
                                       + VLR-FAC-CNCAP  (22)
                                       + VLR-FAC-CNCAP  (23)
                                       + VLR-FAC-CNCAP  (24).

           MOVE VLR-FAC-CNCAP (25) TO SALDO-CONT-W.

        ENVIAR-DATOS.

           MOVE "00"                TO ESTADO-ENV
           MOVE "EXITOSO"           TO MENSAJE-ENV
           MOVE COD-TAR             TO COD-TAR-ENV
           MOVE PORCENT-CONT-W      TO PORCENT-CONT-ENV.

010700  PAGINA-CONFIG.
010720     MOVE "datosrecibidos"    TO COBW3-CNV-NAME
010730     MOVE DATOS-ENVIO         TO COBW3-CNV-VALUE
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
005210* COPY "..\..\FUENTES\WS-HCEVO.CBL".
