004820*=================================================================
004830* CLONACION DE UNA FACTURA AUTOMATICA
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "..\..\FUENTES\FS-USUNET.CBL".
004920     COPY "..\..\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-NUMERACION LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-NUMER-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-NUM
                  ALTERNATE RECORD KEY LLAVE-ELE-NUM   WITH DUPLICATES
                  ALTERNATE RECORD KEY SUCUR-NUM       WITH DUPLICATES
                  ALTERNATE RECORD KEY NIT-NUM         WITH DUPLICATES
                  ALTERNATE RECORD KEY DESCRIP-NUM     WITH DUPLICATES
                  ALTERNATE RECORD KEY NOMBRE-PAC-NUM  WITH DUPLICATES
                  ALTERNATE RECORD KEY HAB-NUM         WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-ING-NUM   WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-RET-NUM   WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-NUMER19.CBL".

005190 WORKING-STORAGE SECTION.
005200 COPY "..\..\FUENTES\COBW3.CBL".
005210 COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LINEA-LLEGADA-W               PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W             PIC X(15).
             03 FECHA-LLEGAD-W          PIC X(8).
             03 HORA-LLEGAD-W           PIC X(6).
          02 DIR-CONTAB-LLEGADA-W       PIC X(30).
          02 MES-CONTAB-LLEGADA-W       PIC XX.
          02 OPERADOR-LLEGADA-W         PIC X(4).
          02 CTA-FACT-LLEGADA-W.
             03 PREFIJO-LLEGADA-W       PIC X.
             03 NRO-CTA-FACT            PIC X(6).
          02 PACIE-LLEGADA-W            PIC X(15).
          02 FECHA-COMP-LLEGADA-W.
             03 ANO-COMP-LLEGADA-W      PIC 9(4).
             03 MES-COMP-LLEGADA-W      PIC 99.
             03 DIA-COMP-LLEGADA-W      PIC 99.
          02 DETALLE-LLEGADA-W          PIC X(60).
          02 NRO-AUTOR-W                PIC X(15).
          02 NRO-OTROS-W                PIC X(6).

       77 MES-SAL-W                     PIC X(4).
       77 NOM-NUMER-W                   PIC X(70).
       77 ANO-ALFA                      PIC 9(4).
       77 CANT-W                        PIC 9(4).

       01 LLAVE-FACT-W.
           02 PREF-W                    PIC X.
           02 NRO-FAC-W                 PIC 9(6).
       
       01 REG-W.
          02 LLAVE-W.
             03 PREFIJO-W               PIC X.
             03 NRO-W                   PIC 9(6).
          02 LLAVE-ELE-W.
             03 PREFIJO-ELE-W           PIC XX.
             03 NRO-ELE-W               PIC 9(7).
          02 SUCUR-W                    PIC X(2).
          02 CONVENIO-W                 PIC XX.
          02 ESTADO-W                   PIC X.
          02 NIT-W                      PIC 9(10).
          02 DESCRIP-W.
             03 DESCRIP1-W              PIC X(30).
             03 DESCRIP2-W              PIC X(10).
          02 DATOS-PAC-W.
             03 NOMBRE-PAC-W.
                05 NOMBRE1-W.
                   07 INIC-NOM-W        PIC X(5).
                   07 REST-NOM-W        PIC X(25).
                05 NOMBRE2-W            PIC X(10).
             03 ID-PAC-W                PIC X(15).
             03 HAB-W                   PIC X(4).
             03 CLASIF-W                PIC 9.
             03 FECHA-ING-W.
                05 LLAVE-MES-ING-W.
                   07 ANO-ING-W         PIC 9(4).
                   07 MES-ING-W         PIC 99.
                05 DIA-ING-W            PIC 99.
             03 FECHA-RET-W.
                05 LLAVE-MES-RET-W.
                   07 ANO-RET-W         PIC 9(4).
                   07 MES-RET-W         PIC 99.
                05 DIA-RET-W            PIC 99.
             03 FECHA-PRE-W.
                05 ANO-PRE-W            PIC 9(4).
                05 MES-PRE-W            PIC 99.
                05 DIA-PRE-W            PIC 99.
             03 OBSERV-W.
                05 OBSERV1-W.
                   07 OBSERV1A-W        PIC X(40).
                   07 OBSERV1B-W        PIC X(20).
                05 OBSERV2-W.
                   07 OBSERV2A-W        PIC X(40).
                   07 OBSERV2B-W        PIC X(20).
                05 OBSERV3-W.
                   07 OBSERV3A-W        PIC X(40).
                   07 OBSERV3B-W        PIC X(20).
                05 OBSERV4-W.
                   07 OBSERV4A-W        PIC X(40).
                   07 OBSERV4B-W        PIC X(20).
             03 ANEXOS-W.
                05 ANEXO1-W.
                   07 ANEXO1A-W         PIC X(40).
                   07 ANEXO1B-W         PIC X(20).
                05 ANEXO2-W.
                   07 ANEXO2A-W         PIC X(40).
                   07 ANEXO2B-W         PIC X(20).
                05 ANEXO3-W.
                   07 ANEXO3A-W         PIC X(40).
                   07 ANEXO3B-W         PIC X(20).
             03 DATOS-ESTAD-W.
                05 NRO-HIST-W           PIC X(16).
                05 RADICACION-W         PIC 9(5).
                05 EST-COB-W            PIC 99.
                05 C-COSTO-W.
                   07 C-COSTO1-W        PIC X.
                   07 C-COSTO2-W        PIC X.
                   07 C-COSTO4-W        PIC X(2).
                05 SERVICIO-W           PIC XX.
                05 DIVISION-W           PIC XX.
                05 HORA-ING-W.
                   07 HR-ING-W          PIC 99.
                   07 MN-ING-W          PIC 99.
                05 DIAS-EST-W           PIC 9(3).
                05 DIAG-ING-W           PIC X(4).
                05 DIAG-SAL-W           PIC X(4).
                05 ESTAD-SAL-W          PIC 9.
                05 REMITE-W             PIC X(30).
                05 CIU-REMITE-W         PIC 9(5).
                05 DIAG-MUER-W          PIC X(4).
                05 HORA-RET-W.
                   07 HR-RET-W          PIC 99.
                   07 MN-RET-W          PIC 99.
             03 CTL-NROPACI-W           PIC X.
             03 FILLER                  PIC X.
             03 PER-CONT-RAD-W.
                05 ANO-CONT-RAD-W       PIC 99.
                05 MES-CONT-RAD-W       PIC 99.
             03 CTA-PIC-W               PIC 9(5).
             03 DIV-W                   PIC XX.
             03 FORMA-COPAG-W           PIC 9.
             03 RED-EXTER-W             PIC X.
             03 TIPO-EVENTO-W           PIC 99.
             03 BOL-SAL-W.
                05 OPER-BOL-W           PIC X(4).
                05 FECH-BOL-W.
                   07 ANO-BOL-W         PIC 9(4).
                   07 MES-BOL-W         PIC 99.
                   07 DIA-BOL-W         PIC 99.
                05 HORA-BOL-W.
                   07 HR-BOL-W          PIC X(2).
                   07 MN-BOL-W          PIC X(2).
             03 FARMA-W.
                05 FECHA-FARMA-W.
                   07 ANO-FARMA-W       PIC 9(4).
                   07 MES-FARMA-W       PIC 99.
                   07 DIA-FARMA-W       PIC 99.
                05 HORA-FARMA-W.
                   07 HR-FARMA-W        PIC 99.
                   07 MN-FARMA-W        PIC 99.
                05 OPER-FARMA-W         PIC X(4).
             03 TIPO-PACI-W             PIC X.
             03 CONTRATO-ANT-W          PIC X(9).
             03 CONTROL-CAP-W           PIC 9(4).
             03 FACT-CAPIT-W.
                05 PRE-CAPIT-W          PIC X.
                05 NRO-CAPIT-W          PIC 9(6).
             03 FILLER                  PIC X(4).
             03 GLOSA-W                 PIC 9(9).
             03 RESP-GLOSA-W            PIC 9(9).
             03 GLOSA-DISC-W            PIC S9(9).
             03 TRASL-GLOSA-W           PIC 9(9).
             03 CARTA-GLO-W             PIC X.
             03 ENTRA-REMIT-W           PIC X.
             03 ORIG-REMIT-W            PIC X(12).
             03 TIPO-FAC-W              PIC 9.
             03 FILLER                  PIC X(4).
             03 PORC-RETENC-W           PIC 99.
             03 ENVIO-W                 PIC 9(6).
             03 SEG-RIPS-W              PIC X.
             03 DETALLE-W               PIC X(50).
             03 DET-CARTERA-W           PIC X(50).
             03 COPAGO-EST-W            PIC 9(9).
             03 PORCENT-COPAGO-W        PIC 99V99.
             03 CO-PAGOS-W              PIC 9(9).
             03 ENVIO-FURIPS-W          PIC 9(6).
             03 FECHA-GLOSA-W.
                05 ANO-GLOSA-W          PIC 9(4).
                05 MES-GLOSA-W          PIC 9(2).
                05 DIA-GLOSA-W          PIC 9(2).
             03 CONTRATO-W              PIC X(20).
             03 CIS-W                   PIC X.
             03 NRO-AUTORI-W            PIC X(15).
             03 FECHA-REPRE-W.
                05 ANO-REPRE-W          PIC 9(4).
                05 MES-REPRE-W          PIC 99.
                05 DIA-REPRE-W          PIC 99.
             03 MYT-W                   PIC X.
             03 CIUDAD-W                PIC 9(5).
             03 FECHA-RADGLO-W.
                05 ANO-RADGLO-W         PIC 9(4).
                05 MES-RADGLO-W         PIC 99.
                05 DIA-RADGLO-W         PIC 99.
             03 RADICADO-EXTER-W        PIC X(15).
             03 CONTROL-X-SERV-W        PIC X.
             03 CONTROL-CL-0-W          PIC X.
             03 CONTROL-CL-1-W          PIC X.
             03 CONTROL-CL-2-W          PIC X.
             03 CONTROL-CL-3-W          PIC X.
             03 CONTROL-CL-4-W          PIC X.
             03 CONTROL-CL-5-W          PIC X.
             03 CONTROL-CL-6-W          PIC X.
             03 CONTROL-CL-7-W          PIC X.
             03 ART-IVA-W               PIC X.
             03 LLAVE-SALID-W.
                05 PER-SALID-W          PIC 9(4).
                05 NRO-SALID-W          PIC 9(6).
             03 FILLER                  PIC X(232).
             03 LEVANT-GLOSA-W          PIC X(10).
             03 VLR-LEV-GLOSA-W         PIC S9(12)V99.
             03 FECHA-LEV-GLOSA-W.
                05 ANO-LEV-GLOSA-W      PIC 9(4).
                05 MES-LEV-GLOSA-W      PIC 99.
                05 DIA-LEV-GLOSA-W      PIC 99.
             03 HORA-FACT-ARMA-W.
                05 HR-FACT-ARMA-W       PIC 99.
                05 MN-FACT-ARMA-W       PIC 99.
             03 DOCUM-FACT-ARMA-W       PIC 9(6).
             03 FECHA-FACT-ARMA-W.
                05 ANO-FACT-ARMA-W      PIC 9(4).
                05 MES-FACT-ARMA-W      PIC 9(2).
                05 DIA-FACT-ARMA-W      PIC 9(2).
             03 DOCUM-ARMA-FACT-W       PIC 9(6).
             03 FECHA-ARMA-FACT-W.
                05 ANO-ARMA-FACT-W      PIC 9(4).
                05 MES-ARMA-FACT-W      PIC 9(2).
                05 DIA-ARMA-FACT-W      PIC 9(2).
             03 DOCUM-ARMA-RADI-W       PIC 9(6).
             03 FECHA-ARMA-RADI-W.
                05 ANO-ARMA-RADI-W      PIC 9(4).
                05 MES-ARMA-RADI-W      PIC 9(2).
                05 DIA-ARMA-RADI-W      PIC 9(2).
             03 DOCUM-RADI-ARMA-W       PIC 9(6).
             03 FECHA-RADI-ARMA-W.
                05 ANO-RADI-ARMA-W      PIC 9(4).
                05 MES-RADI-ARMA-W      PIC 9(2).
                05 DIA-RADI-ARMA-W      PIC 9(2).
             03 NRO-POL-W               PIC X(30).
             03 FILLER                  PIC X(300).
             03 COPAGO-TIPO1-W          PIC 9(9).
             03 COPAGO-TIPO2-W          PIC 9(9).
             03 OPER-MOD-W              PIC X(4).
             03 FUNC-AUTOR-ING-W        PIC 9(10).
             03 OBSERV-CRE-W            PIC X(50).
             03 HORA-CRE-W.
                05 HR-CRE-W             PIC X(2).
                05 MN-CRE-W             PIC X(2).
             03 TABLA-W.
                05 TABLA-FACT-W   OCCURS 20.
                   07 FECHA-FACT-W.
                      09 ANO-FACT-W     PIC 9(4).
                      09 MES-FACT-W     PIC 99.
                   07 VLR-FACT-W        PIC S9(12)V99.
                05 TABLA-RBOS-W   OCCURS 50.
                   06 LLAVE-ABON-W.
                      07 FECHA-ABON-W.
                         09 ANO-ABON-W   PIC 9(4).
                         09 MES-ABON-W   PIC 99.
                         09 DIA-ABON-W   PIC 99.
                      07 SECU-ABON-W.
                         09 LOTE-ABON-W.
                            11 LOTE1-ABON-W   PIC X.
                            11 LOTE2-ABON-W   PIC X.
                         09 NRO-ABON-W        PIC 9(5).
                         09 SEC-ABON-W        PIC XX.
                   06 VLR-ABON-W              PIC S9(12)V99.
             03 DATO-OPER-W.
                05 OPER-W                PIC X(4).
                05 FECHA-CRE-W.
                   07 ANO-CRE-W          PIC 9(4).
                   07 MES-CRE-W          PIC 99.
                   07 DIA-CRE-W          PIC 99.
                05 FECHA-MOD-W.
                   07 ANO-MOD-W          PIC 9(4).
                   07 MES-MOD-W          PIC 99.
                   07 DIA-MOD-W          PIC 99.
                05 OPER-BLOQ-W           PIC X(4).
	  02 DATOS-W2.
             03 TABLA-W2 OCCURS 150.
                05 CONDIC1-W2          PIC X(50).
                05 CONDIC2-W2          PIC X(45).

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV             PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 LLAVE-REGRE-W              PIC X(7).
          02 FILLER                     PIC X VALUE "|".
          02 FECHA-CRE-ENV              PIC X(6).
          02 FILLER                     PIC X VALUE "|".
          02 NRO-ENV                    PIC X(6).

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
              MOVE "SER108DA"               TO MSJ3-HTML
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
              MOVE "SER108DA"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-NUMERACION.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-NUMER-W              TO MSJ2-HTML
              MOVE "SER108DA"               TO MSJ3-HTML
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
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, CTA-FACT-LLEGADA-W,
                    PACIE-LLEGADA-W, FECHA-COMP-LLEGADA-W, DETALLE-LLEGADA-W, NRO-AUTOR-W, NRO-OTROS-W
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
007010                  MOVE "Validacion de Usuario"       TO MSJ1-HTML
007020                  MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007030                  MOVE "Sc"                          TO MSJ3-HTML
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
007230        MOVE "Validacion de Sesion"        TO MSJ1-HTML
007240        MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007250        MOVE "Sc"                          TO MsJ3-HTML
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

001614     INITIALIZE NOM-NUMER-W
           MOVE "\\" TO NOM-NUMER-W

           INSPECT NOM-NUMER-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET
001621     INSPECT NOM-NUMER-W REPLACING FIRST " "
001624                                    BY "\"
007190     INSPECT NOM-NUMER-W REPLACING FIRST "                              "
                                          BY DIR-CONTAB-LLEGADA-W
           INSPECT NOM-NUMER-W REPLACING FIRST "                     "
007300                                      BY "\CONTROL\SC-NUMER.DAT".
        ABRIR-ARCHIVOS.
           OPEN I-O ARCHIVO-NUMERACION. 

           MOVE CTA-FACT-LLEGADA-W TO LLAVE-FACT-W

           IF ANO-COMP-LLEGADA-W > 90
              COMPUTE ANO-ALFA = ANO-COMP-LLEGADA-W + 1900
           ELSE
              COMPUTE ANO-ALFA = ANO-COMP-LLEGADA-W + 2000
           END-IF.

           MOVE 1 TO CANT-W.

        LEER-NUMERACION.
            MOVE LLAVE-FACT-W TO LLAVE-NUM
            READ ARCHIVO-NUMERACION WITH NO LOCK
                                    INVALID KEY
                                     MOVE 01 TO SW-INVALID-ENV
                                     GO TO ENVIAR-DATOS
            END-READ.

            MOVE REG-NUM             TO REG-W
            MOVE ANO-ALFA            TO ANO-ING-W
            MOVE MES-COMP-LLEGADA-W  TO MES-ING-W
            MOVE DIA-COMP-LLEGADA-W  TO DIA-ING-W
            MOVE PACIE-LLEGADA-W     TO ID-PAC-W
            MOVE DETALLE-LLEGADA-W   TO OBSERV1-W
            MOVE NRO-AUTOR-W         TO NRO-AUTORI-W
            INITIALIZE ANEXO1-W
            MOVE NRO-AUTOR-W  TO ANEXO1-W
            INITIALIZE TABLA-W COPAGO-EST-W

            PERFORM CREAR-NUMERACION VARYING I FROM 1 BY 1
                                        UNTIL I > CANT-W.

        ENVIAR-DATOS.
            MOVE FECHA-CRE-W TO FECHA-CRE-ENV
            MOVE NRO-W       TO NRO-ENV
            CLOSE ARCHIVO-NUMERACION.

010700  PAGINA-CONFIG.
010720      MOVE "datosrecibidos"    TO COBW3-CNV-NAME
010730      MOVE DATOS-ENVIO         TO COBW3-CNV-VALUE
010740      CALL "COBW3_SET_CNV" USING COBW3
010750
010760      MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
010770      PERFORM ABRIR-HTML.
007980   
007990 CERRAR-SESION.
008000      CALL "COBW3_FREE" USING COBW3.
008010      MOVE 1 TO PROGRAM-STATUS.
008020      EXIT PROGRAM.

008030
008040 COPY "..\..\FUENTES\SC-WEB.CBL".

       CREAR-NUMERACION.
           MOVE NRO-OTROS-W TO NRO-W
           MOVE OPERADOR-LLEGADA-W    TO OPER-W.
           ACCEPT FECHA-CRE-W FROM DATE.
           MOVE LLAVE-W TO LLAVE-REGRE-W.
           MOVE REG-W      TO REG-NUM.
           WRITE REG-NUM.
005210* COPY "..\..\FUENTES\WS-HCEVO.CBL".
