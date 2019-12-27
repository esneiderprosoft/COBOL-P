004820*=================================================================
004830* GUARDAR FACTURA SAL41 SALUD.
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "..\..\FUENTES\FS-USUNET.CBL".
004920     COPY "..\..\FUENTES\FS-SESION.CBL".

            SELECT FACTURA-SERVICIOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-SAL-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-FACT
                  ALTERNATE RECORD KEY NRO-FACT   WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-FACT WITH DUPLICATES
                  ALTERNATE RECORD KEY CTA-FACT   WITH DUPLICATES
                  ALTERNATE RECORD KEY NIT-FACT   WITH DUPLICATES
                  ALTERNATE RECORD KEY ID-HISTORIA-FACT WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-ING-ESTAD WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-SAL-ESTAD WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-ENTR-RX   WITH DUPLICATES				  
                  FILE STATUS IS OTR-STAT.

            SELECT ARCHIVO-TXT LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TXT-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.


005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-SALUD.CBL".	   

       FD  ARCHIVO-TXT
           LABEL RECORD STANDARD.
       01  REG-TXT.
           02 DATOS-TXT                             PIC X(500).



005190 WORKING-STORAGE SECTION.
005200 COPY "..\..\FUENTES\COBW3.CBL".
005210 COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LINEA-LLEGADA-W                           PIC X(500).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W                         PIC X(15).
             03 FECHA-LLEGAD-W                      PIC X(8).
             03 HORA-LLEGAD-W                       PIC X(6).
          02 DIR-CONTAB-LLEGADA-W                   PIC X(30).
          02 MES-CONTAB-LLEGADA-W                   PIC XX.
          02 OPERADOR-LLEGADA-W                     PIC X(4).
          02 NOMBRE-TXT-LLEGADA-W                   PIC X(28).

       01  REG-W.
           02 LLAVE-W.
              03 SUC-W                              PIC X(2).
              03 CL-W                               PIC X.
              03 NRO-W                              PIC X(6).
           02 FECHA-W.
              03 ANO-W                              PIC X(4).
              03 MES-W                              PIC XX.
              03 DIA-W                              PIC XX.
           02 CTA-W.
              03 PREFIJO-W                          PIC X.
              03 NRO-CTA-W                          PIC X(6).
           02 NIT-W                                 PIC X(10).
           02 ID-HISTORIA-W                         PIC X(15).
           02 FECHA-ING-W.
              03 ANO-ING-W                          PIC X(4).
              03 MES-ING-W                          PIC XX.
              03 DIA-ING-W                          PIC XX.
      * 57
           02 DATOS-W.
              03 DATOS-TABLA-W.
                 04 TABLA-W OCCURS 140.
                    05 ALM-W.
                       07 ALM3-W.
                          09 ALM1-W                 PIC X.
                          09 ALM2-W                 PIC XX.
                        07 ALM5-W                   PIC XX.
                     05 ART-W.
                        07 GRUPO-W.
                           09 GRUPO1-W              PIC X.
                           09 GRUPO2-W              PIC X.
                        07 ART-COMP-W.
                           09 COD-ART-W.
                              10 COD1-ART-W         PIC X.
                              10 COD2-ART-W         PIC X(12).
                           09 CLASE-ART-W           PIC XX.
                     05 COD-LOTE-W                  PIC X(4).
                     05 LOTE-MEDIPOW-W              PIC 9(9).
                     05 CANT-W                      PIC S9(12)V99.
                     05 VLR-W                       PIC S9(12)V99.
                     05 DIAS-TRATA-W                PIC 9(3).
                     05 TABLA-RESUL-W.
                        07 VLR-RESUL-W              PIC 9(3)V9.
                     05 TABLA-INTERP-W.
                        07 VLR-INTERP-W             PIC X.
                     05 TABLA-LATER-W.
                        07 VLR-LATER-W              PIC 9.
                     05 FILLER                      PIC X(20).
      * 12880
              03 VALOR-DES-W                        PIC 9(12)V99.
              03 TIPO-COPAGO-W                      PIC X.
              03 COPAGO-ESTIM-W                     PIC 9(9).
              03 VLR-IVA-W                          PIC S9(12)V99.
              03 MEDICOS-W.
                 05 MED-CIR-W                       PIC 9(10).
                 05 MED-AYU-W                       PIC 9(10).
                 05 MED-ANE-W                       PIC 9(10).
                 05 MED-INS-W                       PIC 9(10).
                 05 MED-OTR-W                       PIC 9(10).
              03 DATOS-CIR-W.
                 05 VIA-W                           PIC 99.
                 05 MULT-W                          PIC 99.
                 05 NRO-CIR-W                       PIC 99.
                 05 CRUENTA-W                       PIC X.
                 05 GRUPO-CIR-W                     PIC 99.
                 05 FILLER                          PIC X(20).
              03 OTROS-DATOS-W.
                 05 PUERTA-ESTAD-W                  PIC 9.
                 05 TARIF-W                         PIC XX.
                 05 COSTO-W.
                    07 COSTO1-W                     PIC XX.
                    07 COSTO2-W                     PIC XX.
                 05 ESPEC-ELAB-W.
                    07 ESPEC1-ELBA-W                PIC XX.
                    07 ESPEC2-ELAB-W                PIC X.
                 05 DETALLE-W                       PIC X(300).
                 05 CONTROL-CAP-ESTAD-W.
                    07 CONTROL1-CAP-ESTAD-W         PIC X.
                    07 CONTROL2-CAP-ESTAD-W         PIC X.
                 05 NRO-AUTOR-ELAB-W.
                    07 NRO1-AUTOR-ELAB-W.
                       09 PREF-AUTOR-ELAB-W         PIC X.
                       09 COMP-AUTOR-ELAB-W         PIC X(6).
                    07 RESTO-AUTOR-ELAB-W           PIC X(20).
                 05 CONTROL-MES-W                   PIC 9.
                 05 F-PAGO-W                        PIC 99.
                 05 BLOQUEO-IMP-W                   PIC 9.
                 05 NRO-FORM-W.
                    07 IND-FORM-W                   PIC 9(9).
                    07 NRO-PED-W                    PIC 9(7).
                 05 CAMA-RX-W                       PIC X(4).
                 05 OPER-ELAB-W.
                    07 OPER-ELAB1-W                 PIC X(3).
                    07 OPER-ELAB2-W                 PIC X.
                 05 FECHA-ELAB-W.
                    07 ANO-ELAB-W                   PIC 9(4).
                    07 MES-ELAB-W                   PIC 99.
                    07 DIA-ELAB-W                   PIC 99.
                 05 HORA-ELAB-W.
                    07 HR-ELAB-W                    PIC 99.
                    07 MN-ELAB-W                    PIC 99.
                 05 HORA-ATEN-ESTAD-W.
                    07 HR-ATEN-ESTAD-W              PIC 99.
                    07 MN-ATEN-ESTAD-W              PIC 99.
                 05 OPER-CORREC-W                   PIC X(4).
                 05 FECHA-CORREC-W.
                    07 ANO-CORREC-W                 PIC 9(4).
                    07 MES-CORREC-W                 PIC 99.
                    07 DIA-CORREC-W                 PIC 99.
                 05 HORA-CORREC-W.
                    07 HR-CORREC-W                  PIC 99.
                    07 MN-CORREC-W                  PIC 99.
           02 FILLER                                PIC X(1000).
      * 1516
           02 ESTAD-W.
              03 EDAD-ELAB-W.
                 05 UNID-EDAD-ELAB-W                PIC X.
                 05 VLR-EDAD-ELAB-W                 PIC 9(3).
              03 CAUSA-ESTAD-W                      PIC 99.
              03 EMBAR-ESTAD-W                      PIC X.
              03 PERSONAL-ELAB-W                    PIC X.
              03 CLASE-PROC-ESTAD-W                 PIC 9.
              03 TIPO-PROC-ESTAD-W                  PIC 9.
              03 COD-SERV-ESTAD-W                   PIC 9(6).
              03 FINALID-ESTAD-W                    PIC 99.
              03 TRIAGE-ESTAD-W                     PIC 9.
              03 TIP-DIAG-ESTAD-W                   PIC X.
              03 REPETID-ESTAD-W                    PIC X.
              03 TABLA-DIAG-ESTAD-W.
                 05 COD-DIAG-ESTAD-W OCCURS 10.
                    07 COD-DIAG1-ESTAD-W.
                       09 INI-DIAG-ESTAD-W.
                          11 COD1-DIAG1-ESTAD-W     PIC X.
                          11 COD2-DIAG1-ESTAD-W     PIC X.
                       09 RESTO-DIAG-ESTAD-W        PIC X.
                    07 COD-DIAG2-ESTAD-W            PIC X.
              03 HOR-OBSER-ESTAD-W                  PIC 9(3).
              03 DIAS-INC-ESTAD-W                   PIC 9(3).
              03 TIPO-DISCAP-ELAB-W                 PIC 9.
              03 GRADO-DISCAP-ELAB-W                PIC 9.
              03 ESTAD-SAL-ESTAD-W                  PIC X.
              03 FECHA-SAL-ESTAD-W.
                 05 ANO-SAL-ESTAD-W                 PIC 9(4).
                 05 MES-SAL-ESTAD-W                 PIC 99.
                 05 DIA-SAL-ESTAD-W                 PIC 99.
              03 HORA-SALID-ESTAD-W.
                 05 HR-SALID-ESTAD-W                PIC 99.
                 05 MN-SALID-ESTAD-W                PIC 99.
              03 DIAG-MUER-ESTAD-W.
                 05 COD-DIAG-MUE1-ESTAD-W           PIC X(3).
                 05 COD-DIAG-MUE2-ESTAD-W           PIC X.
              03 REMITE-W                           PIC 9(10).
              03 ESPEC-REMI-W                       PIC X(3).
              03 UNSERV-W.
                 05 UNSERV1-W                       PIC 9.
                 05 UNSERV2-W                       PIC 9.
              03 TIPO-DR-W                          PIC X.
              03 BIRADS-W                           PIC X.
              03 MACRO-W                            PIC 9.
              03 ULT-EXAMEN-ESTAD-W.
                 05 ANO-ULT-EXAMEN-W                PIC 9(4).
                 05 MES-ULT-EXAMEN-W                PIC 99.
                 05 DIA-ULT-EXAMEN-W                PIC 99.
              03 COMPLEJIDAD-W                      PIC 9.
              03 CUP-PAQ-INT-W                      PIC X(8).
              03 ORDSERV-W.
                 05 PREF-ORDSERV-W                  PIC XX.
                 05 CL-ORDSERV-W                    PIC X.
                 05 NRO-ORDSERV-W                   PIC 9(6).
              03 ENTREGA-RX-W.
                 05 FECHA-ENTR-RX-W.
                    07 ANO-ENTR-RX-W                PIC 9(4).
                    07 MES-ENTR-RX-W                PIC 99.
                    07 DIA-ENTR-RX-W                PIC 99.
                 05 HORA-ENTR-RX-W.
                    07 HR-ENTR-RX-W                 PIC 9(2).
                    07 MN-ENTR-RX-W                 PIC 9(2).
                 05 RECIBIDO-RX-W                   PIC X(50).
                 05 OPER-ENTR-RX-W                  PIC X(4).
              03 CRONICO-W                          PIC X(3).
              03 DESTINO-REW                        PIC 9(6).
              03 REWURA-W                           PIC 9(6).
              03 OPER-CORR-RIPS-W                   PIC X(4).
              03 FECHA-CORR-RIPS-W.
                 05 ANO-CORR-RIPS-W                 PIC 9(4).
                 05 MES-CORR-RIPS-W                 PIC 99.
                 05 DIA-CORR-RIPS-W                 PIC 99.
              03 RECIBIDO2-RX-W                     PIC X(50).
              03 MOT-BLOQUEO-RX-W                   PIC X(50).
              03 DATOS-4505-W.
                 05 SINTOM-RESPI-W                  PIC X.
                 05 1RA-VEZ-W                       PIC X.
                 05 PLANIFIC-W                      PIC X.
                 05 FECHA-PARTO-FUR-W.
                    07 ANO-PARTO-FUR-W              PIC 9(4).
                    07 MES-PARTO-FUR-W              PIC 9(2).
                    07 DIA-PARTO-FUR-W              PIC 9(2).
                 05 TALLA-W                         PIC 9(3).
                 05 PESO-W                          PIC 9(3)V9.
                 05 NRO-CONT-PREN-W                 PIC 99.
                 05 FLUOROSIS-W                     PIC X.
                 05 FILLER                          PIC X(20).
                 05 CALID-MUEST-W                   PIC X.
                 05 EDAD-GEST-W                     PIC 99.
                 05 SINTOM-PIEL-W                   PIC X.
                 05 VICTI-MALTRATO-W                PIC X.
                 05 VICTI-VIOLENCIA-W               PIC X.
                 05 ENFER-MENTAL-W                  PIC X.
                 05 FORM-AUTO-W                     PIC X.
                 05 DESPAC-FOR-AUTO-W               PIC X.
                 05 OPER-DESPAC-FOR-W               PIC X(4).
                 05 TENS-W.
                    10 TENS1-W                      PIC 9(3).
                    10 TENS2-W                      PIC 9(3).
              03 PENDIENTE-W                        PIC X.
              03 NRO-PENDI-W.
                 05 SUC-PENDI-W                     PIC XX.
                 05 CL-PENDI-W                      PIC 9.
                 05 NUM-PENDI-W                     PIC 9(6).
              03 FECHA-PENDI-W.
                 05 ANO-PENDI-W                     PIC 9(4).
                 05 MES-PENDI-W                     PIC 99.
                 05 DIA-PENDI-W                     PIC 99.
              03 HORA-PENDI-W.
                 05 HR-PENDI-W                      PIC 99.
                 05 MN-PENDI-W                      PIC 99.
              03 NRO-ENTREPENDI-W.
                 05 SUC-ENTREPENDI-W                PIC XX.
                 05 CL-ENTREPENDI-W                 PIC 9.
                 05 NUM-ENTREPENDI-W                PIC 9(6).
              03 FECHA-ENTREPENDI-W.
                 05 ANO-ENTREPENDI-W                PIC 9(4).
                 05 MES-ENTREPENDI-W                PIC 99.
                 05 DIA-ENTREPENDI-W                PIC 99.
              03 HORA-ENTREPENDI-W.
                 05 HR-ENTREPENDI-W                 PIC 99.
                 05 MN-ENTREPENDI-W                 PIC 99.
              03 CANT-PENDI-W                       PIC 9(3).
              03 TIPO-PENDI-W                       PIC 9.               
              03 W-AUTO-W                           PIC X.
              03 VLR-IVA1-W                         PIC S9(12)V99.
              03 VLR-IVA2-W                         PIC S9(12)V99.
              03 VLR-IVA3-W                         PIC S9(12)V99.
              03 FILLER                             PIC X(2000).
      * 2473
              03 FECHA-LIMI-AUTO-W.
                 05 ANO-LIMI-AUTO-W                 PIC 9(4).
                 05 MES-LIMI-AUTO-W                 PIC 99.
                 05 DIA-LIMI-AUTO-W                 PIC 99.
              03 ORD-SALIDA-W.
                 05 PER-SALIDA-W                    PIC 9(4).
                 05 NRO-SALIDA-W                    PIC 9(6).
              03 ORIG-RIPS-W                        PIC X(5).
           02 DATOS-CAPIT-W.
              03 UPC-W                              PIC S9(12)V99.
              03 TAB-USUAR-CAPIT-W OCCURS 10.
                 05 NRO-USUAR-CAPIT-W               PIC 9(8).
           02 CONDIC-W.               
              03 DATO-ACCID-RIPS-W.
                 05 FECHA-ACCID-RIPS-W.
                    07 ANO-ACCID-RIPS-W             PIC 9(4).
                    07 MES-ACCID-RIPS-W             PIC 99.
                    07 DIA-ACCID-RIPS-W             PIC 99.
                 05 EMPRESA-PACI-RIPS-W             PIC X(50).
                 05 FECHA-PRESEN-W.
                    07 ANO-PRES-W                   PIC 9(4).
                    07 MES-PRES-W                   PIC 99.
                    07 DIA-PRES-W                   PIC 99.
                 05 MED-TEC-W                       PIC 9(10).
                 05 MED2-RX-W                       PIC 9(10).
                 05 CIUDAD-W                        PIC 9(5).                  
                 05 OBSERV-RIPS-W                   PIC X.
                 05 MOTIVO-CONSUL-RIPS-W.
                    07 MOTIV1-CONSUL-RIPS-W         PIC X(95).
                    07 MOTIV2-CONSUL-RIPS-W         PIC X(95).
           02 CARTERA-SALUD-W.
              03 TABLA-ABON-SALUD-W OCCURS 10.
                 05 FECHA-ABON-SALUD-W.
                    07 ANO-ABON-SALUD-W             PIC 9(4).
                    07 MES-ABON-SALUD-W             PIC 99.
                 05 SECU-ABON-SALUD-W.
                    07 LOTE-ABON-SALUD-W            PIC XX.
                    07 COMP-ABON-SALUD-W            PIC X(6).
                    07 ITEM-ABON-SALUD-W            PIC X(3).
                 05 VLR-ABON-SALUD-W                PIC S9(12)V99.
      * 709
      * 17635 TOTAL
           02 FILLER                                PIC X(15125).
      * 32760 MAXIMO PERMITIDO

       77 MES-SAL-W                     PIC X(4).
       77 NOM-SAL-W                     PIC X(70).
       77 NOM-TXT-W                     PIC X(70).
       77 SW                            PIC 9(3).
       
       01 DATOS-ENVIO.                  
          02 SW-INVALID-ENV             PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 NRO-FACT-ENV               PIC X(6).
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
              MOVE "SAL41-15"               TO MSJ3-HTML
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
              MOVE "SAL41-15"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON FACTURA-SERVICIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-SAL-W                TO MSJ2-HTML
              MOVE "SAL41-15"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TXT.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-TXT-W                TO MSJ2-HTML
              MOVE "SAL41-15"               TO MSJ3-HTML
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

           INITIALIZE REG-W.

           UNSTRING LINEA-LLEGADA-W DELIMITED BY "|"
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W,
                    LLAVE-W, FECHA-W, CTA-W, NIT-W, ID-HISTORIA-W, FECHA-ING-W, VALOR-DES-W,
                    TIPO-COPAGO-W, COPAGO-ESTIM-W, VLR-IVA-W, MED-CIR-W, MED-AYU-W, MED-ANE-W, MED-INS-W,
                    MED-OTR-W, VIA-W, MULT-W, NRO-CIR-W, CRUENTA-W, PUERTA-ESTAD-W, TARIF-W, COSTO-W,
                    ESPEC-ELAB-W, DETALLE-W, CONTROL-CAP-ESTAD-W, NRO-AUTOR-ELAB-W, F-PAGO-W, NRO-FORM-W,
                    CAMA-RX-W, OPER-ELAB-W, FECHA-ELAB-W, HORA-ELAB-W, HORA-ATEN-ESTAD-W, UNID-EDAD-ELAB-W,
                    VLR-EDAD-ELAB-W, CAUSA-ESTAD-W, EMBAR-ESTAD-W, PERSONAL-ELAB-W, CLASE-PROC-ESTAD-W,
                    TIPO-PROC-ESTAD-W, FINALID-ESTAD-W, FECHA-SAL-ESTAD-W, HORA-SALID-ESTAD-W, REMITE-W,
                    ESPEC-REMI-W, UNSERV-W, TIPO-DR-W, MACRO-W, CUP-PAQ-INT-W, ORDSERV-W, RECIBIDO-RX-W,
                    CRONICO-W, PENDIENTE-W, NRO-PENDI-W, FECHA-PENDI-W, HORA-PENDI-W, CANT-PENDI-W,
                    TIPO-PENDI-W, W-AUTO-W, VLR-IVA1-W, VLR-IVA2-W, VLR-IVA3-W, EMPRESA-PACI-RIPS-W,
                    NOMBRE-TXT-LLEGADA-W
            END-UNSTRING.

001855     MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.

000797 INICIAR-SESION.
000806     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.
006840
006900 ABRIR-USUARIO.
006910     INITIALIZE OTR-STAT.
006920     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
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
           END-EVALUATE.
           
        ASIGNAR-NOMBRES. 
           INITIALIZE NOM-SAL-W
                      NOM-TXT-W

           MOVE "\\" TO NOM-SAL-W 

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-SAL-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET
           INSPECT NOM-SAL-W REPLACING FIRST " "
                                          BY "\"
           INSPECT NOM-SAL-W REPLACING FIRST "                              "
                                          BY DIR-CONTAB-LLEGADA-W.
           INSPECT NOM-SAL-W REPLACING FIRST "                     "
                                          BY "\CONTROL\SC-FACSA.DAT".

           MOVE NOM-PLANO-JSON-W TO NOM-TXT-W

           INSPECT NOM-TXT-W REPLACING FIRST "                            "
                                          BY NOMBRE-TXT-LLEGADA-W.

           INSPECT NOM-TXT-W REPLACING FIRST "    "
                                          BY ".TXT".

       ABRIR-ARCHIVOS.
           OPEN INPUT FACTURA-SERVICIOS
                      ARCHIVO-TXT.

           MOVE 1 TO SW.

       LEER-TXT.
           READ ARCHIVO-TXT NEXT WITH NO LOCK AT END GO TO LEER-ARCHIVO.

           UNSTRING DATOS-TXT DELIMITED BY "|"
                    INTO ALM-W (SW), ART-W (SW), COD-LOTE-W (SW), CANT-W (SW),
                         VLR-W (SW), DIAS-TRATA-W (SW), VLR-LATER-W (SW)
           END-UNSTRING

           ADD 1 TO SW

           GO TO LEER-TXT.

       LEER-ARCHIVO.
           MOVE LLAVE-W   TO LLAVE-FACT
           READ FACTURA-SERVICIOS WITH NO LOCK
                INVALID KEY 
                  MOVE "01" TO SW-INVALID-ENV
                  CLOSE FACTURA-SERVICIOS
                  OPEN I-O FACTURA-SERVICIOS
                  INITIALIZE REG-FACT
                  MOVE REG-W TO REG-FACT
                  WRITE REG-FACT END-WRITE
                NOT INVALID KEY
                  MOVE "00" TO SW-INVALID-ENV
           END-READ.
           
           MOVE NRO-FACT     TO NRO-FACT-ENV
           MOVE NRO-W        TO NRO-ENV.
           
        CERRAR-ARCHIVOS.
           CLOSE FACTURA-SERVICIOS
                 ARCHIVO-TXT.

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
008040 COPY "..\..\FUENTES\SC-WEB.CBL".
005210* COPY "..\..\FUENTES\WS-HCEVO.CBL".
