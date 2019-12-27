004820*=================================================================
004830* GUARDA APLICACION DE MEDICAMENTOS //  5-1
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
004920     COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".
004930
005020

           SELECT ARCHIVO-USUARIOS
                  ASSIGN NOM-USUAR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU 
                  ALTERNATE RECORD KEY IS		
                  CLAVE-USU WITH DUPLICATES
                  ALTERNATE RECORD KEY IS
                  CTL-USU WITH DUPLICATES
                  ALTERNATE RECORD KEY IS
                  CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-RESTRICCION
                  ASSIGN NOM-REST-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY LLAVE-REST
                  FILE STATUS OTR-STAT.


           SELECT ARCHIVO-PROFESIONALES
                  ASSIGN NOM-PROF-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PROF   
                  ALTERNATE RECORD KEY
                  DESCRIP-PROF WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.



           SELECT ARCHIVO-PACIENTES
                  ASSIGN  NOM-PACIE-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PACI
                  ALTERNATE RECORD KEY  
                  DESCRIP-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  
                  EPS-PACI  WITH DUPLICATES
                  ALTERNATE RECORD KEY  
                  ID-COTIZ-PACI WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT HISTORIA-CLINICA LOCK MODE IS AUTOMATIC
1                  ASSIGN NOM-HCAPE-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-HC
                  ALTERNATE RECORD KEY IS FOLIO-HC WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FECHA-HC WITH DUPLICATES
                  ALTERNATE RECORD KEY IS MED-HC WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ESTADO-HC WITH DUPLICATES
                  FILE STATUS OTR-STAT.


           SELECT EVOLUCION-HISTORIA
                  ASSIGN NOM-HCEVO-W LOCK MODE IS AUTOMATIC
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-EVO
                  ALTERNATE RECORD KEY IS FOLIO-EVO WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FECHA-EVO WITH DUPLICATES
                  ALTERNATE RECORD KEY IS MED-EVO WITH DUPLICATES
                  FILE STATUS OTR-STAT.


           SELECT EVOLUCION-DETALLADA LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-DETEVO-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-DEVO
                  ALTERNATE RECORD KEY IS FOLIO-DEVO WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FECHA-DEVO WITH DUPLICATES
                  FILE STATUS OTR-STAT.



           SELECT ARCHIVO-INTERCONSULTA
                  ASSIGN NOM-INTCONS-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-INTCONS
                  ALTERNATE RECORD KEY IS
                  FECHA-ING-INTCONS WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ESPEC-INTCONS WITH DUPLICATES
                  FILE STATUS OTR-STAT.


           SELECT ARCHIVO-CITAS
                  ASSIGN NOM-CITAS-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-CIT
                  ALTERNATE RECORD KEY IS LLAVE-ALT-CIT WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PACI-CIT WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
005110 COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-RESTR.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-PROFE.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-USUAR.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-PACIE.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-HCAPER17.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-HCEVO17.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-DETEVO17.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-INTCONS17.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-CITAS.CBL".


005190 WORKING-STORAGE SECTION.
005200 COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
005210 COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".
005210 COPY "P:\PROG\PROYECT\FUENTES\WS-ESQUEMA.CBL".

       77 NOM-PACIE-W                PIC X(70).
       77 NOM-HCAPE-W                PIC X(70).
       77 NOM-USUAR-W                PIC X(70).
       77 NOM-HCEVO-W                PIC X(70).
       77 NOM-DETEVO-W               PIC X(70).
       77 NOM-PROF-W                 PIC X(70).
       77 NOM-TMP-W                  PIC X(70).
       77 NOM-INTCONS-W              PIC X(70).
       77 NOM-CITAS-W                PIC X(70).

       77 MEDICO-W                   PIC 9(10).
       77 SERV-W                     PIC 99.
       77 GLUCOMETRIA-W              PIC 9(3).
       77 DOSIS-OXIGENO-W            PIC 99.
       77 COD-DIAGN-W                PIC X(30).


       01 NUMERO-EDIT.
          02 NUMERO1-W               PIC 99.
          02 NUMERO2-W               PIC 99.

       01 NUMERO-EDIT2.
          02 NUMERO1-W2              PIC 9.
          02 NUMERO2-W2              PIC 99.

       01 NUMERO-EDIT3.
          02 NUMERO1-W3              PIC 9(3).
          02 NUMERO2-W3              PIC 9.

       01 IMC-CORP-EDIT.
          02 IMC-CORP-1              PIC XX.
          02 IMC-CORP-2              PIC XX.

       01 SUP-CORP-EDIT.
          02 SUP-CORP-1              PIC XX.
          02 SUP-CORP-2              PIC XX.

       01 TEMP-EDIT.
          02 TEMP-1                  PIC XX.
          02 TEMP-2                  PIC XX.

       01 G-URIN-EDIT.
          02 G-URIN-1                PIC X.
          02 G-URIN-2                PIC XX.

       01 PER-TORA-EDIT.
          02 PER-TORA-1              PIC X(3).
          02 PER-TORA-2              PIC X.



       01 EDAD-W.
          02 UNID-EDAD-W             PIC X.
          02 VLR-EDAD-W              PIC 9(3).

       01  TABLA-W.
           02 CM-W                   OCCURS 95     PIC X.


       01 FECHA-ACT.
          02 ANO-ACT        PIC 99.
          02 ANO2-ACT       PIC 99.
          02 MES-ACT        PIC 99.
          02 DIA-ACT        PIC 99.


       01  DATO-LLEGADA-W            PIC X(300).


       01 TABLA-TMP-W.
          02 DATOS-TMP-W       OCCURS 900  PIC X.


       01 COD-PACI-W                    PIC X(15).








       01 REG-EVO-W.
          02 LLAVE-EVO-W.
             03 LLAVE-FECHA-EVO-W.
                05 LLAVE-PACI-EVO-W.
                   07 ID-EVO-W           PIC X(15).
                   07 FOLIO-EVO-W.
                      09 FOLIO-SUC-EVO-W PIC XX.
                      09 FOLIO-NRO-EVO-W    PIC 9(6).
                05 FECHA-EVO-W.
                   07 ANO-EVO-W             PIC 9(4).
                   07 MES-EVO-W             PIC 99.
                   07 DIA-EVO-W             PIC 99.
             03 HORA-EVO-W.
                05 HR-EVO-W                 PIC 99.
                05 MN-EVO-W                 PIC 99.
             03 OPER-ELAB-EVO-W             PIC X(4).
      *39
          02 DATOS-EVO-W.
             03 TIPO-EVO-W                  PIC X.
             03 MED-EVO-W                   PIC 9(10).
             03 UNSERV-EVO-W.
                05 UNSERV1-EVO-W            PIC 9.
                05 UNSERV2-EVO-W            PIC 9.
             03 SIGNOS-EVO-W.
                05 PESO-EVO-W               PIC 9(3).
                05 TALLA-EVO-W              PIC 9(3).
                05 IMC-CORP-EVO-W           PIC 99V99.
                05 SUP-CORP-EVO-W           PIC 99V99.
                05 TEMP-EVO-W               PIC 99V99.
                05 FCARD-EVO-W              PIC 9(3).
                05 FRESP-EVO-W              PIC 9(3).
                05 TENS-EVO-W.
                   07 TENS1-EVO-W           PIC 9(3).
                   07 TENS2-EVO-W           PIC 9(3).
                05 TENS-MEDIA-EVO-W         PIC 9(3).
                05 UND-PESO-EVO-W           PIC 9.
                05 TANNER-PUBICO-EVO-W      PIC 9.
                05 TANNER-GENIT-EVO-W       PIC 9.
                05 PVC-EVO-W                PIC 99.
                05 GLASG-EVO-W.
                   07 APER-OCUL-EVO-W       PIC 9.
                   07 RESP-VERB-EVO-W       PIC 9.
                   07 RESP-MOTO-EVO-W       PIC 9.
                   07 VLR-GLASG-EVO-W       PIC 9(2).
                05 G-URIN-EVO-W             PIC 9V99.
                05 OXIMETRIA-EVO-W          PIC 9(3).
                05 GLUCOMETRIA-EVO-W        PIC 9(3).
                05 PESO-GRAMOS-EVO-W        PIC 9(5).
                05 ETCO-EVO-W               PIC 99.
                05 RESUL-CITO-EVO-W         PIC X.
                05 MUEST-CITO-EVO-W         PIC X.
                05 CREATININA-EVO-W         PIC 9V9.
                05 OTR-SIG-EVO-W            PIC X(3).
             03 MACRO-EVO-W.
                05 CL-MACRO-EVO-W           PIC X.
                05 COD-MACRO-EVO-W.
                   07 COD1-MACRO-EVO-W      PIC 9.
                   07 COD2-MACRO-EVO-W      PIC 9(5).
             03 CLASIF-EVO-W                PIC X.
             03 VIA-EVO-W                   PIC 99.
      *89
             03 DATOS-CUPS-EVO-W.
                05 TABLA-CUPS-EVO-W OCCURS 15.
                   07 CUPS-EVO-W.
                      09 GRP-CUP-EVO-W        PIC XX.
                      09 NRO-CUP-EVO-W        PIC X(4).
                  05 DATO-PROC-EVO-W.
                     07 TIPO-ANES-EVO-W         PIC 9.
                     07 TIEMPO-PROC-EVO-W       PIC 9(3).
                     07 TIEMPO-SALA-EVO-W       PIC 9(3).
                     07 SALA-CIR-EVO-W          PIC 99.
                     07 PROC-PROGRA-EVO-W       PIC X.
                     07 PROC-AMBULA-EVO-W       PIC X.
                     07 EVEN-ADVERSO-EVO-W      PIC 9.
                     07 TIPO-CIRUGIA-EVO-W      PIC 9.
                     07 FILLER                PIC X(93).
               03 CREATININA2-EVO-W         PIC 99V99.
      *200
               03 TABLA-FORMU.
                  05 RENG-FORM-EVO-W OCCURS 30.
                     07 TIPO-FORMU               PIC 9.
                     07 COD-FORMU.
                        09 GRUPO-FORMU           PIC XX.
                        09 ARTIC-FORMU           PIC X(11).
                     07 CANT-FORMU               PIC 9(4)V9.
                     07 INDIC-FORMU.
                        09 INDIC1-FORMU.
                           10 ORDEN-DOSIS-FORMU  PIC X(12).
                           10 CANT-DOSIS-FORMU   PIC X(9).
                           10 UNID-DOSIS-FORMU   PIC X(13).
                           10 INMED-FORMU.
                              15 CADA-DOSIS-FORMU PIC X(5).
                              15 CANT-FREC-FORMU  PIC XX.
                              15 UNID-FREC-FORMU  PIC X(7).
                           10 VIA-DOSIS-FORMU     PIC X(6).
                           10 DIAS-TRAT-FORMU     PIC X(3).
                           10 FILLER              PIC X(17).
                           10 MANEJO-FORMU        PIC X.
                           10 ESPEC-FORMU         PIC X(3).
                        09 INDIC2-1-FORMU         PIC X(78).
                     07 VAR-DOSIS-FORMU.
                        09 CANT-DOSIF-FORMU       PIC 9(8)V99.
                        09 TIPO-DOSIF-FORMU       PIC X.
                        09 CANT-FREC-DOSIF-FORMU  PIC 99.
                        09 FREC-DOSIF-FORMU       PIC X.
                        09 VIA-FORMU              PIC X.
                     07 CULTIVO-MICRO-FORMU.
                        09 EST-MAY48-CULT-EVO-W     PIC X.
                        09 REMITIDO-CULT-EVO-W      PIC X.
                        09 TIPO-MUEST-CULT-EVO-W    PIC 99.
                        09 ANTIBIO1-CULT-EVO-W      PIC X.
                        09 ANTIBIO2-CULT-EVO-W      PIC X.
                        09 ANTIBIO3-CULT-EVO-W      PIC X.
                        09 ANTIBIO4-CULT-EVO-W      PIC X.
                        09 ANTIB-OTR-CULT-EVO-W     PIC X(20).
                        09 ESTU1-SOLIC-EVO-W        PIC 99.
                        09 ESTU2-SOLIC-EVO-W        PIC 99.
                        09 ESTU3-SOLIC-EVO-W        PIC 99.
                     07 INSTITUTO-FORMU           PIC X(12).
                     07 TIPO-INCAP-FORMU          PIC 9.
                     07 GRADO-INCAP-FORMU         PIC X.
                     07 PRORROG-FORMU             PIC X.
                     07 NRO-ORD-FORMU             PIC 9.
                     07 NRO-PED-FORMU             PIC 9(6).
      *246*30=7380
                  05 OTROS-FORMU-EVO-W.
                     07 TABLA-INTERP-EVO-W OCCURS 30.
                        09 OTROS1-FORMU-EVO-W.
                           10 OBSERV-INTERP1-EVO-W.
                              15 INTERP1-EVO-W     PIC X(50).
                              15 INTERP2-EVO-W     PIC X(45).
                           10 OBSERV-INTERP2-EVO-W.
                              15 INTERP21-EVO-W     PIC X(50).
                              15 INTERP22-EVO-W     PIC X(45).
                        09 OTROS2-FORMU-EVO-W.
                           10 RESUL-INTERP-EVO-W   PIC X.
                           10 VLR-RESUL-EVO-W      PIC 9(5)V99.
                        09 INDIC2-2-FORMU        PIC X(112).
                        09 FILLER                PIC X(40).
                        09 OPER-COMP-EVO-W         PIC X(4).
                        09 MEDICO-INTERP-EVO-W     PIC 9(10).
                        09 DATOS-FACT-EVO-W.
                           10 LLAVE-FACT-EVO-W.
                              15 SUC-FACT-EVO-W    PIC XX.
                              15 CL-FACT-EVO-W     PIC 9.
                              15 NRO-FACT-EVO-W    PIC 9(6).
                           10 FECHA-COMP-EVO-W.
                              15 ANO-COMP-EVO-W    PIC 99.
                              15 MES-COMP-EVO-W    PIC 99.
                              15 DIA-COMP-EVO-W    PIC 99.
                           10 CTA-FACT-EVO-W.
                              15 PREFIJO-FACT-EVO-W PIC X.
                              15 NRO-CTA-FACT-EVO-W PIC 9(06).
                           10 HORA-ELAB-FACT-EVO-W.
                              15 HR-ELAB-FACT-EVO-W PIC 99.
                              15 MN-ELAB-FACT-EVO-W PIC 99.
                        09 FECHA-INTERP-EVO-W.
                           10 ANO-INTERP-EVO-W PIC 9(4).
                           10 MES-INTERP-EVO-W PIC 99.
                           10 DIA-INTERP-EVO-W PIC 99.
      *398*30=11940
               03 ESTAD-EVO-W.
                  05 TABLA-DIAG-EVO-W.
                     07 COD-DIAGN-EVO-W OCCURS 10.
                        10 COD2-DIAGN-EVO-W.
                           15 COD-DIAGN1-EVO-W   PIC X.
                           15 COD-DIAGN2-EVO-W   PIC X.
                        10 COD-DIAGN3-EVO-W      PIC X.
                        10 COD-DIAGN4-EVO-W      PIC X.
                  05 RIPS-EVO-W.
                     07 EMBAR-EVO-W          PIC 9.
                     07 CAUSA-EVO-W          PIC 99.
                     07 FINALID-EVO-W        PIC 99.
                     07 ESTADO-SAL-EVO-W     PIC 9.
                     07 REMITIDO-EVO-W       PIC X(20).
                     07 ATIENDE-EVO-W        PIC X.
                     07 EDAD-EVO-W.
                        09 UNID-EDAD-EVO-W   PIC X.
                        09 VLR-EDAD-EVO-W    PIC 9(3).
                     07 DIAG-MUER-EVO-W.
                        09 COD1-MUE-EVO-W    PIC X(3).
                        09 COD2-MUE-EVO-W    PIC X.
               03 DATO-LECT-EVO-W.
                  05 OPER-LECT-EVO-W         PIC X(4).
                  05 FECHA-LECT-EVO-W.
                     07 ANO-LECT-EVO-W       PIC 99.
                     07 MES-LECT-EVO-W       PIC 99.
                     07 DIA-LECT-EVO-W       PIC 99.
               03 DATO-FACT-EVO-W.
                  05 OPER-FACT-EVO-W         PIC X(4).
                  05 FECHA-FACT-EVO-W        PIC X(6).
      *95
               03 LIQUIDOS-EVO-W.
                  05 LIQ-ADM-ORAL-EVO-W      PIC 9(5).
                  05 LIQ-ADM-VENA-EVO-W      PIC 9(5).
                  05 LIQ-ADM-TRAN-EVO-W      PIC 9(5).
                  05 LIQ-ELI-ORIN-EVO-W      PIC 9(5).
                  05 LIQ-ELI-DIAR-EVO-W      PIC 9(5).
                  05 LIQ-ELI-SOND-EVO-W      PIC 9(5).
                  05 LIQ-ELI-OTR1-EVO-W      PIC 9(5).
                  05 LIQ-ELI-DET-EVO-W       PIC X(10).
      *45
               03 PUPILAS-EVO-W.
                  05 TAM-PUP-DR-EVO-W        PIC 9.
                  05 REA-PUP-DR-EVO-W        PIC 9.
                  05 TAM-PUP-IZ-EVO-W        PIC 9.
                  05 REA-PUP-IZ-EVO-W        PIC 9.
               03 MUSCULAR-EVO-W.
                  05 MSD-EVO-W               PIC 9.
                  05 MSI-EVO-W               PIC 9.
                  05 MII-EVO-W               PIC 9.
                  05 MID-EVO-W               PIC 9.
               03 REV-CITO-EVO-W             PIC X.
               03 SINTOM-RESPI-EVO-W         PIC X.
               03 1RA-VEZ-EVO-W              PIC X.
               03 SINTOM-PIEL-EVO-W          PIC X.
               03 METODO-OXI-EVO-W.
                  05 TIPO-MET-OXI-EVO-W      PIC 9.
                  05 DESC-MET-OXI-EVO-W      PIC X(10).
               03 FCARD-FET-EVO-W            PIC 9(3).
               03 DOSIS-OXIGENO-EVO-W        PIC 99.
               03 PER-TORA-EVO-W             PIC 9(3)V9.
               03 PER-MUNE-EVO-W             PIC 9(2)V9.
               03 TIPO-DIAG-EVO-W            PIC X.
               03 CRONICO-EVO-W              PIC X(3).
               03 VAR-CRECIM-EVO-W.
                  05 PER-CEF-EVO-W           PIC 9(3)V9.
                  05 PES-ED-ESTAD-EVO-W      PIC S9.
                  05 TALLA-ED-ESTAD-EVO-W    PIC S9.
                  05 PESO-TAL-ESTAD-EVO-W    PIC S9.
                  05 IMC-ESTAD-EVO-W         PIC S9.
                  05 PER-CEF-ESTAD-EVO-W     PIC S9.
                  05 FILLER                PIC X.
               03 VAR-EMBAR-EVO-W.
                  05 GESTACIONES-EVO-W       PIC 99.
                  05 FECHA-REGLA-EVO-W.
                     07 ANO-REGLA-EVO-W      PIC 9(4).
                     07 MES-REGLA-EVO-W      PIC 9(2).
                     07 DIA-REGLA-EVO-W      PIC 9(2).
                  05 EDAD-GEST-REGLA-EVO-W   PIC 99V9.
                  05 ALT-UTER-EVO-W          PIC 99V9.
                  05 PESO-PREV-EVO-W         PIC 9(3)V9.
                  05 EST-NUTRI-EVO-W         PIC X.
                  05 PLANIFIC-EVO-W          PIC X.
                  05 OTR-PLANIFIC-EVO-W      PIC X(10).
                  05 NRO-CONTR-EVO-W         PIC 99.
      *83
               03 PATOLOGIA-EVO-W.
                  05 PIEZA-QUIR-EVO-W        PIC X(95).
                  05 DATOS-CLINICOS-EVO-W.
                     07 TABLA-CLINICOS-EVO-W PIC X(95) OCCURS 3.
                  05 FECHA-MUEST-EVO-W.
                     07 ANO-MUEST-EVO-W      PIC 9(4).
                     07 MES-MUEST-EVO-W      PIC 99.
                     07 DIA-MUEST-EVO-W      PIC 99.
                  05 TIPO-ESTUD-EVO-W        PIC 99.
                  05 HISTOL-ESTUD-EVO-W      PIC 99.
                  05 GRADO-DIF-ESTUD-EVO-W   PIC 99.
               03 VICTI-MALTRATO-EVO-W       PIC X.
               03 VICTI-VIOLENCIA-EVO-W      PIC X.
               03 ENFER-MENTAL-EVO-W         PIC X.
               03 ENFER-ITS-EVO-W            PIC X.
               03 CANCER-SENO-EVO-W          PIC X.
               03 CANCER-CERVIS-EVO-W        PIC X.
               03 LISTA-CHEQUEO-CIR-EVO-W    PIC X.

               03 HEMO-GLICOSILADA-EVO-W PIC 99V99.
               03 HEMO-GLICO-FECHA-EVO-W.
                  04 ANO-HEMO-GLICO-EVO-W PIC 9(4).
                  04 MES-HEMO-GLICO-EVO-W PIC 99.
                  04 DIA-HEMO-GLICO-EVO-W PIC 99.
               03 MICROALBUMINURIA-EVO-W PIC 99V99.
               03 FECHA-MICROALBUMINURIA-EVO-W.
                  04 ANO-MICROALBU-EVO-W PIC 9(4).
                  04 MES-MICROALBU-EVO-W PIC 99.
                  04 DIA-MICROALBU-EVO-W PIC 99.
               03 RIESGO-CARDIO-EVO-W    PIC 9.
               03 MICROALBUMINURIA2-EVO-W PIC 999V99.
               03 EDU-AUTOEXA-SENO-EVO-W PIC X.
               03 FILLER                   PIC X(464).

               03 FILLER                   PIC X(2791).

               03 ID-ACEPTA-CONSEN-EVO-W  PIC X(15).

      *3702
      *PREGUNTAS SOLO PARA APERTURA HC02
               03 ESPECIALIDAD-HC02-EVO-W    PIC 9(3).
               03 FECHA-CITA-ESP-HC02-EVO-W.
                  04 ANO-CITA-ESP-HC02-EVO-W PIC 9(4).
                  04 MES-CITA-ESP-HC02-EVO-W PIC 99.
                  04 DIA-CITA-ESP-HC02-EVO-W PIC 99.
      ******
               03 ACOMPA-EVO-W               PIC X(15).
               03 PRIORIDAD-FORMU-EVO-W      PIC X.
               03 PER-ABDO-EVO-W             PIC 9(3)V9.
               03 DATOS-GRAB-EVO-W.
                  05 FECHA-GRAB-EVO-W.
                     07 ANO-GRAB-EVO-W       PIC 9(4).
                     07 MES-GRAB-EVO-W       PIC 99.
                     07 DIA-GRAB-EVO-W       PIC 99.
                  05 HORA-GRAB-EVO-W.
                     07 HR-GRAB-EVO-W        PIC 99.
                     07 MN-GRAB-EVO-W        PIC 99.
               03 VAR-EXAMEN-EVO-W.
                  05 INST-EXAM-EVO-W         PIC X.
                  05 IPS-EXAM-EVO-W          PIC X(12).
               03 TABLA-CUPS-ANT-EVO-W.
                  05 CUPS-ANT-EVO-W             PIC X(6) OCCURS 5.
               03 FILLER                   PIC X(4).
               03 DATOS-TRAS-EVO-W.
                  05 OPER-TRAS-EVO-W            PIC X(4).
                  05 FECHA-TRAS-EVO-W           PIC X(6).
                  05 LLAVE-PACI-TRAS-EVO-W.
                     07 ID-PACI-TRAS-EVO-W      PIC X(15).
                     07 FOLIO-SUC-TRAS-EVO-W    PIC XX.
                     07 FOLIO-NRO-TRAS-EVO-W    PIC X(6).
      *123


006310 LINKAGE SECTION.
006320 COPY "P:\PROG\PROYECT\FUENTES\ISAPICTX.CBL".
006330 PROCEDURE DIVISION WITH stdcall LINKAGE USING ISAPI-CTX-CNT.
006340
006350 DECLARATIVES.
002570 I-O-TEST SECTION.
002580     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.         
002590 ESCR-EXCEPTIONES.
002600     IF OTR-STAT = "00" 
002610        CONTINUE 
002620     ELSE
002630        MOVE OTR-STAT         TO MENSAJE1-HTML
002640        MOVE NOM-USUAR-W      TO MENSAJE2-HTML
002650        MOVE "HC003"          TO MENSAJE3-HTML
002660        GO TO ENVIAR-ERROR
002670     END-IF.

006360 I-O-TEST SECTION.
006370     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUNET.         
006380 ESCR-EXCEPTIONES.
006390     IF OTR-STAT = "00"
006400        CONTINUE 
006410     ELSE
006420        MOVE OTR-STAT         TO MENSAJE1-HTML
006430        MOVE NOM-USU-W        TO MENSAJE2-HTML
006440        MOVE "HC003"          TO MENSAJE3-HTML
006450        GO TO ENVIAR-ERROR
006460     END-IF.
006470
006480 I-O-TEST SECTION.
006490     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.
006500 ESCR-EXCEPTIONES.
006510     IF OTR-STAT = "00" 
006520        CONTINUE 
006530     ELSE
006540        MOVE OTR-STAT         TO MENSAJE1-HTML
006550        MOVE NOM-SESION-W     TO MENSAJE2-HTML
006560        MOVE "HC003"          TO MENSAJE3-HTML
006570        GO TO ENVIAR-ERROR
006580     END-IF.
006590  
006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-RESTRICCION.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-REST-W       TO MENSAJE2-HTML
006680        MOVE "HC003"          TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PROFESIONALES.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-PROF-W       TO MENSAJE2-HTML
006680        MOVE "HC003"          TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-PACIE-W      TO MENSAJE2-HTML
006680        MOVE "HC003"          TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON HISTORIA-CLINICA.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-HCAPE-W      TO MENSAJE2-HTML
006680        MOVE "HC003"          TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON EVOLUCION-HISTORIA.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-HCEVO-W      TO MENSAJE2-HTML
006680        MOVE "HC003"          TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON EVOLUCION-DETALLADA.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-DETEVO-W     TO MENSAJE2-HTML
006680        MOVE "HC003"          TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CITAS.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-CITAS-W      TO MENSAJE2-HTML
006680        MOVE "HC003"          TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-INTERCONSULTA.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-INTCONS-W    TO MENSAJE2-HTML
006680        MOVE "HC003"          TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

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
006820     MOVE COBW3-GET-DATA    TO DATO-LLEGADA-W.

           INITIALIZE REG-EVO-W

           UNSTRING DATO-LLEGADA-W DELIMITED BY "|"
              INTO LLAVE-SESION-W, ID-EVO-W, FOLIO-EVO-W, FECHA-EVO-W, HORA-EVO-W, OPER-ELAB-EVO-W, PESO-EVO-W, TALLA-EVO-W, IMC-CORP-EDIT, SUP-CORP-EDIT, TEMP-EDIT,
                   FCARD-EVO-W, FRESP-EVO-W, TENS1-EVO-W, TENS2-EVO-W, TENS-MEDIA-EVO-W, PVC-EVO-W, APER-OCUL-EVO-W, RESP-VERB-EVO-W, RESP-MOTO-EVO-W, VLR-GLASG-EVO-W, 
                   G-URIN-EDIT, OXIMETRIA-EVO-W, GLUCOMETRIA-EVO-W, PESO-GRAMOS-EVO-W, CL-MACRO-EVO-W, COD-MACRO-EVO-W, VIA-EVO-W, COD-DIAGN-W, EMBAR-EVO-W, CAUSA-EVO-W,
                   FINALID-EVO-W, LIQ-ADM-ORAL-EVO-W, LIQ-ADM-VENA-EVO-W, LIQ-ADM-TRAN-EVO-W, LIQ-ELI-ORIN-EVO-W, LIQ-ELI-DIAR-EVO-W, LIQ-ELI-SOND-EVO-W, LIQ-ELI-OTR1-EVO-W,
                   LIQ-ELI-DET-EVO-W, REV-CITO-EVO-W, SINTOM-RESPI-EVO-W, 1RA-VEZ-EVO-W, SINTOM-PIEL-EVO-W, PER-TORA-EDIT, CRONICO-EVO-W, VICTI-MALTRATO-EVO-W, VICTI-VIOLENCIA-EVO-W,
                   ENFER-MENTAL-EVO-W, ENFER-ITS-EVO-W, CANCER-SENO-EVO-W, CANCER-CERVIS-EVO-W, EDU-AUTOEXA-SENO-EVO-W, PER-ABDO-EVO-W
           END-UNSTRING


           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.

000797 INICIAR-SESION.
000806     MOVE "D:\WEB\HC-NTAS-EVOL\DATOS\SC-SESION.DAT" TO NOM-SESION-W
000820     MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
000830     ACCEPT HORA-TOTAL FROM TIME.
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.
006840
006900 ABRIR-USUARIO.
006910     INITIALIZE OTR-STAT.
006920     MOVE "D:\WEB\HC-NTAS-EVOL\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
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
007040                  GO TO ENVIAR-ERROR
007050       WHEN OTHER GO TO CERRAR-SESION
007060     END-EVALUATE.
007070
007080     INITIALIZE LLAVE-USUNET.

007100
007110 LEER-USUARIO.
007120     READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
007130
007140     CLOSE ARCHIVO-USUNET.
007200     
007210     IF NOMBRE-USUNET = SPACES
007220     OR NIT-USUNET = ZEROS
007230        MOVE "Validacion de Sesion"        TO MENSAJE1-HTML
007240        MOVE "Falta configurar usuarios"   TO MENSAJE2-HTML
007250        MOVE "Sc"                          TO MENSAJE3-HTML
007260        GO TO ENVIAR-ERROR
007270     END-IF.
007280 
007350

       ASIGNAR-NOMBRES.

           ACCEPT FECHA-ACT FROM DATE.

           INITIALIZE NOM-USUAR-W NOM-REST-W NOM-PACIE-W NOM-HCAPE-W NOM-HCEVO-W NOM-PROF-W NOM-CITAS-W NOM-INTCONS-W
                      

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\".
           MOVE "\\"  TO NOM-REST-W NOM-USUAR-W  NOM-HCAPE-W 
007150     INSPECT NOM-REST-W  REPLACING FIRST "                    "
007160                                      BY IP-DATOS-USUNET
007170     INSPECT NOM-REST-W  REPLACING FIRST " "
007180                                      BY "\"
007190     INSPECT NOM-REST-W  REPLACING FIRST "                          "
007200                                      BY "NEWCOBOL\DATOS\ARCHREST.SC".


           INSPECT NOM-USUAR-W REPLACING FIRST  "                    "
                                            BY  IP-DATOS-USUNET
           INSPECT NOM-USUAR-W REPLACING FIRST  " "
                                            BY  "\"
           INSPECT NOM-USUAR-W REPLACING FIRST  "                              "
007183                                      BY  DIRECTORIO-USUNET
           INSPECT NOM-USUAR-W REPLACING FIRST  "                "
                                            BY  "\CONTROL\ARCHUSU".

           MOVE  NOM-USUAR-W   TO NOM-PROF-W

           INSPECT NOM-PROF-W  REPLACING FIRST  "ARCHUSU     "
                                            BY  "ARCHPROF.DAT".

           MOVE NOM-USUAR-W  TO NOM-PACIE-W       
           
           INSPECT NOM-PACIE-W  REPLACING FIRST "ARCHUSU     "
                                             BY "SC-PACIE.DAT".

           INSPECT NOM-HCAPE-W REPLACING FIRST  "                    "
                                            BY  IP-DATOS-USUNET

           INSPECT NOM-HCAPE-W REPLACING FIRST  "                   "
                                            BY  "\NEWCOBOL\HC\DATOS\"


           MOVE NOM-HCAPE-W    TO NOM-HCEVO-W NOM-DETEVO-W NOM-CITAS-W NOM-INTCONS-W

           INSPECT NOM-HCAPE-W REPLACING FIRST  "             "
                                            BY  "SC-HCAPER.S17".

           INSPECT NOM-CITAS-W REPLACING FIRST "            "
                                            BY "SC-AGEND.DAT".


           INSPECT NOM-INTCONS-W REPLACING FIRST "               "
                                              BY "INTCONS2014.S17".


       ABRIR-USUARIOS.
           OPEN INPUT ARCHIVO-USUARIOS
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.

       LEER-RESTRICCION.
           INITIALIZE OPC-REST
           MOVE ID-EVO-W        TO COD-PACI-W.
           OPEN INPUT ARCHIVO-RESTRICCION.
           MOVE OPER-ELAB-EVO-W  TO COD-REST

           READ ARCHIVO-RESTRICCION WITH NO LOCK 
                INVALID KEY 
                        MOVE "99"                 TO MENSAJE1-HTML
                        MOVE "Usuario no existe"  TO MENSAJE2-HTML
                        MOVE "HC003"              TO MENSAJE3-HTML
                        GO TO ENVIAR-ERROR
           END-READ.

           CLOSE ARCHIVO-RESTRICCION.

           MOVE ID-REST   TO MEDICO-W.


       LEER-PROFESIONALES.
           OPEN INPUT ARCHIVO-PROFESIONALES.

           MOVE MEDICO-W   TO COD-PROF
           READ ARCHIVO-PROFESIONALES WITH NO LOCK 
                INVALID KEY
                        MOVE "99"                      TO MENSAJE1-HTML
                        MOVE "Profesional no existe. " TO MENSAJE2-HTML
                        MOVE "HC003"                   TO MENSAJE3-HTML
                        GO TO ENVIAR-ERROR
           END-READ.

           CLOSE ARCHIVO-PROFESIONALES.


       LEER-PACIENTE.
           OPEN INPUT ARCHIVO-PACIENTES.

           MOVE ID-EVO-W     TO COD-PACI
           READ ARCHIVO-PACIENTES WITH NO LOCK 
                INVALID KEY
                        MOVE "99"                   TO MENSAJE1-HTML
                        MOVE "Paciente no existe "  TO MENSAJE2-HTML
                        MOVE "HC003"                TO MENSAJE3-HTML
                        GO TO ENVIAR-ERROR
           END-READ

           CLOSE ARCHIVO-PACIENTES.


       LEER-HISTORIA.
           OPEN INPUT HISTORIA-CLINICA.

           MOVE ID-EVO-W       TO ID-HC
           MOVE FOLIO-EVO-W    TO FOLIO-HC

           READ HISTORIA-CLINICA WITH NO LOCK 
                INVALID KEY
                        MOVE "99"      TO MENSAJE1-HTML
                        MOVE LLAVE-HC  TO MENSAJE2-HTML
                        MOVE "HC003"   TO MENSAJE3-HTML
                        GO TO ENVIAR-ERROR
           END-READ

           IF ESTADO-HC = 1
              CONTINUE
           ELSE
              MOVE "99"         TO MENSAJE1-HTML
              MOVE "Historia clinica ya esta cerrada" TO MENSAJE2-HTML
              MOVE "HC003"      TO MENSAJE3-HTML
              GO TO ENVIAR-ERROR
           END-IF

           CLOSE HISTORIA-CLINICA.

           MOVE  EDAD-HC   TO EDAD-W
           MOVE 9      TO SERV-W.




       ABRIR-TEMPORAL.

           MOVE FUNCTION CURRENT-DATE TO FECHA-ACT

           INSPECT NOM-HCEVO-W REPLACING FIRST "              "
                                            BY "SC-HCEVOX1.S17"
           INSPECT NOM-HCEVO-W REPLACING FIRST "X1"
                                            BY ANO2-ACT.

           INSPECT NOM-DETEVO-W REPLACING FIRST "               "
                                             BY "SC-DETEVOX1.S17"

           INSPECT NOM-DETEVO-W REPLACING FIRST "X1"
                                             BY ANO2-ACT.


           MOVE IMC-CORP-1   TO NUMERO1-W
           MOVE IMC-CORP-2   TO NUMERO2-W

           MOVE NUMERO-EDIT  TO IMC-CORP-EVO-W


           MOVE SUP-CORP-1   TO NUMERO1-W
           MOVE SUP-CORP-2   TO NUMERO2-W

           MOVE NUMERO-EDIT  TO SUP-CORP-EVO-W


           MOVE TEMP-1       TO NUMERO1-W
           MOVE TEMP-2       TO NUMERO2-W

           MOVE NUMERO-EDIT  TO TEMP-EVO-W

           MOVE G-URIN-1     TO NUMERO1-W2
           MOVE G-URIN-2     TO NUMERO2-W2

           MOVE NUMERO-EDIT2 TO G-URIN-EVO-W

           MOVE PER-TORA-1   TO NUMERO1-W3
           MOVE PER-TORA-1   TO NUMERO2-W3

           MOVE NUMERO-EDIT3 TO PER-TORA-EVO-W

           INITIALIZE TABLA-DIAG-EVO

           IF COD-DIAGN-W IS NOT = SPACES
              UNSTRING COD-DIAGN-W DELIMITED BY ";"
                 INTO COD-DIAGN-EVO-W(1), COD-DIAGN-EVO-W(2), COD-DIAGN-EVO-W(3), COD-DIAGN-EVO-W(4), COD-DIAGN-EVO-W(5)
              END-UNSTRING
           END-IF.


      *     MOVE "77"         TO MSJ1-HTML
      *     MOVE TABLA-DIAG-EVO-W    TO MSJ2-HTML
      *     MOVE SIGNOS-EVO-W    TO MSJ3-HTML
      *     GO TO ENVIAR2-ERROR.

       GRABAR-NTAS-ENFER.

           OPEN I-O EVOLUCION-HISTORIA.

           MOVE FUNCTION CURRENT-DATE TO FECHA-GRAB-EVO-W.
           ACCEPT HORA-GRAB-EVO-W  FROM TIME.

           MOVE LLAVE-EVO-W           TO LLAVE-EVO
           READ EVOLUCION-HISTORIA WITH NO LOCK
                INVALID KEY 
                     MOVE REG-EVO-W    TO REG-EVO
                     MOVE 2            TO TIPO-EVO
                     MOVE MEDICO-W     TO MED-EVO
                     MOVE SERV-W       TO UNSERV-EVO
                     MOVE EDAD-W       TO EDAD-EVO
                     WRITE REG-EVO END-WRITE
                NOT INVALID KEY
                         ADD 1 TO MN-EVO-W
                     CLOSE EVOLUCION-HISTORIA
                     GO TO GRABAR-NTAS-ENFER
           END-READ.

           CLOSE EVOLUCION-HISTORIA.


10700  PAGINA-CONFIG.  
010720     MOVE "datosrecibidos" TO COBW3-CNV-NAME
010730     MOVE 00          TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3
010750
010760     MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
010770     PERFORM ABRIR-HTML.
007980   
007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.


007990
008040 COPY "P:\PROG\PROYECT\FUENTES\SC-WEB.CBL".
005210 COPY "P:\PROG\PROYECT\FUENTES\WS-HCEVO.CBL".

