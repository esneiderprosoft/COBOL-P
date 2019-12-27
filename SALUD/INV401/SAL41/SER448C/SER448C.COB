004820*=================================================================
004830* ACTUALIZA CITAS FACTURADAS
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "..\..\FUENTES\FS-USUNET.CBL".
004920     COPY "..\..\FUENTES\FS-SESION.CBL".


           SELECT  ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                   ASSIGN TO NOM-USUAR-W
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC;
                   RECORD KEY IS NOMBRE-USU
                   ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                   ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                   ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.

           SELECT  FACTURA-SERVICIOS LOCK MODE IS AUTOMATIC
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

           SELECT  ARCHIVO-PACIENTES LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-PACIE-W
                   ORGANIZATION IS INDEXED;
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS COD-PACI
                   ALTERNATE RECORD KEY  DESCRIP-PACI WITH DUPLICATES
                   ALTERNATE RECORD KEY  EPS-PACI WITH DUPLICATES
                   ALTERNATE RECORD KEY  ID-COTIZ-PACI WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.

           SELECT  ARCHIVO-CUPS LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-CUPS-W
      *"/PROG/DATOS/SC-ARCHCUPS.DAT"
                   ORGANIZATION IS INDEXED;
                   ACCESS MODE DYNAMIC;
                   RECORD KEY  IS LLAVE-CUP
                   ALTERNATE RECORD KEY IS DESCRIP-CUP WITH DUPLICATES
                   ALTERNATE RECORD KEY IS LLAVE-ALT-CUP WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.       

           SELECT  ARCHIVO-PROFESIONALES LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-PROF-W
                   ORGANIZATION IS INDEXED;
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS COD-PROF
                   ALTERNATE RECORD KEY DESCRIP-PROF WITH DUPLICATES
      *                 ALTERNATE RECORD KEY ATIENDE-PROF WITH DUPLICATES
      *                 ALTERNATE RECORD KEY SUCURSAL-PROF WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.

           SELECT  ARCHIVO-NUMERACION LOCK MODE IS AUTOMATIC
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

           SELECT  EVOLUCION-DETALLADA LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-DETEVO-W
                   ORGANIZATION INDEXED;
                   ACCESS MODE DYNAMIC;
                   RECORD KEY IS LLAVE-DEVO
                   ALTERNATE RECORD KEY IS FOLIO-DEVO WITH DUPLICATES
                   ALTERNATE RECORD KEY IS FECHA-DEVO WITH DUPLICATES
                   FILE STATUS OTR-STAT.

           SELECT  EVOLUCION-HISTORIA LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-HCEVO-W
                   ORGANIZATION INDEXED;
                   ACCESS MODE DYNAMIC;
                   RECORD KEY IS LLAVE-EVO
                   ALTERNATE RECORD KEY IS FOLIO-EVO WITH DUPLICATES
                   ALTERNATE RECORD KEY IS FECHA-EVO WITH DUPLICATES
                   ALTERNATE RECORD KEY IS MED-EVO WITH DUPLICATES
                   FILE STATUS OTR-STAT.

           SELECT  HISTORIA-DETALLADA LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-HCDET-W
                   ORGANIZATION INDEXED;
                   ACCESS MODE DYNAMIC;
                   RECORD KEY IS LLAVE-DETHC
                   ALTERNATE RECORD KEY IS LLAVE-FOLIO-DETHC
                   WITH DUPLICATES
                   FILE STATUS OTR-STAT.

           SELECT  HISTORIA-CLINICA LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-HCAPE-W
                   ORGANIZATION INDEXED;
                   ACCESS MODE DYNAMIC;
                   RECORD KEY IS LLAVE-HC
                   ALTERNATE RECORD KEY IS FOLIO-HC WITH DUPLICATES
                   ALTERNATE RECORD KEY IS FECHA-HC WITH DUPLICATES
                   ALTERNATE RECORD KEY IS MED-HC WITH DUPLICATES
                   ALTERNATE RECORD KEY IS ESTADO-HC WITH DUPLICATES
                   FILE STATUS OTR-STAT.

005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-PACIE.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-SALUD.CBL".
       COPY "..\..\FUENTES\FD-NUMER19.CBL".
       COPY "..\..\FUENTES\FD-CUPS19.CBL".
       COPY "..\..\FUENTES\FD-PROFE19.CBL".
       COPY "..\..\FUENTES\FD-DETEVO17.CBL".
       COPY "..\..\FUENTES\FD-HCEVO17.CBL".
       COPY "..\..\FUENTES\FD-HCDETAL17.CBL".
       COPY "..\..\FUENTES\FD-HCAPER17.CBL".

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

       01 LLAVE-LNK.
          02 SUC-LNK                    PIC XX.
          02 CL-LNK                     PIC 9.
          02 NRO-LNK                    PIC X(6).

       77 MES-SAL-W                     PIC X(4).
       77 NOM-USUA-W                    PIC X(70).
       77 NOM-SAL-W                     PIC X(70).
       77 NOM-PACIE-W                   PIC X(70).
       77 NOM-CUPS-W                    PIC X(70).
       77 NOM-PROF-W                    PIC X(70).
       77 NOM-NUMER-W                   PIC X(70).
       77 NOM-DETEVO-W                  PIC X(70).
       77 NOM-HCEVO-W                   PIC X(70).
       77 NOM-HCDET-W                   PIC X(70).
       77 NOM-HCAPE-W                   PIC X(70).
       77 FECHA-FIN-W                   PIC 9(8).
       01 FECHA-INI-W.
          02 ANO-INI-W                  PIC 9(4).
          02 MES-INI-W                  PIC 9(2).
          02 DIA-INI-W                  PIC 9(2).
       01 ANO-ING-ESTAD-W               PIC 9(2).

       01 VARIABLES.
          02 SW-HC                      PIC 9.
          02 SW-EVO                     PIC 9.
          02 SW-MAX                     PIC 99.


       01 FECHA-SIG-W.
          02 SIG-W                      PIC 99.
          02 FECHA-W.
             05 ANO-W                   PIC 99.
             05 MES-W                   PIC 99.
             05 DIA-W                   PIC 99.


       01 FECHA-SIG-CTL.
          02 SIG-CTL                    PIC 99.
          02 FECHA-CTL.
             05 ANO-CTL                 PIC 99.
             05 MES-CTL                 PIC 99.
             05 DIA-CTL                 PIC 99.


       01 ID-EDIT.
          02 ID1-EDIT                   PIC X(14).
          02 ID2-EDIT                   PIC X.


       01 FECHA-EDIT-2.
          02 SIG-EDIT-2                 PIC 99.
          02 FECHA-EDIT2-2.
             05 ANO-EDIT-2              PIC 99.
             05 MES-EDIT-2              PIC 99.
             05 DIA-EDIT-2              PIC 99.

       01 RIPS-HC-W.
          02 TRIAGE-HC-W                PIC 9.
          02 CAUSA-HC-W                 PIC 99.
          02 PYP-HC-W                   PIC X.
          02 FINALID-HC-W               PIC 99.
          02 ESTADO-SAL-HC-W            PIC 9.
          02 REMITIDO-HC-W              PIC X(20).
          02 ATIENDE-HC-W               PIC 9.
          02 OBSERV-HC-W                PIC X.
          02 EDAD-HC-W                  PIC X(4).
          02 EMBAR-HC-W                 PIC X.
          02 EDAD-GEST-W                PIC 99.
          02 TIPO-DIAG-W                PIC X.
          02 ITS-HC-W                   PIC X.
          02 CANCER-SENO-HC-W           PIC X.
          02 CANCER-CERVIS-HC-W         PIC X.
          02 CRONICO-W                  PIC X(3).
          02 SINTOM-RESPI-HC-W          PIC X.
          02 SINTOM-PIEL-HC-W           PIC X.
          02 VICTI-MALTRATO-HC-W        PIC X.
          02 VICTI-VIOLENCIA-HC-W       PIC X.
          02 ENFER-MENTAL-HC-W          PIC X.
          02 1RA-VEZ-HC-W               PIC X.
          02 PLANIFIC-HC-W              PIC X.
          02 TALLA-HC-W                 PIC 9(3).
          02 PESO-HC-W                  PIC 9(3)V9.
          02 TENS-HC-W.
             03 TENS1-HC-W              PIC 9(3).
             03 TENS2-HC-W              PIC 9(3).
          02 NRO-CONT-PREN-W            PIC 99.
          02 DIAG-MUER-HC-W.
             03 COD1-MUE-HC-W           PIC X(3).
             03 COD2-MUE-HC-W           PIC X.
          02 FECHA-PARTO-FUR-W.
             03 ANO-PARTO-FUR-W         PIC 9(4).
             03 MES-PARTO-FUR-W         PIC 9(2).
             03 DIA-PARTO-FUR-W         PIC 9(2).
          02 FECHA-SALID-HC-W.
             03 ANO-SALID-HC-W          PIC 99.
             03 MES-SALID-HC-W          PIC 99.
             03 DIA-SALID-HC-W          PIC 99.
          02 HORA-SALID-HC-W.
             03 HR-SALID-HC-W           PIC 99.
             03 MN-SALID-HC-W           PIC 99.



        01 DIAGN-HC-W.
           02 TABLA-DIAGN-HC-W OCCURS 10.
              03 COD-DIAGN-HC-W         PIC X(4).

        01 ESTAD-EVO-W.
           02 TABLA-DIAG-W    OCCURS 10.
              03 COD-DIAGN-W            PIC X(4).
           02 RIPS-EVO-W.
              03 EMBAR-EVO-W            PIC 9.
              03 EDAD-GEST-EVO-W        PIC 99.
              03 CAUSA-EVO-W            PIC 99.
              03 PYP-EVO-W              PIC X.
              03 FINALID-EVO-W          PIC 99.
              03 ESTADO-SAL-EVO-W       PIC 9.
              03 REMITIDO-EVO-W         PIC X(20).
              03 ATIENDE-EVO-W          PIC 9.
              03 EDAD-EVO-W             PIC X(4).
              03 DIAG-MUER-EVO-W.
                 05 COD1-MUE-EVO-W      PIC X(3).
                 05 COD2-MUE-EVO-W      PIC X.
              03 TIPO-DIAG-EVO-W        PIC X.
              03 ITS-EVO-W              PIC X.
              03 CANCER-SENO-EVO-W      PIC X.
              03 CANCER-CERVIS-EVO-W    PIC X.
              03 CRONICO-EVO-W          PIC X(3).
              03 SINTOM-RESPI-EVO-W     PIC X.
              03 SINTOM-PIEL-EVO-W      PIC X.
              03 VICTI-MALTRATO-EVO-W   PIC X.
              03 VICTI-VIOLENCIA-EVO-W  PIC X.
              03 ENFER-MENTAL-EVO-W     PIC X.


              03 1RA-VEZ-EVO-W          PIC X.
              03 PLANIFIC-EVO-W         PIC X.
              03 TALLA-EVO-W            PIC 9(3).
              03 PESO-EVO-W             PIC 9(3)V9.
              03 TENS-EVO-W.
                 05 TENS1-EVO-W         PIC 9(3).
                 05 TENS2-EVO-W         PIC 9(3).
              03 NRO-CONT-PREN-EVO-W    PIC 99.
              03 FECHA-SALID-EVO-W.
                 05 ANO-SALID-EVO-W     PIC 99.
                 05 MES-SALID-EVO-W     PIC 99.
                 05 DIA-SALID-EVO-W     PIC 99.
              03 HORA-SALID-EVO-W.
                 05 HR-SALID-EVO-W      PIC 99.
                 05 MN-SALID-EVO-W      PIC 99.

       01 DATO-4040-ESQ-W.
          03 GINECO-ESQ-W.
             05 HEMOGLOB-ESQ-W               PIC 9(2)V9.
             05 TOMA-CALCIO-ESQ-W            PIC X.
             05 TOMA-HIERRO-ESQ-W            PIC X.
             05 TOMA-ACIDOF-ESQ-W            PIC X.
             05 FILLER                       PIC X(08).
             05 GINE-MUERTOS-ESQ-W           PIC 99.
             05 EMB-ALTO-RIESG-ESQ-W         PIC X.
             05 GINE-PLANE-ESQ-W             PIC X.
             05 GINE-ACEPT-ESQ-W             PIC X.
             05 FILLER                       PIC X.
             05 GINE-MOLAS-ESQ-W             PIC 9.
             05 GINE-OBITO-ESQ-W             PIC 9.
             05 GINE-GEMEL-ESQ-W             PIC 9.
             05 GINE-VIVOS-ESQ-W             PIC 99.
             05 GINE-25-ESQ-W                PIC 9.
             05 GINE-40-ESQ-W                PIC 9.
             05 MENARQUIA-ESQ-W              PIC 99.
             05 CICLOS-ESQ-W                 PIC 9.
             05 CICLO-IRREG-ESQ-W            PIC X(15).
             05 GESTACIONES-ESQ-W            PIC 99.
             05 PARTOS-ESQ-W                 PIC 99.
             05 ABORTOS-ESQ-W                PIC 99.
             05 CESAREAS-ESQ-W               PIC 9.
             05 ECTOPICOS-ESQ-W              PIC 9.
             05 FECHA-REGLA-ESQ-W.
                07 ANO-REGLA-ESQ-W           PIC 9(4).
                07 MES-REGLA-ESQ-W           PIC 9(2).
                07 DIA-REGLA-ESQ-W           PIC 9(2).
             05 ULT-PARTO-ESQ-W.
                07 ANO-ULT-PARTO             PIC 9(4).
                07 MES-ULT-PARTO             PIC 9(2).
                07 DIA-ULT-PARTO             PIC 9(2).
             05 FECHA-CITOL-ESQ-W.
                07 ANO-CITOL-ESQ-W           PIC 9(4).
                07 MES-CITOL-ESQ-W           PIC 99.
                07 DIA-CITOL-ESQ-W           PIC 99.
             05 RESULT-CITOL-ESQ-W           PIC 9.
             05 CITOL-ANORMAL-ESQ-W          PIC X(20).
             05 PARTOS-TERMINO-ESQ-W         PIC 99.
             05 PARTOS-PREMATURO-ESQ-W       PIC 99.
             05 FECHA-PRIMER-EMBAR-ESQ-W.
                06 ANO-PRIMER-EMBAR-ESQ-W    PIC 9(4).
                06 MES-PRIMER-EMBAR-ESQ-W    PIC 99.
                06 DIA-PRIMER-EMBAR-ESQ-W    PIC 99.
             05 DISMENORREA-ESQ-W            PIC X.
             05 INFEC-PELVIC-ESQ-W           PIC X(40).
             05 ENDOMETROS-ESQ-W             PIC X.
             05 ESPECULOSCOP-ESQ-W           PIC 9.
             05 ESPECUL-ANORMAL-ESQ-W        PIC X(70).
             05 ULTRASON-UTERO-ESQ-W         PIC X(70).
             05 ULTRASON-OVARIO-ESQ-W        PIC X(70).
             05 ULTRASON-TROMPA-ESQ-W        PIC X(70).
             05 ULTRASON-OTROS-ESQ-W         PIC X(70).
             05 CIRUGIAS-GINECO-ESQ-W        PIC X(95).
             05 PRUEBA-EMBARAZO-ESQ-W        PIC X.
             05 FECHA-PRU-EMB-ESQ-W.
                07 ANO-PRU-EMB-ESQ-W         PIC 9(4).
                07 MES-PRU-EMB-ESQ-W         PIC 99.
                07 DIA-PRU-EMB-ESQ-W         PIC 99.
             05 FROTIS-FLUJO-ESQ-W           PIC X.
             05 RESULTADO-FROTIS-ESQ-W       PIC X(95).
             05 FECHA-FROTIS-ESQ-W.
                07 ANO-FROTIS-ESQ-W          PIC 9(4).
                07 MES-FROTIS-ESQ-W          PIC 99.
                07 DIA-FROTIS-ESQ-W          PIC 99.
             05 FILLER                       PIC X(1971).
         03 OBSTETRIC-ESQ-W.
             05 EDAD-GEST-REGLA-ESQ-W        PIC 99V9.
             05 EDAD-GEST-ULTRA-ESQ-W        PIC 99V9.
             05 FECHA-PARTO-FUR-ESQ-W        PIC 99V9.
             05 FECHA-PARTO-SON-ESQ-W        PIC 99V9.
             05 FECHA-PARTO-FUR-CALE-ESQ-W.
                07 ANO-PARTO-FUR-CALE-ESQ-W  PIC 9(4).
                07 MES-PARTO-FUR-CALE-ESQ-W  PIC 9(2).
                07 DIA-PARTO-FUR-CALE-ESQ-W  PIC 9(2).
             05 FECHA-PARTO-SON-CALE-ESQ-W.
                07 ANO-PARTO-SON-CALE-ESQ-W  PIC 9(4).
                07 MES-PARTO-SON-CALE-ESQ-W  PIC 9(2).
                07 DIA-PARTO-SON-CALE-ESQ-W  PIC 9(2).
             05 PRESENTAC-ESQ-W              PIC X.
             05 SITUACION-ESQ-W              PIC X.
             05 DORSO-OBS-ESQ-W              PIC X.
             05 LIQ-AMNIOT-VOL-ESQ-W         PIC 99.
             05 POLIHIDRAM-ESQ-W             PIC X.
             05 OLIGOAMNIO-ESQ-W             PIC X.
             05 IMPLANTACION-ESQ-W           PIC 99.
             05 DIST-CERVIX-ESQ-W            PIC 99.
             05 FREC-CARD-FETAL-ESQ-W        PIC 9(3)V9.
             05 UMBIL-IR-ESQ-W               PIC 9V9(3).
             05 UMBIL-IP-ESQ-W               PIC 9V9(3).
             05 UMBIL-SD-ESQ-W               PIC 9V9(3).
             05 CEREB-IR-ESQ-W               PIC 9V9(3).
             05 CEREB-IP-ESQ-W               PIC 9V9(3).
             05 CEREB-SD-ESQ-W               PIC 9V9(3).
             05 UTDER-IR-ESQ-W               PIC 9V9(3).
             05 UTDER-IP-ESQ-W               PIC 9V9(3).
             05 UTDER-SD-ESQ-W               PIC 9V9(3).
             05 UTIZQ-IR-ESQ-W               PIC 9V9(3).
             05 UTIZQ-IP-ESQ-W               PIC 9V9(3).
             05 UTIZQ-SD-ESQ-W               PIC 9V9(3).
             05 OBSERV-FLUJOMET-ESQ-W        PIC X(40).
             05 ANATOM-FETAL-ESQ-W           PIC 9.
             05 ANATOM-ANORMAL-ESQ-W         PIC X(40).
             05 LACTANCIA-ESQ-W              PIC X.
          03 RIESGO-BIOPSICOSOCIAL-ESQ-W.
             05 HISTORIA-REPRODUCTIVA-ESQ-W.
                07 INFERTILIDAD-ABORTO-ESQ-W   PIC X.
                07 RETENCION-PACENTARIA-ESQ-W  PIC X.
                07 HTA-INDUCIDA-EMBAR-ESQ-W    PIC X.
                07 MORTINATO-MUERTE-NEO-ESQ-W  PIC X.
                07 TP-DIFICIL-ESQ-W            PIC X.
             05 CONDICIONES-ASOCIADAS-ESQ-W.
                07 QX-GINECO-ECTOPICO-ESQ-W    PIC X.
                07 ENF-RENAL-CRONICA-ESQ-W     PIC X.
                07 DIAB-GESTACIONAL-ESQ-W   PIC X.
                07 DIAB-MELLITUS-ESQ-W      PIC X.
                07 ENF-CARDIACA-ESQ-ESQ-W   PIC X.
                07 ENF-INFEC-AGUDAS-ESQ-W   PIC X.
                07 ENF-AUTOINMUNES-ESQ-W    PIC X.
                07 ANEMIA-HB-ESQ-W          PIC X.
             05 EMB-ACTUAL-ESQ-W.
                07 HEMORRAGIA-MEN-20-ESQ-W  PIC X.
                07 HEMORRAGIA-MAY-20-ESQ-W  PIC X.
                07 EMB-PROLONGADO-ESQ-W     PIC X.
                07 HTA-ESQ-W           PIC X.
                07 RPM-ESQ-W           PIC X.
                07 RCIU-ESQ-W          PIC X.
                07 EMB-MULTIPLE-ESQ-W  PIC X.
                07 MALA-PRESENTA-ESQ-W PIC X.
                07 ISOINM-RH-ESQ-W     PIC X.
             05 RIESGO-PSICOSOCIAL-ESQ-W.
                07 TENSION-EMOCIONAL-ESQ-W    PIC X.
                07 HUMOR-DEPRESIVO-ESQ-W      PIC X.
                07 SINTO-NEUROVEGETA-ESQ-W    PIC X.
                07 SOPORTE-FAMI-TIEMPO-ESQ-W  PIC X.
                07 SOPORTE-FAMI-ESPACIO-ESQ-W PIC X.
                07 SOPORTE-FAMI-DIENRO-ESQ-W  PIC X.
          03 PRENATAL-ESQ-W.
             05 FECHA-ASESO-PRE-ESQ-W.
                07 ANO-ASESO-PRE-ESQ-W        PIC 9(4).
                07 MES-ASESO-PRE-ESQ-W        PIC 99.
                07 DIA-ASESO-PRE-ESQ-W        PIC 99.
             05 FECHA-ASESO-POS-ESQ-W.
                07 ANO-ASESO-POS-ESQ-W        PIC 9(4).
                07 MES-ASESO-POS-ESQ-W        PIC 99.
                07 DIA-ASESO-POS-ESQ-W        PIC 99.
             05 FECHA-ECO-OBST-ESQ-W.
                07 ANO-ECO-OBST-ESQ-W         PIC 9(4).
                07 MES-ECO-OBST-ESQ-W         PIC 99.
                07 DIA-ECO-OBST-ESQ-W         PIC 99.
             05 FECHA-VAC-INFLU-ESQ-W.
                07 ANO-VAC-INFLU-ESQ-W        PIC 9(4).
                07 MES-VAC-INFLU-ESQ-W        PIC 99.
                07 DIA-VAC-INFLU-ESQ-W        PIC 99.
             05 FECHA-VAC-TDAP-ESQ-W.
                07 ANO-VAC-TDAP-ESQ-W         PIC 9(4).
                07 MES-VAC-TDAP-ESQ-W         PIC 99.
                07 DIA-VAC-TDAP-ESQ-W         PIC 99.
             05 FECHA-VAC-TT-ESQ-W.
                07 ANO-VAC-TT-ESQ-W           PIC 9(4).
                07 MES-VAC-TT-ESQ-W           PIC 99.
                07 DIA-VAC-TT-ESQ-W           PIC 99.
          03 EMBARAZO-EXAMEN-ESQ-W.
             05 TABLA-VIH-ESQ-W OCCURS 3.
                07 FECHA-VIH-ESQ-W.
                   10 ANO-VIH-ESQ-W           PIC 9(4).
                   10 MES-VIH-ESQ-W           PIC 99.
                   10 DIA-VIH-ESQ-W           PIC 99.
                07 RESULTADO-VIH-ESQ-W        PIC 9.
             05 TABLA-SEROLO-ESQ-W OCCURS 3.
                07 FECHA-SEROLO-ESQ-W.
                   10 ANO-SEROLO-ESQ-W        PIC 9(4).
                   10 MES-SEROLO-ESQ-W        PIC 99.
                   10 DIA-SEROLO-ESQ-W        PIC 99.
                07 RESULTADO-SEROLO-ESQ-W     PIC 9.
             05 TABLA-HEMOGL-ESQ-W OCCURS 3.
                07 FECHA-HEMOGL-ESQ-W.
                   10 ANO-HEMOGL-ESQ-W        PIC 9(4).
                   10 MES-HEMOGL-ESQ-W        PIC 99.
                   10 DIA-HEMOGL-ESQ-W        PIC 99.
                07 RESULTADO-HEMOGL-ESQ-W     PIC 99V9.
             05 TABLA-IGG-ESQ-W OCCURS 3.
                07 FECHA-IGG-ESQ-W.
                   10 ANO-IGG-ESQ-W           PIC 9(4).
                   10 MES-IGG-ESQ-W           PIC 99.
                   10 DIA-IGG-ESQ-W           PIC 99.
                07 RESULTADO-IGG-ESQ-W        PIC 9.
             05 TABLA-GLICEM-ESQ-W OCCURS 3.
                07 FECHA-GLICEM-ESQ-W.
                   10 ANO-GLICEM-ESQ-W        PIC 9(4).
                   10 MES-GLICEM-ESQ-W        PIC 99.
                   10 DIA-GLICEM-ESQ-W        PIC 99.
                07 RESULTADO-GLICEM-ESQ-W.
                   10 RESUL-GLICEM1-ESQ-W     PIC 9(3).
                   10 RESUL-GLICEM2-ESQ-W     PIC 9(3).
          03 FECHA-INTERCONSULTA-ESQ-W.
             05 FECHA-GINECO-ESQ-W.
                07 ANO-GINECO-ESQ-W           PIC 9(4).
                07 MES-GINECO-ESQ-W           PIC 99.
                07 DIA-GINECO-ESQ-W           PIC 99.
             05 FECHA-ODONTO-ESQ-W.
                07 ANO-ODONTO-ESQ-W           PIC 9(4).
                07 MES-ODONTO-ESQ-W           PIC 99.
                07 DIA-ODONTO-ESQ-W           PIC 99.
             05 FECHA-NUTRI-ESQ-W.
                07 ANO-NUTRI-ESQ-W            PIC 9(4).
                07 MES-NUTRI-ESQ-W            PIC 99.
                07 DIA-NUTRI-ESQ-W            PIC 99.
             05 FECHA-PSICOL-ESQ-W.
                07 ANO-PSICOL-ESQ-W           PIC 9(4).
                07 MES-PSICOL-ESQ-W           PIC 99.
                07 DIA-PSICOL-ESQ-W           PIC 99.
          03 HEPATITIS-B-ESQ-W.
             05 FECHA-HEPAT-B-ESQ-W.
                07 ANO-HEPAT-B-ESQ-W          PIC 9(4).
                07 MES-HEPAT-B-ESQ-W          PIC 99.
                07 DIA-HEPAT-B-ESQ-W          PIC 99.
             05 RESULTADO-HEPAT-B-ESQ-W       PIC 9.
          03 PRENATAL2-ESQ-W.
             05 NRO-DOSIS-TT-ESQ-W            PIC 9.
          03 EMBARAZO2-EXAMEN-ESQ-W.
             05 TABLA-HEMOGRA-ESQ-W OCCURS 3.
                07 FECHA-HEMOGRA-ESQ-W.
                   10 ANO-HEMOGRA-ESQ-W       PIC 9(4).
                   10 MES-HEMOGRA-ESQ-W       PIC 99.
                   10 DIA-HEMOGRA-ESQ-W       PIC 99.
                07 RESULTADO-HEMOGRA-ESQ-W    PIC 9.
             05 TABLA-HEMOPARA-ESQ-W OCCURS 3.
                07 FECHA-HEMOPARA-ESQ-W.
                   10 ANO-HEMOPARA-ESQ-W      PIC 9(4).
                   10 MES-HEMOPARA-ESQ-W      PIC 99.
                   10 DIA-HEMOPARA-ESQ-W      PIC 99.
                07 RESULTADO-HEMOPARA-ESQ-W   PIC 9.
             05 TABLA-FTA-ABS-ESQ-W OCCURS 3.
                07 FECHA-FTA-ABS-ESQ-W.
                   10 ANO-FTA-ABS-ESQ-W       PIC 9(4).
                   10 MES-FTA-ABS-ESQ-W       PIC 99.
                   10 DIA-FTA-ABS-ESQ-W       PIC 99.
                07 RESULTADO-FTA-ABS-ESQ-W    PIC 9.
             05 TABLA-UROANALI-ESQ-W OCCURS 3.
                07 FECHA-UROANALI-ESQ-W.
                   10 ANO-UROANALI-ESQ-W      PIC 9(4).
                   10 MES-UROANALI-ESQ-W      PIC 99.
                   10 DIA-UROANALI-ESQ-W      PIC 99.
                07 RESULTADO-UROANALI-ESQ-W   PIC 9.
             05 TABLA-UROCULTI-ESQ-W OCCURS 3.
                07 FECHA-UROCULTI-ESQ-W.
                   10 ANO-UROCULTI-ESQ-W      PIC 9(4).
                   10 MES-UROCULTI-ESQ-W      PIC 99.
                   10 DIA-UROCULTI-ESQ-W      PIC 99.
                07 RESULTADO-UROCULTI-ESQ-W   PIC 9.
             05 TABLA-FROTISV-ESQ-W OCCURS 3.
                07 FECHA-FROTISV-ESQ-W.
                   10 ANO-FROTISV-ESQ-W       PIC 9(4).
                   10 MES-FROTISV-ESQ-W       PIC 99.
                   10 DIA-FROTISV-ESQ-W       PIC 99.
                07 RESULTADO-FROTISV-ESQ-W    PIC 9.
             05 TABLA-GLICEMIA-ESQ-W OCCURS 3.
                07 FECHA-GLICEMIA-ESQ-W.
                   10 ANO-GLICEMIA-ESQ-W      PIC 9(4).
                   10 MES-GLICEMIA-ESQ-W      PIC 99.
                   10 DIA-GLICEMIA-ESQ-W      PIC 99.
                07 RESULTADO-GLICEMIA-ESQ-W   PIC 9(3).

       01 TENS-FACT.
          02 TENS1-FACT                 PIC 9(3).
          02 TENS2-FACT                 PIC 9(3).

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV             PIC XX.

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
              MOVE "SER448C"                TO MSJ3-HTML
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
              MOVE "SER448C"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USUA-W               TO MSJ2-HTML
              MOVE "SER448C"                TO MSJ3-HTML
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
              MOVE "SER448C"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PACIE-W              TO MSJ2-HTML
              MOVE "SER448C"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CUPS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-CUPS-W               TO MSJ2-HTML
              MOVE "SER448C"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PROFESIONALES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PROF-W               TO MSJ2-HTML
              MOVE "SER448C"                TO MSJ3-HTML
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
              MOVE "SER448C"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON EVOLUCION-DETALLADA.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-DETEVO-W             TO MSJ2-HTML
              MOVE "SER448C"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON EVOLUCION-HISTORIA.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-HCEVO-W              TO MSJ2-HTML
              MOVE "SER448C"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON HISTORIA-DETALLADA.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-HCDET-W              TO MSJ2-HTML
              MOVE "SER448C"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON HISTORIA-CLINICA.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-HCAPE-W              TO MSJ2-HTML
              MOVE "SER448C"                TO MSJ3-HTML
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
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W,
                    OPERADOR-LLEGADA-W, LLAVE-LNK
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
            
            INITIALIZE NOM-USUA-W

            MOVE "\\" TO NOM-USUA-W

            INSPECT NOM-USUA-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET 
         
001621      INSPECT NOM-USUA-W REPLACING FIRST " "
001624                                     BY "\"

            MOVE NOM-USUA-W TO NOM-CUPS-W NOM-HCEVO-W NOM-DETEVO-W

007190      INSPECT NOM-USUA-W REPLACING FIRST "                              "
                                           BY DIR-CONTAB-LLEGADA-W

            MOVE NOM-USUA-W  TO NOM-NUMER-W NOM-PACIE-W  NOM-SAL-W NOM-PROF-W

            INSPECT NOM-USUA-W REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHUSU.DAT".

            INSPECT NOM-SAL-W REPLACING FIRST "                     "
007300                                     BY "\CONTROL\SC-FACSA.DAT".

            INSPECT NOM-NUMER-W REPLACING FIRST "                     "
                                             BY "\CONTROL\SC-NUMER.DAT".

            INSPECT NOM-PACIE-W REPLACING FIRST "                     "
                                             BY "\CONTROL\SC-PACIE.DAT".

            INSPECT NOM-CUPS-W REPLACING FIRST "                      "
                                            BY "PROG\DATOS\SC-ARCHCUPS".

            INSPECT NOM-HCEVO-W REPLACING FIRST "                            "
                                             BY "PROG\HC\DATOS\SC-HCEVOX1.S17".

            INSPECT NOM-DETEVO-W REPLACING FIRST "                             "
                                              BY "PROG\HC\DATOS\SC-DETEVOX1.S17".

            INSPECT NOM-HCDET-W REPLACING FIRST "                             "
                                             BY "PROG\HC\DATOS\SC-HCDETA-X.S17".

            INSPECT NOM-HCAPE-W REPLACING FIRST "                           "
                                             BY "PROG\HC\DATOS\SC-HCAPER.S17".

        ABRIR-ARCHIVOS.
           MOVE "00"     TO SW-INVALID-ENV

           IF CL-LNK = 2 OR 3
              MOVE "01"  TO SW-INVALID-ENV
              GO TO PAGINA-CONFIG
           END-IF.

           OPEN INPUT  ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK
           END-READ.
           CLOSE ARCHIVO-USUARIOS.

           IF PREFIJO-USU = "  "
              MOVE "00"  TO PREFIJO-USU.

           INITIALIZE REG-FACT.

        LEER-COMPROBANTE.
           OPEN I-O FACTURA-SERVICIOS
           MOVE LLAVE-LNK TO LLAVE-FACT.

           READ FACTURA-SERVICIOS WITH NO LOCK
                 INVALID KEY 
                   MOVE "01"  TO SW-INVALID-ENV
                   CLOSE FACTURA-SERVICIOS
                   GO TO CERRAR-ARCHIVOS.
           IF GRUPO1-FACT  (1) = "S"
            OR GRUPO-FACT   (1) = "MO"
            OR CL-FACT = 1
               CONTINUE
            ELSE
               IF CL-FACT = 0
                  MOVE "N" TO DIAGN-CUP
               ELSE
                  OPEN INPUT ARCHIVO-CUPS
                  MOVE ART-FACT (1) TO LLAVE-CUP
                  READ ARCHIVO-CUPS INVALID KEY
                       MOVE "S" TO DIAGN-CUP
                  END-READ
                  CLOSE ARCHIVO-CUPS
               END-IF
               IF DIAGN-CUP = "N"
                  CLOSE FACTURA-SERVICIOS
                  MOVE "01" TO SW-INVALID-ENV
                  GO TO PAGINA-CONFIG
               END-IF.

            MOVE 20              TO SIG-W
            MOVE FECHA-ING-ESTAD TO FECHA-W.
            MOVE FECHA-SIG-W     TO FECHA-INI-W FECHA-FIN-W.

            IF GRUPO-FACT (1) = "73" OR "74"
               IF DIA-INI-W > 05
                  SUBTRACT 5 FROM DIA-INI-W
               ELSE
                  ADD 25 TO DIA-INI-W
                  IF MES-INI-W = 01
                     MOVE 12 TO MES-INI-W
                     SUBTRACT 1 FROM ANO-INI-W
                  ELSE
                     SUBTRACT 1 FROM MES-INI-W
                  END-IF
               END-IF
            END-IF.

      *      DISPLAY FECHA-INI-W LINE 01 POSITION 02.
      *      DISPLAY LLAVE-FACT  LINE 01 POSITION 12.


            INITIALIZE VARIABLES
                       RIPS-HC-W
                       ESTAD-EVO-W
                       DIAGN-HC-W.


           OPEN INPUT ARCHIVO-NUMERACION.
           MOVE CTA-FACT TO LLAVE-NUM
           READ ARCHIVO-NUMERACION
                INVALID KEY MOVE "N" TO RED-EXTER-NUM
           END-READ.
           CLOSE ARCHIVO-NUMERACION.


       ABRIR-PROFESIONALES.
           OPEN INPUT ARCHIVO-PROFESIONALES
                      ARCHIVO-PACIENTES.


       ABRIR-HISTORIA.
           OPEN INPUT HISTORIA-CLINICA.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              CLOSE FACTURA-SERVICIOS
                    ARCHIVO-PROFESIONALES
                    ARCHIVO-PACIENTES
               MOVE "01" TO SW-INVALID-ENV
               GO TO PAGINA-CONFIG
           END-IF.

       ABRIR-EVOLUCION.
      *     MOVE "\NEWCOBOL\HC\DATOS\SC-HCEVOX1.S17" TO NOM-HCEVO
      *     MOVE "\NEWCOBOL\HC\DATOS\SC-DETEVOX1.S17" TO NOM-DETEVO

           ADD 2000 TO ANO-ING-ESTAD
           IF ANO-ING-ESTAD > 4000
              SUBTRACT 4000 FROM ANO-ING-ESTAD
              MOVE ANO-ING-ESTAD TO ANO-ING-ESTAD-W
           ELSE
              SUBTRACT 3000 FROM ANO-ING-ESTAD
              MOVE ANO-ING-ESTAD TO ANO-ING-ESTAD-W
           END-IF

           INSPECT NOM-HCEVO-W REPLACING FIRST "X1"
                                            BY ANO-ING-ESTAD-W.
           INSPECT NOM-DETEVO-W REPLACING FIRST "X1"
                                             BY ANO-ING-ESTAD-W.


           OPEN INPUT EVOLUCION-HISTORIA.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              CLOSE FACTURA-SERVICIOS
                    ARCHIVO-PROFESIONALES
                    HISTORIA-CLINICA
                    ARCHIVO-PACIENTES
              MOVE "01" TO SW-INVALID-ENV
              GO TO PAGINA-CONFIG
           END-IF

           OPEN INPUT EVOLUCION-DETALLADA.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              CLOSE FACTURA-SERVICIOS
                    ARCHIVO-PROFESIONALES
                    HISTORIA-CLINICA
                    EVOLUCION-HISTORIA
                    ARCHIVO-PACIENTES
              MOVE "01" TO SW-INVALID-ENV
              GO TO PAGINA-CONFIG
           END-IF.

       BUSCAR-HISTORIA.
           MOVE ID-HISTORIA-FACT TO ID-HC.
           START HISTORIA-CLINICA KEY = ID-HC
                 INVALID KEY GO TO BUSCAR-EVOLUCION
           END-START.


       LEER-HISTORIA.
           INITIALIZE ESP-PROF (1) ESP-PROF (2) ESP-PROF (3)
                      ESP-PROF (4) ESP-PROF (5).


           READ HISTORIA-CLINICA NEXT AT END
                                 GO TO BUSCAR-EVOLUCION
           END-READ.


           IF ID-HC IS NOT = ID-HISTORIA-FACT
              GO TO BUSCAR-EVOLUCION.


           IF  FECHA-HC >= FECHA-INI-W
           AND FECHA-HC <= FECHA-FIN-W
               CONTINUE
           ELSE
              GO TO LEER-HISTORIA
           END-IF.

           IF SERV-HC =  01
               IF UNSERV-FACT = 01
               OR (GRUPO-FACT (1) = "73" OR "74")
                  CONTINUE
               ELSE
                  GO TO LEER-HISTORIA
               END-IF
           END-IF.


            IF FECHA-FACT <= 131131
               CONTINUE
            ELSE
               IF ART-FACT (1) = "890701"
                  CONTINUE
               ELSE
                 IF  GRUPO-FACT (1) = 89
                   MOVE MED-HC                 TO COD-PROF
                   READ ARCHIVO-PROFESIONALES WITH NO LOCK
                   END-READ
                   IF (UNSERV-FACT = 02 OR 08)
                      IF MED-OTR-FACT = MED-HC
                         CONTINUE
                      ELSE
                         GO TO LEER-HISTORIA
                      END-IF
                   END-IF
                    IF  (ESPEC-ELAB = ESP-PROF (1) OR ESP-PROF (2) OR
                        ESP-PROF (3) OR ESP-PROF (4) OR ESP-PROF (5))
                        CONTINUE
                    ELSE
                        INITIALIZE ESP-PROF (1) ESP-PROF (2)
                        ESP-PROF (3) ESP-PROF (4) ESP-PROF (5)
                        GO TO LEER-HISTORIA
                    END-IF
                 END-IF
               END-IF
            END-IF.


      *     DISPLAY FECHA-HC     LINE 02 POSITION 02
      *     DISPLAY LLAVE-HC     LINE 02 POSITION 12
      *     DISPLAY MED-HC       LINE 03 POSITION 02
      *     DISPLAY MED-OTR-FACT LINE 03 POSITION 13.


      * ESTO LO PIDIO YEIMI
           IF (NIT-USU = 900306771)
           AND CL-FACT = 7
               IF FINALID-ESTAD = FINALID-HC
                  CONTINUE
               ELSE
                  GO TO LEER-HISTORIA
               END-IF
           END-IF.


           IF  EGRESO-HC = FECHA-INI-W
           AND DIAG-MUER-HC IS NOT = SPACES
           AND GRUPO-FACT (1) = "S1"
               CONTINUE
           ELSE
              IF FECHA-HC < FECHA-INI-W
              OR FECHA-HC > FECHA-SIG-W
                 GO TO LEER-HISTORIA
              END-IF
           END-IF.


           IF GRUPO-FACT (1) = "S1"
              MOVE EGRESO-HC     TO FECHA-SIG-CTL
              MOVE FECHA-CTL     TO FECHA-SALID-HC-W
              MOVE HORA-EGRES-HC TO HORA-SALID-HC-W
           ELSE
              MOVE FECHA-HC      TO FECHA-SIG-CTL
              MOVE FECHA-CTL     TO FECHA-SALID-HC-W
              MOVE HORA-HC       TO HORA-SALID-HC-W
           END-IF.

           MOVE TRIAGE-HC   TO TRIAGE-HC-W.
           IF CAUSA-ESTAD > 00
              CONTINUE
           ELSE
              MOVE CAUSA-HC    TO CAUSA-HC-W
           END-IF.

           IF TIPO-DIAG-HC = 1 OR 2 OR 3
              MOVE TIPO-DIAG-HC TO TIPO-DIAG-W.


           MOVE ATIENDE-HC  TO ATIENDE-HC-W
           MOVE EDAD-HC     TO EDAD-HC-W.
      *     MOVE ANO-HC      TO ANO-ELAB-RIPS  (1).
      *     MOVE MES-HC      TO MES-ELAB-RIPS  (1).
      *     MOVE DIA-HC      TO DIA-ELAB-RIPS  (1).
      *     MOVE HORA-HC     TO HORA-ELAB-RIPS (1).

           MOVE ANO-HC      TO ANO-ING-ESTAD.
           MOVE MES-HC      TO MES-ING-ESTAD.
           MOVE DIA-HC      TO DIA-ING-ESTAD.
           MOVE HORA-HC     TO HORA-ATEN-ESTAD.


           IF  CL-FACT = 7
           AND UNSERV-HC = 08
           AND FINALID-ESTAD IS ZERO
           AND FINALID-HC > 00
               MOVE FINALID-HC TO FINALID-HC-W.


           IF FINALID-HC = 03
              MOVE PLANIFIC-HC TO PLANIFIC-HC-W
           END-IF.
           IF ENFER-ITS-HC = "S" OR "N"
              MOVE ENFER-ITS-HC TO ITS-HC-W
           END-IF.
      *     IF CANCER-SENO-HC = "S" OR "N"
      *        MOVE CANCER-SENO-HC TO CANCER-SENO-HC-W
      *     END-IF.
      *     IF CANCER-CERVIS-HC = "S" OR "N"
      *        MOVE CANCER-CERVIS-HC TO CANCER-CERVIS-HC-W
      *     END-IF.

           IF SINTOM-RESPI-HC = "S" OR "N"
              MOVE SINTOM-RESPI-HC TO SINTOM-RESPI-HC-W
           END-IF.
           IF SINTOM-PIEL-HC = "S" OR "N"
              MOVE SINTOM-PIEL-HC TO SINTOM-PIEL-HC-W
           END-IF.
           IF VICTI-MALTRATO-HC = "S" OR "N"
              MOVE VICTI-MALTRATO-HC TO VICTI-MALTRATO-HC-W
           END-IF.
           IF VICTI-VIOLENCIA-HC = "S" OR "N"
              MOVE VICTI-VIOLENCIA-HC TO VICTI-VIOLENCIA-HC-W
           END-IF.
           IF ENFER-MENTAL-HC = "S" OR "N"
              MOVE ENFER-MENTAL-HC TO ENFER-MENTAL-HC-W
           END-IF.

           IF UNSERV-HC  = 08
              MOVE 1RA-VEZ-HC  TO 1RA-VEZ-HC-W
           END-IF

           IF PESO-HC > 0
              IF  PESO-HC > 500
                  COMPUTE PESO-HC-W ROUNDED = PESO-HC / 1000
              ELSE
                  MOVE PESO-HC TO PESO-HC-W
              END-IF
           END-IF.
           IF TALLA-HC > 0
              MOVE TALLA-HC TO TALLA-HC-W
           END-IF.
           IF TENS1-HC > 0
              MOVE TENS-HC TO TENS-HC-W
           END-IF.

           IF ESTADO-SAL-HC > ESTADO-SAL-HC-W
              MOVE ESTADO-SAL-HC TO ESTADO-SAL-HC-W.

           IF ESTADO-SAL-HC = 2
              MOVE DIAG-MUER-HC TO DIAG-MUER-HC-W


           IF REMITIDO-HC IS NOT = SPACES
               MOVE REMITIDO-HC TO REMITIDO-HC-W.

           MOVE OBSERV-HC   TO OBSERV-HC-W.
           MOVE DIAGN-HC    TO DIAGN-HC-W.


           IF EMBARAZO-HC = 1 OR 2 OR 3 OR 4
              PERFORM LEER-DETALLES
              MOVE EMBARAZO-HC        TO EMBAR-HC-W
              MOVE EDAD-GEST-REGLA-ESQ-W TO EDAD-GEST-W
              IF MES-PARTO-FUR-CALE-ESQ-W > 0
                 MOVE FECHA-PARTO-FUR-CALE-ESQ-W TO FECHA-PARTO-FUR-W
              END-IF.

           IF (UNID-EDAD-HC = "D" OR "M")
           OR  VLR-EDAD-HC < 6
               MOVE EDAD-GEST-HC TO EDAD-GEST-W
           END-IF.

           IF NRO-CONTR-HC > 0
              MOVE NRO-CONTR-HC TO NRO-CONT-PREN-W
           END-IF.


           IF SW-HC IS ZERO
              MOVE 1 TO SW-HC
           END-IF.

           MOVE MOTIV-HC    TO MOTIVO-CONSUL-RIPS.


           GO TO LEER-HISTORIA.


       BUSCAR-EVOLUCION.
           MOVE ID-HISTORIA-FACT TO ID-EVO.


           START EVOLUCION-HISTORIA KEY = ID-EVO
                INVALID KEY GO TO REVISAR-CAMBIOS.

       LEER-EVOLUCION.
            INITIALIZE ESP-PROF (1) ESP-PROF (2) ESP-PROF (3)
                       ESP-PROF (4) ESP-PROF (5).

            READ EVOLUCION-HISTORIA NEXT AT END
                           GO TO REVISAR-CAMBIOS.



            IF ID-EVO IS NOT = ID-HISTORIA-FACT
               GO TO REVISAR-CAMBIOS.


            IF FECHA-EVO = FECHA-FIN-W
               CONTINUE
            ELSE
               GO TO LEER-EVOLUCION
            END-IF.

      *      PERFORM LEER-TABLA-EVO


           MOVE FECHA-EVO     TO FECHA-SIG-CTL.
           IF FECHA-CTL > FECHA-SALID-EVO-W
              MOVE FECHA-CTL     TO FECHA-SALID-EVO-W
           END-IF.
           IF  FECHA-CTL = FECHA-SALID-EVO-W
           AND HORA-EVO > HORA-SALID-EVO-W
               MOVE HORA-EVO      TO HORA-SALID-EVO-W
           END-IF.


            IF FECHA-FACT <= 131031
               CONTINUE
            ELSE
               IF ART-FACT (1) = "890701"
               OR (CL-FACT = 0 AND GRUPO-FACT (1) = "MO")
                  MOVE MED-EVO                 TO COD-PROF
                  READ ARCHIVO-PROFESIONALES WITH NO LOCK
      *            DISPLAY "NO EXISTE PROFESIONAL" LINE 03 POSITION 10
                  END-READ
               ELSE
                  IF GRUPO-FACT (1) = 89
                   MOVE MED-EVO                 TO COD-PROF
                   READ ARCHIVO-PROFESIONALES WITH NO LOCK
      *             DISPLAY "NO EXISTE PROFESIONAL" LINE 03 POSITION 10
                   END-READ
                   IF (UNSERV-FACT = 02 OR 08)
                      IF MED-OTR-FACT = MED-EVO
                         CONTINUE
                      ELSE
                         GO TO LEER-EVOLUCION
                      END-IF
                   END-IF
                       IF  ESPEC-ELAB = ESP-PROF (1) OR ESP-PROF (2) OR
                       ESP-PROF (3) OR ESP-PROF (4) OR ESP-PROF (5)
                         CONTINUE
                       ELSE
                         INITIALIZE ESP-PROF (1) ESP-PROF (2)
                         ESP-PROF (3) ESP-PROF (4) ESP-PROF (5)
                         GO TO LEER-EVOLUCION
                       END-IF
                   END-IF
               END-IF
            END-IF.


            IF (CL-FACT = 0 AND GRUPO-FACT (1) = "MO")
               IF  (ESP-PROF (1) = 514 OR 440 OR 138
             	OR ESP-PROF (2) = 514 OR 440 OR 138
             	OR ESP-PROF (3)	= 514 OR 440 OR 138
         	    OR ESP-PROF (4) = 514 OR 440 OR 138
         	    OR ESP-PROF (5)  = 514 OR 440 OR 138)
      *         MOVE OPER-ELAB-EVO TO OPER-ELAB-RIPS	(1)
               MOVE FECHA-EVO     TO FECHA-EDIT-2
      *         MOVE FECHA-EDIT2-2  TO FECHA-ELAB-RIPS (1)
      *         MOVE HORA-EVO       TO HORA-ELAB-RIPS   (1)
      *         MOVE TABLA-EVO      TO CONDIC-VIS-ESTAD (1)
               END-IF
            END-IF.


            IF  PESO-EVO IS ZERO
            AND PESO-GRAMOS-EVO > 500
                COMPUTE PESO-EVO = PESO-GRAMOS-EVO / 1000
            END-IF.


      *      DISPLAY LLAVE-EVO   LINE 02 POSITION 02.


      * ESTO LO PIDIO YEIMI
           IF (NIT-USU = 90030677)
           AND CL-FACT = 7
               IF FINALID-ESTAD = FINALID-EVO
                  CONTINUE
               ELSE
                  GO TO LEER-EVOLUCION
               END-IF
           END-IF.





            IF COD-DIAGN-EVO (1) IS NOT = SPACES
               PERFORM ACTUALIZAR-DIAGN-EVO VARYING J FROM 1 BY 1
                                            UNTIL J > 10.

            IF CAUSA-ESTAD > 00
               CONTINUE
            ELSE
              IF CAUSA-EVO = SPACES
              OR CAUSA-EVO = 00
                 MOVE CAUSA-HC-W TO CAUSA-EVO
              END-IF
            END-IF.


            IF CAUSA-ESTAD > 00
               CONTINUE
            ELSE
              MOVE CAUSA-EVO      TO CAUSA-EVO-W
            END-IF.

            MOVE ATIENDE-EVO    TO ATIENDE-EVO-W
            MOVE EDAD-EVO       TO EDAD-EVO-W.

      *      MOVE ANO-EVO        TO ANO-ELAB-RIPS  (1).
      *      MOVE MES-EVO        TO MES-ELAB-RIPS  (1).
      *      MOVE DIA-EVO        TO DIA-ELAB-RIPS  (1).
      *      MOVE HORA-EVO       TO HORA-ELAB-RIPS (1).

            MOVE ANO-EVO        TO ANO-ING-ESTAD.
            MOVE MES-EVO        TO MES-ING-ESTAD.
            MOVE DIA-EVO        TO DIA-ING-ESTAD.
            MOVE HORA-EVO       TO HORA-ATEN-ESTAD.


            IF TIPO-DIAG-EVO = 1 OR 2 OR 3
               MOVE TIPO-DIAG-EVO TO TIPO-DIAG-EVO-W.


            IF DIAG-MUER-EVO IS NOT = SPACES
               MOVE DIAG-MUER-EVO TO DIAG-MUER-EVO-W.

            IF REMITIDO-EVO IS NOT = SPACES
               MOVE REMITIDO-EVO TO REMITIDO-EVO-W.


            IF EMBAR-EVO = 1 OR 2 OR 3 OR 4
               MOVE EMBAR-EVO           TO EMBAR-EVO-W
               MOVE EDAD-GEST-REGLA-EVO TO EDAD-GEST-EVO-W
            END-IF.


            IF ESTADO-SAL-EVO > ESTADO-SAL-EVO-W
               MOVE ESTADO-SAL-EVO TO ESTADO-SAL-EVO-W.


            IF  CL-FACT = 7
            AND UNSERV-EVO = 08
                MOVE FINALID-EVO TO FINALID-EVO-W
                MOVE CRONICO-EVO TO CRONICO-EVO-W
            END-IF.


           IF FINALID-EVO = 03
              MOVE PLANIFIC-EVO TO PLANIFIC-EVO-W
           END-IF.

           IF UNSERV-EVO = 08
              MOVE 1RA-VEZ-EVO TO 1RA-VEZ-EVO-W
           END-IF.


           IF ENFER-ITS-EVO = "S" OR "N"
              MOVE ENFER-ITS-EVO TO ITS-EVO-W
           END-IF.

      *     IF CANCER-SENO-EVO = "S" OR "N"
      *        MOVE CANCER-SENO-EVO TO CANCER-SENO-EVO-W
      *     END-IF.
      *     IF CANCER-CERVIS-EVO = "S" OR "N"
      *        MOVE CANCER-CERVIS-EVO TO CANCER-CERVIS-EVO-W
      *     END-IF.

           IF SINTOM-RESPI-EVO = "S" OR "N"
              MOVE SINTOM-RESPI-EVO TO SINTOM-RESPI-EVO-W
           END-IF.

           IF SINTOM-PIEL-EVO = "S" OR "N"
              MOVE SINTOM-PIEL-EVO TO SINTOM-PIEL-EVO-W
           END-IF.
           IF VICTI-MALTRATO-EVO = "S" OR "N"
              MOVE VICTI-MALTRATO-EVO TO VICTI-MALTRATO-EVO-W
           END-IF.
           IF VICTI-VIOLENCIA-EVO = "S" OR "N"
              MOVE VICTI-VIOLENCIA-EVO TO VICTI-VIOLENCIA-EVO-W
           END-IF.
           IF ENFER-MENTAL-EVO = "S" OR "N"
              MOVE ENFER-MENTAL-EVO TO ENFER-MENTAL-EVO-W
           END-IF.


           IF PESO-EVO > 0
              IF  PESO-GRAMOS-EVO > 500
                  COMPUTE PESO-EVO-W ROUNDED = PESO-GRAMOS-EVO / 1000
              ELSE
                  MOVE PESO-EVO TO PESO-EVO-W
              END-IF
           END-IF.
           IF TALLA-EVO > 0
              MOVE TALLA-EVO TO TALLA-EVO-W
           END-IF.

           IF TENS1-EVO > 0
               MOVE TENS-EVO TO TENS-EVO-W
           END-IF.



           IF NRO-CONTR-EVO > 0
              MOVE NRO-CONTR-EVO TO NRO-CONT-PREN-EVO-W
           END-IF.


            IF SW-EVO IS ZERO
               MOVE 1 TO SW-EVO.

            GO TO LEER-EVOLUCION.

       REVISAR-CAMBIOS.

           IF  NIT-USU = 800162035
           AND SW-EVO = 0
           AND SW-HC = 0
           AND ART-FACT (1) = "890202"
           AND ESPEC-ELAB   = "021"
           AND COD-DIAG-ESTAD (1) = SPACES
               MOVE "Z018" TO  COD-DIAGN-HC-W (1)
               MOVE 1 TO SW-EVO
           END-IF.

           IF  NIT-USU = 800162035
           AND ART-FACT (1) = "892901"
           AND COD-DIAG-ESTAD (1) = SPACES
               MOVE "Z124" TO  COD-DIAGN-HC-W (1)
               MOVE 1 TO SW-EVO
           END-IF.

           IF  NIT-USU = 844003225
           AND COD-DIAG-ESTAD (1) = "Z539"
               MOVE 10 TO CAUSA-ESTAD
               MOVE 13 TO FINALID-ESTAD
               MOVE 1 TO SW-EVO
           END-IF.


           IF  NIT-USU = 800162035
           AND ART-FACT (1) = "997310"
           AND COD-DIAG-ESTAD (1) = SPACES
               MOVE "Z012" TO  COD-DIAGN-HC-W (1)
               MOVE 1 TO SW-EVO
           END-IF.

             IF  NIT-USU = 800162035
             AND ART-FACT (1) = "997301" OR "997103" OR "997102" OR
                                "997300"
             AND COD-DIAG-ESTAD (1) = SPACES
                 MOVE "K055" TO  COD-DIAGN-HC-W (1)
                 MOVE 1 TO SW-EVO
             END-IF.


      * PARA SERVIMEDICOS DOC LILIANA

           IF  NIT-USU = 800162035
               IF  (FECHA-FACT > 131223 AND FECHA-FACT < 131231)
               OR  RED-EXTER-NUM = "S"
               OR  GRUPO-FACT (1) = "93"
               OR  GRUPO-FACT (1) = "99"
               OR  ART-FACT (1) = "892901"
                   CONTINUE
               ELSE
                  IF SW-EVO = 0
                  AND SW-HC = 0
                  AND PREFIJO-USU = 01
                  AND (UNSERV-FACT = "02" OR "08")
                  AND GRUPO-FACT (1) = "89"
                  AND COD-DIAG-ESTAD (1) = SPACES
                      MOVE "Z532" TO  COD-DIAGN-HC-W (1)
                      MOVE 1 TO SW-EVO
                  END-IF
               END-IF
           END-IF.





            IF  SW-EVO IS ZERO
            AND SW-HC  IS ZERO
                GO TO CERRAR-ARCHIVOS
            END-IF.


       MOVER-DATOS.

      * BORRA DIAGNOSTICO SI NO HAY HC NI EVOLUCION.
            IF  OPER-CORR-RIPS = SPACES
            AND SW-HC = 0
            AND SW-EVO = 0
                INITIALIZE TIP-DIAG-ESTAD TABLA-DIAG-ESTAD
            END-IF.


            IF  SW-HC > 0
            AND COD-DIAGN-HC-W (1) IS NOT = SPACES
                IF (COD-DIAG-ESTAD (1) = SPACES OR "Z532")
                 PERFORM COMPARAR-DIAGNOSTICOS VARYING I FROM 1 BY 1
                                               UNTIL I > 10
      *          ELSE
      *             MOVE DIAGN-HC-W    TO TABLA-DIAG-ESTAD
                END-IF
      * NO SE QUIEN COLOCO QUE TODAS LAS HC ACTUALICEN SI YA TIENE DIAGNOSTICO
      * R4 2015/12/14

                IF CAUSA-ESTAD > 00
                   CONTINUE
                ELSE
                  IF CAUSA-HC-W > 0
                  AND CAUSA-ESTAD IS NOT =  CAUSA-HC-W
                      MOVE CAUSA-HC-W    TO CAUSA-ESTAD
                  ELSE
                     IF (ART-FACT (1) = "890206" OR "890306")
                       MOVE 13 TO  CAUSA-ESTAD
                     END-IF
                  END-IF
                END-IF
                IF ESTADO-SAL-HC-W > ESTAD-SAL-ESTAD
                   MOVE ESTADO-SAL-HC-W TO ESTAD-SAL-ESTAD
                END-IF
                IF ESTADO-SAL-HC-W = 2
                   MOVE ESTADO-SAL-HC-W TO ESTAD-SAL-ESTAD
                   MOVE DIAG-MUER-HC-W  TO DIAG-MUER-ESTAD
                END-IF
                IF TIPO-DIAG-W = 1 OR 2 OR 3
                   MOVE TIPO-DIAG-W TO TIP-DIAG-ESTAD
                END-IF
                MOVE EMBAR-HC-W      TO EMBAR-ESTAD
                IF EMBAR-ESTAD = ZERO OR SPACES
                   IF SEXO-PACI = "F"
                      MOVE 4 TO EMBAR-ESTAD
                   ELSE
                      MOVE 9 TO EMBAR-ESTAD
                   END-IF
                END-IF
                MOVE EDAD-GEST-W     TO EDAD-GEST-FACT
                MOVE ATIENDE-HC-W    TO PERSONAL-ELAB
                MOVE OBSERV-HC-W     TO OBSERV-RIPS
                MOVE TRIAGE-HC-W     TO TRIAGE-ESTAD
                IF  CL-FACT = 7
                AND FINALID-HC-W > 0
                AND FINALID-ESTAD < 01
                    MOVE FINALID-HC-W  TO FINALID-ESTAD
                END-IF
                IF TIP-DIAG-ESTAD = SPACES
                   MOVE 1  TO TIP-DIAG-ESTAD
                END-IF
                IF PLANIFIC-HC-W IS NOT = SPACES
                   MOVE PLANIFIC-HC-W TO PLANIFIC-FACT
                END-IF
                IF SINTOM-RESPI-HC-W IS NOT = SPACES
                   MOVE SINTOM-RESPI-HC-W TO SINTOM-RESPI-FACT
                END-IF
      *          IF ITS-HC-W IS NOT = SPACES
      *             MOVE ITS-HC-W TO ITS-FACT
      *          END-IF
      *          IF CANCER-SENO-HC-W IS NOT = SPACES
      *             MOVE CANCER-SENO-HC-W TO CANCER-SENO-FACT
      *          END-IF
      *          IF CANCER-CERVIS-HC-W IS NOT = SPACES
      *             MOVE CANCER-CERVIS-HC-W TO CANCER-CERVIS-FACT
      *          END-IF
                IF SINTOM-PIEL-HC-W IS NOT = SPACES
                   MOVE SINTOM-PIEL-HC-W TO SINTOM-PIEL-FACT
                END-IF
                IF VICTI-MALTRATO-HC-W IS NOT = SPACES
                   MOVE VICTI-MALTRATO-HC-W TO VICTI-MALTRATO-FACT
                END-IF
                IF VICTI-VIOLENCIA-HC-W IS NOT = SPACES
                   MOVE VICTI-VIOLENCIA-HC-W TO VICTI-VIOLENCIA-FACT
                END-IF
                IF ENFER-MENTAL-HC-W IS NOT = SPACES
                   MOVE ENFER-MENTAL-HC-W TO ENFER-MENTAL-FACT
                END-IF


                IF 1RA-VEZ-HC-W IS NOT = SPACES
                   MOVE 1RA-VEZ-HC-W TO 1RA-VEZ-FACT
                END-IF
                IF MES-PARTO-FUR-W > 0
                   MOVE FECHA-PARTO-FUR-W TO FECHA-PARTO-FUR-FACT
                END-IF


                IF TALLA-HC-W > 0
                   MOVE TALLA-HC-W TO TALLA-FACT
                END-IF
                IF PESO-HC-W > 0
                   MOVE PESO-HC-W TO PESO-FACT
                END-IF
                IF TENS1-HC-W > 0
                   MOVE TENS-HC-W TO TENS-FACT
                END-IF


                IF NRO-CONT-PREN-W > 0
                   MOVE NRO-CONT-PREN-W TO NRO-CONT-PREN-FACT
                END-IF
                IF FECHA-SALID-HC-W > FECHA-SAL-ESTAD
                   MOVE FECHA-SALID-HC-W TO FECHA-SAL-ESTAD
                END-IF
                IF FECHA-SALID-HC-W = FECHA-SAL-ESTAD
                   MOVE HORA-SALID-HC-W TO HORA-SALID-ESTAD
                END-IF
                MOVE "HC" TO ORIG-RIPS-FACT
            END-IF.

            IF  SW-EVO > 0
                IF  COD-DIAGN-HC-W (1) IS NOT = SPACES
                AND (COD-DIAG-ESTAD (1) = SPACES OR "Z532")
                   PERFORM COMPARAR-DIAGNOSTICOS VARYING I FROM 1 BY 1
                                                 UNTIL I > 10
                END-IF
                IF CAUSA-ESTAD > 00
                   CONTINUE
                ELSE
                  IF  CAUSA-EVO-W > 0
                  AND CAUSA-ESTAD IS NOT = CAUSA-EVO-W
                      MOVE CAUSA-EVO-W TO CAUSA-ESTAD
                  ELSE
                     IF (ART-FACT (1) = "890206" OR "890306")
                        MOVE 13 TO  CAUSA-ESTAD
                     END-IF
                  END-IF
                END-IF

                IF EMBAR-EVO-W > 0
                   MOVE EMBAR-EVO-W      TO EMBAR-ESTAD
                   MOVE EDAD-GEST-EVO-W  TO EDAD-GEST-FACT
                END-IF
                IF EMBAR-ESTAD = ZERO OR SPACES
                   IF SEXO-PACI = "F"
                      MOVE 4 TO EMBAR-ESTAD
                   ELSE
                      MOVE 9 TO EMBAR-ESTAD
                   END-IF
                END-IF

                IF TIPO-DIAG-EVO-W = 1 OR 2 OR 3
                   MOVE TIPO-DIAG-EVO-W TO TIP-DIAG-ESTAD
                END-IF
                IF ESTADO-SAL-EVO-W > ESTAD-SAL-ESTAD
                   MOVE ESTADO-SAL-HC-W TO ESTAD-SAL-ESTAD
                END-IF
                IF ESTADO-SAL-EVO-W = 2
                   MOVE DIAG-MUER-EVO-W TO DIAG-MUER-ESTAD
                END-IF
                IF  CL-FACT = 7
                AND FINALID-EVO-W > 0
                AND FINALID-ESTAD < 01
                    MOVE FINALID-EVO-W TO FINALID-ESTAD
                END-IF
                IF ATIENDE-EVO-W > 0
                   MOVE ATIENDE-EVO-W TO PERSONAL-ELAB
                END-IF
                IF TIP-DIAG-ESTAD < 2
                   MOVE 2 TO TIP-DIAG-ESTAD
                END-IF
                IF PLANIFIC-EVO-W IS NOT = SPACES
                   MOVE PLANIFIC-EVO-W TO PLANIFIC-FACT
                END-IF
      *          IF ITS-EVO-W IS NOT = SPACES
      *             MOVE ITS-EVO-W TO ITS-FACT
      *          END-IF
      *          IF CANCER-SENO-EVO-W IS NOT = SPACES
      *             MOVE CANCER-SENO-EVO-W TO CANCER-SENO-FACT
      *          END-IF
      *          IF CANCER-CERVIS-EVO-W IS NOT = SPACES
      *             MOVE CANCER-CERVIS-EVO-W TO CANCER-CERVIS-FACT
      *          END-IF
                IF SINTOM-RESPI-EVO-W IS NOT = SPACES
                   MOVE SINTOM-RESPI-EVO-W TO SINTOM-RESPI-FACT
                END-IF
                IF SINTOM-PIEL-EVO-W IS NOT = SPACES
                   MOVE SINTOM-PIEL-EVO-W TO SINTOM-PIEL-FACT
                END-IF
                IF VICTI-MALTRATO-EVO-W IS NOT = SPACES
                   MOVE VICTI-MALTRATO-EVO-W TO VICTI-MALTRATO-FACT
                END-IF
                IF VICTI-VIOLENCIA-EVO-W IS NOT = SPACES
                   MOVE VICTI-VIOLENCIA-EVO-W TO VICTI-VIOLENCIA-FACT
                END-IF
                IF ENFER-MENTAL-EVO-W IS NOT = SPACES
                   MOVE ENFER-MENTAL-EVO-W TO ENFER-MENTAL-FACT
                END-IF


                IF 1RA-VEZ-EVO-W IS NOT = SPACES
                   MOVE 1RA-VEZ-EVO-W TO 1RA-VEZ-FACT
                END-IF
                IF TALLA-EVO-W > 0
                   MOVE TALLA-EVO-W TO TALLA-FACT
                END-IF
                IF PESO-EVO-W > 0
                   MOVE PESO-EVO-W TO PESO-FACT
                END-IF
                IF TENS1-EVO-W > 0
                   MOVE TENS-EVO-W TO TENS-FACT
                END-IF


                IF NRO-CONT-PREN-EVO-W > 0
                   MOVE NRO-CONT-PREN-EVO-W TO NRO-CONT-PREN-FACT
                END-IF
                IF FECHA-SALID-EVO-W > FECHA-SAL-ESTAD
                   MOVE FECHA-SALID-EVO-W TO FECHA-SAL-ESTAD
                END-IF
                IF FECHA-SALID-EVO-W = FECHA-SAL-ESTAD
                OR HORA-SALID-ESTAD = "0000"
                   MOVE HORA-SALID-EVO-W TO HORA-SALID-ESTAD
                END-IF
                MOVE "EVO" TO ORIG-RIPS-FACT
            END-IF.


            REWRITE REG-FACT.



       CERRAR-ARCHIVOS.
           CLOSE FACTURA-SERVICIOS
                 ARCHIVO-PROFESIONALES
                 HISTORIA-CLINICA
                 EVOLUCION-HISTORIA
                 EVOLUCION-DETALLADA
                 ARCHIVO-PACIENTES.


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
       COMPARAR-DIAGNOSTICOS.

           MOVE 0 TO SW-BUSCAR.
           PERFORM BUSCAR-DIAGNOSTICOS VARYING J FROM 1 BY 1
                                       UNTIL J > 10 OR SW-BUSCAR = 1.

           IF SW-BUSCAR = 0
              PERFORM ASIGNAR-DIAGN-NUEVO VARYING J FROM 1 BY 1
                                    UNTIL J > 10 OR SW-BUSCAR = 1.


       BUSCAR-DIAGNOSTICOS.
           IF COD-DIAGN-HC-W (I) = COD-DIAG-ESTAD (J)
              MOVE 1 TO SW-BUSCAR.

       ASIGNAR-DIAGN-NUEVO.
           IF COD-DIAG-ESTAD (J) = SPACES
           OR COD-DIAG-ESTAD (J) = "Z532"
              MOVE 1 TO SW-BUSCAR
              MOVE COD-DIAGN-HC-W (I)  TO COD-DIAGN-W (I)
              MOVE COD-DIAGN-W (I) TO COD-DIAG-ESTAD (J)
           END-IF.

       ACTUALIZAR-DIAGN-EVO.
           IF COD-DIAGN-EVO (J) IS NOT = SPACES
              MOVE COD-DIAGN-EVO (J) TO COD-DIAGN-HC-W (J).

       DIA-MAX.
           EVALUATE MES-W
           WHEN 01 MOVE 31 TO SW-MAX
           WHEN 02 MOVE 28 TO SW-MAX
           WHEN 03 MOVE 31 TO SW-MAX
           WHEN 04 MOVE 30 TO SW-MAX
           WHEN 05 MOVE 31 TO SW-MAX
           WHEN 06 MOVE 30 TO SW-MAX
           WHEN 07 MOVE 31 TO SW-MAX
           WHEN 08 MOVE 31 TO SW-MAX
           WHEN 09 MOVE 30 TO SW-MAX
           WHEN 10 MOVE 31 TO SW-MAX
           WHEN 11 MOVE 30 TO SW-MAX
           WHEN 12 MOVE 31 TO SW-MAX.

       LEER-DETALLES.
           MOVE ID-HC TO COD-PACI
           READ ARCHIVO-PACIENTES INVALID KEY
                INITIALIZE REG-PACI
           END-READ

           MOVE ID-HC TO ID-EDIT

           IF ID2-EDIT = "1" OR "2" OR "3" OR "4" OR "5" OR
                         "6" OR "7" OR "8" OR "9" OR "0"
              CONTINUE
           ELSE
              MOVE "0" TO ID2-EDIT
           END-IF

           INSPECT NOM-HCDET-W REPLACING FIRST "X" BY ID2-EDIT



           OPEN INPUT HISTORIA-DETALLADA

           MOVE LLAVE-HC   TO LLAVE-HC-DETHC
           MOVE 4040       TO COD-DETHC
           READ HISTORIA-DETALLADA
                INVALID KEY
                   INITIALIZE DATO-4040-ESQ-W
                NOT INVALID KEY
                   MOVE DETALLE-DETHC TO DATO-4040-ESQ-W
           END-READ

           CLOSE HISTORIA-DETALLADA.
005210* COPY "..\..\FUENTES\WS-HCEVO.CBL".
