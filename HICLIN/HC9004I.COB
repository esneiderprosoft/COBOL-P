      *==================================================================
      *  CREA JSON CON DATOS IMPRESION HC9004 //
      *==================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           
           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-USUAR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU 
                  ALTERNATE RECORD KEY IS       
                  CLAVE-USU WITH DUPLICATES
                  ALTERNATE RECORD KEY IS
                  CTL-USU WITH DUPLICATES
                  ALTERNATE RECORD KEY IS
                  CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS USU-STAT.

           SELECT HISTORIA-CLINICA LOCK MODE IS AUTOMATIC
                  ASSIGN TO  NOM-HCAPE-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-HC
                  ALTERNATE RECORD KEY IS FOLIO-HC WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FECHA-HC WITH DUPLICATES
                  ALTERNATE RECORD KEY IS MED-HC WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ESTADO-HC WITH DUPLICATES
                  FILE STATUS OTR-STAT.

           SELECT HISTORIA-DETALLADA LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-HCDET-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-DETHC
                  ALTERNATE RECORD KEY IS LLAVE-FOLIO-DETHC
                  WITH DUPLICATES
                  FILE STATUS OTR-STAT.

           SELECT ARCHIVO-PROFESIONALES
                  ASSIGN TO NOM-PROF-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PROF   
                  ALTERNATE RECORD KEY
                  DESCRIP-PROF WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-PACIENTES LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-PACIE-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PACI
                  ALTERNATE RECORD KEY
                  DESCRIP-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY
                  EPS-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY
                  ID-COTIZ-PACI WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-OCUPACIONES 
                  ASSIGN TO NOM-OCUPA-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-OCU
                  ALTERNATE RECORD KEY IS
                  NOMBRE-OCU WITH DUPLICATES
                  FILE STATUS OTR-STAT.


           SELECT ARCHIVO-ENTIDADES 
                  ASSIGN TO NOM-ENTID-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC   
                  RECORD KEY   IS COD-ENT
                  ALTERNATE RECORD KEY IS
                  NOMBRE-ENT WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-POS
                  ASSIGN TO NOM-POS-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-POS
                  ALTERNATE RECORD KEY IS
                  NOMBRE-POS WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-UNID-SERV   
                  ASSIGN TO NOM-UNSER-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CODIGO-UNID-SERV
                  FILE STATUS IS OTR-STAT.      

           SELECT ARCHIVO-ENFERMEDAD
                  ASSIGN TO NOM-ENFERMEDAD-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC   
                  RECORD KEY   IS LLAVE-ENF
                  ALTERNATE RECORD KEY IS
                  NOMBRE-ENF WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-ESPECIALIDAD 
                 ASSIGN TO NOM-ESPEC-W
                 ORGANIZATION IS INDEXED;
                 ACCESS MODE  IS DYNAMIC    
                 RECORD KEY   IS COD-ESP
                 ALTERNATE RECORD KEY IS
                 NOMBRE-ESP WITH DUPLICATES.
            
           SELECT ARCHIVO-CIUDAD LOCK MODE IS AUTOMATIC
                 ASSIGN TO NOM-CIU-W
                 ORGANIZATION IS INDEXED;
                 ACCESS MODE  IS DYNAMIC
                 RECORD KEY   IS COD-CIU
                 ALTERNATE RECORD KEY IS
                 NOMBRE-CIU WITH DUPLICATES
                 FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-JSON
                ASSIGN TO NOM-PLANO-W
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS IS OTR-STAT.


       DATA DIVISION.
       FILE SECTION.
       COPY "S:\NEWCOBOL\FUENTES\FD-USUAR.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-HCAPER17.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-HCDETAL17.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-PACIE.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-PROFE.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-OCUPA.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-ENTID.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-CODPO.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-UNSER.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-ENFER.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-ESPEC.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-CIUDA.CBL".


       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-PLANO     PIC X(25000).

       WORKING-STORAGE  SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".
       COPY "S:\NEWCOBOL\FUENTES\WS-9004.CBL".
       
      * VARIABLES NOMBRES ARCHIVOS
       77 NOM-USUAR-W               PIC X(70).
       77 NOM-HCAPE-W               PIC X(70).
       77 NOM-PACIE-W               PIC X(70).
       77 NOM-HCDET-W               PIC X(70).
       77 NOM-PROF-W                PIC X(70).
       77 NOM-OCUPA-W               PIC X(70).
       77 NOM-ENTID-W               PIC X(70).
       77 NOM-POS-W                 PIC X(70).
       77 NOM-UNSER-W               PIC X(70).
       77 NOM-PROFESIONES-W         PIC X(70).    
       77 NOM-ENFERMEDAD-W          PIC X(70).
       77 NOM-ESPEC-W               PIC X(70).
       77 NOM-CIU-W                 PIC X(70).
       77 NOM-PLANO-W               PIC X(90).

      * VARIABLES LOCALES
       77 MES-NUM PIC 99.
       77 MES-LET PIC X(10).
       01 NIT-LOGO.
         02 FILLER PIC XX.
         02 NOM-LOGO PIC X(8).

       77 USU-STAT        PIC XX. 
       01 LLAVE-HC-W.
          02 LLAVE-PACI-HC-W.
            03 ID-HC-W            PIC X(15).
            03 FOLIO-HC-W.
               05 FOLIO-SUC-HC-W  PIC XX.
               05 FOLIO-NRO-HC-W  PIC 9(6).       
       01 ID-EDIT.
          02 ID1-EDIT PIC X(14).
          02 ID2-EDIT PIC X.      
       77 IP-SERVIDOR-W              PIC X(20).
       77 OCUP-PACI-W                PIC X(4).
       77 PARE-W PIC X.
       77 PASO-W PIC 9.
       77 TEMP-W        PIC X(20).
       77 AUX PIC 9.     
      * VARIABLES WS 9004
       01 DATOS-WS9004.
          02 PORC-ACT-FACTOR-W         PIC X.
          02 INHIBIDORES-W             PIC X.
          02 FECHA-ULT-REPOR-INHIB-W.
             03 ANO-ULT-REPOR-INHIB-W  PIC 9(4).
             03 MES-ULT-REPOR-INHIB-W  PIC 99.
             03 DIA-ULT-REPOR-INHIB-W  PIC 99.
          02 HA-RECIBIDO-ITI-W         PIC X.
          02 ESTA-RECIBIENDO-ITI-W     PIC X.
      *  VARIABLE NUMERICA EN MESES
          02 TIEMPO-EN-ITI-W           PIC 9(3).
          02 HEMARTROSIS-W             PIC X.
          02 SANGRADO-HEMORRAGIA-W.
             03 SAN-ILIOPSOAS-W        PIC X.
             03 SAN-TEJIDOS-BLANDOS-W  PIC X.
             03 SAN-INTRACRANEAL-W      PIC X.
             03 SAN-CUELLO-GARGANTA-W   PIC X.
             03 SAN-ORAL-W             PIC X.
             03 SAN-OTRAS-W            PIC X.
          02 ARTROPATIA-HEMO-CRON-W    PIC X.
          02 NRO-ART-COMPRO-W          PIC X(3).
          02 VHC-W                     PIC X.
          02 VHB-W                     PIC X.
          02 VIH-W                     PIC X.
      *29
          02 ANTECEDENTES-W.
             03 ANTE-FAMIL-W               PIC 99.
             03 MEDICOS-W.
                04 NRO-ATEN-URG-HEMO-W     PIC 9(4).
                04 NRO-EVEN-HOSPI-HEMO-W   PIC 9(4).
             03 HEMORRAGICOS-W.
                04 NRO-HEMAR-ESPON-12MES-W PIC 99.
                04 NRO-HEMAR-TRAUM-12MES-W PIC 99.     
                04 NRO-OTR-HEMOR-12MES-W   PIC 99.
             03 QUIRURGICOS-W.
                04 REEMPLAZOS-ARTICULARES-W PIC X.
                04 REEMPLAZOS-ART-12MES-W   PIC X.
                04 NRO-HEMOR-PROC-12MES-W   PIC 99.
             03 FARMACOLOGICOS-W.
                04 FACTOR-RECIBIDO-W        PIC X.
                04 ESQUEMA-RECIBIDO-W       PIC X.
                04 FECHA-INI-PRIM-TRAT-W.
                   05 ANO-INI-1ER-TRAT-W    PIC 9(4).
                   05 MES-INI-1ER-TRAT-W    PIC 99.
                   05 DIA-INI-1ER-TRAT-W    PIC 99.
                04 ANAFILAXIS-W             PIC X.
                04 CUM-CAUSA-ANAFIL-W       PIC X(12).
             03 TRAUMATICOS-W.
                04 FRACT-OSTEO-W            PIC X.
                04 PSEUDOTUMORES-W          PIC X.
                04 NRO-HEMO-TRAUM-ULT-12MES-W PIC 99.
             03 GINECO-OBST-W.
                04 PLAN-FAMIL-W    PIC X.
             03 EDAD-DX-W.
                04 UNID-EDAD-DX-W  PIC X.
                04 VLR-EDAD-DX-W   PIC 9(3).
             03 MOTIVO-PRUEBA-DX-W PIC X.
             03 FECHA-DX.
                04 ANO-DX-W     PIC 9(4).
                04 MES-DX-W     PIC 99.
                04 DIA-DX-W     PIC 99.
             03 TIPO-DEF-DX-W   PIC X.
             03 SEVERIDAD-DX-W  PIC X.
      *63
          02 ANALISIS-W.
             03 PROF-LIDER-ATENCION-W  PIC X.
             03 NRO-CON-HEMATOLOGIA-W  PIC 99.
             03 NRO-CON-ORTOPEDIA-W    PIC 99.
             03 INTER-PROF-ENFER-W    PIC X.
             03 NRO-CON-ODONTOLOGIA-W  PIC 99.
             03 NRO-CON-NUTRICION-W    PIC 99.
             03 NRO-CON-T-SOCIAL-W      PIC 99.
             03 NRO-CON-FISIATRIA-W     PIC 99.
             03 NRO-CON-PSICOLOGIA-W   PIC 99.
             03 NRO-CON-QUIM-FARMA-W   PIC 99.
             03 NRO-CON-FISIOTERAPIA-W PIC 99.
          02 TRATAMIENTO-ACTUAL-W.
             03 FACTOR-RECIBIDO-ACT-W  PIC X.
             03 ESQUEMA-RECIBIDO-ACT-W PIC X.
             03 ULT-DOSIS-ACT-W        PIC 9(4).
             03 FRECUENCIA-SEMANA-W    PIC X.
             03 NRO-UI-DL-UNID-W       PIC 9(4).
             03 NRO-APL-FACTOR-W       PIC 9(4).
             03 MODALIDAD-APL-TRAT-W   PIC X.
             03 VIA-ADMIN-W            PIC X.
             03 CUM-POS-W              PIC X(12).
             03 CUM-NOPOS-W            PIC X(12).
             03 CUM-POS-OTROS-W        PIC X(12).
             03 CUM-NOPOS-OTROS-W      PIC X(12).
      *61
          02 OBSERVACIONES-W.
             03 RENG-OBS-W      PIC X(95) OCCURS 30.
             03 NOVEDADES-W     PIC X.
      * VARIABLES DE DETALLES
       01 VARIABLES-DETALLES.
          02 ENF-ACT-HC.
             03 RENG-ENF-ACT-HC PIC X(95) OCCURS 200.

          02 FAMIL-HC.
             03 RENG-FAMIL-HC   PIC X(95) OCCURS 200.
          
          02 MEDIC-HC.
             03 RENG-MEDIC-HC   PIC X(95) OCCURS 200.
          
          02 HEMOR-HC.
             03 RENG-MEDIC-HC   PIC X(95) OCCURS 200.
          
          02 QUIRU-HC.
             03 RENG-QUIRU-HC   PIC X(95) OCCURS 200.
          
          02 TOXIC-HC.
             03 RENG-TOXIC-HC PIC X(95) OCCURS 200.
          
          02 TRAUM-HC.
             03 RENG-TRAUM-HC PIC X(95) OCCURS 200.

          02 FARMA-HC.
             03 RENG-FARMA-HC PIC X(95) OCCURS 200.
          
          02 GINEC-HC.
             03 RENG-GINEC-HC PIC X(95) OCCURS 200.
          
          02 EXA-GENERAL-HC.
             03 RENG-EXAGENERAL-HC PIC X(95) OCCURS 200.
          
          02 ANALISIS-HC.
             03 RENG-ANALISIS-HC PIC X(95) OCCURS 200.
          
          02 MOTI-HC.
             03 RENG-MOTI-HC PIC X(95) OCCURS 4.

       01 LINEA-LLEGAD-W             PIC X(200).

       01 DATOS-ENVIO.
          02 ESTADO-ENV              PIC XX.
          02 FILLER                  PIC X VALUE "|".
          
       01  DATO-LLEGADA-W.
           02 SESION-LLEGAD-W.
              03 ID-LLEGAD-W         PIC X(15).
              03 FECHA-LLEGAD-W      PIC X(8).
              03 HORA-LLEGAD-W       PIC X(6).
           02 DIRECTORIO-W           PIC X(30).

      * ----   ESTRUCTURA JSON DATOS IMPRESION----

       01 LIN-01.
      *  VARIABLES APERTURA HC
          02 FILLER                  PIC X(9)   VALUE "{*LLAVE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LLAVE-HC-J              PIC X(23).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FECHA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-HC-J              PIC X(40).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*HORA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HORA-HC-J               PIC X(40).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*SERV*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SERV-HC-J               PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FOLIO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FOLIO-HC-J              PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*SUCURSAL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SUCURSAL-HC-J           PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)   VALUE "*TIPO_HC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-HC-J               PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*OPER-ELAB*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OPER-ELAB-HC-J          PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*ESPEC_MED*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESP-MED-HC-J            PIC X(70).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*REG_MED_HC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 REG-MED-HC-J            PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*ACOMPA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ACOMPA-HC-J             PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*ID-ACOMPA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ID-ACOMPA-HC-J          PIC X(15).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*PARENT-ACOMPA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PARENT-ACOMPA-HC-J      PIC X(15).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*PLANIFIC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PLANIFIC-HC-J           PIC X(18). 
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*MOTIVO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 MOTIV-HC-J              PIC X(380).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*NOMBRE_USU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 USUAR-HC-J              PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*NIT_USU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NIT-USU-HC-J            PIC 9(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*NOM_LOGO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOM-LOGO-J              PIC X(70).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)  VALUE "*FIRMA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FIRMA-J                 PIC X(70).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*CIUD_USU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CIUD-USU-HC-J           PIC X(20).
          02 FILLER                  PIC X(13)  VALUE "*,*UND-PESO*:".   
          02 FILLER                  PIC X      VALUE "*".
          02 UND-PESO-HC-J           PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*PESO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PESO-HC-J               PIC ZZZZZ.Z.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*TALLA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TALLA-HC-J              PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*IMC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 IMC-CORP-HC-J           PIC ZZ.ZZ.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*SUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SUP-CORP-HC-J           PIC ZZ.ZZ.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*TEMP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TEMP-HC-J               PIC ZZ.ZZ.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FCARD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FCARD-HC-J              PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FRESP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FRESP-HC-J              PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*TENS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TENS-HC-J               PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*TENS-M*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TENS-MEDIA-HC-J         PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*OXIMETRIA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OXIMETRIA-HC-J          PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*PVC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PVC-HC-J                PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*GLASG*:".
          02 FILLER                  PIC X      VALUE "*".
          02 GLASG-HC-J              PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*PER_CEF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PER-CEF-HC-J            PIC ZZZ.Z.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*PER_TORA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PER-TORA-HC-J           PIC ZZZ.Z.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*PER_ABDO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PER-ABDO-HC-J           PIC ZZZ.Z.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*TIPO-DIAG*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-DIAG-HC-J          PIC X(30).
          02 FILLER                  PIC X(12)  VALUE "*,*ATIENDE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ATIENDE-HC-J            PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)   VALUE "*EPS_ATIENDE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EPS-ATIENDE-HC-J        PIC X(70).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*TRIAGE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TRIAGE-HC-J             PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*CAUSA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CAUSA-HC-J              PIC X(18).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*FINALIDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FINALID-HC-J            PIC X(18).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*EMBARAZO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EMBARAZO-HC-J           PIC X(14).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*ESTADO-SAL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESTADO-SAL-HC-J         PIC X(12).
          02 FILLER                  PIC X(13)  VALUE "*,*NOM_PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOMBRE-PACI-J           PIC X(56).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*LUG_PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PROCEDE-PACI-J          PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*ETNIA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ETNIA-HC-J              PIC X(9).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*COD_PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-PACI-J           PIC X(15).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*OCUPA_PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OCUP-PACI-J             PIC X(120).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)   VALUE "*DIREC_PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIREC-PACI-J            PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*EST-CIV-PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EST-CIV-PACI-J          PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*TELEFONO-PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TELEFONO-PACI-J         PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(18)  VALUE "*NACIMIENTO_PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NACIMIENTO-PACI-J       PIC X(40).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*EDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EDAD-HC-J               PIC 9(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*UNID_EDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UNID-EDAD-HC-J          PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*SEXO_PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SEXO-PACI-J             PIC X(9).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*TIPO_PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-PACI-J             PIC X(12).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*TIPO_AFIL_PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-AFIL-PACI-J        PIC X(14).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*GRP-SANG-PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 GRP-SANG-PACI-J         PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*RH-PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 RH-PACI-J               PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)   VALUE "*EPS-PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EPS-PACI-J              PIC X(70).
          02 FILLER                  PIC XX     VALUE "*,".
      * VARIABLES WS-9004 ONCOLOGIA
       01 LIN-HC9004.
          02 FILLER                  PIC X(23)  VALUE "*PORC_ACT_FACTOR_9004*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PORC-ACT-FACTOR-J       PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(19)  VALUE "*INHIBIDORES_9004*:".
          02 FILLER                  PIC X      VALUE "*".
          02 INHIBIDORES-J           PIC X(23).
          02 FILLER                  PIC XX    VALUE "*,".
          02 FILLER                  PIC X(19) VALUE "*FECHA_ULT_REPORT*:".
          02 FILLER                  PIC X     VALUE "*".
          02 FECHA-REPOR-INHIB-J     PIC X(40).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(18)  VALUE "*HA_RECIBIDO_ITI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HA-RECIBIDO-ITI-J       PIC X(9).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(22)  VALUE "*ESTA_RECIBIENDO_ITI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESTA-RECIBIENDO-ITI-J   PIC X(9).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*TIEMPO_EN_ITI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIEMPO-EN-ITI-J         PIC XXX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*HEMARTROSIS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HEMARTROSIS-J           PIC X(9).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(23)  VALUE "*ARTROPATIA_HEMO_CRON*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ARTROPATIA-HEMO-CRON-J  PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*NRO_ART_COMPRO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NRO-ART-COMPRO-J        PIC XXX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*VHC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VHC-J                   PIC X(9).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*VHB*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VHB-J                   PIC X(9).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*VIH*:".
          02 FILLER                  PIC X(9)   VALUE "*".
          02 VIH-J                   PIC X(9).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*SAN_ILIOPSOAS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SAN-ILIOPSOAS-J         PIC X(9).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(22)  VALUE "*SAN_TEJIDOS_BLANDOS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SAN-TEJIDOS-BLANDOS-J   PIC X(9).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(19)  VALUE "*SAN_INTRACRANEAL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SAN-INTRACRANEAL-J      PIC X(9).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(22)  VALUE "*SAN_CUELLO_GARGANTA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SAN-CUELLO-GARGANTA-J   PIC X(9).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*SAN_ORAL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SAN-ORAL-J              PIC X(9).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*SAN_OTRAS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SAN-OTRAS-J             PIC X(9).
        *>   ANT FAMIL
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*ANTE_FAMIL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ANTE-FAMIL-J            PIC X(40).
        *>   ANT MEDIC
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(20) VALUE "*NRO_ATEN_URG_HEMO*:".
          02 FILLER                  PIC X     VALUE "*".
          02 NRO-ATEN-URG-HEMO-J     PIC X(4).
          02 FILLER                  PIC XX    VALUE "*,".
          02 FILLER                  PIC X(22)  VALUE "*NRO_EVEN_HOSPI_HEMO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NRO-EVEN-HOSPI-HEMO-J   PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(24)  VALUE "*NRO_HEMAR_ESPON_12MES*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NRO-HEMAR-ESPON-12MES-J PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(24)  VALUE "*NRO_HEMAR_TRAUM_12MES*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NRO-HEMAR-TRAUM-12MES-J PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(22)  VALUE "*NRO_OTR_HEMOR_12MES*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NRO-OTR-HEMOR-12MES-J   PIC X.
        *>   ANT QUIRU
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                   PIC X(25)  VALUE "*REEMPLAZOS_ARTICULARES*:".
          02 FILLER                   PIC X      VALUE "*".
          02 REEMPLAZOS-ARTICULARES-J PIC X(10).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(23)  VALUE "*REEMPLAZOS_ART_12MES*:".
          02 FILLER                   PIC X      VALUE "*".
          02 REEMPLAZOS-ART-12MES-J   PIC X(10).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(23)  VALUE "*NRO_HEMOR_PROC_12MES*:".
          02 FILLER                   PIC X      VALUE "*".
          02 NRO-HEMOR-PROC-12MES-J   PIC XX.
        *>   ANT FARMACO
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(18)  VALUE "*FACTOR_RECIBIDO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 FACTOR-RECIBIDO-J        PIC X(23).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(19) VALUE "*ESQUEMA_RECIBIDO*:".
          02 FILLER                   PIC X     VALUE "*".
          02 ESQUEMA-RECIBIDO-J       PIC X(18).
          02 FILLER                   PIC XX    VALUE "*,".
          02 FILLER                   PIC X(19) VALUE "*ANO_INI_1ER_TRAT*:".
          02 FILLER                   PIC X     VALUE "*".
          02 ANO-INI-1ER-TRAT-J       PIC X(4).
          02 FILLER                   PIC XX    VALUE "*,".
          02 FILLER                   PIC X(19) VALUE "*MES_INI_1ER_TRAT*:".
          02 FILLER                   PIC X     VALUE "*".
          02 MES-INI-1ER-TRAT-J       PIC XX.
          02 FILLER                   PIC XX    VALUE "*,".
          02 FILLER                   PIC X(19) VALUE "*DIA_INI_1ER_TRAT*:".
          02 FILLER                   PIC X     VALUE "*".
          02 DIA-INI-1ER-TRAT-J       PIC XX.
          02 FILLER                   PIC XX    VALUE "*,".
          02 FILLER                   PIC X(13)  VALUE "*ANAFILAXIS*:".
          02 FILLER                   PIC X      VALUE "*".
          02 ANAFILAXIS-J             PIC X(10).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(19) VALUE "*CUM_CAUSA_ANAFIL*:".
          02 FILLER                   PIC X     VALUE "*".
          02 CUM-CAUSA-ANAFIL-J       PIC X(40).
        *>   ANT TRAUM
          02 FILLER                   PIC XX    VALUE "*,".
          02 FILLER                   PIC X(14)  VALUE "*FRACT_OSTEO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 FRACT-OSTEO-J            PIC X(9).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(16)  VALUE "*PSEUDOTUMORES*:".
          02 FILLER                   PIC X      VALUE "*".
          02 PSEUDOTUMORES-J          PIC X(9).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                        PIC X(27)  VALUE "*NRO_HEMO_TRAUM_ULT_12MES*:".
          02 FILLER                        PIC X      VALUE "*".
          02 NRO-HEMO-TRAUM-ULT-12MES-J    PIC XX.
          02 FILLER                        PIC XX     VALUE "*,".
        *>   ANT GINECO OBST
          02 FILLER                PIC X(13)  VALUE "*PLAN_FAMIL*:".
          02 FILLER                PIC X      VALUE "*".
          02 PLAN-FAMIL-J          PIC X(35).
          02 FILLER                PIC XX     VALUE "*,".
          02 FILLER                PIC X(19)  VALUE "*ESTADO_GESTACION*:".
          02 FILLER                PIC X      VALUE "*".
          02 SW-EMBAR-W-J          PIC X(14).
        *>   ANALISIS PLAN
          02 FILLER                PIC XX     VALUE "*,".
          02 FILLER                PIC X(15)  VALUE "*UNID_EDAD_DX*:".
          02 FILLER                PIC X      VALUE "*".
          02 UNID-EDAD-DX-J        PIC X(8).
          02 FILLER                PIC XX     VALUE "*,".
          02 FILLER                PIC X(14)  VALUE "*VLR_EDAD_DX*:".
          02 FILLER                PIC X      VALUE "*".
          02 VLR-EDAD-DX-J         PIC XXX.
          02 FILLER                PIC XX     VALUE "*,".
          02 FILLER                PIC X(19)  VALUE "*MOTIVO_PRUEBA_DX*:".
          02 FILLER                PIC X      VALUE "*".
          02 MOTIVO-PRUEBA-DX-J    PIC X(24).
          02 FILLER                PIC XX     VALUE "*,".
          02 FILLER                PIC X(11)   VALUE "*FECHA_DX*:".
          02 FILLER                PIC X      VALUE "*".
          02 FECHA-DX-J            PIC X(40).
          02 FILLER                PIC XX     VALUE "*,".
          02 FILLER                PIC X(14)  VALUE "*TIPO_DEF_DX*:".
          02 FILLER                PIC X      VALUE "*".
          02 TIPO-DEF-DX-J         PIC X(25).
          02 FILLER                PIC XX     VALUE "*,".
          02 FILLER                PIC X(15)  VALUE "*SEVERIDAD_DX*:".
          02 FILLER                PIC X      VALUE "*".
          02 SEVERIDAD-DX-J        PIC X(11).
          02 FILLER                PIC XX     VALUE "*,".
          02 FILLER                PIC X(22)  VALUE "*PROF_LIDER_ATENCION*:".
          02 FILLER                PIC X      VALUE "*".
          02 PROF-LIDER-ATENCION-J PIC X(18).
          02 FILLER                PIC XX     VALUE "*,".
          02 FILLER                PIC X(22)  VALUE "*NRO_CON_HEMATOLOGIA*:".
          02 FILLER                PIC X      VALUE "*".
          02 NRO-CON-HEMATOLOGIA-J PIC XX.
          02 FILLER                PIC XX     VALUE "*,".
          02 FILLER                    PIC X(20) VALUE "*NRO_CON_ORTOPEDIA*:".
          02 FILLER                    PIC X     VALUE "*".
          02 NRO-CON-ORTOPEDIA-J       PIC XX.
          02 FILLER                    PIC XX    VALUE "*,".
          02 FILLER                    PIC X(19) VALUE "*INTER_PROF_ENFER*:".
          02 FILLER                    PIC X     VALUE "*".
          02 INTER-PROF-ENFER-J        PIC X(9).
          02 FILLER                    PIC XX    VALUE "*,".
          02 FILLER                        PIC X(22)  VALUE "*NRO_CON_ODONTOLOGIA*:".
          02 FILLER                        PIC X      VALUE "*".
          02 NRO-CON-ODONTOLOGIA-J         PIC XX.
          02 FILLER                        PIC XX     VALUE "*,".
          02 FILLER                        PIC X(20)  VALUE "*NRO_CON_NUTRICION*:".
          02 FILLER                        PIC X      VALUE "*".
          02 NRO-CON-NUTRICION-J           PIC XX.
          02 FILLER                        PIC XX     VALUE "*,".
          02 FILLER                        PIC X(19)  VALUE "*NRO_CON_T_SOCIAL*:".
          02 FILLER                        PIC X      VALUE "*".
          02 NRO-CON-T-SOCIAL-J            PIC X.
          02 FILLER                        PIC XX     VALUE "*,".
          02 FILLER                        PIC X(20)  VALUE "*NRO_CON_FISIATRIA*:".
          02 FILLER                        PIC X      VALUE "*".
          02 NRO-CON-FISIATRIA-J           PIC XX.
          02 FILLER                        PIC XX     VALUE "*,".
          02 FILLER                    PIC X(21)  VALUE "*NRO_CON_PSICOLOGIA*:".
          02 FILLER                    PIC X      VALUE "*".
          02 NRO-CON-PSICOLOGIA-J      PIC XX.
          02 FILLER                    PIC XX     VALUE "*,".
          02 FILLER                        PIC X(21)  VALUE "*NRO_CON_QUIM_FARMA*:".
          02 FILLER                        PIC X      VALUE "*".
          02 NRO-CON-QUIM-FARMA-J          PIC XX.
          02 FILLER                        PIC XX     VALUE "*,".
          02 FILLER                        PIC X(23)  VALUE "*NRO_CON_FISIOTERAPIA*:".
          02 FILLER                        PIC X      VALUE "*".
          02 NRO-CON-FISIOTERAPIA-J        PIC XX.
          02 FILLER                        PIC XX     VALUE "*,".
          02 FILLER                        PIC X(22)  VALUE "*FACTOR_RECIBIDO_ACT*:".
          02 FILLER                        PIC X      VALUE "*".
          02 FACTOR-RECIBIDO-ACT-J         PIC X(56).
          02 FILLER                        PIC XX     VALUE "*,".
          02 FILLER                        PIC X(23)  VALUE "*ESQUEMA_RECIBIDO_ACT*:".
          02 FILLER                        PIC X      VALUE "*".
          02 ESQUEMA-RECIBIDO-ACT-J        PIC X(56).
          02 FILLER                        PIC XX     VALUE "*,".
          02 FILLER                        PIC X(16)  VALUE "*ULT_DOSIS_ACT*:".
          02 FILLER                        PIC X      VALUE "*".
          02 ULT-DOSIS-ACT-J               PIC X(57).
          02 FILLER                        PIC XX     VALUE "*,".
          02 FILLER                        PIC X(20)  VALUE "*FRECUENCIA_SEMANA*:".
          02 FILLER                        PIC X      VALUE "*".
          02 FRECUENCIA-SEMANA-J           PIC X(49).
          02 FILLER                        PIC XX     VALUE "*,".
          02 FILLER                        PIC X(17)  VALUE "*NRO_UI_DL_UNID*:".
          02 FILLER                        PIC X      VALUE "*".
          02 NRO-UI-DL-UNID-J              PIC X(56).
          02 FILLER                        PIC XX     VALUE "*,".
          02 FILLER                        PIC X(17)  VALUE "*NRO_APL_FACTOR*:".
          02 FILLER                        PIC X      VALUE "*".
          02 NRO-APL-FACTOR-J              PIC X(56).
          02 FILLER                        PIC XX     VALUE "*,".
          02 FILLER                        PIC X(21)  VALUE "*MODALIDAD_APL_TRAT*:".
          02 FILLER                        PIC X      VALUE "*".
          02 MODALIDAD-APL-TRAT-J          PIC X(56).
          02 FILLER                        PIC XX     VALUE "*,".
          02 FILLER            PIC X(12)  VALUE "*VIA_ADMIN*:".
          02 FILLER            PIC X      VALUE "*".
          02 VIA-ADMIN-J       PIC X(56).
          02 FILLER            PIC XX     VALUE "*,".
          02 FILLER            PIC X(10)  VALUE "*CUM_POS*:".
          02 FILLER            PIC X      VALUE "*".
          02 CUM-POS-J         PIC X(47).
          02 FILLER            PIC XX     VALUE "*,".
          02 FILLER            PIC X(12)  VALUE "*CUM_NOPOS*:".
          02 FILLER            PIC X      VALUE "*".
          02 CUM-NOPOS-J       PIC X(47).
          02 FILLER            PIC XX     VALUE "*,".
          02 FILLER            PIC X(16)  VALUE "*CUM_POS_OTROS*:".
          02 FILLER            PIC X      VALUE "*".
          02 CUM-POS-OTROS-J   PIC X(47).
          02 FILLER            PIC XX     VALUE "*,".
          02 FILLER            PIC X(18)  VALUE "*CUM_NOPOS_OTROS*:".
          02 FILLER            PIC X      VALUE "*".
          02 CUM-NOPOS-OTROS-J PIC X(47).
          02 FILLER            PIC XX     VALUE "*,".

          02 FILLER            PIC X(12)  VALUE "*NOVEDADES*:".
          02 FILLER            PIC X      VALUE "*".
          02 NOVEDADES-J       PIC X(56).
          02 FILLER            PIC XX      VALUE "*,". 
          
       01 LIN-ENF-ACT.
          02 FILLER            PIC X(13)     VALUE "*ENF_ACT_HC*:".
          02 FILLER            PIC X         VALUE "*".
          02 ENF-ACT-J         PIC X(19000).
          02 FILLER            PIC XX        VALUE "*,".          

       01 LIN-FAMIL-HC.
          02 FILLER            PIC X(11)     VALUE "*FAMIL_HC*:".
          02 FILLER            PIC X         VALUE "*".
          02 FAMIL-HC-J        PIC X(19000).
          02 FILLER            PIC XX        VALUE "*,".          

       01 LIN-MEDIC-HC.
          02 FILLER            PIC X(11)     VALUE "*MEDIC_HC*:".
          02 FILLER            PIC X         VALUE "*".
          02 MEDIC-HC-J        PIC X(19000).
          02 FILLER            PIC XX        VALUE "*,".

       01 LIN-HEMOR-HC.
          02 FILLER            PIC X(11)     VALUE "*HEMOR_HC*:".
          02 FILLER            PIC X         VALUE "*".
          02 HEMOR-HC-J        PIC X(19000).
          02 FILLER            PIC XX        VALUE "*,".

       01 LIN-TOXIC-HC.
          02 FILLER            PIC X(11)     VALUE "*TOXIC_HC*:". 
          02 FILLER            PIC X         VALUE "*".
          02 TOXIC-HC-J        PIC X(19000).
          02 FILLER            PIC XX        VALUE "*,".

       01 LIN-TRAUM-HC.
          02 FILLER            PIC X(11)     VALUE "*TRAUM_HC*:". 
          02 FILLER            PIC X         VALUE "*".
          02 TRAUM-HC-J        PIC X(19000).
          02 FILLER            PIC XX        VALUE "*,".

       01 LIN-GINEC-HC.
          02 FILLER            PIC X(11)     VALUE "*GINEC_HC*:". 
          02 FILLER            PIC X         VALUE "*".
          02 GINEC-HC-J        PIC X(19000).
          02 FILLER            PIC XX        VALUE "*,".

       01 LIN-ANALISIS-HC.
          02 FILLER                PIC X(14)     VALUE "*ANALISIS_HC*:". 
          02 FILLER                PIC X         VALUE "*".
          02 ANALISIS-HC-J         PIC X(19000).
          02 FILLER                PIC XX        VALUE "*,".

       01 LIN-EXA-GENERAL-HC.
          02 FILLER                PIC X(17)     VALUE "*EXA_GENERAL_HC*:".
          02 FILLER                PIC X         VALUE "*".
          02 EXA-GENERAL-HC-J          PIC X(19000).
          02 FILLER                    PIC XX        VALUE "*,".

       01 LIN-QUIRU-HC.
          02 FILLER            PIC X(11)     VALUE "*QUIRU_HC*:".
          02 FILLER            PIC X         VALUE "*".
          02 QUIRU-HC-J        PIC X(19000).
          02 FILLER            PIC XX        VALUE "*,".

       01 LIN-OBSERVACIONES-9004.
          02 FILLER                PIC X(16)     VALUE "*OBSERVACIONES*:".
          02 FILLER                PIC X         VALUE "*".
          02 OBSERVACIONES-J       PIC X(2850).
          02 FILLER                PIC X        VALUE "*".                  

       LINKAGE SECTION.

       COPY "P:\PROG\PROYECT\FUENTES\ISAPICTX.CBL".

       PROCEDURE DIVISION WITH stdcall LINKAGE USING ISAPI-CTX-CNT.

       DECLARATIVES.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF USU-STAT = "00" 
              CONTINUE 
           ELSE
              MOVE USU-STAT         TO MSJ1-HTML
              MOVE NOM-USUAR-W      TO MSJ2-HTML
              MOVE "HC9004-2"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON HISTORIA-CLINICA.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-HCAPE-W      TO MSJ2-HTML
              MOVE "HC9004-2"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR  
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON HISTORIA-DETALLADA.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-HCDET-W      TO MSJ2-HTML
              MOVE "HC9004-2"       TO MSJ3-HTML
              GO TO ENVIAR2-ERROR  
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" 
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-PACIE-W      TO MSJ2-HTML
              MOVE "HC9004-2"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-OCUPACIONES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-OCUPA-W      TO MSJ2-HTML
              MOVE "HC9004-2"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PROFESIONALES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-PROF-W       TO MSJ2-HTML
              MOVE "HC9004-2"       TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ENTIDADES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-ENTID-W        TO MSJ2-HTML
              MOVE "HC9004-2-ENT"       TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-POS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-POS-W        TO MSJ2-HTML
              MOVE "HC9004-2"       TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-UNID-SERV.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-UNSER-W      TO MSJ2-HTML
              MOVE "HC9004-2"       TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ENFERMEDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-ENFERMEDAD-W TO MSJ2-HTML
              MOVE "HC9004-2"       TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ESPECIALIDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-ESPEC-W TO MSJ2-HTML
              MOVE "HC9004-2"       TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CIUDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-CIU-W       TO MSJ2-HTML
              MOVE "HC9004-2"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.


       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-PLANO-W      TO MSJ2-HTML
              MOVE "HC9004-2"        TO MSJ3-HTML
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
              INTO DIRECTORIO-W, LLAVE-HC-W, IP-SERVIDOR-W
           END-UNSTRING
           INSPECT DIRECTORIO-W CONVERTING "$" TO "\"
           MOVE 0 TO PASO-W.
       
        ASIGNAR-NOMBRES-ARCHIVOS.
           INITIALIZE NOM-USUAR-W NOM-HCAPE-W NOM-HCDET-W NOM-PROF-W
                      NOM-OCUPA-W NOM-ENTID-W NOM-POS-W NOM-UNSER-W 
                      NOM-ENFERMEDAD-W NOM-PACIE-W NOM-ESPEC-W 
                      NOM-CIU-W NOM-PLANO-W

           MOVE "\\" TO NOM-HCAPE-W
           INSPECT NOM-HCAPE-W REPLACING
           FIRST "                    "
               BY IP-SERVIDOR-W
           IF PASO-W = 0
              INSPECT NOM-HCAPE-W REPLACING FIRST "   " BY "\SC"
           ELSE
              INSPECT NOM-HCAPE-W REPLACING FIRST "    " BY "\SC$"
           END-IF

           MOVE NOM-HCAPE-W 
                TO NOM-USUAR-W NOM-HCDET-W NOM-PROF-W
                   NOM-OCUPA-W NOM-ENTID-W NOM-POS-W NOM-UNSER-W 
                   NOM-ENFERMEDAD-W NOM-PACIE-W NOM-ESPEC-W 
                   NOM-CIU-W NOM-PLANO-W
        
           INSPECT NOM-USUAR-W REPLACING
           FIRST "                              "
           BY    DIRECTORIO-W
           INSPECT NOM-USUAR-W REPLACING
           FIRST "                "
           BY    "\CONTROL\ARCHUSU"

           INSPECT NOM-HCAPE-W REPLACING
           FIRST "                                "
           BY    "\NEWCOBOL\HC\DATOS\SC-HCAPER.S17"

           INSPECT NOM-HCDET-W REPLACING 
           FIRST  "                                  "
           BY     "\NEWCOBOL\HC\DATOS\SC-HCDETA-X.S17"

           MOVE ID-HC-W TO ID-EDIT
           INSPECT NOM-HCDET-W REPLACING FIRST "X" BY ID2-EDIT
           
           INSPECT NOM-PACIE-W REPLACING
           FIRST "                              "
           BY    DIRECTORIO-W
           
           INSPECT NOM-PACIE-W REPLACING 
           FIRST "                     "
           BY    "\CONTROL\SC-PACIE.DAT"

           INSPECT NOM-OCUPA-W REPLACING 
           FIRST "                             " 
           BY    "\newcobol\DATOS\SC-OCUPAC.DAT"

           INSPECT NOM-PROF-W REPLACING
           FIRST "                              "
           BY      DIRECTORIO-W.
           INSPECT NOM-PROF-W  REPLACING
           FIRST  "                     "
           BY     "\CONTROL\ARCHPROF.DAT"
                                        
           INSPECT NOM-ENTID-W REPLACING 
           FIRST "                           " 
           BY    "\NEWCOBOL\DATOS\ARCHENT.DAT"

           INSPECT NOM-POS-W  REPLACING
           FIRST  "                            "
           BY     "\NEWCOBOL\DATOS\SC-FARMA.DAT"

           INSPECT NOM-UNSER-W REPLACING
           FIRST   "                              "
           BY      DIRECTORIO-W
           INSPECT NOM-UNSER-W REPLACING 
           FIRST "                     "
           BY    "\CONTROL\SC-UNSER.DAT"

           INSPECT NOM-ENFERMEDAD-W  REPLACING
           FIRST  "                           "
           BY     "\NEWCOBOL\DATOS\ARCHENF.DAT"
           
           INSPECT NOM-ESPEC-W REPLACING 
           FIRST "                           " 
           BY    "\NEWCOBOL\DATOS\ARCHESP.DAT"                                        

           INSPECT NOM-CIU-W REPLACING
           FIRST "                            "
           BY    "\NEWCOBOL\DATOS\ARCHCIUD.DAT"

           INSPECT NOM-PLANO-W  REPLACING
           FIRST "                                                      "
           BY    "\NEWCOBOL\DATOS\JSON\SC4-XXXXXXXXXXXXXXXXXXXXXXXI.JSON"

           INSPECT NOM-PLANO-W REPLACING
           FIRST "XXXXXXXXXXXXXXXXXXXXXXX"
           BY   LLAVE-HC-W.

       ABRIR-ARCHIVOS.    
           OPEN INPUT ARCHIVO-USUARIOS
           IF USU-STAT = 30
               MOVE 1 TO PASO-W
               GO TO ASIGNAR-NOMBRES-ARCHIVOS
           END-IF

           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK
           END-READ
           CLOSE ARCHIVO-USUARIOS
           OPEN  INPUT HISTORIA-CLINICA
           MOVE LLAVE-HC-W TO LLAVE-HC
           READ HISTORIA-CLINICA 
                INVALID KEY
                   INITIALIZE REG-HC
           END-READ
           CLOSE HISTORIA-CLINICA 

           OPEN INPUT ARCHIVO-PACIENTES
           MOVE ID-HC-W TO COD-PACI
           READ ARCHIVO-PACIENTES 
                   INVALID KEY CONTINUE
                   INITIALIZE REG-PACI
            END-READ
            CLOSE ARCHIVO-PACIENTES
           

           OPEN INPUT ARCHIVO-CIUDAD
           MOVE LUGAR-ID-PACI TO COD-CIU

           READ ARCHIVO-CIUDAD
                INVALID KEY
                 MOVE COD-CIU TO NOMBRE-CIU
           END-READ
           CLOSE ARCHIVO-CIUDAD

           OPEN INPUT ARCHIVO-PROFESIONALES
           MOVE MED-HC TO COD-PROF
           READ ARCHIVO-PROFESIONALES
               INVALID KEY CONTINUE
                   INITIALIZE REG-PROF
            END-READ
            CLOSE ARCHIVO-PROFESIONALES

           OPEN INPUT ARCHIVO-UNID-SERV
           MOVE SERV-HC TO CODIGO-UNID-SERV
           READ ARCHIVO-UNID-SERV
              INVALID KEY MOVE CODIGO-UNID-SERV TO DESCRIP-UNID-SERV
              NOT INVALID KEY CONTINUE  
            END-READ
            CLOSE ARCHIVO-UNID-SERV
           
           OPEN INPUT ARCHIVO-POS
           READ ARCHIVO-POS
               INVALID KEY CONTINUE
               INITIALIZE REG-POS
           END-READ
           CLOSE ARCHIVO-POS

           OPEN INPUT ARCHIVO-ENTIDADES
           MOVE EPS-HC TO COD-ENT
           READ ARCHIVO-ENTIDADES WITH NO LOCK
                INVALID KEY
                        MOVE "NO EXISTE ENTIDAD" TO NOMBRE-ENT
           END-READ
           CLOSE ARCHIVO-ENTIDADES.

           OPEN INPUT ARCHIVO-ESPECIALIDAD
           MOVE ESP-PROF(1) TO COD-ESP
           READ ARCHIVO-ESPECIALIDAD WITH NO LOCK
                INVALID KEY
                        MOVE "NO EXISTE ESPECIALIDAD" TO ESP-MED-HC-J
           END-READ
           CLOSE ARCHIVO-ESPECIALIDAD.
    
       LLENAR-DATOS-HC.
       *>    DATOS HC
            MOVE LLAVE-HC-W  TO LLAVE-HC-J
            MOVE MES-HC TO MES-NUM
            PERFORM SELECCIONAR-MES.
            STRING 'DIA: ' DIA-HC ' MES:' MES-LET 'AÑO: ' ANO-HC
            DELIMITED BY SIZE INTO FECHA-HC-J 
            STRING HR-HC ': ' MN-HC DELIMITED BY SIZE
            INTO HORA-HC-J 
            MOVE DESCRIP-UNID-SERV  TO SERV-HC-J
            MOVE DESCRIP-PROF       TO OPER-ELAB-HC-J
            COMPUTE EDAD-HC-J = (ANO-HC)-(ANO-PACI)
            MOVE UNID-EDAD-HC       TO UNID-EDAD-HC-J
            MOVE ACOMPA-PACI        TO ACOMPA-HC-J
            MOVE ID-ACOMPA-HC       TO ID-ACOMPA-HC-J
            MOVE PARENT-ACOMPA-HC   TO PARENT-ACOMPA-HC-J
            MOVE FOLIO-SUC-HC TO SUCURSAL-HC-J
            MOVE FOLIO-NRO-HC TO FOLIO-HC-J
            MOVE 'HISTORIA CLÝNICA DE ONCOLOGÝA' TO TIPO-HC-J
            EVALUATE PLANIFIC-HC
               WHEN "1" MOVE "DIU               " TO PLANIFIC-HC-J
               WHEN "2" MOVE "ORAL              " TO PLANIFIC-HC-J
               WHEN "3" MOVE "BARRERA           " TO PLANIFIC-HC-J
               WHEN "4" MOVE "OTRO              " TO PLANIFIC-HC-J
               WHEN "5" MOVE "NINGUNO           " TO PLANIFIC-HC-J
               WHEN "6" MOVE "DIU + BARRERA     " TO PLANIFIC-HC-J
               WHEN "7" MOVE "IMPL. SUBDERMICO  " TO PLANIFIC-HC-J
               WHEN "8" MOVE "I.SUBDERM+BARRERA " TO PLANIFIC-HC-J
               WHEN "9" MOVE "ORAL + BARRERA    " TO PLANIFIC-HC-J
               WHEN "A" MOVE "INYECTABLE MENSUAL" TO PLANIFIC-HC-J
               WHEN "B" MOVE "INYECTABLE+BARRERA" TO PLANIFIC-HC-J
               WHEN "C" MOVE "INYECTABLE TRIMEST" TO PLANIFIC-HC-J
               WHEN "D" MOVE "TRIMESTRAL+BERRERA" TO PLANIFIC-HC-J
               WHEN "E" MOVE "EMERGENCIA        " TO PLANIFIC-HC-J
               WHEN "F" MOVE "EMERGENCIA+BARRERA" TO PLANIFIC-HC-J
               WHEN "G" MOVE "ESTERILIZACION    " TO PLANIFIC-HC-J
               WHEN "H" MOVE "ESTERILIZA+BARRERA" TO PLANIFIC-HC-J
               WHEN "I" MOVE "NO USA X TRADICION" TO PLANIFIC-HC-J
               WHEN "J" MOVE "NO USA X SALUD    " TO PLANIFIC-HC-J
               WHEN "K" MOVE "NO USA X NEGACION " TO PLANIFIC-HC-J
               WHEN "L" MOVE "COITUS INTERRUPTUS" TO PLANIFIC-HC-J
               WHEN "M" MOVE "METODO DEL RITMO  " TO PLANIFIC-HC-J
               WHEN OTHER 
                        MOVE "NO DECLARA        " TO PLANIFIC-HC-J
            END-EVALUATE
            MOVE MOTIV-HC            TO MOTIV-HC-J
            MOVE UND-PESO-HC        TO UND-PESO-HC-J
            MOVE PESO-HC            TO PESO-HC-J
            MOVE TALLA-HC           TO TALLA-HC-J               
            MOVE IMC-CORP-HC        TO IMC-CORP-HC-J
            MOVE SUP-CORP-HC        TO SUP-CORP-HC-J
            MOVE TEMP-HC            TO TEMP-HC-J 
            MOVE FCARD-HC           TO FCARD-HC-J    
            MOVE FRESP-HC           TO FRESP-HC-J 
            MOVE TENS-HC            TO TENS-HC-J
            MOVE TENS-MEDIA-HC      TO TENS-MEDIA-HC-J   
            MOVE OXIMETRIA-HC       TO OXIMETRIA-HC-J
            MOVE PVC-HC             TO PVC-HC-J
            MOVE VLR-GLASG-HC       TO GLASG-HC-J
            MOVE PER-CEF-HC         TO PER-CEF-HC-J
            MOVE PER-TORA-HC        TO PER-TORA-HC-J
            MOVE PER-ABDO-HC        TO PER-ABDO-HC-J
            EVALUATE TIPO-DIAG-HC
               WHEN "1" MOVE "1-IMPRESION DIAGNOSTICA     " TO TIPO-DIAG-HC-J
               WHEN "2" MOVE "2-CONFIRMADO NUEVO          " TO TIPO-DIAG-HC-J
               WHEN "3" MOVE "3-CONFIRMADO REPETIDO       " TO TIPO-DIAG-HC-J
               WHEN "9" MOVE "9-NO APLICA                 " TO TIPO-DIAG-HC-J
           END-EVALUATE
           IF NIT-USU = SPACES OR LOW-VALUES
               MOVE NIT-ALT TO NIT-USU-HC-J
               MOVE NOMBRE-ALTERNO TO USUAR-HC-J
           ELSE    
               MOVE NOMBRE-USU TO USUAR-HC-J
               MOVE NIT-USU TO NIT-USU-HC-J
           END-IF
           IF NOMBRE-ESP = SPACES OR LOW-VALUES
               MOVE COD-ESP TO ESP-MED-HC-J
           ELSE
               STRING COD-ESP "  " NOMBRE1-ESP NOMBRE2-ESP 
               DELIMITED BY SIZE 
               INTO ESP-MED-HC-J
           END-IF
           MOVE NIT-USU TO NOM-LOGO
           STRING 'HTTP://' IP-SERVIDOR-W '/HICLIN/PROGLOGOS/' NOM-LOGO '.BMP'
           DELIMITED BY SIZE INTO NOM-LOGO-J
           STRING 'HTTP://' IP-SERVIDOR-W '/HICLIN/HCDATOS/' NOM-LOGO '.BMP'
           DELIMITED BY SIZE INTO FIRMA-J
           MOVE DESCRIP-PROF TO REG-MED-HC-J
           OPEN INPUT ARCHIVO-CIUDAD
           MOVE COD-CIU-USU TO COD-CIU
           READ ARCHIVO-CIUDAD
           INVALID KEY
           MOVE "          " TO CIUD-USU-HC-J
           END-READ
           MOVE NOMBRE-CIU TO CIUD-USU-HC-J
           CLOSE ARCHIVO-CIUDAD

            *> RIPS
            EVALUATE ATIENDE-HC
               WHEN "1" MOVE "MEDICO ESPECIALISTA" TO ATIENDE-HC-J
               WHEN "2" MOVE "MEDICO GENERAL     " TO ATIENDE-HC-J
               WHEN "3" MOVE "ENFERMERA          " TO ATIENDE-HC-J
               WHEN "4" MOVE "AUXILIAR ENFERMERIA" TO ATIENDE-HC-J
               WHEN "5" MOVE "TERAPIAS Y OTROS   " TO ATIENDE-HC-J
               WHEN "6" MOVE "ENFERMERA JEFE PYP " TO ATIENDE-HC-J
               WHEN "7" MOVE "SICOLOGIA          " TO ATIENDE-HC-J
               WHEN "8" MOVE "NUTRICIONISTA      " TO ATIENDE-HC-J
               WHEN "9" MOVE "SIN DETERMINAR     " TO ATIENDE-HC-J
               WHEN "A" MOVE "ODONTOLOGO         " TO ATIENDE-HC-J
               WHEN "H" MOVE "HIGIENISTA ORAL    " TO ATIENDE-HC-J
               WHEN "I" MOVE "INSTRUMENTACION    " TO ATIENDE-HC-J
               WHEN "O" MOVE "OPTOMETRA          " TO ATIENDE-HC-J
               WHEN "T" MOVE "TRABAJO SOCIAL     " TO ATIENDE-HC-J
            END-EVALUATE            
           OPEN INPUT ARCHIVO-ENTIDADES
           MOVE EPS-PACI TO COD-ENT
           READ ARCHIVO-ENTIDADES WITH NO LOCK
                INVALID KEY
                        MOVE "NO EXISTE ENTIDAD" TO NOMBRE-ENT
           END-READ
           MOVE NOMBRE-ENT   TO EPS-PACI-J
            MOVE NOMBRE-ENT         TO EPS-ATIENDE-HC-J  
           CLOSE ARCHIVO-ENTIDADES
            MOVE TRIAGE-HC          TO TRIAGE-HC-J
            EVALUATE CAUSA-HC
               WHEN 1  MOVE "ACCIDENTE TRABAJO " TO CAUSA-HC-J
               WHEN 2  MOVE "ACCIDENTE TRANSITO" TO CAUSA-HC-J
               WHEN 3  MOVE "ACCIDENTE RABICO  " TO CAUSA-HC-J
               WHEN 4  MOVE "ACCIDENTE OFIDICO " TO CAUSA-HC-J
               WHEN 5  MOVE "OTRO ACCIDENTE    " TO CAUSA-HC-J
               WHEN 6  MOVE "EVENTO CATASTROFIC" TO CAUSA-HC-J
               WHEN 7  MOVE "LESION AGRESION   " TO CAUSA-HC-J
               WHEN 8  MOVE "LESION AUTO INFLIG" TO CAUSA-HC-J
               WHEN 9  MOVE "SOSP.MALTRATO FIS " TO CAUSA-HC-J
               WHEN 10 MOVE "SOSP.ABUSO SEXUAL " TO CAUSA-HC-J
               WHEN 11 MOVE "SOSP.VIOLENCIA SEX" TO CAUSA-HC-J
               WHEN 12 MOVE "SOSP.MALTRATO EMOC" TO CAUSA-HC-J
               WHEN 13 MOVE "ENFERMEDAD GENERAL" TO CAUSA-HC-J
               WHEN 14 MOVE "ENFERMEDAD PROFES." TO CAUSA-HC-J
               WHEN 15 MOVE "OTRA CAUSA        " TO CAUSA-HC-J
               WHEN OTHER MOVE "SIN CAUSA         " TO CAUSA-HC-J
            END-EVALUATE                
            EVALUATE FINALID-HC
               WHEN 1  MOVE "ATENCION PARTO    " TO FINALID-HC-J
               WHEN 2  MOVE "ATENCION REC.NACID" TO FINALID-HC-J
               WHEN 3  MOVE "ATENC.PLANIF.FAMIL" TO FINALID-HC-J
               WHEN 4  MOVE "DET.ALT CRECIM <10" TO FINALID-HC-J
               WHEN 5  MOVE "DET.ALT.DESA.JOVEN" TO FINALID-HC-J
               WHEN 6  MOVE "DET.ALT.EMBARAZO  " TO FINALID-HC-J
               WHEN 7  MOVE "DET.ALT. ADULTO   " TO FINALID-HC-J
               WHEN 8  MOVE "DET.ALT.AGUD.VISUA" TO FINALID-HC-J
               WHEN 9  MOVE "DET.ENFERM.PROFES." TO FINALID-HC-J
               WHEN 10 MOVE "NO APLICA         " TO FINALID-HC-J
               WHEN 11 MOVE "PATOLOGIA CRONICA " TO FINALID-HC-J
               WHEN OTHER MOVE "SIN FINALIDAD     " TO FINALID-HC-J
            END-EVALUATE    
            EVALUATE EMBARAZO-HC
                WHEN 1     MOVE "1ER TRIM. EMBA" TO EMBARAZO-HC-J
                WHEN 2     MOVE "2DO TRIM. EMBA" TO EMBARAZO-HC-J
                WHEN 3     MOVE "3ER TRIM. EMBA" TO EMBARAZO-HC-J
                WHEN 4     MOVE "NO DECLARA    " TO EMBARAZO-HC-J
                WHEN 9     IF  SEXO-PACI = "F" CONTINUE
                             ELSE
                             MOVE "NO APLICA    "  TO EMBARAZO-HC-J
                           END-IF  
            END-EVALUATE
            EVALUATE ESTADO-SAL-HC
                WHEN " " MOVE  "            " TO ESTADO-SAL-HC-J     
                WHEN "1" MOVE  "VIVO   (a)  " TO ESTADO-SAL-HC-J    
                WHEN "2" MOVE  "MUERTO (a)  " TO ESTADO-SAL-HC-J    
                WHEN "3" MOVE  "REMITIDO    " TO ESTADO-SAL-HC-J    
                WHEN "4" MOVE  "HOSPITALIZAD" TO ESTADO-SAL-HC-J    
                WHEN "5" MOVE  "OBSERVACION " TO ESTADO-SAL-HC-J    
                WHEN "6" MOVE  "NO APLICA   " TO ESTADO-SAL-HC-J
            END-EVALUATE.             

       LLENAR-DATOS-PACI.
           STRING 1ER-APEL-PACI 2DO-APEL-PACI NOMBRE-PACI DELIMITED BY SIZE
           INTO NOMBRE-PACI-J
           MOVE NOMBRE-CIU TO PROCEDE-PACI-J
           EVALUATE ETNIA-PACI
               WHEN 1     MOVE "INDIGENA " TO ETNIA-HC-J
               WHEN 2     MOVE "RAIZAL   " TO ETNIA-HC-J
               WHEN 3     MOVE "GITANO   " TO ETNIA-HC-J
               WHEN 4     MOVE "AFROCOLOM" TO ETNIA-HC-J
               WHEN 5     MOVE "ROM      " TO ETNIA-HC-J
               WHEN 6     MOVE "MESTIZO  " TO ETNIA-HC-J
               WHEN 9     MOVE "NO APLICA" TO ETNIA-HC-J
               WHEN OTHER MOVE SPACES      TO ETNIA-HC-J
           END-EVALUATE
           MOVE COD-PACI TO COD-PACI-J
           MOVE MES-PACI TO MES-NUM
           PERFORM SELECCIONAR-MES.
           STRING 'DIA: ' DIA-PACI ' MES:' MES-LET 'AÑO: ' ANO-PACI
           DELIMITED BY SIZE INTO NACIMIENTO-PACI-J  
           OPEN INPUT ARCHIVO-OCUPACIONES
           IF OCUP-PACI = SPACES OR LOW-VALUE
               MOVE "SIN OCUPACION" TO OCUP-PACI-J
           ELSE
               MOVE  OCUP-PACI TO  OCUP-PACI-W
               MOVE OCUP-PACI-W TO COD-OCU
               READ ARCHIVO-OCUPACIONES
                 INVALID KEY MOVE "NO SE ENCONTRO OCUPACION" TO OCUP-PACI-J
                 NOT INVALID KEY
                 MOVE NOMBRE-OCU TO OCUP-PACI-J
               END-READ
            END-IF
           CLOSE ARCHIVO-OCUPACIONES     
           MOVE DIRECC-PACI  TO DIREC-PACI-J
           INSPECT DIREC-PACI-J REPLACING ALL "'" BY " "

           IF SEXO-PACI IS NOT = SPACE OR LOW-VALUE
              AND SEXO-PACI = "F"
                 MOVE "FEMENINO " TO SEXO-PACI-J
           ELSE
                 MOVE "MASCULINO" TO SEXO-PACI-J
           END-IF                      
           EVALUATE TIPO-PACI
             WHEN "C"   MOVE "CONTRIBUTIVO" TO TIPO-PACI-J
             WHEN "S"   MOVE "SUBSIDIADO  " TO TIPO-PACI-J
             WHEN "V"   MOVE "VINCULADO   " TO TIPO-PACI-J
             WHEN "P"   MOVE "PARTICULAR  " TO TIPO-PACI-J
             WHEN "O"   MOVE "OTRO TIPO   " TO TIPO-PACI-J
             WHEN "D"   MOVE "DESPLAZ CONT" TO TIPO-PACI-J
             WHEN "E"   MOVE "DESPLAZ SUBS" TO TIPO-PACI-J
             WHEN "F"   MOVE "DESPLAZ VINC" TO TIPO-PACI-J
             WHEN OTHER MOVE  SPACES        TO TIPO-PACI-J
           END-EVALUATE
           EVALUATE TIPO-AFIL-PACI
             WHEN 1  MOVE "COTIZANTE     "  TO TIPO-AFIL-PACI-J
             WHEN 2  MOVE "BENEFICIARIO  "  TO TIPO-AFIL-PACI-J
             WHEN 3  MOVE "COT.PENSIONADO"  TO TIPO-AFIL-PACI-J
             WHEN 4  MOVE "UPC ADICIONAL "  TO TIPO-AFIL-PACI-J
             WHEN 5  MOVE "CABEZA FAMILIA"  TO TIPO-AFIL-PACI-J
             WHEN 6  MOVE "GRUPO FAMILIAR"  TO TIPO-AFIL-PACI-J
             WHEN 0  MOVE "SIN DETERMINAR"  TO TIPO-AFIL-PACI-J
             WHEN OTHER  MOVE SPACES        TO TIPO-AFIL-PACI-J
           END-EVALUATE
           MOVE RH-PACI TO RH-PACI-J   
           MOVE GRP-SANG-PACI TO GRP-SANG-PACI-J
           EVALUATE EST-CIV-PACI
           WHEN "S"   MOVE "SOLTERO"  TO EST-CIV-PACI-J
           WHEN "C"   MOVE "CASADO "  TO EST-CIV-PACI-J
           WHEN "U"   MOVE "U.LIBRE"  TO EST-CIV-PACI-J
           WHEN "D"   MOVE "SEPARADO" TO EST-CIV-PACI-J
           WHEN "V"   MOVE "VIUDO  "  TO EST-CIV-PACI-J
           WHEN OTHER MOVE SPACES     TO EST-CIV-PACI-J
           END-EVALUATE
           MOVE TELEFONO-PACI         TO TELEFONO-PACI-J  
           OPEN INPUT ARCHIVO-ENTIDADES
           MOVE EPS-PACI TO COD-ENT
           READ ARCHIVO-ENTIDADES WITH NO LOCK
                INVALID KEY
                        MOVE "NO EXISTE ENTIDAD" TO NOMBRE-ENT
           END-READ
           MOVE NOMBRE-ENT   TO EPS-PACI-J
           CLOSE ARCHIVO-ENTIDADES.
           
       CARGAR-DATOS-WS9004.
           OPEN INPUT HISTORIA-DETALLADA
      * LEER DETALLES WS9004     
           MOVE LLAVE-HC-W TO LLAVE-HC-DETHC
           MOVE 9004 TO COD-DETHC
           READ HISTORIA-DETALLADA
               INVALID KEY INITIALIZE DETALLE-DETHC
           END-READ
           MOVE DETALLE-DETHC TO DATOS-WS9004         
      * CONTROL GENERAL
           EVALUATE PORC-ACT-FACTOR-W
               WHEN 0  MOVE  "< 1%      "   TO PORC-ACT-FACTOR-J
               WHEN 1  MOVE  "> 1%      "   TO PORC-ACT-FACTOR-J
               WHEN 2  MOVE  "> 5 Y < 40%"  TO PORC-ACT-FACTOR-J
               WHEN 3  MOVE  "DESCONOCIDO"  TO PORC-ACT-FACTOR-J
               WHEN 4  MOVE  "NO APLICA  "  TO PORC-ACT-FACTOR-J
           END-EVALUATE    
           EVALUATE INHIBIDORES-W
               WHEN 0     MOVE  "BAJA RTA O <5 UB       "  TO INHIBIDORES-J
               WHEN 1     MOVE  "ALTA RTA O > 5 UB      "  TO INHIBIDORES-J
               WHEN 2     MOVE  "NO PRESENTA INHIBIDORES"  TO INHIBIDORES-J
               WHEN 3     MOVE  "DESCONOCIDO            "  TO INHIBIDORES-J
               WHEN 9     MOVE  "NO APLICA              "  TO INHIBIDORES-J
           END-EVALUATE
            MOVE MES-ULT-REPOR-INHIB-9004 TO MES-NUM
            PERFORM SELECCIONAR-MES.
            STRING 'DIA: ' DIA-ULT-REPOR-INHIB-9004 ' MES:' MES-LET 'AÑO: ' 
                ANO-ULT-REPOR-INHIB-9004
            DELIMITED BY SIZE INTO FECHA-REPOR-INHIB-J
           EVALUATE HA-RECIBIDO-ITI-W
               WHEN "S"  MOVE "SI       " TO HA-RECIBIDO-ITI-J
               WHEN "N"  MOVE "NO       " TO HA-RECIBIDO-ITI-J
               WHEN "9"  MOVE "NO APLICA" TO HA-RECIBIDO-ITI-J
           END-EVALUATE
           EVALUATE ESTA-RECIBIENDO-ITI-W
               WHEN "S"  MOVE "SI       "  TO ESTA-RECIBIENDO-ITI-J
               WHEN "N"  MOVE "NO       "  TO ESTA-RECIBIENDO-ITI-J
               WHEN "9"  MOVE "NO APLICA"  TO ESTA-RECIBIENDO-ITI-J   
           END-EVALUATE
           MOVE TIEMPO-EN-ITI-W  TO  TIEMPO-EN-ITI-J
           EVALUATE HEMARTROSIS-W
               WHEN "S"  MOVE "SI       " TO HEMARTROSIS-J
               WHEN "N"  MOVE "NO       " TO HEMARTROSIS-J
               WHEN "9"  MOVE "NO APLICA" TO HEMARTROSIS-J
           END-EVALUATE
           EVALUATE ARTROPATIA-HEMO-CRON-W
               WHEN "S"  MOVE "SI       " TO ARTROPATIA-HEMO-CRON-J
               WHEN "N"  MOVE "NO       " TO ARTROPATIA-HEMO-CRON-J
               WHEN "9"  MOVE "NO APLICA" TO ARTROPATIA-HEMO-CRON-J
           END-EVALUATE
           MOVE NRO-ART-COMPRO-W       TO NRO-ART-COMPRO-J
           EVALUATE VHC-W
               WHEN "S"  MOVE "SI       " TO VHC-J
               WHEN "N"  MOVE "NO       " TO VHC-J
               WHEN "9"  MOVE "NO APLICA" TO VHC-J
           END-EVALUATE
           EVALUATE VHB-W
               WHEN "S"  MOVE "SI       " TO VHB-J
               WHEN "N"  MOVE "NO       " TO VHB-J
               WHEN "9"  MOVE "NO APLICA" TO VHB-J
           END-EVALUATE
           EVALUATE VIH-W
               WHEN "S"  MOVE "SI       " TO VIH-J
               WHEN "N"  MOVE "NO       " TO VIH-J
               WHEN "9"  MOVE "NO APLICA" TO VIH-J
           END-EVALUATE
      * HEMORRAGIAS Y SANGRADOS
           EVALUATE SAN-ILIOPSOAS-W
               WHEN "S"  MOVE "SI       " TO SAN-ILIOPSOAS-J
               WHEN "N"  MOVE "NO       " TO SAN-ILIOPSOAS-J
               WHEN "9"  MOVE "NO APLICA" TO SAN-ILIOPSOAS-J   
           END-EVALUATE
           EVALUATE SAN-TEJIDOS-BLANDOS-W
               WHEN "S"  MOVE "SI       " TO SAN-TEJIDOS-BLANDOS-J
               WHEN "N"  MOVE "NO       " TO SAN-TEJIDOS-BLANDOS-J
               WHEN "9"  MOVE "NO APLICA" TO SAN-TEJIDOS-BLANDOS-J
           END-EVALUATE
           EVALUATE SAN-INTRACRANEAL-W
               WHEN "S"  MOVE "SI      "  TO SAN-INTRACRANEAL-J
               WHEN "N"  MOVE "NO      "  TO SAN-INTRACRANEAL-J
               WHEN "9"  MOVE "NO APLICA" TO SAN-INTRACRANEAL-J   
           END-EVALUATE
           EVALUATE SAN-CUELLO-GARGANTA-W
               WHEN "S"  MOVE "SI       " TO SAN-CUELLO-GARGANTA-J
               WHEN "N"  MOVE "NO       " TO SAN-CUELLO-GARGANTA-J
               WHEN "9"  MOVE "NO APLICA" TO SAN-CUELLO-GARGANTA-J
           END-EVALUATE
           EVALUATE SAN-ORAL-W
               WHEN "S"  MOVE "SI       " TO SAN-ORAL-J
               WHEN "N"  MOVE "NO       " TO SAN-ORAL-J
               WHEN "9"  MOVE "NO APLICA" TO SAN-ORAL-J   
           END-EVALUATE
           EVALUATE SAN-OTRAS-W
               WHEN "S"  MOVE "SI       " TO SAN-OTRAS-J
               WHEN "N"  MOVE "NO       " TO SAN-OTRAS-J
               WHEN "9"  MOVE "NO APLICA" TO SAN-OTRAS-J
           END-EVALUATE    
      * ANTECEDENTES FAMILIARES 
           EVALUATE ANTE-FAMIL-W
               WHEN 0 MOVE "NO TIENE ANTECEDENTES FAMILIARES" TO ANTE-FAMIL-J
               WHEN 1 MOVE "MADRE                           " TO ANTE-FAMIL-J
               WHEN 2 MOVE "PADRE                           " TO ANTE-FAMIL-J
               WHEN 3 MOVE "1 HERMANO                       " TO ANTE-FAMIL-J
               WHEN 4 MOVE "2 HERMANOS                      " TO ANTE-FAMIL-J
               WHEN 5 MOVE "1 HERMANA                       " TO ANTE-FAMIL-J
               WHEN 6 MOVE "2 HERMANAS                      " TO ANTE-FAMIL-J
               WHEN 7 MOVE "PADRE Y MADRE                   " TO ANTE-FAMIL-J
               WHEN 8 MOVE "PADRE Y 1 HERMANO               " TO ANTE-FAMIL-J
               WHEN 9 MOVE "PADRE Y 2 HERMANOS              " TO ANTE-FAMIL-J
               WHEN 10 MOVE "PADRE Y 1 HERMANA              " TO ANTE-FAMIL-J
               WHEN 11 MOVE "PADRE Y 2 HERMANAS             " TO ANTE-FAMIL-J
               WHEN 12 MOVE "MADRE Y 1 HEMANO               " TO ANTE-FAMIL-J
               WHEN 13 MOVE "MADRE Y 2 HERMANOS             " TO ANTE-FAMIL-J
               WHEN 14 MOVE "MADRE Y 1 HERMANA              " TO ANTE-FAMIL-J
               WHEN 15 MOVE "MADRE Y 2 HERMANAS             " TO ANTE-FAMIL-J
               WHEN 16 MOVE "1 SOBRINA                      " TO ANTE-FAMIL-J
               WHEN 17 MOVE "2 SOBRINAS                     " TO ANTE-FAMIL-J
               WHEN 18 MOVE "1 SOBRINO                      " TO ANTE-FAMIL-J
               WHEN 19 MOVE "2 SOBRINOS                     " TO ANTE-FAMIL-J
               WHEN 20 MOVE "DESCONOCIDO                    " TO ANTE-FAMIL-J
               WHEN 99 MOVE "NO APLICA                      " TO ANTE-FAMIL-J
           END-EVALUATE
      * ANTECEDENTES MÉDICOS.
           MOVE NRO-ATEN-URG-HEMO-W    TO  NRO-ATEN-URG-HEMO-J
           MOVE NRO-EVEN-HOSPI-HEMO-W  TO  NRO-EVEN-HOSPI-HEMO-J
           MOVE NRO-HEMAR-ESPON-12MES-W TO  NRO-HEMAR-ESPON-12MES-J
           MOVE NRO-HEMAR-TRAUM-12MES-W TO  NRO-HEMAR-TRAUM-12MES-J
           MOVE NRO-OTR-HEMOR-12MES-W   TO  NRO-OTR-HEMOR-12MES-J
      * ANTECEDENTES-QUIRURGICOS.
           EVALUATE REEMPLAZOS-ARTICULARES-W
               WHEN "S"  MOVE "SI       " TO REEMPLAZOS-ARTICULARES-J
               WHEN "N"  MOVE "NO       " TO REEMPLAZOS-ARTICULARES-J
               WHEN "9"  MOVE "NO APLICA" TO REEMPLAZOS-ARTICULARES-J
           END-EVALUATE
           EVALUATE REEMPLAZOS-ART-12MES-W
               WHEN "S"  MOVE "SI       " TO REEMPLAZOS-ART-12MES-J
               WHEN "N"  MOVE "NO       " TO REEMPLAZOS-ART-12MES-J
               WHEN "9"  MOVE "NO APLICA" TO REEMPLAZOS-ART-12MES-J
           END-EVALUATE
           MOVE NRO-HEMOR-PROC-12MES-W TO NRO-HEMOR-PROC-12MES-J
      * ANTECEDENTES-FARMACOLOGICOS.
           EVALUATE FACTOR-RECIBIDO-W
               WHEN 0  MOVE "FVIII                  " TO FACTOR-RECIBIDO-J
               WHEN 1  MOVE "FIX                    " TO FACTOR-RECIBIDO-J
               WHEN 2  MOVE "PLASMA FRESCO CONGELADO" TO FACTOR-RECIBIDO-J
               WHEN 3  MOVE "CRIOPRECIPITADO        " TO FACTOR-RECIBIDO-J
               WHEN 4  MOVE "DESCONOCIDO            " TO FACTOR-RECIBIDO-J
               WHEN 5  MOVE "NO RECIBIDO            " TO FACTOR-RECIBIDO-J
               WHEN 9  MOVE "NO APLICA              " TO FACTOR-RECIBIDO-J
           END-EVALUATE
           EVALUATE ESQUEMA-RECIBIDO-W
               WHEN 0  MOVE "A DEMANDA         "            TO ESQUEMA-RECIBIDO-J
               WHEN 1  MOVE "PROF. PRIMARIA    "       TO ESQUEMA-RECIBIDO-J
               WHEN 2  MOVE "PROF. SECUNDARIA  "     TO ESQUEMA-RECIBIDO-J
               WHEN 3  MOVE "PROF. TERCIARIA   "      TO ESQUEMA-RECIBIDO-J
               WHEN 4  MOVE "PROF. INTERMITENTE"   TO ESQUEMA-RECIBIDO-J
               WHEN 5  MOVE "SIN TRATAMIENTO   "      TO ESQUEMA-RECIBIDO-J
               WHEN 6  MOVE "DESCONOCIDO       "          TO ESQUEMA-RECIBIDO-J
               WHEN 9  MOVE "NO APLICA         "            TO ESQUEMA-RECIBIDO-J
           END-EVALUATE
           EVALUATE ANAFILAXIS-W
               WHEN "S"  MOVE "SI       "        TO ANAFILAXIS-J
               WHEN "N"  MOVE "NO       "        TO ANAFILAXIS-J
               WHEN "9"  MOVE "NO APLICA" TO ANAFILAXIS-J
           END-EVALUATE      
           MOVE ANO-INI-1ER-TRAT-W   TO   ANO-INI-1ER-TRAT-J
           MOVE MES-INI-1ER-TRAT-W   TO   MES-INI-1ER-TRAT-J
           MOVE DIA-INI-1ER-TRAT-W   TO   DIA-INI-1ER-TRAT-J
           OPEN INPUT ARCHIVO-POS
           IF CUM-CAUSA-ANAFIL-W = SPACES OR LOW-VALUE
               MOVE "NO SE SUMINISTRO MEDICAMENTO POS O NOPOS" TO CUM-CAUSA-ANAFIL-J
           ELSE        
               MOVE CUM-CAUSA-ANAFIL-W TO LLAVE-POS
               READ ARCHIVO-POS
                   INVALID KEY MOVE LLAVE-POS
                                TO CUM-CAUSA-ANAFIL-J
                   NOT INVALID KEY 
        
                       IF (GRUPO-POS = "PO" OR GRUPO-POS = "NP")
                           MOVE  NOMBRE-POS TO CUM-CAUSA-ANAFIL-J
                       END-IF
               END-READ
            END-IF   
            
      * ANTECEDENTES-TRAUMATICOS.
            EVALUATE FRACT-OSTEO-W
               WHEN "S"  MOVE "SI       "        TO FRACT-OSTEO-J
               WHEN "N"  MOVE "NO       "        TO FRACT-OSTEO-J
               WHEN "9"  MOVE "NO APLICA" TO FRACT-OSTEO-J
           END-EVALUATE
           EVALUATE PSEUDOTUMORES-W
               WHEN "S"  MOVE "SI       "        TO PSEUDOTUMORES-J
               WHEN "N"  MOVE "NO       "        TO PSEUDOTUMORES-J
               WHEN "9"  MOVE "NO APLICA" TO PSEUDOTUMORES-J
           END-EVALUATE
           MOVE NRO-HEMO-TRAUM-ULT-12MES-W  TO NRO-HEMO-TRAUM-ULT-12MES-J
      * ANTECEDENTES-GINECO-OBSTETRICOS.    
            EVALUATE PLAN-FAMIL-W
               WHEN  0  MOVE "PLANIFICACION                      "  TO PLAN-FAMIL-J
               WHEN  1  MOVE "CONSEJERIA GENETICA                "  TO PLAN-FAMIL-J
               WHEN  2  MOVE "PLANIFICACION Y CONSEJERIA GENETICA"  TO PLAN-FAMIL-J
               WHEN  3  MOVE "NO APLICA                          "  TO PLAN-FAMIL-J
               WHEN  4  MOVE "NINGUNO                            "  TO PLAN-FAMIL-J
            END-EVALUATE   
           MOVE EMBARAZO-HC-J TO  SW-EMBAR-W-J
      * DIAGNOSTICO-PLAN.
           EVALUATE UNID-EDAD-DX-W
               WHEN "A"    MOVE "AÑOS    " TO UNID-EDAD-DX-J
               WHEN "M"    MOVE "MES     "  TO UNID-EDAD-DX-J
               WHEN "D"    MOVE "DIA     "  TO UNID-EDAD-DX-J
               WHEN OTHER  MOVE "SIN DATO" TO UNID-EDAD-DX-J
           END-EVALUATE
           EVALUATE MOTIVO-PRUEBA-DX-W
               WHEN 0 MOVE "MADRE PORTADORA CONOCIDA"  TO MOTIVO-PRUEBA-DX-J
               WHEN 1 MOVE "OTRO HISTORIAL FAMILIAR "  TO MOTIVO-PRUEBA-DX-J
               WHEN 2 MOVE "SINTOMA HEMORRAGICO     "  TO MOTIVO-PRUEBA-DX-J
               WHEN 3 MOVE "OTRO                    "  TO MOTIVO-PRUEBA-DX-J
               WHEN 4 MOVE "DESCONOCIDO             "  TO MOTIVO-PRUEBA-DX-J
           END-EVALUATE
           IF VLR-EDAD-DX-W = SPACES
               MOVE "   "  TO VLR-EDAD-DX-J
            ELSE
               MOVE VLR-EDAD-DX-W TO VLR-EDAD-DX-J
            END-IF
           MOVE MES-DX-9004 TO MES-NUM
           PERFORM SELECCIONAR-MES.
           STRING 'DIA: ' DIA-DX-9004 ' MES:' MES-LET ' AÑO:' ANO-DX-9004
           DELIMITED BY SIZE INTO FECHA-DX-J
           EVALUATE TIPO-DEF-DX-W
              WHEN "0" MOVE "FACTOR VIII (HEMOFILIA A)" TO TIPO-DEF-DX-J
              WHEN "1" MOVE "FACTOR IX (HEMOFILIA B)  " TO TIPO-DEF-DX-J
              WHEN "2" MOVE "PORTADOR / PORTADORA     " TO TIPO-DEF-DX-J
              WHEN "3" MOVE "VON WILEBRAND            " TO TIPO-DEF-DX-J
              WHEN "4" MOVE "FIBRINOGENO              " TO TIPO-DEF-DX-J
              WHEN "5" MOVE "PROTOMBINA               " TO TIPO-DEF-DX-J
              WHEN "6" MOVE "FV Y FVIII               " TO TIPO-DEF-DX-J
              WHEN "8" MOVE "FVII                     " TO TIPO-DEF-DX-J
              WHEN "9" MOVE "FX                       " TO TIPO-DEF-DX-J
              WHEN "A" MOVE "FXI                      " TO TIPO-DEF-DX-J
              WHEN "B" MOVE "FXIII                    " TO TIPO-DEF-DX-J
           END-EVALUATE
           EVALUATE SEVERIDAD-DX-W
              WHEN 0  MOVE "LEVE       "  TO SEVERIDAD-DX-J
              WHEN 1  MOVE "MODERADO   "  TO SEVERIDAD-DX-J
              WHEN 2  MOVE "SEVERO"  TO SEVERIDAD-DX-J
              WHEN 3  MOVE "DESCONOCIDO"  TO SEVERIDAD-DX-J
              WHEN 9  MOVE "NO APLICA  "  TO SEVERIDAD-DX-J
           END-EVALUATE
           EVALUATE PROF-LIDER-ATENCION-W
              WHEN 0  MOVE "HEMATOLOGO       "  TO PROF-LIDER-ATENCION-J
              WHEN 1  MOVE "MEDICO GENERAL   "  TO PROF-LIDER-ATENCION-J
              WHEN 2  MOVE "MEDICO FAMILIAR  "  TO PROF-LIDER-ATENCION-J
              WHEN 3  MOVE "MEDICO INTERNISTA"  TO PROF-LIDER-ATENCION-J
              WHEN 4  MOVE "ORTOPEDISTA      "  TO PROF-LIDER-ATENCION-J
              WHEN 9  MOVE "NO APLICA        "  TO PROF-LIDER-ATENCION-J
           END-EVALUATE
           MOVE NRO-CON-HEMATOLOGIA-W  TO   NRO-CON-HEMATOLOGIA-J
           MOVE NRO-CON-ORTOPEDIA-W    TO   NRO-CON-ORTOPEDIA-J
           EVALUATE INTER-PROF-ENFER-W
              WHEN "S" MOVE "SI       " TO INTER-PROF-ENFER-J
              WHEN "N" MOVE "NO       " TO INTER-PROF-ENFER-J
              WHEN "9" MOVE "NO APLICA" TO INTER-PROF-ENFER-J
           END-EVALUATE   
           MOVE NRO-CON-ODONTOLOGIA-W TO NRO-CON-ODONTOLOGIA-J
           MOVE NRO-CON-NUTRICION-W TO NRO-CON-NUTRICION-J 
           MOVE NRO-CON-T-SOCIAL-W TO NRO-CON-T-SOCIAL-J
           MOVE NRO-CON-FISIATRIA-W TO NRO-CON-FISIATRIA-J
           MOVE NRO-CON-PSICOLOGIA-W TO NRO-CON-PSICOLOGIA-J
           MOVE NRO-CON-QUIM-FARMA-W TO NRO-CON-QUIM-FARMA-J
           MOVE NRO-CON-FISIOTERAPIA-W TO NRO-CON-FISIOTERAPIA-J
      * PLAN-MANEJO.
           EVALUATE FACTOR-RECIBIDO-ACT-W
               WHEN 0 MOVE "CONCENTRADO FACTOR VIII                                 " 
                      TO FACTOR-RECIBIDO-ACT-J
               WHEN 1 MOVE "CONCENTRADO FACTOR IX                                   "           
                      TO FACTOR-RECIBIDO-ACT-J
               WHEN 2 MOVE "FVIIRa (FACTOR VIII RECOMBINANTE ACTIVADO)              " 
                      TO FACTOR-RECIBIDO-ACT-J
               WHEN 3 MOVE "CCPa (CONCENTRADO COMPLEJO DE PROTOMBINA ACTIVADO)      "
                      TO FACTOR-RECIBIDO-ACT-J
               WHEN 4 MOVE "PLASMA FRESCO CONGELADO                                 "
                      TO FACTOR-RECIBIDO-ACT-J
               WHEN 5 MOVE "CRIOPRECIPITADO                                         " 
                      TO FACTOR-RECIBIDO-ACT-J
               WHEN 6 MOVE "NO APLICA                                               " 
                      TO FACTOR-RECIBIDO-ACT-J
               WHEN 7 MOVE "PACIENTE CON HEMOFILIA NO RECIBE TRATAMIENTO            " 
                      TO FACTOR-RECIBIDO-ACT-J
               WHEN 9 
                MOVE "NO APLICA USUARIO CON COAGULOPATIA DIFRERETE A HEMOFILIA" 
                TO FACTOR-RECIBIDO-ACT-J
            END-EVALUATE
           EVALUATE ESQUEMA-RECIBIDO-ACT-W
               WHEN 0 MOVE "A DEMANDA                                               " 
                       TO ESQUEMA-RECIBIDO-ACT-J
               WHEN 1 MOVE "PROF. PRIMARIA                                          " 
                       TO ESQUEMA-RECIBIDO-ACT-J
               WHEN 2 MOVE "PROF. SECUNDARIA                                        " 
                       TO ESQUEMA-RECIBIDO-ACT-J
               WHEN 3 MOVE "PROF. TERCIARIA                                         " 
                       TO ESQUEMA-RECIBIDO-ACT-J
               WHEN 4 MOVE "PROF. INTERMITENTE                                      " 
                       TO ESQUEMA-RECIBIDO-ACT-J
               WHEN 5 
                      MOVE "USUARIO EN PROFILAXIS POR TRAUMA INTER. QUIRURGICA      "
                     TO ESQUEMA-RECIBIDO-ACT-J
               WHEN 9 MOVE "NO APLICA USUARIO CON COAGULOPATIA DIFRERETE A HEMOFILIA" 
                     TO ESQUEMA-RECIBIDO-ACT-J
            END-EVALUATE
            EVALUATE ULT-DOSIS-ACT-W
               WHEN 9998 MOVE "NO APLICA USUARIO HEMOFILICO CON MANEJO A DEMANDA        " 
                         TO ULT-DOSIS-ACT-J
               WHEN 9998 MOVE "NO APLICA, USUARIO CON COAGULOPATIA DIFERENTE A HEMOFILIA" 
                         TO ULT-DOSIS-ACT-J
               WHEN OTHER MOVE ULT-DOSIS-ACT-W TO ULT-DOSIS-ACT-J
            END-EVALUATE
            EVALUATE FRECUENCIA-SEMANA-W
               WHEN 0 MOVE "NO APLICA USUARIO HEMOFILICO NO RECIBE PROFILAXIS" 
                      TO FRECUENCIA-SEMANA-J
               WHEN 1 MOVE "1 VEZ POR SEMANA                                 " 
                   TO FRECUENCIA-SEMANA-J
               WHEN 2 MOVE "2 VECES POR SEMANA                               " 
                   TO FRECUENCIA-SEMANA-J
               WHEN 3 MOVE "3 VECES POR SEMANA                               " 
                   TO FRECUENCIA-SEMANA-J
               WHEN 3 MOVE "4 VECES POR SEMANA                               " 
                   TO FRECUENCIA-SEMANA-J
               WHEN 4 MOVE "MAS DE 4 VECES POR SEMANA                        " 
                   TO FRECUENCIA-SEMANA-J
               WHEN 4 MOVE "NO APLICA, USUARIO CON COAGULOPATIA DIF.HEMOFILIA" 
                   TO FRECUENCIA-SEMANA-J
            END-EVALUATE
           EVALUATE NRO-UI-DL-UNID-W
               WHEN 9998  MOVE "NO APLICA USUARIO HEMOFILICO CON MANEJO EN PROFILAXIS   "
                   TO NRO-UI-DL-UNID-J
               WHEN 9999  MOVE "NO APLICA USUARIO CON COAGULOPATIA DIFERENTE A HEMOFILIA"
                   TO NRO-UI-DL-UNID-J
                WHEN OTHER
                   MOVE NRO-UI-DL-UNID-W TO NRO-UI-DL-UNID-J   
           END-EVALUATE
           EVALUATE NRO-APL-FACTOR-W
               WHEN 0  MOVE "NO APLICA USUARIO HEMOFILICO NO RECIBE MANEJO A DEMANDA    "
                   TO NRO-APL-FACTOR-J
               WHEN 9999 MOVE "NO APLICA, USUARIO CON COAGULOPATIA DIFERENTE A HEMOFILIA"
                   TO NRO-APL-FACTOR-J
                WHEN OTHER
                   MOVE NRO-APL-FACTOR-W TO NRO-APL-FACTOR-J   
           END-EVALUATE
           EVALUATE  MODALIDAD-APL-TRAT-W
               WHEN 0 MOVE "INSTITUCIONAL                                           "
                    TO MODALIDAD-APL-TRAT-J
               WHEN 1 MOVE "DOMICILIARIO                                            " 
                   TO MODALIDAD-APL-TRAT-J
               WHEN 2 MOVE "MIXTO" 
                   TO MODALIDAD-APL-TRAT-J
               WHEN 9 MOVE "NO APLICA USUARIO CON COAGULOPATIA DIFERENTE A HEMOFILIA" 
                   TO MODALIDAD-APL-TRAT-J   
           END-EVALUATE
           EVALUATE  VIA-ADMIN-W
               WHEN 0 MOVE "ACCESO PERIFERICO                                       " 
                   TO VIA-ADMIN-J
               WHEN 1 MOVE "ACCESO CENTRAL                                          " 
                   TO VIA-ADMIN-J
               WHEN 9 MOVE "NO APLICA USUARIO CON COAGULOPATIA DIFERENTE A HEMOFILIA" 
                           TO VIA-ADMIN-J
               WHEN OTHER
                   MOVE   "NO SE LE SUMINISTRO NINGUN MEDICAMENTO                   "               
                          TO VIA-ADMIN-J
           END-EVALUATE
           IF CUM-POS-W = SPACES OR LOW-VALUE
               MOVE "LA PERSONA NO RECIBIO MEDICAMENTO POS " 
               TO CUM-POS-J
           ELSE
                MOVE CUM-POS-W TO LLAVE-POS
                READ ARCHIVO-POS
                   INVALID KEY MOVE LLAVE-POS
                       TO CUM-POS-J
                   NOT INVALID KEY
                       IF GRUPO-POS = "PO"
                         MOVE  NOMBRE-POS  TO CUM-POS-J 
                       END-IF
                END-READ
            END-IF 
            
            IF CUM-NOPOS-W = SPACES OR LOW-VALUE
               MOVE "LA PERSONA NO RECIBIO MEDICAMENTO NOPOS" 
               TO CUM-NOPOS-W
           ELSE
                MOVE CUM-NOPOS-W TO LLAVE-POS
                READ ARCHIVO-POS
                   INVALID KEY 
                   MOVE LLAVE-POS     TO CUM-NOPOS-J
                   NOT INVALID KEY
                       IF GRUPO-POS = "NP"
                        MOVE   NOMBRE-POS  TO CUM-NOPOS-J 
                       END-IF
                END-READ
            END-IF  
            IF CUM-POS-OTROS-W = SPACES OR LOW-VALUE
               MOVE "LA PERSONA NO RECIBIO MEDICAMENTO POS  " 
               TO CUM-POS-OTROS-W
           ELSE
               MOVE CUM-POS-OTROS-W TO LLAVE-POS
                READ ARCHIVO-POS
                   INVALID KEY 
                   MOVE LLAVE-POS    TO CUM-POS-OTROS-J
                   NOT INVALID KEY
                       IF GRUPO-POS = "PO"
                         MOVE  NOMBRE-POS  TO CUM-POS-OTROS-J 
                       END-IF
                END-READ
            END-IF  
           IF CUM-NOPOS-OTROS-W = SPACES OR LOW-VALUE
               MOVE "LA PERSONA NO RECIBIO MEDICAMENTO NOPOS " 
               TO CUM-NOPOS-OTROS-W
           ELSE
                MOVE CUM-NOPOS-OTROS-W TO LLAVE-POS
                READ ARCHIVO-POS
                   INVALID KEY 
                   MOVE LLAVE-POS    TO CUM-NOPOS-OTROS-J
                   NOT INVALID KEY
                       IF GRUPO-POS = "PO"
                           MOVE NOMBRE-POS  TO CUM-NOPOS-OTROS-J 
                       END-IF
                END-READ               
            END-IF  
           CLOSE ARCHIVO-POS
           EVALUATE NOVEDADES-W
               WHEN 1 MOVE "USUARIO INGRESO A LA EAPB CON DIAGNÓSTICO DE HEMOFILIA  "
                       TO NOVEDADES-J
               WHEN 2 MOVE "USUARIO ANTIGUO EN LA EAPB, SE REALIZO NUEVO DIAGNÓSTICO" 
                       TO NOVEDADES-J
               WHEN OTHER 
                      MOVE "NO PRESENTA NOVEDAD" 
                        TO NOVEDADES-J                       
           END-EVALUATE
           
            MOVE RENG-OBS-W(1) TO OBSERVACIONES-J .

       LEER-DETALLES.
      * LEER ENFERMEDAD
           MOVE LLAVE-HC TO LLAVE-HC-DETHC
           MOVE 1001 TO COD-DETHC
           READ HISTORIA-DETALLADA
                INVALID KEY INITIALIZE DETALLE-DETHC
           END-READ
           MOVE DETALLE-DETHC    TO ENF-ACT-J
      *LEER ANTECEDENTES FAMILIARES   
           MOVE LLAVE-HC TO LLAVE-HC-DETHC
           MOVE 2002 TO COD-DETHC
           READ HISTORIA-DETALLADA
                INVALID KEY
                  INITIALIZE DETALLE-DETHC
           END-READ
           MOVE DETALLE-DETHC TO  FAMIL-HC-J

      *LEER ANTECEDENTES MEDICOS     
           MOVE LLAVE-HC TO LLAVE-HC-DETHC
           MOVE 2010 TO COD-DETHC
           READ HISTORIA-DETALLADA
                INVALID KEY INITIALIZE DETALLE-DETHC
           END-READ
           MOVE DETALLE-DETHC  TO MEDIC-HC-J

      *LEER ANTECEDENTES HEMORRAGICOS     
           MOVE LLAVE-HC TO LLAVE-HC-DETHC
           MOVE 2011 TO COD-DETHC
           READ HISTORIA-DETALLADA
                INVALID KEY INITIALIZE DETALLE-DETHC
           END-READ
           MOVE DETALLE-DETHC  TO HEMOR-HC-J

      *LEER ANTECEDENTES QUIRURGICOS
           MOVE LLAVE-HC TO LLAVE-HC-DETHC    
           MOVE 2020 TO COD-DETHC
           READ HISTORIA-DETALLADA
                INVALID KEY INITIALIZE DETALLE-DETHC
           END-READ
           MOVE DETALLE-DETHC TO  QUIRU-HC-J

      *LEER ANTECEDENTES TOXICO-ALERGICOS
           MOVE LLAVE-HC TO LLAVE-HC-DETHC
           MOVE 2035 TO COD-DETHC
           READ HISTORIA-DETALLADA
                INVALID KEY INITIALIZE DETALLE-DETHC
           END-READ
           MOVE DETALLE-DETHC TO  TOXIC-HC-J

      *LEER ANTECEDENTES TRAUMATICOS
           MOVE LLAVE-HC TO LLAVE-HC-DETHC
           MOVE 2040 TO COD-DETHC
           READ HISTORIA-DETALLADA
                INVALID KEY INITIALIZE DETALLE-DETHC
           END-READ
           MOVE DETALLE-DETHC TO  TRAUM-HC-J

      *LEER ANTECEDENTES GINECOLOGICOS
           MOVE LLAVE-HC TO LLAVE-HC-DETHC
           MOVE 2060 TO COD-DETHC
           READ HISTORIA-DETALLADA
                INVALID KEY INITIALIZE DETALLE-DETHC
           END-READ
           MOVE DETALLE-DETHC TO  GINEC-HC-J

      *LEER EXAMEN GENERAL
           MOVE LLAVE-HC TO LLAVE-HC-DETHC
           MOVE 4005 TO COD-DETHC
           READ HISTORIA-DETALLADA
                INVALID KEY INITIALIZE DETALLE-DETHC
           END-READ
           MOVE DETALLE-DETHC TO  EXA-GENERAL-HC-J
      *LEER ANALISIS
           MOVE LLAVE-HC TO LLAVE-HC-DETHC
           MOVE 7501 TO COD-DETHC
           READ HISTORIA-DETALLADA
                INVALID KEY INITIALIZE DETALLE-DETHC
           END-READ
           MOVE DETALLE-DETHC TO  ANALISIS-HC-J
           CLOSE HISTORIA-DETALLADA. 

       GRABAR-DATOS-JSON.
           OPEN OUTPUT ARCHIVO-JSON
           MOVE "{*IMPRESION*:[" TO  DATOS-PLANO
008180     INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
008190     WRITE REG-JSON.
           MOVE LIN-01 TO REG-JSON
           INSPECT REG-JSON REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT REG-JSON CONVERTING LOW-VALUE TO SPACES
           WRITE REG-JSON END-WRITE
           
           MOVE LIN-HC9004 TO REG-JSON
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT REG-JSON CONVERTING LOW-VALUE TO SPACES
           WRITE REG-JSON END-WRITE

           MOVE LIN-ENF-ACT TO REG-JSON
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT REG-JSON CONVERTING LOW-VALUE TO SPACES
           WRITE REG-JSON END-WRITE
           
           MOVE LIN-FAMIL-HC TO REG-JSON
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT REG-JSON CONVERTING LOW-VALUE TO SPACES
           WRITE REG-JSON END-WRITE
           
           MOVE LIN-MEDIC-HC TO REG-JSON
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT REG-JSON CONVERTING LOW-VALUE TO SPACES
           WRITE REG-JSON END-WRITE

           MOVE LIN-HEMOR-HC TO REG-JSON
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT REG-JSON CONVERTING LOW-VALUE TO SPACES
           WRITE REG-JSON END-WRITE
           
           MOVE LIN-TOXIC-HC TO REG-JSON
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT REG-JSON CONVERTING LOW-VALUE TO SPACES
           WRITE REG-JSON END-WRITE

           MOVE LIN-TRAUM-HC TO REG-JSON
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT REG-JSON CONVERTING LOW-VALUE TO SPACES
           WRITE REG-JSON END-WRITE
           
           MOVE LIN-GINEC-HC TO REG-JSON
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT REG-JSON CONVERTING LOW-VALUE TO SPACES
           WRITE REG-JSON END-WRITE
           
           MOVE LIN-ANALISIS-HC TO REG-JSON
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT REG-JSON CONVERTING LOW-VALUE TO SPACES
           WRITE REG-JSON END-WRITE
           
           MOVE LIN-EXA-GENERAL-HC TO REG-JSON
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT REG-JSON CONVERTING LOW-VALUE TO SPACES
           WRITE REG-JSON END-WRITE
           
           MOVE LIN-QUIRU-HC TO REG-JSON
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT REG-JSON CONVERTING LOW-VALUE TO SPACES
           WRITE REG-JSON END-WRITE
           
           MOVE LIN-OBSERVACIONES-9004 TO REG-JSON
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT REG-JSON CONVERTING LOW-VALUE TO SPACES
           WRITE REG-JSON END-WRITE

           MOVE "}" TO REG-JSON
           WRITE REG-JSON END-WRITE

           MOVE "]}" TO REG-JSON
           WRITE REG-JSON END-WRITE

           CLOSE ARCHIVO-JSON 
           MOVE 00 TO ESTADO-ENV.            

       SELECCIONAR-MES.
           EVALUATE MES-NUM
            WHEN '01' MOVE 'ENERO     ' TO MES-LET 
            WHEN '02' MOVE 'FEBRERO   ' TO MES-LET 
            WHEN '03' MOVE 'MARZO     ' TO MES-LET 
            WHEN '04' MOVE 'ABRIL     ' TO MES-LET 
            WHEN '05' MOVE 'MAYO      ' TO MES-LET 
            WHEN '06' MOVE 'JUNIO     ' TO MES-LET 
            WHEN '07' MOVE 'JULIO     ' TO MES-LET 
            WHEN '08' MOVE 'AGOSTO    ' TO MES-LET 
            WHEN '09' MOVE 'SEPTIEMBRE' TO MES-LET 
            WHEN '10' MOVE 'OCTUBRE   ' TO MES-LET 
            WHEN '11' MOVE 'NOVIEMBRE ' TO MES-LET 
            WHEN '12' MOVE 'DICIMBRE  ' TO MES-LET 
           END-EVALUATE.

       ENVIO-DATOS.
           MOVE "datosrecibidos"  TO COBW3-CNV-NAME
004393     MOVE DATOS-ENVIO        TO COBW3-CNV-VALUE
004403     CALL "COBW3_SET_CNV"   USING COBW3
004413     MOVE "../PAGINAS/RECIBIDOS.ASPX"  TO SALIDA-HTML
004430     PERFORM ABRIR-HTML.

002545 CERRAR-SESION.
002546     CALL "COBW3_FREE" USING COBW3.
002547     MOVE 1 TO PROGRAM-STATUS
           EXIT PROGRAM.
           

       ABRIR-HTML.
000610     SET  COBW3-PHYSICALPATH TO TRUE.
000620     CALL "COBW3_GET_REQUEST_INFO" USING COBW3.
000630
000640     IF COBW3-STATUS = ZERO
000650       THEN
000660         MOVE COBW3-REQUEST-INFO TO PATHNAME-W
000670     END-IF.
000680
000690     MOVE SPACE TO COBW3-HTML-FILENAME.
000700     STRING COBW3-REQUEST-INFO(1:COBW3-REQUEST-INFO-LENGTH)
000710      "\"                    DELIMITED BY SIZE
000720      SALIDA-HTML            DELIMITED BY SPACE
000730     INTO COBW3-HTML-FILENAME
000740     CALL "COBW3_PUT_HTML" using COBW3. 


       ENVIAR2-ERROR.
           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE MSJ-HTML         TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"  USING COBW3
           MOVE "..\paginas\RECIBIDOS.ASPX" TO SALIDA-HTML
           PERFORM ABRIR-HTML.
           GO TO CERRAR-SESION.                                       
