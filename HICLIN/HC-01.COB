004820*=================================================================
004830* CREA JSON DE CONSULTA DE DATOS POR PACIENTE HC.
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
004920     COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".
           COPY "P:\PROG\PROYECT\FUENTES\FS-PACIE.CBL".
           COPY "P:\PROG\PROYECT\FUENTES\FS-HCAPER17.CBL".

           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-USUAR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.



005080 DATA DIVISION.
005090 FILE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUAR.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-HCAPER17.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-PACIE.CBL".

005190 WORKING-STORAGE SECTION.
005200 COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
005210 COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC19.CBL".

       77 NOM-USUAR-W                PIC X(70).
       77 DATOS-PLANO-W              PIC X(1024).
       77 LLAVE-HC-W                 PIC X(23).


       77 PESO-EDIT                  PIC ZZZZZ.Z.
       77 IMC-CORP-EDIT              PIC ZZ.ZZ.
       77 PER-CEF-EDIT               PIC ZZZ.Z.
       77 PESO-ANT-EDIT              PIC ZZZZZ.Z.
       77 NUMER-EDIT                 PIC ZZ.Z.
       77 CREATININA-EDIT            PIC Z.Z.

       01 TABLA-EDIT.
          02 TABLA-EDIT-W            PIC X(1024) OCCURS 10.

       01 LIN-01.
          02 FILLER                  PIC X(11)  VALUE "{*NOVEDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOVEDAD-J               PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*LLAVE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LLAVE-HC-J              PIC X(23).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FECHA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-HC-J              PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*HORA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HORA-HC-J               PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*SERV*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SERV-HC-J               PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*OPER-ELAB*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OPER-ELAB-HC-J          PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*MED*:".
          02 FILLER                  PIC X      VALUE "*".
          02 MED-HC-J                PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*ESQUEMA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESQUEMA-HC-J            PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*EDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EDAD-HC-J               PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*EDAD-DIAS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EDAD-DIAS-HC-J          PIC X(7).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*NIV-ESTUD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NIV-ESTUD-HC-J          PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*ACOMPA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ACOMPA-HC-J             PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*ID-ACOMPA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ID-ACOMPA-HC-J          PIC X(15).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*HORA-TRANS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HORA-TRANS-HC-J         PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*EMBRIAGUEZ*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EMBRIAGUEZ-TRANS-HC-J   PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*PLANIFIC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PLANIFIC-HC-J           PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*PROCEDEN*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PROCEDEN-HC-J           PIC X(50).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*MOTIVO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 MOTIV-HC-J              PIC X(380).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*PRORROGA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PRORROGA-INC-HC-J       PIC X.
          02 FILLER                  PIC X(26)  VALUE "*,*SIGNOS*:[{*UND-PESO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UND-PESO-HC-J           PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*PESO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PESO-HC-J               PIC X(7).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*TALLA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TALLA-HC-J              PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*IMC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 IMC-CORP-HC-J           PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*SUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SUP-CORP-HC-J           PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*TEMP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TEMP-HC-J               PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FCARD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FCARD-HC-J              PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FRESP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FRESP-HC-J              PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*TENS1*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TENS1-HC-J              PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*TENS2*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TENS2-HC-J              PIC X(3).
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
          02 FILLER                  PIC X(10)  VALUE "*PER-CEF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PER-CEF-HC-J            PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*SINTOM-RESP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SINTOM-RESPI-HC-J       PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*PER-ABDO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PER-ABDO-HC-J           PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*TANNER-PUBICO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TANNER-PUBICO-HC-J      PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*TANNER-GENIT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TANNER-GENIT-HC-J       PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*ALT-UTER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ALT-UTER-HC-J           PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*EST-NUTRI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EST-NUTRI-HC-J          PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*PESO-PREV*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PESO-PREV-HC-J          PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*CREATININA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CREATININA-HC-J         PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*PER-TORA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PER-TORA-HC-J           PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*PER-MUNE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PER-MUNE-HC-J           PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*SINTOM-PIEL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SINTOM-PIEL-HC-J        PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*VICTI-MALTRATO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VICTI-MALTRATO-HC-J     PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(18)  VALUE "*VICTI-VIOLENCIA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VICTI-VIOLENCIA-HC-J    PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*ENFER-MENTAL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ENFER-MENTAL-HC-J       PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*ENFER-ITS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ENFER-ITS-HC-J          PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*CANCER-SENO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CANCER-SENO-HC-J        PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*CANCER-CERVIS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CANCER-CERVIS-HC-J      PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*PER-BRAQ*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PER-BRAQ-HC-J           PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(20)  VALUE "*HEMO-GLICOSILADA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HEMO-GLICOSILADA-HC-J   PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(20)  VALUE "*HEMO-GLICO-FECHA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HEMO-GLICO-FECHA-HC-J   PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(19)  VALUE "*MICROALBUMINURIA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 MICROALBUMINURIA-HC-J   PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(25)  VALUE "*FECHA-MICROALBUMINURIA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-MICROALBUMINURIA-HC-J PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*RIESGO-CARDIO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 RIESGO-CARDIO-HC-J      PIC X.
          02 FILLER                  PIC X(4)   VALUE "*}],".
          02 FILLER                  PIC X(50)  VALUE "*TRATAMIENTO-SIFILIS*:[{*TABLA-TTO-SIFI*:[".

       01 LIN-TTO-SIFI.
          02 FILLER                  PIC X(10)  VALUE "{*FECHA*:*".
          02 FECHA-TTO-SIFI-HC-J     PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*MEDICAMENTO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 MEDICAMENTO-SIFI-HC-J   PIC X(45).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*DOSIS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DOSIS-SIFI-HC-J         PIC X(45).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*PROFE-APLICO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PROFE-APLICO-SIFI-HC-J  PIC X(45).
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-TTO-SIFI         PIC X(3).

       01 LIN-SEGUI-SEROL.
          02 FILLER                  PIC X(9)   VALUE "{*TIPO*:*".
          02 TIPO-SEROL-HC-J         PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FECHA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-SEROL-HC-J        PIC X(45).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*RESUL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 RESUL-SEROL-HC-J        PIC X.
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-SEGUI-SEROL      PIC X(3).

       01 LIN-02.
          02 FILLER                  PIC X(14) VALUE "{*NECE-RETTO*:".
          02 FILLER                  PIC X     VALUE "*".
          02 NECE-RETTO-SIFI-HC-J    PIC X.
          02 FILLER                  PIC XX    VALUE "*,".
          02 FILLER                  PIC X(14) VALUE "*FECHA-RETTO*:". 
          02 FILLER                  PIC X     VALUE "*".
          02 FECHA-RETTO-SIFI-HC-J   PIC X(8).
          02 FILLER                  PIC XX    VALUE "*,".
          02 FILLER                  PIC X(15) VALUE "*MEDICA-RETTO*:". 
          02 FILLER                  PIC X     VALUE "*".
          02 MEDICAMENTO-RETTO-HC-J  PIC X(45).
          02 FILLER                  PIC XX    VALUE "*,".
          02 FILLER                  PIC X(14) VALUE "*DOSIS-RETTO*:". 
          02 FILLER                  PIC X     VALUE "*".
          02 DOSIS-RETTO-HC-J        PIC X(45).
          02 FILLER                  PIC XX    VALUE "*,".
          02 FILLER                  PIC X(21) VALUE "*PROFE-APLICO-RETTO*:". 
          02 FILLER                  PIC X     VALUE "*".
          02 PROFE-APLICO-RETTO-HC-J PIC X(45).
          02 FILLER                  PIC XX    VALUE "*,".
          02 FILLER                  PIC X(21) VALUE "*CUANTOS-DIAS-RETTO*:". 
          02 FILLER                  PIC X     VALUE "*".
          02 CUANTOS-DIAS-RETTO-HC-J PIC XX.
          02 FILLER                  PIC XX    VALUE "*,".
          02 FILLER                  PIC X(14) VALUE "*DX-CONTACTO*:". 
          02 FILLER                  PIC X     VALUE "*".
          02 DX-CONTACTO-HC-J        PIC X.
          02 FILLER                  PIC XX    VALUE "*,".
          02 FILLER                  PIC X(17) VALUE "*TTO-CONTACTO*:".
          02 FILLER                  PIC X     VALUE "*".
          02 TTO-CONTACTO-HC-J       PIC X.
          02 FILLER                  PIC XX    VALUE "*,".
          02 FILLER                  PIC X(21) VALUE "*REMITIO-CONTACTO*:".
          02 FILLER                  PIC X     VALUE "*".
          02 REMITIO-CONTACTO-HC-J   PIC X(45).


          02 FILLER                  PIC X(23)  VALUE "*}],*RIPS*:[{*ATIENDE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ATIENDE-HC-J            PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*TRIAGE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TRIAGE-HC-J             PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*CAUSA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CAUSA-HC-J              PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*FINALIDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FINALID-HC-J            PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*EMBARAZO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EMBARAZO-HC-J           PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*ESTADO-SAL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESTADO-SAL-HC-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*REMITIDO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 REMITIDO-HC-J           PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*NIVEL-REMIT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NIVEL-REMIT-HC-J        PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*OBSERV*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OBSERV-HC-J             PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*CRONICO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CRONICO-HC-J            PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*1RA-VEZ*:".
          02 FILLER                  PIC X      VALUE "*".
          02 1RA-VEZ-HC-J            PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*NRO-CONTR*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NRO-CONTR-HC-J          PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*TIPO-DIAG*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-DIAG-HC-J          PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*TABLA-DIAG*:[".

       01 LIN-DIAG.
          02 FILLER                  PIC X(10)  VALUE "{*DIAGN*:*".
          02 COD-DIAGN-HC-J          PIC X(4).
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-DIAG             PIC XX.


       01 LIN-03.
          02 FILLER                  PIC X(4)   VALUE "}],".
          02 FILLER                  PIC X(19)  VALUE "*CIERRE*:[{*CLASE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLASE-HC-J              PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESTADO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESTADO-HC-J             PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*TEMPORAL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TEMPORAL-HC-J           PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*PREFIJO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PREFIJO-HC-J            PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*UNSERV*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UNSERV-HC-J             PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*FECHA-UNSERV*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-UNSERV-HC-J       PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*DIAG-MUER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIAG-MUER-HC-J          PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*PACIEN-CRONIC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PACIENTE-CRONIC-HC-J    PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*FECHA-MI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-MI-HC-J           PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*FECHA-ENDOCRI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-ENDOCRI-HC-J      PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*FECHA-CARDIO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-CARDIO-HC-J       PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*FECHA-OFTAMOL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-OFTAMOL-HC-J      PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*FECHA-NEFRO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-NEFRO-HC-J        PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*FECHA-PSICOL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-PSICOL-HC-J       PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*FECHA-NUTRI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-NUTRI-HC-J        PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*FECHA-TRABSOC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-TRABSOC-HC-J      PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*RESUL-LABORAT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 RESUL-LABORAT-HC-J      PIC X(45).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*FECHA-INI-TTO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-INI-TTO-HC-J      PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*FECHA-FIN-TTO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-FIN-TTO-HC-J      PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*CTL-MAL-3DIAS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CTL-MAL-3DIA-HC-J       PIC X(45).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*CTL-MAL-7DIAS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CTL-MAL-7DIA-HC-J       PIC X(45).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*CTL-MAL-14DIAS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CTL-MAL-14DIA-HC-J      PIC X(45).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(18)  VALUE "*CTL-LEIS-45DIAS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CTL-LEIS-45DIAS-HC-J    PIC X(45).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(18)  VALUE "*CTL-LEIS-6MESES*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CTL-LEIS-6MESES-HC-J    PIC X(45).
          02 FILLER                  PIC X(4)   VALUE "*}],".
          02 FILLER                  PIC X(28)  VALUE "*CITOLOGIA*:[{*COLPOSCOPIA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COLPOSCOPIA-HC-J        PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(20)  VALUE "*FECHA-COLPOSCOPIA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-COLPOSCOPIA-HC-J  PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*BIOSPIA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 BIOPSIA-HC-J            PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*FECHA-BIOSPIA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-BIOPSIA-HC-J      PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(18)  VALUE "*FECHA-PATOLOGIA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-PATOLOGIA-HC-J    PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(18)  VALUE "*DIAGN-CITOLOGIA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIAGN-CITOLOGIA-HC-J    PIC X(45).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(18)  VALUE "*VALORACION-MEDI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VALORACION-MEDI-HC-J    PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*FECHA-VAL-MED*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-VALORACION-MEDI-HC-J PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(18)  VALUE "*VALORACION-GINE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VALORACION-GINE-HC-J    PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*FECHA-VAL-GINE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-VALORACION-GINE-HC-J PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*CONTROL-CITOL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CONTROL-CITOL-HC-J      PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(18)  VALUE "*FECHA-CTR-CITOL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-CONTROL-CITOL-HC-J PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(23)  VALUE "*FECHA-CANCER-CUELLO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-CANCER-CUELLO-HC-J PIC X(8).
          02 FILLER                  PIC X(4)   VALUE "*}],".
          02 FILLER                  PIC X(32)  VALUE "*VIOLENCIA*:[{*ORIENTACION-SEX*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ORIENTACION-SEX-HC-J    PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*DISCAPACIDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DISCAPACIDAD-HC-J       PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*VULNERABILIDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VULNERABILIDAD-HC-J     PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*RIESGO-PSICOS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 RIESGO-PSICOS-HC-J      PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*ACCIONES-REALI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ACCIONES-REALI-HC-J     PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*TIPO-VIOLENCIA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-VIOLENCIA-HC-J     PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*PENETRACION*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PENETRACION-ANAL-HC-J   PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(19)  VALUE "*DILIGENCIA-VIOLE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DILIGENCIA-VIOLE-HC-J   PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(23)  VALUE "*FECHA-REM-PSIQUIATRA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-REM-PSIQUIATRIA-HC-J PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(22)  VALUE "*FECHA-ATE-MEDICGRAL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-ATE-MEDICGRAL-HC-J PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*TRABAJO-SOCIAL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TRABAJO-SOCIAL-HC-J     PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(23)  VALUE "*FECHA-ATE-PSICOLOGIA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-ATE-PSICOLOGIA-HC-J PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(24)  VALUE "*PSICOLOGIA-INDIVIDUAL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PSICOTERAPIA-INDIVIDUAL-HC-J PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(25)  VALUE "*PSICOTERAPIA-IND-EJECU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PSICOTERAPIA-IND-EJECU-HC-J PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(24)  VALUE "*PSICOTERAPIA-FAMILIAR*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PSICOTERAPIA-FAMILIAR-HC-J PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(25)  VALUE "*PSICOTERAPIA-FAM-EJECU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PSICOTERAPIA-FAM-EJECU-HC-J PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(21)  VALUE "*FECHA-NOTIFICACION*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-NOTIFICACION-HC-J PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(22)  VALUE "*PREVENCION-ITS-VSX*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PREVENCION-ITS-VSX-HC-J PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(23)  VALUE "*ANTICONCEPCION-EMER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ANTICONCEPCION-EMER-HC-J PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*TIPO-METODO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-METODO-HC-J        PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(22)  VALUE "*ASESORIA-CONSEJERIA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ASESORIA-CONSEJERIA-HC-J PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*CONSEJERIA-IVE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CONSEJERIA-IVE-HC-J     PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(18)  VALUE "*RUTA-PROTECCION*:".
          02 FILLER                  PIC X      VALUE "*".
          02 RUTA-PROTECCION-HC-J    PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(18)  VALUE "*RUTA-JUSTIFICA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 RUTA-JUSTICIA-HC-J      PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(26)  VALUE "*FECHA-SEGUI-RES459-SEM2*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-SEGUI-RES459-SEM2-HC-J PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(26)  VALUE "*FECHA-SEGUI-RES459-SEM4*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-SEGUI-RES459-SEM4-HC-J PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(26)  VALUE "*FECHA-SEGUI-RES459-3MES*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-SEGUI-RES459-3MES-HC-J PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(26)  VALUE "*FECHA-SEGUI-RES459-6MES*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-SEGUI-RES459-6MES-HC-J PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(26)  VALUE "*FECHA-SEGUI-RES459-1ANO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-SEGUI-RES459-1ANO-HC-J PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(20)  VALUE "*SEGUIMIENTO-MENOR*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SEGUIMIENTO-MENOR-HC-J   PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(20)  VALUE "*SEGUIMIENTO-MUJER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SEGUIMIENTO-MUJER-HC-J   PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(22)  VALUE "*FECHA-CIERRE-RES459*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-CIERRE-RES459-HC-J PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(22)  VALUE "*CAPTACION-POBLACION*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CAPTACION-POBLACION-HC-J PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(23)  VALUE "*OBSERVACIONES-RES459*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OBSERVACIONES-RES459-HC-J PIC X(95).
          02 FILLER                  PIC X(4)   VALUE "*}],".
          02 FILLER                  PIC X(44)  VALUE "*TRASTORNO-MENTAL*:[{*ORIENTACION-SEX-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ORIENTACION-SEX-MENT-HC-J PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(20)  VALUE "*DISCAPACIDAD-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DISCAPACIDAD-MENT-HC-J  PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(22)  VALUE "*VULNERABILIDAD-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VULNERABILIDAD-MENT-HC-J PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(19)  VALUE "*TIPO-EVENTO-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-EVENTO-MENT-HC-J   PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*RECAIDAS-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 RECAIDAS-MENT-HC-J      PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(21)  VALUE "*RIESGO-PSICOS-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 RIESGO-PSICOS-MENT-HC-J PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(22)  VALUE "*ACCIONES-REALI-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ACCIONES-REALI-MENT-HC-J PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(21)  VALUE "*MEDICINA-GRAL-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 MEDICINA-GRAL-MENT-HC-J PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(18)  VALUE "*PSICOLOGIA-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PSICOLOGIA-MENT-HC-J    PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(19)  VALUE "*PSICOTE-IND-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PSICOTE-IND-MENT-HC-J    PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(24)  VALUE "*PSICOTE-IND-EJEC-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PSICOTE-IND-EJEC-MENT-HC-J PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(19)  VALUE "*PSICOTE-FAM-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PSICOTE-FAM-MENT-HC-J    PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(24)  VALUE "*PSICOTE-FAM-EJEC-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PSICOTE-FAM-EJEC-MENT-HC-J PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(29)  VALUE "*FECHA-REMISION-PSIQUI-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-REMISION-PSIQUI-HC-J PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(23)  VALUE "*FECHA-NOTIFICA-OBLIG*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-NOTIFICA-OBLIG-HC-J PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(22)  VALUE "*FAMILIAR-CUIDA-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FAMILIAR-CUIDA-MENT-HC-J  PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*TAMIZAJES-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TAMIZAJES-MENT-HC-J       PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(23)  VALUE "*MANEJO-TAMIZAJE-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 MANEJO-TAMIZAJE-MENT-HC-J PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(20)  VALUE "*CAPACITACION-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CAPTACION-MENT-HC-J     PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(19)  VALUE "*CONSUMO-SPA-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CONSUMO-SPA-MENT-HC-J   PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(22)  VALUE "*CANALIZA-DIPOS-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CANALIZA-DISPOS-MENT-HC-J PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(23)  VALUE "*ACCIONES-SEGUI-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ACCIONES-SEGUI-MENT-HC-J  PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(23)  VALUE "*ADHERENCIA-TRAT-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ADHERENCIA-TRAT-MENT-HC-J PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(20)  VALUE "*SEGUI-CONTRA-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SEGUI-CONTRA-MENT-HC-J  PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(20)  VALUE "*FECHA-CIERRE-CASO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-CIERRE-CASO-HC-J  PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(19)  VALUE "*EVALUACION-MHGAP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EVALUACION-MHGAP-HC-J   PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(23)  VALUE "*PRONOSTICO-TERA-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PRONOSTICO-TERA-MENT-HC-J PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(23)  VALUE "*OBSERVACIONES-MENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OBSERVACIONES-MENT-HC-J PIC X(95).
          02 FILLER                  PIC X(4)   VALUE "*}],".
          02 FILLER                  PIC X(43)  VALUE "*MULTIDROGORESISTENTE*:[{*MULTIDROGORESIS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 MULTIDROGORESIS-HC-J    PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(23)  VALUE "*CUAL-MULTIDROGORESIS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CUAL-MULTIDROGORESIS-HC-J PIC X(95).
          02 FILLER                  PIC X(4)   VALUE "*}],".
          02 FILLER                  PIC X(43)  VALUE "*EXAMEN-VISUAL*:[{*AGUDEZA-VISUAL-OI-1*:".
          02 FILLER                  PIC X      VALUE "*".
          02 AGUD-VISUAL-OI-1-HC-J   PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(22)  VALUE "*AGUDEZA-VISUAL-OI-2*:".
          02 FILLER                  PIC X      VALUE "*".
          02 AGUD-VISUAL-OI-2-HC-J   PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(22)  VALUE "*AGUDEZA-VISUAL-OD-1*:".
          02 FILLER                  PIC X      VALUE "*".
          02 AGUD-VISUAL-OD-1-HC-J   PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(22)  VALUE "*AGUDEZA-VISUAL-OD-2*:".
          02 FILLER                  PIC X      VALUE "*".
          02 AGUD-VISUAL-OD-2-HC-J   PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(26)  VALUE "*ESTRUCTURAS-OCULARES-OI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESTRUCTURAS-OCULARES-OI-HC-J PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(26)  VALUE "*ESTRUCTURAS-OCULARES-OD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESTRUCTURAS-OCULARES-OD-HC-J PIC X.
          02 FILLER                  PIC X(6)   VALUE "*}]}]}".


006310 LINKAGE SECTION.
006320 COPY "P:\PROG\PROYECT\FUENTES\ISAPICTX.CBL".
006330 PROCEDURE DIVISION WITH stdcall LINKAGE USING ISAPI-CTX-CNT.
006340
006350 DECLARATIVES.
006360 I-O-TEST SECTION.
006370     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUNET.         
006380 ESCR-EXCEPTIONES.
006390     IF OTR-STAT = "00"
006400        CONTINUE 
006410     ELSE
006420        MOVE OTR-STAT         TO MSJ1-HTML
006430        MOVE NOM-USU-W        TO MSJ2-HTML
006440        MOVE "HC-01"          TO MSJ3-HTML
006450        GO TO ENVIAR2-ERROR
006460     END-IF.
006470
006480 I-O-TEST SECTION.
006490     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.
006500 ESCR-EXCEPTIONES.
006510     IF OTR-STAT = "00" 
006520        CONTINUE 
006530     ELSE
006540        MOVE OTR-STAT         TO MSJ1-HTML
006550        MOVE NOM-SESION-W     TO MSJ2-HTML
006560        MOVE "HC-01"          TO MSJ3-HTML
006570        GO TO ENVIAR2-ERROR
006580     END-IF.
006590  

006480 I-O-TEST SECTION.
006490     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
006500 ESCR-EXCEPTIONES.
006510     IF OTR-STAT = "00" 
006520        CONTINUE 
006530     ELSE
006540        MOVE OTR-STAT         TO MSJ1-HTML
006550        MOVE NOM-USUAR-W      TO MSJ2-HTML
006560        MOVE "HC-01"          TO MSJ3-HTML
006570        GO TO ENVIAR2-ERROR
006580     END-IF.
006590  

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON HISTORIA-CLINICA.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-HCAPE        TO MSJ2-HTML
006680        MOVE "HC-01"          TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR  
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" 
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-PACIE-LNK    TO MSJ2-HTML
006680        MOVE "HC811"          TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

006710
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
006820     MOVE COBW3-GET-DATA    TO LINEA-LLEGAD-W.


           UNSTRING LINEA-LLEGAD-W DELIMITED BY "|"
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W, LLAVE-HC-W, ADMIN-W
           END-UNSTRING.

001855     MOVE SESION-LLEGAD-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.

000797 INICIAR-SESION.
000806     MOVE "D:\PROGELECT\HICLIN\DATOS\SC-SESION.DAT" TO NOM-SESION-W
000820     MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
000830     ACCEPT HORA-TOTAL FROM TIME.
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.
006840
006900 ABRIR-USUARIO.
006910     INITIALIZE OTR-STAT.
006920     MOVE "D:\PROGELECT\HICLIN\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
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
007130
007140     CLOSE ARCHIVO-USUNET.
007200     
007210     IF NOMBRE-USUNET = SPACES
007220     OR NIT-USUNET = ZEROS
007230        MOVE "Validacion de Sesion"        TO MSJ1-HTML
007240        MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007250        MOVE "Sc"                          TO MSJ3-HTML
007260        GO TO ENVIAR2-ERROR
007270     END-IF.
007280 
007350
       ASIGNAR-NOMBRES.
           INITIALIZE NOM-USUAR-W NOM-HCAPE
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           MOVE "\\" TO NOM-USUAR-W

           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-USUAR-W REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-USUAR-W REPLACING FIRST "                              "
                                            BY DIRECTORIO-LLEGAD-W

           MOVE NOM-USUAR-W TO NOM-PACIE-LNK

           INSPECT NOM-USUAR-W REPLACING FIRST  "                       "
                                            BY  "\CONTROL\SC-ARCHUSU.DAT".

           INSPECT NOM-PACIE-LNK REPLACING FIRST "                     "
                                              BY "\CONTROL\SC-PACIE.DAT".


           MOVE "\\" TO NOM-HCAPE

           INSPECT NOM-HCAPE  REPLACING FIRST "                    "
                                           BY IP-DATOS-USUNET

           INSPECT NOM-HCAPE REPLACING FIRST " "
                                          BY "\"

           INSPECT NOM-HCAPE REPLACING FIRST "                        "
                                          BY "PROG\DATOS\SC-HCAPER.S17".

       LEER-USUARIO.

           OPEN INPUT ARCHIVO-USUARIOS
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.

       LEER-PACIENTE.
           OPEN INPUT ARCHIVO-PACIENTES.
           MOVE LLAVE-HC-W    TO  COD-PACI.
           READ ARCHIVO-PACIENTES WITH NO LOCK
                INVALID KEY
                        MOVE "99"         TO MSJ1-HTML
                        MOVE "Paciente no existe" To MSJ2-HTML
                        MOVE "HC-01"      TO MSJ3-HTML
                        GO TO ENVIAR2-ERROR
           END-READ.
           CLOSE ARCHIVO-PACIENTES.


       BUSCAR-HISTORIAS.
           OPEN INPUT HISTORIA-CLINICA.
           MOVE LLAVE-HC-W   TO LLAVE-HC

           READ HISTORIA-CLINICA WITH NO LOCK
                INVALID KEY
                        MOVE 7 TO NOVEDAD-W
                        INITIALIZE DATOS-HC
                NOT INVALID KEY
                        MOVE 8 TO NOVEDAD-W
                        IF ADMIN-W = "GEBC" OR TEMPORAL-HC = 1
                           OR (ESTADO-HC = 1 AND OPER-ELAB-HC = ADMIN-W)
                           OR (NIT-USU = 800037021 AND ADMIN-W = "ADMI")
                           OR (NIT-USU = 892000401 AND ADMIN-W = "ADMI")
                              CONTINUE
                        ELSE
                           MOVE "99"                  TO MSJ1-HTML
                           MOVE "Acesso restringido"  TO MSJ2-HTML
                           MOVE "HC-01"               TO MSJ3-HTML
                           GO TO ENVIAR2-ERROR
                     END-IF
           END-READ.

           PERFORM LLENAR-HC.

010700 PAGINA-CONFIG.  
010720     MOVE "statuscode" TO COBW3-CNV-NAME
010730     MOVE "00"         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010720     MOVE "programa-id" TO COBW3-CNV-NAME
010730     MOVE "HC-01"       TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010760     MOVE "..\PAGINAS\RECIBIDOSLISTADO.ASPX" TO SALIDA-HTML
010770     PERFORM ABRIR-HTML.
007980   
007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.

       LLENAR-HC.

           MOVE NOVEDAD-W                      TO NOVEDAD-J
           MOVE LLAVE-HC                       TO LLAVE-HC-J
           MOVE FECHA-HC                       TO FECHA-HC-J
           MOVE HORA-HC                        TO HORA-HC-J
           MOVE SERV-HC                        TO SERV-HC-J
           MOVE OPER-ELAB-HC                   TO OPER-ELAB-HC-J
           MOVE MED-HC                         TO MED-HC-J
           MOVE ESQUEMA-HC                     TO ESQUEMA-HC-J
           MOVE EDAD-HC                        TO EDAD-HC-J
           MOVE EDAD-DIAS-HC                   TO EDAD-DIAS-HC-J

           MOVE NIV-ESTUD-HC                   TO NIV-ESTUD-HC-J
           MOVE ACOMPA-HC                      TO ACOMPA-HC-J
           MOVE ID-ACOMPA-HC                   TO ID-ACOMPA-HC-J

           MOVE HORA-TRANS-HC                  TO HORA-TRANS-HC-J
           MOVE EMBRIAGUEZ-TRANS-HC            TO EMBRIAGUEZ-TRANS-HC-J
           MOVE PLANIFIC-HC                    TO PLANIFIC-HC-J
           MOVE PROCEDEN-HC                    TO PROCEDEN-HC-J
           MOVE MOTIV-HC                       TO MOTIV-HC-J
           MOVE PRORROGA-INC-HC                TO PRORROGA-INC-HC-J

           MOVE UND-PESO-HC                    TO UND-PESO-HC-J

           MOVE PESO-HC                        TO PESO-EDIT
           MOVE PESO-EDIT                      TO PESO-HC-J

           MOVE TALLA-HC                       TO TALLA-HC-J

           MOVE IMC-CORP-HC                    TO IMC-CORP-EDIT
           MOVE IMC-CORP-EDIT                  TO IMC-CORP-HC-J

           MOVE SUP-CORP-HC                    TO IMC-CORP-EDIT
           MOVE IMC-CORP-EDIT                  TO SUP-CORP-HC-J

           MOVE TEMP-HC                        TO IMC-CORP-EDIT 
           MOVE IMC-CORP-EDIT                  TO TEMP-HC-J

           MOVE FCARD-HC                       TO FCARD-HC-J
           MOVE FRESP-HC                       TO FRESP-HC-J
           MOVE TENS1-HC                       TO TENS1-HC-J
           MOVE TENS2-HC                       TO TENS2-HC-J
           MOVE TENS-MEDIA-HC                  TO TENS-MEDIA-HC-J
           MOVE OXIMETRIA-HC                   TO OXIMETRIA-HC-J
           MOVE PVC-HC                         TO PVC-HC-J
           MOVE GLASG-HC                       TO GLASG-HC-J

           MOVE PER-CEF-HC                     TO PER-CEF-EDIT
           MOVE PER-CEF-EDIT                   TO PER-CEF-HC-J

           MOVE SINTOM-RESPI-HC                TO SINTOM-RESPI-HC-J

           MOVE PER-ABDO-HC                    TO PER-CEF-EDIT
           MOVE PER-CEF-EDIT                   TO PER-ABDO-HC-J
           MOVE TANNER-PUBICO-HC               TO TANNER-PUBICO-HC-J
           MOVE TANNER-GENIT-HC                TO TANNER-GENIT-HC-J

           MOVE ALT-UTER-HC                    TO NUMER-EDIT
           MOVE NUMER-EDIT                     TO ALT-UTER-HC-J

           MOVE EST-NUTRI-HC                   TO EST-NUTRI-HC-J

           MOVE PESO-PREV-HC                   TO PER-CEF-EDIT
           MOVE PER-CEF-EDIT                   TO PESO-PREV-HC-J


           MOVE CREATININA-HC                  TO CREATININA-EDIT
           MOVE CREATININA-EDIT                TO CREATININA-HC-J

           MOVE PER-TORA-HC                    TO PER-CEF-EDIT 
           MOVE PER-CEF-EDIT                   TO PER-TORA-HC-J

           MOVE PER-MUNE-HC                    TO NUMER-EDIT
           MOVE NUMER-EDIT                     TO PER-MUNE-HC-J

           MOVE SINTOM-PIEL-HC                 TO SINTOM-PIEL-HC-J
           MOVE VICTI-MALTRATO-HC              TO VICTI-MALTRATO-HC-J
           MOVE VICTI-VIOLENCIA-HC             TO VICTI-VIOLENCIA-HC-J
           MOVE ENFER-MENTAL-HC                TO ENFER-MENTAL-HC-J
           MOVE ENFER-ITS-HC                   TO ENFER-ITS-HC-J
           MOVE CANCER-SENO-HC                 TO CANCER-SENO-HC-J
           MOVE CANCER-CERVIS-HC               TO CANCER-CERVIS-HC-J

           MOVE PER-BRAQ-HC                    TO NUMER-EDIT
           MOVE NUMER-EDIT                     TO PER-BRAQ-HC-J

           MOVE HEMO-GLICOSILADA-HC            TO IMC-CORP-EDIT
           MOVE IMC-CORP-EDIT                  TO HEMO-GLICOSILADA-HC-J

           MOVE HEMO-GLICO-FECHA-HC            TO HEMO-GLICO-FECHA-HC-J

           MOVE MICROALBUMINURIA-HC            TO IMC-CORP-EDIT
           MOVE IMC-CORP-EDIT                  TO MICROALBUMINURIA-HC-J


           MOVE FECHA-MICROALBUMINURIA-HC      TO FECHA-MICROALBUMINURIA-HC-J
           MOVE RIESGO-CARDIO-HC               TO RIESGO-CARDIO-HC-J


           STRING "{*HC-PAC*:[", LIN-01 DELIMITED BY SIZE
                  INTO TABLA-EDIT
           END-STRING

           PERFORM PASAR-LINEA VARYING I FROM 1 BY 1 UNTIL I > 2


           PERFORM TABLA-TTO-SIFI VARYING I FROM 1 BY 1
                                            UNTIL I > 3

           MOVE '{"TABLA-SEGUI-SEROL":['  TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           PERFORM TABLA-SEGUI-SEROL VARYING I FROM 1 BY 1
                                               UNTIL I > 4


           MOVE NECE-RETTO-SIFI-HC             TO NECE-RETTO-SIFI-HC-J
           MOVE FECHA-RETTO-SIFI-HC            TO FECHA-RETTO-SIFI-HC-J
           MOVE MEDICAMENTO-RETTO-HC           TO MEDICAMENTO-RETTO-HC-J
           MOVE DOSIS-RETTO-HC                 TO DOSIS-RETTO-HC-J
           MOVE PROFE-APLICO-RETTO-HC          TO PROFE-APLICO-RETTO-HC-J
           MOVE CUANTOS-DIAS-RETTO-HC          TO CUANTOS-DIAS-RETTO-HC-J
           MOVE DX-CONTACTO-HC                 TO DX-CONTACTO-HC-J
           MOVE TTO-CONTACTO-HC                TO TTO-CONTACTO-HC-J
           MOVE REMITIO-CONTACTO-HC            TO REMITIO-CONTACTO-HC-J

           MOVE ATIENDE-HC                     TO ATIENDE-HC-J
           MOVE TRIAGE-HC                      TO TRIAGE-HC-J
           MOVE CAUSA-HC                       TO CAUSA-HC-J
           MOVE FINALID-HC                     TO FINALID-HC-J
           MOVE EMBARAZO-HC                    TO EMBARAZO-HC-J
           MOVE ESTADO-SAL-HC                  TO ESTADO-SAL-HC-J
           MOVE REMITIDO-HC                    TO REMITIDO-HC-J
           MOVE NIVEL-REMIT-HC                 TO NIVEL-REMIT-HC-J
           MOVE OBSERV-HC                      TO OBSERV-HC-J
           MOVE CRONICO-HC                     TO CRONICO-HC-J
           MOVE 1RA-VEZ-HC                     TO 1RA-VEZ-HC-J
           MOVE NRO-CONTR-HC                   TO NRO-CONTR-HC-J
           MOVE TIPO-DIAG-HC                   TO TIPO-DIAG-HC-J


           INSPECT LIN-02 REPLACING ALL "*" BY CARAC-COMILLA 
           INSPECT LIN-02 REPLACING ALL "\" BY " "
           INSPECT LIN-02 REPLACING ALL "/" BY " "
           INSPECT LIN-02 REPLACING ALL LOW-VALUES BY SPACES

           MOVE LIN-02 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO


           PERFORM TABLA-DIAGNOSTICOS VARYING I FROM 1 BY 1
                                               UNTIL I > 10

           MOVE CLASE-HC                       TO CLASE-HC-J
           MOVE ESTADO-HC                      TO ESTADO-HC-J
           MOVE TEMPORAL-HC                    TO TEMPORAL-HC-J
           MOVE PREFIJO-HC                     TO PREFIJO-HC-J
           MOVE UNSERV-HC                      TO UNSERV-HC-J
           MOVE FECHA-UNSERV-HC                TO FECHA-UNSERV-HC-J
           MOVE DIAG-MUER-HC                   TO DIAG-MUER-HC-J

           MOVE PACIENTE-CRONIC-HC             TO PACIENTE-CRONIC-HC-J
           MOVE FECHA-MI-HC                    TO FECHA-MI-HC-J
           MOVE FECHA-ENDOCRI-HC               TO FECHA-ENDOCRI-HC-J
           MOVE FECHA-CARDIO-HC                TO FECHA-CARDIO-HC-J
           MOVE FECHA-OFTAMOL-HC               TO FECHA-OFTAMOL-HC-J
           MOVE FECHA-NEFRO-HC                 TO FECHA-NEFRO-HC-J
           MOVE FECHA-PSICOL-HC                TO FECHA-PSICOL-HC-J
           MOVE FECHA-NUTRI-HC                 TO FECHA-NUTRI-HC-J
           MOVE FECHA-TRABSOC-HC               TO FECHA-TRABSOC-HC-J
           MOVE RESUL-LABORAT-HC               TO RESUL-LABORAT-HC-J
           MOVE FECHA-INI-TTO-HC               TO FECHA-INI-TTO-HC-J
           MOVE FECHA-FIN-TTO-HC               TO FECHA-FIN-TTO-HC-J
           MOVE CTL-MAL-3DIA-HC                TO CTL-MAL-3DIA-HC-J
           MOVE CTL-MAL-7DIA-HC                TO CTL-MAL-7DIA-HC-J
           MOVE CTL-MAL-14DIA-HC               TO CTL-MAL-14DIA-HC-J
           MOVE CTL-LEIS-45DIAS-HC             TO CTL-LEIS-45DIAS-HC-J
           MOVE CTL-LEIS-6MESES-HC             TO CTL-LEIS-6MESES-HC-J


           MOVE COLPOSCOPIA-HC                 TO COLPOSCOPIA-HC-J
           MOVE FECHA-COLPOSCOPIA-HC           TO FECHA-COLPOSCOPIA-HC-J
           MOVE BIOPSIA-HC                     TO BIOPSIA-HC-J
           MOVE FECHA-BIOPSIA-HC               TO FECHA-BIOPSIA-HC-J
           MOVE FECHA-PATOLOGIA-HC             TO FECHA-PATOLOGIA-HC-J
           MOVE DIAGN-CITOLOGIA-HC             TO DIAGN-CITOLOGIA-HC-J
           MOVE VALORACION-MEDI-HC             TO VALORACION-MEDI-HC-J
           MOVE FECHA-VALORACION-MEDI-HC       TO FECHA-VALORACION-MEDI-HC-J
           MOVE VALORACION-GINE-HC             TO VALORACION-GINE-HC-J
           MOVE FECHA-VALORACION-GINE-HC       TO FECHA-VALORACION-GINE-HC-J
           MOVE CONTROL-CITOL-HC               TO CONTROL-CITOL-HC-J
           MOVE FECHA-CONTROL-CITOL-HC         TO FECHA-CONTROL-CITOL-HC-J
           MOVE FECHA-CANCER-CUELLO-HC         TO FECHA-CANCER-CUELLO-HC-J

           MOVE ORIENTACION-SEX-HC             TO ORIENTACION-SEX-HC-J
           MOVE DISCAPACIDAD-HC                TO DISCAPACIDAD-HC-J
           MOVE VULNERABILIDAD-HC              TO VULNERABILIDAD-HC-J
           MOVE RIESGO-PSICOS-HC               TO RIESGO-PSICOS-HC-J
           MOVE ACCIONES-REALI-HC              TO ACCIONES-REALI-HC-J
           MOVE ACCIONES-REALI-HC              TO ACCIONES-REALI-HC-J
           MOVE TIPO-VIOLENCIA-HC              TO TIPO-VIOLENCIA-HC-J
           MOVE PENETRACION-ANAL-HC            TO PENETRACION-ANAL-HC-J
           MOVE DILIGENCIA-VIOLE-HC            TO DILIGENCIA-VIOLE-HC-J
           MOVE FECHA-REM-PSIQUIATRIA-HC       TO FECHA-REM-PSIQUIATRIA-HC-J
           MOVE FECHA-ATE-MEDICGRAL-HC         TO FECHA-ATE-MEDICGRAL-HC-J
           MOVE TRABAJO-SOCIAL-HC              TO TRABAJO-SOCIAL-HC-J
           MOVE FECHA-ATE-PSICOLOGIA-HC        TO FECHA-ATE-PSICOLOGIA-HC-J
           MOVE PSICOTERAPIA-INDIVIDUAL-HC     TO PSICOTERAPIA-INDIVIDUAL-HC-J
           MOVE PSICOTERAPIA-IND-EJECU-HC      TO PSICOTERAPIA-IND-EJECU-HC-J
           MOVE PSICOTERAPIA-FAMILIAR-HC       TO PSICOTERAPIA-FAMILIAR-HC-J
           MOVE PSICOTERAPIA-FAM-EJECU-HC      TO PSICOTERAPIA-FAM-EJECU-HC-J
           MOVE FECHA-NOTIFICACION-HC          TO FECHA-NOTIFICACION-HC-J
           MOVE PREVENCION-ITS-VSX-HC          TO PREVENCION-ITS-VSX-HC-J
           MOVE ANTICONCEPCION-EMER-HC         TO ANTICONCEPCION-EMER-HC-J
           MOVE TIPO-METODO-HC                 TO TIPO-METODO-HC-J
           MOVE ASESORIA-CONSEJERIA-HC         TO ASESORIA-CONSEJERIA-HC-J
           MOVE CONSEJERIA-IVE-HC              TO CONSEJERIA-IVE-HC-J
           MOVE RUTA-PROTECCION-HC             TO RUTA-PROTECCION-HC-J
           MOVE RUTA-JUSTICIA-HC               TO RUTA-JUSTICIA-HC-J
           MOVE FECHA-SEGUI-RES459-SEM2-HC     TO FECHA-SEGUI-RES459-SEM2-HC-J
           MOVE FECHA-SEGUI-RES459-SEM4-HC     TO FECHA-SEGUI-RES459-SEM4-HC-J
           MOVE FECHA-SEGUI-RES459-3MES-HC     TO FECHA-SEGUI-RES459-3MES-HC-J
           MOVE FECHA-SEGUI-RES459-6MES-HC     TO FECHA-SEGUI-RES459-6MES-HC-J
           MOVE FECHA-SEGUI-RES459-1ANO-HC     TO FECHA-SEGUI-RES459-1ANO-HC-J
           MOVE SEGUIMIENTO-MENOR-HC           TO SEGUIMIENTO-MENOR-HC-J
           MOVE SEGUIMIENTO-MUJER-HC           TO SEGUIMIENTO-MUJER-HC-J
           MOVE FECHA-CIERRE-RES459-HC         TO FECHA-CIERRE-RES459-HC-J
           MOVE CAPTACION-POBLACION-HC         TO CAPTACION-POBLACION-HC-J
           MOVE OBSERVACIONES-RES459-HC        TO OBSERVACIONES-RES459-HC-J

           MOVE ORIENTACION-SEX-MENT-HC        TO ORIENTACION-SEX-MENT-HC-J
           MOVE DISCAPACIDAD-MENT-HC           TO DISCAPACIDAD-MENT-HC-J
           MOVE VULNERABILIDAD-MENT-HC         TO VULNERABILIDAD-MENT-HC-J
           MOVE TIPO-EVENTO-MENT-HC            TO TIPO-EVENTO-MENT-HC-J 
           MOVE RECAIDAS-MENT-HC               TO RECAIDAS-MENT-HC-J
           MOVE RIESGO-PSICOS-MENT-HC          TO RIESGO-PSICOS-MENT-HC-J
           MOVE ACCIONES-REALI-MENT-HC         TO ACCIONES-REALI-MENT-HC-J
           MOVE MEDICINA-GRAL-MENT-HC          TO MEDICINA-GRAL-MENT-HC-J
           MOVE PSICOLOGIA-MENT-HC             TO PSICOLOGIA-MENT-HC-J
           MOVE PSICOTE-IND-MENT-HC            TO PSICOTE-IND-MENT-HC-J
           MOVE PSICOTE-IND-EJEC-MENT-HC       TO PSICOTE-IND-EJEC-MENT-HC-J
           MOVE PSICOTE-FAM-MENT-HC            TO PSICOTE-FAM-MENT-HC-J
           MOVE PSICOTE-FAM-EJEC-MENT-HC       TO PSICOTE-FAM-EJEC-MENT-HC-J
           MOVE FECHA-REMISION-PSIQUI-HC       TO FECHA-REMISION-PSIQUI-HC-J
           MOVE FECHA-NOTIFICA-OBLIG-HC        TO FECHA-NOTIFICA-OBLIG-HC-J
           MOVE FAMILIAR-CUIDA-MENT-HC         TO FAMILIAR-CUIDA-MENT-HC-J
           MOVE TAMIZAJES-MENT-HC              TO TAMIZAJES-MENT-HC-J
           MOVE MANEJO-TAMIZAJE-MENT-HC        TO MANEJO-TAMIZAJE-MENT-HC-J
           MOVE CAPTACION-MENT-HC              TO CAPTACION-MENT-HC-J
           MOVE CONSUMO-SPA-MENT-HC            TO CONSUMO-SPA-MENT-HC-J
           MOVE CANALIZA-DISPOS-MENT-HC        TO CANALIZA-DISPOS-MENT-HC-J
           MOVE ACCIONES-SEGUI-MENT-HC         TO ACCIONES-SEGUI-MENT-HC-J
           MOVE ADHERENCIA-TRAT-MENT-HC        TO ADHERENCIA-TRAT-MENT-HC-J
           MOVE SEGUI-CONTRA-MENT-HC           TO SEGUI-CONTRA-MENT-HC-J
           MOVE FECHA-CIERRE-CASO-HC           TO FECHA-CIERRE-CASO-HC-J
           MOVE EVALUACION-MHGAP-HC            TO EVALUACION-MHGAP-HC-J
           MOVE PRONOSTICO-TERA-MENT-HC        TO PRONOSTICO-TERA-MENT-HC-J
           MOVE OBSERVACIONES-MENT-HC          TO OBSERVACIONES-MENT-HC-J

           MOVE MULTIDROGORESIS-HC             TO MULTIDROGORESIS-HC-J
           MOVE CUAL-MULTIDROGORESIS-HC        TO CUAL-MULTIDROGORESIS-HC-J

           MOVE AGUD-VISUAL-OI-1-HC            TO AGUD-VISUAL-OI-1-HC-J
           MOVE AGUD-VISUAL-OI-2-HC            TO AGUD-VISUAL-OI-2-HC-J
           MOVE AGUD-VISUAL-OD-1-HC            TO AGUD-VISUAL-OD-1-HC-J
           MOVE AGUD-VISUAL-OD-2-HC            TO AGUD-VISUAL-OD-2-HC-J
           MOVE ESTRUCTURAS-OCULARES-OI-HC     TO ESTRUCTURAS-OCULARES-OI-HC-J
           MOVE ESTRUCTURAS-OCULARES-OD-HC     TO ESTRUCTURAS-OCULARES-OD-HC-J

           INITIALIZE TABLA-EDIT
           MOVE LIN-03    TO TABLA-EDIT
           PERFORM PASAR-LINEA VARYING I FROM 1 BY 1 UNTIL I > 4.

       PASAR-LINEA.
           MOVE TABLA-EDIT-W (I)  TO DATOS-PLANO-W

           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA 
           INSPECT DATOS-PLANO-W REPLACING ALL "\" BY " "
           INSPECT DATOS-PLANO-W REPLACING ALL "/" BY " "
           INSPECT DATOS-PLANO-W REPLACING ALL LOW-VALUES BY SPACES

           PERFORM DATOS-ENVIO.

       TABLA-TTO-SIFI.
           MOVE ","     TO CIERRE-TTO-SIFI

           MOVE FECHA-TTO-SIFI-HC   (I)  TO FECHA-TTO-SIFI-HC-J
           MOVE MEDICAMENTO-SIFI-HC (I)  TO MEDICAMENTO-SIFI-HC-J
           MOVE DOSIS-SIFI-HC       (I)  TO DOSIS-SIFI-HC-J
           MOVE PROFE-APLICO-SIFI-HC(I)  TO PROFE-APLICO-SIFI-HC-J


           INSPECT LIN-TTO-SIFI REPLACING ALL "*" BY CARAC-COMILLA 
           INSPECT LIN-TTO-SIFI REPLACING ALL "\" BY " "
           INSPECT LIN-TTO-SIFI REPLACING ALL "/" BY " "
           INSPECT LIN-TTO-SIFI REPLACING ALL LOW-VALUES BY SPACES

           IF I = 3
              MOVE "]},"   TO CIERRE-TTO-SIFI
           END-IF

           MOVE LIN-TTO-SIFI TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO.

       TABLA-SEGUI-SEROL.

           MOVE ","   TO CIERRE-SEGUI-SEROL
           MOVE TIPO-SEROL-HC  (I)    TO TIPO-SEROL-HC-J
           MOVE FECHA-SEROL-HC (I)    TO FECHA-SEROL-HC-J
           MOVE RESUL-SEROL-HC (I)    TO RESUL-SEROL-HC-J


           INSPECT LIN-SEGUI-SEROL REPLACING ALL "*" BY CARAC-COMILLA 
           INSPECT LIN-SEGUI-SEROL REPLACING ALL "\" BY " "
           INSPECT LIN-SEGUI-SEROL REPLACING ALL "/" BY " "
           INSPECT LIN-SEGUI-SEROL REPLACING ALL LOW-VALUES BY SPACES

           IF I = 4
              MOVE "]},"         TO CIERRE-SEGUI-SEROL
           END-IF

           MOVE LIN-SEGUI-SEROL TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO.

       TABLA-DIAGNOSTICOS.

           MOVE ","   TO CIERRE-DIAG
           MOVE COD-DIAGN-HC (I)   TO COD-DIAGN-HC-J

           IF I = 10 
              MOVE "]" TO CIERRE-DIAG
           END-IF

           INSPECT LIN-DIAG REPLACING ALL "*" BY CARAC-COMILLA 
           INSPECT LIN-DIAG REPLACING ALL "\" BY " "
           INSPECT LIN-DIAG REPLACING ALL "/" BY " "
           INSPECT LIN-DIAG REPLACING ALL LOW-VALUES BY SPACES

           MOVE LIN-DIAG TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO.

       DATOS-ENVIO.
010720     MOVE "datosrecibidos" TO COBW3-CNV-NAME
010730     MOVE DATOS-PLANO-W    TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_REPEAT" USING COBW3.


008040 COPY "P:\PROG\PROYECT\FUENTES\SC-WEB19.CBL".
