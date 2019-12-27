004820*=================================================================
004830*CREACION DE ARCHIVOS JSON DE NOTAS Y ENFERMERIA // JSON POR PACIENTES SELECIONADOS
004830*EL ARCHIVO DE EVOLUCIONES SE DESGLOSA POR ARCHIVOS DEPENDIENDO DE LA OPCION // DESDE LA OPCION 5-2-2 HASTA 5-2-6
004830*VALIDACION DE ESTADO-HC = 1  ABIERTA SOLO PARA GLUCOMETRIA // SIGNOS VITALES
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


           SELECT ARCHIVO-PROFESIONALES
                  ASSIGN NOM-PROF-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PROF   
                  ALTERNATE RECORD KEY
                  DESCRIP-PROF WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


	   SELECT ARCHIVO-OCUPACIONES 
                  ASSIGN NOM-OCUPA-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-OCU
                  ALTERNATE RECORD KEY IS
                  NOMBRE-OCU WITH DUPLICATES
                  FILE STATUS OTR-STAT.


	   SELECT ARCHIVO-ENTIDADES 
                  ASSIGN NOM-ENT-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC	
                  RECORD KEY   IS COD-ENT
                  ALTERNATE RECORD KEY IS
                  NOMBRE-ENT WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.



	   SELECT ARCHIVO-POS
                  ASSIGN NOM-POS-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-POS
                  ALTERNATE RECORD KEY IS
                  NOMBRE-POS WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-UNID-SERV   
                  ASSIGN NOM-UNSER-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CODIGO-UNID-SERV
                  FILE STATUS IS OTR-STAT.


           SELECT EVOLUCION-HISTORIA
                  ASSIGN NOM-HCEVO-W LOCK MODE IS AUTOMATIC
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-EVO
                  ALTERNATE RECORD KEY IS FOLIO-EVO WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FECHA-EVO WITH DUPLICATES
                  ALTERNATE RECORD KEY IS MED-EVO WITH DUPLICATES
                  FILE STATUS OTR-STAT.


           SELECT HISTORIA-CLINICA LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-HCAPE-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-HC
                  ALTERNATE RECORD KEY IS FOLIO-HC WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FECHA-HC WITH DUPLICATES
                  ALTERNATE RECORD KEY IS MED-HC WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ESTADO-HC WITH DUPLICATES
                  FILE STATUS OTR-STAT.


           SELECT HISTORIA-DETALLADA LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-HCDET-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-DETHC
                  ALTERNATE RECORD KEY IS LLAVE-FOLIO-DETHC
                  WITH DUPLICATES
                  FILE STATUS OTR-STAT.



           SELECT EVOLUCION-DETALLADA LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-DETEVO-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-DEVO
                  ALTERNATE RECORD KEY IS FOLIO-DEVO WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FECHA-DEVO WITH DUPLICATES
                  FILE STATUS OTR-STAT.


           SELECT ARCHIVO-FORMULA-LAB
                  ASSIGN  NOM-FORM-LAB-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-LAB
                  ALTERNATE RECORD KEY DESCRIP-LAB WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-FORMULA-IMAG
                  ASSIGN NOM-FORM-IMAG-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-IMAG
                  ALTERNATE RECORD KEY DESCRIP-IMAG WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-FORMULA-ORD
                  ASSIGN NOM-FORM-ORD-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-ORD
                  ALTERNATE RECORD KEY DESCRIP-ORD  WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-FORMULA-CONS
                  ASSIGN NOM-FORM-CONS-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-CONS
                  ALTERNATE RECORD KEY DESCRIP-CONS WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


	   SELECT ARCHIVO-MACRO-HIS
                  ASSIGN NOM-MACRO-HIS-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-MACROH
                  ALTERNATE RECORD KEY IS
                  NOMBRE-MACROH WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


	   SELECT ARCHIVO-VIA-ACCESO
                  ASSIGN NOM-VIA-ACCESO-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC	
                  RECORD KEY   IS COD-VIA
                  ALTERNATE RECORD KEY IS
                  NOMBRE-VIA WITH DUPLICATES 
                  FILE STATUS IS OTR-STAT.


	   SELECT ARCHIVO-ENFERMEDAD
                  ASSIGN NOM-ENFERMEDAD-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC	
                  RECORD KEY   IS LLAVE-ENF
                  ALTERNATE RECORD KEY IS
                  NOMBRE-ENF WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-MACRO-EVOL
                  ASSIGN NOM-MACRO-EVOL-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-MACROEVOL
                  ALTERNATE RECORD KEY DETALLE-MACROEVOL WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-CITAS
                  ASSIGN NOM-CITAS-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-CIT 
                  ALTERNATE RECORD KEY IS PACI-CIT WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LLAVE-ALT-CIT WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


	   SELECT PATOLOGIAS-CRONICAS
                  ASSIGN NOM-PAT-CRO-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-CRONIC
                  ALTERNATE RECORD KEY IS
                  DESCRIP-CRONIC WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


005030     SELECT ARCHIVO-JSON
005040            ASSIGN NOM-PLANO-W
005050            ORGANIZATION IS LINE SEQUENTIAL
005060            FILE STATUS IS OTR-STAT.

005030     SELECT ARCHIVO-JSON2
005040            ASSIGN NOM-PLANO2-W
005050            ORGANIZATION IS LINE SEQUENTIAL
005060            FILE STATUS IS OTR-STAT.

005030     SELECT ARCHIVO-JSON3
005040            ASSIGN NOM-PLANO3-W
005050            ORGANIZATION IS LINE SEQUENTIAL
005060            FILE STATUS IS OTR-STAT.

005070
005080 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
005110 COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".


005110 COPY "S:\NEWCOBOL\FUENTES\FD-RESTR.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-USUAR.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-PACIE.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-PROFE.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-OCUPA.CBL".            
005110 COPY "S:\NEWCOBOL\FUENTES\FD-ENTID.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-CODPO.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-UNSER.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-HCEVO17.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-HCAPER17.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-HCDETAL17.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-DETEVO17.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-FORLA.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-FORIM.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-FOROM.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-FORCT.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-MACR1.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-HCVIA.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-ENFER.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-MACREV.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-CITAS.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-CRONI.CBL".

005130
005140 FD  ARCHIVO-JSON
005150     LABEL RECORD STANDARD.
005160 01  REG-PLANO.
005170     02 DATOS-PLANO     PIC X(700).

005140 FD  ARCHIVO-JSON2
005150     LABEL RECORD STANDARD.
005160 01  REG-PLANO2.
005170     02 DATOS-PLANO2     PIC X(700).

005140 FD  ARCHIVO-JSON3
005150     LABEL RECORD STANDARD.
005160 01  REG-PLANO3.
005170     02 DATOS-PLANO3     PIC X(700).

005190 WORKING-STORAGE SECTION.
005200 COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
005210 COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".
005220 
000981 77  NOM-PACIE-W               PIC X(70).
       77  NOM-PROF-W                PIC X(70).
000982 77  NOM-OCUPA-W               PIC X(70).
       77  NOM-POS-W                 PIC X(70).
       77  NOM-ENT-W                 PIC X(70).
       77  NOM-CIUD-W                PIC X(70).
       77  NOM-UNSER-W               PIC X(70).
       77  NOM-HCEVO-W               PIC X(70).
       77  NOM-HCAPE-W               PIC X(70).
       77  NOM-HCDET-W               PIC X(70).
       77  NOM-DETEVO-W              PIC X(70).
       77  NOM-FORM-LAB-W            PIC X(70).
       77  NOM-FORM-IMAG-W           PIC X(70).
       77  NOM-FORM-ORD-W            PIC X(70).
       77  NOM-FORM-CONS-W           PIC X(70). 
       77  NOM-MACRO-HIS-W           PIC X(70).
       77  NOM-VIA-ACCESO-W          PIC X(70).
       77  NOM-ENFERMEDAD-W          PIC X(70).

       77  NOM-CITAS-W               PIC X(70).
       77  NOM-PAT-CRO-W             PIC X(70).

001000 77  NOM-PLANO-W               PIC X(90).
001000 77  NOM-PLANO2-W              PIC X(90).
       77  NOM-PLANO3-W              PIC X(90).
       77  NOM-USUAR-W               PIC X(70).
       77  PACI-EDIT                 PIC ZZZZZZZZZZZZZZZ.
       77  EDITADO-W                 PIC X(10).
       77  NUMERO-EDIT               PIC ZZ.ZZ.
       77  HORAS-EDIT                PIC ZZZ.Z.


005260
005230 77  SW-VAR                    PIC 9.
005240 77  K                         PIC 999.
       01  CONT-T                    PIC 999.
       01  CONT                      PIC 999.
       77  NRO-EDIT                  PIC ZZZZZZZZZZZZ.
       77  CANT-EDIT                 PIC ZZZZ.Z.
       77  PROCENTAJE-EDIT           PIC ZZ.Z.

000228 01 COD-TMP.
000229    02 SW-ALFA                 PIC 9.
000230    02 TABLA-COD1.
000231       03 COD1-PAC             PIC X OCCURS 10.
000232    02 TABLA-COD2.
000233       03 COD2-PAC             PIC X OCCURS 10.


       01 ID-EDIT                    PIC 9.
       01 ID-PACIENT-W               PIC X(15).


       01 FECHA-ACT.
          02 ANO-ACT        PIC 99.
          02 MES-ACT        PIC 99.
          02 DIA-ACT        PIC 99.

005270 01  DATO-LLEGADA-W.
           02 SESION-LLEGAD-W.
005280        03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 FILLER                    PIC X.
           02 LISTA-PACIENTES   OCCURS 10.
              03 PACIENT-W              PIC X(15).
              03 FILLER                 PIC X.


       01 LINEA-HTML.
          02 GRP-MACROH-W            PIC XX.
          02 FILLER                  PIC X VALUE ";".
          02 ITEM-MACROH-W           PIC X(11).
          02 FILLER                  PIC X VALUE "|".


       01 REGLON-HTML.
          02 RENG-HTML               PIC X(15) OCCURS 30.


       01 LINEA-HTML2.
          02 VIAS-MACROEVOL-W        PIC XX.
          02 FILLER                  PIC X VALUE "|".


       01 REGLON2-HTML.
          02 RENG2-HTML               PIC X(3) OCCURS 6.


       01 TABLA-DEVO-W.
          02 COMA-W                   PIC X.
          02 NOM-REG-W                PIC X(11).
          02 FILLER                   PIC X     VALUE "*".
          02 RENG-DEVO-W              PIC X(95).
          02 FILLER                   PIC XX     VALUE "*}".


       01 DETALLE-HC-W.
          02 COMA-DETALLE-W           PIC X.
          02 NOM-DETALLE-W            PIC X(11).
          02 FILLER                   PIC X     VALUE "*".
          02 RENG-DETALLE-W           PIC X(95).
          02 FILLER                   PIC X     VALUE "|".
          02 FILLER                   PIC XX    VALUE "*}".



       01 LIN-1.
          02 FILLER                  PIC X(9)   VALUE "{*LLAVE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LLAVE-HC-J              PIC X(23).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*ESTA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESTADO-HC-J             PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*UNSERV*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UNSERV-HC-J             PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*SERV*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SERV-HC-J               PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FECHA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-HC-J              PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*HORA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HORA-HC-J               PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*UNID-EDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UNID-EDAD-HC-J          PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*VLR-EDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VLR-EDAD-HC-J           PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*ESQUEMA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESQUEMA-HC-J            PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*CLASE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLASE-HC-J              PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*CAUSA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CAUSA-HC-J              PIC XX.
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-LIN-1            PIC X.

      *** Signos vitales 

       01 LIN-2.
          02 FILLER                  PIC X(9)   VALUE "{*LLAVE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LLAVE-PACI-EVO-J        PIC X(23).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FECHA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-EVO-J             PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*HORA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HORA-EVO-J              PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*OPER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OPER-EVO-J              PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*PESO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PESO-EVO-J              PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*TALLA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TALLA-EVO-J             PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*IMC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 IMC-CORP-EVO-J          PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*SUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SUP-CORP-EVO-J          PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*TEMP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TEMP-EVO-J              PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FCARD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FCARD-EVO-J             PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FRESP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FRESP-EVO-J             PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*TENS1*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TENS1-EVO-J             PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*TENS2*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TENS2-EVO-J             PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*TENS-MED*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TENS-MEDIA-EVO-J        PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*PVC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PVC-EVO-J               PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*OXIMETRIA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OXIMETRIA-EVO-J         PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*FCARD-FET*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FCARD-FET-EVO-J         PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*UNID-EDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UNID-EDAD-J             PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*VLR-EDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VLR-EDAD-J              PIC X(3).
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-LIN-2           PIC X.

      *** Glucometrias

       01 LIN-3.
          02 FILLER                  PIC X(9)   VALUE "{*LLAVE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LLAVE-FORM-EVO-J        PIC X(23).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FECHA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-FORM-EVO-J        PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*HORA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HORA-FORM-EVO-J         PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*OPER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OPER-FORM-EVO-J         PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*TIPO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-FORM-EVO-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*GLUCO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 GLUCOMETRIA-EVO-J       PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-FORM-EVO-J          PIC X(13).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-FORM-EVO-J      PIC X(57).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*CANT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CANT-FORM-EVO-J         PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*INDIC1*:".
          02 FILLER                  PIC X      VALUE "*".
          02 INDIC1-FORM-EVO-J       PIC X(78).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*INDIC2*:".
          02 FILLER                  PIC X      VALUE "*".
          02 INDIC2-FORM-EVO-J       PIC X(78).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*DOSIS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DOSIS-FORM-EVO-J        PIC X(15).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*CULTIVO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CULTIVO-MICRO-J         PIC X(34).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*INST*:".
          02 FILLER                  PIC X      VALUE "*".
          02 INST-FORM-EVO-J         PIC X(12).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*TIPO-INCAP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-INCAP-FORM-J       PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*GRDO-INCAP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 GRDO-INCAP-FORM-J       PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*PORROG*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PORROG-FORM-EVO-J       PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*NRO-ORD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NRO-ORD-FORM-EVO-J      PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*NRO-PED*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NRO-PED-FORM-EVO-J      PIC X(6).
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-LIN-3            PIC X.

      *** Control nuerologia

       01 LIN-4.
          02 FILLER                  PIC X(9)   VALUE "{*LLAVE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LLAVE-NEUR-EVO-J        PIC X(23).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FECHA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-NEUR-EVO-J        PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*HORA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HORA-NEUR-EVO-J         PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*OPER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OPER-NEUR-EVO-J         PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*TEMP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TEMP-NEUR-EVO-J         PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FCARD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FCARD-NEUR-EVO-J        PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FRESP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FRESP-NEUR-EVO-J        PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*TENS1*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TENS1-NEUR-EVO-J        PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*TENS2*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TENS2-NEUR-EVO-J        PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*PVC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PVC-NEUR-EVO-J          PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*APER-OCUL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 APER-OCUL-EVO-J         PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*RESP-VERB*:".
          02 FILLER                  PIC X      VALUE "*".
          02 RESP-VERB-EVO-J         PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*RESP-MOTO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 RESP-MOTO-EVO-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*VLR-GLSG*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VLR-GLASG-EVO-J         PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*PUPILAS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PUPILAS-EVO-J           PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*MUSCULAR*:".
          02 FILLER                  PIC X      VALUE "*".
          02 MUSCULAR-EVO-J          PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*OBSERV*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OBSERV-EVO-J            PIC X(95)  VALUE SPACES.
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-LIN-4            PIC X.



      *** Registros oxigeno

       01 LIN-5.
          02 FILLER                  PIC X(9)   VALUE "{*LLAVE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LLAVE-OXI-EVO-J         PIC X(23).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FECHA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-OXI-EVO-J         PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*HORA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HORA-OXI-EVO-J          PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*OPER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OPER-OXI-EVO-J          PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*DOSIS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DOSIS-OXI-EVO-J         PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*TIPO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-OXI-EVO-J          PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESC-OXI-EVO-J          PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*TIEM*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CANT-FORM-OXI-EVO-J     PIC X(5).
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-LIN-5            PIC X.

      **** Control de liquidos 

       01 LIN-6.
          02 FILLER                  PIC X(9)   VALUE "{*LLAVE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LLAVE-LIQ-EVO-J         PIC X(23).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FECHA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-LIQ-EVO-J         PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*HORA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HORA-LIQ-EVO-J          PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*OPER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OPER-LIQ-EVO-J          PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*ADM-ORAL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LIQ-ADM-ORAL-EVO-J      PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*ADM-VENA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LIQ-ADM-VENA-EVO-J      PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*ADM-TRAN*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LIQ-ADM-TRAN-EVO-J      PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*ADM-ORIN*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LIQ-ELI-ORIN-EVO-J      PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*ADM-DIAR*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LIQ-ELI-DIAR-EVO-J      PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*ADM-SOND*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LIQ-ELI-SOND-EVO-J      PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*ADM-OTR1*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LIQ-ELI-OTR1-EVO-J      PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*ADM-DET*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LIQ-ELI-DET-EVO-J       PIC X(10).
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-LIN-6            PIC X.


      * macro evoluciones

001812 01 LIN-7.
001820    02 FILLER                  PIC X(7)   VALUE "{*COD*:".
001830    02 FILLER                  PIC X      VALUE "*".
001840    02 LLAVE-MACROEVOL-J       PIC X(7).
001850    02 FILLER                  PIC XX     VALUE "*,".
001860    02 FILLER                  PIC X(10)  VALUE "*DETALLE*:".
001870    02 FILLER                  PIC X      VALUE "*".
001880    02 DETALLE-MACROEVOL-J     PIC X(40).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*VIAS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VIAS-MACROEVOL-J        PIC X(18).
001970    02 FILLER                  PIC XX     VALUE "*}".    
001980    02 CIERRE-LIN-7            PIC X.


      * citas...


001812 01 LIN-8.
001820    02 FILLER                  PIC X(9)   VALUE "{*FECHA*:".
001830    02 FILLER                  PIC X      VALUE "*".
001840    02 FECHA-CIT-J             PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*MEDI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 MED-CIT-J               PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PACI-CIT-J              PIC X(15).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*TIPO-FACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-FACT-CIT-J         PIC X.
001850    02 FILLER                  PIC XX     VALUE "*,".
001860    02 FILLER                  PIC X(10)  VALUE "*CUPSCIT*:".
001870    02 FILLER                  PIC X      VALUE "*".
001880    02 CUPS-CIT-J              PIC X(12).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*ESTADO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESTADO-CIT-J            PIC X.
001970    02 FILLER                  PIC XX     VALUE "*}".    
001980    02 CIERRE-LIN-8            PIC X.


      * historias detalladas...

001812 01 LIN-9.
001820    02 FILLER                  PIC X(7)   VALUE "{*COD*:".
001830    02 FILLER                  PIC X      VALUE "*".
001840    02 LLAVE-HC-DETHC-J        PIC X(23).
001850    02 FILLER                  PIC XX     VALUE "*,".
001860    02 FILLER                  PIC X(10)  VALUE "*CODGRPO*:".
001870    02 FILLER                  PIC X      VALUE "*".
001880    02 COD-DETHC-J             PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*DETALLE*:[".



       01 LIN-10.
          02 FILLER                  PIC X(9)   VALUE "{*LLAVE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LLAVE-PACI-DEVO-J       PIC X(23).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FECHA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-DEVO-J            PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*HORA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HORA-DEVO-J             PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*OPER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OPER-ELAB-DEVO-J        PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*TIPO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-DEVO-J             PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*DATOS-DEVO*:[".



006300
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
002650        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
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
006440        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
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
006560        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.
006710

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-OCUPACIONES.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-OCUPA-W      TO MENSAJE2-HTML
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.


006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ENTIDADES.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-ENT-W        TO MENSAJE2-HTML
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-POS.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-POS-W        TO MENSAJE2-HTML
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-UNID-SERV.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-UNSER-W      TO MENSAJE2-HTML
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON HISTORIA-DETALLADA.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" OR "35"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-HCDET-W      TO MENSAJE2-HTML
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-FORMULA-LAB.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-FORM-LAB-W   TO MENSAJE2-HTML
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-FORMULA-IMAG.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-FORM-IMAG-W  TO MENSAJE2-HTML
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-FORMULA-ORD.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-FORM-ORD-W   TO MENSAJE2-HTML
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-FORMULA-CONS.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-FORM-CONS-W  TO MENSAJE2-HTML
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-MACRO-HIS.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-MACRO-HIS-W  TO MENSAJE2-HTML
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-VIA-ACCESO.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-VIA-ACCESO-W TO MENSAJE2-HTML
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ENFERMEDAD.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-ENFERMEDAD-W TO MENSAJE2-HTML
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON PATOLOGIAS-CRONICAS.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-PAT-CRO-W    TO MENSAJE2-HTML
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" OR "35"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-PLANO-W      TO MENSAJE2-HTML
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON2.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" OR "35"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-PLANO2-W     TO MENSAJE2-HTML
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON3.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" OR "35"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-PLANO3-W     TO MENSAJE2-HTML
006680        MOVE "HC-NTAS-AM1"    TO MENSAJE3-HTML
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
           INITIALIZE DATO-LLEGADA-W
006800     MOVE "datosh" TO COBW3-SEARCH-DATA.
006810     CALL "COBW3_GET_VALUE" USING COBW3.
006820     MOVE COBW3-GET-DATA    TO DATO-LLEGADA-W.

001855     MOVE SESION-LLEGAD-W   TO LLAVE-SESION-W.
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

           INITIALIZE NOM-USU-W NOM-USUAR-W NOM-REST-W NOM-PLANO-W NOM-PROF-W 
                      NOM-OCUPA-W NOM-ENT-W NOM-POS-W NOM-UNSER-W NOM-HCEVO-W
                      NOM-FORM-LAB-W NOM-FORM-IMAG-W NOM-FORM-ORD-W NOM-FORM-CONS-W NOM-MACRO-HIS-W
                      NOM-HCDET-W NOM-VIA-ACCESO-W NOM-ENFERMEDAD-W NOM-CITAS-W NOM-PAT-CRO-W.


           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\".
           MOVE "\\"  TO NOM-REST-W  NOM-HCEVO-W  NOM-USUAR-W  NOM-PLANO-W NOM-VIA-ACCESO-W
007150     INSPECT NOM-REST-W  REPLACING FIRST "                    "
007160                                      BY IP-DATOS-USUNET
007170     INSPECT NOM-REST-W  REPLACING FIRST " "
007180                                      BY "\"
007190     INSPECT NOM-REST-W  REPLACING FIRST "                          "
007200                                      BY "NEWCOBOL\DATOS\ARCHREST.SC".

           MOVE NOM-REST-W     TO NOM-OCUPA-W NOM-ENT-W NOM-POS-W NOM-ENFERMEDAD-W NOM-PAT-CRO-W.

           INSPECT NOM-OCUPA-W REPLACING FIRST   "ARCHREST.SC  "
                                            BY   "SC-OCUPAC.DAT".

           INSPECT NOM-ENT-W   REPLACING FIRST   "ARCHREST.SC"
                                            BY   "ARCHENT.DAT".

           INSPECT NOM-POS-W   REPLACING FIRST   "ARCHREST.SC "
                                            BY   "SC-FARMA.DAT".


           INSPECT NOM-ENFERMEDAD-W REPLACING FIRST "ARCHREST.SC"
                                                 BY "ARCHENF.DAT".

           INSPECT NOM-PAT-CRO-W  REPLACING FIRST   "ARCHREST.SC "
                                                 BY "SC-CRONI.DAT"


           INSPECT NOM-HCEVO-W REPLACING FIRST  "                    "
                                            BY  IP-DATOS-USUNET

           INSPECT NOM-HCEVO-W REPLACING FIRST  " "
                                            BY  "\"

           INSPECT NOM-HCEVO-W REPLACING FIRST  "                  "
                                            BY  "NEWCOBOL\HC\DATOS\"

           INSPECT NOM-HCEVO-W REPLACING FIRST  "              "
                                            BY  "SC-HCEVOX1.S17".


           MOVE  NOM-HCEVO-W   TO NOM-FORM-LAB-W NOM-FORM-IMAG-W NOM-FORM-ORD-W NOM-FORM-CONS-W 
                                  NOM-MACRO-HIS-W  NOM-CITAS-W NOM-HCDET-W NOM-HCAPE-W NOM-DETEVO-W.


           INSPECT NOM-HCEVO-W REPLACING FIRST  "X1"
                                            BY  ANO-ACT.


           INSPECT NOM-FORM-LAB-W  REPLACING FIRST  "SC-HCEVOX1.S17"
                                                 BY "SC-LABOR.DAT  ".

           INSPECT NOM-FORM-IMAG-W REPLACING FIRST  "SC-HCEVOX1.S17"
                                                 BY "SC-IMAGE.DAT  ".

           INSPECT NOM-FORM-ORD-W  REPLACING FIRST  "SC-HCEVOX1.S17 "
                                                 BY "SC-ORDMEDIC.DAT".

           INSPECT NOM-FORM-CONS-W REPLACING FIRST  "SC-HCEVOX1.S17"
                                                 BY "SC-CONST.DAT  ".

           INSPECT NOM-MACRO-HIS-W REPLACING FIRST  "SC-HCEVOX1.S17"
                                                 BY "SC-MACDR.DAT  ".

           INSPECT NOM-CITAS-W    REPLACING FIRST   "SC-HCEVOX1.S17"
                                                 BY "SC-AGEND.DAT  ".


           INSPECT NOM-VIA-ACCESO-W REPLACING FIRST "                    "
                                                 BY  IP-DATOS-USUNET

           INSPECT NOM-VIA-ACCESO-W REPLACING FIRST " "
                                                 BY "\"

           INSPECT NOM-VIA-ACCESO-W REPLACING FIRST "                            "
                                                 BY "NEWCOBOL\DATOS-H\SC-VIAS.DAT".


           INSPECT NOM-HCAPE-W REPLACING FIRST  "SC-HCEVOX1.S17"
                                            BY  "SC-HCAPER.S17 ".

           INSPECT NOM-USUAR-W REPLACING FIRST  "                    "
                                            BY  IP-DATOS-USUNET
           INSPECT NOM-USUAR-W REPLACING FIRST  " "
                                            BY  "\"
           INSPECT NOM-USUAR-W REPLACING FIRST  "                              "
007183                                      BY  DIRECTORIO-USUNET
           INSPECT NOM-USUAR-W REPLACING FIRST  "                "
                                            BY  "\CONTROL\ARCHUSU".

           MOVE NOM-USUAR-W  TO NOM-PACIE-W NOM-PROF-W NOM-UNSER-W
           
           INSPECT NOM-PACIE-W  REPLACING FIRST "ARCHUSU     "
                                             BY "SC-PACIE.DAT".

           INSPECT NOM-PROF-W  REPLACING FIRST  "ARCHUSU     "
                                            BY  "ARCHPROF.DAT".

           INSPECT NOM-UNSER-W REPLACING FIRST  "ARCHUSU     "
                                            BY  "SC-UNSER.DAT".

007220     INSPECT NOM-PLANO-W  REPLACING FIRST "                    "
007230                                       BY IP-DATOS-USUNET
007240     INSPECT NOM-PLANO-W  REPLACING FIRST " "
007250                                       BY "\"
007260     INSPECT NOM-PLANO-W  REPLACING FIRST "                                                                 "
007280                                       BY "NEWCOBOL\DATOS\JSON\SC-ARCHAPE-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"
007281     INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
007282                                       BY SESION-LLEGAD-W.

        ABRIR-USUARIOS.
           OPEN INPUT ARCHIVO-USUARIOS
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.
          

       ABRIR-HISTORIA.

           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT  HISTORIA-CLINICA
                       ARCHIVO-PACIENTES.

           INITIALIZE REG-HC CONT J.
           MOVE "{*HISTORIA*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.



       LEER-HISTORIA.
           READ HISTORIA-CLINICA NEXT WITH NO LOCK AT END GO TO CERRAR-HISTORIA.

           MOVE 1 TO SW-VAR 
           MOVE ID-HC    TO ID-PACIENT-W

           PERFORM VERIFICAR-PACIENTES VARYING J FROM 1 BY 1
                                                UNTIL J > 10
           INITIALIZE J

           IF SW-VAR = 1
              GO TO LEER-HISTORIA
           END-IF 

      *     IF ESTADO-HC = 2 OR ESTADO-HC = 0
      *        GO TO LEER-HISTORIA
      *     END-IF

      *     IF UNSERV-HC IS NOT EQUAL TO 09
      *        GO TO LEER-HISTORIA
      *     END-IF
      
           MOVE ID-HC             TO COD-PACI
           READ ARCHIVO-PACIENTES WITH NO LOCK
                INVALID KEY GO TO LEER-HISTORIA
           END-READ

           ADD 1 TO CONT

           MOVE ID-HC             TO PACI-EDIT
           MOVE PACI-EDIT         TO ID-HC

           MOVE LLAVE-HC          TO LLAVE-HC-J
           MOVE ESTADO-HC         TO ESTADO-HC-J
           MOVE UNSERV-HC         TO UNSERV-HC-J
           MOVE SERV-HC           TO SERV-HC-J
           MOVE FECHA-HC          TO FECHA-HC-J
           MOVE HORA-HC           TO HORA-HC-J
           MOVE UNID-EDAD-HC      TO UNID-EDAD-HC-J
           MOVE VLR-EDAD-HC       TO VLR-EDAD-HC-J
           MOVE ESQUEMA-HC        TO ESQUEMA-HC-J
           MOVE CLASE-HC          TO CLASE-HC-J
           MOVE CAUSA-HC          TO CAUSA-HC-J

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-1
           MOVE LIN-1 TO DATOS-PLANO
           WRITE REG-PLANO

           GO TO LEER-HISTORIA.

       CERRAR-HISTORIA.

           INITIALIZE CIERRE-LIN-1

           IF CONT < 1
              INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
              INITIALIZE LLAVE-HC-J UNSERV-HC-J SERV-HC-J FECHA-HC-J HORA-HC-J 
                         UNID-EDAD-HC-J VLR-EDAD-HC-J ESQUEMA-HC-J CLASE-HC-J ESTADO-HC-J
           END-IF

           MOVE LIN-1 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           MOVE "]}" TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           CLOSE ARCHIVO-JSON
                 ARCHIVO-PACIENTES
                 HISTORIA-CLINICA.

       ABRIR-EVOLUCIONES.

           INSPECT NOM-PLANO-W  REPLACING FIRST "SC-ARCHAPE"
                                             BY "SC-ARCHSIG"

           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT  HISTORIA-CLINICA
                       EVOLUCION-HISTORIA
                       ARCHIVO-POS
                       ARCHIVO-PACIENTES.

           INITIALIZE REG-EVO CONT J.
           MOVE "{*HC-SIG*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.


           MOVE NOM-PLANO-W     TO  NOM-PLANO2-W


           INSPECT NOM-PLANO2-W REPLACING FIRST "SC-ARCHSIG"
                                             BY "SC-ARCHGLC"

           OPEN OUTPUT ARCHIVO-JSON2.

           MOVE "{*HC-GLUCO*:[" TO  DATOS-PLANO2
           INSPECT DATOS-PLANO2 REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO2.


           MOVE NOM-PLANO-W     TO  NOM-PLANO3-W


           INSPECT NOM-PLANO3-W REPLACING FIRST "SC-ARCHSIG"
                                             BY "SC-ARCHNEU"

           OPEN OUTPUT ARCHIVO-JSON3.

           MOVE "{*HC-NEURO*:[" TO  DATOS-PLANO3
           INSPECT DATOS-PLANO3 REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO3.


       LEER-EVOLUCIONES.
           READ EVOLUCION-HISTORIA NEXT WITH NO LOCK AT END GO TO CERRAR-EVOLUCIONES.

           MOVE 1 TO SW-VAR 
           MOVE ID-EVO   TO ID-PACIENT-W

           PERFORM VERIFICAR-PACIENTES VARYING J FROM 1 BY 1
                                                UNTIL J > 10
           INITIALIZE J

           IF SW-VAR = 1
              GO TO LEER-EVOLUCIONES
           END-IF 

           MOVE ID-EVO           TO COD-PACI
           READ ARCHIVO-PACIENTES WITH NO LOCK
                INVALID KEY GO TO LEER-EVOLUCIONES
           END-READ


           MOVE LLAVE-PACI-EVO   TO LLAVE-HC

           READ HISTORIA-CLINICA WITH NO LOCK
                INVALID KEY GO TO LEER-EVOLUCIONES
           END-READ

           IF (MUSCULAR-EVO > ZEROS) AND (PUPILAS-EVO > ZEROS)
              PERFORM LLENAR-NEUROLOGIA
           END-IF


           IF  ESTADO-HC = 2 OR ESTADO-HC = 0
              GO TO LEER-EVOLUCIONES
           END-IF

           IF GLUCOMETRIA-EVO IS NOT ZEROS 
              PERFORM LLENAR-TABLA-FORMU
           END-IF

           MOVE 0 TO SW-BUSCAR

           IF TEMP-EVO      IS NOT = ZEROS MOVE 1 TO SW-BUSCAR.
           IF FCARD-EVO     IS NOT = ZEROS MOVE 1 TO SW-BUSCAR.
           IF FRESP-EVO     IS NOT = ZEROS MOVE 1 TO SW-BUSCAR.
           IF TENS1-EVO     IS NOT = ZEROS MOVE 1 TO SW-BUSCAR.
           IF TENS2-EVO     IS NOT = ZEROS MOVE 1 TO SW-BUSCAR.
           IF PVC-EVO       IS NOT = ZEROS MOVE 1 TO SW-BUSCAR.
           IF OXIMETRIA-EVO IS NOT = ZEROS MOVE 1 TO SW-BUSCAR.
           IF FCARD-FET-EVO IS NOT = ZEROS MOVE 1 TO SW-BUSCAR.

           IF SW-BUSCAR = 0 
              GO TO LEER-EVOLUCIONES
           END-IF

           ADD  1 TO CONT-T

           MOVE ID-EVO         TO PACI-EDIT
           MOVE PACI-EDIT      TO ID-EVO

           MOVE LLAVE-PACI-EVO TO LLAVE-PACI-EVO-J
           MOVE FECHA-EVO      TO FECHA-EVO-J
           MOVE HORA-EVO       TO HORA-EVO-J
           MOVE OPER-ELAB-EVO  TO OPER-EVO-J

           IF PESO-EVO = LOW-VALUES
              INITIALIZE PESO-EVO
           END-IF 

           MOVE PESO-EVO       TO PESO-EVO-J
           MOVE TALLA-EVO      TO TALLA-EVO-J

           MOVE IMC-CORP-EVO   TO NUMERO-EDIT
           MOVE NUMERO-EDIT    TO IMC-CORP-EVO-J

           MOVE SUP-CORP-EVO   TO NUMERO-EDIT  
           MOVE NUMERO-EDIT    TO SUP-CORP-EVO-J

           MOVE TEMP-EVO       TO NUMERO-EDIT
           MOVE NUMERO-EDIT    TO TEMP-EVO-J

           MOVE FCARD-EVO      TO FCARD-EVO-J
           MOVE FRESP-EVO      TO FRESP-EVO-J
           MOVE TENS1-EVO      TO TENS1-EVO-J
           MOVE TENS2-EVO      TO TENS2-EVO-J
           MOVE TENS-MEDIA-EVO TO TENS-MEDIA-EVO-J
           MOVE PVC-EVO        TO PVC-EVO-J
           MOVE OXIMETRIA-EVO  TO OXIMETRIA-EVO-J
           MOVE FCARD-FET-EVO  TO FCARD-FET-EVO-J
           MOVE UNID-EDAD-HC   TO UNID-EDAD-J
           MOVE VLR-EDAD-HC    TO VLR-EDAD-J

           INSPECT LIN-2 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-2 
           MOVE LIN-2  TO DATOS-PLANO
           WRITE REG-PLANO


           GO TO LEER-EVOLUCIONES.


       LLENAR-TABLA-FORMU.

           ADD  1 TO CONT
           MOVE COD-FORMU          (1) TO LLAVE-POS
           READ ARCHIVO-POS WITH NO LOCK
                INVALID KEY MOVE SPACES   TO NOMBRE-POS
           END-READ
           
           MOVE NOMBRE-POS             TO DESCRIP-FORM-EVO-J
           INSPECT DESCRIP-FORM-EVO-J REPLACING ALL "*" BY "X"

           MOVE ID-EVO                 TO PACI-EDIT
           MOVE PACI-EDIT              TO ID-EVO

           MOVE LLAVE-PACI-EVO         TO LLAVE-FORM-EVO-J
           MOVE FECHA-EVO              TO FECHA-FORM-EVO-J
           MOVE HORA-EVO               TO HORA-FORM-EVO-J
           MOVE OPER-ELAB-EVO          TO OPER-FORM-EVO-J
           MOVE GLUCOMETRIA-EVO        TO GLUCOMETRIA-EVO-J
           MOVE TIPO-FORMU         (1) TO TIPO-FORM-EVO-J
           MOVE COD-FORMU          (1) TO COD-FORM-EVO-J
           MOVE CANT-FORMU         (1) TO CANT-EDIT
           MOVE CANT-EDIT              TO CANT-FORM-EVO-J
           MOVE INDIC-FORMU        (1) TO INDIC1-FORM-EVO-J
           MOVE INDIC2-1-FORMU     (1) TO INDIC2-FORM-EVO-J
           MOVE VAR-DOSIS-FORMU    (1) TO DOSIS-FORM-EVO-J
           MOVE CULTIVO-MICRO-FORMU(1) TO CULTIVO-MICRO-J
           MOVE INSTITUTO-FORMU    (1) TO INST-FORM-EVO-J
           MOVE TIPO-INCAP-FORMU   (1) TO TIPO-INCAP-FORM-J
           MOVE GRADO-INCAP-FORMU  (1) TO GRDO-INCAP-FORM-J
           MOVE PRORROG-FORMU      (1) TO PORROG-FORM-EVO-J
           MOVE NRO-ORD-FORMU      (1) TO NRO-ORD-FORM-EVO-J
           MOVE NRO-PED-FORMU      (1) TO NRO-PED-FORM-EVO-J

           INSPECT LIN-3 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-3
           MOVE LIN-3 TO DATOS-PLANO2
           WRITE REG-PLANO2.


       LLENAR-NEUROLOGIA.

           ADD  1 TO K
           MOVE ID-EVO                 TO PACI-EDIT
           MOVE PACI-EDIT              TO ID-EVO

           MOVE LLAVE-PACI-EVO         TO LLAVE-NEUR-EVO-J
           MOVE FECHA-EVO              TO FECHA-NEUR-EVO-J
           MOVE HORA-EVO               TO HORA-NEUR-EVO-J
           MOVE OPER-ELAB-EVO          TO OPER-NEUR-EVO-J
           MOVE TEMP-EVO               TO NUMERO-EDIT
           MOVE NUMERO-EDIT            TO TEMP-NEUR-EVO-J
           MOVE FCARD-EVO              TO FCARD-NEUR-EVO-J
           MOVE FRESP-EVO              TO FRESP-NEUR-EVO-J
           MOVE TENS1-EVO              TO TENS1-NEUR-EVO-J
           MOVE TENS2-EVO              TO TENS2-NEUR-EVO-J
           MOVE PVC-EVO                TO PVC-NEUR-EVO-J
           MOVE APER-OCUL-EVO          TO APER-OCUL-EVO-J
           MOVE RESP-VERB-EVO          TO RESP-VERB-EVO-J
           MOVE RESP-MOTO-EVO          TO RESP-MOTO-EVO-J
           MOVE VLR-GLASG-EVO          TO VLR-GLASG-EVO-J
           MOVE PUPILAS-EVO            TO PUPILAS-EVO-J
           MOVE MUSCULAR-EVO           TO MUSCULAR-EVO-J

      *     INITIALIZE OBSERV-EVO-J


           INSPECT LIN-4 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-4
           MOVE LIN-4 TO DATOS-PLANO3
           WRITE REG-PLANO3.


       CERRAR-EVOLUCIONES.

           IF CONT-T < 1
              INSPECT LIN-2 REPLACING ALL "*" BY CARAC-COMILLA 
              INITIALIZE LLAVE-PACI-EVO-J FECHA-EVO-J HORA-EVO-J OPER-EVO-J PESO-EVO-J TALLA-EVO-J IMC-CORP-EVO-J 
                         SUP-CORP-EVO-J TEMP-EVO-J FCARD-EVO-J FRESP-EVO-J TENS1-EVO-J TENS2-EVO-J TENS-MEDIA-EVO-J 
                         PVC-EVO-J OXIMETRIA-EVO-J FCARD-FET-EVO-J UNID-EDAD-J VLR-EDAD-J
           END-IF


           INITIALIZE CIERRE-LIN-2
           MOVE LIN-2 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           MOVE "]}" TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE


           IF CONT < 1
              INSPECT LIN-3 REPLACING ALL "*" BY CARAC-COMILLA 
              INITIALIZE LLAVE-FORM-EVO-J FECHA-FORM-EVO-J HORA-FORM-EVO-J GLUCOMETRIA-EVO-J TIPO-FORM-EVO-J COD-FORM-EVO-J OPER-FORM-EVO-J
                         CANT-FORM-EVO-J INDIC1-FORM-EVO-J INDIC2-FORM-EVO-J DOSIS-FORM-EVO-J CULTIVO-MICRO-J INST-FORM-EVO-J
                         TIPO-INCAP-FORM-J GRDO-INCAP-FORM-J PORROG-FORM-EVO-J NRO-ORD-FORM-EVO-J NRO-PED-FORM-EVO-J DESCRIP-FORM-EVO-J
           END-IF

           INITIALIZE CIERRE-LIN-3
           MOVE LIN-3 TO DATOS-PLANO2
           WRITE REG-PLANO2 END-WRITE

           MOVE "]}" TO DATOS-PLANO2
           WRITE REG-PLANO2 END-WRITE

           IF K < 1
              INSPECT LIN-4 REPLACING ALL "*" BY CARAC-COMILLA 
              INITIALIZE LLAVE-NEUR-EVO-J FECHA-NEUR-EVO-J HORA-NEUR-EVO-J OPER-NEUR-EVO-J TEMP-NEUR-EVO-J FCARD-NEUR-EVO-J FRESP-NEUR-EVO-J
                         TENS1-NEUR-EVO-J TENS2-NEUR-EVO-J PVC-NEUR-EVO-J APER-OCUL-EVO-J RESP-VERB-EVO-J RESP-MOTO-EVO-J VLR-GLASG-EVO-J
                         PUPILAS-EVO-J MUSCULAR-EVO-J
           END-IF

           INITIALIZE CIERRE-LIN-4
           MOVE LIN-4 TO DATOS-PLANO3
           WRITE REG-PLANO3 END-WRITE

           MOVE "]}" TO DATOS-PLANO3
           WRITE REG-PLANO3 END-WRITE


           CLOSE HISTORIA-CLINICA
                 EVOLUCION-HISTORIA
                 ARCHIVO-PACIENTES
                 ARCHIVO-POS
                 ARCHIVO-JSON
                 ARCHIVO-JSON2
                 ARCHIVO-JSON3.


       ABRIR-EVOLUCIONES2.

           INSPECT NOM-PLANO-W  REPLACING FIRST "SC-ARCHSIG"
                                             BY "SC-ARCHOXI"

           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT  HISTORIA-CLINICA
                       EVOLUCION-HISTORIA
                       ARCHIVO-PACIENTES.

           INITIALIZE REG-EVO CONT K J.
           MOVE "{*HC-OXI*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.

           MOVE NOM-PLANO-W     TO  NOM-PLANO2-W

           INSPECT NOM-PLANO2-W REPLACING FIRST "SC-ARCHOXI"
                                             BY "SC-ARCHLIQ"

           OPEN OUTPUT ARCHIVO-JSON2.

           MOVE "{*HC-LIQUI*:[" TO  DATOS-PLANO2
           INSPECT DATOS-PLANO2 REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO2.

       LEER-EVOLUCIONES2.
           READ EVOLUCION-HISTORIA NEXT WITH NO LOCK AT END GO TO CERRAR-EVOLUCIONES2.

           MOVE 1 TO SW-VAR 
           MOVE ID-EVO   TO ID-PACIENT-W

           PERFORM VERIFICAR-PACIENTES VARYING J FROM 1 BY 1
                                                UNTIL J > 10
           INITIALIZE J

           IF SW-VAR = 1
              GO TO LEER-EVOLUCIONES2
           END-IF 

           MOVE ID-EVO           TO COD-PACI
           READ ARCHIVO-PACIENTES WITH NO LOCK
                INVALID KEY GO TO LEER-EVOLUCIONES2
           END-READ

           MOVE LLAVE-PACI-EVO   TO LLAVE-HC

           READ HISTORIA-CLINICA WITH NO LOCK
                INVALID KEY GO TO LEER-EVOLUCIONES2
           END-READ


           IF LIQ-ADM-ORAL-EVO > 0 OR LIQ-ADM-VENA-EVO > 0
           OR LIQ-ADM-TRAN-EVO > 0 OR LIQ-ELI-ORIN-EVO > 0
           OR LIQ-ELI-DIAR-EVO > 0 OR LIQ-ELI-SOND-EVO > 0
           OR LIQ-ELI-OTR1-EVO > 0 
              PERFORM LLENAR-LIQUIDOS
           END-IF

           IF DOSIS-OXIGENO-EVO > ZEROS
              CONTINUE 
           ELSE
              IF (DOSIS-OXIGENO-EVO = ZEROS)
                 IF (OXIMETRIA-EVO > ZEROS) AND (OXIMETRIA-EVO < 50)
                    CONTINUE
                 ELSE
                    GO TO LEER-EVOLUCIONES2
                 END-IF
              END-IF
           END-IF

           ADD 1 TO CONT
           MOVE ID-EVO            TO PACI-EDIT
           MOVE PACI-EDIT         TO ID-EVO

           MOVE LLAVE-PACI-EVO    TO LLAVE-OXI-EVO-J
           MOVE FECHA-EVO         TO FECHA-OXI-EVO-J
           MOVE HORA-EVO          TO HORA-OXI-EVO-J
           MOVE OPER-ELAB-EVO     TO OPER-OXI-EVO-J
           MOVE CANT-FORMU (1)    TO HORAS-EDIT
           MOVE HORAS-EDIT        TO CANT-FORM-OXI-EVO-J
           MOVE TIPO-MET-OXI-EVO  TO TIPO-OXI-EVO-J
           MOVE DESC-MET-OXI-EVO  TO DESC-OXI-EVO-J

           IF DOSIS-OXIGENO-EVO > ZEROS
              MOVE DOSIS-OXIGENO-EVO  TO DOSIS-OXI-EVO-J
           ELSE
              IF (OXIMETRIA-EVO < 50) AND (OXIMETRIA-EVO > ZEROS)
                 MOVE OXIMETRIA-EVO    TO DOSIS-OXI-EVO-J
              ELSE
                 INITIALIZE DOSIS-OXI-EVO-J
              END-IF
           END-IF

           INSPECT LIN-5 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-5
           MOVE LIN-5 TO DATOS-PLANO
           WRITE REG-PLANO.

           GO TO LEER-EVOLUCIONES2.


       LLENAR-LIQUIDOS.

           ADD 1 TO K
           MOVE ID-EVO            TO PACI-EDIT
           MOVE PACI-EDIT         TO ID-EVO

           MOVE LLAVE-PACI-EVO    TO LLAVE-LIQ-EVO-J
           MOVE FECHA-EVO         TO FECHA-LIQ-EVO-J
           MOVE HORA-EVO          TO HORA-LIQ-EVO-J
           MOVE OPER-ELAB-EVO     TO OPER-LIQ-EVO-J
           MOVE LIQ-ADM-ORAL-EVO  TO LIQ-ADM-ORAL-EVO-J
           MOVE LIQ-ADM-VENA-EVO  TO LIQ-ADM-VENA-EVO-J
           MOVE LIQ-ADM-TRAN-EVO  TO LIQ-ADM-TRAN-EVO-J
           MOVE LIQ-ELI-ORIN-EVO  TO LIQ-ELI-ORIN-EVO-J
           MOVE LIQ-ELI-DIAR-EVO  TO LIQ-ELI-DIAR-EVO-J
           MOVE LIQ-ELI-SOND-EVO  TO LIQ-ELI-SOND-EVO-J
           MOVE LIQ-ELI-OTR1-EVO  TO LIQ-ELI-OTR1-EVO-J
           MOVE LIQ-ELI-DET-EVO   TO LIQ-ELI-DET-EVO-J

           INSPECT LIN-6 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-6
           MOVE LIN-6 TO DATOS-PLANO2
           WRITE REG-PLANO2.


       CERRAR-EVOLUCIONES2.

           IF CONT < 1
              INSPECT LIN-5 REPLACING ALL "*" BY CARAC-COMILLA 
              INITIALIZE LLAVE-OXI-EVO-J FECHA-OXI-EVO-J HORA-OXI-EVO-J OPER-OXI-EVO-J 
                         CANT-FORM-OXI-EVO-J TIPO-OXI-EVO-J DESC-OXI-EVO-J DOSIS-OXI-EVO-J
           END-IF

           INITIALIZE CIERRE-LIN-5
           MOVE LIN-5 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           MOVE "]}" TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           IF K < 1
              INSPECT LIN-6 REPLACING ALL "*" BY CARAC-COMILLA 
              INITIALIZE LLAVE-LIQ-EVO-J FECHA-LIQ-EVO-J HORA-LIQ-EVO-J OPER-LIQ-EVO-J LIQ-ADM-ORAL-EVO-J
                         LIQ-ADM-VENA-EVO-J LIQ-ADM-TRAN-EVO-J LIQ-ELI-ORIN-EVO-J LIQ-ELI-DIAR-EVO-J
                         LIQ-ELI-SOND-EVO-J LIQ-ELI-OTR1-EVO-J LIQ-ELI-DET-EVO-J
           END-IF

           INITIALIZE CIERRE-LIN-6
           MOVE LIN-6 TO DATOS-PLANO2
           WRITE REG-PLANO2 END-WRITE

           MOVE "]}" TO DATOS-PLANO2
           WRITE REG-PLANO2 END-WRITE

           CLOSE HISTORIA-CLINICA
                 EVOLUCION-HISTORIA
                 ARCHIVO-PACIENTES
                 ARCHIVO-JSON
                 ARCHIVO-JSON2.

           GO TO ABRIR-HC-DETALLADA.

       ABRIR-CITAS.

           INSPECT NOM-PLANO-W  REPLACING FIRST "SC-ARCHOXI"
                                             BY "SC-ARCHCIT"

           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT  ARCHIVO-CITAS
                       ARCHIVO-PACIENTES.
                       
           INITIALIZE REG-CIT CONT J.
           MOVE "{*CITAS*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.

       LEER-CITAS.
           READ ARCHIVO-CITAS NEXT WITH NO LOCK AT END GO TO CERRAR-CITAS.

           MOVE 1 TO SW-VAR 
           MOVE PACI-CIT   TO ID-PACIENT-W

           IF ESTADO-CIT = "*"
              GO TO LEER-CITAS
           END-IF

           PERFORM VERIFICAR-PACIENTES VARYING J FROM 1 BY 1
                                                UNTIL J > 10
           INITIALIZE J


           IF SW-VAR = 1
              GO TO LEER-CITAS
           END-IF 


           MOVE PACI-CIT  TO COD-PACI
           READ ARCHIVO-PACIENTES WITH NO LOCK
                INVALID KEY GO TO LEER-CITAS
           END-READ

           ADD 1 TO CONT

           MOVE FECHA-CIT         TO FECHA-CIT-J
           MOVE MED-CIT           TO MED-CIT-J
           MOVE PACI-CIT          TO PACI-EDIT        
           MOVE PACI-EDIT         TO PACI-CIT-J
           MOVE TIPO-FACT-CIT     TO TIPO-FACT-CIT-J

           MOVE CUPS-CIT          TO CUPS-CIT-J

           INSPECT ESTADO-CIT REPLACING ALL "*" BY "x"
           MOVE ESTADO-CIT        TO ESTADO-CIT-J


           INSPECT LIN-8 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-8
           MOVE LIN-8 TO DATOS-PLANO
           WRITE REG-PLANO.

           GO TO LEER-CITAS.


       CERRAR-CITAS.

           INITIALIZE CIERRE-LIN-8
           IF CONT < 1
              INITIALIZE CUPS-CIT-J  ESTADO-CIT-J FECHA-CIT-J MED-CIT-J PACI-CIT-J TIPO-FACT-CIT-J
              INSPECT LIN-8 REPLACING ALL "*" BY CARAC-COMILLA
           END-IF

           MOVE LIN-8 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           MOVE "]}" TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           CLOSE ARCHIVO-JSON
                 ARCHIVO-PACIENTES
                 ARCHIVO-CITAS.


       ABRIR-HC-DETALLADA.

           INSPECT NOM-PLANO-W REPLACING FIRST "SC-ARCHOXI"
                                            BY "SC-HCDETAL"

           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT  ARCHIVO-PACIENTES.

           INITIALIZE ID-EDIT J.
           MOVE "{*HC-DETALLA*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.

       ABRIR-OPC-DETALLE.


           INSPECT NOM-HCDET-W REPLACING FIRST "SC-HCEVOX1.S17 "
                                            BY "SC-HCDETA-X.S17"

           INSPECT NOM-HCDET-W REPLACING FIRST "X" BY ID-EDIT

           OPEN INPUT HISTORIA-DETALLADA.

           INITIALIZE I REG-DETHC CONT-T K.


       LEER-HC-DETALLADA.
           READ HISTORIA-DETALLADA NEXT WITH NO LOCK AT END GO TO CERRAR-HC-DETALLADA.

           MOVE 1 TO SW-VAR 
           MOVE ID-DETHC   TO ID-PACIENT-W

           PERFORM VERIFICAR-PACIENTES VARYING J FROM 1 BY 1
                                                UNTIL J > 10
           INITIALIZE J

           IF SW-VAR = 1
              GO TO LEER-HC-DETALLADA
           END-IF 

           MOVE ID-DETHC     TO COD-PACI
           READ ARCHIVO-PACIENTES WITH NO LOCK 
                INVALID KEY 
                        GO TO LEER-HC-DETALLADA
           END-READ

           ADD 1 TO K

           MOVE ID-DETHC           TO PACI-EDIT
           MOVE PACI-EDIT          TO ID-DETHC

           MOVE LLAVE-HC-DETHC     TO LLAVE-HC-DETHC-J
           MOVE COD-DETHC          TO COD-DETHC-J

           INSPECT LIN-9 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE LIN-9 TO DATOS-PLANO
           WRITE REG-PLANO.


           PERFORM LLENAR-DETALLE VARYING I FROM 1 BY 1
                                           UNTIL I > 200

           INITIALIZE I CONT-T


           MOVE "]},"    TO DATOS-PLANO
           WRITE REG-PLANO.



           GO TO LEER-HC-DETALLADA.


       LLENAR-DETALLE.
           IF RENG-DETHC (I) = SPACES OR LOW-VALUES
              CONTINUE 
           ELSE
              ADD 1 TO CONT-T
              IF CONT-T > 1
                 MOVE "," TO COMA-DETALLE-W
              ELSE
                 INITIALIZE  COMA-DETALLE-W
              END-IF
                                 
              MOVE "{*RENGXXX*:"     TO NOM-DETALLE-W
              INSPECT NOM-DETALLE-W REPLACING FIRST "XXX" BY CONT-T

              MOVE RENG-DETHC (I)   TO RENG-DETALLE-W

              INSPECT RENG-DETALLE-W REPLACING ALL '"' BY " "
              INSPECT RENG-DETALLE-W REPLACING ALL "'" BY " "
              INSPECT RENG-DETALLE-W REPLACING ALL "*" BY "X"


              INSPECT DETALLE-HC-W  REPLACING ALL "*"  BY CARAC-COMILLA

              MOVE DETALLE-HC-W     TO DATOS-PLANO
              WRITE REG-PLANO
           END-IF.



       CERRAR-HC-DETAL.
           CLOSE HISTORIA-DETALLADA.

           IF ID-EDIT = 9
              GO TO CERRAR-HC-DETALLADA
           ELSE
              ADD 1 TO ID-EDIT
           END-IF 

           GO TO ABRIR-OPC-DETALLE.

       CERRAR-HC-DETALLADA.

           IF K < 1
              INSPECT LIN-9 REPLACING ALL "*" BY CARAC-COMILLA 
              INITIALIZE LLAVE-HC-DETHC-J COD-DETHC-J
           END-IF

           MOVE LIN-9 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           MOVE "]}"  TO DATOS-PLANO
           WRITE REG-PLANO

           MOVE "]}"   TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           CLOSE ARCHIVO-JSON
                 ARCHIVO-PACIENTES.


       ABRIR-HCEVO-DETALLADA.

           INSPECT NOM-PLANO-W REPLACING FIRST "SC-HCDETAL"
                                            BY "SC-HCDTEVO"


           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT  ARCHIVO-PACIENTES.

                       
           INITIALIZE CONT-T K J.
           MOVE "{*DETEVO*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.

       ABRIR-OPC-DETALLE-EVO.

           INSPECT NOM-DETEVO-W REPLACING FIRST "SC-HCEVOX1.S17 "
                                             BY "SC-DETEVO19.S17"

           OPEN INPUT EVOLUCION-DETALLADA.
           INITIALIZE REG-DEVO.


       LEER-HCEVO-DETALLADA.
           READ EVOLUCION-DETALLADA NEXT WITH NO LOCK AT END GO TO CERRAR-HCEVO-DETALLADA.

           MOVE 1 TO SW-VAR 
           MOVE ID-DETHC   TO ID-PACIENT-W

           PERFORM VERIFICAR-PACIENTES VARYING J FROM 1 BY 1
                                                UNTIL J > 10
           INITIALIZE J

           IF SW-VAR = 1 
              GO TO LEER-HCEVO-DETALLADA
           END-IF


           MOVE ID-DEVO        TO COD-PACI
           READ ARCHIVO-PACIENTES WITH NO LOCK
                INVALID KEY GO TO LEER-HCEVO-DETALLADA
           END-READ

           ADD 1 TO K

           MOVE ID-DEVO           TO PACI-EDIT
           MOVE PACI-EDIT         TO ID-DEVO

           MOVE LLAVE-PACI-DEVO   TO LLAVE-PACI-DEVO-J
           MOVE FECHA-DEVO        TO FECHA-DEVO-J
           MOVE HORA-DEVO         TO HORA-DEVO-J
           MOVE OPER-ELAB-DEVO    TO OPER-ELAB-DEVO-J
           MOVE TIPO-DEVO         TO TIPO-DEVO-J

           INSPECT LIN-10 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE LIN-10 TO DATOS-PLANO
           WRITE REG-PLANO.
          

           PERFORM LLENAR-RENG-DEVO VARYING I FROM 1 BY 1
                                             UNTIL I > 300

           INITIALIZE I CONT-T.

           MOVE "]},"          TO DATOS-PLANO 
           WRITE REG-PLANO.

           GO TO LEER-HCEVO-DETALLADA.


       LLENAR-RENG-DEVO.

           IF RENG-DEVO (I) = SPACES OR LOW-VALUES 
              CONTINUE
           ELSE
              ADD 1   TO CONT-T
              IF CONT-T  > 1 
                 MOVE ","  TO COMA-W
              ELSE
                 INITIALIZE   COMA-W
              END-IF

              MOVE "{*RENGXXX*:"  TO NOM-REG-W
              INSPECT NOM-REG-W  REPLACING FIRST "XXX" BY   CONT-T

              MOVE RENG-DEVO (I) TO RENG-DEVO-W
              INSPECT RENG-DEVO-W REPLACING ALL LOW-VALUES BY SPACES
              INSPECT TABLA-DEVO-W REPLACING ALL "*"  BY CARAC-COMILLA

              MOVE TABLA-DEVO-W  TO DATOS-PLANO
              WRITE REG-PLANO

           END-IF.

       CERRAR-HCEVO-DETALLADA.

           IF K < 1
              INSPECT LIN-10 REPLACING ALL "*" BY CARAC-COMILLA 
              INITIALIZE LLAVE-PACI-DEVO-J FECHA-DEVO-J HORA-DEVO-J OPER-ELAB-DEVO-J TIPO-DEVO-J
           END-IF

           MOVE LIN-10 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           MOVE "]}"  TO DATOS-PLANO 
           WRITE REG-PLANO.

           MOVE "]}"   TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           CLOSE ARCHIVO-JSON
                 EVOLUCION-DETALLADA
                 ARCHIVO-PACIENTES.



010700 PAGINA-CONFIG.
010720     MOVE "datosrecibidos" TO COBW3-CNV-NAME
010730     MOVE 00               TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3
010750
010760     MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
010770     PERFORM ABRIR-HTML.
007980   
007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.


       VERIFICAR-PACIENTES.

           IF PACIENT-W  (J)  = ID-PACIENT-W
              MOVE 0  TO SW-VAR
           END-IF.

       EDITAR-NUMERO.

           INSPECT EDITADO-W REPLACING FIRST  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                          BY  "                          ".

           MOVE EDITADO-W TO TABLA-COD1
           INITIALIZE TABLA-COD2 SW-ALFA
           MOVE 10    TO LP
           PERFORM MOVER-COD VARYING LN FROM 10 BY -1
                               UNTIL LN < 1 OR LP < 1.

           INSPECT TABLA-COD2 REPLACING ALL " " BY "0".

       MOVER-COD.
           IF COD1-PAC (LN) IS NUMERIC
           OR COD1-PAC (LN) = "-"
              MOVE COD1-PAC (LN)    TO COD2-PAC(LN)
              SUBTRACT 1 FROM LP
           END-IF.

007990
008030
008040 COPY "P:\PROG\PROYECT\FUENTES\SC-WEB.CBL".
0
