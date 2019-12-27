004820*=================================================================
004830*CREACION DE ARCHIVOS JSON DE NOTAS Y ENFERMERIA 
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


	   SELECT PATOLOGIAS-CRONICAS
                  ASSIGN NOM-PAT-CRO-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-CRONIC
                  ALTERNATE RECORD KEY IS
                  DESCRIP-CRONIC WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-ESPECIALIDAD 
                  ASSIGN NOM-ESPEC-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC	
                  RECORD KEY   IS COD-ESP
                  ALTERNATE RECORD KEY IS
                  NOMBRE-ESP WITH DUPLICATES.


005030     SELECT ARCHIVO-JSON
005040            ASSIGN NOM-PLANO-W
005050            ORGANIZATION IS LINE SEQUENTIAL
005060            FILE STATUS IS OTR-STAT.


005030     SELECT ARCHIVO-JSON2
005040            ASSIGN NOM-PLANO2-W
005050            ORGANIZATION IS LINE SEQUENTIAL
005060            FILE STATUS IS OTR-STAT.

005070
005080 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
005110 COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".


005110 COPY "S:\NEWCOBOL\FUENTES\FD-RESTR.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-USUAR.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-PROFE.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-OCUPA.CBL".            
005110 COPY "S:\NEWCOBOL\FUENTES\FD-ENTID.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-CODPO.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-UNSER.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-FORLA.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-FORIM.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-FOROM.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-FORCT.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-HCVIA.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-ENFER.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-CRONI.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-MACR1.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-MACREV.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-ESPEC.CBL".
005130
005140 FD  ARCHIVO-JSON
005150     LABEL RECORD STANDARD.
005160 01  REG-PLANO.
005170     02 DATOS-PLANO     PIC X(800).

005140 FD  ARCHIVO-JSON2
005150     LABEL RECORD STANDARD.
005160 01  REG-PLANO2.
005170     02 DATOS-PLANO2     PIC X(31000).

005190 WORKING-STORAGE SECTION.
005200 COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
005210 COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".
005220 
       77  NOM-PROF-W                PIC X(70).
000982 77  NOM-OCUPA-W               PIC X(70).
       77  NOM-POS-W                 PIC X(70).
       77  NOM-ENT-W                 PIC X(70).
       77  NOM-UNSER-W               PIC X(70).

       77  NOM-FORM-LAB-W            PIC X(70).
       77  NOM-FORM-IMAG-W           PIC X(70).
       77  NOM-FORM-ORD-W            PIC X(70).
       77  NOM-FORM-CONS-W           PIC X(70). 
       77  NOM-MACRO-HIS-W           PIC X(70).
       77  NOM-VIA-ACCESO-W          PIC X(70).
       77  NOM-ENFERMEDAD-W          PIC X(70).
       77  NOM-MACRO-EVOL-W          PIC X(70).
       77  NOM-PAT-CRO-W             PIC X(70).
       77  NOM-ESPEC-W               PIC X(70).

001000 77  NOM-PLANO-W               PIC X(90).
       77  NOM-PLANO2-W              PIC X(90).

       77  NOM-USUAR-W               PIC X(70).
       77  EDITADO-W                 PIC X(10).
       77  NUMERO-EDIT               PIC ZZ.ZZ.
       77  HORAS-EDIT                PIC ZZZ.Z.


005260
001067 77  SUC-TMP                   PIC X.
005230 77  SW-VAR                    PIC 9.
005240 77  K                         PIC 99.
001252 77  MES-SAL-W                 PIC X(4).
001240 77  TAR-W                     PIC 99V9.
001250 77  VALOR-VENTA               PIC 9(12).
001251 77  IVA-TMP                   PIC 9(12).
001253 77  EXISTENCIAS-TMP           PIC S9(12)V99.
       77  NRO-EDIT                  PIC ZZZZZZZZZZZZ.
       77  CANT-EDIT                 PIC ZZZZ.Z.
       77  PROCENTAJE-EDIT           PIC ZZ.Z.
       01  CONT-T                    PIC 999.
       01  CONT                      PIC 999999.

000228 01 COD-TMP.
000229    02 SW-ALFA                 PIC 9.
000230    02 TABLA-COD1.
000231       03 COD1-PAC             PIC X OCCURS 10.
000232    02 TABLA-COD2.
000233       03 COD2-PAC             PIC X OCCURS 10.

005270 01  DATO-LLEGADA-W.
005280     02 ID-LLEGAD-W            PIC X(15).
           02 FECHA-LLEGAD-W         PIC X(8).
           02 HORA-LLEGAD-W          PIC X(6).


       01 TBLA-VIAS-W.
          02 TABLA-VIAS-MACROEVOL-W  OCCURS 6.
             03 VIAS-MACROEVOL-W     PIC XX.
             03 FILLER               PIC X VALUE "|".


       01 TBLA-FORM-MACR-W.
          02 TABLA-FORM-MACROEVOL-W  OCCURS 3.
             03 COD-FORM-W           PIC XX.
             03 FILLER               PIC X VALUE "|".


       01 TBLA-CMD-MACR-W.
          02 COMANDOS-MACROEVOL-W   OCCURS 10.
             03 PROG-MACRO-W        PIC X(10).
             03 FILLER              PIC X VALUE "|".


       01 LINEA-HTML.
          02 GRP-MACROH-W            PIC XX.
          02 FILLER                  PIC X VALUE ";".
          02 ITEM-MACROH-W           PIC X(11).
          02 FILLER                  PIC X VALUE "|".

       01 REGLON-HTML.
          02 RENG-HTML               PIC X(15) OCCURS 30.

       01 CLAVE-W.
          02 CLAVE1-W                PIC X(6).
          02 CLAVE2-W                PIC XX.


001812 01 LIN-1.  
001820    02 FILLER                  PIC X(8)   VALUE "{*USER*:".
001830    02 FILLER                  PIC X      VALUE "*".
001840    02 COD-REST-J              PIC X(4).
001850    02 FILLER                  PIC XX     VALUE "*,".
001860    02 FILLER                  PIC X(5)   VALUE "*ID*:".
001870    02 FILLER                  PIC X      VALUE "*".
001880    02 ID-REST-J               PIC X(10).
001890    02 FILLER                  PIC XX     VALUE "*,".    
001900    02 FILLER                  PIC X(9)   VALUE "*NOMBRE*:".
001910    02 FILLER                  PIC X      VALUE "*".
001920    02 NOMBRE-REST-J           PIC X(30).
001930    02 FILLER                  PIC XX     VALUE "*,".
001940    02 FILLER                  PIC X(6)   VALUE "*PSW*:".
001950    02 FILLER                  PIC X      VALUE "*".
001960    02 CLAVE-REST-J            PIC X(8).
001970    02 FILLER                  PIC XX     VALUE "*}".    
001980    02 CIERRE-LIN-1            PIC X.
001990 

001812 01 LIN-2.
001820    02 FILLER                  PIC X(7)   VALUE "{*COD*:".
001830    02 FILLER                  PIC X      VALUE "*".
001840    02 COD-PROF-J              PIC X(10).
001850    02 FILLER                  PIC XX     VALUE "*,".
001860    02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
001870    02 FILLER                  PIC X      VALUE "*".
001880    02 DESCRIP-PROF-J          PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*COD-ATIEND*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-ATIEN-PROF-J        PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*DESCRIP-ATIEND*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ATIENDE-PROF-J          PIC X(10).
001970    02 FILLER                  PIC XX     VALUE "*}".    
001980    02 CIERRE-LIN-2            PIC X.

001812 01 LIN-3.
001820    02 FILLER                  PIC X(7)   VALUE "{*COD*:".
001830    02 FILLER                  PIC X      VALUE "*".
001840    02 COD-OCU-J               PIC X(4).
001850    02 FILLER                  PIC XX     VALUE "*,".
001860    02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
001870    02 FILLER                  PIC X      VALUE "*".
001880    02 NOMBRE-OCU-J            PIC X(120).
001970    02 FILLER                  PIC XX     VALUE "*}".    
001980    02 CIERRE-LIN-3            PIC X.


001812 01 LIN-4.
001820    02 FILLER                  PIC X(7)   VALUE "{*COD*:".
001830    02 FILLER                  PIC X      VALUE "*".
001840    02 COD-ENT-J               PIC X(6).
001850    02 FILLER                  PIC XX     VALUE "*,".
001860    02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
001870    02 FILLER                  PIC X      VALUE "*".
001880    02 NOMBRE-ENT-J            PIC X(120).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*NIT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NIT-ENT-J               PIC X(10).
001970    02 FILLER                  PIC XX     VALUE "*}".    
001980    02 CIERRE-LIN-4            PIC X.


       01 LIN-5.
          02 FILLER                  PIC X(7)   VALUE "{*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-POS-J               PIC X(13).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-POS-J           PIC X(57).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*ORDEN*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ORD-DOSIS-POS-J         PIC X(12).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*CANT-DOSIS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CANT-DOSIS-POS-J        PIC X(9).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*UNID-DOSIS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UNID-DOSIS-POS-J        PIC X(13).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*CADA-DOSIS*:".
          02 FILLER                  PIC XX     VALUE "*,".
          02 CADA-DOSIS-POS-J        PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*CANT-FREC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CANT-FREC-POS-J         PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*UNID-FREC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UNID-FREC-POS-J         PIC X(7).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*VIA-DOSIS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VIA-DOSIS-POS-J         PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*CANT-DOSIF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CANT-DOSIF-POS-J        PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*TIPO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-POS-J              PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(19)  VALUE "*CANT-FREC-DOSISF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CANT-FREC-DOSISF-POS-J  PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*FREC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FREC-POS-J              PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*VIA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VIA-DOSISF-POS-J        PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*UNSERV*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UNSERV-POS-J            PIC XX.
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-LIN-5            PIC X.


       01 LIN-6.
          02 FILLER                  PIC X(7)   VALUE "{*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-SER-J               PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-SER-J           PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*UND-EDAD-MIN*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UND-EDAD-MIN-SER-J      PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*VLR-EDAD-MIN*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VLR-EDAD-MIN-SER-J      PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*UND-EDAD-MAX*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UND-EDAD-MAX-SER-J      PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*VLR-EDAD-MAX*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VLR-EDAD-MAX-SER-J      PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*COSTO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 C-COSTO-SER-J           PIC XX.
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-LIN-6            PIC X.


      * formulas laboratorio


001812 01 LIN-7.
001820    02 FILLER                  PIC X(7)   VALUE "{*COD*:".
001830    02 FILLER                  PIC X      VALUE "*".
001840    02 LLAVE-LAB-J             PIC X(7).
001850    02 FILLER                  PIC XX     VALUE "*,".
001860    02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
001870    02 FILLER                  PIC X      VALUE "*".
001880    02 DESCRIP-LAB-J           PIC X(80).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*OBSERV*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OBSERV-LAB-J            PIC X(80).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*OPER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OPER-LAB-J              PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FECHA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-ACT-LAB-J         PIC X(6).
001970    02 FILLER                  PIC XX     VALUE "*}".    
001980    02 CIERRE-LIN-7            PIC X.


      * formulas imagen


001812 01 LIN-8.
001820    02 FILLER                  PIC X(7)   VALUE "{*COD*:".
001830    02 FILLER                  PIC X      VALUE "*".
001840    02 LLAVE-IMAG-J            PIC X(7).
001850    02 FILLER                  PIC XX     VALUE "*,".
001860    02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
001870    02 FILLER                  PIC X      VALUE "*".
001880    02 DESCRIP-IMAG-J          PIC X(80).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*OBSERV*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OBSERV-IMAG-J           PIC X(80).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*OPER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OPER-IMAG-J             PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FECHA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-ACT-IMAG-J        PIC X(6).
001970    02 FILLER                  PIC XX     VALUE "*}".    
001980    02 CIERRE-LIN-8            PIC X.


      * formulas ord...


001812 01 LIN-9.
001820    02 FILLER                  PIC X(7)   VALUE "{*COD*:".
001830    02 FILLER                  PIC X      VALUE "*".
001840    02 LLAVE-ORD-J             PIC X(7).
001850    02 FILLER                  PIC XX     VALUE "*,".
001860    02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
001870    02 FILLER                  PIC X      VALUE "*".
001880    02 DESCRIP-ORD-J           PIC X(80).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*OBSERV*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OBSERV-ORD-J            PIC X(80).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*OPER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OPER-ORD-J              PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FECHA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-ACT-ORD-J         PIC X(6).
001970    02 FILLER                  PIC XX     VALUE "*}".    
001980    02 CIERRE-LIN-9            PIC X.


      * formulas cons...


001812 01 LIN-10.
001820    02 FILLER                  PIC X(7)   VALUE "{*COD*:".
001830    02 FILLER                  PIC X      VALUE "*".
001840    02 LLAVE-CONS-J            PIC X(7).
001850    02 FILLER                  PIC XX     VALUE "*,".
001860    02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
001870    02 FILLER                  PIC X      VALUE "*".
001880    02 DESCRIP-CONS-J          PIC X(80).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*OBSERV*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OBSERV-CONS-J           PIC X(80).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*OPER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OPER-CONS-J             PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FECHA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-ACT-CONS-J        PIC X(6).
001970    02 FILLER                  PIC XX     VALUE "*}".    
001980    02 CIERRE-LIN-10           PIC X.


      * macro historias...


001812 01 LIN-11.
001820    02 FILLER                  PIC X(7)   VALUE "{*COD*:".
001830    02 FILLER                  PIC X      VALUE "*".
001840    02 LLAVE-MACROH-J          PIC X(3).
001850    02 FILLER                  PIC XX     VALUE "*,".
001860    02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
001870    02 FILLER                  PIC X      VALUE "*".
001880    02 NOMBRE-MACROH-J         PIC X(40).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*ARTICU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ART-MACROH-J            PIC X(450).
001970    02 FILLER                  PIC XX     VALUE "*}".    
001980    02 CIERRE-LIN-11           PIC X.


      * vias acceso...

001812 01 LIN-12.
001820    02 FILLER                  PIC X(7)   VALUE "{*COD*:".
001830    02 FILLER                  PIC X      VALUE "*".
001840    02 COD-VIA-J               PIC X(2).
001850    02 FILLER                  PIC XX     VALUE "*,".
001860    02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
001870    02 FILLER                  PIC X      VALUE "*".
001880    02 NOMBRE-VIA-J            PIC X(30).
001970    02 FILLER                  PIC XX     VALUE "*}".    
001980    02 CIERRE-LIN-12           PIC X.


      * enfermedades...

001812 01 LIN-13.
001820    02 FILLER                  PIC X(8)   VALUE "{*TIPO*:".
001830    02 FILLER                  PIC X      VALUE "*".
001840    02 TIPO-ENF-J              PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-ENF-J               PIC X(4).
001850    02 FILLER                  PIC XX     VALUE "*,".
001860    02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
001870    02 FILLER                  PIC X      VALUE "*".
001880    02 NOMBRE-ENF-J            PIC X(80).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*SEX*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SEXO-ENF-J              PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*EDAD-MIN*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EDAD-MIN-ENF-J          PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*EDAD-MAX*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EDAD-MAX-ENF-J          PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*UNID-EDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UNID-EDAD-ENF-J         PIC X.
001970    02 FILLER                  PIC XX     VALUE "*}".    
001980    02 CIERRE-LIN-13           PIC X.


      * patologia cronicas...


001812 01 LIN-14.
001820    02 FILLER                  PIC X(7)   VALUE "{*COD*:".
001830    02 FILLER                  PIC X      VALUE "*".
001840    02 LLAVE-CRONIC-J          PIC X(3).
001850    02 FILLER                  PIC XX     VALUE "*,".
001860    02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
001870    02 FILLER                  PIC X      VALUE "*".
001880    02 DESCRIP-CRONIC-J        PIC X(80).
001970    02 FILLER                  PIC XX     VALUE "*}".    
001980    02 CIERRE-LIN-14           PIC X.


      * macro evoluciones 

001812 01 LIN-15.
001820    02 FILLER                  PIC X(7)   VALUE "{*COD*:".
001830    02 FILLER                  PIC X      VALUE "*".
001840    02 LLAVE-MACROEVOL-J       PIC X(7).
001850    02 FILLER                  PIC XX     VALUE "*,".
001860    02 FILLER                  PIC X(10)  VALUE "*DETALLE*:".
001870    02 FILLER                  PIC X      VALUE "*".
001880    02 DETALLE-MACROEVOL-J     PIC X(40).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*TBLA-MACR*:".
          02 FILLER                  PIC X      VALUE "*".
          02 RENG-MACROEVOL-J        PIC X(28500).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*TBLA-VIAS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TABLA-VIAS-MACROEVOL-J  PIC X(18).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*TBLA-FORM*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TABLA-FORM-MACROEVOL-J  PIC X(9).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*TBLA-PROG*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COMANDOS-MACROEVOL-J    PIC X(160).
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-LIN-15           PIC X.

       01 LIN-16.
          02 FILLER                  PIC X(7)   VALUE "{*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-ESP-J               PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOMBRE-ESP-J            PIC X(70).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*COSTO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COSTO-ESP-J             PIC XX.
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-LIN-16           PIC X.


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
002650        MOVE "HC-NTAS02"      TO MENSAJE3-HTML
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
006440        MOVE "HC-NTAS002"     TO MENSAJE3-HTML
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
006560        MOVE "HC-NTAS002"     TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS002"     TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS002"     TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS002"     TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS002"     TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS002"     TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS002"     TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS002"     TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS002"     TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS002"     TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS002"     TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS002"     TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS002"     TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS002"     TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-MACRO-EVOL.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-MACRO-EVOL-W TO MENSAJE2-HTML
006680        MOVE "HC-NTAS002"     TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS002"     TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ESPECIALIDAD.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-ESPEC-W      TO MENSAJE2-HTML
006680        MOVE "HC-NTAS002"     TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS002"     TO MENSAJE3-HTML
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
006680        MOVE "HC-NTAS002"     TO MENSAJE3-HTML
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

001855     MOVE DATO-LLEGADA-W    TO LLAVE-SESION-W.
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
           INITIALIZE NOM-USU-W NOM-USUAR-W NOM-REST-W NOM-PLANO-W NOM-PROF-W NOM-OCUPA-W 
                      NOM-ENT-W NOM-POS-W NOM-UNSER-W NOM-FORM-LAB-W NOM-FORM-IMAG-W NOM-FORM-ORD-W 
                      NOM-FORM-CONS-W NOM-MACRO-HIS-W NOM-VIA-ACCESO-W NOM-ENFERMEDAD-W  NOM-PAT-CRO-W NOM-ESPEC-W.


           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\".
           MOVE "\\"  TO NOM-REST-W  NOM-FORM-LAB-W NOM-USUAR-W NOM-VIA-ACCESO-W
007150     INSPECT NOM-REST-W  REPLACING FIRST "                    "
007160                                      BY IP-DATOS-USUNET
007170     INSPECT NOM-REST-W  REPLACING FIRST " "
007180                                      BY "\"
007190     INSPECT NOM-REST-W  REPLACING FIRST "                          "
007200                                      BY "NEWCOBOL\DATOS\ARCHREST.SC".

           MOVE NOM-REST-W     TO NOM-OCUPA-W NOM-ENT-W NOM-POS-W NOM-ENFERMEDAD-W NOM-PAT-CRO-W NOM-ESPEC-W.

           INSPECT NOM-OCUPA-W REPLACING FIRST   "ARCHREST.SC  "
                                            BY   "SC-OCUPAC.DAT".

           INSPECT NOM-ENT-W   REPLACING FIRST   "ARCHREST.SC"
                                            BY   "ARCHENT.DAT".

           INSPECT NOM-POS-W   REPLACING FIRST   "ARCHREST.SC "
                                            BY   "SC-FARMA.DAT".


           INSPECT NOM-ENFERMEDAD-W REPLACING FIRST "ARCHREST.SC"
                                                 BY "ARCHENF.DAT".

           INSPECT NOM-PAT-CRO-W  REPLACING FIRST   "ARCHREST.SC "
                                                 BY "SC-CRONI.DAT".

           INSPECT NOM-ESPEC-W  REPLACING FIRST  "ARCHREST.SC"
                                              BY "ARCHESP.DAT".


           INSPECT NOM-FORM-LAB-W REPLACING FIRST  "                    "
                                               BY  IP-DATOS-USUNET

           INSPECT NOM-FORM-LAB-W REPLACING FIRST  " "
                                               BY  "\"

           INSPECT NOM-FORM-LAB-W REPLACING FIRST  "                  "
                                               BY  "NEWCOBOL\HC\DATOS\"

           INSPECT NOM-FORM-LAB-W REPLACING FIRST  "            "
                                               BY  "SC-LABOR.DAT".


           MOVE  NOM-FORM-LAB-W TO NOM-FORM-IMAG-W NOM-FORM-ORD-W NOM-FORM-CONS-W NOM-MACRO-HIS-W NOM-MACRO-EVOL-W. 


           INSPECT NOM-FORM-IMAG-W REPLACING FIRST  "SC-LABOR.DAT"
                                                 BY "SC-IMAGE.DAT".

           INSPECT NOM-FORM-ORD-W  REPLACING FIRST  "SC-LABOR.DAT   "
                                                 BY "SC-ORDMEDIC.DAT".

           INSPECT NOM-FORM-CONS-W REPLACING FIRST  "SC-LABOR.DAT"
                                                 BY "SC-CONST.DAT".

           INSPECT NOM-MACRO-HIS-W REPLACING FIRST  "SC-LABOR.DAT"
                                                 BY "SC-MACDR.DAT".

           INSPECT NOM-MACRO-EVOL-W REPLACING FIRST "SC-LABOR.DAT   "
                                                 BY "SC-MACRO-EV.DAT".


           INSPECT NOM-VIA-ACCESO-W REPLACING FIRST "                    "
                                                 BY  IP-DATOS-USUNET

           INSPECT NOM-VIA-ACCESO-W REPLACING FIRST " "
                                                 BY "\"

           INSPECT NOM-VIA-ACCESO-W REPLACING FIRST "                            "
                                                 BY "NEWCOBOL\DATOS-H\SC-VIAS.DAT".

           INSPECT NOM-USUAR-W REPLACING FIRST  "                    "
                                            BY  IP-DATOS-USUNET
           INSPECT NOM-USUAR-W REPLACING FIRST  " "
                                            BY  "\"
           INSPECT NOM-USUAR-W REPLACING FIRST  "                              "
007183                                      BY  DIRECTORIO-USUNET
           INSPECT NOM-USUAR-W REPLACING FIRST  "                "
                                            BY  "\CONTROL\ARCHUSU".

           MOVE NOM-USUAR-W  TO NOM-PROF-W NOM-UNSER-W
           

           INSPECT NOM-PROF-W  REPLACING FIRST  "ARCHUSU     "
                                            BY  "ARCHPROF.DAT".

           INSPECT NOM-UNSER-W REPLACING FIRST  "ARCHUSU     "
                                            BY  "SC-UNSER.DAT".

           MOVE "C:\PROSOFT\TEMP\SC-ARCHRES-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON" TO  NOM-PLANO-W
           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                             BY DATO-LLEGADA-W.  

        ABRIR-USUARIOS.
           OPEN INPUT ARCHIVO-USUARIOS
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.

       ABRIR-RESTRICCION.     
007580     OPEN INPUT  ARCHIVO-RESTRICCION.
007590     OPEN OUTPUT ARCHIVO-JSON.
007600
007610     INITIALIZE REG-REST.
007620     MOVE "{*Usuarios*:[" TO  DATOS-PLANO
007630     INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
007640     WRITE REG-PLANO.
007650     MOVE 0 TO SW-FIN.
007660
007670 LEER-RESTRICCION.
007680     IF SW-FIN = 0
007690        MOVE 1 TO SW-FIN
007700     ELSE
007710         MOVE LIN-1 TO DATOS-PLANO
007720        WRITE REG-PLANO
007730     END-IF 
007740
007750     READ ARCHIVO-RESTRICCION NEXT WITH NO LOCK AT END GO TO CERRAR-RESTRICCION.
007760
           IF NOM1-REST = "*"
              MOVE 0 TO SW-FIN
              GO TO LEER-RESTRICCION
           END-IF

007780     IF COD-REST = SPACES OR LOW-VALUES
007790        MOVE 0 TO SW-FIN
007800     ELSE
007810        IF COD-REST = COD-REST-J
007820           MOVE 0 TO SW-FIN
007830        ELSE
007840           INITIALIZE LIN-1
007850           MOVE COD-REST    TO COD-REST-J
007860           MOVE ID-REST     TO ID-REST-J
007870           MOVE NOMBRE-REST TO NOMBRE-REST-J
007880           IF CLAVE-REST = SPACES
007890              INITIALIZE CLAVE-REST-J
007900           ELSE
007910              INSPECT CLAVE-REST CONVERTING CARAC-SENC TO "0A1EIO23U45e6i8o79u"
                    INSPECT CLAVE2-REST CONVERTING CARAC-SENC TO "0A1EIO23U45e6i8o79u"

                    MOVE CLAVE-REST      TO CLAVE1-W
                    MOVE CLAVE2-REST     TO CLAVE2-W

                    MOVE CLAVE-W         TO CLAVE-REST-J
007930           END-IF          
007940           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
007950           MOVE "," TO CIERRE-LIN-1
007960        END-IF
007970     END-IF
           GO TO LEER-RESTRICCION.

008010 CERRAR-RESTRICCION.
008020     INITIALIZE CIERRE-LIN-1
008030     MOVE LIN-1 TO DATOS-PLANO
008040     WRITE REG-PLANO END-WRITE
008050
008060     MOVE "]}" TO DATOS-PLANO
008070     WRITE REG-PLANO END-WRITE
008080          
008090     CLOSE ARCHIVO-RESTRICCION
008100           ARCHIVO-JSON.

       ABRIR-PROFESIONALES.
           INSPECT NOM-PLANO-W  REPLACING FIRST "SC-ARCHRES"
                                             BY "SC-ARCHPRO".

           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT ARCHIVO-PROFESIONALES.

           INITIALIZE REG-PROF.
           MOVE "{*Profesionales*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.

       LEER-PROFESIONALES.
           READ ARCHIVO-PROFESIONALES NEXT WITH NO LOCK AT END GO TO CERRAR-PROFESIONALES.

           MOVE COD-PROF         TO COD-PROF-J
           MOVE DESCRIP-PROF     TO DESCRIP-PROF-J
           MOVE ATIENDE-PROF     TO COD-ATIEN-PROF-J

           EVALUATE ATIENDE-PROF
             WHEN "1"  MOVE "MD ESPECIA" TO ATIENDE-PROF-J
             WHEN "2"  MOVE "MD GENERAL" TO ATIENDE-PROF-J
             WHEN "3"  MOVE "ENFERMERA " TO ATIENDE-PROF-J
             WHEN "4"  MOVE "AUX. ENFER" TO ATIENDE-PROF-J
             WHEN "5"  MOVE "TERAPEUTAS" TO ATIENDE-PROF-J
             WHEN "6"  MOVE "JEFE P Y P" TO ATIENDE-PROF-J
             WHEN "7"  MOVE "SICOLOGIA " TO ATIENDE-PROF-J
             WHEN "8"  MOVE "NUTRICION." TO ATIENDE-PROF-J
             WHEN "A"  MOVE "ODONTOLOGO" TO ATIENDE-PROF-J
             WHEN "B"  MOVE "AUDITOR MD" TO ATIENDE-PROF-J
             WHEN "I"  MOVE "INSTRUMENT" TO ATIENDE-PROF-J
             WHEN "O"  MOVE "OPTOMETRA " TO ATIENDE-PROF-J
             WHEN "T"  MOVE "T. SOCIAL " TO ATIENDE-PROF-J
             WHEN OTHER
                  MOVE "NO ATIENDE" TO ATIENDE-PROF-J
           END-EVALUATE.


           INSPECT LIN-2 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-2
           MOVE LIN-2 TO DATOS-PLANO
           WRITE REG-PLANO

           GO TO LEER-PROFESIONALES.

       CERRAR-PROFESIONALES.

           INITIALIZE CIERRE-LIN-2
008030     MOVE LIN-2 TO DATOS-PLANO
008040     WRITE REG-PLANO END-WRITE
008050
008060     MOVE "]}" TO DATOS-PLANO
008070     WRITE REG-PLANO END-WRITE
008080          
008090     CLOSE ARCHIVO-PROFESIONALES
008100           ARCHIVO-JSON.

       ABRIR-OCUPACIONES.
           INSPECT NOM-PLANO-W REPLACING FIRST "SC-ARCHPRO"
                                          BY "SC-ARCHOCU"

           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT ARCHIVO-OCUPACIONES.

           INITIALIZE REG-OCU.
           MOVE "{*Ocupaciones*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.

       LEER-OCUPACIONES.
           READ ARCHIVO-OCUPACIONES NEXT WITH NO LOCK AT END GO TO CERRAR-OCUPACIONES.

           MOVE COD-OCU        TO COD-OCU-J
           MOVE NOMBRE-OCU     TO NOMBRE-OCU-J

           INSPECT LIN-3 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-3
           MOVE LIN-3 TO DATOS-PLANO
           WRITE REG-PLANO

           GO TO LEER-OCUPACIONES.

       CERRAR-OCUPACIONES.

           INITIALIZE CIERRE-LIN-3
008030     MOVE LIN-3 TO DATOS-PLANO
008040     WRITE REG-PLANO END-WRITE
008050
008060     MOVE "]}" TO DATOS-PLANO
008070     WRITE REG-PLANO END-WRITE
008080          
008090     CLOSE ARCHIVO-OCUPACIONES
008100           ARCHIVO-JSON.

       ABRIR-ENTIDADES.
           INSPECT NOM-PLANO-W  REPLACING FIRST "SC-ARCHOCU"
                                           BY "SC-ARCHENT"

           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT ARCHIVO-ENTIDADES

           INITIALIZE REG-ENT.
           MOVE "{*Entidades*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.

       LEER-ENTIDADES.
           READ ARCHIVO-ENTIDADES NEXT WITH NO LOCK AT END GO TO CERRAR-ENTIDADES.

           MOVE COD-ENT            TO COD-ENT-J
           MOVE NOMBRE-ENT         TO NOMBRE-ENT-J
           INSPECT NOMBRE-ENT-J REPLACING ALL "'" BY " "
           MOVE NIT-ENT            TO NIT-ENT-J

           INSPECT LIN-4 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-4
           MOVE LIN-4 TO DATOS-PLANO
           WRITE REG-PLANO
           
           GO TO LEER-ENTIDADES.


       CERRAR-ENTIDADES.
           INITIALIZE CIERRE-LIN-4
008030     MOVE LIN-4 TO DATOS-PLANO
008040     WRITE REG-PLANO END-WRITE
008050
008060     MOVE "]}" TO DATOS-PLANO
008070     WRITE REG-PLANO END-WRITE
008080          
008090     CLOSE ARCHIVO-ENTIDADES
008100           ARCHIVO-JSON.

       ABRIR-FARMA.
           INSPECT NOM-PLANO-W REPLACING FIRST "SC-ARCHENT"
                                            BY "SC-ARCHFAR".
           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT ARCHIVO-POS.

           INITIALIZE REG-POS.
           MOVE "{*Medicamentos*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.


       LEER-FARMA.
           READ ARCHIVO-POS NEXT WITH NO LOCK AT END GO TO CERRAR-FARMA.

           IF INICIAL-POS = "*"
              GO TO LEER-FARMA
           END-IF

           MOVE LLAVE-POS               TO COD-POS-J
           MOVE NOMBRE-POS              TO DESCRIP-POS-J

           MOVE ORDEN-DOSIS-POS         TO ORD-DOSIS-POS-J
           INSPECT ORD-DOSIS-POS-J  REPLACING ALL "'"  BY " ".

           MOVE CANT-DOSIS-POS          TO CANT-DOSIS-POS-J
           MOVE UNID-DOSIS-POS          TO UNID-DOSIS-POS-J
           MOVE CADA-DOSIS-POS          TO CADA-DOSIS-POS-J
           MOVE CANT-FREC-POS           TO CANT-FREC-POS-J
           MOVE UNID-FREC-POS           TO UNID-FREC-POS-J
           MOVE VIA-DOSIS-POS           TO VIA-DOSIS-POS-J



          
           MOVE CANT-DOSIF-POS          TO CANT-DOSIF-POS-J
           MOVE TIPO-DOSIF-POS          TO TIPO-POS-J
           MOVE CANT-FREC-DOSIF-POS     TO CANT-FREC-DOSISF-POS-J
           MOVE FREC-DOSIF-POS          TO FREC-POS-J
           MOVE VIA-DOSIF-POS           TO VIA-DOSISF-POS-J
           MOVE UNSERV-POS              TO UNSERV-POS-J

           INSPECT DESCRIP-POS-J REPLACING ALL "*" BY "X"

           INSPECT LIN-5 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-5
           MOVE LIN-5 TO DATOS-PLANO
           WRITE REG-PLANO

           GO TO LEER-FARMA.


       CERRAR-FARMA.

           INITIALIZE CIERRE-LIN-5
008030     MOVE LIN-5 TO DATOS-PLANO
008040     WRITE REG-PLANO END-WRITE
008050
008060     MOVE "]}" TO DATOS-PLANO
008070     WRITE REG-PLANO END-WRITE
008080          
008090     CLOSE ARCHIVO-POS
008100           ARCHIVO-JSON.


       ABRIR-UNID-SERV.

           INSPECT NOM-PLANO-W REPLACING FIRST "SC-ARCHFAR"
                                            BY "SC-ARCHSER"
           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT ARCHIVO-UNID-SERV.

           INITIALIZE REG-UNID-SERV.
           MOVE "{*UnidServicio*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.

       LEER-UNID-SERV.
           READ ARCHIVO-UNID-SERV NEXT WITH NO LOCK AT END GO TO CERRAR-UNID-SERV.


           MOVE CODIGO-UNID-SERV          TO COD-SER-J
           MOVE DESCRIP-UNID-SERV         TO DESCRIP-SER-J
           MOVE UND-EDAD-MIN-SERV         TO UND-EDAD-MIN-SER-J
           MOVE VLR-EDAD-MIN-SERV         TO VLR-EDAD-MIN-SER-J
           MOVE UND-EDAD-MAX-SERV         TO UND-EDAD-MAX-SER-J
           MOVE VLR-EDAD-MAX-SERV         TO VLR-EDAD-MAX-SER-J
           MOVE C-COSTO-SERV              TO C-COSTO-SER-J

           INSPECT LIN-6 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-6
           MOVE LIN-6 TO DATOS-PLANO
           WRITE REG-PLANO

           GO TO LEER-UNID-SERV.

       CERRAR-UNID-SERV.
           INITIALIZE CIERRE-LIN-6
008030     MOVE LIN-6 TO DATOS-PLANO
008040     WRITE REG-PLANO END-WRITE
008050
008060     MOVE "]}" TO DATOS-PLANO
008070     WRITE REG-PLANO END-WRITE
008080          
008090     CLOSE ARCHIVO-UNID-SERV
008100           ARCHIVO-JSON.


       ABRIR-FORMU-LAB.

           INSPECT NOM-PLANO-W  REPLACING FIRST "SC-ARCHSER"
                                             BY "SC-ARCHLAB"

           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT  ARCHIVO-FORMULA-LAB.

           INITIALIZE REG-LAB.
           MOVE "{*FORLAB*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.

       LEER-FORMU-LAB.
           READ ARCHIVO-FORMULA-LAB NEXT WITH NO LOCK AT END GO TO CERRAR-FORMULA-LAB.

           MOVE LLAVE-LAB           TO LLAVE-LAB-J

           INSPECT DESCRIP-LAB REPLACING ALL "*" BY "x"
           MOVE DESCRIP-LAB         TO DESCRIP-LAB-J

           INSPECT DESCRIP-LAB-J REPLACING ALL "'" BY " "

           MOVE OBSERV-LAB          TO OBSERV-LAB-J
           MOVE OPER-LAB            TO OPER-LAB-J
           MOVE FECHA-ACT-LAB       TO FECHA-ACT-LAB-J


           INSPECT LIN-7  REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-7 
           MOVE LIN-7  TO DATOS-PLANO
           WRITE REG-PLANO.


           GO TO LEER-FORMU-LAB.

       CERRAR-FORMULA-LAB.

           INITIALIZE CIERRE-LIN-7 
           MOVE LIN-7 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           MOVE "]}" TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           CLOSE ARCHIVO-JSON
                 ARCHIVO-FORMULA-LAB.


       ABRIR-FORMULA-IMAG.

           INSPECT NOM-PLANO-W  REPLACING FIRST "SC-ARCHLAB"
                                             BY "SC-ARCHIMA"

           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT  ARCHIVO-FORMULA-IMAG.

           INITIALIZE REG-IMAG.
           MOVE "{*FORIMA*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.

       LEER-FORMU-IMAG.
           READ ARCHIVO-FORMULA-IMAG NEXT WITH NO LOCK AT END GO TO CERRAR-FORMU-IMAG.

           MOVE LLAVE-IMAG        TO LLAVE-IMAG-J

           INSPECT DESCRIP-IMAG REPLACING ALL "*" BY "x"
           MOVE DESCRIP-IMAG      TO DESCRIP-IMAG-J

           MOVE OBSERV-IMAG       TO OBSERV-IMAG-J
           MOVE OPER-IMAG         TO OPER-IMAG-J
           MOVE FECHA-ACT-IMAG    TO FECHA-ACT-IMAG-J



           INSPECT LIN-8 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-8 
           MOVE LIN-8 TO DATOS-PLANO
           WRITE REG-PLANO.

           GO TO LEER-FORMU-IMAG.

       CERRAR-FORMU-IMAG.

           INITIALIZE CIERRE-LIN-8
           MOVE LIN-8 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           MOVE "]}" TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           CLOSE ARCHIVO-JSON
                 ARCHIVO-FORMULA-IMAG.


       ABRIR-FORMU-ORD.

           INSPECT NOM-PLANO-W  REPLACING FIRST "SC-ARCHIMA"
                                             BY "SC-ARCHORD"

           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT  ARCHIVO-FORMULA-ORD.
                       
           INITIALIZE REG-ORD CONT.
           MOVE "{*FORMOM*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.

       LEER-FORMU-ORD.
           READ ARCHIVO-FORMULA-ORD NEXT WITH NO LOCK AT END GO TO CERRAR-FORMU-ORD.

           IF GRUPO-ORD = "**"
              GO TO LEER-FORMU-ORD
           END-IF

           ADD 1 TO CONT
           MOVE LLAVE-ORD          TO LLAVE-ORD-J

           INSPECT DESCRIP-ORD REPLACING ALL "*" BY "x"
           MOVE DESCRIP-ORD        TO DESCRIP-ORD-J

           MOVE OBSERV-ORD (1)     TO OBSERV-ORD-J
           MOVE OPER-ORD           TO OPER-ORD-J
           MOVE FECHA-ACT-ORD      TO FECHA-ACT-ORD-J

           INSPECT LIN-9 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-9
           MOVE LIN-9 TO DATOS-PLANO
           WRITE REG-PLANO.


           GO TO LEER-FORMU-ORD.

       CERRAR-FORMU-ORD.

           INITIALIZE CIERRE-LIN-9 
           IF CONT < 1 
              INITIALIZE LLAVE-ORD-J DESCRIP-ORD-J OBSERV-ORD-J OPER-ORD-J FECHA-ACT-ORD-J
              INSPECT LIN-9 REPLACING ALL "*" BY CARAC-COMILLA 
           END-IF

           MOVE LIN-9 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           MOVE "]}" TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           CLOSE ARCHIVO-JSON
                 ARCHIVO-FORMULA-ORD.


       ABRIR-FORMULA-CONS.

           INSPECT NOM-PLANO-W  REPLACING FIRST "SC-ARCHORD"
                                             BY "SC-ARCHCON"

           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT  ARCHIVO-FORMULA-CONS.
                       
           INITIALIZE REG-CONS.
           MOVE "{*FORCON*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.


       LEER-FORMULA-CONS.
           READ ARCHIVO-FORMULA-CONS NEXT WITH NO LOCK AT END GO TO CERRAR-FORM-CONS.

           MOVE LLAVE-CONS          TO LLAVE-CONS-J

           INSPECT DESCRIP-CONS REPLACING ALL "*" BY "x"
           MOVE DESCRIP-CONS        TO DESCRIP-CONS-J

           MOVE OBSERV-CONS         TO OBSERV-CONS-J
           MOVE OPER-CONS           TO OPER-CONS-J
           MOVE FECHA-ACT-CONS      TO FECHA-ACT-CONS-J

           INSPECT LIN-10 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-10
           MOVE LIN-10 TO DATOS-PLANO
           WRITE REG-PLANO.

           GO TO LEER-FORMULA-CONS.


       CERRAR-FORM-CONS.

           INITIALIZE CIERRE-LIN-10
           MOVE LIN-10 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           MOVE "]}" TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           CLOSE ARCHIVO-JSON
                 ARCHIVO-FORMULA-CONS.



       ABRIR-MARCO-HIS.

           INSPECT NOM-PLANO-W  REPLACING FIRST "SC-ARCHCON"
                                             BY "SC-MACRHIS"

           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT  ARCHIVO-MACRO-HIS.
                       
           INITIALIZE REG-MACROH I CONT.
           MOVE "{*MACHIS*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.

       LEER-MACRO-HIS.

           READ ARCHIVO-MACRO-HIS NEXT WITH NO LOCK AT END GO TO CERRAR-MACRO-HIS.


           ADD 1 TO CONT
           MOVE LLAVE-MACROH     TO LLAVE-MACROH-J
           MOVE NOMBRE-MACROH    TO NOMBRE-MACROH-J
           

           PERFORM LLENAR-RENGLON VARYING I FROM  1 BY 1
                                            UNTIL I >  30 


           INITIALIZE I
           MOVE REGLON-HTML      TO ART-MACROH-J

           INSPECT LIN-11 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-11
           MOVE LIN-11 TO DATOS-PLANO
           WRITE REG-PLANO.

           GO TO LEER-MACRO-HIS.

       LLENAR-RENGLON.

           MOVE GRP-MACROH  (I)  TO GRP-MACROH-W
           MOVE ITEM-MACROH (I)  TO ITEM-MACROH-W

           MOVE LINEA-HTML       TO RENG-HTML (I).


       CERRAR-MACRO-HIS.

           INITIALIZE CIERRE-LIN-11

           IF CONT < 1
              INITIALIZE LLAVE-MACROH-J NOMBRE-MACROH-J ART-MACROH-J
              INSPECT LIN-11 REPLACING ALL "*" BY CARAC-COMILLA 
           END-IF

           MOVE LIN-11 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           MOVE "]}" TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           CLOSE ARCHIVO-JSON
                 ARCHIVO-MACRO-HIS.

       ABRIR-VIAS-ACCESO.

           INSPECT NOM-PLANO-W  REPLACING FIRST "SC-MACRHIS"
                                             BY "SC-ARCHVIA"

           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT  ARCHIVO-VIA-ACCESO.
                       
           INITIALIZE REG-VIA CONT.
           MOVE "{*HCVIAS*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.

       LEER-VIAS-ACCESO.

           READ ARCHIVO-VIA-ACCESO NEXT WITH NO LOCK AT END GO TO CERRAR-VIAS-ACCESO.


           ADD 1 TO CONT
           MOVE COD-VIA       TO COD-VIA-J
           MOVE NOMBRE-VIA    TO NOMBRE-VIA-J
           
           INSPECT LIN-12 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-12
           MOVE LIN-12 TO DATOS-PLANO
           WRITE REG-PLANO.

           GO TO LEER-VIAS-ACCESO.

       CERRAR-VIAS-ACCESO.

           INITIALIZE CIERRE-LIN-12

           IF CONT < 1
              INITIALIZE COD-VIA-J NOMBRE-VIA-J
              INSPECT LIN-12 REPLACING ALL "*" BY CARAC-COMILLA 
           END-IF

           MOVE LIN-12 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           MOVE "]}" TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           CLOSE ARCHIVO-JSON
                 ARCHIVO-VIA-ACCESO.


       ABRIR-ENFERMEDADES.

           INSPECT NOM-PLANO-W  REPLACING FIRST "SC-ARCHVIA"
                                             BY "SC-ARCHENF"

           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT  ARCHIVO-ENFERMEDAD.
                       
           INITIALIZE REG-ENF.
           MOVE "{*ENFERM*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.

       LEER-ENFERMEDADES.

           READ ARCHIVO-ENFERMEDAD NEXT WITH NO LOCK AT END GO TO CERRAR-ENFERMEDADES.

           IF TIPO-ENF = 2
              CONTINUE
           ELSE
              GO TO LEER-ENFERMEDADES
           END-IF

           IF COD-ENF = SPACES OR LOW-VALUES
              GO TO LEER-ENFERMEDADES
           END-IF 

           MOVE TIPO-ENF       TO TIPO-ENF-J
           MOVE COD-ENF        TO COD-ENF-J

           MOVE NOMBRE-ENF     TO NOMBRE-ENF-J
           MOVE SEXO-ENF       TO SEXO-ENF-J 
           MOVE EDAD-MIN-ENF   TO EDAD-MIN-ENF-J
           MOVE EDAD-MAX-ENF   TO EDAD-MAX-ENF-J
           MOVE UNID-EDAD-ENF  TO UNID-EDAD-ENF-J


           INSPECT NOMBRE-ENF-J REPLACING ALL "*" BY " "
           INSPECT NOMBRE-ENF-J REPLACING ALL '"' BY " "
           INSPECT NOMBRE-ENF-J REPLACING ALL "'" BY "x"

           
           INSPECT LIN-13 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-13
           MOVE LIN-13 TO DATOS-PLANO
           WRITE REG-PLANO.

           GO TO LEER-ENFERMEDADES.

       CERRAR-ENFERMEDADES.

           INITIALIZE CIERRE-LIN-13
           MOVE LIN-13 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           MOVE "]}" TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           CLOSE ARCHIVO-JSON
                 ARCHIVO-ENFERMEDAD.

       ABRIR-PATO-CRONIC.

           INSPECT NOM-PLANO-W  REPLACING FIRST "SC-ARCHENF"
                                             BY "SC-ARCHPAT"

           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT  PATOLOGIAS-CRONICAS.
                       
           INITIALIZE REG-CRONIC.
           MOVE "{*PATOCRONIC*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.

       LEER-PATO-CRONIC.
           READ PATOLOGIAS-CRONICAS NEXT WITH NO LOCK AT END GO TO CERRAR-PATO-CRONIC.

           MOVE LLAVE-CRONIC      TO LLAVE-CRONIC-J
           MOVE DESCRIP-CRONIC    TO DESCRIP-CRONIC-J


           INSPECT LIN-14 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-14
           MOVE LIN-14 TO DATOS-PLANO
           WRITE REG-PLANO.

           GO TO LEER-PATO-CRONIC.


       CERRAR-PATO-CRONIC.

           INITIALIZE CIERRE-LIN-14
           MOVE LIN-14 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           MOVE "]}" TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           CLOSE ARCHIVO-JSON
                 PATOLOGIAS-CRONICAS.


       ABRIR-MACRO-EVOL.

           INSPECT NOM-PLANO-W  REPLACING FIRST "SC-ARCHPAT"
                                             BY "SC-MACREVO"

           MOVE NOM-PLANO-W   TO NOM-PLANO2-W

           OPEN OUTPUT ARCHIVO-JSON2.
           OPEN INPUT  ARCHIVO-MACRO-EVOL.
                       
           INITIALIZE REG-MACROEVOL CONT.
           MOVE "{*MACREV*:[" TO  DATOS-PLANO2
           INSPECT DATOS-PLANO2 REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO2.

       LEER-MACRO-EVOL.
           READ ARCHIVO-MACRO-EVOL NEXT WITH NO LOCK AT END GO TO CERRAR-MACCRO-EVOL.

           INITIALIZE I CONT-T

           ADD 1 TO CONT
           MOVE LLAVE-MACROEVOL     TO LLAVE-MACROEVOL-J
           MOVE DETALLE-MACROEVOL   TO DETALLE-MACROEVOL-J

           INSPECT DETALLE-MACROEVOL-J REPLACING ALL "/" BY " "
           INSPECT DETALLE-MACROEVOL-J REPLACING ALL "\" BY " "
           INSPECT DETALLE-MACROEVOL-J REPLACING ALL "'" BY " "
           INSPECT DETALLE-MACROEVOL-J REPLACING ALL "*" BY " "


           MOVE TABLA-MACROEVOL     TO RENG-MACROEVOL-J

           INSPECT RENG-MACROEVOL-J REPLACING ALL '"' BY " "
           INSPECT RENG-MACROEVOL-J REPLACING ALL "*" BY "x"
           INSPECT RENG-MACROEVOL-J REPLACING ALL "'" BY " "
           INSPECT RENG-MACROEVOL-J REPLACING ALL "/" BY " "
           INSPECT RENG-MACROEVOL-J REPLACING ALL "\" BY " "

           PERFORM LLENAR-VIAS-MACROEVOL VARYING I FROM 1 BY 1
                                                  UNTIL I  > 6

           PERFORM LLENAR-FORM-MACROEVOL VARYING I FROM 1 BY 1
                                                  UNTIL I  > 3

           PERFORM LLENAR-CMDS-MACROEVOL VARYING I FROM 1 BY 1
                                                  UNTIL I  > 15


           MOVE TBLA-VIAS-W               TO TABLA-VIAS-MACROEVOL-J 
           INSPECT TABLA-VIAS-MACROEVOL-J REPLACING ALL '"' BY " "
           INSPECT TABLA-VIAS-MACROEVOL-J REPLACING ALL "*" BY "x"
           INSPECT TABLA-VIAS-MACROEVOL-J REPLACING ALL "'" BY " ".


           MOVE TBLA-FORM-MACR-W          TO TABLA-FORM-MACROEVOL-J
           INSPECT TABLA-FORM-MACROEVOL-J REPLACING ALL '"' BY " "
           INSPECT TABLA-FORM-MACROEVOL-J REPLACING ALL "*" BY "x"
           INSPECT TABLA-FORM-MACROEVOL-J REPLACING ALL "'" BY " ".


           MOVE TBLA-CMD-MACR-W         TO COMANDOS-MACROEVOL-J
           INSPECT COMANDOS-MACROEVOL-J REPLACING ALL '"' BY " "
           INSPECT COMANDOS-MACROEVOL-J REPLACING ALL "*" BY "x"
           INSPECT COMANDOS-MACROEVOL-J REPLACING ALL "'" BY " ".




           INSPECT LIN-15 REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT LIN-15 REPLACING ALL LOW-VALUES BY SPACES
           MOVE "," TO CIERRE-LIN-15

           MOVE LIN-15 TO DATOS-PLANO2
           WRITE REG-PLANO2 END-WRITE

           GO TO LEER-MACRO-EVOL.

       LLENAR-VIAS-MACROEVOL.

           IF VIAS-MACROEVOL (I) = SPACES OR LOW-VALUES
              INITIALIZE VIAS-MACROEVOL (I)
           END-IF.

           MOVE VIAS-MACROEVOL (I) TO VIAS-MACROEVOL-W (I).
          
       LLENAR-FORM-MACROEVOL.

           IF COD-FORM-MACROEVOL (I) = SPACES OR LOW-VALUES
              INITIALIZE COD-FORM-MACROEVOL (I)
           END-IF.
           MOVE COD-FORM-MACROEVOL (I)   TO COD-FORM-W (I).

       LLENAR-CMDS-MACROEVOL.

           IF PROG-MACROEVOL (I) = SPACES OR LOW-VALUES
              INITIALIZE PROG-MACROEVOL (I)
           END-IF.

           MOVE PROG-MACROEVOL(I) TO PROG-MACRO-W (I).

       CERRAR-MACCRO-EVOL.

           INITIALIZE LIN-15
           INSPECT LIN-15 REPLACING ALL "*" BY CARAC-COMILLA 

           MOVE LIN-15 TO DATOS-PLANO2
           WRITE REG-PLANO2 END-WRITE

           MOVE "]}" TO DATOS-PLANO2
           WRITE REG-PLANO2 END-WRITE

           CLOSE ARCHIVO-JSON2
                 ARCHIVO-MACRO-EVOL.

       ABRIR-ESPEC.

           INSPECT NOM-PLANO-W  REPLACING FIRST "SC-MACREVO"
                                             BY "SC-ESPECIA"

           OPEN OUTPUT ARCHIVO-JSON.
           OPEN INPUT  ARCHIVO-ESPECIALIDAD.
                       
           INITIALIZE REG-ESP.
           MOVE "{*ESPEC*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.

       LEER-ESPEC.
           READ ARCHIVO-ESPECIALIDAD NEXT WITH NO LOCK AT END GO TO CERRAR-ESPEC.

           IF INIC-ESP = "*"
              GO TO LEER-ESPEC
           END-IF

           MOVE COD-ESP         TO COD-ESP-J
           MOVE NOMBRE-ESP      TO NOMBRE-ESP-J          
           MOVE COSTO-ESP       TO COSTO-ESP-J

           INSPECT LIN-16 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-16
           MOVE LIN-16 TO DATOS-PLANO
           WRITE REG-PLANO.

           GO TO LEER-ESPEC.


       CERRAR-ESPEC.

           INITIALIZE CIERRE-LIN-16
           MOVE LIN-16 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           MOVE "]}" TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           CLOSE ARCHIVO-JSON
                 ARCHIVO-ESPECIALIDAD.


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
