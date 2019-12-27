      *=================================================================
      * BUSCA TODOS LOS DATOS DE UN PACIENTE - SER810-1
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
      
      
           SELECT ARCHIVO-PACIENTES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PACIE-LNK
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PACI
                  ALTERNATE RECORD KEY  DESCRIP-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  EPS-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  ID-COTIZ-PACI WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.                            
                  
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-PACIE.CBL".
      
       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".

       77 LLEGADA-W                   PIC X(100).
       01 DATOS-PLANO-W               PIC X(1024).
       77 DIRECTORIO-LLEGADA-W        PIC X(20).
       

       01 TABLA-EDIT.
          02 TABLA-EDIT-W             PIC X(1024) OCCURS 33.

      
       77 ID-BUSQ-PACI-W             PIC X(15).

       01 LIN-1.  
          02 FILLER                  PIC X(7)   VALUE "{*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-PACI-J              PIC X(15).

          02 FILLER                  PIC XX      VALUE "*,".       
          02 FILLER                  PIC X(10)   VALUE "*TIPO-ID*:".
          02 FILLER                  PIC X       VALUE "*".
          02 TIPO-ID-PACI-J          PIC X(3).

          02 FILLER                  PIC XX      VALUE "*,".       
          02 FILLER                  PIC X(10)   VALUE "*DESCRIP*:".
          02 FILLER                  PIC X       VALUE "*".
          02 DESCRIP-PACI-J          PIC X(54).

          02 FILLER                  PIC XX      VALUE "*,".       
          02 FILLER                  PIC X(8)    VALUE "*NACIM*:".
          02 FILLER                  PIC X       VALUE "*".
          02 NACIM-PACI-J            PIC X(8).

          02 FILLER                  PIC XX      VALUE "*,".       
          02 FILLER                  PIC X(7)    VALUE "*SEXO*:".
          02 FILLER                  PIC X       VALUE "*".
          02 SEXO-PACI-J             PIC X.

          02 FILLER                  PIC XX      VALUE "*,".       
          02 FILLER                  PIC X(9)    VALUE "*CIUDAD*:".
          02 FILLER                  PIC X       VALUE "*".
          02 CIUDAD-PACI-J            PIC X(9).

          02 FILLER                  PIC XX      VALUE "*,".       
          02 FILLER                  PIC X(7)    VALUE "*ZONA*:".
          02 FILLER                  PIC X       VALUE "*".
          02 ZONA-PACI-J             PIC X(9).

          02 FILLER                  PIC XX      VALUE "*,".       
          02 FILLER                  PIC X(7)    VALUE "*TIPO*:".
          02 FILLER                  PIC X       VALUE "*".
          02 TIPO-PACI-J             PIC X.

          02 FILLER                  PIC XX      VALUE "*,".       
          02 FILLER                  PIC X(6)    VALUE "*EPS*:".
          02 FILLER                  PIC X       VALUE "*".
          02 EPS-PACI-J              PIC X(6).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)    VALUE "*TIPO-AFIL*:".
          02 FILLER                 PIC X       VALUE "*".
          02 TIPO-AFIL-PACI-J       PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(11)    VALUE "*NRO-AFIL*:".
          02 FILLER                 PIC X       VALUE "*".
          02 NRO-AFIL-PACI-J        PIC X(15).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(11)    VALUE "*ID-COTIZ*:".
          02 FILLER                 PIC X       VALUE "*".
          02 ID-COTIZ-PACI-J        PIC X(15).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(11)    VALUE "*CONTRATO*:".
          02 FILLER                 PIC X       VALUE "*".
          02 CONTRATO-PACI-J        PIC X(15).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(8)    VALUE "*FICHA*:".
          02 FILLER                 PIC X       VALUE "*".
          02 FICHA-PACI-J           PIC X(6).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(10)   VALUE "*ESTRATO*:".
          02 FILLER                 PIC X       VALUE "*".
          02 ESTRATO-PACI-J         PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(9)    VALUE "*ACOMPA*:".
          02 FILLER                 PIC X       VALUE "*".
          02 ACOMPA-PACI-J          PIC X(30).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(9)    VALUE "*CLASIF*:".
          02 FILLER                 PIC X       VALUE "*".
          02 CLASIF-PACI-J          PIC XX.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(10)   VALUE "*EST-CIV*:".
          02 FILLER                 PIC X       VALUE "*".
          02 EST-CIV-PACI-J         PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(11)   VALUE "*TELEFONO*:".
          02 FILLER                 PIC X       VALUE "*".
          02 TELEFONO-PACI-J         PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(9)    VALUE "*DIRECC*:".
          02 FILLER                 PIC X       VALUE "*".
          02 DIRECC-PACI-J          PIC X(30).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(7)    VALUE "*OCUP*:".
          02 FILLER                 PIC X       VALUE "*".
          02 OCUP-PACI-J            PIC X(3).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(10)   VALUE "*DERECHO*:".
          02 FILLER                 PIC X       VALUE "*".
          02 DERECHO-PACI-J         PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(10)   VALUE "*CRONICO*:".
          02 FILLER                 PIC X       VALUE "*".
          02 CRONICO-PACI-J         PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(9)    VALUE "*PARENT*:".
          02 FILLER                 PIC X       VALUE "*".
          02 PARENT-PACI-J          PIC XX.

          02 FILLER                 PIC XX       VALUE "*,".       
          02 FILLER                 PIC X(10)    VALUE "*EMPRESA*:".
          02 FILLER                 PIC X        VALUE "*".
          02 EMPRESA-PACI-J         PIC X(30).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(9)    VALUE "*OBSERV*:".
          02 FILLER                 PIC X       VALUE "*".
          02 OBSERV-PACI-J          PIC X(50).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(6)    VALUE "*NOV*:".
          02 FILLER                 PIC X       VALUE "*".
          02 NOV-PACI-J             PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*FECHA-NOV*:".
          02 FILLER                 PIC X       VALUE "*".
          02 FECHA-NOV-PACI-J       PIC X(8).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(14)   VALUE "*FECHA-VENCE*:".
          02 FILLER                 PIC X       VALUE "*".
          02 FECHA-VENCE-PACI-J     PIC X(8).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(13)   VALUE "*FECHA-AFIL*:".
          02 FILLER                 PIC X       VALUE "*".
          02 FECHA-AFIL-PACI-J     PIC X(8).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*REST-APLI*:".
          02 FILLER                 PIC X       VALUE "*".
          02 REST-APLI-PACI-J       PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*REST-DROG*:".
          02 FILLER                 PIC X       VALUE "*".
          02 REST-DROG-PACI-J       PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*REST-CIRU*:".
          02 FILLER                 PIC X       VALUE "*".
          02 REST-CIRU-PACI-J       PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*REST-LABO*:".
          02 FILLER                 PIC X       VALUE "*".
          02 REST-LABO-PACI-J       PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*REST-IMAG*:".
          02 FILLER                 PIC X       VALUE "*".
          02 REST-IMAG-PACI-J       PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*REST-ESTA*:".
          02 FILLER                 PIC X       VALUE "*".
          02 REST-ESTA-PACI-J       PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*REST-CONS*:".
          02 FILLER                 PIC X       VALUE "*".
          02 REST-CONS-PACI-J       PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*REST-TERF*:".
          02 FILLER                 PIC X       VALUE "*".
          02 REST-TERF-PACI-J       PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*REST-TERO*:".
          02 FILLER                 PIC X       VALUE "*".
          02 REST-TERO-PACI-J       PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*REST-ODON*:".
          02 FILLER                 PIC X       VALUE "*".
          02 REST-ODON-PACI-J       PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(11)   VALUE "*REST-PYP*:".
          02 FILLER                 PIC X       VALUE "*".
          02 REST-PYP-PACI-J        PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(11)    VALUE "*GRP-SANG*:".
          02 FILLER                 PIC X       VALUE "*".
          02 GRP-SANG-PACI-J        PIC XX.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(5)    VALUE "*RH*:".
          02 FILLER                 PIC X       VALUE "*".
          02 RH-PACI-J              PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(11)   VALUE "*ULT-MAMO*:".
          02 FILLER                 PIC X       VALUE "*".
          02 ULT-MAMO-PACI-J        PIC X(6).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(11)   VALUE "*NIT-FACT*:".
          02 FILLER                 PIC X       VALUE "*".
          02 NIT-FACT-PACI-J        PIC X(10).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*FECHA-NIT*:".
          02 FILLER                 PIC X       VALUE "*".
          02 FECHA-NIT-PACI-J       PIC X(8).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(17)   VALUE "*ANTECED-CANCER*:".
          02 FILLER                 PIC X       VALUE "*".
          02 ANTECED-CANCER-PACI-J  PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(8)    VALUE "*ETNIA*:".
          02 FILLER                 PIC X       VALUE "*".
          02 ETNIA-PACI-J           PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(8)   VALUE "*PADRE*:".
          02 FILLER                 PIC X       VALUE "*".
          02 PADRE-PACI-J           PIC X(30).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(8)    VALUE "*MADRE*:".
          02 FILLER                 PIC X       VALUE "*".
          02 MADRE-PACI-J           PIC X(30).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(6)    VALUE "*CEL*:".
          02 FILLER                 PIC X       VALUE "*".
          02 CEL-PACI-J             PIC X(12).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(11)   VALUE "*TEL-ACOM*:".
          02 FILLER                 PIC X       VALUE "*".
          02 TEL-ACOM-PACI-J        PIC X(10).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(9)    VALUE "*COPAGO*:".
          02 FILLER                 PIC X       VALUE "*".
          02 COPAGO-PACI-J          PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(10)   VALUE "*ALT-COS*:".
          02 FILLER                 PIC X       VALUE "*".
          02 ALT-COS-PACI-J         PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(17)   VALUE "*EMB-ALTO-RIESG*:".
          02 FILLER                 PIC X       VALUE "*".
          02 EMB-ALTO-RIESG-PACI-J  PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*NIV-ESTUD*:".
          02 FILLER                 PIC X       VALUE "*".
          02 NIV-ESTUD-PACI-J       PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(13)   VALUE "*CERT-ESTUD*:".
          02 FILLER                 PIC X       VALUE "*".
          02 CERT-ESTUD-PACI-J      PIC X(6).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*FECHA-RET*:".
          02 FILLER                 PIC X       VALUE "*".
          02 FECHA-RET-PACI-J       PIC X(8).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(9)    VALUE "*E-MAIL*:".
          02 FILLER                 PIC X       VALUE "*".
          02 E-MAIL-PACI-J          PIC X(80).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(11)   VALUE "*MED-FAMI*:".
          02 FILLER                 PIC X       VALUE "*".
          02 MED-FAMI-PACI-J        PIC X(10).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*INSTITUTO*:".
          02 FILLER                 PIC X       VALUE "*".
          02 INSTITUTO-PACI-J       PIC X(12).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(15)   VALUE "*PATOL-CRONIC*:".
          02 FILLER                 PIC X       VALUE "*".
          02 PATOL-CRONIC-PACI-J    PIC X(3).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(13)   VALUE "*PERI-ESTUD*:".
          02 FILLER                 PIC X       VALUE "*".
          02 PERI-ESTUD-PACI-J      PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(13)   VALUE "*CERT-ECONO*:".
          02 FILLER                 PIC X       VALUE "*".
          02 CERT-ECONO-PACI-J      PIC X(6).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(11)   VALUE "*PERI-ECO*:".
          02 FILLER                 PIC X       VALUE "*".
          02 PERI-ECO-PACI-J        PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*ETNIA-IND*:".
          02 FILLER                 PIC X       VALUE "*".
          02 ETNIA-IND-PACI-J       PIC XX.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*TIPO-POBL*:".
          02 FILLER                 PIC X       VALUE "*".
          02 TIPO-POBL-PACI-J       PIC XX.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(9)    VALUE "*DISCAP*:".
          02 FILLER                 PIC X       VALUE "*".
          02 DISCAP-PACI-J          PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(13)   VALUE "*CABEZA-FAM*:".
          02 FILLER                 PIC X       VALUE "*".
          02 CABEZA-FAM-PACI-J      PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(9)    VALUE "*TUTELA*:".
          02 FILLER                 PIC X       VALUE "*".
          02 TUTELA-PACI-J          PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(10)   VALUE "*OCUP-V8*:".
          02 FILLER                 PIC X       VALUE "*".
          02 OCUP-V8-PACI-J         PIC X(4).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(11)   VALUE "*PROG-ESP*:".
          02 FILLER                 PIC X       VALUE "*".
          02 PROG-ESP-PACI-J        PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(14)   VALUE "*MULTICONSUL*:".
          02 FILLER                 PIC X       VALUE "*".
          02 MULTICONSUL-PACI-J     PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(13)   VALUE "*COD-ORIGEN*:".
          02 FILLER                 PIC X       VALUE "*".
          02 COD-ORIGEN-PACI-J      PIC X(15).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(13)   VALUE "*DEMAN-INDU*:".
          02 FILLER                 PIC X       VALUE "*".
          02 DEMAN-INDU-PACI-J      PIC X.

          02 FILLER                   PIC XX      VALUE "*,".       
          02 FILLER                   PIC X(19)   VALUE "*FECHA-DEMAN-INDU*:".
          02 FILLER                   PIC X       VALUE "*".
          02 FECHA-DEMAN-INDU-PACI-J  PIC X(8).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(18)   VALUE "*VICTI-CONFLICTO*:".
          02 FILLER                 PIC X       VALUE "*".
          02 VICTI-CONFLICTO-PACI-J PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(13)   VALUE "*BLOQUEO-HC*:".
          02 FILLER                 PIC X       VALUE "*".
          02 BLOQUEO-HC-PACI-J     PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(16)   VALUE "*NOM-COMUNIDAD*:".
          02 FILLER                 PIC X       VALUE "*".
          02 NOM-COMUNIDAD-PACI-J   PIC X(3).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(16)   VALUE "*NOM-RESGUARDO*:".
          02 FILLER                 PIC X       VALUE "*".
          02 NOM-RESGUARDO-PACI-J   PIC X(3).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*PAIS-ORIG*:".
          02 FILLER                 PIC X       VALUE "*".
          02 PAIS-ORIG-PACI-J       PIC X(3).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(11)   VALUE "*CONT-WEB*:".
          02 FILLER                 PIC X       VALUE "*".
          02 CONT-WEB-PACI-J        PIC X(8).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*REGIS-WEB*:".
          02 FILLER                 PIC X       VALUE "*".
          02 REGIS-WEB-PACI-J       PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(15)   VALUE "*PORTABILIDAD*:".
          02 FILLER                 PIC X       VALUE "*".
          02 PORTABILIDAD-PACI-J    PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(14)   VALUE "*CIUDAD-ASEG*:".
          02 FILLER                 PIC X       VALUE "*".
          02 CIUDAD-ASEG-PACI-J     PIC X(5).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(17)   VALUE "*VICT-ABUSO-SEX*:".
          02 FILLER                 PIC X       VALUE "*".
          02 VICT-ABUSO-SEX-PACI-J  PIC X.

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(11)   VALUE "*LUGAR-ID*:".
          02 FILLER                 PIC X       VALUE "*".
          02 LUGAR-ID-PACI-J        PIC X(5).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(13)   VALUE "*FECHA-NOTA*:".
          02 FILLER                 PIC X       VALUE "*".
          02 FECHA-NOTA-PAC2-J      PIC X(8).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(15)   VALUE "*OBSERV1-NOTA*:".
          02 FILLER                 PIC X       VALUE "*".
          02 OBSERV1-NOTA-PAC2-J    PIC X(80).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(15)   VALUE "*OBSERV2-NOTA*:".
          02 FILLER                 PIC X       VALUE "*".
          02 OBSERV2-NOTA-PAC2-J    PIC X(80).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(15)   VALUE "*OBSERV3-NOTA*:".
          02 FILLER                 PIC X       VALUE "*".
          02 OBSERV3-NOTA-PAC2-J    PIC X(80).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(15)   VALUE "*OBSERV4-NOTA*:".
          02 FILLER                 PIC X       VALUE "*".
          02 OBSERV4-NOTA-PAC2-J    PIC X(80).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(17)   VALUE "*OBSERV5-NOTA*:".
          02 FILLER                 PIC X       VALUE "*".
          02 OBSERV5-NOTA-PAC2-J    PIC X(80).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(17)   VALUE "*OPER-OBSE-NOTA*:".
          02 FILLER                 PIC X       VALUE "*".
          02 OPER-OBSE-NOTA-PAC2-J  PIC X(4).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*FACT-NOTA*:".
          02 FILLER                 PIC X       VALUE "*".
          02 FACT-NOTA-PAC2-J.
             05 PREFIJO-NOTA-PAC2-J PIC X.
             05 NUMERO-NOTA-PAC2-J  PIC X(6).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)    VALUE "*OPER-CREA*:".
          02 FILLER                 PIC X       VALUE "*".
          02 OPER-CREA-PACI-J       PIC X(4).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(13)    VALUE "*FECHA-CREA*:".
          02 FILLER                 PIC X       VALUE "*".
          02 FECHA-CREA-PACI-J      PIC X(8).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)    VALUE "*HORA-CREA*:".
          02 FILLER                 PIC X       VALUE "*".
          02 HORA-CREA-PACI-J       PIC X(4).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)    VALUE "*OPER-CORR*:".
          02 FILLER                 PIC X       VALUE "*".
          02 OPER-CORR-PACI-J       PIC X(4).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(13)    VALUE "*FECHA-CORR*:".
          02 FILLER                 PIC X       VALUE "*".
          02 FECHA-CORR-PACI-J       PIC X(8).

          02 FILLER                 PIC XX      VALUE "*,".       
          02 FILLER                 PIC X(12)   VALUE "*HORA-CORR*:".
          02 FILLER                 PIC X       VALUE "*".
          02 HORA-CORR-PACI-J       PIC X(4).
          02 FILLER                 PIC X(4)    VALUE "*}]}". 
       
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
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-USU-W        TO MSJ2-HTML
              MOVE "SER810-1"         TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" 
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-SESION-W     TO MSJ2-HTML
              MOVE "SER810-1"         TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" 
              CONTINUE 
           ELSE
              MOVE OTR-STAT            TO MSJ1-HTML
              MOVE NOM-PACIE-LNK         TO MSJ2-HTML
              MOVE "SER810-1"            TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       END DECLARATIVES.

      
       INICIAR-IIS.
001162     MOVE LOW-VALUE TO COBW3.
001163     MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
001164     CALL "COBW3_INIT" USING COBW3.
001220

       LEER-DATO-HTML.
000788     MOVE "datosh" TO COBW3-SEARCH-DATA.
000790     CALL "COBW3_GET_VALUE" USING COBW3.
000791     MOVE COBW3-GET-DATA TO LINEA-LLEGAD-W.

           UNSTRING LINEA-LLEGAD-W DELIMITED BY "|" 
             INTO SESION-LLEGAD-W, DIRECTORIO-LLEGADA-W, CARPTA-LLEGAD-W, ID-BUSQ-PACI-W
           END-UNSTRING.

           MOVE SESION-LLEGAD-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.

001238 INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W.
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.
       
001303 ABRIR-USUARIO.
           INITIALIZE OTR-STAT
001304     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
001305
001306     OPEN INPUT ARCHIVO-USUNET
001307     EVALUATE OTR-STAT
001308       WHEN "00"  CONTINUE
001309       WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
001310                  CLOSE       ARCHIVO-USUNET
001311                  OPEN I-O    ARCHIVO-USUNET
001312                  INITIALIZE REG-USUNET
001314                  WRITE REG-USUNET
001315                  CLOSE       ARCHIVO-USUNET
001316                  MOVE "SC-2"                      TO MSJ1-HTML
                        MOVE "Falta configurar usuario"  TO MSJ2-HTML
                        MOVE "SER810-1"                    TO MSJ3-HTML
001319                  GO TO ENVIAR2-ERROR
001320       WHEN OTHER GO TO CERRAR-SESION
001321     END-EVALUATE     
001322
001332     INITIALIZE LLAVE-USUNET.

       LEER-USUARIO.
001335     READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
001345     CLOSE ARCHIVO-USUNET.
001336
001337     IF NOMBRE-USUNET = SPACES
001339        MOVE "SC-2"                        TO MSJ1-HTML
001340        MOVE "Falta configurar usuario"    TO MSJ2-HTML
001341        MOVE "SER810-1"                    TO MSJ3-HTML
001342        GO TO ENVIAR2-ERROR
001343     END-IF.

       ASIGNAR-NOMBRES.

           INITIALIZE NOM-PACIE-LNK
           MOVE "\\" TO NOM-PACIE-LNK
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

           INSPECT NOM-PACIE-LNK REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

           INSPECT NOM-PACIE-LNK REPLACING FIRST " "
                                             BY "\"

           INSPECT NOM-PACIE-LNK REPLACING FIRST "                    "
                                           BY DIRECTORIO-LLEGADA-W.

           INSPECT NOM-PACIE-LNK REPLACING FIRST "                     "
                                            BY "\CONTROL\SC-PACIE.DAT".
                                                       
           
       ABRIR-REGISTRO-PACIENTE.
           OPEN INPUT ARCHIVO-PACIENTES.
           INITIALIZE REG-PACI.
           MOVE ID-BUSQ-PACI-W TO  COD-PACI
           READ ARCHIVO-PACIENTES WITH NO LOCK
              INVALID KEY
                 MOVE "SC-2"      TO MSJ1-HTML
007250           MOVE "08"        TO MSJ2-HTML
007250           MOVE "SER810"    TO MSJ3-HTML
007260           GO TO ENVIAR2-ERROR
           END-READ


           MOVE COD-PACI              TO COD-PACI-J
           MOVE TIPO-PACI             TO TIPO-ID-PACI-J
           MOVE DESCRIP-PACI          TO DESCRIP-PACI-J
           MOVE NACIM-PACI            TO NACIM-PACI-J
           MOVE SEXO-PACI             TO SEXO-PACI-J
           MOVE ZONA-PACI             TO ZONA-PACI-J
           MOVE CIUDAD-PACI           TO CIUDAD-PACI-J
           MOVE TIPO-PACI             TO TIPO-PACI-J
           MOVE EPS-PACI              TO EPS-PACI-J
           MOVE TIPO-AFIL-PACI        TO TIPO-AFIL-PACI-J
           MOVE NRO-AFIL-PACI         TO NRO-AFIL-PACI-J
           MOVE ID-COTIZ-PACI         TO ID-COTIZ-PACI-J
           MOVE CONTRATO-PACI         TO CONTRATO-PACI-J
           MOVE FICHA-PACI            TO FICHA-PACI-J
           MOVE ESTRATO-PACI          TO ESTRATO-PACI-J
           MOVE ACOMPA-PACI           TO ACOMPA-PACI-J
           MOVE CLASIF-PACI           TO CLASIF-PACI-J
           MOVE EST-CIV-PACI          TO EST-CIV-PACI-J
           MOVE TELEFONO-PACI         TO TELEFONO-PACI-J
           MOVE DIRECC-PACI           TO DIRECC-PACI-J
           MOVE OCUP-PACI             TO OCUP-PACI-J
           MOVE DERECHO-PACI          TO DERECHO-PACI-J
           MOVE CRONICO-PACI          TO CRONICO-PACI-J
           MOVE PARENT-PACI           TO PARENT-PACI-J
           MOVE EMPRESA-PACI          TO EMPRESA-PACI-J
           MOVE OBSERV-PACI           TO OBSERV-PACI-J
           MOVE NOV-PACI              TO NOV-PACI-J
           MOVE FECHA-NOV-PACI        TO FECHA-NOV-PACI-J
           MOVE FECHA-VENCE-PACI      TO FECHA-VENCE-PACI-J
           MOVE FECHA-AFIL-PACI       TO FECHA-AFIL-PACI-J
           MOVE REST-APLI-PACI        TO REST-APLI-PACI-J
           MOVE REST-DROG-PACI        TO REST-DROG-PACI-J
           MOVE REST-CIRU-PACI        TO REST-CIRU-PACI-J
           MOVE REST-LABO-PACI        TO REST-LABO-PACI-J
           MOVE REST-IMAG-PACI        TO REST-IMAG-PACI-J
           MOVE REST-ESTA-PACI        TO REST-ESTA-PACI-J
           MOVE REST-CONS-PACI        TO REST-CONS-PACI-J
           MOVE REST-TERF-PACI        TO REST-TERF-PACI-J
           MOVE REST-TERO-PACI        TO REST-TERO-PACI-J
           MOVE REST-ODON-PACI        TO REST-ODON-PACI-J
           MOVE REST-PYP-PACI         TO REST-PYP-PACI-J
           MOVE GRP-SANG-PACI         TO GRP-SANG-PACI-J
           MOVE RH-PACI               TO RH-PACI-J
           MOVE ULT-MAMO-PACI         TO ULT-MAMO-PACI-J
           MOVE NIT-FACT-PACI         TO NIT-FACT-PACI-J
           MOVE FECHA-NIT-PACI        TO FECHA-NIT-PACI-J
           MOVE ANTECED-CANCER-PACI   TO ANTECED-CANCER-PACI-J
           MOVE ETNIA-PACI            TO ETNIA-PACI-J
           MOVE PADRE-PACI            TO PADRE-PACI-J
           MOVE MADRE-PACI            TO MADRE-PACI-J
           MOVE CEL-PACI              TO CEL-PACI-J
           MOVE TEL-ACOM-PACI         TO TEL-ACOM-PACI-J
           MOVE COPAGO-PACI           TO COPAGO-PACI-J
           MOVE ALT-COS-PACI          TO ALT-COS-PACI-J
           MOVE EMB-ALTO-RIESG-PACI   TO EMB-ALTO-RIESG-PACI-J
           MOVE CERT-ESTUD-PACI       TO CERT-ESTUD-PACI-J
           MOVE NIV-ESTUD-PACI        TO NIV-ESTUD-PACI-J
           MOVE FECHA-RET-PACI        TO FECHA-RET-PACI-J
           MOVE E-MAIL-PACI           TO E-MAIL-PACI-J
           MOVE MED-FAMI-PACI         TO MED-FAMI-PACI-J
           MOVE PATOL-CRONIC-PACI     TO PATOL-CRONIC-PACI-J
           MOVE PERI-ESTUD-PACI       TO PERI-ESTUD-PACI-J
           MOVE CERT-ECONO-PACI       TO CERT-ECONO-PACI-J
           MOVE PERI-ECO-PACI         TO PERI-ECO-PACI-J
           MOVE ETNIA-IND-PACI        TO ETNIA-IND-PACI-J
           MOVE TIPO-POBL-PACI        TO TIPO-POBL-PACI-J
           MOVE DISCAP-PACI           TO DISCAP-PACI-J
           MOVE CABEZA-FAM-PACI       TO CABEZA-FAM-PACI-J
           MOVE TUTELA-PACI           TO TUTELA-PACI-J
           MOVE OCUP-V8-PACI          TO OCUP-V8-PACI-J
           MOVE PROG-ESP-PACI         TO PROG-ESP-PACI-J
           MOVE MULTICONSUL-PACI      TO MULTICONSUL-PACI-J
           MOVE COD-ORIGEN-PACI       TO COD-ORIGEN-PACI-J
           MOVE DEMAN-INDU-PACI       TO DEMAN-INDU-PACI-J
           MOVE FECHA-DEMAN-INDU-PACI TO FECHA-DEMAN-INDU-PACI-J
           MOVE VICTI-CONFLICTO-PACI  TO VICTI-CONFLICTO-PACI-J
           MOVE BLOQUEO-HC-PACI       TO BLOQUEO-HC-PACI-J
           MOVE NOM-COMUNIDAD-PACI    TO NOM-COMUNIDAD-PACI-J
           MOVE NOM-RESGUARDO-PACI    TO NOM-RESGUARDO-PACI-J
           MOVE INSTITUTO-PACI        TO INSTITUTO-PACI-J
           MOVE PAIS-ORIG-PACI        TO PAIS-ORIG-PACI-J
           MOVE CONT-WEB-PACI         TO CONT-WEB-PACI-J
           MOVE PORTABILIDAD-PACI     TO PORTABILIDAD-PACI-J
           MOVE CIUDAD-ASEG-PACI      TO CIUDAD-ASEG-PACI-J
           MOVE VICT-ABUSO-SEX-PACI   TO VICT-ABUSO-SEX-PACI-J
           MOVE LUGAR-ID-PACI         TO LUGAR-ID-PACI-J
           MOVE FECHA-NOTA-PAC2       TO FECHA-NOTA-PAC2-J
           MOVE OBSERV1-NOTA-PAC2     TO OBSERV1-NOTA-PAC2-J
           MOVE OBSERV2-NOTA-PAC2     TO OBSERV2-NOTA-PAC2-J
           MOVE OBSERV3-NOTA-PAC2     TO OBSERV3-NOTA-PAC2-J
           MOVE OBSERV4-NOTA-PAC2     TO OBSERV4-NOTA-PAC2-J
           MOVE OBSERV5-NOTA-PAC2     TO OBSERV5-NOTA-PAC2-J
           MOVE OPER-OBSE-NOTA-PAC2   TO OPER-OBSE-NOTA-PAC2-J
           MOVE FACT-NOTA-PAC2        TO FACT-NOTA-PAC2-J
           MOVE OPER-CREA-PACI        TO OPER-CREA-PACI-J
           MOVE FECHA-CREA-PACI       TO FECHA-CREA-PACI-J
           MOVE HORA-CREA-PACI        TO HORA-CREA-PACI-J
           MOVE OPER-CORR-PACI        TO OPER-CORR-PACI-J
           MOVE FECHA-CORR-PACI       TO FECHA-CORR-PACI-J
           MOVE HORA-CORR-PACI        TO HORA-CORR-PACI-J


           STRING "{*REG-PACI*:[", LIN-1 
                  DELIMITED BY SIZE INTO TABLA-EDIT
           END-STRING


           INSPECT TABLA-EDIT  REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT TABLA-EDIT  REPLACING ALL LOW-VALUES BY SPACES
           
           PERFORM MONTAR-PLANO VARYING I FROM 1 BY 1 UNTIL I > 33.

  
       CERRAR-ARCHIVOS.  
           CLOSE  ARCHIVO-PACIENTES.



010700 PAGINA-CONFIG.
010720     MOVE "statuscode" TO COBW3-CNV-NAME
010730     MOVE "00"         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010720     MOVE "programa-id" TO COBW3-CNV-NAME
010730     MOVE "SER810-1"      TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010760     MOVE "..\PAGINAS\RECIBIDOSLISTADO.ASPX" TO SALIDA-HTML
010770     PERFORM ABRIR-HTML.

007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.

       DATOS-ENVIO.
           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE DATOS-PLANO-W    TO COBW3-CNV-VALUE
           CALL "COBW3_SET_REPEAT" USING COBW3. 

       MONTAR-PLANO.
           IF TABLA-EDIT-W (I) = SPACES OR LOW-VALUES
             CONTINUE
           ELSE
             INITIALIZE DATOS-PLANO-W
             INSPECT TABLA-EDIT-W (I) REPLACING ALL "*"        BY CARAC-COMILLA
             INSPECT TABLA-EDIT-W (I) REPLACING ALL LOW-VALUES BY SPACES
             INSPECT TABLA-EDIT-W (I) REPLACING ALL "*" BY CARAC-COMILLA
             MOVE TABLA-EDIT-W    (I) TO DATOS-PLANO-W
             PERFORM DATOS-ENVIO
           END-IF.    

008040 COPY "..\..\FUENTES\SC-WEB19.CBL".
