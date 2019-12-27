      *=================================================================
      * BUSQUEDA Y CONSULTA DE ARCHIVO PACIENTES 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
           COPY "..\..\FUENTES\FS-USUNET.CBL".
           COPY "..\..\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                 ASSIGN NOM-USUAR-W
                 ORGANIZATION IS INDEXED
                 ACCESS MODE IS DYNAMIC;
                 RECORD KEY IS NOMBRE-USU
                 ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                 ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                 ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                 FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-PACIENTES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PACIE-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PACI
                  ALTERNATE RECORD KEY  DESCRIP-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  EPS-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  ID-COTIZ-PACI WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-CIUDAD  LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-CIUD-W
      *"P:\PROG\DATOS\SC-ARCHCIUD.DAT";
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-CIU
                  ALTERNATE RECORD KEY IS NOMBRE-CIU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-COLEGIOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-COLE-W 
      *"\PROG\DATOS\SC-COLEG.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY LLAVE-COLEG
                  ALTERNATE RECORD KEY IS DESCRIP-COLEG WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-OCUPACIONES
	          ASSIGN NOM-OCUP-W 
      *"\PROG\DATOS\SC-ARCHOCU.DAT";
                  ORGANIZATION IS INDEXED;
	          ACCESS MODE  IS DYNAMIC
	          RECORD KEY   IS COD-OCU
                  ALTERNATE RECORD KEY IS
	          NOMBRE-OCU WITH DUPLICATES
                  FILE STATUS OTR-STAT.

           SELECT ARCHIVO-ENTIDADES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-ENTI-W
      *"\PROG\DATOS\SC-ARCHENT.DAT";
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-ENT
                  ALTERNATE RECORD KEY IS NOMBRE-ENT WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-TERCEROS  LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TER-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-TERCERO
                  ALTERNATE RECORD KEY DESCRIP-TER WITH DUPLICATES
                  ALTERNATE RECORD KEY ACT-TER     WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT EXTENSION-PROFESIONALES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PROF-W 
      *"..\CONTROL\SC-ARCHPROF2.DAT"
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-PROF2
                  ALTERNATE RECORD KEY FECHA-PROF2 WITH DUPLICATES
                  FILE STATUS IS OTR-STAT. 

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-PACIE.CBL".
       COPY "..\..\FUENTES\FD-CIUDA.CBL".
       COPY "..\..\FUENTES\FD-COLEG.CBL".
       COPY "..\..\FUENTES\FD-OCUPA.CBL".
       COPY "..\..\FUENTES\FD-ENTID.CBL".
       COPY "..\..\FUENTES\FD-TERCE.CBL".
       COPY "..\..\FUENTES\FD-PROFE2.CBL".
    

       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-USUAR-W                            PIC X(70).
       77 NOM-PACIE-W                            PIC X(70).
       77 NOM-CIUD-W                             PIC X(70).
       77 NOM-COLE-W                             PIC X(70). 
       77 NOM-OCUP-W                             PIC X(70).
       77 NOM-ENTI-W                             PIC X(70).
       77 NOM-TER-W                              PIC X(70).
       77 NOM-PROF-W                             PIC X(70).
       01 LLEGADA-W                              PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W                      PIC X(15).
             03 FECHA-LLEGAD-W                   PIC X(8).
             03 HORA-LLEGAD-W                    PIC X(6).
          02 DIR-CONTAB-LLEGADA-W                PIC X(30).
          02 MES-CONTAB-LLEGADA-W                PIC XX.
          02 COD-PACI-LLEGADA-W                  PIC X(15). 

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV                      PIC XX.
          02 FILLER                              PIC X     VALUE "|".
          02 COD-PACI-ENV                        PIC X(15).
          02 FILLER                              PIC X     VALUE "|".
          02 TIPO-ID-PACI-ENV                    PIC X(3).
          02 FILLER                              PIC X     VALUE "|".
          02 LUGAR-ID-PACI-ENV                   PIC X(5).
          02 FILLER                              PIC X     VALUE "|".
          02 DESCRIP-PACI-ENV.
             03 LLAVE-APEL-PACI.
                05 1ER-APEL-PACI.
                   07 INICIAL-PACI               PIC X.
                   07 RESTO-PACI                 PIC X(14).
                05 2DO-APEL-PACI                 PIC X(15).
             03 NOMBRE-PACI.
                05 1ER-NOM-PACI                  PIC X(12).
                05 2DO-NOM-PACI                  PIC X(12).
          02 FILLER                              PIC X     VALUE "|". 
          02 NACIM-PACI-ENV.
             05 ANO-PACI                         PIC 9(4).
             05 MES-PACI                         PIC 99.
             05 DIA-PACI                         PIC 99.
          02 FILLER                              PIC X     VALUE "|".
          02 HEMOCLAS-PACI-ENV.
             05 GRP-SANG-PACI                    PIC XX.
             05 RH-PACI                          PIC X.
          02 FILLER                              PIC X     VALUE "|".
          02 SEXO-PACI-ENV                       PIC X.
          02 FILLER                              PIC X     VALUE "|".
          02 EST-CIV-PACI-ENV                    PIC X.
          02 FILLER                              PIC X     VALUE "|".
          02 NIV-ESTUD-PACI-ENV                  PIC X.
          02 FILLER                              PIC X     VALUE "|".
          02 ZONA-PACI-ENV                       PIC X.
          02 FILLER                              PIC X     VALUE "|".
          02 PADRE-PACI-ENV                      PIC X(30).
          02 FILLER                              PIC X     VALUE "|".
          02 MADRE-PACI-ENV                      PIC X(30).
          02 FILLER                              PIC X     VALUE "|".
          02 DIRECC-PACI-ENV                     PIC X(30).
          02 FILLER                              PIC X     VALUE "|". 
          02 TELEFONO-PACI-ENV                   PIC X(12).
          02 FILLER                              PIC X     VALUE "|". 
          02 CEL-PACI-ENV                        PIC X(12).
          02 FILLER                              PIC X     VALUE "|". 
          02 CIUDAD-PACI-ENV.
             05 DPTO-PACI                        PIC 99.
             05 CIUD-PACI                        PIC 9(3).
          02 FILLER                              PIC X     VALUE "|".
          02 NOMBRE-CIU-ENV.
             05 NOMBRE1-CIU                      PIC X(16).
             05 NOMBRE2-CIU                      PIC X(4).
          02 FILLER                              PIC X     VALUE "|".
          02 OCUP-V8-PACI-ENV                    PIC 9(4).
          02 FILLER                              PIC X     VALUE "|".
          02 NOMBRE-OCU-ENV.
             05 NOM1-OCU                         PIC X(40).
             05 NOM2-OCU                         PIC X(80).
          02 FILLER                              PIC X     VALUE "|". 
          02 PAIS-ORIG-PACI-ENV                  PIC X(3).
          02 FILLER                              PIC X     VALUE "|".
          02 ESTRATO-PACI-ENV                    PIC 9.
          02 FILLER                              PIC X     VALUE "|". 
          02 COPAGO-PACI-ENV                     PIC X.
          02 FILLER                              PIC X     VALUE "|". 
          02 TIPO-PACI-ENV                       PIC X.
          02 FILLER                              PIC X     VALUE "|".
          02 INSTITUTO-PACI-ENV                  PIC X(12).
          02 FILLER                              PIC X     VALUE "|".
          02 DESCRIP-COLEG-ENV                   PIC X(50).
          02 FILLER                              PIC X     VALUE "|". 
          02 ETNIA-PACI-ENV                      PIC X.
          02 FILLER                              PIC X     VALUE "|". 
          02 TIPO-AFIL-PACI-ENV                  PIC 9.
          02 FILLER                              PIC X     VALUE "|". 
          02 PORTABILIDAD-PACI-ENV               PIC X.
          02 FILLER                              PIC X     VALUE "|".  
          02 CIUDAD-ASEG-PACI-ENV                PIC 9(5).
          02 FILLER                              PIC X     VALUE "|". 
          02 EPS-PACI-ENV.
             05 EPS1-PACI                        PIC X(3).
             05 EPS2-PACI                        PIC X(3). 
          02 FILLER                              PIC X     VALUE "|".
          02 NOMBRE-ENT-ENV.
             05 NOMBRE1-ENT                      PIC X(30).
             05 NOMBRE2-ENT                      PIC X(40).
          02 FILLER                              PIC X     VALUE "|".
          02 CONTRATO-PACI-ENV                   PIC X(15).
          02 FILLER                              PIC X     VALUE "|".
          02 FECHA-AFIL-PACI-ENV.
             05 ANO-AFIL-PACI                    PIC 9(4).
             05 MES-AFIL-PACI                    PIC 99.
             05 DIA-AFIL-PACI                    PIC 99.
          02 FILLER                              PIC X     VALUE "|". 
          02 FICHA-PACI-ENV                      PIC X(6).
          02 FILLER                              PIC X     VALUE "|".
          02 NRO-AFIL-PACI-ENV                   PIC X(15).
          02 FILLER                              PIC X     VALUE "|".
          02 FECHA-VENCE-PACI-ENV.
             05 ANO-VENCE-PACI                   PIC 9(4).
             05 MES-VENCE-PACI                   PIC 99.
             05 DIA-VENCE-PACI                   PIC 99.
          02 FILLER                              PIC X     VALUE "|".
          02 FECHA-DEMAN-INDU-PACI-ENV.
             05 ANO-DEMAN-INDU-PACI              PIC 9(4).
             05 MES-DEMAN-INDU-PACI              PIC 99.
             05 DIA-DEMAN-INDU-PACI              PIC 99.
          02 FILLER                              PIC X     VALUE "|". 
          02 DEMAN-INDU-PACI-ENV                 PIC X.
          02 FILLER                              PIC X     VALUE "|".
          02 ID-COTIZ-PACI-ENV                   PIC X(15).
          02 FILLER                              PIC X     VALUE "|".
          02 DESCRIP-COTIZANTE-ENV.               
             03 LLAVE-APEL-PACI.
                05 1ER-APEL-PACI.
                   07 INICIAL-PACI               PIC X.
                   07 RESTO-PACI                 PIC X(14).
                05 2DO-APEL-PACI                 PIC X(15).
             03 NOMBRE-PACI.
                05 1ER-NOM-PACI                  PIC X(12).
                05 2DO-NOM-PACI                  PIC X(12).
          02 FILLER                              PIC X     VALUE "|". 
          02 PARENT-PACI-ENV                     PIC 99.
          02 FILLER                              PIC X     VALUE "|". 
          02 VICTI-CONFLICTO-PACI-ENV            PIC X.
          02 FILLER                              PIC X     VALUE "|". 
          02 PROG-ESP-PACI-ENV                   PIC X.
          02 FILLER                              PIC X     VALUE "|".
          02 ALT-COS-PACI-ENV                    PIC X.
          02 FILLER                              PIC X     VALUE "|". 
          02 TUTELA-PACI-ENV                     PIC X.
          02 FILLER                              PIC X     VALUE "|". 
          02 EMPRESA-PACI-ENV                    PIC X(30).
          02 FILLER                              PIC X     VALUE "|". 
          02 CRONICO-PACI-ENV                    PIC X.
          02 FILLER                              PIC X     VALUE "|". 
          02 PATOL-CRONIC-PACI-ENV               PIC 9(3).
          02 FILLER                              PIC X     VALUE "|".
          02 CLASIF-PACI-ENV                     PIC XX.
          02 FILLER                              PIC X     VALUE "|". 
          02 ACOMPA-PACI-ENV                     PIC X(30).
          02 FILLER                              PIC X     VALUE "|". 
          02 TEL-ACOM-PACI-ENV                   PIC X(10).
          02 FILLER                              PIC X     VALUE "|". 
          02 CERT-ESTUD-PACI-ENV.
             05 ANO-CERT-ESTUD-PACI              PIC 9(4).
             05 MES-CERT-ESTUD-PACI              PIC 99.
          02 FILLER                              PIC X     VALUE "|".
          02 PERI-ESTUD-PACI-ENV                 PIC 9.
          02 FILLER                              PIC X     VALUE "|".
          02 ULT-MAMO-PACI-ENV.
             05 ANO-MAMO-PACI                    PIC 9(4).
             05 MES-MAMO-PACI                    PIC 99.
          02 FILLER                              PIC X     VALUE "|". 
          02 CERT-ECONO-PACI-ENV.
             05 ANO-CERT-ECO-PACI                PIC 9(4).
             05 MES-CERT-ECO-PACI                PIC 99.
          02 FILLER                              PIC X     VALUE "|". 
          02 PERI-ECO-PACI-ENV                   PIC 9.
          02 FILLER                              PIC X     VALUE "|". 
          02 MULTICONSUL-PACI-ENV                PIC X.
          02 FILLER                              PIC X     VALUE "|". 
          02 RESTRIC-PACI-ENV.
             05 REST-APLI-PACI                   PIC X.
             05 REST-DROG-PACI                   PIC X.
             05 REST-CIRU-PACI                   PIC X.
             05 REST-LABO-PACI                   PIC X.
             05 REST-IMAG-PACI                   PIC X.
             05 REST-ESTA-PACI                   PIC X.
             05 REST-CONS-PACI                   PIC X.
             05 REST-TERF-PACI                   PIC X.
             05 REST-TERO-PACI                   PIC X.
             05 REST-ODON-PACI                   PIC X.
             05 REST-PYP-PACI                    PIC X.
          02 FILLER                              PIC X     VALUE "|".
          02 VICT-ABUSO-SEX-PACI-ENV             PIC X.
          02 FILLER                              PIC X     VALUE "|".
          02 DERECHO-PACI-ENV                    PIC X.
          02 FILLER                              PIC X     VALUE "|". 
          02 OBSERV-PACI-ENV                     PIC X(50).
          02 FILLER                              PIC X     VALUE "|". 
          02 DISCAP-PACI-ENV                     PIC X.
          02 FILLER                              PIC X     VALUE "|".
          02 EMB-ALTO-RIESG-PACI-ENV             PIC X.
          02 FILLER                              PIC X     VALUE "|".
          02 NIT-FACT-PACI-ENV                   PIC 9(10).
          02 FILLER                              PIC X     VALUE "|".
          02 DESCRIP-TER-ENV.
             03 DESCRIP1-TER.
                05 INI1-TER                      PIC X.
                05 INI2-TER                      PIC X(29).
             03 DESCRIP2-TER                     PIC X(20).             
          02 FILLER                              PIC X     VALUE "|".
          02 FECHA-NIT-PACI-ENV.    
             03 ANO-NIT-PACI                     PIC 9(4).
             03 MES-NIT-PACI                     PIC 99.
             03 DIA-NIT-PACI                     PIC 99.
          02 FILLER                              PIC X     VALUE "|".
          02 ANTECED-CANCER-PACI-ENV             PIC X.
          02 FILLER                              PIC X     VALUE "|".
          02 MED-FAMI-PACI-ENV                   PIC 9(10).
          02 FILLER                              PIC X     VALUE "|".
      *    02 DESCRIP-PROF-ENV. 
      *       03 INI1-PROF                        PIC X.
      *       03 INI2-PROF                        PIC X(29).
                           
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
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USU-W                TO MSJ2-HTML
              MOVE "INV103-15"              TO MSJ3-HTML
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
              MOVE "SER110C_03"              TO MSJ3-HTML
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
              MOVE "SER110C_03"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.


       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-PACIE-W               TO MSJ2-HTML
              MOVE "SER110C_03"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CIUDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-CIUD-W                TO MSJ2-HTML
              MOVE "SER110C_03"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-COLEGIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-COLE-W               TO MSJ2-HTML
              MOVE "SER110C_03"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-OCUPACIONES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-OCUP-W               TO MSJ2-HTML
              MOVE "SER110C_03"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ENTIDADES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-ENTI-W               TO MSJ2-HTML
              MOVE "SER110C_03"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TERCEROS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-TER-W                TO MSJ2-HTML
              MOVE "SER110C_03"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON EXTENSION-PROFESIONALES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-PROF-W                TO MSJ2-HTML
              MOVE "SER110C_03"              TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA    TO LLEGADA-W.

           UNSTRING LLEGADA-W DELIMITED BY "|"
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, COD-PACI-LLEGADA-W     
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.


       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
          
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.

       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
      
           OPEN INPUT ARCHIVO-USUNET
      
           INITIALIZE LLAVE-USUNET.

       LEER-USUARIO.           
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
           CLOSE ARCHIVO-USUNET.
      
           IF NOMBRE-USUNET = SPACES
              MOVE "Validacion de usuarios"      TO MSJ1-HTML
              MOVE "Falta configurar usuario"    TO MSJ2-HTML
              MOVE "Sc"                          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
 
       ASIGNAR-NOMBRES.                  
         
           INITIALIZE NOM-USUAR-W 
                      NOM-PACIE-W
                      NOM-CIUD-W
                      NOM-OCUP-W
                      NOM-ENTI-W
                      NOM-TER-W
                      NOM-COLE-W


           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
 
           MOVE "\\" TO NOM-USUAR-W 
                        NOM-PACIE-W
                        NOM-CIUD-W
                        NOM-OCUP-W
                        NOM-ENTI-W
                        NOM-TER-W
                        NOM-COLE-W


           INSPECT NOM-PACIE-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET
           INSPECT NOM-PACIE-W REPLACING FIRST " "
                                          BY "\"          
           INSPECT NOM-PACIE-W REPLACING FIRST "                              "
                                          BY DIR-CONTAB-LLEGADA-W 

           MOVE NOM-PACIE-W TO  NOM-TER-W
                                NOM-PROF-W
                                  

           INSPECT NOM-PACIE-W REPLACING FIRST "                     "
                                            BY "\CONTROL\SC-PACIE.DAT".

           INSPECT NOM-TER-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-ARCHTER.DAT".

           INSPECT NOM-PROF-W REPLACING FIRST "                         "
                                           BY "\CONTROL\SC-ARCHPROF2.DAT".                                         

           INSPECT NOM-CIUD-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET
           INSPECT NOM-CIUD-W REPLACING FIRST " "
                                          BY "\"
           MOVE NOM-CIUD-W TO NOM-OCUP-W
                              NOM-ENTI-W
                              NOM-COLE-W

                                                                    
           INSPECT NOM-CIUD-W REPLACING FIRST "                          "
                                           BY "PROG\DATOS\SC-ARCHCIUD.DAT".

           INSPECT NOM-OCUP-W REPLACING FIRST "                         "
                                           BY "PROG\DATOS\SC-ARCHOCU.DAT".

           INSPECT NOM-ENTI-W REPLACING FIRST "                         "
                                           BY "PROG\DATOS\SC-ARCHENT.DAT".

           INSPECT NOM-COLE-W REPLACING FIRST "                       "
                                           BY "PROG\DATOS\SC-COLEG.DAT".


           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET
           INSPECT NOM-USUAR-W REPLACING FIRST " "
                                            BY "\"          
           INSPECT NOM-USUAR-W REPLACING FIRST "                              "
                                            BY DIR-CONTAB-LLEGADA-W 
           INSPECT NOM-USUAR-W REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHUSU.DAT".          

       ABRIR-ARCHIVOS.
         
           OPEN INPUT ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.

           OPEN INPUT ARCHIVO-PACIENTES
                      ARCHIVO-CIUDAD
                      ARCHIVO-OCUPACIONES
                      ARCHIVO-ENTIDADES
                      ARCHIVO-TERCEROS
                      EXTENSION-PROFESIONALES
                      ARCHIVO-COLEGIOS.

           MOVE COD-PACI-LLEGADA-W  TO COD-PACI
           MOVE "00"               TO SW-INVALID

           READ ARCHIVO-PACIENTES WITH NO LOCK 
                INVALID KEY
                  MOVE "01" TO SW-INVALID                  
           END-READ.

           MOVE SW-INVALID              TO SW-INVALID-ENV
           MOVE COD-PACI                TO COD-PACI-ENV
           MOVE TIPO-ID-PACI            TO TIPO-ID-PACI-ENV
           MOVE LUGAR-ID-PACI           TO LUGAR-ID-PACI-ENV 
           MOVE DESCRIP-PACI            TO DESCRIP-PACI-ENV 
           MOVE NACIM-PACI              TO NACIM-PACI-ENV
           MOVE HEMOCLAS-PACI           TO HEMOCLAS-PACI-ENV 
           MOVE SEXO-PACI               TO SEXO-PACI-ENV 
           MOVE EST-CIV-PACI            TO EST-CIV-PACI-ENV 
           MOVE NIV-ESTUD-PACI          TO NIV-ESTUD-PACI-ENV
           MOVE ZONA-PACI               TO ZONA-PACI-ENV
           MOVE PADRE-PACI              TO PADRE-PACI-ENV
           MOVE MADRE-PACI              TO MADRE-PACI-ENV 
           MOVE DIRECC-PACI             TO DIRECC-PACI-ENV 
           MOVE TELEFONO-PACI           TO TELEFONO-PACI-ENV 
           MOVE CEL-PACI                TO CEL-PACI-ENV 
           MOVE CIUDAD-PACI             TO CIUDAD-PACI-ENV
    
           MOVE CIUDAD-PACI-ENV         TO COD-CIU
           READ ARCHIVO-CIUDAD WITH NO LOCK
                INVALID KEY 
                   MOVE COD-CIU         TO NOMBRE-CIU
           END-READ

           MOVE NOMBRE-CIU              TO NOMBRE-CIU-ENV
 
           MOVE OCUP-V8-PACI             TO OCUP-V8-PACI-ENV

           MOVE OCUP-V8-PACI-ENV         TO COD-OCU
           READ ARCHIVO-OCUPACIONES WITH NO LOCK
                INVALID KEY 
                   MOVE COD-OCU         TO NOMBRE-OCU
           END-READ

           MOVE NOMBRE-OCU              TO NOMBRE-OCU-ENV
 
           MOVE PAIS-ORIG-PACI          TO PAIS-ORIG-PACI-ENV
           MOVE ESTRATO-PACI            TO ESTRATO-PACI-ENV 
           MOVE COPAGO-PACI             TO COPAGO-PACI-ENV 
           MOVE TIPO-PACI               TO TIPO-PACI-ENV
           MOVE INSTITUTO-PACI          TO INSTITUTO-PACI-ENV

           MOVE INSTITUTO-PACI-ENV      TO LLAVE-COLEG
           READ ARCHIVO-COLEGIOS WITH NO LOCK
                INVALID KEY 
                   MOVE LLAVE-COLEG         TO DESCRIP-COLEG 
           END-READ
           MOVE DESCRIP-COLEG              TO DESCRIP-COLEG-ENV 
 
           MOVE ETNIA-PACI              TO ETNIA-PACI-ENV 
           MOVE TIPO-AFIL-PACI          TO TIPO-AFIL-PACI-ENV 
           MOVE PORTABILIDAD-PACI       TO PORTABILIDAD-PACI-ENV  
           MOVE CIUDAD-ASEG-PACI        TO CIUDAD-ASEG-PACI-ENV 
           MOVE EPS-PACI                TO EPS-PACI-ENV

           MOVE EPS-PACI-ENV            TO COD-ENT 
           READ ARCHIVO-ENTIDADES WITH NO LOCK
                INVALID KEY 
                   MOVE COD-ENT         TO NOMBRE-ENT 
           END-READ
           MOVE NOMBRE-ENT              TO NOMBRE-ENT-ENV

           MOVE CONTRATO-PACI           TO CONTRATO-PACI-ENV 
           MOVE FECHA-AFIL-PACI         TO FECHA-AFIL-PACI-ENV 
           MOVE FICHA-PACI              TO FICHA-PACI-ENV
           MOVE NRO-AFIL-PACI           TO NRO-AFIL-PACI-ENV
           MOVE FECHA-VENCE-PACI        TO FECHA-VENCE-PACI-ENV
           MOVE FECHA-DEMAN-INDU-PACI   TO FECHA-DEMAN-INDU-PACI-ENV 
           MOVE DEMAN-INDU-PACI         TO DEMAN-INDU-PACI-ENV 
      
           MOVE ID-COTIZ-PACI           TO ID-COTIZ-PACI-ENV

           MOVE ID-COTIZ-PACI-ENV       TO COD-PACI
           READ ARCHIVO-PACIENTES WITH NO LOCK
                INVALID KEY 
                   MOVE COD-PACI        TO DESCRIP-PACI
           END-READ
           MOVE DESCRIP-PACI            TO DESCRIP-COTIZANTE-ENV

           MOVE PARENT-PACI             TO PARENT-PACI-ENV 
           MOVE VICTI-CONFLICTO-PACI    TO VICTI-CONFLICTO-PACI-ENV 
           MOVE PROG-ESP-PACI           TO PROG-ESP-PACI-ENV
           MOVE ALT-COS-PACI            TO ALT-COS-PACI-ENV 
           MOVE TUTELA-PACI             TO TUTELA-PACI-ENV 
           MOVE EMPRESA-PACI            TO EMPRESA-PACI-ENV 
           MOVE CRONICO-PACI            TO CRONICO-PACI-ENV 
           MOVE PATOL-CRONIC-PACI       TO PATOL-CRONIC-PACI-ENV
           MOVE CLASIF-PACI             TO CLASIF-PACI-ENV 
           MOVE ACOMPA-PACI             TO ACOMPA-PACI-ENV 
           MOVE TEL-ACOM-PACI           TO TEL-ACOM-PACI-ENV 
           MOVE CERT-ESTUD-PACI         TO CERT-ESTUD-PACI-ENV
           MOVE PERI-ESTUD-PACI         TO PERI-ESTUD-PACI-ENV
           MOVE ULT-MAMO-PACI           TO ULT-MAMO-PACI-ENV 
           MOVE CERT-ECONO-PACI         TO CERT-ECONO-PACI-ENV 
           MOVE PERI-ECO-PACI           TO PERI-ECO-PACI-ENV 
           MOVE MULTICONSUL-PACI        TO MULTICONSUL-PACI-ENV 
           MOVE RESTRIC-PACI            TO RESTRIC-PACI-ENV
           MOVE VICT-ABUSO-SEX-PACI     TO VICT-ABUSO-SEX-PACI-ENV
           MOVE DERECHO-PACI            TO DERECHO-PACI-ENV 
           MOVE OBSERV-PACI             TO OBSERV-PACI-ENV 
           MOVE DISCAP-PACI             TO DISCAP-PACI-ENV
           MOVE EMB-ALTO-RIESG-PACI     TO EMB-ALTO-RIESG-PACI-ENV
           MOVE NIT-FACT-PACI           TO NIT-FACT-PACI-ENV

           MOVE NIT-FACT-PACI-ENV       TO COD-TERCERO
           READ ARCHIVO-TERCEROS WITH NO LOCK
                INVALID KEY 
                   MOVE COD-TERCERO     TO DESCRIP-TER
           END-READ
           MOVE DESCRIP-TER             TO DESCRIP-TER-ENV
           MOVE FECHA-NIT-PACI          TO FECHA-NIT-PACI-ENV
           MOVE ANTECED-CANCER-PACI     TO ANTECED-CANCER-PACI-ENV
           MOVE MED-FAMI-PACI           TO MED-FAMI-PACI-ENV.

      *     MOVE MED-FAMI-PACI-ENV       TO COD-PROF
      *     READ ARCHIVO-PROFESIONALES WITH NO LOCK
      *          INVALID KEY 
      *             MOVE COD-PROF        TO DESCRIP-PROF
      *     END-READ
      *     MOVE DESCRIP-PROF            TO DESCRIP-PROF-ENV. 
                

       CERRAR-MAESTRO-ARTICULOS.           
           CLOSE ARCHIVO-PACIENTES.
               
       PAGINA-CONFIG.	 
           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO      TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3
         
           MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
           PERFORM ABRIR-HTML.

       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.
      
              
       COPY "..\..\FUENTES\SC-WEB19.CBL".
