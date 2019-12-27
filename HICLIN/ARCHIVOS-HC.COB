000012* VALIDA ARCHIVOS HISTORIAS CLINICAS
000020 IDENTIFICATION DIVISION.
000021* PROGRAM-ID. "ARCHIVOS-HC".
000022
000023 PROGRAM-ID. "HttpExtensionProc".
000024 ENVIRONMENT DIVISION.
000025 CONFIGURATION SECTION.
000026 INPUT-OUTPUT SECTION.
000027 FILE-CONTROL.
000080 COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
000150 COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-USUAR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS OTR-STAT.


           SELECT ARCHIVO-RESTRICCION LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-RESTR-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY LLAVE-REST 
                  ALTERNATE RECORD KEY NOMBRE-REST WITH DUPLICATES
                  FILE STATUS OTR-STAT.


           SELECT ARCHIVO-PROFESIONALES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PROF-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PROF
                  ALTERNATE RECORD KEY DESCRIP-PROF WITH DUPLICATES
                  ALTERNATE RECORD KEY ATIENDE-PROF  WITH DUPLICATES
                  ALTERNATE RECORD KEY SUCURSAL-PROF WITH DUPLICATES
                  FILE STATUS OTR-STAT.

           SELECT REGCONT LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-FECHA-W
                  FILE STATUS OTR-STAT.

           SELECT ARCHIVO-FORMULA-ORD
                  ASSIGN NOM-FORMU-ORD-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-ORD
                  ALTERNATE RECORD KEY DESCRIP-ORD  WITH DUPLICATES
                  FILE STATUS OTR-STAT.

           SELECT ARCHIVO-TRIAGE LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TRIAG-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-TRIA
                  ALTERNATE RECORD KEY IS ID-TRIA WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FOLIO-CONSUL-TRIA WITH DUPLICATES
                  FILE STATUS OTR-STAT.

           SELECT ARCHIVO-NOPOS LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-NOPOS-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-NOPOS 
                  ALTERNATE RECORD KEY IS FOLIO-NOPOS WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FECHA-NOPOS WITH DUPLICATES
                  ALTERNATE RECORD KEY IS MED-NOPOS WITH DUPLICATES
                  FILE STATUS OTR-STAT.


           SELECT ARCHIVO-CAMAS-HOSP LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-CAMAS-HOSP-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS COD-CAM
                  ALTERNATE RECORD KEY IS UNSERV-CAM WITH DUPLICATES
                  ALTERNATE RECORD KEY IS SERVHO-CAM WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FACT-CAM WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PACI-CAM WITH DUPLICATES
                  ALTERNATE RECORD KEY IS NOM-PACI-CAM WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FOLIO-CAM WITH DUPLICATES
                  FILE STATUS OTR-STAT.


           SELECT ARCHIVO-CONTRAREFER LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-CONTRA-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-REF
                  ALTERNATE RECORD KEY IS FOLIO-REF WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FECHA-REF WITH DUPLICATES
                  ALTERNATE RECORD KEY IS MED-REF   WITH DUPLICATES
                  FILE STATUS OTR-STAT.


           SELECT PEDIDOS-HISTORIA LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PED-HC-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-PED
                  ALTERNATE RECORD KEY IS FECHA-PED WITH DUPLICATES
                  ALTERNATE RECORD KEY IS MED-PED WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LLAVE-ALT-PED WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FOLIO-PED WITH DUPLICATES
                  FILE STATUS OTR-STAT.


           SELECT ARCHIVO-PARTOGRAMA LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-PARTOG-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-PARTO
                  ALTERNATE RECORD KEY IS FOLIO-PARTO  DUPLICATES
                  ALTERNATE RECORD KEY IS FECHA-PARTO  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ESTADO-PARTO WITH DUPLICATES
                  ALTERNATE RECORD KEY IS MED-PARTO WITH DUPLICATES
                  FILE STATUS OTR-STAT.


           SELECT ARCHIVO-INTERCONSULTA LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-INTC-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-INTCONS
                  ALTERNATE RECORD KEY IS
                  FECHA-ING-INTCONS WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ESPEC-INTCONS WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FOLIO-INTCONS WITH DUPLICATES
                  FILE STATUS OTR-STAT.


           SELECT DOCUMENTOS-HISTORIA LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-DOC-HC-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-EPI
                  ALTERNATE RECORD KEY IS FOLIO-EPI WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FECHA-EPI WITH DUPLICATES
                  ALTERNATE RECORD KEY IS MED-EPI WITH DUPLICATES
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
                  ALTERNATE RECORD KEY IS LLAVE-FOLIO-DETHC WITH DUPLICATES
                  FILE STATUS OTR-STAT.


           SELECT EVOLUCION-HISTORIA LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-HCEVO-W
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


           SELECT HISTORIA-CLAP LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-CLAP-HC-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC;
                  RECORD KEY   IS LLAVE-CLAP 
                  ALTERNATE RECORD KEY FECHA-CLAP WITH DUPLICATES
                  ALTERNATE RECORD KEY MED-ABRE-CLAP WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT BITACORA-CAMAS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-BIT-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS LLAVE-BC
                  ALTERNATE RECORD KEY IS LLAVE-FECHA-BC WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PACI-CAM-BC   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS OPER-MOD-CAM-BC WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

	       SELECT ARCHIVO-ENFERMEDAD LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-ENFER-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC	
                  RECORD KEY   IS LLAVE-ENF
                  ALTERNATE RECORD KEY IS NOMBRE-ENF WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.
			

           SELECT ARCHIVO-MACRO-EVOL LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-MACR-EVOL-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-MACROEVOL
                  ALTERNATE RECORD KEY DETALLE-MACROEVOL WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-MACRO-EVOL-EXT LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-MACRE-EVOL-EXT-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-MACROEVOL-EXT
                  FILE STATUS IS OTR-STAT.


000400 DATA DIVISION.
000410 FILE SECTION.
000420 COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
000421 COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
000450 COPY "P:\PROG\PROYECT\FUENTES\FD-USUAR.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-RESTR.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-PROFE.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-FECHA.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-FOROM.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-TRIAG17.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-NOPOS17.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-CAMAS17.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-HCREF17.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-HCPED17.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-PARTOGRAMA17.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-INTCONS17.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-HCEPI17.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-HCAPER17.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-HCDETAL17.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-HCEVO17.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-DETEVO17.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-CLAP.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-BITACORA-CAMAS.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-ENFER.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-MACREV.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-MACREV-EXT.CBL".


000540 WORKING-STORAGE  SECTION.
000550 COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
000560 COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC19.CBL".

       77 NOM-USUAR-W                PIC X(70).
       77 NOM-RESTR-W                PIC X(70).
       77 NOM-PROF-W                 PIC X(70).
       77 NOM-FECHA-W                PIC X(70).
       77 NOM-FORMU-ORD-W            PIC X(70).
       77 NOM-TRIAG-W                PIC X(70).
       77 NOM-NOPOS-W                PIC X(70).
       77 NOM-CAMAS-HOSP-W           PIC X(70).
       77 NOM-CONTRA-W               PIC X(70).
       77 NOM-PED-HC-W               PIC X(70).
       77 NOM-PARTOG-W               PIC X(70).
       77 NOM-INTC-W                 PIC X(70).
       77 NOM-DOC-HC-W               PIC X(70).
       77 NOM-HCAPE-W                PIC X(70).
       77 NOM-HCDET-W                PIC X(70).
       77 NOM-HCEVO-W                PIC X(70).
       77 NOM-DETEVO-W               PIC X(70).
       77 NOM-CLAP-HC-W              PIC X(70).
       77 NOM-BIT-W                  PIC X(70).
       77 NOM-ENFER-W                PIC X(70).
       77 NOM-MACR-EVOL-W            PIC X(70).
       77 NOM-MACRE-EVOL-EXT-W       PIC X(70).
       77 CONT-W                     PIC 9.
       77 ANO-X                      PIC 99.
       77 MES-EDIT                   PIC X(5).
       77 PORC-CREE-EDIT             PIC Z.ZZ.

       01 FECHA-ACT-W.
          02 ANO-ACT-W               PIC 99.
          02 MES-ACT-W               PIC 99.
          02 DIA-ACT-W               PIC 99.

       01 ULT-FECHA-NUM.
          02 ULT-ANO-NUM             PIC 99.
          02 ULT-MES-NUM             PIC 99.
          02 ULT-DIA-NUM             PIC 99.


       01 LINEA-LLEGADA-W             PIC X(100).



       01 DATOS-ENVIO.
          02 DAT0-ENV                PIC XX.
          02 FILLER                  PIC X VALUE "|".
          02 DAT1-ENV                PIC X(10).
          02 FILLER                  PIC X VALUE "|".
          02 TABLA-PROF-ENV.
             03 TBLA-ESP-PROF-ENV    OCCURS 10.
                05 ESP-PROF-ENV      PIC X(3).
                05 FILLER            PIC X VALUE ";".
          02 FILLER                  PIC X VALUE "|".

000791 LINKAGE SECTION.
000792 COPY "P:\PROG\PROYECT\FUENTES\ISAPICTX.CBL".
000793
000794 PROCEDURE DIVISION WITH stdcall LINKAGE USING ISAPI-CTX-CNT.
000795
000810 DECLARATIVES.
000811 I-O-TEST SECTION.
000812     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUNET.         
000813 ESCR-EXCEPTIONES.
000814     IF OTR-STAT = "00"
000815        CONTINUE 
000816     ELSE
000817        MOVE OTR-STAT                TO MSJ1-HTML
000818        MOVE NOM-USU-W               TO MSJ2-HTML
000819        MOVE "ARCHIVOS-HC"           TO MSJ3-HTML
000820        GO TO ENVIAR2-ERROR
000821     END-IF.
001030
001040 I-O-TEST SECTION.
001050     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.         
001060 ESCR-EXCEPTIONES.
001070     IF OTR-STAT = "00"
001080        CONTINUE
001090     ELSE
001100        MOVE OTR-STAT                TO MSJ1-HTML
001101        MOVE NOM-SESION-W            TO MSJ2-HTML
001102        MOVE "ARCHIVOS-HC"           TO MSJ3-HTML
001103        GO TO ENVIAR2-ERROR
001104     END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                TO MSJ1-HTML
              MOVE NOM-USUAR-W             TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"           TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-RESTRICCION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                TO MSJ1-HTML
              MOVE NOM-RESTR-W             TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"           TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PROFESIONALES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                TO MSJ1-HTML
              MOVE NOM-PROF-W              TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"           TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON REGCONT.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT               TO MSJ1-HTML
              MOVE NOM-FECHA-W            TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-FORMULA-ORD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT               TO MSJ1-HTML
              MOVE NOM-FORMU-ORD-W        TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TRIAGE.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT               TO MSJ1-HTML
              MOVE NOM-TRIAG-W            TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-NOPOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT               TO MSJ1-HTML
              MOVE NOM-NOPOS-W            TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CAMAS-HOSP.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT               TO MSJ1-HTML
              MOVE NOM-CAMAS-HOSP-W       TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CONTRAREFER.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT               TO MSJ1-HTML
              MOVE NOM-CONTRA-W           TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON PEDIDOS-HISTORIA.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT               TO MSJ1-HTML
              MOVE NOM-PED-HC-W           TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PARTOGRAMA.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT               TO MSJ1-HTML
              MOVE NOM-PARTOG-W           TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-INTERCONSULTA.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT               TO MSJ1-HTML
              MOVE NOM-INTC-W             TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON DOCUMENTOS-HISTORIA.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT               TO MSJ1-HTML
              MOVE NOM-DOC-HC-W           TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON HISTORIA-CLINICA.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT               TO MSJ1-HTML
              MOVE NOM-HCAPE-W            TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.


       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON HISTORIA-DETALLADA.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT               TO MSJ1-HTML
              MOVE NOM-HCDET-W            TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

        I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON EVOLUCION-HISTORIA.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT               TO MSJ1-HTML
              MOVE NOM-HCEVO-W            TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

        I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON EVOLUCION-DETALLADA.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT               TO MSJ1-HTML
              MOVE NOM-DETEVO-W           TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.


         I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON HISTORIA-CLAP.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT               TO MSJ1-HTML
              MOVE NOM-CLAP-HC-W          TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

         I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON BITACORA-CAMAS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT               TO MSJ1-HTML
              MOVE NOM-BIT-W              TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

         I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ENFERMEDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT               TO MSJ1-HTML
              MOVE NOM-ENFER-W            TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

         I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-MACRO-EVOL.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT               TO MSJ1-HTML
              MOVE NOM-MACR-EVOL-W        TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

         I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-MACRO-EVOL-EXT.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT               TO MSJ1-HTML
              MOVE NOM-MACRE-EVOL-EXT-W   TO MSJ2-HTML
              MOVE "ARCHIVOS-HC"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

001150 END DECLARATIVES.
001160
001161 INICIAR-IIS.
001162     MOVE LOW-VALUE TO COBW3.
001163     MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
001164     CALL "COBW3_INIT" USING COBW3.
001220
001221 LEER-DATO-HTML.
001852     MOVE "datosh" TO COBW3-SEARCH-DATA.
001853     CALL "COBW3_GET_VALUE" USING COBW3.
001854     MOVE COBW3-GET-DATA    TO LINEA-LLEGADA-W.

           INITIALIZE DATO-LLEGADA-W
           UNSTRING LINEA-LLEGADA-W DELIMITED BY "|"
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W, ADMIN-W
           END-UNSTRING.

           MOVE SESION-LLEGAD-W   TO LLAVE-SESION-W.

001238 INICIAR-SESION.
001245     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
001246
000820     MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
000830     ACCEPT HORA-TOTAL FROM TIME.
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.

001303 ABRIR-USUARIO.
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
001316                  MOVE "Validacion de usuarios"      TO MSJ1-HTML
001317                  MOVE "Falta configurar usuario"    TO MSJ2-HTML
001318                  MOVE "Sc"                          TO MSJ3-HTML
001319                  GO TO ENVIAR2-ERROR
001320       WHEN OTHER GO TO CERRAR-SESION
001321     END-EVALUATE.
001322
001332     INITIALIZE REG-USUNET.

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

       INICIAR-RESTRICION.
           ACCEPT ULT-FECHA-NUM FROM DATE.

           EVALUATE ULT-MES-NUM
               WHEN 01 MOVE "\ENE\"    TO MES-EDIT
               WHEN 02 MOVE "\FEB\"    TO MES-EDIT
               WHEN 03 MOVE "\MAR\"    TO MES-EDIT
               WHEN 04 MOVE "\ABR\"    TO MES-EDIT
               WHEN 05 MOVE "\MAY\"    TO MES-EDIT
               WHEN 06 MOVE "\JUN\"    TO MES-EDIT
               WHEN 07 MOVE "\JUL\"    TO MES-EDIT
               WHEN 08 MOVE "\AGT\"    TO MES-EDIT
               WHEN 09 MOVE "\SEP\"    TO MES-EDIT
               WHEN 10 MOVE "\OCT\"    TO MES-EDIT
               WHEN 11 MOVE "\NOV\"    TO MES-EDIT
               WHEN 12 MOVE "\DIC\"    TO MES-EDIT
           END-EVALUATE 


           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
001614     INITIALIZE NOM-USUAR-W NOM-FORMU-ORD-W
001615     MOVE "\\" TO NOM-USUAR-W NOM-FORMU-ORD-W
001616
001617     INSPECT NOM-USUAR-W REPLACING FIRST "                    "
001618                                    BY IP-DATOS-USUNET
001621     INSPECT NOM-USUAR-W REPLACING FIRST " "
001624                                     BY "\"
007292     INSPECT NOM-USUAR-W REPLACING FIRST "                              "
007293                                     BY DIRECTORIO-LLEGAD-W

           MOVE NOM-USUAR-W    TO NOM-PROF-W NOM-FECHA-W


007190     INSPECT NOM-USUAR-W   REPLACING FIRST "                       "
007200                                        BY "\CONTROL\SC-ARCHUSU.DAT".

           INSPECT NOM-PROF-W    REPLACING FIRST "                        "
                                              BY "\CONTROL\SC-ARCHPROF.DAT".

           INSPECT NOM-FECHA-W   REPLACING FIRST "     "
                                              BY MES-EDIT

           INSPECT NOM-FECHA-W   REPLACING FIRST "              "
                                              BY "SC-REGCONT.DAT".


      * "PROG\DATOS"

           INSPECT NOM-FORMU-ORD-W REPLACING FIRST "                    "
                                                BY IP-DATOS-USUNET

           MOVE NOM-FORMU-ORD-W   TO NOM-TRIAG-W NOM-NOPOS-W NOM-CAMAS-HOSP-W NOM-CONTRA-W  NOM-PED-HC-W 
                                     NOM-PARTOG-W NOM-INTC-W NOM-DOC-HC-W NOM-CLAP-HC-W NOM-BIT-W NOM-ENFER-W 
                                     NOM-MACR-EVOL-W NOM-MACRE-EVOL-EXT-W NOM-HCAPE-W DIRECTORIO-LLEGAD-W NOM-RESTR-W

           INSPECT NOM-RESTR-W   REPLACING FIRST "                          "
                                              BY "\PROG\DATOS\SC-ARCHREST.SC".

           INSPECT NOM-FORMU-ORD-W REPLACING FIRST "                           "
                                                BY "\PROG\DATOS\SC-ORDMEDIC.DAT".

           INSPECT NOM-TRIAG-W  REPLACING FIRST "                        "
                                             BY "\PROG\DATOS\SC-TRIAG.S17".

           INSPECT NOM-NOPOS-W  REPLACING FIRST "                        "
                                             BY "\PROG\DATOS\SC-NOPOS.S17".

           INSPECT NOM-CAMAS-HOSP-W REPLACING FIRST "                        "
                                                 BY "\PROG\DATOS\SC-CAMAS.S17".

           INSPECT NOM-CONTRA-W REPLACING FIRST "                        "
                                             BY "\PROG\DATOS\SC-REFER.S17".

           INSPECT NOM-PED-HC-W REPLACING FIRST "                        "
                                             BY "\PROG\DATOS\SC-PEDID.S17".

           INSPECT NOM-PARTOG-W REPLACING FIRST "                        "
                                             BY "\PROG\DATOS\SC-PARTO.S17".

           INSPECT NOM-INTC-W   REPLACING FIRST "                           "
                                             BY "\PROG\DATOS\INTCONS2014.S17".

           INSPECT NOM-HCAPE-W  REPLACING FIRST "                         "
                                             BY "\PROG\DATOS\SC-HCAPER.S17".

           INSPECT NOM-DOC-HC-W REPLACING FIRST "                        "
                                             BY "\PROG\DATOS\SC-HCDOC.S17".

           INSPECT NOM-CLAP-HC-W REPLACING FIRST "                       "
                                              BY "\PROG\DATOS\SC-CLAP.DAT".

           INSPECT NOM-BIT-W   REPLACING FIRST "                              "
                                            BY "\PROG\DATOS\BITACORA-CAMAS.DAT".

           INSPECT NOM-ENFER-W REPLACING FIRST "                          "
                                            BY "\PROG\DATOS\SC-ARCHENF.DAT".

           INSPECT NOM-MACR-EVOL-W REPLACING FIRST "                           "
                                                BY "\PROG\DATOS\SC-MACRO-EV.DAT".

           INSPECT NOM-MACRE-EVOL-EXT-W REPLACING FIRST "                               "
                                                     BY "\PROG\DATOS\SC-MACRO-EV-EXT.DAT".


        ABRIR-USUARIOS.
           OPEN INPUT ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.

       VALIDAR-ORDENES-MEDICAS.
           OPEN INPUT ARCHIVO-FORMULA-ORD.
           EVALUATE OTR-STAT
             WHEN "00"  CLOSE  ARCHIVO-FORMULA-ORD
             WHEN "35"  OPEN OUTPUT ARCHIVO-FORMULA-ORD
                        CLOSE  ARCHIVO-FORMULA-ORD
           END-EVALUATE.


       VALIDAR-TRIAGE.
           OPEN INPUT ARCHIVO-TRIAGE
           EVALUATE OTR-STAT
             WHEN "00"  CLOSE  ARCHIVO-TRIAGE
             WHEN "35"  OPEN OUTPUT ARCHIVO-TRIAGE
                        CLOSE  ARCHIVO-TRIAGE
           END-EVALUATE.

       VALIDAR-NOPOS.
           OPEN INPUT ARCHIVO-NOPOS
           EVALUATE OTR-STAT
             WHEN "00"  CLOSE  ARCHIVO-NOPOS
             WHEN "35"  OPEN OUTPUT ARCHIVO-NOPOS
                        CLOSE  ARCHIVO-NOPOS
           END-EVALUATE.

       VALIDAR-CAMAS.
           OPEN INPUT ARCHIVO-CAMAS-HOSP
           EVALUATE OTR-STAT
             WHEN "00"  CLOSE  ARCHIVO-CAMAS-HOSP
             WHEN "35"  OPEN OUTPUT ARCHIVO-CAMAS-HOSP
                        CLOSE  ARCHIVO-CAMAS-HOSP
           END-EVALUATE.

       VALIDAR-REFER.
           OPEN INPUT ARCHIVO-CONTRAREFER
           EVALUATE OTR-STAT
             WHEN "00"  CLOSE  ARCHIVO-CONTRAREFER
             WHEN "35"  OPEN OUTPUT ARCHIVO-CONTRAREFER
                        CLOSE  ARCHIVO-CONTRAREFER
           END-EVALUATE.

       VALIDAR-HCPED.
           OPEN INPUT PEDIDOS-HISTORIA
           EVALUATE OTR-STAT
             WHEN "00"  CLOSE  PEDIDOS-HISTORIA
             WHEN "35"  OPEN OUTPUT PEDIDOS-HISTORIA
                        CLOSE  PEDIDOS-HISTORIA
           END-EVALUATE.

       VALIDAR-PARTO.
           OPEN INPUT ARCHIVO-PARTOGRAMA
           EVALUATE OTR-STAT
             WHEN "00"  CLOSE  ARCHIVO-PARTOGRAMA
             WHEN "35"  OPEN OUTPUT ARCHIVO-PARTOGRAMA
                        CLOSE  ARCHIVO-PARTOGRAMA
           END-EVALUATE.

       VALIDAR-INTERCON.
           OPEN INPUT ARCHIVO-INTERCONSULTA
           EVALUATE OTR-STAT
             WHEN "00"  CLOSE  ARCHIVO-INTERCONSULTA
             WHEN "35"  OPEN OUTPUT ARCHIVO-INTERCONSULTA
                        CLOSE  ARCHIVO-INTERCONSULTA
           END-EVALUATE.

       VALIDAR-HCEPI.
           OPEN INPUT DOCUMENTOS-HISTORIA
           EVALUATE OTR-STAT
             WHEN "00"  CLOSE  DOCUMENTOS-HISTORIA
             WHEN "35"  OPEN OUTPUT DOCUMENTOS-HISTORIA
                        CLOSE  DOCUMENTOS-HISTORIA
           END-EVALUATE.

       VALIDAR-APERTURA.
           OPEN INPUT HISTORIA-CLINICA
           EVALUATE OTR-STAT
             WHEN "00"  CLOSE  HISTORIA-CLINICA
             WHEN "35"  OPEN OUTPUT HISTORIA-CLINICA
                        CLOSE  HISTORIA-CLINICA
           END-EVALUATE.

       VALIDAR-DETALLES-HC.
           INITIALIZE CONT-W
           PERFORM VALIDAR-HCDETA VARYING I FROM 0 BY 1
                                            UNTIL I > 9.

       VALIAR-EVOLUCIONES-HC.
           ACCEPT FECHA-ACT-W FROM DATE.

           PERFORM VALIDAR-HCEVO VARYING ANO-X FROM 0 BY 1 
                                   UNTIL ANO-X > (ANO-ACT-W + 1).

           PERFORM VALIDAR-DETEVO VARYING ANO-X FROM 0 BY 1 
                                    UNTIL ANO-X > (ANO-ACT-W + 1).

       VALIDAR-CLAP.
           OPEN INPUT HISTORIA-CLAP
           EVALUATE OTR-STAT
             WHEN "00"  CLOSE  HISTORIA-CLAP
             WHEN "35"  OPEN OUTPUT HISTORIA-CLAP
                        CLOSE  HISTORIA-CLAP
           END-EVALUATE.

       CREAR-BITACORA-CAMAS.
           OPEN INPUT BITACORA-CAMAS
           EVALUATE OTR-STAT
             WHEN "00"  CLOSE  BITACORA-CAMAS
             WHEN "35"  OPEN OUTPUT BITACORA-CAMAS
                        CLOSE  BITACORA-CAMAS
           END-EVALUATE.

       LEER-ENFERMEDAD.
           OPEN INPUT ARCHIVO-ENFERMEDAD
           EVALUATE OTR-STAT
             WHEN "00"  CLOSE  ARCHIVO-ENFERMEDAD
             WHEN "35"  OPEN OUTPUT ARCHIVO-ENFERMEDAD
                        CLOSE  ARCHIVO-ENFERMEDAD
           END-EVALUATE.

       VALIDAR-MACROEV.
           OPEN INPUT ARCHIVO-MACRO-EVOL
           EVALUATE OTR-STAT
             WHEN "00"  CLOSE  ARCHIVO-MACRO-EVOL
             WHEN "35"  OPEN OUTPUT ARCHIVO-MACRO-EVOL
                        CLOSE  ARCHIVO-MACRO-EVOL
           END-EVALUATE.

       VALIDAR-MACRO-EVOL-EXT.
           OPEN INPUT ARCHIVO-MACRO-EVOL-EXT
           EVALUATE OTR-STAT
             WHEN "00"  CLOSE  ARCHIVO-MACRO-EVOL-EXT
             WHEN "35"  OPEN OUTPUT ARCHIVO-MACRO-EVOL-EXT
                        CLOSE  ARCHIVO-MACRO-EVOL-EXT
           END-EVALUATE.

       VALIDAR-USUARIO.
           OPEN INPUT ARCHIVO-RESTRICCION.
           INITIALIZE OPC-REST
           MOVE ADMIN-W   TO COD-REST
           READ ARCHIVO-RESTRICCION WITH NO LOCK
                INVALID KEY
                        MOVE "SC-2"    TO MSJ1-HTML
                        MOVE "Usuario no existe!" TO MSJ2-HTML
                        MOVE "ARCHIVOS-HC" TO MSJ3-HTML
                        GO TO ENVIAR2-ERROR
           END-READ.

           CLOSE ARCHIVO-RESTRICCION.

       VALIDAR-PROFESIONAL.
           OPEN INPUT ARCHIVO-PROFESIONALES
           MOVE ID-REST  TO COD-PROF
           READ ARCHIVO-PROFESIONALES WITH NO LOCK
                INVALID KEY 
                        CONTINUE
      *                  MOVE "SC-2"    TO MSJ1-HTML
      *                  MOVE "No existe profesional" TO MSJ2-HTML
      *                  MOVE "ARCHIVOS-HC" TO MSJ3-HTML
      *                  GO TO ENVIAR2-ERROR
           END-READ.
           CLOSE ARCHIVO-PROFESIONALES

           MOVE 00           TO DAT0-ENV
           MOVE COD-PROF     TO DAT1-ENV
               

             INITIALIZE TABLA-PROF-ENV.

             PERFORM LLENAR-ESPECIALIDAD VARYING I FROM 1 BY 1
                                                 UNTIL I > 10.

       PAGINA-CONFIG.
           MOVE "statuscode" TO COBW3-CNV-NAME
010730     MOVE "00"         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

000570	   MOVE "datosrecibidos" TO COBW3-CNV-NAME
000580      MOVE DATOS-ENVIO      TO COBW3-CNV-VALUE
000590	   CALL "COBW3_SET_CNV"  USING COBW3

010720     MOVE "programa-id" TO COBW3-CNV-NAME
010730     MOVE "ARCHIVOS-HC" TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010760     MOVE "..\PAGINAS\RECIBIDOSPLANO.ASPX" TO SALIDA-HTML
010770     PERFORM ABRIR-HTML.

002545 CERRAR-SESION.
002546     CALL "COBW3_FREE" USING COBW3.
002547     MOVE 1 TO PROGRAM-STATUS.
002548     EXIT PROGRAM.

       LLENAR-ESPECIALIDAD.
           IF ESP-PROF (I) = SPACES OR LOW-VALUES
              CONTINUE
           ELSE
              ADD 1 TO J
              MOVE ESP-PROF (I) TO ESP-PROF-ENV (J)
           END-IF.


       VALIDAR-HCDETA.
           INITIALIZE NOM-HCDET-W
           MOVE DIRECTORIO-LLEGAD-W   TO NOM-HCDET-W
           INSPECT NOM-HCDET-W REPLACING FIRST "                           "
                                            BY "\PROG\DATOS\SC-HCDETA-X.S17".

           INSPECT NOM-HCDET-W REPLACING FIRST "X"
                                            BY CONT-W

           OPEN INPUT HISTORIA-DETALLADA
           EVALUATE OTR-STAT
             WHEN "00"  CLOSE  HISTORIA-DETALLADA
             WHEN "35"  OPEN OUTPUT HISTORIA-DETALLADA
                        CLOSE  HISTORIA-DETALLADA
           END-EVALUATE
           ADD 1 TO CONT-W.

       VALIDAR-HCEVO.
           MOVE DIRECTORIO-LLEGAD-W  TO NOM-HCEVO-W
           IF (ANO-X = ZEROS) OR (ANO-X > 04)
              INSPECT NOM-HCEVO-W REPLACING FIRST "                          "
                                               BY "\PROG\DATOS\SC-HCEVOX1.S17"

              INSPECT NOM-HCEVO-W REPLACING FIRST "X1" BY ANO-X

              OPEN INPUT EVOLUCION-HISTORIA
              EVALUATE OTR-STAT
                  WHEN "00"  CLOSE  EVOLUCION-HISTORIA
                  WHEN "35"  OPEN OUTPUT EVOLUCION-HISTORIA
                             CLOSE  EVOLUCION-HISTORIA
              END-EVALUATE
           END-IF.

       VALIDAR-DETEVO.
           MOVE DIRECTORIO-LLEGAD-W  TO NOM-DETEVO-W
           IF (ANO-X = ZEROS) OR (ANO-X > 04)

              INSPECT NOM-DETEVO-W REPLACING FIRST "                           "
                                                BY "\PROG\DATOS\SC-DETEVOX1.S17"

              INSPECT NOM-DETEVO-W REPLACING FIRST "X1" BY ANO-X

              OPEN INPUT EVOLUCION-DETALLADA
              EVALUATE OTR-STAT
                  WHEN "00"  CLOSE  EVOLUCION-DETALLADA
                  WHEN "35"  OPEN OUTPUT EVOLUCION-DETALLADA
                             CLOSE  EVOLUCION-DETALLADA
              END-EVALUATE
           END-IF.

002566 COPY "P:\PROG\PROYECT\FUENTES\SC-WEB19.CBL".
