      *==================================================================
      *  GUARDA HISTORIA DE ONCOLOGIA // DETALLES HISTORIA //
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

            SELECT ARCHIVO-PROFESIONALES
                  ASSIGN NOM-PROF-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PROF
                  ALTERNATE RECORD KEY
                  DESCRIP-PROF WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-CIUDAD LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-CIU-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-CIU
                  ALTERNATE RECORD KEY IS
                  NOMBRE-CIU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-DETALLES-WEB
                  ASSIGN  TO NOM-DETALLES-WEB
                  ORGANIZATION LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "S:\NEWCOBOL\FUENTES\FD-USUAR.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-HCAPER17.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-HCDETAL17.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-PACIE.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-PROFE.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-CIUDA.CBL".

        FD  ARCHIVO-DETALLES-WEB
            LABEL RECORD STANDARD.
        01  LINEA-RENG.
            02 DATOS-ARCHIVO-WEB.
               03 SECCION-DWEB      PIC X(20).
               03 RENG-DWEB         PIC X(19000).

       WORKING-STORAGE  SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".
       COPY "S:\newcobol\FUENTES\WS-9004.CBL".
       
      * -W VARIABLES NOMBRE ARCHIVOS

       01 NOM-HCAPE-W     PIC X(100).
       01 NOM-HCDET-W     PIC X(100).
       01 NOM-USUAR-W     PIC X(100).
       01 NOM-PACIE-W     PIC X(100).
       01 NOM-PROFE-W     PIC X(100).
       01 NOM-POS-W       PIC X(70).
       01 NOM-CIU-W       PIC X(100).
       01 NOM-DETALLES-WEB  PIC X(70).

      * VARIABLES LOCALES
       01 ID-EDIT.
          02 ID1-EDIT PIC X(14).
          02 ID2-EDIT PIC X.
       77 PARE-W PIC X.
       77 PASO-W PIC 9.
       77 OPER-HC-W PIC X(10).
       77 SECCION-W       PIC X(20).
       77 IP-SERVIDOR-W   PIC X(20).
       77 USU-STAT        PIC XX.
       77 CALC-EDAD PIC 9(3).

       01 LLAVE-HC-W.
             02 LLAVE-PACI-HC-W.
                03 ID-HC-W            PIC X(15).
                03 FOLIO-HC-W.
                   05 FOLIO-SUC-HC-W  PIC XX.
                   05 FOLIO-NRO-HC-W  PIC 9(6).

      * VARIABLES D-WEB     
       77 DATO-LLEGADA-W   PIC X(630).             

        01 DATOS-ENVIO.
          02 ESTADO-ENV              PIC XX.
          02 FILLER                  PIC X VALUE "|".

       77 RENG-DETWEB PIC X(95).


      * *****VARIABLES QUE VIENEN DEL HTML******
       01 REG-HC-W.    
          02 FECHA-HC-W      PIC X(8).
          02 HORA-HC-W       PIC X(4).
          02 SERV-HC-W       PIC 99.
          02 PESO-HC-W       PIC X(7).
          02 TALLA-HC-W      PIC X(3).
          02 IMC-CORP-HC-W   PIC X(5).
          02 PER-CEF-HC-W    PIC X(5).
          02 PER-ABDO-HC-W   PIC X(5).
          02 PER-TORA-HC-W   PIC X(5).
          02 SUP-CORP-HC-W   PIC X(5).
          02 MEDIC-HC-W      PIC X(10).
          02 FINALID-HC-W    PIC 99.
          02 SW-EMBAR-W      PIC 9.
          02 MOTIV-HC-W      PIC X(300). 
          02 EDAD-HC-W.
              03 UNID-EDAD-W    PIC X.
              03 VLR-EDAD-W     PIC 9(3).
       77 NACIM-PACI-W PIC 9(8).
       77 FECH-HC-W PIC 9(8).
       01 REG-USU-W.
          02 DIRECTORIO-W    PIC X(30). 
       
      * ******FIN VARIABLES QUE VIENEN DEL HTML  ******

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
               MOVE "HC9004"         TO MSJ3-HTML
               GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST1 SECTION.
           USE AFTER EXCEPTION PROCEDURE ON HISTORIA-CLINICA.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
             CONTINUE
           ELSE
               MOVE OTR-STAT          TO MSJ1-HTML
               MOVE NOM-HCAPE-W       TO MSJ2-HTML
               MOVE "HC9004"          TO MSJ3-HTML
               GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST2 SECTION.
           USE AFTER EXCEPTION PROCEDURE ON HISTORIA-DETALLADA.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT       TO MSJ1-HTML
              MOVE NOM-HCDET-W    TO MSJ2-HTML
              MOVE "HC9004"       TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST3 SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT       TO MSJ1-HTML
              MOVE NOM-PACIE-W    TO MSJ2-HTML
              MOVE "HC9004"       TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST4 SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CIUDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT       TO MSJ1-HTML
              MOVE NOM-CIU-W      TO MSJ2-HTML
              MOVE "HC9004"       TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

          I-O-TEST5 SECTION.
                  USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-DETALLES-WEB.
          ESCR-EXCEPTIONES.
              IF OTR-STAT = "00"
                 CONTINUE
              ELSE
                  MOVE OTR-STAT         TO MSJ1-HTML
                  MOVE NOM-DETALLES-WEB      TO MSJ2-HTML
                  MOVE "HC9004"       TO MSJ3-HTML
                  GO TO ENVIAR2-ERROR
              END-IF.

       END DECLARATIVES.

       INICIAR-IIS.
           MOVE LOW-VALUE TO COBW3.
           MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
           CALL "COBW3_INIT" USING COBW3.

       LEER-DATO-HTML.
           MOVE "datosh" TO COBW3-SEARCH-DATA
           CALL "COBW3_GET_VALUE" USING COBW3
           MOVE COBW3-GET-DATA TO  DATO-LLEGADA-W.
           UNSTRING DATO-LLEGADA-W DELIMITED BY "|"
           INTO  
           IP-SERVIDOR-W DIRECTORIO-W ADMIN-W OPER-HC-W LLAVE-HC-W SERV-HC-W 
           FECHA-HC-W HORA-HC-W MOTIV-HC-W                  
           
           PESO-HC-W TALLA-HC-W IMC-CORP-HC-W
           PER-CEF-HC-W PER-ABDO-HC-W PER-TORA-HC-W SUP-CORP-HC-W 
                 
           PORC-ACT-FACTOR-9004 INHIBIDORES-9004 ANO-ULT-REPOR-INHIB-9004 
           MES-ULT-REPOR-INHIB-9004 DIA-ULT-REPOR-INHIB-9004 
           HA-RECIBIDO-ITI-9004
           ESTA-RECIBIENDO-ITI-9004 TIEMPO-EN-ITI-9004 HEMARTROSIS-9004 
           ARTROPATIA-HEMO-CRON-9004 NRO-ART-COMPRO-9004 VHC-9004 VHB-9004 
           VIH-9004 
           
           SAN-CUELLO-GARGANTA-9004 SAN-ILIOPSOAS-9004 SAN-INTRACRANEAL-9004 
           SAN-ORAL-9004 SAN-OTRAS-9004 SAN-TEJIDOS-BLANDOS-9004 

           ANTE-FAMIL-9004 NRO-ATEN-URG-HEMO-9004 NRO-EVEN-HOSPI-HEMO-9004
           NRO-HEMAR-ESPON-12MES-9004 NRO-HEMAR-TRAUM-12MES-9004 
           NRO-OTR-HEMOR-12MES-9004 PLAN-FAMIL-9004 
           
           SW-EMBAR-W FACTOR-RECIBIDO-9004 ESQUEMA-RECIBIDO-9004 
           REEMPLAZOS-ARTICULARES-9004 
           REEMPLAZOS-ART-12MES-9004 NRO-HEMOR-PROC-12MES-9004 
           ANO-INI-1ER-TRAT-9004
           MES-INI-1ER-TRAT-9004 DIA-INI-1ER-TRAT-9004 ANAFILAXIS-9004
           CUM-CAUSA-ANAFIL-9004 FRACT-OSTEO-9004 PSEUDOTUMORES-9004 
           UNID-EDAD-DX-9004 VLR-EDAD-DX-9004 
           ANO-DX-9004 MES-DX-9004 DIA-DX-9004 MOTIVO-PRUEBA-DX-9004
           TIPO-DEF-DX-9004 SEVERIDAD-DX-9004 PROF-LIDER-ATENCION-9004 
           NRO-CON-HEMATOLOGIA-9004 NRO-CON-ORTOPEDIA-9004 INTER-PROF-ENFER-9004 
           NRO-CON-ODONTOLOGIA-9004 NRO-CON-NUTRICION-9004 NRO-CON-T-SOCIAL-9004 
           NRO-CON-FISIATRIA-9004 NRO-CON-PSICOLOGIA-9004 NRO-CON-QUIM-FARMA-9004 
           NRO-CON-FISIOTERAPIA-9004 FACTOR-RECIBIDO-ACT-9004 
           ESQUEMA-RECIBIDO-ACT-9004 
           ULT-DOSIS-ACT-9004 FRECUENCIA-SEMANA-9004 NRO-UI-DL-UNID-9004 
           NRO-APL-FACTOR-9004 MODALIDAD-APL-TRAT-9004 VIA-ADMIN-9004 CUM-POS-9004
           CUM-NOPOS-9004 CUM-POS-OTROS-9004 CUM-NOPOS-OTROS-9004 NOVEDADES-9004 
           RENG-OBS-9004(1).

           INSPECT DIRECTORIO-W CONVERTING "$" TO "\"
           
           MOVE 0 TO PASO-W.

       ASIGNAR-NOMBRES.
           INITIALIZE NOM-HCAPE-W
           MOVE "\\" TO NOM-HCAPE-W
           INSPECT NOM-HCAPE-W REPLACING
           FIRST "                    "
           BY    IP-SERVIDOR-W

           IF PASO-W = 0
              INSPECT NOM-HCAPE-W REPLACING
              FIRST "   "
              BY    "\SC"
           ELSE
              INSPECT NOM-HCAPE-W REPLACING
              FIRST "    "
              BY    "\SC$"
           END-IF
           
           MOVE NOM-HCAPE-W TO NOM-USUAR-W NOM-PACIE-W NOM-CIU-W NOM-HCDET-W.
           INSPECT NOM-USUAR-W REPLACING
           FIRST "                              "
           BY    DIRECTORIO-W.
           INSPECT NOM-USUAR-W REPLACING
           FIRST "                "
           BY    "\CONTROL\ARCHUSU".
           
           MOVE NOM-USUAR-W TO NOM-PACIE-W
           INSPECT NOM-PACIE-W REPLACING FIRST "ARCHUSU     "
                                         BY    "SC-PACIE.DAT".
           INSPECT NOM-CIU-W REPLACING
           FIRST "                            "
           BY    "\newcobol\DATOS\ARCHCIUD.DAT"

           INSPECT NOM-HCAPE-W REPLACING
           FIRST "                                "
           BY    "\NEWCOBOL\HC\DATOS\SC-HCAPER.S17".

           INSPECT NOM-HCDET-W REPLACING 
           FIRST  "                                  "
           BY     "\NEWCOBOL\HC\DATOS\SC-HCDETA-X.S17".
           
           MOVE ID-HC-W TO ID-EDIT
           INSPECT NOM-HCDET-W REPLACING FIRST "X" BY ID2-EDIT

           MOVE "C:\PROSOFT\TEMP\DETALLES_HC-XXXXXXXXXXXXXXXXXXXXXXX.TXT" TO NOM-DETALLES-WEB

           INSPECT NOM-DETALLES-WEB REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXX"
                                                    BY LLAVE-HC-W.

      * ********** LECTURA DE ARCHIVOS ******************
       LEER-USUARIO.

           OPEN INPUT ARCHIVO-USUARIOS
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK
           END-READ
           CLOSE ARCHIVO-USUARIOS.

       LEER-PACIENTE.
           OPEN INPUT ARCHIVO-PACIENTES
           READ ARCHIVO-PACIENTES WITH NO LOCK
                INVALID KEY
                 INITIALIZE DATOS-PACI
           END-READ
           CLOSE ARCHIVO-PACIENTES

           OPEN INPUT ARCHIVO-CIUDAD
           MOVE LUGAR-ID-PACI TO COD-CIU
           MOVE NACIM-PACI TO NACIM-PACI-W
           READ ARCHIVO-CIUDAD WITH NO LOCK
                INVALID KEY
                 MOVE COD-CIU TO NOMBRE-CIU
           END-READ

           CLOSE ARCHIVO-CIUDAD.

        LEER-HISTORIA-CLINICA.
           OPEN I-O HISTORIA-CLINICA
           MOVE LLAVE-HC-W         TO LLAVE-HC
           
           READ HISTORIA-CLINICA WITH NO LOCK
               INVALID KEY
               INITIALIZE DATOS-HC
      *  ***** VARIABLES HISTORIA - HC *******
            END-READ
            
            MOVE SERV-HC-W          TO SERV-HC
            MOVE FINALID-HC-W       TO FINALID-HC
            MOVE PESO-HC-W          TO PESO-HC
            MOVE TALLA-HC-W         TO TALLA-HC
            MOVE IMC-CORP-HC-W      TO IMC-CORP-HC
            MOVE SUP-CORP-HC-W      TO SUP-CORP-HC
            MOVE PER-CEF-HC-W       TO PER-CEF-HC
            MOVE PER-ABDO-HC-W      TO PER-ABDO-HC 
            MOVE PER-TORA-HC-W      TO PER-TORA-HC
            MOVE SW-EMBAR-W         TO EMBARAZO-HC
            MOVE MEDIC-HC-W         TO MED-HC
            MOVE HORA-HC-W          TO HORA-HC
            MOVE UNID-EDAD-W        TO UNID-EDAD-HC
            MOVE MOTIV-HC-W         TO MOTIV-HC
            MOVE FUNCTION CURRENT-DATE TO FECHA-HC
            ACCEPT HORA-HC FROM TIME
            MOVE "9004"             TO ESQUEMA-HC
            MOVE ADMIN-W            TO OPER-ELAB-HC
            MOVE ADMIN-W            TO OPER-ULT-CIERRE-HC
            MOVE OPER-HC-W          TO MED-HC 
            MOVE 2                  TO ESTADO-HC
            MOVE FECHA-HC TO FECH-HC-W
            COMPUTE CALC-EDAD = FECH-HC-W - NACIM-PACI-W
            MOVE CALC-EDAD TO VLR-EDAD-HC
            WRITE REG-HC END-WRITE

           CLOSE HISTORIA-CLINICA
           OPEN INPUT ARCHIVO-DETALLES-WEB  
           OPEN I-O HISTORIA-DETALLADA.

       LEER-DETALLES-WEB.
            READ ARCHIVO-DETALLES-WEB NEXT AT END GO TO FIN-LEER-DETALLES-WEB.
            INITIALIZE REG-DETHC
            MOVE LLAVE-HC-W TO LLAVE-HC-DETHC 
            EVALUATE SECCION-DWEB
                WHEN "ENFERMEDAD-ACT-DWEB"
                        MOVE 1001 TO COD-DETHC
                        PERFORM GRABAR-DETALLES-WEB
                WHEN "ANT-FAMILIARES-DWEB"
                        MOVE 2002 TO COD-DETHC
                        PERFORM GRABAR-DETALLES-WEB
                WHEN "ANT-MEDICOS-DWEB"
                        MOVE 2010 TO COD-DETHC   
                        PERFORM GRABAR-DETALLES-WEB
                WHEN "ANT-HEMOR-DWEB"
                        MOVE 2011 TO COD-DETHC   
                        PERFORM GRABAR-DETALLES-WEB
                WHEN "ANT-QUIRURGICOS-DWEB"
                        MOVE 2020 TO COD-DETHC        
                        PERFORM GRABAR-DETALLES-WEB
                WHEN "ANT-TOXICOS-DWEB"
                        MOVE 2035 TO COD-DETHC
                        PERFORM GRABAR-DETALLES-WEB
                WHEN "ANT-TRAUMATICOS-DWEB"
                        MOVE 2040 TO COD-DETHC
                        PERFORM GRABAR-DETALLES-WEB        
                WHEN "ANT-GINEC-DWEB"
                        MOVE 2060 TO COD-DETHC
                        PERFORM GRABAR-DETALLES-WEB
                WHEN "EXAGENERAL-DWEB"
                        MOVE 4005 TO COD-DETHC
                        PERFORM GRABAR-DETALLES-WEB           
                WHEN "ANALISIS-DWEB"
                        MOVE 7501 TO COD-DETHC
                        PERFORM GRABAR-DETALLES-WEB
                END-EVALUATE

           GO TO LEER-DETALLES-WEB.

       FIN-LEER-DETALLES-WEB.
           CLOSE ARCHIVO-DETALLES-WEB
           CLOSE HISTORIA-DETALLADA.
                 
       GRABAR-DETALLES-ONCOLOGIA.
           OPEN I-O HISTORIA-DETALLADA
           IF DATO-9004 = SPACES OR LOW-VALUE
               CONTINUE
           ELSE    
               INITIALIZE REG-DETHC
               MOVE LLAVE-HC-W TO LLAVE-HC-DETHC
               MOVE 9004 TO COD-DETHC
               READ HISTORIA-DETALLADA
                INVALID KEY 
                   MOVE 9004 TO COD-DETHC
                   MOVE DATO-9004 TO DETALLE-DETHC
                   WRITE REG-DETHC END-WRITE
                NOT INVALID KEY
                    IF DATO-9004 = DETALLE-DETHC
                      CONTINUE
                    ELSE
                       MOVE DATO-9004 TO DETALLE-DETHC
                       REWRITE REG-DETHC END-REWRITE
                    END-IF
                END-READ
            END-IF
           CLOSE HISTORIA-DETALLADA

           MOVE 00 TO ESTADO-ENV.

       ENVIO-DATOS.
           MOVE "datosrecibidos"  TO COBW3-CNV-NAME
004393     MOVE ESTADO-ENV        TO COBW3-CNV-VALUE
004403     CALL "COBW3_SET_CNV"   USING COBW3
004413     MOVE "../PAGINAS/RECIBIDOS.ASPX"  TO SALIDA-HTML
004430     PERFORM ABRIR-HTML.

002545 CERRAR-SESION.
002546     CALL "COBW3_FREE" USING COBW3.
002547     MOVE 1 TO PROGRAM-STATUS
           EXIT PROGRAM.

       GRABAR-DETALLES-WEB.
           IF RENG-DWEB IS NOT = SPACES OR LOW-VALUE
               READ HISTORIA-DETALLADA  WITH NO LOCK
                INVALID KEY
                   MOVE RENG-DWEB TO DETALLE-DETHC
                   WRITE REG-DETHC END-WRITE
                NOT INVALID KEY
                   IF RENG-DWEB = DETALLE-DETHC
                       CONTINUE
                   ELSE
                       MOVE RENG-DWEB TO DETALLE-DETHC 
                       REWRITE REG-DETHC END-REWRITE
                   END-IF
               END-READ
           END-IF.


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
