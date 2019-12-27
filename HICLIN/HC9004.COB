      *==================================================================
      *  1-  CREA  APERTURA HISTORIA CLINICA DE ONCOLOGIA
      *  2-  GRABA HISTORIA CLINICA DE ONCOLOGIA
      *==================================================================
      
      * PO - PABLO OLGUIN 25/10/2019 CREACION APERTURA ONCOLOGIA
      * PO - PABLO OLGUIN 12/11/2019 CREACION APERTURA ONCOLOGIA
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-USUAR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.  

            SELECT HISTORIA-CLINICA LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-HCAPE-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-HC
                  ALTERNATE RECORD KEY IS FOLIO-HC  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FECHA-HC  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS MED-HC    WITH DUPLICATES
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

            SELECT ARCHIVO-PLANO
                  ASSIGN  TO NOM-PLANO-W
                  ORGANIZATION LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.      
                                    
       DATA DIVISION.
       FILE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUAR.CBL".       
       COPY "P:\PROG\PROYECT\FUENTES\FD-HCAPER17.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-HCDETAL17.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-CIUDA.CBL".

       FD  ARCHIVO-PLANO
            LABEL RECORD STANDARD.
        01  LINEA-RENG.
            02 DATOS-ARCHIVO-WEB.
               03 SECCION-DWEB      PIC X(20).
               03 RENG-DWEB         PIC X(19000).
       
       WORKING-STORAGE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC19.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WS-9004.CBL".

       77 NOM-USUAR-W        PIC X(100).
       77 NOM-HCAPE-W        PIC X(100).
       77 NOM-HCDET-W        PIC X(100).
       77 NOM-PLANO-W     PIC X(100).

       01 ID-EDIT.
          02 ID1-EDIT                PIC X(14).
          02 ID2-EDIT                PIC X.

       01  DATO-LLEGADA-W.
           02 SESION-W               PIC X(29).
           02 DIRECTORIO-LLEGAD-W    PIC X(30).
           02 MES-TRABAJO-W          PIC X(10).
           02 LLAVE-HC-W             PIC X(23).
           02 OPER-W                 PIC X(4).
           02 SERV-HC-W              PIC 99.
           02 NOVEDAD-HC-W           PIC 99.
           02 FINALID-HC-W           PIC 99.
           02 LUGAR-ID-PACI-W        PIC X(5).
           02 EDAD-W.
              05 UNID-EDAD-W         PIC X.
              05 VLR-EDAD-W          PIC 9(3).
           02 MOTIV-HC-W             PIC X(380).
           02 PESO-HC-W              PIC X(7).
           02 TALLA-HC-W             PIC 9(3).
           02 IMC-CORP-HC-W          PIC X(5).
           02 PER-CEF-HC-W           PIC X(5).
           02 PER-ABDO-HC-W          PIC X(5).
           02 PER-TORA-HC-W          PIC X(5).
           02 SUP-CORP-HC-W          PIC X(5).
           02 SW-EMBAR-W             PIC 9.
           02 MEDIC-HC-W             PIC X(10).


       01 LINEA-LLEGADA-W     PIC X(800).


       LINKAGE SECTION.

       COPY "P:\PROG\PROYECT\FUENTES\ISAPICTX.CBL".
       PROCEDURE DIVISION WITH stdcall LINKAGE USING ISAPI-CTX-CNT.

       DECLARATIVES.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUNET.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"    
              CONTINUE
           ELSE
               MOVE OTR-STAT   TO MSJ1-HTML
               MOVE NOM-USU-W  TO MSJ2-HTML
               MOVE "HC9004"   TO MSJ3-HTML
               GO TO ENVIAR2-ERROR
           END-IF.    

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USU-W                TO MSJ2-HTML
              MOVE "HC9004"                 TO MSJ3-HTML
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
              MOVE "HC9004"                 TO MSJ3-HTML
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
              MOVE "HC811"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR  
           END-IF.    

       I-O-TEST5 SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PLANO.
       ESCR-EXCEPTIONES.
              IF OTR-STAT = "00"
                 CONTINUE
              ELSE
                  MOVE OTR-STAT              TO MSJ1-HTML
                  MOVE NOM-PLANO-W      TO MSJ2-HTML
                  MOVE "HC9004"              TO MSJ3-HTML
                  GO TO ENVIAR2-ERROR
              END-IF.

       END DECLARATIVES.

       INICIAR-IIS.
           MOVE LOW-VALUE TO COBW3.
           MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
           CALL "COBW3_INIT" USING COBW3.


       LEER-DATOS-APERTURA-HTML.
           MOVE "datosh" TO COBW3-SEARCH-DATA.
           CALL "COBW3_GET_VALUE" USING COBW3.
           MOVE COBW3-GET-DATA TO LINEA-LLEGADA-W.

           INITIALIZE DATO-LLEGADA-W DATO-9004

           UNSTRING LINEA-LLEGADA-W DELIMITED BY "|"
           INTO SESION-W, DIRECTORIO-LLEGAD-W, MES-TRABAJO-W, LLAVE-HC-W,
                OPER-W, SERV-HC-W, NOVEDAD-HC-W, FINALID-HC-W, 
                LUGAR-ID-PACI-W, EDAD-W, MOTIV-HC-W, PESO-HC-W, TALLA-HC-W, 
                IMC-CORP-HC-W, PER-CEF-HC-W, PER-ABDO-HC-W, PER-TORA-HC-W, 
                SUP-CORP-HC-W, SW-EMBAR-W, MEDIC-HC-W,
                PORC-ACT-FACTOR-9004, INHIBIDORES-9004, FECHA-ULT-REPOR-INHIB-9004, 
                HA-RECIBIDO-ITI-9004, ESTA-RECIBIENDO-ITI-9004, TIEMPO-EN-ITI-9004,
                HEMARTROSIS-9004, VHC-9004, VHB-9004, VIH-9004, ARTROPATIA-HEMO-CRON-9004,
                NRO-ART-COMPRO-9004,
                SAN-ILIOPSOAS-9004, SAN-TEJIDOS-BLANDOS-9004, 
                SAN-INTRACRANEAL-9004, SAN-CUELLO-GARGANTA-9004, 
                SAN-ORAL-9004, SAN-OTRAS-9004,
                ANTE-FAMIL-9004,
                NRO-ATEN-URG-HEMO-9004, NRO-EVEN-HOSPI-HEMO-9004,
                NRO-HEMAR-ESPON-12MES-9004, NRO-HEMAR-TRAUM-12MES-9004, 
                NRO-OTR-HEMOR-12MES-9004, 
                REEMPLAZOS-ARTICULARES-9004, REEMPLAZOS-ART-12MES-9004, 
                NRO-HEMOR-PROC-12MES-9004,
                FACTOR-RECIBIDO-9004, ESQUEMA-RECIBIDO-9004, FECHA-INI-PRIM-TRAT-9004,
                ANAFILAXIS-9004, CUM-CAUSA-ANAFIL-9004,
                FRACT-OSTEO-9004, PSEUDOTUMORES-9004, NRO-HEMO-TRAUM-ULT-12MES-9004,
                GINECO-OBST-9004,
                EDAD-DX-9004, MOTIVO-PRUEBA-DX-9004,
                FECHA-DX, TIPO-DEF-DX-9004, SEVERIDAD-DX-9004,
                PROF-LIDER-ATENCION-9004, INTER-PROF-ENFER-9004,
                NRO-CON-HEMATOLOGIA-9004, NRO-CON-ORTOPEDIA-9004, NRO-CON-ODONTOLOGIA-9004,
                NRO-CON-NUTRICION-9004, NRO-CON-T-SOCIAL-9004, NRO-CON-FISIATRIA-9004, 
                NRO-CON-PSICOLOGIA-9004, NRO-CON-QUIM-FARMA-9004, NRO-CON-FISIOTERAPIA-9004,
                CUM-POS-9004, CUM-NOPOS-9004, CUM-POS-OTROS-9004, CUM-NOPOS-OTROS-9004, 
                FACTOR-RECIBIDO-ACT-9004, ESQUEMA-RECIBIDO-ACT-9004, ULT-DOSIS-ACT-9004, 
                FRECUENCIA-SEMANA-9004, NRO-UI-DL-UNID-9004, NRO-APL-FACTOR-9004, 
                VIA-ADMIN-9004, MODALIDAD-APL-TRAT-9004 

           END-UNSTRING.

           MOVE SESION-W TO LLAVE-SESION-W SESION-ENV.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.

       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.


       ABRIR-USUARIO.
           INITIALIZE OTR-STAT.
006920     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
006930
006940     OPEN INPUT ARCHIVO-USUNET
006950     EVALUATE OTR-STAT
006960       WHEN "00"  CONTINUE
006970       WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
006980                  INITIALIZE REG-USUNET
006990                  WRITE REG-USUNET
007000                  CLOSE ARCHIVO-USUNET
007010                  MOVE "SC-2"                          TO MSJ1-HTML
007020                  MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007030                  MOVE "HC9004"                      TO MSJ3-HTML
007040                  GO TO ENVIAR2-ERROR
007050       WHEN OTHER GO TO CERRAR-SESION
007060     END-EVALUATE.
007070
007080     INITIALIZE LLAVE-USUNET.

       LEER-USUARIO.
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
007130
007140     CLOSE ARCHIVO-USUNET.
007200     
007210     IF NOMBRE-USUNET = SPACES
007220     OR NIT-USUNET = ZEROS
007230        MOVE "SC-2"        TO MSJ1-HTML
007240        MOVE "Falta configurar usuarios" TO MSJ2-HTML
007250        MOVE "HC9004"    TO MSJ3-HTML
007260        GO TO ENVIAR2-ERROR
007270     END-IF.
       
       ASIGNAR-NOMBRES-ARCHIVOS.

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           INITIALIZE NOM-USUAR-W
           MOVE "\\" TO NOM-USUAR-W

           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                           BY  IP-DATOS-USUNET
           INSPECT NOM-USUAR-W REPLACING FIRST " "
                                            BY "\"

           MOVE NOM-USUAR-W TO NOM-HCAPE-W NOM-HCDET-W                                    
           INSPECT NOM-USUAR-W REPLACING FIRST "                              "
                                           BY DIRECTORIO-LLEGAD-W


           INSPECT NOM-USUAR-W   REPLACING FIRST "                       "
                                              BY "\CONTROL\SC-ARCHUSU.DAT".

           INSPECT NOM-HCAPE-W REPLACING FIRST   "                        "
                                           BY    "PROG\DATOS\SC-HCAPER.S17".

           INSPECT NOM-HCDET-W REPLACING FIRST "                          "
                                            BY "PROG\DATOS\SC-HCDETA-X.S17".

           MOVE "C:\PROSOFT\TEMP\DETALLES_HC-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.TXT"
                TO NOM-PLANO-W

           INSPECT NOM-PLANO-W REPLACING 
               FIRST   "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
               BY       SESION-W.
       
      * ABRIR HISTORIA CLINICA
       ABRIR-HISTORIA.
           OPEN INPUT HISTORIA-CLINICA.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              CLOSE HISTORIA-CLINICA
           END-IF.

       LEER-HISTORIA.
           INITIALIZE DATOS-HC
           MOVE LLAVE-HC-W  TO LLAVE-HC
           READ HISTORIA-CLINICA
               INVALID KEY
                     MOVE 7 TO NOVEDAD-HC-W
                     MOVE SERV-HC-W TO SERV-HC
                     MOVE FINALID-HC-W TO FINALID-HC
                     MOVE EMBARAZO-HC TO SW-EMBARW
                     
               NOT INVALID KEY
                     MOVE 8 TO NOVEDAD-HC-W.
                     MOVE SERV-HC-W TO SERV-HC
                     MOVE FINALID-HC-W TO FINALID-HC-W
                     GO TO ERROR-YA-EXISTE.
           CLOSE HISTORIA-CLINICA.
       
      * GRABAR HISTORIA 
       GRABAR-HISTORIA-CLINICA.
           MOVE LLAVE-HC-W TO ID-EDIT
           IF ID2-EDIT = "1" OR "2" OR "3" OR "4" OR "5" OR
                          "6" OR "7" OR "8" OR "9" OR "0"
              CONTINUE
           ELSE
              MOVE "0" TO ID2-EDIT
           END-IF

           INSPECT NOM-HCDET-W REPLACING FIRST "X" BY ID2-EDIT

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
            MOVE UNID-EDAD-W        TO UNID-EDAD-HC
            MOVE MOTIV-HC-W         TO MOTIV-HC
            MOVE FUNCTION CURRENT-DATE TO FECHA-HC
            ACCEPT HORA-HC FROM TIME
            MOVE "9004"             TO ESQUEMA-HC
            MOVE OPER-W             TO OPER-ELAB-HC
      * PENDIENTE CIERRE HISTORIA VA DESPUES DE FORMULACION     
            MOVE OPER-W             TO OPER-ULT-CIERRE-HC
            MOVE 2                  TO ESTADO-HC
            MOVE EDAD-W             TO EDAD-HC  
            WRITE REG-HC END-WRITE

           CLOSE HISTORIA-CLINICA
           OPEN INPUT ARCHIVO-PLANO  
           OPEN I-O HISTORIA-DETALLADA.

       GRABAR-DETALLES-HISTORIA.
            READ ARCHIVO-PLANO NEXT AT END GO TO GRABAR-DETALLES-HC-ONCOLOGIA.
            INITIALIZE REG-DETHC
            MOVE LLAVE-HC-W TO LLAVE-HC-DETHC 
            EVALUATE SECCION-DWEB
                WHEN "ENFERMEDAD-ACT-HC"
                        MOVE 1001 TO COD-DETHC
                        PERFORM GRABAR-DETALLES-HC
                WHEN "ANT-FAMILIARES-HC"
                        MOVE 2002 TO COD-DETHC
                        PERFORM GRABAR-DETALLES-HC
                WHEN "ANT-MEDICOS-HC"
                        MOVE 2010 TO COD-DETHC   
                        PERFORM GRABAR-DETALLES-HC
                WHEN "ANT-HEMOR-HC"
                        MOVE 2011 TO COD-DETHC   
                        PERFORM GRABAR-DETALLES-HC
                WHEN "ANT-QUIRURGICOS-HC"
                        MOVE 2020 TO COD-DETHC        
                        PERFORM GRABAR-DETALLES-HC
                WHEN "ANT-TOXICOS-HC"
                        MOVE 2035 TO COD-DETHC
                        PERFORM GRABAR-DETALLES-HC
                WHEN "ANT-TRAUMATICOS-HC"
                        MOVE 2040 TO COD-DETHC
                        PERFORM GRABAR-DETALLES-HC        
                WHEN "ANT-GINEC-HC"
                        MOVE 2060 TO COD-DETHC
                        PERFORM GRABAR-DETALLES-HC
                WHEN "EXAGENERAL-HC"
                        MOVE 4005 TO COD-DETHC
                        PERFORM GRABAR-DETALLES-HC           
                WHEN "ANALISIS-HC"
                        MOVE 7501 TO COD-DETHC
                        PERFORM GRABAR-DETALLES-HC
                WHEN "ANALISIS-HC"
                        MOVE 7501 TO COD-DETHC
                        PERFORM GRABAR-DETALLES-HC
                WHEN "OBSERVACIONES-HC"
                      IF RENG-DWEB IS NOT = SPACES OR LOW-VALUE
                         MOVE RENG-DWEB TO OBSERVACIONES-9004
                      END-IF
              
                END-EVALUATE

           GO TO GRABAR-DETALLES-HISTORIA. 
       
       GRABAR-DETALLES-HC-ONCOLOGIA.
           CLOSE ARCHIVO-PLANO
           CLOSE HISTORIA-DETALLADA

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
           GO TO ENVIO-DATOS.

       ERROR-YA-EXISTE.
           MOVE "SC-1" TO MSJ1-HTML
           MOVE "ATENCION! ESTE PACIENTE YA POSEE UNA HISTORIA ABIERTA" TO MSJ2-HTML
           MOVE "HC9004" TO MSJ3-HTML
           IF TEMPORAL-HC = 1
              INSPECT MSJ2-HTML REPLACING FIRST "    " 
                                             BY "; NO FUE TOTALMENTE DILIGENCIADA".
                                             
           GO TO ENVIAR2-ERROR.


       ENVIO-DATOS.
           MOVE "statuscode" TO COBW3-CNV-NAME
010730     MOVE "00"         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

000570	   MOVE "datosrecibidos" TO COBW3-CNV-NAME
000580     MOVE  DATOS-ENVIO     TO COBW3-CNV-VALUE
000590	   CALL "COBW3_SET_CNV"  USING COBW3

010720     MOVE "programa-id" TO COBW3-CNV-NAME
010730     MOVE "HC9004"      TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010760     MOVE "..\PAGINAS\RECIBIDOSPLANO.ASPX" TO SALIDA-HTML
010770     PERFORM ABRIR-HTML.

       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.

        GRABAR-DETALLES-HC.
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
           
       COPY "P:\PROG\PROYECT\FUENTES\SC-WEB19.CBL".         

                                       

