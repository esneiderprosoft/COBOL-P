004820*=================================================================
004830* GUARDA APLICACION DE MEDICAMENTOS //  5-2-1
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


           SELECT EVOLUCION-HISTORIA
                  ASSIGN NOM-HCEVO-W LOCK MODE IS AUTOMATIC
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


	   SELECT ARCHIVO-POS
                  ASSIGN NOM-POS-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-POS
                  ALTERNATE RECORD KEY IS
                  NOMBRE-POS WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


005030     SELECT ARCHIVO-TEMPORAL
005040            ASSIGN NOM-TMP-W
005050            ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC;
                  RECORD KEY   IS LLAVE-TMP
                  ALTERNATE RECORD KEY 
                  FOLIO-TMP WITH DUPLICATES
                  ALTERNATE RECORD KEY 
                  FECHA-TMP WITH DUPLICATES
005060            FILE STATUS IS OTR-STAT.


005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
005110 COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-RESTR.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-PROFE.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-USUAR.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-PACIE.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-HCAPER17.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-HCEVO17.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-DETEVO17.CBL".
005110 COPY "S:\NEWCOBOL\FUENTES\FD-CODPO.CBL".

005140 FD  ARCHIVO-TEMPORAL
005150     LABEL RECORD STANDARD.
005160 01  REG-TMP.
005170     02 LLAVE-TMP.
              03 LLAVE-FECHA-TMP.
                 05 LLAVE-PACI-TMP.
                    07 ID-PACI-TMP     PIC X(15). 
                    07 FOLIO-TMP.
                       09 SUC-TMP      PIC XX.
                       09 NRO-TMP      PIC 9(6).
                 05 FECHA-TMP.
                    07 AÑO-TMP         PIC 9(4).
                    07 MES-TMP         PIC 99.
                    07 DIA-TMP         PIC 99.
              03 HORA-TMP.
                 05 HR-TMP             PIC 99.
                 05 MN-TMP             PIC 99.
              03 OPER-TMP              PIC X(4).
           02 ESTADO-TMP               PIC 9.
           02 DATOS-TMP                PIC X(900)  OCCURS 35.



005190 WORKING-STORAGE SECTION.
005200 COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
005210 COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".
005210 COPY "P:\PROG\PROYECT\FUENTES\WS-ESQUEMA.CBL".

       77 NOM-PACIE-W                PIC X(70).
       77 NOM-HCAPE-W                PIC X(70).
       77 NOM-USUAR-W                PIC X(70).
       77 NOM-HCEVO-W                PIC X(70).
       77 NOM-DETEVO-W               PIC X(70).
       77 NOM-POS-W                  PIC X(70).
       77 NOM-PROF-W                 PIC X(70).
       77 NOM-TMP-W                  PIC X(70).
       77 EDITADO-W                  PIC X(10).
       77 SW-VAR                     PIC 999.
       77 K                          PIC 999.
       77 MEDICO-W                   PIC 9(10).
       77 SERV-W                     PIC 99.
       77 GLUCOMETRIA-W              PIC 9(3).
       77 DOSIS-OXIGENO-W            PIC 99.
       77 CONT                       PIC 99.
       77 CONT2                       PIC 99.

       01 EDAD-W.
          02 UNID-EDAD-W             PIC X.
          02 VLR-EDAD-W              PIC 9(3).

       01  TABLA-W.
           02 CM-W                   OCCURS 95     PIC X.

       01 COD-TMP.
          02 SW-ALFA                 PIC 9.
          02 TABLA-COD1.
             03 COD1-PAC             PIC X OCCURS 10.
          02 TABLA-COD2.
             03 COD2-PAC             PIC X OCCURS 10.

       01 FECHA-ACT.
          02 ANO-ACT        PIC 99.
          02 ANO2-ACT       PIC 99.
          02 MES-ACT        PIC 99.
          02 DIA-ACT        PIC 99.


       01  DATO-LLEGADA-W.
           02 SESION-LLEGAD-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 FILLER                    PIC X.
           02 ID-PACIENTE-W             PIC X(15).
           02 FILLER                    PIC X.
           02 FOLIO-LLEGAD-W            PIC X(8).
           02 FILLER                    PIC X.
           02 OPER-LLEGAD-W             PIC X(4).


       01 TABLA-TMP-W.
          02 DATOS-TMP-W       OCCURS 900  PIC X.


       01 COD-PACI-W                    PIC X(15).



      * estructura para formulacion

       01 TABLA-FORM-W.
          03 TAB-FOR-W OCCURS 60.
             05 LLAVE-FECHA-FOR-W.
                07 FECHA-FOR-W.
                   11 ANO-FOR-W      PIC 9(4).
                   11 MES-FOR-W      PIC 99.
                   11 DIA-FOR-W      PIC 99.
                07 HORA-FOR-W.
                   11 HR-FOR-W       PIC 99.
                   11 MN-FOR-W       PIC 99.
             05 COD-FORMU-W.
                07  GRP-W            PIC XX.
                07  ARTIC-W          PIC X(11).
             05 TIPO-DOSIF-W         PIC X.
             05 VIA-W                PIC X.
             05 CANT-FORMU-W         PIC 9(3)V999.
             05 DOSIS-W.
                07  ORDEN-DOSIS-W    PIC X(12).
                07  DET-DOSIS-W.
                   11 CANT-DOSIS-W   PIC X(9).
                   11 UNID-DOSIS-W   PIC X(13).
                   11 INMED-W.
                      13 CADA-DOSIS-W PIC X(5).
                      13 CANT-FREC-W PIC XX.
                      13 UNID-FREC-W PIC X(7).
                   11 VIA-DOSIS-W    PIC X(6).
                   11 FILLER         PIC X.
      *88*60 = 5280
          03 TABLA-MEZCLA-W OCCURS 60.
             05 ORDEN-MEZCLA-W       PIC X(12).
             05 BASE-MEZCLA-W.
                07 CC-CANT-MEZCLA-W  PIC Z(3).
                07 CC-TIT-MEZCLA-W   PIC X(7).
                07 BASE-TIT-MEZCLA-W PIC X(7).
                07 PORC-BASE-MEZCLA-W PIC 99.9.
             05 MAS-OTR-MEZCLA-W     PIC X(5).
             05 CANT-MEDIC2-MEZCLA-W PIC ZZZ.9.
             05 UNID-DOSIS-MEZCLA-W  PIC X(13).
             05 MEDIC2-MEZCLA-W      PIC X(15).
             05 CADA-MEZCLA-W        PIC X(6).
             05 CANT-CADA-MEZCLA-W   PIC ZZ.
             05 UNID-FREC-MEZCLA-W   PIC X(6).
             05 VIA-MEZCLA-W         PIC X(6).
      *89 * 60 = 5340
          03 TABLA-OBSERVACIONES-FORM-W.
             05 TABLA-OBS-FORM-W     OCCURS 60.
                07 OBSERVACIONES-FORM-W PIC X(95) OCCURS 2.


       01 HORA-ENV.
          02 HRA-ENV             PIC 99.
          02 MIN-ENV             PIC 99.
          02 SEG-ENV             PIC 99.


       01 DATO-INICIO-W.
          02 FECHA-INI-W.
             03 ANO-INI-W        PIC 9(4).
             03 MES-INI-W        PIC 99.
             03 DIA-INI-W        PIC 99.
          02 HORA-INI-W.
             03 HR-INI-W         PIC 99.
             03 MN-INI-W         PIC 99.
             03 SG-INI-W         PIC 99.

       01 ADICION.
          02 TIPO PIC X.
          02 CANT PIC S9(4).

       01 DATO-RETURN.
          02 FECHA-RETURN.
             03 ANO-RETURN        PIC 9(4).
             03 MES-RETURN        PIC 99.
             03 DIA-RETURN        PIC 99.
          02 HORA-RETURN.
             03 HR-RETURN         PIC 99.
             03 MN-RETURN         PIC 99.
             03 SG-RETURN         PIC 99.

       01 SW-INVALID-W              PIC 99.
       01 MENSAJE-INDALID-W         PIC X(50).

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
002650        MOVE "HC003A"         TO MENSAJE3-HTML
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
006440        MOVE "HC003A"         TO MENSAJE3-HTML
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
006560        MOVE "HC003A"         TO MENSAJE3-HTML
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
006680        MOVE "HC003A"         TO MENSAJE3-HTML
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
006680        MOVE "HC003A"         TO MENSAJE3-HTML
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
006680        MOVE "HC003A"         TO MENSAJE3-HTML
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
006680        MOVE "HC003A"         TO MENSAJE3-HTML
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
006680        MOVE "HC003A"         TO MENSAJE3-HTML
006690        GO TO ENVIAR-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON EVOLUCION-DETALLADA.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-DETEVO-W     TO MENSAJE2-HTML
006680        MOVE "HC003A"         TO MENSAJE3-HTML
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
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TEMPORAL.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MENSAJE1-HTML
006670        MOVE NOM-TMP-W        TO MENSAJE2-HTML
006680        MOVE "HC-NTAS-TMP"    TO MENSAJE3-HTML
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

           ACCEPT FECHA-ACT FROM DATE.

           INITIALIZE NOM-USUAR-W NOM-REST-W NOM-PACIE-W NOM-HCAPE-W NOM-HCEVO-W NOM-PROF-W NOM-POS-W
                      

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\".
           MOVE "\\"  TO NOM-REST-W NOM-USUAR-W  NOM-HCAPE-W
007150     INSPECT NOM-REST-W  REPLACING FIRST "                    "
007160                                      BY IP-DATOS-USUNET
007170     INSPECT NOM-REST-W  REPLACING FIRST " "
007180                                      BY "\"
007190     INSPECT NOM-REST-W  REPLACING FIRST "                          "
007200                                      BY "NEWCOBOL\DATOS\ARCHREST.SC".

           MOVE NOM-REST-W TO NOM-POS-W

           INSPECT NOM-POS-W REPLACING FIRST "ARCHREST.SC "
                                          BY "SC-FARMA.DAT".

           INSPECT NOM-USUAR-W REPLACING FIRST  "                    "
                                            BY  IP-DATOS-USUNET
           INSPECT NOM-USUAR-W REPLACING FIRST  " "
                                            BY  "\"
           INSPECT NOM-USUAR-W REPLACING FIRST  "                              "
007183                                      BY  DIRECTORIO-USUNET
           INSPECT NOM-USUAR-W REPLACING FIRST  "                "
                                            BY  "\CONTROL\ARCHUSU".

           MOVE  NOM-USUAR-W   TO NOM-PROF-W

           INSPECT NOM-PROF-W  REPLACING FIRST  "ARCHUSU     "
                                            BY  "ARCHPROF.DAT".

           MOVE NOM-USUAR-W  TO NOM-PACIE-W       
           
           INSPECT NOM-PACIE-W  REPLACING FIRST "ARCHUSU     "
                                             BY "SC-PACIE.DAT".

           INSPECT NOM-HCAPE-W REPLACING FIRST  "                    "
                                            BY  IP-DATOS-USUNET

           INSPECT NOM-HCAPE-W REPLACING FIRST  "                   "
                                            BY  "\NEWCOBOL\HC\DATOS\"


           MOVE NOM-HCAPE-W    TO NOM-HCEVO-W NOM-DETEVO-W

           INSPECT NOM-HCAPE-W REPLACING FIRST  "             "
                                            BY  "SC-HCAPER.S17".




       ABRIR-USUARIOS.
           OPEN INPUT ARCHIVO-USUARIOS
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.

       LEER-RESTRICCION.
           INITIALIZE OPC-REST
           OPEN INPUT ARCHIVO-RESTRICCION.
           MOVE OPER-LLEGAD-W  TO COD-REST

           READ ARCHIVO-RESTRICCION WITH NO LOCK 
                INVALID KEY 
                        MOVE "99"                 TO MENSAJE1-HTML
                        MOVE "Usuario no existe"  TO MENSAJE2-HTML
                        MOVE "HC003A"             TO MENSAJE3-HTML
                        GO TO ENVIAR-ERROR
           END-READ.

           CLOSE ARCHIVO-RESTRICCION.

           MOVE ID-REST   TO MEDICO-W.


       LEER-PROFESIONALES.
           OPEN INPUT ARCHIVO-PROFESIONALES.

           MOVE MEDICO-W   TO COD-PROF
           READ ARCHIVO-PROFESIONALES WITH NO LOCK 
                INVALID KEY
                        MOVE "99"                      TO MENSAJE1-HTML
                        MOVE "Profesional no existe. " TO MENSAJE2-HTML
                        MOVE "HC003A"                  TO MENSAJE3-HTML
                        GO TO ENVIAR-ERROR
           END-READ.

           CLOSE ARCHIVO-PROFESIONALES.


       LEER-PACIENTE.
           OPEN INPUT ARCHIVO-PACIENTES.

           MOVE ID-PACIENTE-W     TO COD-PACI
           READ ARCHIVO-PACIENTES WITH NO LOCK 
                INVALID KEY
                        MOVE "99"                   TO MENSAJE1-HTML
                        MOVE "Paciente no existe "  TO MENSAJE2-HTML
                        MOVE "HC003A"               TO MENSAJE3-HTML
                        GO TO ENVIAR-ERROR
           END-READ

           CLOSE ARCHIVO-PACIENTES.


       LEER-HISTORIA.
           OPEN INPUT HISTORIA-CLINICA
                      ARCHIVO-POS.

           MOVE ID-PACIENTE-W  TO ID-HC
           MOVE FOLIO-LLEGAD-W      TO FOLIO-HC

           READ HISTORIA-CLINICA WITH NO LOCK 
                INVALID KEY
                        MOVE "99"                        TO MENSAJE1-HTML
                        MOVE LLAVE-HC  TO MENSAJE2-HTML
                        MOVE "HC003A"                    TO MENSAJE3-HTML
                        GO TO ENVIAR-ERROR
           END-READ

           IF ESTADO-HC = 1
              CONTINUE
           ELSE
              MOVE "99"         TO MENSAJE1-HTML
              MOVE "Historia clinica ya esta cerrada" TO MENSAJE2-HTML
              MOVE LLAVE-HC     TO MENSAJE3-HTML
              GO TO ENVIAR-ERROR
           END-IF

           CLOSE HISTORIA-CLINICA.

           MOVE  EDAD-HC   TO EDAD-W
           MOVE 9      TO SERV-W.




       ABRIR-TEMPORAL.

           MOVE "C:\PROSOFT\TEMP\XXXXXXXX-HCEVO.TMP"   TO NOM-TMP-W

           INSPECT NOM-TMP-W REPLACING FIRST "XXXXXXXX"
                                               BY FECHA-TOTAL.

           OPEN INPUT ARCHIVO-TEMPORAL.

           MOVE FUNCTION CURRENT-DATE TO FECHA-ACT

           INSPECT NOM-HCEVO-W REPLACING FIRST "              "
                                            BY "SC-HCEVOX1.S17"
           INSPECT NOM-HCEVO-W REPLACING FIRST "X1"
                                            BY ANO2-ACT.

           INSPECT NOM-DETEVO-W REPLACING FIRST "               "
                                             BY "SC-DETEVOX1.S17"

           INSPECT NOM-DETEVO-W REPLACING FIRST "X1"
                                             BY ANO2-ACT.


           MOVE ID-PACIENTE-W        TO COD-PACI-W.


       LEER-TEMPORAL.
           READ ARCHIVO-TEMPORAL NEXT WITH NO LOCK AT END GO TO CERRAR-TEMPORAL.

           IF COD-PACI-W  = ID-PACI-TMP
              CONTINUE
           ELSE
              GO TO LEER-TEMPORAL
           END-IF

           IF ESTADO-TMP =  1
              CONTINUE
           ELSE
              GO TO LEER-TEMPORAL
           END-IF


           INITIALIZE TABLA-FORM-W CONT TABLA-EVO REG-EVO I TABLA-W SW-VAR K

           PERFORM LLENAR-VARIABLES VARYING I FROM 1 BY 1
                                             UNTIL I >  35

           ADD 1 TO CONT2
           MOVE ID-PACI-TMP     TO ID-EVO
           MOVE FOLIO-TMP       TO FOLIO-EVO
           MOVE FECHA-TMP       TO FECHA-EVO
           MOVE HORA-TMP        TO HORA-EVO
           MOVE OPER-TMP        TO OPER-ELAB-EVO.

      *     IF CONT2  = 2
      *        MOVE CONT            TO MSJ1-HTML
      *        MOVE DATOS-TMP (1)   TO MSJ2-HTML
      *        MOVE TAB-FOR-W (1)   TO MSJ3-HTML
      *       GO TO ENVIAR2-ERROR
      *     END-IF. 



       GRABAR-ADM-MEDICAMENTOS.

           OPEN I-O EVOLUCION-HISTORIA
                    EVOLUCION-DETALLADA.

           MOVE FUNCTION CURRENT-DATE TO FECHA-GRAB-EVO.
           ACCEPT HORA-GRAB-EVO  FROM TIME.


           READ EVOLUCION-HISTORIA WITH NO LOCK
                INVALID KEY 
                     INITIALIZE DATOS-EVO TABLA-EVO
                     MOVE 5            TO TIPO-EVO
                     MOVE MEDICO-W     TO MED-EVO
                     MOVE SERV-W       TO UNSERV-EVO
                     MOVE EDAD-W       TO EDAD-EVO
                     WRITE REG-EVO END-WRITE
      
                     MOVE TABLA-FORM-W TO TABLA-EVO
                     PERFORM GRABAR-TABLA-EVO
                NOT INVALID KEY
                     IF MN-TMP = 59

                        MOVE FECHA-TMP  TO FECHA-INI-W
                        MOVE HR-TMP     TO HR-INI-W
                        MOVE MN-TMP     TO MN-INI-W

                        INITIALIZE SG-INI-W DATO-RETURN ADICION

                        MOVE "T"        TO TIPO
                        MOVE 1          TO CANT

                        CALL "SC-RELOJ" USING DATO-INICIO-W ADICION DATO-RETURN SW-INVALID-W MENSAJE-INDALID-W

                        IF SW-INVALID-W > 0
                           MOVE SW-INVALID-W       TO MENSAJE1-HTML
                           MOVE MENSAJE-INDALID-W  TO MENSAJE2-HTML
                           MOVE "HC003A"           TO MENSAJE3-HTML
                           GO TO ENVIAR-ERROR
                        ELSE
                           MOVE FECHA-RETURN    TO FECHA-TMP
                           MOVE HORA-RETURN     TO HORA-TMP
                        END-IF
                     ELSE
                         ADD 1 TO MN-TMP
                     END-IF

                     CLOSE EVOLUCION-HISTORIA
                           EVOLUCION-DETALLADA
                     GO TO GRABAR-ADM-MEDICAMENTOS
           END-READ.

           CLOSE EVOLUCION-HISTORIA
                 EVOLUCION-DETALLADA.

           GO TO LEER-TEMPORAL.

       CERRAR-TEMPORAL.
           CLOSE ARCHIVO-TEMPORAL
                 ARCHIVO-POS.


       ABRIR-TMP.

           OPEN I-O ARCHIVO-TEMPORAL.

           MOVE ZEROS TO FECHA-TMP
           START ARCHIVO-TEMPORAL KEY >= FECHA-TMP
                 INVALID KEY CONTINUE 
           END-START.

       FINALIZAR-TMP.
           READ ARCHIVO-TEMPORAL NEXT WITH NO LOCK AT END GO TO CERRAR-TMP.

           IF COD-PACI-W  = ID-PACI-TMP
              CONTINUE
           ELSE
              GO TO FINALIZAR-TMP
           END-IF

           IF ESTADO-TMP =  1
              CONTINUE
           ELSE
              GO TO FINALIZAR-TMP
           END-IF

           MOVE 2 TO ESTADO-TMP
           REWRITE REG-TMP END-REWRITE


           GO TO FINALIZAR-TMP.

       CERRAR-TMP.

           CLOSE ARCHIVO-TEMPORAL.


10700  PAGINA-CONFIG.  
010720     MOVE "datosrecibidos" TO COBW3-CNV-NAME
010730     MOVE 00          TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3
010750
010760     MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
010770     PERFORM ABRIR-HTML.
007980   
007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.


       LLENAR-VARIABLES.

           INITIALIZE J

           IF DATOS-TMP (I) = SPACES OR LOW-VALUES
              MOVE 35   TO I
           ELSE
              MOVE DATOS-TMP (I)    TO TABLA-TMP-W
              ADD 1 TO CONT
              PERFORM LLENAR-ADM-MEDICAMENTOS VARYING J FROM 1 BY 1
                                                       UNTIL J > 900
           END-IF.

       LLENAR-ADM-MEDICAMENTOS.

           IF DATOS-TMP-W (J) = "|"
              ADD 1 TO SW-VAR

              EVALUATE SW-VAR
                 WHEN 1 MOVE TABLA-W    TO COD-FORMU-W (CONT)
                        MOVE FECHA-TMP  TO FECHA-FOR-W (CONT)
                        MOVE HORA-TMP   TO HORA-FOR-W  (CONT)

                        MOVE COD-FORMU-W (CONT)  TO LLAVE-POS

                        READ ARCHIVO-POS WITH NO LOCK
                             INVALID KEY 
                                     MOVE "99"        TO MENSAJE1-HTML
                                     MOVE COD-FORMU-W (CONT) TO MENSAJE2-HTML
                                     MOVE LLAVE-POS    TO MENSAJE3-HTML
                                     GO TO ENVIAR-ERROR
                        END-READ


                 WHEN OTHER 
                        IF GRP-W (I) = "MZ"
                           EVALUATE SW-VAR
                               WHEN 2  MOVE TABLA-W    TO ORDEN-MEZCLA-W       (CONT)
                               WHEN 3  MOVE TABLA-W    TO CC-CANT-MEZCLA-W     (CONT)
                               WHEN 4  MOVE TABLA-W    TO CC-TIT-MEZCLA-W      (CONT)
                               WHEN 5  MOVE TABLA-W    TO BASE-TIT-MEZCLA-W    (CONT)
                               WHEN 5  MOVE TABLA-W    TO PORC-BASE-MEZCLA-W   (CONT)
                               WHEN 6  MOVE TABLA-W    TO MAS-OTR-MEZCLA-W     (CONT)
                               WHEN 7  MOVE TABLA-W    TO CANT-MEDIC2-MEZCLA-W (CONT)
                               WHEN 8  MOVE TABLA-W    TO UNID-DOSIS-MEZCLA-W  (CONT)
                               WHEN 9  MOVE TABLA-W    TO MEDIC2-MEZCLA-W      (CONT)
                               WHEN 10 MOVE TABLA-W    TO CADA-MEZCLA-W        (CONT)
                               WHEN 11 MOVE TABLA-W    TO CANT-CADA-MEZCLA-W   (CONT)
                               WHEN 12 MOVE TABLA-W    TO UNID-FREC-MEZCLA-W   (CONT)
                               WHEN 13 MOVE TABLA-W    TO VIA-MEZCLA-W         (CONT)
                           END-EVALUATE
                        ELSE
                           EVALUATE SW-VAR
                               WHEN 2  MOVE TABLA-W   TO TIPO-DOSIF-W  (I)
                                    EVALUATE TIPO-DOSIF-W (1)
                                       WHEN  1  MOVE " C.C.        " TO UNID-DOSIS-W (CONT)
                                       WHEN  2  MOVE " Gramos      " TO UNID-DOSIS-W (CONT)
                                       WHEN  3  MOVE " Miligramos  " TO UNID-DOSIS-W (CONT)
                                       WHEN  4  MOVE " Microgramos " TO UNID-DOSIS-W (CONT)
                                       WHEN  5  MOVE " Tiempo      " TO UNID-DOSIS-W (CONT)
                                       WHEN  6  MOVE " Unidades    " TO UNID-DOSIS-W (CONT)
                                       WHEN  7  MOVE " U. Internac." TO UNID-DOSIS-W (CONT)
                                       WHEN  8  MOVE " Puff        " TO UNID-DOSIS-W (CONT)
                                       WHEN  9  MOVE " Gotas       " TO UNID-DOSIS-W (CONT)
                                       WHEN "A" MOVE " %           " TO UNID-DOSIS-W (CONT)
                                       WHEN "B" MOVE " Litros/min  " TO UNID-DOSIS-W (CONT)
                                       WHEN "C" MOVE " MCG/KL/MIN  " TO UNID-DOSIS-W (CONT)
                                       WHEN "D" MOVE " Tableta     " TO UNID-DOSIS-W (CONT)
                                       WHEN "E" MOVE " Cucharada   " TO UNID-DOSIS-W (CONT)
                                       WHEN "F" MOVE " Crema/ungu. " TO UNID-DOSIS-W (CONT)
                                       WHEN "G" MOVE " Ampolla     " TO UNID-DOSIS-W (CONT)
                                       WHEN "H" MOVE " Sobre       " TO UNID-DOSIS-W (CONT)
                                       WHEN "I" MOVE " MiliEquivale" TO UNID-DOSIS-W (CONT)
                                       WHEN "J" MOVE " Capsulas    " TO UNID-DOSIS-W (CONT)
                                       WHEN "K" MOVE " Locion      " TO UNID-DOSIS-W (CONT)
                                    END-EVALUATE
                               WHEN 3  MOVE TABLA-W   TO VIA-W         (CONT)
                               WHEN 4  MOVE TABLA-W   TO CANT-FORMU-W  (CONT)
                               WHEN 5  MOVE TABLA-W   TO ORDEN-DOSIS-W (CONT)
                               WHEN 6  MOVE TABLA-W   TO CANT-DOSIS-W  (CONT)
                               WHEN 7  MOVE TABLA-W   TO CADA-DOSIS-W  (CONT)
                               WHEN 8  MOVE TABLA-W   TO CANT-FREC-W   (CONT)
                               WHEN 9  MOVE TABLA-W   TO UNID-FREC-W   (CONT)
                               WHEN 10 MOVE TABLA-W   TO VIA-DOSIS-W   (CONT)
                                       MOVE 900        TO J
                                       INITIALIZE SW-VAR
                           END-EVALUATE
                        END-IF
              END-EVALUATE

              INITIALIZE TABLA-W K

           ELSE
               ADD 1 TO K
               MOVE DATOS-TMP-W (J)  TO CM-W(K)
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
005210 COPY "P:\PROG\PROYECT\FUENTES\WS-HCEVO.CBL".

0
