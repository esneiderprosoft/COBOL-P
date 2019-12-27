004820*=================================================================
004830* CONSULTA DE ARCHIVO ENFERMEDADES.
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "..\..\FUENTES\FS-USUNET.CBL".
004920     COPY "..\..\FUENTES\FS-SESION.CBL".

                SELECT ARCHIVO-PROFESIONALES LOCK MODE IS AUTOMATIC
                       ASSIGN NOM-PROF-W
                       ORGANIZATION IS INDEXED;
                       ACCESS MODE  IS DYNAMIC
                       RECORD KEY   IS COD-PROF
                       ALTERNATE RECORD KEY DESCRIP-PROF  WITH DUPLICATES
                       ALTERNATE RECORD KEY ATIENDE-PROF  WITH DUPLICATES
                       ALTERNATE RECORD KEY SUCURSAL-PROF WITH DUPLICATES
                       FILE STATUS IS OTR-STAT.

005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-PROFE.CBL".	   

005190 WORKING-STORAGE SECTION.
005200 COPY "..\..\FUENTES\COBW3.CBL".
005210 COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LINEA-LLEGADA-W               PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W             PIC X(15).
             03 FECHA-LLEGAD-W          PIC X(8).
             03 HORA-LLEGAD-W           PIC X(6).
          02 DIR-CONTAB-LLEGADA-W       PIC X(30).
          02 MES-CONTAB-LLEGADA-W       PIC XX.
          02 OPERADOR-LLEGADA-W         PIC X(4).
          02 COD-PROF-LLEGADA-W         PIC X(10).

       77 MES-SAL-W                     PIC X(4).
       77 NOM-PROF-W                    PIC X(70).

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV             PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 DESCRIP-PROF-ENV           PIC X(78).
          02 FILLER                     PIC X VALUE "|".
          02 ESTADO-PROF-ENV            PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CL1-PROF-ENV               PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CL2-PROF-ENV               PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CL3-PROF-ENV               PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CL4-PROF-ENV               PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CL5-PROF-ENV               PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CL6-PROF-ENV               PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CL7-PROF-ENV               PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 ESP1-PROF-ENV              PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESP2-PROF-ENV              PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESP3-PROF-ENV              PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESP4-PROF-ENV              PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESP5-PROF-ENV              PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESP6-PROF-ENV              PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESP7-PROF-ENV              PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESP8-PROF-ENV              PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESP9-PROF-ENV              PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESP10-PROF-ENV             PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 DIV-PROF-ENV               PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 ATIENDE-PROF-ENV           PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 REG-MED-PROF-ENV           PIC X(10).


006310 LINKAGE SECTION.
006320 COPY "..\..\FUENTES\ISAPICTX.CBL".
006330 PROCEDURE DIVISION WITH stdcall LINKAGE USING ISAPI-CTX-CNT.
006340
006350 DECLARATIVES.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUNET.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USU-W                TO MSJ2-HTML
              MOVE "SAL41-11"               TO MSJ3-HTML
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
              MOVE "SAL41-11"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PROFESIONALES.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PROF-W               TO MSJ2-HTML
              MOVE "SAL41-11"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

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
006820     MOVE COBW3-GET-DATA    TO LINEA-LLEGADA-W.

           UNSTRING LINEA-LLEGADA-W DELIMITED BY "|"
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, 
                    OPERADOR-LLEGADA-W, COD-PROF-LLEGADA-W
            END-UNSTRING.

001855     MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.

000797 INICIAR-SESION.
000806     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.
006840
006900 ABRIR-USUARIO.
006910     INITIALIZE OTR-STAT.
006920     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
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
007040                  GO TO ENVIAR2-ERROR
007050       WHEN OTHER GO TO CERRAR-SESION
007060     END-EVALUATE.
007070
007080     INITIALIZE LLAVE-USUNET.

007100
007110 LEER-USUARIO.
007120     READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
007140     CLOSE ARCHIVO-USUNET.
007200     
007210     IF NOMBRE-USUNET = SPACES
007230        MOVE "Validacion de Sesion"        TO MENSAJE1-HTML
007240        MOVE "Falta configurar usuarios"   TO MENSAJE2-HTML
007250        MOVE "Sc"                          TO MENSAJE3-HTML
007260        GO TO ENVIAR2-ERROR
007270     END-IF.
007280 
007350

        ASIGNAR-NOMBRES.
           EVALUATE MES-CONTAB-LLEGADA-W
             WHEN 01 MOVE "\ENE" TO MES-SAL-W 
             WHEN 02 MOVE "\FEB" TO MES-SAL-W 
             WHEN 03 MOVE "\MAR" TO MES-SAL-W 
             WHEN 04 MOVE "\ABR" TO MES-SAL-W 
             WHEN 05 MOVE "\MAY" TO MES-SAL-W 
             WHEN 06 MOVE "\JUN" TO MES-SAL-W 
             WHEN 07 MOVE "\JUL" TO MES-SAL-W 
             WHEN 08 MOVE "\AGT" TO MES-SAL-W 
             WHEN 09 MOVE "\SEP" TO MES-SAL-W 
             WHEN 10 MOVE "\OCT" TO MES-SAL-W 
             WHEN 11 MOVE "\NOV" TO MES-SAL-W 
             WHEN 12 MOVE "\DIC" TO MES-SAL-W 
           END-EVALUATE.
           
        ASIGNAR-NOMBRES. 
           INITIALIZE NOM-PROF-W 

           MOVE "\\" TO NOM-PROF-W 
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".
           INSPECT NOM-PROF-W REPLACING FIRST "                    "
                                           BY IP-DATOS-USUNET
           INSPECT NOM-PROF-W REPLACING FIRST " "
                                           BY "\"
           INSPECT NOM-PROF-W REPLACING FIRST "                              "
                                           BY DIR-CONTAB-LLEGADA-W.
           INSPECT NOM-PROF-W REPLACING FIRST "                        "
                                           BY "\CONTROL\SC-ARCHPROF.DAT".

       ABRIR-PROF.
           OPEN INPUT ARCHIVO-PROFESIONALES.
           
       LEER-PROF.
           MOVE COD-PROF-LLEGADA-W   TO COD-PROF
           READ ARCHIVO-PROFESIONALES WITH NO LOCK
                INVALID KEY 
                  MOVE "01" TO SW-INVALID-ENV               
                NOT INVALID KEY
                  MOVE "00" TO SW-INVALID-ENV
           END-READ.
           
           MOVE DESCRIP-PROF  TO DESCRIP-PROF-ENV
           MOVE ESTADO-PROF   TO ESTADO-PROF-ENV
           MOVE CL-PROF (1)   TO CL1-PROF-ENV
           MOVE CL-PROF (2)   TO CL2-PROF-ENV
           MOVE CL-PROF (3)   TO CL3-PROF-ENV
           MOVE CL-PROF (4)   TO CL4-PROF-ENV
           MOVE CL-PROF (5)   TO CL5-PROF-ENV
           MOVE CL-PROF (6)   TO CL6-PROF-ENV
           MOVE CL-PROF (7)   TO CL7-PROF-ENV
           MOVE ESP-PROF (1)  TO ESP1-PROF-ENV
           MOVE ESP-PROF (2)  TO ESP2-PROF-ENV
           MOVE ESP-PROF (3)  TO ESP3-PROF-ENV
           MOVE ESP-PROF (4)  TO ESP4-PROF-ENV
           MOVE ESP-PROF (5)  TO ESP5-PROF-ENV
           MOVE ESP-PROF (6)  TO ESP6-PROF-ENV
           MOVE ESP-PROF (7)  TO ESP7-PROF-ENV
           MOVE ESP-PROF (8)  TO ESP8-PROF-ENV
           MOVE ESP-PROF (9)  TO ESP9-PROF-ENV
           MOVE ESP-PROF (10) TO ESP10-PROF-ENV
           MOVE DIV-PROF      TO DIV-PROF-ENV
           MOVE REG-MED-PROF  TO REG-MED-PROF-ENV
           MOVE ATIENDE-PROF  TO ATIENDE-PROF-ENV.
           
        CERRAR-ARCHIVOS.
           CLOSE ARCHIVO-PROFESIONALES.

010700  PAGINA-CONFIG.
010720     MOVE "datosrecibidos"    TO COBW3-CNV-NAME
010730     MOVE DATOS-ENVIO         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3
010750
010760     MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
010770     PERFORM ABRIR-HTML.
007980   
007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.
008040 COPY "..\..\FUENTES\SC-WEB.CBL".
005210* COPY "..\..\FUENTES\WS-HCEVO.CBL".
