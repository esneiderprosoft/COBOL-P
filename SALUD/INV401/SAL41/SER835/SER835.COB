004820*=================================================================
004830* COMPROBANTES POR PACIENTE.
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "..\..\FUENTES\FS-USUNET.CBL".
004920     COPY "..\..\FUENTES\FS-SESION.CBL".

           SELECT FACTURA-SERVICIOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-SAL-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-FACT
                  ALTERNATE RECORD KEY NRO-FACT   WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-FACT WITH DUPLICATES
                  ALTERNATE RECORD KEY CTA-FACT   WITH DUPLICATES
                  ALTERNATE RECORD KEY NIT-FACT   WITH DUPLICATES
                  ALTERNATE RECORD KEY ID-HISTORIA-FACT WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-ING-ESTAD WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-SAL-ESTAD WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-ENTR-RX   WITH DUPLICATES				  
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-TERCEROS  LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TER-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-TERCERO
                  ALTERNATE RECORD KEY DESCRIP-TER WITH DUPLICATES
                  ALTERNATE RECORD KEY ACT-TER     WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-JSON LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-USUA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-SALUD.CBL".
       COPY "..\..\FUENTES\FD-TERCE.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON                PIC X(500).	   

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
          02 ID-FACT-LLEGADA-W          PIC X(15).
          02 CL-W                       PIC X.

       77 MES-SAL-W                     PIC X(4).
       77 NOM-SAL-W                     PIC X(70).
       77 NOM-USUA-W                    PIC X(70).
       77 NOM-TER-W                     PIC X(70).
       77 NOM-PLANO-W                   PIC X(70).

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV             PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 NOMBRE-JSON-ENV            PIC X(28).

       01 LIN-1.
          02 FILLER                     PIC X(16)  VALUE "{*FECHA_IESTAD*:".
          02 FILLER                     PIC X      VALUE "*".
          02 FECHA-ING-ESTD-J           PIC X(10).
          02 FILLER                     PIC XX     VALUE "*,".
          02 FILLER                     PIC X(11)  VALUE "*CTA_FACT*:".
          02 FILLER                     PIC X      VALUE "*".    
          02 CTA-FACT-J                 PIC X(7).
          02 FILLER                     PIC XX     VALUE "*,".
          02 FILLER                     PIC X(11)  VALUE "*ART_FACT*:".
          02 FILLER                     PIC X      VALUE "*". 
          02 ART-FACT-J                 PIC X(17).
          02 FILLER                     PIC XX     VALUE "*,".
          02 FILLER                     PIC X(15)  VALUE "*DETALLE_FACT*:".
          02 FILLER                     PIC X      VALUE "*". 
          02 DETALLE-FACT-J             PIC X(10).
          02 FILLER                     PIC XX     VALUE "*,".
          02 FILLER                     PIC X(13)  VALUE "*MEDIC_FACT*:".
          02 FILLER                     PIC X      VALUE "*". 
          02 MEDIC-FACT-J               PIC X(30).
          02 FILLER                     PIC XX     VALUE "*,".
          02 FILLER                     PIC X(14)  VALUE "*COPAGO_FACT*:".
          02 FILLER                     PIC X      VALUE "*". 
          02 COPAGO-ESTIM-FACT-J        PIC X.
          02 FILLER                     PIC XX     VALUE "*,".
          02 FILLER                     PIC X(14)  VALUE "*TIPODR_FACT*:".
          02 FILLER                     PIC X      VALUE "*". 
          02 TIPO-DR-FACT-J             PIC X.
          02 FILLER                     PIC XX     VALUE "*,".
          02 FILLER                     PIC X(12)  VALUE "*DIAG1_CUP*:".
          02 FILLER                     PIC X      VALUE "*". 
          02 DIAG1-CUP-J                PIC X(5).
          02 FILLER                     PIC XX     VALUE "*,".
          02 FILLER                     PIC X(13)  VALUE "*LLAVE_FACT*:".
          02 FILLER                     PIC X      VALUE "*". 
          02 LLAVE-FACT-J               PIC X(9).
          02 FILLER                     PIC XX     VALUE "*,".
          02 FILLER                     PIC X(11)  VALUE "*REF_FACT*:".
          02 FILLER                     PIC X      VALUE "*".           
          02 REF-FACT-J                 PIC X(6).
          02 FILLER                     PIC XX     VALUE "*,".
          02 FILLER                     PIC X(12)  VALUE "*FINAL_ANT*:".
          02 FILLER                     PIC X      VALUE "*". 
          02 FINAL-ANT-J                PIC XX.

          02 FILLER                     PIC XX     VALUE "*}".    
          02 CIERRE-LIN-1               PIC X. 


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
              MOVE "SER835"                 TO MSJ3-HTML
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
              MOVE "SER835"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON FACTURA-SERVICIOS.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-SAL-W                TO MSJ2-HTML
              MOVE "SER835"                 TO MSJ3-HTML
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
              MOVE "SER835"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PLANO-W              TO MSJ2-HTML
              MOVE "SER835"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USU-W                TO MSJ2-HTML
              MOVE "SER835"                 TO MSJ3-HTML
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
                    OPERADOR-LLEGADA-W, ID-FACT-LLEGADA-W
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
007140     CLOSE ARCHIVO-USUNET.
007200     
007210     IF NOMBRE-USUNET = SPACES
007230        MOVE "Validacion de Sesion"        TO MSJ1-HTML
007240        MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007250        MOVE "Sc"                          TO MsJ3-HTML
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
           END-EVALUATE    

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

001614     INITIALIZE NOM-SAL-W

           MOVE "\\" TO NOM-SAL-W

           INSPECT NOM-SAL-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET
001621     INSPECT NOM-SAL-W REPLACING FIRST " "
001624                                    BY "\"
007190     INSPECT NOM-SAL-W REPLACING FIRST "                              "
                                          BY DIR-CONTAB-LLEGADA-W
           INSPECT NOM-SAL-W REPLACING FIRST "                     "
007300                                    BY "\CONTROL\SC-FACSA.DAT".
           MOVE NOM-SAL-W TO NOM-TER-W
           INSPECT NOM-TER-W REPLACING FIRST "SC-FACSA.DAT  "
                                          BY "SC-ARCHTER.DAT".

           MOVE NOM-SAL-W TO NOM-USUA-W
           INSPECT NOM-USUA-W REPLACING FIRST "SC-FACSA.DAT  "
                                           BY "SC-ARCHUSU.DAT".

           MOVE OPERADOR-LLEGADA-W TO OPER-JSON-W
           MOVE FUNCTION CURRENT-DATE TO FECHA-JSON-W
           ACCEPT HORA-JSON-W FROM TIME

           MOVE NOMBRE-JSON-W      TO NOM-PLANO-W
           MOVE NOM-JSON-W         TO NOMBRE-JSON-ENV
           MOVE "00"               TO SW-INVALID-ENV.

        ABRIR-ARCHIVOS.
           OPEN INPUT FACTURA-SERVICIOS
                      ARCHIVO-USUARIOS
                      ARCHIVO-TERCEROS.

        ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.

           MOVE "{*FACTURAS*:[" TO DATOS-JSON
           INSPECT DATOS-JSON  REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON  END-WRITE
           MOVE 0 TO SW-FIN.

        ABRIR-USUARIO.
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END GO TO BUSCAR-FACTURA
           END-READ.

        BUSCAR-FACTURA.
           READ ARCHIVO-USUARIOS WITH NO LOCK
           END-READ.

           MOVE ID-FACT-LLEGADA-W   TO ID-HISTORIA-FACT
           START FACTURA-SERVICIOS KEY = ID-HISTORIA-FACT
                 INVALID KEY 
                   MOVE "01" TO SW-INVALID-ENV
                   GO TO CERRAR-JSON.

        ESCRIBIR-JSON-FACTURA.
           READ FACTURA-SERVICIOS NEXT WITH NO LOCK AT END
                 GO TO CERRAR-JSON
           END-READ.

           IF ID-HISTORIA-FACT = ID-FACT-LLEGADA-W

              MOVE FECHA-ING-ESTAD TO FECHA-ING-ESTD-J
              MOVE CTA-FACT        TO CTA-FACT-J
              MOVE ART-FACT (1)    TO ART-FACT-J
              MOVE DETALLE-FACT    TO DETALLE-FACT-J
           
              IF MED-OTR-FACT IS ZERO
                 MOVE REMITE-FACT TO MED-OTR-FACT
              END-IF

              IF MED-OTR-FACT IS ZERO
                 MOVE SPACES TO DESCRIP-TER
              ELSE
                 MOVE MED-OTR-FACT TO COD-TER
                 READ ARCHIVO-TERCEROS WITH NO LOCK
                      INVALID KEY
                        MOVE COD-TER TO DESCRIP-TER
                 END-READ
              END-IF

              MOVE DESCRIP-TER     TO MEDIC-FACT-J

              IF CL-FACT = 0
                 MOVE ALM-FACT       (1) TO DIAG1-CUP-J
              ELSE
                 MOVE COD-DIAG-ESTAD (1) TO DIAG1-CUP-J
              END-IF
               
              MOVE TIPO-DR-FACT      TO TIPO-DR-FACT-J
              MOVE COPAGO-ESTIM-FACT TO COPAGO-ESTIM-FACT-J
              MOVE LLAVE-FACT        TO LLAVE-FACT-J

              IF  NIT-USU = 800162035
              AND CL-W = 7
              AND ART-FACT (1) = "890201"
              AND FECHA-FACT > 120430
              AND COD-DIAG-ESTAD (1) = "Z340" OR "Z348" OR "Z349" OR
                                       "Z350" OR "Z351" OR "Z352" OR
                                       "Z353" OR "Z354" OR "Z355" OR
                                       "Z356" OR "Z357" OR "Z358" OR
                                       "Z359"
                  MOVE "890301" TO REF-FACT-J
                  MOVE "06" TO FINAL-ANT-J
              ELSE
                  MOVE SPACES TO REF-FACT-J
                  MOVE SPACES TO FINAL-ANT-J
              END-IF

              INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
              MOVE "," TO CIERRE-LIN-1

              MOVE LIN-1 TO DATOS-JSON
              WRITE REG-JSON END-WRITE

              GO TO ESCRIBIR-JSON-FACTURA
           ELSE
              GO TO ESCRIBIR-JSON-FACTURA
           END-IF.


        CERRAR-JSON.
           INITIALIZE LIN-1
           MOVE LIN-1 TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
          
           MOVE "]}" TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE.

        CERRAR-ARCHIVOS.
           CLOSE FACTURA-SERVICIOS
                 ARCHIVO-TERCEROS
                 ARCHIVO-JSON.

010700  PAGINA-CONFIG.
010720      MOVE "datosrecibidos"    TO COBW3-CNV-NAME
010730      MOVE DATOS-ENVIO         TO COBW3-CNV-VALUE
010740      CALL "COBW3_SET_CNV" USING COBW3
010750
010760      MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
010770      PERFORM ABRIR-HTML.
007980   
007990 CERRAR-SESION.
008000      CALL "COBW3_FREE" USING COBW3.
008010      MOVE 1 TO PROGRAM-STATUS.
008020      EXIT PROGRAM.

008030
008040 COPY "..\..\FUENTES\SC-WEB.CBL".
005210* COPY "..\..\FUENTES\WS-HCEVO.CBL".
