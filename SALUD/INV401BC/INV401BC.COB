      *=================================================================
      * BUSCA VALOR CUPS - INV401BC
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
                  ASSIGN TO NOM-USUAR-W
      * "../CONTROL/SC-ARCHUSU.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-TABLAS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TAB-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS LLAVE-TAB
                  ALTERNATE RECORD KEY IS DESCRIP-TAB   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS COD-RIPS-TAB  WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-TARIFAS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TAR-W
      * "../CONTROL/SC-ARCHTAR.DAT"
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS COD-TAR.


           SELECT ARCHIVO-NUMERACION LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-NUMER-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-NUM
		  ALTERNATE RECORD KEY NIT-NUM         WITH DUPLICATES
                  ALTERNATE RECORD KEY DESCRIP-NUM     WITH DUPLICATES
                  ALTERNATE RECORD KEY NOMBRE-PAC-NUM  WITH DUPLICATES
                  ALTERNATE RECORD KEY HAB-NUM         WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-ING-NUM   WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-RET-NUM   WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.
           

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-TABLA.CBL".
       COPY "..\..\FUENTES\FD-TARIF.CBL".
       COPY "..\..\FUENTES\FD-NUMER.CBL".

       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".
    
       77 NOM-USUAR-W                  PIC X(70).
       77 NOM-TAB-W                    PIC X(70).
       77 NOM-TAR-W                    PIC X(70).
       77 NOM-NUMER-W                  PIC X(70).
       77 MES-SAL-W                    PIC X(4).

       01  LLAVE-TIPO-W.
           02  COD-TAB-W               PIC XX.
           02  TIPO-TAB-W              PIC 9.
      * 01  OPCION-1                    PIC X.
       01  SAL-MIN-W                   PIC 9(6)V99.
       01  SW-CL                       PIC 9.
       01  SW-APR                      PIC 9(4).
       01  VLR-UNIT-W                  PIC 9(9)V99.
       01  VALOR-APROX                 PIC 9(9).
       01  FACTOR-W                    PIC 9V9(3).
       01  VALORES-CIR-W.
           02 VLR-CIRU-W               PIC 9(9).
           02 VLR-AYUD-W               PIC 9(9).
           02 VLR-ANES-W               PIC 9(9).
           02 VLR-MAT-W                PIC 9(9).
           02 VLR-SALA-W               PIC 9(9).
           02 VLR-ART-W                PIC 9(9).

       01 LLEGADA-W                    PIC X(100).
      
       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(20).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 CTA-LLEGA.
             03 PREFIJO-LLEGA          PIC X.
             03 NRO-CTA-LLEGA          PIC 9(06).
          02 CL-LLEGA                  PIC 9.
          02 CUP-LLEGA.
             03 GRUPO-LLEGA            PIC XX.
             03 COD-CUP-LLEGA          PIC X(10).
          02 VLR-ART-LLEGA             PIC 9(9).
      
       01 DATOS-ENVIO.
          02 ESTADO-ENV                   PIC XX.
          02 FILLER                       PIC X VALUE "|". 
          02 VLR-ENV                      PIC 9(9).

         
               
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
              MOVE "INV401BC"               TO MSJ3-HTML
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
              MOVE "INV401BC"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USUAR-W              TO MSJ2-HTML
              MOVE "INV401BC"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TABLAS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-TAB-W                TO MSJ2-HTML
              MOVE "INV401BC"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TARIFAS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-TAR-W                TO MSJ2-HTML
              MOVE "INV401BC"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-NUMERACION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-NUMER-W              TO MSJ2-HTML
              MOVE "INV401BC"               TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, CTA-LLEGA, CL-LLEGA,
                   CUP-LLEGA, VLR-ART-LLEGA                   
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.


       INICIAR-SESION.
           MOVE "D:\PROGELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
          
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.

       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\PROGELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
      
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

           MOVE "\\" TO NOM-USUAR-W

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET
           INSPECT NOM-USUAR-W REPLACING FIRST " "
                                            BY "\"
           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                            BY  DIR-CONTAB-LLEGADA-W. 
           MOVE NOM-USUAR-W TO NOM-TAR-W 
                               NOM-NUMER-W
                               NOM-TAB-W
                 
           INSPECT NOM-USUAR-W REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHUSU.DAT".

           INSPECT NOM-TAR-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-ARCHTAR.DAT".

           INSPECT NOM-NUMER-W REPLACING FIRST "                     "
                                            BY "\CONTROL\SC-NUMER.DAT".

           INSPECT NOM-TAB-W REPLACING FIRST "                     "
                                          BY "\CONTROL\SC-TABLA.DAT".


       ABRIR-USUARIO.

           OPEN INPUT  ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                MOVE 0 TO OPCION-1
           END-READ
           CLOSE ARCHIVO-USUARIOS.

           COMPUTE SAL-MIN-W ROUNDED = SAL-MIN-USU / 30.
           INITIALIZE SAL-MIN-W SW-CL SW-APR VLR-UNIT-W VALOR-APROX FACTOR-W 
                      VALORES-CIR-W VLR-ART-LLEGA.

           IF CL-LLEGA = 0
              GO TO CERRAR-ARCHIVOS
           END-IF.

       ABRIR-ARCHIVOS.
           OPEN INPUT ARCHIVO-TABLAS
                      ARCHIVO-TARIFAS.

       UBICAR-CUENTA.

           IF PREFIJO-LLEGA = "E" OR "C"
              MOVE 0         TO ESTADO-NUM
              MOVE "CL"      TO CONVENIO-NUM
           ELSE
              MOVE PREFIJO-LLEGA  TO PREFIJO-NUM
              MOVE NRO-CTA-LLEGA  TO NRO-NUM
              OPEN INPUT ARCHIVO-NUMERACION
              READ ARCHIVO-NUMERACION WITH NO LOCK
                   INVALID KEY
                       MOVE "CL" TO CONVENIO-NUM
              END-READ
              CLOSE ARCHIVO-NUMERACION
           END-IF.

       LEER-CONVENIO.
           MOVE CONVENIO-NUM TO COD-TAR.
           READ ARCHIVO-TARIFAS WITH NO LOCK
                INVALID KEY 
                GO TO CERRAR-ARCHIVOS
           END-READ

           IF CL-LLEGA  = 7
              MOVE 5       TO SW-CL
           ELSE
              MOVE CL-LLEGA  TO SW-CL
           END-IF

           IF SAL-MIN-TAR IS ZERO
           OR SAL-MIN-TAR IS NOT NUMERIC
              MOVE SAL-MIN-W TO SAL-MIN-TAR
           END-IF

           MOVE CL-LLEGA                  TO TIPO-TAB-W
           MOVE COD-TAB-TAR (SW-CL)       TO COD-TAB-W.

       LEER-TARIFA.
           MOVE LLAVE-TIPO-W    TO LLAVE-TIPO-TAB
           MOVE CUP-LLEGA       TO COD-SER-TAB
           READ ARCHIVO-TABLAS WITH NO LOCK
                INVALID KEY
                GO TO CERRAR-ARCHIVOS
           END-READ


           IF (COD-TAB = "I4" OR "IS")
              AND PREFIJO-LLEGA = "P"
              AND GR-SER-TAB = 93
               IF CD-SER-TAB = "1000" OR "9400" OR "8300" OR "7000"
                  COMPUTE MONTO-TAB = MONTO-TAB * 1.1
               END-IF
           END-IF

           EVALUATE FORMA-LIQ-TAB
           WHEN 1     MOVE 1 TO SW-APR
                      COMPUTE VLR-UNIT-W ROUNDED =
                              MONTO-TAB * HN-QUIR-TAR (30)
           WHEN 2     MOVE 100 TO SW-APR
                      MOVE MONTO-TAB TO VLR-UNIT-W
           WHEN 4     MOVE 100 TO SW-APR
                      COMPUTE VLR-UNIT-W ROUNDED =
                              MONTO-TAB * SAL-MIN-TAR
           WHEN 5     MOVE 1 TO SW-APR
                      COMPUTE VLR-UNIT-W ROUNDED =
                              MONTO-TAB * VLR-ART-W / 100
           WHEN OTHER MOVE 1 TO SW-APR
                      MOVE MONTO-TAB TO VLR-UNIT-W
           END-EVALUATE.


       CALCULAR-MONTO.
           PERFORM  BUSCAR-INCREMENTO

           IF CL-LLEGA  = 1
              PERFORM DATO-HONORARIOS
           ELSE
              COMPUTE VLR-ART-W  ROUNDED = VLR-UNIT-W * FACTOR-W

           MOVE VLR-ART-W TO VLR-ART-LLEGA
           END-IF.

       CERRAR-ARCHIVOS.
           CLOSE ARCHIVO-TABLAS
                 ARCHIVO-TARIFAS.
     
       ENVIO-DATOS.

           MOVE "00"             TO ESTADO-ENV
           MOVE VLR-ART-LLEGA    TO VLR-ENV.

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

       BUSCAR-INCREMENTO.
           IF INCREM-TAB IS ZERO
              IF CL-LLEGA = 7
                 EVALUATE GRUPO-LLEGA
                 WHEN 90 MOVE 2 TO INCREM-TAB
                 WHEN 87 MOVE 3 TO INCREM-TAB
                 WHEN 88 MOVE 3 TO INCREM-TAB
                 WHEN 89 MOVE 5 TO INCREM-TAB
                 WHEN OTHER
                      IF GRUPO-LLEGA < 87
                         MOVE 1 TO INCREM-TAB
                      ELSE
                         MOVE 4 TO INCREM-TAB
                      END-IF
                 END-EVALUATE
              ELSE
                 MOVE CL-LLEGA  TO INCREM-TAB
              END-IF
           END-IF

           IF SAL-MIN-TAR IS ZERO
           OR SAL-MIN-TAR IS NOT NUMERIC
              MOVE SAL-MIN-W TO SAL-MIN-TAR
           END-IF.

           IF INCREM-TAB = 9
              MOVE 1 TO FACTOR-W
           ELSE
              COMPUTE FACTOR-W ROUNDED =
                      PORC-TAB-TAR (INCREM-TAB) / 100
           END-IF

           IF FORMA-LIQ-TAB = 5
              MOVE 1 TO FACTOR-W
           END-IF.

       DATO-HONORARIOS.
            MOVE 0   TO VLR-ART-W.
            IF  COD-TAB  = "SO"
            AND FORMA-LIQ-TAB = 2
                MOVE 4 TO FORMA-LIQ-TAB
            END-IF

            EVALUATE FORMA-LIQ-TAB
            WHEN 1   COMPUTE VLR-CIRU-W ROUNDED =
                             MONTO-TAB * HN-QUIR-TAR (30)
                     COMPUTE VLR-AYUD-W ROUNDED =
                             MONTO-TAB * HN-AYUD-TAR (30)
                     COMPUTE VLR-ANES-W ROUNDED =
                             MONTO-TAB * HN-ANES-TAR (30)
                     IF MONTO-TAB <  21 MOVE 01 TO J ELSE
                     IF MONTO-TAB <  31 MOVE 02 TO J ELSE
                     IF MONTO-TAB <  41 MOVE 03 TO J ELSE
                     IF MONTO-TAB <  51 MOVE 04 TO J ELSE
                     IF MONTO-TAB <  61 MOVE 05 TO J ELSE
                     IF MONTO-TAB <  71 MOVE 06 TO J ELSE
                     IF MONTO-TAB <  81 MOVE 07 TO J ELSE
                     IF MONTO-TAB <  91 MOVE 08 TO J ELSE
                     IF MONTO-TAB < 101 MOVE 09 TO J ELSE
                     IF MONTO-TAB < 111 MOVE 10 TO J ELSE
                     IF MONTO-TAB < 131 MOVE 11 TO J ELSE
                     IF MONTO-TAB < 151 MOVE 12 TO J ELSE
                     IF MONTO-TAB < 171 MOVE 13 TO J ELSE
                     IF MONTO-TAB < 201 MOVE 14 TO J ELSE
                     IF MONTO-TAB < 231 MOVE 15 TO J ELSE
                     IF MONTO-TAB < 261 MOVE 16 TO J ELSE
                     IF MONTO-TAB < 291 MOVE 17 TO J ELSE
                     IF MONTO-TAB < 321 MOVE 18 TO J ELSE
                     IF MONTO-TAB < 351 MOVE 19 TO J ELSE
                     IF MONTO-TAB < 381 MOVE 20 TO J ELSE
                     IF MONTO-TAB < 411 MOVE 21 TO J ELSE
                                        MOVE 22 TO J
                     END-IF END-IF END-IF END-IF END-IF END-IF
                     END-IF END-IF END-IF END-IF END-IF END-IF
                     END-IF END-IF END-IF END-IF END-IF END-IF
                     END-IF END-IF END-IF
                     MOVE DR-SALA-TAR (J) TO VLR-SALA-W
                     MOVE MAT-QUI-TAR (J) TO VLR-MAT-W
            WHEN 2   MOVE MONTO-TAB TO J
                     IF J > 29 OR < 1
                        MOVE 1 TO J
                     END-IF
                     MOVE HN-QUIR-TAR (J) TO VLR-CIRU-W
                     MOVE HN-AYUD-TAR (J) TO VLR-AYUD-W
                     MOVE HN-ANES-TAR (J) TO VLR-ANES-W
                     MOVE MAT-QUI-TAR (J) TO VLR-MAT-W
                     MOVE DR-SALA-TAR (J) TO VLR-SALA-W
            WHEN 4   MOVE MONTO-TAB TO J
                     IF J > 29 OR < 1
                        MOVE 1 TO J
                     END-IF
                     COMPUTE VLR-CIRU-W ROUNDED =
                             HN-QUIR-TAR (J) * SAL-MIN-TAR
                     COMPUTE VLR-AYUD-W ROUNDED =
                             HN-AYUD-TAR (J) * SAL-MIN-TAR
                     COMPUTE VLR-ANES-W ROUNDED =
                             HN-ANES-TAR (J) * SAL-MIN-TAR
                     COMPUTE VLR-MAT-W ROUNDED =
                             MAT-QUI-TAR (J) * SAL-MIN-TAR
                     COMPUTE VLR-SALA-W ROUNDED =
                             DR-SALA-TAR (J) * SAL-MIN-TAR
            WHEN OTHER INITIALIZE VALORES-CIR-W.

            IF COD-TAR = "H4"
               COMPUTE VALOR-APROX  ROUNDED = VLR-CIRU-W / SW-APR
               COMPUTE VLR-CIRU-W   ROUNDED = VALOR-APROX * SW-APR
               COMPUTE VALOR-APROX  ROUNDED = VLR-AYUD-W / SW-APR
               COMPUTE VLR-AYUD-W   ROUNDED = VALOR-APROX * SW-APR
               COMPUTE VALOR-APROX  ROUNDED = VLR-ANES-W / SW-APR
               COMPUTE VLR-ANES-W   ROUNDED = VALOR-APROX * SW-APR
               COMPUTE VALOR-APROX  ROUNDED = VLR-MAT-W / SW-APR
               COMPUTE VLR-MAT-W    ROUNDED = VALOR-APROX * SW-APR

               COMPUTE VALOR-APROX  ROUNDED = VLR-SALA-W / SW-APR
               COMPUTE VLR-SALA-W   ROUNDED = VALOR-APROX * SW-APR

               COMPUTE VLR-CIRU-W   ROUNDED = VLR-CIRU-W * FACTOR-W
               COMPUTE VLR-AYUD-W   ROUNDED = VLR-AYUD-W * FACTOR-W
               COMPUTE VLR-ANES-W   ROUNDED = VLR-ANES-W * FACTOR-W
               IF FACTOR-W < 1
                  COMPUTE VLR-MAT-W  ROUNDED = VLR-MAT-W  * FACTOR-W
                  COMPUTE VLR-SALA-W ROUNDED = VLR-SALA-W * FACTOR-W
               ELSE
                  COMPUTE VLR-MAT-W  ROUNDED = VLR-MAT-W   * 1
                  COMPUTE VLR-SALA-W ROUNDED = VLR-SALA-W  * 1
               END-IF
            ELSE
               COMPUTE VLR-CIRU-W ROUNDED = VLR-CIRU-W * FACTOR-W
               COMPUTE VLR-AYUD-W ROUNDED = VLR-AYUD-W * FACTOR-W
               COMPUTE VLR-ANES-W ROUNDED = VLR-ANES-W * FACTOR-W
               IF FACTOR-W < 1
                  COMPUTE VLR-MAT-W ROUNDED = VLR-MAT-W  * FACTOR-W
                  COMPUTE VLR-SALA-W ROUNDED = VLR-SALA-W * FACTOR-W
               ELSE
                  COMPUTE VLR-MAT-W  ROUNDED = VLR-MAT-W  * 1
                  COMPUTE VLR-SALA-W ROUNDED = VLR-SALA-W  * 1
               END-IF
            END-IF

            COMPUTE VLR-ART-W = VLR-CIRU-W
                              + VLR-AYUD-W
                              + VLR-ANES-W
                              + VLR-MAT-W
                              + VLR-SALA-W.
              
       COPY "..\..\FUENTES\SC-WEB19.CBL".
