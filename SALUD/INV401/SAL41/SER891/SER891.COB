004820*=================================================================
004830* ACTUALIZA CITAS FACTURADAS
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "..\..\FUENTES\FS-USUNET.CBL".
004920     COPY "..\..\FUENTES\FS-SESION.CBL".


           SELECT  ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                   ASSIGN TO NOM-USUA-W
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC;
                   RECORD KEY IS NOMBRE-USU
                   ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                   ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                   ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.

           SELECT  ARCHIVO-CITAS LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-CITAS-W
                   ORGANIZATION INDEXED;
                   ACCESS MODE  DYNAMIC;
                   RECORD KEY LLAVE-CIT
                   ALTERNATE RECORD KEY IS FECHA-CIT        WITH DUPLICATES
                   ALTERNATE RECORD KEY IS MEDICO-CIT       WITH DUPLICATES
                   ALTERNATE RECORD KEY IS PACI-CIT         WITH DUPLICATES
                   ALTERNATE RECORD KEY IS LLAVE-ALT-CIT    WITH DUPLICATES
                   ALTERNATE RECORD KEY IS ATIENDE-PROF-CIT WITH DUPLICATES				  
                   ALTERNATE RECORD KEY IS FECHA-FACTU-CIT  WITH DUPLICATES
                   ALTERNATE RECORD KEY IS FECHA-SOLIC-CIT  WITH DUPLICATES				  
                   FILE STATUS IS OTR-STAT.

           SELECT  FACTURA-SERVICIOS LOCK MODE IS AUTOMATIC
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

           SELECT  ARCHIVO-PACIENTES LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-PACIE-W
                   ORGANIZATION IS INDEXED;
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS COD-PACI
                   ALTERNATE RECORD KEY  DESCRIP-PACI WITH DUPLICATES
                   ALTERNATE RECORD KEY  EPS-PACI WITH DUPLICATES
                   ALTERNATE RECORD KEY  ID-COTIZ-PACI WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.

           SELECT  ARCHIVO-TERCEROS  LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-TER-W
                   ORGANIZATION IS INDEXED;
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS COD-TERCERO
                   ALTERNATE RECORD KEY DESCRIP-TER WITH DUPLICATES
                   ALTERNATE RECORD KEY ACT-TER     WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.
                  
005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-CITAS.CBL".
       COPY "..\..\FUENTES\FD-PACIE.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-SALUD.CBL".
       COPY "..\..\FUENTES\FD-TERCE.CBL".

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

       01 LLAVE-W.
          02 SUC-W                      PIC XX.
          02 TIPO-W                     PIC X.
          02 COMPROB-W                  PIC 9(6).
       01 FINALID-W                     PIC 99.
       01 ACT-CIT-W                     PIC X.

       77 MES-SAL-W                     PIC X(4).
       77 NOM-CITAS-W                   PIC X(70).
       77 NOM-PACIE-W                   PIC X(70).
       77 NOM-TER-W                     PIC X(70).
       77 NOM-SAL-W                     PIC X(70).
       77 NOM-USUA-W                    PIC X(70).
       77  SW-INI                       PIC 9.

       01 LLAVE-ALT-CIT-W.
          02 LLAVE-FECHA-CIT-W.
             03 MED-CIT-ALT-W           PIC 9(10).
             03 FECHA-CIT-ALT-W.
                05 ANO-CIT-ALT-W        PIC 9(4).
                05 MES-CIT-ALT-W        PIC 99.
                05 DIA-CIT-ALT-W        PIC 99.
          02 HORA-CIT-W.
             03 HR-CIT-W                PIC 99.
             03 MN-CIT-W                PIC 99.

       01 LLAVE-PACI-W.
          02 LLAVE-CIT-MED-W.
             03 FECHA-W.
                05 ANO-W                PIC 9(4).
                05 MES-W                PIC 99.
                05 DIA-W                PIC 99.
             03 MED-W                   PIC 9(10).
          02 PACI-W                     PIC X(15).
          02 TIPO-FACT-W                PIC X.

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV             PIC XX.

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
              MOVE "SER891"                 TO MSJ3-HTML
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
              MOVE "SER891"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CITAS.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-CITAS-W              TO MSJ2-HTML
              MOVE "SER891"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USUA-W               TO MSJ2-HTML
              MOVE "SER891"                 TO MSJ3-HTML
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
              MOVE "SER891"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PACIE-W              TO MSJ2-HTML
              MOVE "SER891"                 TO MSJ3-HTML
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
              MOVE "SER891"                 TO MSJ3-HTML
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
                    OPERADOR-LLEGADA-W, LLAVE-W, FINALID-W, ACT-CIT-W
           END-UNSTRING.

001855     MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.

000797 INICIAR-SESION.
000806     MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.
006840
006900 ABRIR-USUARIO.
006910     INITIALIZE OTR-STAT.
006920     MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
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
            
            INITIALIZE NOM-USUA-W

            MOVE "\\" TO NOM-USUA-W

            INSPECT NOM-USUA-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET 
         
001621      INSPECT NOM-USUA-W REPLACING FIRST " "
001624                                     BY "\"

007190      INSPECT NOM-USUA-W REPLACING FIRST "                              "
                                           BY DIR-CONTAB-LLEGADA-W

            MOVE NOM-USUA-W  TO NOM-TER-W NOM-PACIE-W  NOM-SAL-W NOM-TER-W

            INSPECT NOM-USUA-W REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHUSU.DAT".

            INSPECT NOM-SAL-W REPLACING FIRST "                     "
007300                                     BY "\CONTROL\SC-FACSA.DAT".

            INSPECT NOM-TER-W REPLACING FIRST "                       "
                                           BY "\CONTROL\SC-ARCHTER.DAT".

            INSPECT NOM-PACIE-W REPLACING FIRST "                     "
                                             BY "\CONTROL\SC-PACIE.DAT".
            

        ABRIR-ARCHIVOS.
           MOVE "00"     TO SW-INVALID-ENV
           OPEN INPUT  ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK
           END-READ.
           CLOSE ARCHIVO-USUARIOS.

           IF PREFIJO-USU = "  "
              MOVE "00"  TO PREFIJO-USU.

           OPEN I-O ARCHIVO-CITAS.
           OPEN INPUT FACTURA-SERVICIOS
                      ARCHIVO-TERCEROS.

        LEER-FACTURA.
           INITIALIZE REG-CIT.
           MOVE LLAVE-W TO LLAVE-FACT.
           READ FACTURA-SERVICIOS WITH NO LOCK 
                                  INVALID KEY
                                    GO TO CERRAR-ARCHIVOS
           END-READ.

           IF MES-ING-ESTAD IS NOT NUMERIC OR MES-ING-ESTAD = 00
              MOVE FECHA-FACT TO FECHA-ING-ESTAD
           END-IF.

           MOVE NIT-FACT TO COD-TER.
           READ ARCHIVO-TERCEROS INVALID KEY
                MOVE SPACES TO DESCRIP-TER.

        INICIAR-CITAS.
           INITIALIZE LLAVE-PACI-W LLAVE-ALT-CIT

           MOVE ID-HISTORIA-FACT   TO PACI-W
           MOVE CL-FACT            TO TIPO-FACT-W

           MOVE FECHA-ING-ESTAD    TO FECHA-CIT-ALT-W

           IF CL-FACT = 1
              MOVE MED-CIR-FACT    TO MED-CIT-ALT-W
           ELSE
              MOVE MED-OTR-FACT    TO MED-CIT-ALT-W
           END-IF.

        LEER-CITAS.
           MOVE LLAVE-ALT-CIT-W TO LLAVE-ALT-CIT.
           START ARCHIVO-CITAS KEY = LLAVE-ALT-CIT
                 INVALID KEY
                    MOVE "01"  TO SW-INVALID-ENV
                    MOVE 0  TO TIPO-FACT-W
                    EVALUATE TIPO-FACT-W
                    WHEN "0" MOVE "1" TO TIPO-FACT-W
                    WHEN "1" MOVE "2" TO TIPO-FACT-W
                    WHEN "2" MOVE "3" TO TIPO-FACT-W
                    WHEN "3" MOVE "4" TO TIPO-FACT-W
                    WHEN "4" MOVE "5" TO TIPO-FACT-W
                    WHEN "5" MOVE "6" TO TIPO-FACT-W
                    WHEN "6" MOVE "7" TO TIPO-FACT-W
                    WHEN "7" MOVE "A" TO TIPO-FACT-W
                    WHEN "A" MOVE "B" TO TIPO-FACT-W
                    WHEN "B" MOVE "C" TO TIPO-FACT-W
                    WHEN "C" MOVE "D" TO TIPO-FACT-W
                    WHEN "D" MOVE "E" TO TIPO-FACT-W
                    WHEN "E" MOVE "F" TO TIPO-FACT-W
                    WHEN "F" MOVE "G" TO TIPO-FACT-W
                    WHEN "G" MOVE "H" TO TIPO-FACT-W
                    WHEN "H" MOVE "I" TO TIPO-FACT-W
                    WHEN "I" MOVE "J" TO TIPO-FACT-W
                    WHEN "J" MOVE "K" TO TIPO-FACT-W
                    WHEN "K" MOVE "L" TO TIPO-FACT-W
                    WHEN "L" MOVE "M" TO TIPO-FACT-W
                    WHEN "M" MOVE "N" TO TIPO-FACT-W
                    WHEN "N" MOVE "O" TO TIPO-FACT-W
                    WHEN "O" MOVE "P" TO TIPO-FACT-W
                    WHEN "P" MOVE "Q" TO TIPO-FACT-W
                    WHEN "Q" MOVE "R" TO TIPO-FACT-W
                    WHEN "R" MOVE "S" TO TIPO-FACT-W
                    WHEN "S" MOVE "T" TO TIPO-FACT-W
                    WHEN "T" MOVE "U" TO TIPO-FACT-W
                    WHEN OTHER GO TO CERRAR-ARCHIVOS
                    END-EVALUATE
                    GO TO INICIAR-CITAS
           END-START.

       LEER-ARCHIVO-CITAS.
           READ ARCHIVO-CITAS NEXT WITH NO LOCK AT END GO TO CERRAR-ARCHIVOS.


           IF LLAVE-CIT-MED IS NOT = LLAVE-CIT-MED-W
           OR MEDICO-CIT       IS NOT = MED-W
              GO TO CERRAR-ARCHIVOS
           END-IF.


           IF TIPO-FACT-CIT = "A"
              CONTINUE
           ELSE
              IF LLAVE-PACI-CIT IS NOT = LLAVE-PACI-W
                  GO TO CERRAR-ARCHIVOS
              END-IF
           END-IF.

      * ESTO LO PIDIO YEIMI PARA ACTIVAR LAS CITAS.
           IF NIT-USU = 900306771
             IF TIPO-W = TIPO-FACT-CIT
                CONTINUE
             ELSE
                GO TO LEER-ARCHIVO-CITAS
             END-IF
           END-IF.

      * ESTO LO PIDIO YEIMI PARA ACTIVAR LAS CITAS.
           IF NIT-USU = 900306771
           AND TIPO-W = 7
              IF FINALID-CIT = FINALID-W
                 CONTINUE
              ELSE
                 GO TO LEER-ARCHIVO-CITAS
              END-IF
           END-IF.


           PERFORM ACTUALIZAR-CITA
           GO TO LEER-ARCHIVO-CITAS.

       CERRAR-ARCHIVOS.
           CLOSE ARCHIVO-CITAS
                 ARCHIVO-PACIENTES
                 ARCHIVO-TERCEROS
                 FACTURA-SERVICIOS.


        ENVIAR-DATOS.
            CLOSE ARCHIVO-CITAS.

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

       ACTUALIZAR-CITA.
      * ACT-CIT-W = "S" COMPROB ANULADO
           IF  ACT-CIT-W = "S"
           AND COMPROB-CIT = COMPROB-W
               INITIALIZE COMPROB-CIT
               MOVE NIT-FACT         TO CLAVE-CANC-CIT
               MOVE ACT-CIT-W        TO ACT-FACT-CIT
               REWRITE  REG-CIT END-REWRITE
           ELSE
              IF COMPROB-CIT = SPACES
              OR SUC-FACT-CIT IS NOT = SUC-W
                 IF EPS-CIT = SPACES
                    MOVE ENTIDAD-TER   TO EPS-CIT
                 END-IF
                 MOVE SUC-W            TO SUC-FACT-CIT
                 MOVE COMPROB-W        TO COMPROB-CIT
                 MOVE FECHA-ING-ESTAD  TO FECHA-FACTU-CIT
                 MOVE HORA-ATEN-ESTAD  TO HORA-FACTU-CIT
                 MOVE NIT-FACT         TO CLAVE-CANC-CIT
                 MOVE ACT-CIT-W        TO ACT-FACT-CIT
                 IF TIPO-W = 7
                    MOVE FINALID-W     TO FINALID-CIT
                 ELSE
                    MOVE 10            TO FINALID-CIT
                 END-IF
                 REWRITE  REG-CIT END-REWRITE
              END-IF
           END-IF.
005210* COPY "..\..\FUENTES\WS-HCEVO.CBL".
