      *============================================================
      * RX 424 CONSULTA NUMEROS Y FECHA DE FACTURA DE RX
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
       COPY "..\..\FUENTES\FS-TERCE.CBL".
       COPY "..\..\FUENTES\FS-RESRX.CBL".

             SELECT ARCHIVO-PROFESIONALES LOCK MODE IS AUTOMATIC
                    ASSIGN NOM-PROF-W
                    ORGANIZATION IS INDEXED;
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS COD-PROF
                    ALTERNATE RECORD KEY DESCRIP-PROF  WITH DUPLICATES
                    ALTERNATE RECORD KEY ATIENDE-PROF  WITH DUPLICATES
                    ALTERNATE RECORD KEY SUCURSAL-PROF WITH DUPLICATES
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

             SELECT ARCHIVO-CUPS LOCK MODE IS AUTOMATIC
                    ASSIGN NOM-CUPS-W
                    ORGANIZATION IS INDEXED;
                    ACCESS MODE DYNAMIC;
                    RECORD KEY  IS LLAVE-CUP
                    ALTERNATE RECORD KEY IS DESCRIP-CUP WITH DUPLICATES
                    ALTERNATE RECORD KEY IS LLAVE-ALT-CUP WITH DUPLICATES
                    FILE STATUS IS OTR-STAT.

             SELECT ARCHIVO-ENFERMEDAD LOCK MODE IS AUTOMATIC
                    ASSIGN NOM-ENFER-W
                    ORGANIZATION IS INDEXED;
                    ACCESS MODE  IS DYNAMIC	
                    RECORD KEY   IS LLAVE-ENF
                    ALTERNATE RECORD KEY IS NOMBRE-ENF WITH DUPLICATES
                    FILE STATUS IS OTR-STAT.

             SELECT ARCHIVO-COSTO LOCK MODE IS AUTOMATIC
                    ASSIGN NOM-COSTO-W
                    ORGANIZATION IS INDEXED
                    ACCESS MODE IS DYNAMIC;
                    RECORD KEY IS COD-COSTO
                    FILE STATUS IS OTR-STAT.
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-RESRX.CBL".
       COPY "..\..\FUENTES\FD-TERCE.CBL".
       COPY "..\..\FUENTES\FD-PACIE.CBL".
       COPY "..\..\FUENTES\FD-CUPS19.CBL".
       COPY "..\..\FUENTES\FD-ENFER.CBL".
       COPY "..\..\FUENTES\FD-COSTO.CBL".
       COPY "..\..\FUENTES\FD-PROFE.CBL".

       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".

       01 LLEGADA-W                         PIC X(120).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W                 PIC X(15).
             03 FECHA-LLEGAD-W              PIC X(8).
             03 HORA-LLEGAD-W               PIC X(6).
          02 DIR-CONTAB-LLEGADA-W           PIC X(30).
          02 MES-CONTAB-LLEGADA-W           PIC XX.
          02 OPERADOR-LLEGADA-W             PIC X(4).
          02 PASO-LLEGADA-W                 PIC X.
          02 FECHA-FACT-LLEGADA-W           PIC X(8).
          02 COD-TER-LLEGADA-W              PIC X(10).
          02 COD-PROF-LLEGADA-W             PIC X(10).
          02 SUC-W                          PIC X(2).
          02 PREF-FACT-W                    PIC X.
          02 CTA-FACTURA-W                  PIC X(6).
          02 FECHA-FIN-LLEGADA-W            PIC X(8).
          02 NRO-INI-LLEGADA-W              PIC X(6).
          02 NRO-FIN-LLEGADA-W              PIC X(6).


       77 SW-NIT                            PIC X(10).
       77 SW-MED                            PIC X(10).
       77 NOM-PROF-W                        PIC X(70).
       77 NOM-PACIE-W                       PIC X(70).
       77 NOM-CUPS-W                        PIC X(70).
       77 NOM-ENFER-W                       PIC X(70).
       77 NOM-COSTO-W                       PIC X(70).
       77 DATOS-PLANO-W                     PIC X(800).
       01 FECHA-FIN.
          02 ANO-FIN                        PIC 9(4).
          02 MES-FIN                        PIC 99.
          02 DIA-FIN                        PIC 99.
       01 FECHA-INI.
          02 ANO-INI                        PIC 9(4).
          02 MES-INI                        PIC 99.
          02 DIA-INI                        PIC 99.
       01 NRO-INI-W                         PIC 9(6).
       01 NRO-FIN-W                         PIC 9(6).

       01 LLAVE-CUP-W.
          02 GRP-CUP-W                      PIC XX.
          02 COD-CUP-W                      PIC X(10).

       01 LIN-1.
          02 FILLER                         PIC X(11) VALUE "{*DESCRIP*:".
          02 FILLER                         PIC X     VALUE "*".
          02 DESCRIP-ENV                    PIC X(50).
          02 FILLER                         PIC XX    VALUE "*}".
          02 CIERRE-LIN-1                   PIC X.

       01 LIN-2.
          02 FILLER                         PIC X(14) VALUE "{*LLAVE_FACT*:".
          02 FILLER                         PIC X     VALUE "*".
          02 LLAVE-FACT-ENV                 PIC X(40).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(15) VALUE "*FECHA_FACTRX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 FECHA-FACT-RX-J                PIC X(8).
          02 FILLER                         PIC XX    VALUE "*}".
          02 CIERRE-LIN-2                   PIC X.


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
              MOVE "ARCHIVO-USUNET"         TO MSJ2-HTML
              MOVE "RX424"                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.          
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "ARCHIVO-SESION"         TO MSJ2-HTML
              MOVE "RX424"                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON RESULTADOS-RX.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-RES-RX               TO MSJ2-HTML
              MOVE "RX424"                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TERCEROS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PROF-W               TO MSJ2-HTML
              MOVE "RX424"                  TO MSJ3-HTML
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
              MOVE "RX424"                  TO MSJ3-HTML
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
              MOVE "RX424"                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CUPS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-CUPS-W               TO MSJ2-HTML
              MOVE "RX424"                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ENFERMEDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-ENFER-W              TO MSJ2-HTML
              MOVE "RX424"                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-COSTO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-COSTO-W              TO MSJ2-HTML
              MOVE "RX424"                  TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO LLEGADA-W


           UNSTRING LLEGADA-W DELIMITED BY "|"
              INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W,
                   OPERADOR-LLEGADA-W, PASO-LLEGADA-W, FECHA-FACT-LLEGADA-W, COD-TER-LLEGADA-W,
                   COD-PROF-LLEGADA-W, SUC-W, PREF-FACT-W, CTA-FACTURA-W, FECHA-FIN-LLEGADA-W,
                   NRO-INI-LLEGADA-W, NRO-FIN-LLEGADA-W
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGAD-W    TO LLAVE-SESION-W.
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
           INITIALIZE NOM-PACIE-W

           MOVE "\\" TO NOM-PACIE-W

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\"

           INSPECT NOM-PACIE-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET

           MOVE NOM-PACIE-W    TO NOM-RES-RX
                                  NOM-ENFER-W
                                  NOM-CUPS-W

           INSPECT NOM-PACIE-W REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-PACIE-W REPLACING FIRST "                              "
                                          BY DIR-CONTAB-LLEGADA-W

           MOVE NOM-PACIE-W    TO NOM-TER-LNK
                                  NOM-PROF-W
                                  NOM-PACIE-W
                                  NOM-COSTO-W

           INSPECT NOM-PACIE-W REPLACING FIRST "                     "
                                            BY "\CONTROL\SC-PACIE.DAT"

           INSPECT NOM-TER-LNK REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHTER.DAT"

           INSPECT NOM-PROF-W REPLACING FIRST "                        "
                                           BY "\CONTROL\SC-ARCHPROF.DAT"

           INSPECT NOM-COSTO-W REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHCOS.DAT"

           INSPECT NOM-RES-RX REPLACING FIRST "                        "
                                           BY "\PROG\DATOS\SC-RESRX.DAT"

           INSPECT NOM-ENFER-W REPLACING FIRST "                          "
                                            BY "\PROG\DATOS\SC-ARCHENF.DAT"

           INSPECT NOM-CUPS-W REPLACING FIRST "                           "
                                           BY "\PROG\DATOS\SC-ARCHCUPS.DAT".


       EVALUAR-PASO-W.
           EVALUATE PASO-LLEGADA-W
               WHEN "1"  GO TO CONSULTAR-FECHA
               WHEN "2"  GO TO CONSULTAR-TERCEROS
               WHEN "3"  GO TO CONSULTAR-PROFESIONALES
               WHEN "4"  GO TO LEER-FACTURA
           END-EVALUATE.

       CONSULTAR-FECHA.
           OPEN INPUT RESULTADOS-RX
           MOVE FECHA-FACT-LLEGADA-W  TO FECHA-FACT-RX
           START RESULTADOS-RX KEY >= FECHA-FACT-RX
                 INVALID KEY
                 CLOSE RESULTADOS-RX
                 MOVE "SC-1"         TO MSJ1-HTML
                 MOVE "08"           TO MSJ2-HTML
                 MOVE "RX424"        TO MSJ3-HTML
                 GO TO ENVIAR2-ERROR
           END-START


           MOVE "{*CONSULTA*:["       TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO

           MOVE "00"                  TO DESCRIP-ENV
           MOVE "," TO CIERRE-LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           INITIALIZE LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           MOVE "]}" TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           CLOSE RESULTADOS-RX
           GO TO PAGINA-CONFIG.

       CONSULTAR-TERCEROS.
           OPEN INPUT ARCHIVO-TERCEROS
           MOVE COD-TER-LLEGADA-W     TO COD-TER
           READ ARCHIVO-TERCEROS WITH NO LOCK
                INVALID KEY
                  CLOSE ARCHIVO-TERCEROS
                  MOVE "SC-1"         TO MSJ1-HTML
                  MOVE "01"           TO MSJ2-HTML
                  MOVE "RX424"        TO MSJ3-HTML
                  GO TO ENVIAR2-ERROR
           END-READ

           MOVE "{*CONSULTA*:["       TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO

           MOVE DESCRIP-TER           TO DESCRIP-ENV
           MOVE "," TO CIERRE-LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           INITIALIZE LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO.
           MOVE "]}" TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           CLOSE ARCHIVO-TERCEROS
           GO TO PAGINA-CONFIG.

       CONSULTAR-PROFESIONALES.
           OPEN INPUT ARCHIVO-PROFESIONALES
           MOVE COD-PROF-LLEGADA-W    TO COD-PROF
           READ ARCHIVO-PROFESIONALES WITH NO LOCK
                INVALID KEY
                  CLOSE ARCHIVO-PROFESIONALES
                  MOVE "SC-1"         TO MSJ1-HTML
                  MOVE "01"           TO MSJ2-HTML
                  MOVE "RX424"        TO MSJ3-HTML
                  GO TO ENVIAR2-ERROR
           END-READ

           MOVE "{*CONSULTA*:["       TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO

           MOVE DESCRIP-PROF          TO DESCRIP-ENV
           MOVE "," TO CIERRE-LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           INITIALIZE LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO.        
           MOVE "]}" TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           CLOSE ARCHIVO-PROFESIONALES
           GO TO PAGINA-CONFIG.

       LEER-FACTURA.
           OPEN INPUT RESULTADOS-RX
           MOVE "{*FACTURA*:["       TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO
           MOVE COD-TER-LLEGADA-W    TO SW-NIT
           MOVE COD-PROF-LLEGADA-W   TO SW-MED
           MOVE FECHA-FIN-LLEGADA-W  TO FECHA-FIN
           MOVE FECHA-FACT-LLEGADA-W TO FECHA-INI
           MOVE NRO-INI-LLEGADA-W    TO NRO-INI-W
           MOVE NRO-FIN-LLEGADA-W    TO NRO-FIN-W.

       MOVER-FACTURA.
           READ RESULTADOS-RX NEXT WITH NO LOCK AT END GO TO CERRAR-MOVERF.

           IF  RENGLON-PPAL-RX (1) = SPACES
           AND RENGLON-PPAL-RX (2) = SPACES
           AND RENGLON-PPAL-RX (3) = SPACES
               GO TO MOVER-FACTURA
           END-IF

           IF SUC-W = "**"
              CONTINUE
           ELSE
              IF SUC-W IS NOT = SUC-FACT-RX
                 GO TO MOVER-FACTURA
              END-IF
           END-IF

           IF SW-NIT = "0000000099"
              CONTINUE
           ELSE
              IF SW-NIT IS NOT = ID-ENTIDAD-RX
                 GO TO MOVER-FACTURA
              END-IF
           END-IF

           IF SW-MED = "0000000099"
              CONTINUE
           ELSE
              IF SW-MED IS NOT = SALA-RX
                 GO TO MOVER-FACTURA
              END-IF
           END-IF

           IF PREF-FACT-W = "*"
              CONTINUE
           ELSE
              IF CTA-FACTURA-W IS NOT = TIPO-NUM-RX
                 GO TO MOVER-FACTURA
              END-IF
           END-IF

           IF FECHA-FACT-RX > FECHA-FIN
           OR FECHA-FACT-RX < FECHA-INI
              GO TO MOVER-FACTURA
           END-IF

           IF NRO-FACT-RX < NRO-INI-W
              GO TO MOVER-FACTURA
           END-IF

           IF NRO-FACT-RX > NRO-FIN-W
              GO TO MOVER-FACTURA
           END-IF

           MOVE LLAVE-RX TO LLAVE-FACT-ENV
           MOVE FECHA-FACT-RX TO FECHA-FACT-RX-J
           MOVE "," TO CIERRE-LIN-2
           INSPECT LIN-2 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-2 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           GO TO MOVER-FACTURA.

       CERRAR-MOVERF.
           INITIALIZE LIN-2
           INSPECT LIN-2 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-2 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           MOVE "]}" TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           CLOSE RESULTADOS-RX
           GO TO PAGINA-CONFIG.

       PAGINA-CONFIG.
           MOVE "statuscode" TO COBW3-CNV-NAME
           MOVE "00"         TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3

           MOVE "programa-id" TO COBW3-CNV-NAME
           MOVE "RX424"       TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3

           MOVE "..\PAGINAS\RECIBIDOSLISTADO.ASPX" TO SALIDA-HTML
           PERFORM ABRIR-HTML.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.

       DATOS-ENVIO.
           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE DATOS-PLANO-W    TO COBW3-CNV-VALUE
           CALL "COBW3_SET_REPEAT" USING COBW3.

       COPY "..\..\FUENTES\SC-WEB19.CBL".
