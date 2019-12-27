      *============================================================
      * RXI02 CONSULTA FACTURAS PARA IMPRIMIR
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

       01 LLEGADA-W                         PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W                 PIC X(15).
             03 FECHA-LLEGAD-W              PIC X(8).
             03 HORA-LLEGAD-W               PIC X(6).
          02 DIR-CONTAB-LLEGADA-W           PIC X(30).
          02 MES-CONTAB-LLEGADA-W           PIC XX.
          02 OPERADOR-LLEGADA-W             PIC X(4).
          02 LLAVE-FACT-LLEGADA-W           PIC X(40).


       77 NOM-PROF-W                        PIC X(70).
       77 NOM-PACIE-W                       PIC X(70).
       77 NOM-CUPS-W                        PIC X(70).
       77 NOM-ENFER-W                       PIC X(70).
       77 NOM-COSTO-W                       PIC X(70).
       77 DATOS-PLANO-W                     PIC X(1024).
       77 NOM-FIRMA-W                       PIC X(60).

       01 TABLA-RESULTADO-PPAL-W.
          02 RESULTADOS-PPAL-W  OCCURS 11.
             03 RENGLONES-W                 PIC X(1000).

       01 TABLA-COMP-PPAL-W.
          02 RESULTADOS-COMP-W OCCURS 8.
             03 RENGLONES-COMP-W            PIC X(1000).

       01 TABLA-ADIC-PPAL-W.
          02 RESULTADOS-ADIC-W OCCURS 8.
             03 RENGLONES-ADIC-W            PIC X(1000).

       01 COD-PROF-W                        PIC X(10).

       01 LLAVE-CUP-W.
          02 GRP-CUP-W                      PIC XX.
          02 COD-CUP-W                      PIC X(10).

       01 LIN-1.
          02 FILLER                         PIC X(16) VALUE "{*LLAVE_FACTRX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 LLAVE-FACT-RX-J                PIC X(19).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(15) VALUE "*FECHA_FACTRX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 FECHA-FACT-RX-J                PIC X(8).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(14) VALUE "*HORA_FACTRX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 HORA-FACT-RX-J                 PIC X(4).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(15) VALUE "*IDHIS_FACTRX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 ID-HISTORIA-FACT-RX-J          PIC X(15).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(17) VALUE "*DESCRIP_PACIRX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 DESCRIP-PACI-RX-J              PIC X(54).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(10) VALUE "*EDAD_RX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 EDAD-RX-J                      PIC X(4).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(10) VALUE "*SEXO_RX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 SEXO-RX-J                      PIC X.
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(14) VALUE "*DESCRIP_TER*:".
          02 FILLER                         PIC X     VALUE "*".
          02 DESCRIP-TER-J                  PIC X(30).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(14) VALUE "*IDENTID_RX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 IDENTIDAD-RX-J                 PIC X(10).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(17) VALUE "*DESCRIP1_CUPRX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 DESCRIP-CUP-RX-J               PIC X(60).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(17) VALUE "*IDRADIOLOGO_RX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 IDRADIOLOGO-RX-J               PIC X(10).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(12) VALUE "*REGMED_RX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 REG-MED-RX-J                   PIC X(10).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(15) VALUE "*NOMMEDICO_RX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 NOM-MEDICO-RX-J                PIC X(30).
          02 FILLER                         PIC XX    VALUE "*}".
          02 CIERRE-LIN-1                   PIC X.

       01 LIN-2.
          02 FILLER                         PIC X(20) VALUE "{*RESULTADOPPAL_RX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 TAB-RESULTADO-PPAL-RX-J        PIC X(1000).
          02 FILLER                         PIC XX    VALUE "*}".
          02 CIERRE-LIN-2                   PIC X.


       01 LIN-3.
          02 FILLER                         PIC X(20) VALUE "{*RESULTADOCOMP_RX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 TAB-RESULTADO-COMP-RX-J        PIC X(1000).
          02 FILLER                         PIC XX    VALUE "*}".
          02 CIERRE-LIN-3                   PIC X.

       01 LIN-4.
          02 FILLER                         PIC X(20) VALUE "{*RESULTADOADIC_RX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 TAB-RESULTADO-ADIC-RX-J        PIC X(1000).
          02 FILLER                         PIC XX    VALUE "*}".
          02 CIERRE-LIN-4                   PIC X.

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
                   OPERADOR-LLEGADA-W, LLAVE-FACT-LLEGADA-W
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

           INSPECT NOM-PACIE-W REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-PACIE-W REPLACING FIRST "                              "
                                            BY DIR-CONTAB-LLEGADA-W

           MOVE NOM-PACIE-W    TO NOM-TER-LNK
                                  NOM-PACIE-W
                                  NOM-COSTO-W

           INSPECT NOM-TER-LNK REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHTER.DAT"

           INSPECT NOM-PACIE-W REPLACING FIRST "                     "
                                            BY "\CONTROL\SC-PACIE.DAT"

           INSPECT NOM-RES-RX REPLACING FIRST "                        "
                                           BY "\PROG\DATOS\SC-RESRX.DAT".


       CONSULTAR-FACTURA.
           OPEN INPUT RESULTADOS-RX
                      ARCHIVO-PACIENTES
                      ARCHIVO-TERCEROS

           MOVE "{*IMPRESION*:["       TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO
           MOVE LLAVE-FACT-LLEGADA-W  TO LLAVE-RX.

       ABRIR-FACTURA.
           READ RESULTADOS-RX WITH NO LOCK
                INVALID KEY
                  CLOSE RESULTADOS-RX
                  MOVE "SC-1"          TO MSJ1-HTML
                  MOVE "01"            TO MSJ2-HTML
                  MOVE "RX424"         TO MSJ3-HTML
                  GO TO ENVIAR2-ERROR.

           MOVE LLAVE-RX              TO LLAVE-FACT-RX-J
           MOVE FECHA-FACT-RX         TO FECHA-FACT-RX-J
           MOVE HORA-FACT-RX          TO HORA-FACT-RX-J
           MOVE DESCRIP-PACI-RX       TO DESCRIP-PACI-RX-J
           MOVE ID-HISTORIA-FACT-RX   TO COD-PACI

           READ ARCHIVO-PACIENTES WITH NO LOCK
                INVALID KEY
                  MOVE COD-PACI       TO DESCRIP-PACI
           END-READ

           MOVE COD-PACI              TO ID-HISTORIA-FACT-RX-J
           MOVE EDAD-RX               TO EDAD-RX-J
           MOVE SEXO-RX               TO SEXO-RX-J

           IF ID-ENTIDAD-RX < 1
              MOVE 900219120 TO ID-ENTIDAD-RX
           END-IF
             
           MOVE ID-ENTIDAD-RX TO IDENTIDAD-RX-J
           MOVE ID-ENTIDAD-RX TO COD-TER

           READ ARCHIVO-TERCEROS WITH NO LOCK
                INVALID KEY
                  MOVE ID-ENTIDAD-RX  TO DESCRIP-TER
           END-READ

           MOVE DESCRIP-TER           TO DESCRIP-TER-J
           MOVE DESCRIP-CUP-RX        TO DESCRIP-CUP-RX-J
           MOVE ID-RADIOLOGO-RX       TO IDRADIOLOGO-RX-J
           MOVE NOM-MEDICO-RX         TO NOM-MEDICO-RX-J
           MOVE REG-MEDICO-RX         TO REG-MED-RX-J

           MOVE "," TO CIERRE-LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO.

           MOVE TAB-RESULTADO-PPAL-RX TO TABLA-RESULTADO-PPAL-W
           MOVE TAB-COMP-PPAL-RX TO TABLA-COMP-PPAL-W
           MOVE TAB-ADIC-PPAL-RX TO TABLA-ADIC-PPAL-W

           PERFORM MONTAR-RESULTADO VARYING I FROM 1 BY 1
                                              UNTIL I > 11

           PERFORM MONTAR-COMPLEMENTARIO VARYING I FROM 1 BY 1
                                                   UNTIL I > 8

           PERFORM MONTAR-ADICIONAL VARYING I FROM 1 BY 1
                                              UNTIL I > 8.

       CERRAR-CFECHA.
           INITIALIZE LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           MOVE "]}" TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           CLOSE RESULTADOS-RX
                 ARCHIVO-PACIENTES
                 ARCHIVO-TERCEROS.

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

       MONTAR-RESULTADO.
           MOVE RENGLONES-W (I)  TO TAB-RESULTADO-PPAL-RX-J
           MOVE "," TO CIERRE-LIN-2
           INSPECT LIN-2 REPLACING ALL "*"        BY CARAC-COMILLA
           INSPECT LIN-2 REPLACING ALL "\.br\"    BY "-    "
           INSPECT LIN-2 REPLACING ALL LOW-VALUES BY SPACES
           MOVE LIN-2 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO.

       MONTAR-COMPLEMENTARIO.
           MOVE RENGLONES-COMP-W (I)  TO TAB-RESULTADO-COMP-RX-J
           MOVE "," TO CIERRE-LIN-3
           INSPECT LIN-3 REPLACING ALL "*"        BY CARAC-COMILLA
           INSPECT LIN-3 REPLACING ALL "\.br\"    BY "-    "
           INSPECT LIN-3 REPLACING ALL LOW-VALUES BY SPACES
           MOVE LIN-3 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO.

       MONTAR-ADICIONAL.
           MOVE RENGLONES-ADIC-W (I)  TO TAB-RESULTADO-ADIC-RX-J
           MOVE "," TO CIERRE-LIN-4
           INSPECT LIN-4 REPLACING ALL "*"        BY CARAC-COMILLA
           INSPECT LIN-4 REPLACING ALL "\.br\"    BY "-    "
           INSPECT LIN-4 REPLACING ALL LOW-VALUES BY SPACES
           MOVE LIN-4 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO.

       COPY "..\..\FUENTES\SC-WEB19.CBL".
