      *============================================================
      * RX 505 CONSULTA DE FECHA, Y FACTURAS EN RX
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

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-RESRX.CBL".
       COPY "..\..\FUENTES\FD-TERCE.CBL".

       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".

       01 LLEGADA-W                         PIC X(200).

       01 DATOS-PLANO-W                     PIC X(1024).
       01 PASO-LLEGADA-W                    PIC X.
       01 FECHAINICIAL-LLEGAD-W             PIC X(8).
       01 FECHAFIN-LLEGAD-W                 PIC X(8).
       01 FECHA-INI                         PIC 9(8).
       01 FECHA-FIN                         PIC 9(8).

       01 LIN-1.
          02 FILLER                         PIC X(11) VALUE "{*DESCRIP*:".
          02 FILLER                         PIC X     VALUE "*".
          02 DESCRIP-ENV                    PIC X(50).
          02 FILLER                         PIC XX    VALUE "*}".
          02 CIERRE-LIN-1                   PIC X.

       01 LIN-2.
          02 FILLER                         PIC X(14) VALUE "{*LLAVE_FACT*:".
          02 FILLER                         PIC X     VALUE "*".
          02 LLAVE-FACT-RX-J                PIC X(38).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(15) VALUE "*FECHA_FACTRX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 FECHA-FACT-RX-J                PIC X(8).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(20) VALUE "*IDHISTORIA_FACTRX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 ID-HISTORIA-FACT-RX-J          PIC X(15).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(21) VALUE "*DESCRIPPACI_FACTRX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 DESCRIP-PACI-RX-J              PIC X(54).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(16) VALUE "*DESCRIPCUP_RX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 DESCRIP-CUP-RX-J               PIC X(60).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(13) VALUE "*ENTIDAD_RX*:".
          02 FILLER                         PIC X     VALUE "*".
          02 ENTIDAD-RX-J                   PIC X(10).
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
              MOVE "SER548"                 TO MSJ3-HTML
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
              MOVE "SER548"                 TO MSJ3-HTML
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
              MOVE "SER548"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TERCEROS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-TER-LNK              TO MSJ2-HTML
              MOVE "SER548"                 TO MSJ3-HTML
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
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W, PASO-LLEGADA-W,
                   FECHAINICIAL-LLEGAD-W, FECHAFIN-LLEGAD-W
           END-UNSTRING.

           MOVE SESION-LLEGAD-W       TO LLAVE-SESION-W.
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
           INITIALIZE NOM-TER-LNK

           MOVE "\\" TO NOM-TER-LNK

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\"

           INSPECT NOM-TER-LNK REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET

           MOVE NOM-TER-LNK    TO NOM-RES-RX

           INSPECT NOM-TER-LNK REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-TER-LNK REPLACING FIRST "                              "
                                            BY DIRECTORIO-LLEGAD-W

           INSPECT NOM-TER-LNK REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHTER.DAT"

           INSPECT NOM-RES-RX REPLACING FIRST "                        "
                                           BY "\PROG\DATOS\SC-RESRX.DAT".

       EVALUAR-PASO-W.
           EVALUATE PASO-LLEGADA-W
               WHEN "1"  GO TO CONSULTAR-FECHA
               WHEN "2"  GO TO LEER-FACTURA
           END-EVALUATE.

       CONSULTAR-FECHA.
           OPEN INPUT RESULTADOS-RX
           MOVE FECHAINICIAL-LLEGAD-W  TO FECHA-FACT-RX
           START RESULTADOS-RX KEY >= FECHA-FACT-RX
                 INVALID KEY
                 CLOSE RESULTADOS-RX
                 MOVE "SC-1"         TO MSJ1-HTML
                 MOVE "08"           TO MSJ2-HTML
                 MOVE "SER548"       TO MSJ3-HTML
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

       LEER-FACTURA.
           OPEN INPUT RESULTADOS-RX
           MOVE "{*FACTURA*:["       TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO

           MOVE FECHAFIN-LLEGAD-W     TO FECHA-FIN
           MOVE FECHAINICIAL-LLEGAD-W TO FECHA-INI.

       MOVER-FACTURA.
           READ RESULTADOS-RX NEXT WITH NO LOCK AT END GO TO CERRAR-MOVERF.

           IF FECHA-FACT-RX > FECHA-FIN
           OR FECHA-FACT-RX < FECHA-INI
              GO TO MOVER-FACTURA
           END-IF

           MOVE LLAVE-RX            TO LLAVE-FACT-RX-J
           MOVE FECHA-FACT-RX       TO FECHA-FACT-RX-J
           MOVE ID-HISTORIA-FACT-RX TO ID-HISTORIA-FACT-RX-J
           MOVE DESCRIP-PACI-RX     TO DESCRIP-PACI-RX-J
           MOVE DESCRIP-CUP-RX      TO DESCRIP-CUP-RX-J
           MOVE ID-ENTIDAD-RX       TO ENTIDAD-RX-J
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
           MOVE "RX505"       TO COBW3-CNV-VALUE
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