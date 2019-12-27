      *============================================================
      * SER109D - CONSULTA DE ARCHIVOS Y GUARDADO
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
       COPY "..\..\FUENTES\FS-NUMER19.CBL".
       COPY "..\..\FUENTES\FS-TARIF.CBL".

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-NUMER19.CBL".
       COPY "..\..\FUENTES\FD-TARIF.CBL".

       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".

       01 LLEGADA-W                         PIC X(500).

       01 DATOS-PLANO-W                     PIC X(1024).
       01 PASO-LLEGADA-W                    PIC X.
       01 LLAVE-LLEGAD-W                    PIC X(7).
       01 OBSERV-LLEGAD-W                   PIC X(240).
       01 ANEXOS-LLEGAD-W                   PIC X(180).
       01 ESTADO-LLEGAD-W                   PIC X.

       01 LIN-1.
          02 FILLER                         PIC X(15) VALUE "{*DESCRIP_NUM*:".
          02 FILLER                         PIC X     VALUE "*".
          02 DESCRIP-NUM-J                  PIC X(40).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(12) VALUE "*IDPAC_NUM*:".
          02 FILLER                         PIC X     VALUE "*".
          02 ID-PAC-NUM-J                   PIC X(15).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(16) VALUE "*NOMBREPAC_NUM*:".
          02 FILLER                         PIC X     VALUE "*".
          02 NOMBRE-PAC-NUM-J               PIC X(40).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(15) VALUE "*CONVENIO_NUM*:".
          02 FILLER                         PIC X     VALUE "*".
          02 CONVENIO-NUM-J                 PIC X(25).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(14) VALUE "*ESTADO_NUM*:".
          02 FILLER                         PIC X     VALUE "*".
          02 ESTADO-NUM-J                   PIC X(1).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(16) VALUE "*FECHARET_NUM*:".
          02 FILLER                         PIC X     VALUE "*".
          02 FECHA-RET-NUM-J                PIC X(8).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(17) VALUE "*FACTCAPIT_NUM*:".
          02 FILLER                         PIC X     VALUE "*".
          02 FACT-CAPIT-NUM-J               PIC X(7).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(14) VALUE "*OBSERV_NUM*:".
          02 FILLER                         PIC X     VALUE "*".
          02 OBSERV-NUM-J                   PIC X(240).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(14) VALUE "*ANEXOS_NUM*:".
          02 FILLER                         PIC X     VALUE "*".
          02 ANEXOS-NUM-J                   PIC X(180).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(15) VALUE "*FECHAING_NUM*:".
          02 FILLER                         PIC X     VALUE "*".
          02 FECHA-ING-NUM-J                PIC X(8).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(15) VALUE "*FECHAPRE_NUM*:".
          02 FILLER                         PIC X     VALUE "*".
          02 FECHA-PRE-NUM-J                PIC X(8).
          02 FILLER                         PIC XX    VALUE "*}".
          02 CIERRE-LIN-1                   PIC X.

       01 LIN-2.
          02 FILLER                         PIC X(11) VALUE "{*DESCRIP*:".
          02 FILLER                         PIC X     VALUE "*".
          02 DESCRIP-ENV                    PIC X(50).
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
              MOVE "SER109D"                TO MSJ3-HTML
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
              MOVE "SER109D"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-NUMERACION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-NUMER-LNK            TO MSJ2-HTML
              MOVE "SER109D"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TARIFAS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-TAR-LNK              TO MSJ2-HTML
              MOVE "SER109D"                TO MSJ3-HTML
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
                   LLAVE-LLEGAD-W, OBSERV-LLEGAD-W, ANEXOS-LLEGAD-W, ESTADO-LLEGAD-W
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
           INITIALIZE NOM-NUMER-LNK

           MOVE "\\" TO NOM-NUMER-LNK

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\"

           INSPECT NOM-NUMER-LNK REPLACING FIRST "                    "
                                              BY IP-DATOS-USUNET

           INSPECT NOM-NUMER-LNK REPLACING FIRST " "
                                              BY "\"

           INSPECT NOM-NUMER-LNK REPLACING FIRST "                              "
                                              BY DIRECTORIO-LLEGAD-W

           MOVE NOM-NUMER-LNK  TO NOM-TAR-LNK

           INSPECT NOM-NUMER-LNK REPLACING FIRST "                     "
                                              BY "\CONTROL\SC-NUMER.DAT"

           INSPECT NOM-TAR-LNK REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHTAR.DAT".

       EVALUAR-PASO-W.
           EVALUATE PASO-LLEGADA-W
               WHEN "1"  GO TO CONSULTAR-NUMERACION
               WHEN "2"  GO TO GRABAR-NUMERACION
           END-EVALUATE.

       CONSULTAR-NUMERACION.
           OPEN INPUT ARCHIVO-NUMERACION
           OPEN INPUT ARCHIVO-TARIFAS
           MOVE LLAVE-LLEGAD-W  TO LLAVE-NUM
           READ ARCHIVO-NUMERACION WITH NO LOCK
                 INVALID KEY
                 CLOSE ARCHIVO-NUMERACION
                 MOVE "SC-1"         TO MSJ1-HTML
                 MOVE "01"           TO MSJ2-HTML
                 MOVE "SER109D"      TO MSJ3-HTML
                 GO TO ENVIAR2-ERROR
           END-READ

           MOVE "{*NUMERACION*:["    TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO

           MOVE DESCRIP-NUM          TO DESCRIP-NUM-J
           MOVE ID-PAC-NUM           TO ID-PAC-NUM-J
           MOVE NOMBRE-PAC-NUM       TO NOMBRE-PAC-NUM-J
           MOVE CONVENIO-NUM         TO COD-TAR
           READ ARCHIVO-TARIFAS WITH NO LOCK
                INVALID KEY
                  MOVE COD-TAR TO CONVENIO-NUM
           END-READ
           MOVE DESCRIP-TAR          TO CONVENIO-NUM-J
           MOVE ESTADO-NUM           TO ESTADO-NUM-J
           MOVE FECHA-RET-NUM        TO FECHA-RET-NUM-J
           MOVE FACT-CAPIT-NUM       TO FACT-CAPIT-NUM-J
           MOVE OBSERV-NUM           TO OBSERV-NUM-J
           MOVE ANEXOS-NUM           TO ANEXOS-NUM-J
           MOVE FECHA-ING-NUM        TO FECHA-ING-NUM-J
           MOVE FECHA-PRE-NUM        TO FECHA-PRE-NUM-J
           MOVE "," TO CIERRE-LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           INITIALIZE LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           MOVE "]}" TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           CLOSE ARCHIVO-NUMERACION
                 ARCHIVO-TARIFAS
           GO TO PAGINA-CONFIG.

       GRABAR-NUMERACION.
           OPEN I-O ARCHIVO-NUMERACION
           MOVE LLAVE-LLEGAD-W TO LLAVE-NUM
           READ ARCHIVO-NUMERACION WITH NO LOCK
                INVALID KEY
                 CLOSE ARCHIVO-NUMERACION
                 MOVE "SC-1"         TO MSJ1-HTML
                 MOVE "01"           TO MSJ2-HTML
                 MOVE "SER109D"      TO MSJ3-HTML
                 GO TO ENVIAR2-ERROR
           END-READ

           MOVE OBSERV-LLEGAD-W TO OBSERV-NUM
           INSPECT OBSERV-NUM REPLACING ALL LOW-VALUES BY SPACES
           MOVE ANEXOS-LLEGAD-W TO ANEXOS-NUM
           INSPECT ANEXOS-NUM REPLACING ALL LOW-VALUES BY SPACES
           MOVE ESTADO-LLEGAD-W TO ESTADO-NUM
           REWRITE REG-NUM END-REWRITE
           CLOSE ARCHIVO-NUMERACION.

           MOVE "{*NUMERACION*:["    TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO

           MOVE "00"            TO DESCRIP-ENV
           MOVE "," TO CIERRE-LIN-2
           INSPECT LIN-2 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-2 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           INITIALIZE LIN-2
           INSPECT LIN-2 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-2 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           MOVE "]}" TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
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