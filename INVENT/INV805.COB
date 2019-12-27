      *============================================================
      * VENTANA DE CONSULTA DE ARTICULOS
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FS-ARTIC.CBL".

       DATA DIVISION.
       FILE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-ARTIC.CBL".

       WORKING-STORAGE  SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC19.CBL".

       01 LLEGADA-W                         PIC X(100).

       01 TIPO-ART-LLEGADA-W                PIC X.
       01 DATOS-PLANO-W                     PIC X(1000).

       01 LIN-1.
          02 FILLER                         PIC X(13) VALUE "{*LLAVE_ART*:".
          02 FILLER                         PIC X     VALUE "*".
          02 COD-ART-J                      PIC X(18).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(15) VALUE "*DESCRIP_ART*:".
          02 FILLER                         PIC X     VALUE "*".
          02 DESCRIP-ART-J                  PIC X(50).
          02 FILLER                         PIC XX    VALUE "*}".
          02 CIERRE-LIN-1                   PIC X.


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
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "ARCHIVO-USUNET"         TO MSJ2-HTML
              MOVE "INV803"                 TO MSJ3-HTML
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
              MOVE "INV803"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MAESTRO-ARTICULOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-ART-LNK              TO MSJ2-HTML
              MOVE "INV803"                 TO MSJ3-HTML
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
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W, TIPO-ART-LLEGADA-W
           END-UNSTRING.

           MOVE SESION-LLEGAD-W    TO LLAVE-SESION-W.
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
           INITIALIZE NOM-ART-LNK

           MOVE "\\" TO NOM-ART-LNK

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\"

           INSPECT NOM-ART-LNK REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-ART-LNK REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-ART-LNK REPLACING FIRST "                              "
                                          BY DIRECTORIO-LLEGAD-W

           INSPECT NOM-ART-LNK REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-MAESART.DAT".

       ABRIR-ARCHIVO.
           OPEN INPUT MAESTRO-ARTICULOS.
           MOVE "{*ARTICULOS*:["       TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO.

       CONSULTAR-ARCHIVO.
           READ MAESTRO-ARTICULOS NEXT WITH NO LOCK AT END GO TO CERRARM.

           IF TIPO-ART = TIPO-ART-LLEGADA-W
              GO TO CONSULTAR-ARCHIVO
           END-IF

           MOVE COD-ART                TO COD-ART-J
           MOVE DESCRIP-ART            TO DESCRIP-ART-J
           INSPECT DESCRIP-ART-J REPLACING ALL CARAC-COMILLA BY SPACES
           INSPECT DESCRIP-ART-J REPLACING ALL "*" BY SPACES
           MOVE "," TO CIERRE-LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           GO TO CONSULTAR-ARCHIVO.

       CERRARM.
           INITIALIZE LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           MOVE "]}" TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           CLOSE MAESTRO-ARTICULOS.

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

       COPY "P:\PROG\PROYECT\FUENTES\SC-WEB19.CBL".
