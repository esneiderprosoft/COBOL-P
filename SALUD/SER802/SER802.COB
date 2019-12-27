
      *=================================================================
      * CREACION DE JSON PARA CONSUTA DE TABLAS TARIFAS - SER802
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
       COPY "..\..\FUENTES\FS-TABLA.CBL".
 
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-TABLA19.CBL".
      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".

       01 LLEGADA-W                  PIC X(100).
       01 DATOS-PLANO-W              PIC X(1000).
       01 LLAVE-TIPO-TAB-LLEGADA-W   PIC X(3).

       01 LIN-1.  
          02 FILLER                  PIC X(7)   VALUE "{*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-TAB-J               PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)  VALUE "*TIPO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-TAB-J              PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*COD_SER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-SER-TAB-J           PIC X(12).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-TAB-J           PIC X(30).
          02 FILLER                  PIC XX     VALUE "*}".    
          02 CIERRE-LIN-1            PIC X.
        

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
              MOVE "SER802"                 TO MSJ3-HTML
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
              MOVE "SER802"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TABLAS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE                    
           ELSE
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-TAB-LNK               TO MSJ2-HTML
              MOVE "SER802"                  TO MSJ3-HTML
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
           INITIALIZE LLAVE-TIPO-TAB-LLEGADA-W

           UNSTRING LLEGADA-W DELIMITED BY "|"
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W, 
                   LLAVE-TIPO-TAB-LLEGADA-W
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
           INITIALIZE NOM-TAB-LNK 

           MOVE "\\" TO NOM-TAB-LNK

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

           INSPECT NOM-TAB-LNK REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-TAB-LNK REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-TAB-LNK REPLACING FIRST "                              "
                                            BY DIRECTORIO-LLEGAD-W.

           INSPECT NOM-TAB-LNK REPLACING FIRST "                     "
                                            BY "\CONTROL\SC-TABLA.DAT".

       ABRIR-ARCHIVO.
           OPEN INPUT ARCHIVO-TABLAS
           MOVE "{*TABLA*:[" TO  DATOS-PLANO-W
           INSPECT DATOS-PLANO-W  REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO.

       CONSULTAR-ARCHIVO.
           READ ARCHIVO-TABLAS NEXT WITH NO LOCK AT END 
                GO TO CERRAR-TAB
           END-READ.

           IF LLAVE-TIPO-TAB-LLEGADA-W = SPACES
              CONTINUE
           ELSE
              IF LLAVE-TIPO-TAB NOT = LLAVE-TIPO-TAB-LLEGADA-W
                 GO TO CONSULTAR-ARCHIVO
              END-IF
           END-IF

           MOVE COD-TAB             TO COD-TAB-J
           MOVE COD-SER-TAB         TO COD-SER-TAB-J

       
           INSPECT DESCRIP-TAB REPLACING ALL '"' BY " "
           INSPECT DESCRIP-TAB REPLACING ALL "*" BY " "
           INSPECT DESCRIP-TAB REPLACING ALL "Ñ" BY CARAC-ENE-MAY
           INSPECT DESCRIP-TAB REPLACING ALL "ñ" BY CARAC-ENE-MIN
           INSPECT DESCRIP-TAB REPLACING ALL LOW-VALUES BY SPACES

           MOVE TIPO-TAB    TO TIPO-TAB-J
           MOVE DESCRIP-TAB TO DESCRIP-TAB-J

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES
           MOVE "," TO CIERRE-LIN-1
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           GO TO CONSULTAR-ARCHIVO.

       CERRAR-TAB.
           INITIALIZE LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
          
           MOVE "]}" TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           CLOSE ARCHIVO-TABLAS.
      
       PAGINA-CONFIG.
           MOVE "statuscode" TO COBW3-CNV-NAME
           MOVE "00"         TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3

           MOVE "programa-id" TO COBW3-CNV-NAME
           MOVE "SER802"      TO COBW3-CNV-VALUE
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