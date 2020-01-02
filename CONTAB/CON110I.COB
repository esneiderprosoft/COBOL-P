      *=================================================================
      *GRABAR ARCHIVO-NOMBRE-TAR CON110I-01  SALUD
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
      * "\PROG\DATOS\SC-ARCHLOTE.DAT";

           SELECT ARCHIVO-LOTES LOCK MODE IS AUTOMATIC
	               ASSIGN LOTES-W;
                    ORGANIZATION IS INDEXED;
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS COD-LOTE
                    ALTERNATE RECORD KEY IS NOMBRE-LOTE WITH DUPLICATES
                    FILE STATUS IS OTR-STAT.

      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-LOTES.CBL".

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".

       77 DATOS-PLANO-W           PIC X(500).
       77 LOTES-W             PIC X(100).

       01 LIN-1.
          02 FILLER                  PIC X(8)  VALUE "{*LOTE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LOTE-J                  PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)  VALUE "*NOMBRE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOMBRE-J                PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*CONSECUTIVO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CONSECUTIVO-J           PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*PREFIJO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PREFIJO-J               PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*PRESUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PRESUP-J                PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*SIN_SIT_FOND*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SINSITFOND-J            PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*CONTRATO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CONTRATO-J              PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*SIMPLIF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SIMPLIF-J               PIC X.
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-LIN-1            PIC X.

    *>    01 DATOS-ENVIO.
    *>       02 DAT1-ENV                  PIC XX.
    *>       02 FILLER                    PIC X VALUE "|".
    *>       02 NOMBRE-ENV                PIC X(30).
    *>       02 FILLER                    PIC X VALUE "|".
    *>       02 CONSECUTIVO-ENV           PIC X.
    *>       02 FILLER                    PIC X VALUE "|".
    *>       02 PREFIJO-ENV               PIC XX.
    *>       02 FILLER                    PIC X VALUE "|".
    *>       02 PRESUP-ENV                PIC X.
    *>       02 FILLER                    PIC X VALUE "|".
    *>       02 SINSIT-ENV                PIC X.
    *>       02 FILLER                    PIC X VALUE "|".
    *>       02 CONTRATO-ENV              PIC X.
    *>       02 FILLER                    PIC X VALUE "|".
    *>       02 SIMPLIF-ENV               PIC X.

      
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
              MOVE "CON110I"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-LOTES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT           TO MSJ1-HTML
              MOVE LOTES-W            TO MSJ2-HTML
              MOVE "CON110I"          TO MSJ3-HTML
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
              MOVE "CON110I"                TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO LINEA-LLEGAD-W.

           UNSTRING LINEA-LLEGAD-W DELIMITED BY "|"
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W
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
           EVALUATE OTR-STAT
             WHEN "00"  CONTINUE
             WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
                        CLOSE       ARCHIVO-USUNET
                        OPEN I-O    ARCHIVO-USUNET
                        INITIALIZE REG-USUNET
                        MOVE "." TO NOMBRE-USUNET
                        WRITE REG-USUNET
                        CLOSE       ARCHIVO-USUNET
                        MOVE "SC-2"                        TO MSJ1-HTML
                        MOVE "Falta configurar usuario"    TO MSJ2-HTML
                        MOVE "CON110I"                          TO MSJ3-HTML
             WHEN OTHER GO TO CERRAR-SESION
           END-EVALUATE.
      
           INITIALIZE LLAVE-USUNET.

       LEER-USUARIO.
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
           CLOSE ARCHIVO-USUNET.
      
           IF NOMBRE-USUNET = SPACES
              MOVE "SC-2"                        TO MSJ1-HTML
              MOVE "Falta configurar usuario"    TO MSJ2-HTML
              MOVE "CON110I"                          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.
           INITIALIZE LOTES-W

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           
           MOVE "\\" TO LOTES-W

           INSPECT LOTES-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET
                                          
           INSPECT LOTES-W   REPLACING FIRST "                           "
                                               BY "\PROG\DATOS\SC-ARCHLOTE.DAT".


      *      MOVE 99                 TO MSJ1-HTML
      *      MOVE LOTE-LLEGADA-W             TO MSJ2-HTML
      *      MOVE LOTE-LLEGADA-W             TO MSJ3-HTML
      *      GO TO ENVIAR2-ERROR.

       ABRIR-LOTE.
           OPEN INPUT ARCHIVO-LOTES.  

           MOVE "{*LOTES*:[" TO  DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO.
           MOVE 0 TO SW-FIN.

       LEER-LOTE.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-PLANO-W
              PERFORM DATOS-ENVIO
           END-IF
           READ ARCHIVO-LOTES NEXT WITH NO LOCK AT END GO TO CERRAR-PLANO.

           MOVE COD-LOTE          TO LOTE-J
           MOVE NOMBRE-LOTE       TO NOMBRE-J
           MOVE CONSEC-LOTE       TO CONSECUTIVO-J
           MOVE PREFIJO-LOTE      TO PREFIJO-J

           IF PRESUP-LOTE = SPACES
              MOVE "N" TO PRESUP-J
           ELSE 
              MOVE PRESUP-LOTE       TO PRESUP-J
           END-IF

           IF SINSIT-FOND-LOTE = SPACES
              MOVE "N" TO SINSITFOND-J
           ELSE 
              MOVE SINSIT-FOND-LOTE       TO SINSITFOND-J
           END-IF
               
           IF CONTRATO-LOTE = SPACES
              MOVE "N" TO CONTRATO-J
           ELSE 
              MOVE CONTRATO-LOTE       TO CONTRATO-J
           END-IF

           IF SIMPLIF-LOTE = SPACES
              MOVE "N" TO SIMPLIF-J
           ELSE 
              MOVE SIMPLIF-LOTE       TO SIMPLIF-J
           END-IF

           MOVE ","               TO CIERRE-LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           GO TO LEER-LOTE.

       CERRAR-PLANO.
           INITIALIZE LIN-1
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           
           MOVE "]}" TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           CLOSE ARCHIVO-LOTES.

       PAGINA-CONFIG.  
           MOVE "statuscode" TO COBW3-CNV-NAME
           MOVE "00"         TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3

           MOVE "programa-id" TO COBW3-CNV-NAME
           MOVE "CON110I"      TO COBW3-CNV-VALUE
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