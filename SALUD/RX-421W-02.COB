      *=================================================================
      *GRABAR ARCHIVO-RESULTADOS-RX  SALUD
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".                                                         
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
       COPY "..\..\FUENTES\FS-RESRX.CBL".  
                                                                                   
           SELECT ARCHIVO-PLANO
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.

      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-RESRX.CBL".

       FD  ARCHIVO-PLANO
           LABEL RECORD STANDARD.
       01  REG-PLANO.
           02 DATOS-PLANO      PIC X(1500).
      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".

       77 NOM-PLANO-W                  PIC X(60).
       
       01 REG-W.
           02 LLAVE-LLEGADA-W.
              03 LLAVE1-LLEGADA-W.
                 05 NIT-LLEGADA-W          PIC X(10).
                 05 LLAVE-FACT-LLEGADA-W.   
                    07 SUC-LLEGADA-W       PIC XX.
                    07 CLASE-LLEGADA-W     PIC X.
                    07 NRO-LLEGADA-W       PIC X(6).
              03 ART-LLEGADA-W.
                 05 GRUPO-LLEGADA-W        PIC XX.
                 05 ART-COMP-LLEGADA-W.
                    07 COD-ART-LLEGADA-W   PIC X(13).                  
                    07 CLASE-ART-LLEGADA-W PIC XX.
                 05 ITEM-LLEGADA-W         PIC XX.
           02 RADIOLOGO-LLEGADA-W          PIC X(10).
           02 REG-MEDICO-LLEGADA-W         PIC X(10).
           02 NOMBRE-MEDICO-LLEGADA-W      PIC X(30).
           02 TECNOLOGO-LLEGADA-W          PIC X(10).
           02 NOMBRE-TECNOLOGO-LLEGADA-W   PIC X(30).
           02 TIPO-DX-LLEGADA-W            PIC X.
           02 DX-LLEGADA-W                 PIC X(4).
           02 DESCRIP-DX-LLEGADA-W         PIC X(80).
           02 BIRADS-LLEGADA-W             PIC X.
           02 COMPLEJIDAD-LLEGADA-W        PIC X.
           02 FECHA-MODIF-LLEGADA-W        PIC X(8).
           02 HORA-MODIF-LLEGADA-W         PIC X(6).
           02 PAGINA-LLEGADA-W             PIC X.
           02 NOMBRE-TXT-LLEGADA-W         PIC X(60).
           02 OPERADOR-LLEGADA-W           PIC X(4).

       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.
          02 FILLER                    PIC X VALUE "|".
          02 DAT1-ENV                  PIC X(500).

      
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
              MOVE "RX-421W"                TO MSJ3-HTML
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
              MOVE "RX-421W"                TO MSJ3-HTML
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
              MOVE "RX-421W"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.


       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PLANO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PLANO-W              TO MSJ2-HTML
              MOVE "RX-421W"                TO MSJ3-HTML
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

           INITIALIZE DATO-LLEGADA-W LLAVE-SESION-W

           UNSTRING LINEA-LLEGAD-W DELIMITED BY "|"
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W,
               NIT-LLEGADA-W, SUC-LLEGADA-W, CLASE-LLEGADA-W, NRO-LLEGADA-W, GRUPO-LLEGADA-W , COD-ART-LLEGADA-W, 
               CLASE-ART-LLEGADA-W, ITEM-LLEGADA-W, RADIOLOGO-LLEGADA-W, REG-MEDICO-LLEGADA-W, NOMBRE-MEDICO-LLEGADA-W,
               TECNOLOGO-LLEGADA-W, NOMBRE-TECNOLOGO-LLEGADA-W, TIPO-DX-LLEGADA-W, DX-LLEGADA-W, DESCRIP-DX-LLEGADA-W,
               BIRADS-LLEGADA-W, COMPLEJIDAD-LLEGADA-W, FECHA-MODIF-LLEGADA-W, HORA-MODIF-LLEGADA-W, PAGINA-LLEGADA-W,
               NOMBRE-TXT-LLEGADA-W, OPERADOR-LLEGADA-W
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
                        MOVE "RX-421W-02"                  TO MSJ3-HTML
                        GO TO ENVIAR2-ERROR
             WHEN OTHER GO TO CERRAR-SESION
           END-EVALUATE.
      
           INITIALIZE LLAVE-USUNET.

       LEER-USUARIO.
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
           CLOSE ARCHIVO-USUNET.
      
           IF NOMBRE-USUNET = SPACES
              MOVE "SC-2"                        TO MSJ1-HTML
              MOVE "Falta configurar usuario"    TO MSJ2-HTML
              MOVE "RX-421W-02"                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.
           INITIALIZE NOM-RES-RX

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           
           MOVE "\\" TO NOM-RES-RX

           INSPECT NOM-RES-RX REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET        

           INSPECT NOM-RES-RX REPLACING FIRST "                        "
                                           BY "\PROG\DATOS\SC-RESRX.DAT".
                                                                                                                                                                                                            

       ABRIR-PLANO.
           OPEN I-O RESULTADOS-RX.

           MOVE LLAVE-LLEGADA-W TO LLAVE-RX
           READ RESULTADOS-RX WITH NO LOCK
                INVALID KEY
                   MOVE "SC-2"                      TO MSJ1-HTML
                   MOVE "Comprobante no Existe!"    TO MSJ2-HTML
                   MOVE "RX421W-02"                 TO MSJ3-HTML
                   GO TO ENVIAR2-ERROR
           END-READ.
           
           MOVE RADIOLOGO-LLEGADA-W TO ID-RADIOLOGO-RX
           MOVE REG-MEDICO-LLEGADA-W TO REG-MEDICO-RX
           MOVE NOMBRE-MEDICO-LLEGADA-W TO NOM-MEDICO-RX
           MOVE TECNOLOGO-LLEGADA-W TO ID-TECNOLOGO-RX
           MOVE NOMBRE-TECNOLOGO-LLEGADA-W TO NOM-TECNOLOGO-RX
           MOVE TIPO-DX-LLEGADA-W TO TIPO-DX-RX
           MOVE DX-LLEGADA-W TO DX-RX
           MOVE DESCRIP-DX-LLEGADA-W TO DESCRIP-DX-RX
           MOVE BIRADS-LLEGADA-W TO BIRADS-RX
           MOVE COMPLEJIDAD-LLEGADA-W TO COMPLEJIDAD-RX
           MOVE FECHA-MODIF-LLEGADA-W TO FECHA-MODIF-RX
           MOVE HORA-MODIF-LLEGADA-W TO HORA-MODIF-RX
           MOVE OPERADOR-LLEGADA-W TO ADMI-MODIF-RX. 


           
       LEER-PLANO.
           MOVE NOMBRE-TXT-LLEGADA-W TO NOM-PLANO-W
           OPEN INPUT ARCHIVO-PLANO.

           READ ARCHIVO-PLANO NEXT WITH NO LOCK AT END GO TO CERRAR-PLANO.

           IF DATOS-PLANO = SPACES OR LOW-VALUES
              GO TO CERRAR-PLANO
           END-IF
           
           EVALUATE PAGINA-LLEGADA-W 
               WHEN 1 MOVE DATOS-PLANO TO TAB-RESULTADO-PPAL-RX
               WHEN 2 MOVE DATOS-PLANO TO TAB-COMP-PPAL-RX
               WHEN 3 MOVE DATOS-PLANO TO TAB-ADIC-PPAL-RX
           END-EVALUATE.
 
       CERRAR-PLANO.
           REWRITE REG-RX END-REWRITE.

           CLOSE ARCHIVO-PLANO
                 RESULTADOS-RX.

010700 PAGINA-CONFIG.  
010720     MOVE "statuscode" TO COBW3-CNV-NAME
010730     MOVE "00"         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010720     MOVE "datosrecibidos" TO COBW3-CNV-NAME
010730     MOVE "Modificado correctamente!"  TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010720     MOVE "programa-id" TO COBW3-CNV-NAME
010730     MOVE "RX-421W-02"      TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010760     MOVE "..\PAGINAS\RECIBIDOSPLANO.ASPX" TO SALIDA-HTML
010770     PERFORM ABRIR-HTML.

      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.
      
       COPY "..\..\FUENTES\SC-WEB19.CBL".
