      *============================================================
      * CONSULTA EL SALDO ACTUAL DE UN ARTICULO  - INV808S
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-SALDOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-SAL-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-SAL
                  ALTERNATE RECORD KEY COD-ART-SAL  WITH DUPLICATES
                  ALTERNATE RECORD KEY COD-LOTE-SAL WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-SALDO.CBL".
           
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LLEGADA-W                    PIC X(100).

       77 NOM-USUA-W                   PIC X(70).
       77 NOM-SAL-W                    PIC X(70).
       77 NOM-PLANO-W                  PIC X(90).
       77 CANT-EDIT                    PIC 9(14)V99.
       77 VLR-UNIT                     PIC 9(9)V99.
       01 SDO-ACT-CANT                 PIC 9(11)V99.
       01 SDO-ACT-VLR                  PIC 9(12)V99.
       01 SDO-ACT-CANT-EDIT            PIC ZZZZZZZZZZZ.ZZ.
       01 SDO-ACT-VLR-EDIT             PIC ZZZZZZZZZZZZ.ZZ.
       01 SDO-AC-CANT-SAL-W            PIC S9(12)V99.
       01 PUC-USU                      PIC X.
       01 LOTE-FARM-USU                PIC X.

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(30).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 LLAVE-W.
             03 LOCAL-W                PIC X(5).
             03 CTA-W.
                 05 TIPO-W             PIC 9.
                 05 GRUPO-W            PIC XX.
                 05 NUMERO-W           PIC X(13).
                 05 CLASE-W            PIC XX.
             03 COD-LOTE-W             PIC X(4).
          02 DIA-CORTE-W               PIC XX.

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV            PIC X(2).
          02 FILLER                    PIC X VALUE "|".
          02 SDO-ACT-CANT-ENV          PIC X(14).  
          02 FILLER                    PIC X VALUE "|".
          02 SDO-ACT-VLR-ENV           PIC X(15).
      
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
              MOVE "INV808S"                TO MSJ3-HTML
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
              MOVE "INV808S"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SALDOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-SAL-W                TO MSJ2-HTML
              MOVE "INV808S"                TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W,
                   LOCAL-W, CTA-W, DIA-CORTE-W, PUC-USU
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE   TO FECHA-TOTAL.
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

           INITIALIZE NOM-SAL-W
           MOVE "\\" TO NOM-SAL-W

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-SAL-W REPLACING FIRST "                    "
                                           BY IP-DATOS-USUNET

           INSPECT NOM-SAL-W REPLACING FIRST " "
                                           BY "\"

           INSPECT NOM-SAL-W REPLACING FIRST "                              "
                                           BY DIR-CONTAB-LLEGADA-W

           INSPECT NOM-SAL-W REPLACING FIRST "                     "           
                                           BY "\CONTROL\SC-SALDO.DAT".
                                                           

       ABRIR-USUARIOS.
           IF CTA-W = SPACES
              MOVE "01" TO SW-INVALID-ENV
              GO TO CERRAR-ARCHIVOS.

           IF LOTE-FARM-USU = "S" OR "N"
              CONTINUE
           ELSE
              IF (PUC-USU = 4 OR 6)
                 MOVE "S" TO LOTE-FARM-USU
              ELSE
                 MOVE "N" TO LOTE-FARM-USU
              END-IF.

           IF LOTE-FARM-USU = "N"
              MOVE SPACES TO COD-LOTE-W.
 
       ABRIR-ARCHIVOS.
            MOVE "00" TO SW-INVALID-ENV
            OPEN INPUT  ARCHIVO-SALDOS.
            INITIALIZE REG-SALDOS

            MOVE CTA-W TO COD-ART-SAL.
            START ARCHIVO-SALDOS KEY = COD-ART-SAL
                          INVALID KEY
                            MOVE "01"  TO SW-INVALID-ENV
                            INITIALIZE SDO-ACT-CANT-ENV SDO-ACT-VLR-ENV
                            GO TO CERRAR-ARCHIVOS
            END-START.

       LEER-SALDOS.
            READ ARCHIVO-SALDOS WITH NO LOCK
            END-READ

            IF COD-ART-SAL IS NOT = CTA-W
               GO TO CERRAR-ARCHIVOS.

       MOSTRAR-SALDO.
            PERFORM CALCULAR-SALDO-ACTUAL VARYING DIA-SDO-W FROM 1 BY 1 UNTIL
                                                  DIA-SDO-W > DIA-CORTE-W.

       CERRAR-ARCHIVOS.
            CLOSE ARCHIVO-SALDOS.

       ENVIO-DATOS.
            MOVE SDO-ACT-CANT       TO SDO-ACT-CANT-EDIT
            MOVE SDO-ACT-CANT-EDIT  TO SDO-ACT-CANT-ENV
            MOVE SDO-ACT-VLR        TO SDO-ACT-VLR-EDIT
            MOVE SDO-ACT-VLR-EDIT   TO SDO-ACT-VLR-ENV.

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
       
       
       COPY "..\..\FUENTES\SC-WEB19.CBL".
       CALCULAR-SALDO-ACTUAL.
            IF DIA-SDO-W = 1
                ADD  ACUM-ENT-CANT (32)    TO SDO-ACT-CANT-W
                ADD  ACUM-ENT-VLR  (32)    TO SDO-ACT-VLR-W
            END-IF

            ADD ACUM-ENT-CANT (DIA-SDO-W)  TO SDO-ACT-CANT-W
            ADD ACUM-ENT-VLR  (DIA-SDO-W)  TO SDO-ACT-VLR-W
