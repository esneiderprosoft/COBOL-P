      *============================================================
      * INVENTARIOS -CALCULA -COSTO -PROMEDIO -VALORIZA MOVIENTOS
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-USUAR-W
      *"../CONTROL/SC-ARCHUSU.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.              

           SELECT MAESTRO-ARTICULOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-ART-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY COD-ART
                  ALTERNATE RECORD KEY CLASE-ART       WITH DUPLICATES
                  ALTERNATE RECORD KEY COD-BARRAS-ART  WITH DUPLICATES
                  ALTERNATE RECORD KEY DESCRIP-ART     WITH DUPLICATES
                  ALTERNATE RECORD KEY USO-ART         WITH DUPLICATES
                  ALTERNATE RECORD KEY CL-ROTAC-ART    WITH DUPLICATES
                  ALTERNATE RECORD KEY UBICAC-ART      WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-VENCE-ART WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.
     
           SELECT ARCHIVO-SALDOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-SALDOS-W
      * "SC-SALDO.DAT"
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-SAL
                  ALTERNATE RECORD KEY COD-ART-SAL  WITH DUPLICATES
                  ALTERNATE RECORD KEY COD-LOTE-SAL WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-TRANSACC LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TRANS-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS LLAVE-TRANS
                  FILE STATUS IS OTR-STAT.

           SELECT MOVIMIENTO-INVENT LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-INVENT-W
      * "SC-MOVINV.DAT"
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-INV
                  ALTERNATE RECORD KEY COD-ART-INV   WITH DUPLICATES
                  ALTERNATE RECORD KEY SECUENCIA-INV
                  ALTERNATE RECORD KEY NIT-INV           WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-INV         WITH DUPLICATES
                  ALTERNATE RECORD KEY LLAVE-DOC-CTL-INV WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-GRUPOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-GRUPOS-W
      * "../control/SC-GRUPOINV.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-GRUPO
                  ALTERNATE RECORD KEY IS DESCRIP-GRUPO WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.                                                          
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-ARTIC.CBL".
       COPY "..\..\FUENTES\FD-SALDO.CBL".
       COPY "..\..\FUENTES\FD-TRANS.CBL".
       COPY "..\..\FUENTES\FD-INVEN.CBL".
       COPY "..\..\FUENTES\FD-GRUPO.CBL".

       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LLEGADA-W                          PIC X(100).

       77 NOM-USUAR-W                        PIC X(70).
       77 NOM-ART-W                          PIC X(70).
       77 NOM-SALDOS-W                       PIC X(70).
       77 NOM-TRANS-W                        PIC X(70).
       77 NOM-INVENT-W                       PIC X(70).
       77 NOM-GRUPOS-W                       PIC X(70).
       77  FECHA-CTL-W                       PIC 9(8).
       77  SW9                               PIC 9.
       01 SW-OK                              PIC X.
       01  SDO-VLR-W                         PIC 9(12)V99.
       01  SDO-CANT-W                        PIC 9(12)V9(4).
       01 VARIABLES.
          02 VLR-UNIT-W                      PIC 9(11)V99.
          02 VLR-VENTA-W                     PIC 9(11).
          02 VLR-VENTA-CALC                  PIC 9(11).
          02 MARGEN-W                        PIC 9(3)V9(4).
          02 VLR-ULT-COMPRA-W                PIC 9(12)V99.
          02 PROMEDIO-W                      PIC 9(11)V99.
          02 VLR-EST-UNIT-W                  PIC 9(11)V99.
          02 VLR-VEN-UNIT-W                  PIC 9(12)V99.

       01 FECHA-LT-GRP-W.
          02 ANO-LT-GRP-W                    PIC 9(4).
          02 MES-LT-GRP-W                    PIC 99.
          02 DIA-LT-GRP-W                    PIC 99.

       01 REG-TMP.
          02 LLAVE-TMP.
             03 COD-ALM-TMP                  PIC X(5).
             03 COD-ART-TMP.
                05 LLAVE-GRUPO-TMP.
                   07 TIPO-ART-TMP           PIC 9.
                   07 GRUPO-ART-TMP          PIC XX.
                05 NUMERO-ART-TMP            PIC X(13).
                05 CLASE-ART-TMP             PIC XX.
             03 COD-LOTE-TMP                 PIC 9(9).
     
          02 DATOS-TMP.
             03 TABLA-SAL-TMP.
                05 TAB-SAL-TMP OCCURS 32.
                   07 ACUM-ENT-CANT-TMP      PIC 9(12)V99.
                   07 ACUM-ENT-VLR-TMP       PIC 9(12)V99.
                   07 ACUM-SAL-CANT-TMP      PIC 9(12)V99.
                   07 ACUM-SAL-VLR-TMP       PIC 9(12)V99.
                   07 DEP-ENT-VLR-TMP        PIC 9(12).
                   07 INF-ENT-VLR-TMP        PIC 9(12).
                   07 INFD-ENT-VLR-TMP       PIC 9(12).
                   07 DEP-SAL-VLR-TMP        PIC 9(12).
                   07 INF-SAL-VLR-TMP        PIC 9(12).
                   07 INFD-SAL-VLR-TMP       PIC 9(12).
             03 VLR-RAZ-NIIF-VLR-TMP         PIC 9(12).
             03 SDO-ANT-NIIF-CANT-TMP        PIC 9(12).
             03 SDO-ANT-NIIF-VLR-TMP         PIC 9(12).
             03 ACUM-ENT-NIIF-CANT-TMP       PIC 9(12).
             03 ACUM-ENT-NIIF-VLR-TMP        PIC 9(12).
             03 ACUM-SAL-NIIF-CANT-TMP       PIC 9(12).
             03 ACUM-SAL-NIIF-VLR-TMP        PIC 9(12).
             03 SDO-ACT-NIIF-CANT-TMP        PIC 9(12).
             03 SDO-ACT-NIIF-VLR-TMP         PIC 9(12).
             03 DEP-ANT-NIIF-VLR-TMP         PIC 9(12).
             03 DEP-ACT-NIIF-VLR-TMP         PIC 9(12).
             03 COD-RECALC-SAL-TMP           PIC X(10).
             03 FILLER                       PIC X(1088).
             03 DEP-TRA-VLR-TMP              PIC 9(12).


    
       01 LINEA-DSP1-EDIT.
          02 NOMBRE-DSP1-EDIT                PIC X(10).
          02 DATO-DSP1-EDIT                  PIC X(21).

       01 REG-INV-W.
          02 LLAVE-INV-W.
             03 LLAVE-ART-INV-W.
                05 ALMACEN-INV-W.
                   07 ALM1-INV-W             PIC XX.
                   07 ALM2-INV-W             PIC X(3).
                05 COD-ART-INV-W.
                   07 LLAVE-GRUPO-INV-W.
                      10 TIPO-ART-INV-W      PIC 9.
                      10 GRUPO-ART-INV-W.
                         15 GRP1-INV-W       PIC X.
                         15 GRP2-INV-W       PIC X.
                   07 NUMERO-ART-INV-W       PIC X(13).
                   07 CLASE-INV-W            PIC XX.
                05 COD-LOTE-INV-W            PIC 9(9).
             03 FECHA-INV-W.
                05 ANO-INV-W                  PIC 9(4).
                05 MES-INV-W                  PIC 99.
                05 DIA-INV-W                  PIC 99.
             03 SECUENCIA-INV-W.
                05 LLAVE-SEC-W.
                   07 LLAVE-TRANS-INV-W.
                      10 COD-TRANS-INV-W.
                         15 COD1-TRANS-INV-W  PIC X.
                         15 COD2-TRANS-INV-W  PIC X.
                      10 SUC-TRANS-INV-W      PIC X.
                   07 COMPROB-INV-W           PIC X(6).
                05 SECU-INV-W                 PIC X(3).
          02 DATOS-INV-W.
             05 DOCUM-INV-W                   PIC X(6).
             05 CANT-INV-W                    PIC 9(12)V99.
             05 VLR-INV-W                     PIC 9(12)V99.
             05 VLR-VEN-INV-W                 PIC 9(12)V99.
             05 NIT-INV-W                     PIC 9(10).
          02 DETALLE-INV-W.
             05 DETALLE1-INV-W                PIC X(6).
             05 DETALLE2-INV-W                PIC X(4).
             05 DETALLE3-INV-W                PIC X(40).
          02 COSTO-INV-W.
             05 COSTO1-INV-W                  PIC X.
             05 COSTO2-INV-W                  PIC X.
             05 COSTO4-INV-W                  PIC XX.
          02 DOCUM-INV-W                      PIC X(6).
          02 FACT-REM-INV-W                   PIC 9.
          02 DIV-INV-W                        PIC XX.
          02 OBSERV-INV-W                     PIC X(30).
          02 NIT-RECIBE-INV-W                 PIC 9(10).
          02 INI-FACT-INV-W                   PIC X.
          02 OBSERV2-INV-W                    PIC X(30).
          02 ORD-COMPRA-INV-W                 PIC 9(6).
          02 TIP-DOC-INV-W                    PIC X.
          02 VEND-INV-W                       PIC X(5).
          02 LLAVE-DOC-CTL-INV-W.
             05 TIPO-DOC-CTL-INV-W            PIC X(4).
             05 NIT-CTL-INV-W                 PIC 9(10).
             05 DOCUM-CTL-INV-W               PIC 9(6).
          02 RESERVA-INV-W.
             05 ALF-RESERV-INV-W              PIC X(4).
             05 TIP-RESERV-INV-W              PIC 9.
             05 NRO-RESERV-INV-W              PIC 9(5).
          02 REFER-INV-W                      PIC X(10).
          02 FECHA-VENCE-INV-W.
             05 ANO-VENCE-INV-W               PIC 9(4).
             05 MES-VENCE-INV-W               PIC 9(2).
             05 DIA-VENCE-INV-W               PIC 9(2).
          02 FILLER                           PIC X(1972).
          02 REST-INV-W.
             05 OPER-ELAB-INV-W               PIC X(4).
             05 FECHA-ELAB-INV-W              PIC X(6).
             05 HORA-ELAB-INV-W               PIC X(4).
             05 OPER-MOD-INV-W                PIC X(4).
             05 FECHA-MOD-INV-W               PIC X(6).
             05 HORA-MOD-INV-W                PIC X(4).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W                   PIC X(15).
             03 FECHA-LLEGAD-W                PIC X(8).
             03 HORA-LLEGAD-W                 PIC X(6).
          02 DIR-CONTAB-LLEGADA-W             PIC X(21).
          02 MES-CONTAB-LLEGADA-W             PIC XX.
          02 LLAVE-010-LNK.
             03 LLAVE-ART-010-LNK.
                05 LOCAL-010-LNK                  PIC X(5).
                05 CTA-010-LNK.
                   07 TIPO-010-LNK                PIC 9.
                   07 GRUPO-010-LNK               PIC XX.
                   07 NUMERO-010-LNK              PIC X(13).
                   07 CLASE-010-LNK               PIC XX.
             03 COD-LOTE-010-LNK                  PIC 9(9).


       01 DATOS-ENVIO.
          02 ESTADO-ENV                   PIC XX.
          02 FILLER                       PIC X VALUE "|". 
         
      
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
              MOVE "INV808"                TO MSJ3-HTML
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
              MOVE "INV808"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USUAR-W              TO MSJ2-HTML
              MOVE "INV808"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR    
           END-IF. 

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MAESTRO-ARTICULOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-ART-W                TO MSJ2-HTML
              MOVE "INV808"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR    
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TRANSACC.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-TRANS-W              TO MSJ2-HTML
              MOVE "INV808"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR    
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SALDOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-SALDOS-W             TO MSJ2-HTML
              MOVE "INV808"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR    
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MOVIMIENTO-INVENT.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-INVENT-W             TO MSJ2-HTML
              MOVE "INV808"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR    
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-GRUPOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-GRUPOS-W             TO MSJ2-HTML
              MOVE "INV808"                 TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.

       INICIAR-SESION.
           MOVE "D:\PROGELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W 
         
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.
              
       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\PROGELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
     
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

           INITIALIZE NOM-SALDOS-W NOM-ART-W NOM-USUAR-W NOM-INVENT-W NOM-GRUPOS-W
           MOVE "\\" TO NOM-SALDOS-W NOM-ART-W NOM-USUAR-W NOM-INVENT-W NOM-GRUPOS-W
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-SALDOS-W REPLACING FIRST "                    "
                                           BY IP-DATOS-USUNET

           INSPECT NOM-SALDOS-W REPLACING FIRST " "
                                           BY "\"

           INSPECT NOM-SALDOS-W REPLACING FIRST "                     "
                                           BY DIR-CONTAB-LLEGADA-W.

           MOVE NOM-SALDOS-W TO NOM-ART-W
                                NOM-USUAR-W
                                NOM-TRANS-W
                                NOM-INVENT-W
                                NOM-GRUPOS-W
                                
           INSPECT NOM-SALDOS-W REPLACING FIRST "                     "           
                                             BY "\CONTROL\SC-SALDO.DAT".              

           INSPECT NOM-ART-W REPLACING FIRST "                    "           
                                          BY "\CONTROL\MAESART.DAT".   

           INSPECT NOM-USUAR-W REPLACING FIRST "                       "           
                                            BY "\CONTROL\SC-ARCHUSU.DAT".
  
           INSPECT NOM-TRANS-W REPLACING FIRST "                        "           
                                            BY "\CONTROL\SC-TRANSINV.DAT".

           INSPECT NOM-INVENT-W REPLACING FIRST "                      "           
                                             BY "\CONTROL\SC-MOVINV.DAT".
                                             
           INSPECT NOM-GRUPOS-W REPLACING FIRST "                        "           
                                             BY "\CONTROL\SC-GRUPOINV.DAT".

       LEER-USUARIO.
           OPEN INPUT ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END
                MOVE 0 TO SW-OK
           END-READ.
           CLOSE ARCHIVO-USUARIOS.                                                                                                        

     
       ABRIR-ARCHIVOS.
           OPEN INPUT  ARCHIVO-TRANSACC
                       MAESTRO-ARTICULOS
                       ARCHIVO-GRUPOS
                I-O    MOVIMIENTO-INVENT
                       ARCHIVO-SALDOS.

       UBICAR-MOVIMIENTO.
           IF COD-LOTE-010-LNK = SPACES
              OR COD-LOTE-010-LNK = ZEROS
              OR COD-LOTE-010-LNK = LOW-VALUES
              INITIALIZE COD-LOTE-010-LNK
           END-IF.
     
           MOVE 0 TO SW9.
           MOVE LLAVE-010-LNK TO LLAVE-ART-INV.

           START MOVIMIENTO-INVENT KEY = LLAVE-ART-INV
                 INVALID KEY
                   PERFORM LEER-SALDOS
                   GO TO GRABAR-SALDOS
           END-START.

       LEER-MOVIMIENTO.
           READ MOVIMIENTO-INVENT NEXT WITH NO LOCK AT END
                GO TO GRABAR-SALDOS
           END-READ 

           IF COD-TRANS-INV = 99
              GO TO LEER-MOVIMIENTO
           END-IF.

           IF LLAVE-ART-INV = LLAVE-010-LNK
              CONTINUE
           ELSE
              GO TO GRABAR-SALDOS
           END-IF.

           MOVE LLAVE-INV   TO DATO-DSP1-EDIT
           MOVE "SECUENCIA" TO NOMBRE-DSP1-EDIT
          
           IF  LOTE-FARM-USU = "S"
               IF COD-LOTE-INV = COD-LOTE-010-LNK
                  CONTINUE
               ELSE
                  GO TO LEER-MOVIMIENTO
               END-IF
           END-IF.

       VALIDAR-LOTE.
           IF LOTE-FARM-USU = 1
              INITIALIZE COD-LOTE-010-LNK
              GO TO LEER-SALDOS
           END-IF.

           IF  LOTE-FARM-USU = 2
               IF (COD-TRANS-INV = "10" OR "1A")
                  MOVE LLAVE-GRUPO-INV TO LLAVE-GRUPO
                  READ ARCHIVO-GRUPOS WITH NO LOCK
                       INVALID KEY
                        MOVE "N"  TO OPC-LOTE-GR
                  END-READ
                  MOVE 2000        TO ANO-LT-GRP-W
                  ADD  ANO-LOTE-GR TO ANO-LT-GRP-W
                  MOVE MES-LOTE-GR TO MES-LT-GRP-W
                  MOVE DIA-LOTE-GR TO DIA-LT-GRP-W

                  IF FECHA-INV >= FECHA-LT-GRP-W
                     CONTINUE
                  ELSE
                     INITIALIZE COD-LOTE-010-LNK
                     GO TO LEER-SALDOS
                  END-IF
               ELSE
                  INITIALIZE COD-LOTE-010-LNK
                  GO TO LEER-SALDOS
               END-IF
           END-IF.

           IF LOTE-FARM-USU = 3
              MOVE LLAVE-GRUPO-INV TO LLAVE-GRUPO
              READ ARCHIVO-GRUPOS WITH NO LOCK
                   INVALID KEY
                     MOVE "N"  TO OPC-LOTE-GR
              END-READ
              MOVE 2000        TO ANO-LT-GRP-W
              ADD  ANO-LOTE-GR TO ANO-LT-GRP-W
              MOVE MES-LOTE-GR TO MES-LT-GRP-W
              MOVE DIA-LOTE-GR TO DIA-LT-GRP-W

              IF FECHA-INV >= FECHA-LT-GRP-W
                 CONTINUE
              ELSE
                 INITIALIZE COD-LOTE-010-LNK
                 GO TO LEER-SALDOS
              END-IF
           END-IF.

           IF COD-LOTE-INV = COD-LOTE-010-LNK
              CONTINUE
           ELSE
              GO TO LEER-MOVIMIENTO
           END-IF.

       LEER-SALDOS.
           IF SW9 = 0
              MOVE 1 TO SW9
              MOVE FECHA-INV  TO FECHA-CTL-W
              INITIALIZE REG-SALDOS
              MOVE LLAVE-010-LNK  TO LLAVE-SAL
              READ ARCHIVO-SALDOS WITH NO LOCK 
                   INVALID KEY
                     WRITE REG-SALDOS END-WRITE
              END-READ

              MOVE ACUM-ENT-CANT     (32)    TO SDO-CANT-W
              MOVE ACUM-ENT-VLR      (32)    TO SDO-VLR-W
              INITIALIZE TABLA-SAL
              MOVE SDO-CANT-W                TO ACUM-ENT-CANT (32) 
              MOVE SDO-VLR-W                 TO ACUM-ENT-VLR  (32).
               
       COMPARAR-TRANSACCION.
           IF  LLAVE-TRANS-INV = "091" OR "891"
               ADD  VLR-INV TO INF-ENT-VLR (DIA-INV)
               REWRITE REG-SALDOS END-REWRITE
               GO TO LEER-MOVIMIENTO
           END-IF.

           IF LLAVE-TRANS-INV = "0D1"
              ADD  VLR-INV TO DEP-ENT-VLR (DIA-INV)
              REWRITE REG-SALDOS END-REWRITE
              GO TO LEER-MOVIMIENTO
           END-IF.

           IF LLAVE-TRANS-INV = "8D1"
              ADD  VLR-INV TO DEP-TRA-VLR
              REWRITE REG-SALDOS END-REWRITE
              GO TO LEER-MOVIMIENTO
           END-IF.

           IF LLAVE-TRANS-INV = "0E1" OR "8E1"
              ADD  VLR-INV TO INFD-ENT-VLR (DIA-INV)
              REWRITE REG-SALDOS END-REWRITE
              GO TO LEER-MOVIMIENTO
           END-IF.

      ******DATOS NIIF********
           IF LLAVE-TRANS-INV = "0DN"
              ADD  VLR-INV TO DEP-ACT-NIIF-VLR
              REWRITE REG-SALDOS END-REWRITE
              GO TO LEER-MOVIMIENTO
           END-IF.

           IF LLAVE-TRANS-INV = "11N"
              ADD  VLR-INV TO VLR-RAZ-NIIF-SAL
              REWRITE REG-SALDOS END-REWRITE
              GO TO LEER-MOVIMIENTO
           END-IF.
      ******FIN DATOS NIIF********

           MOVE LLAVE-TRANS-INV TO LLAVE-TRANS.
           MOVE 1               TO SUC-TRANS.
           READ ARCHIVO-TRANSACC WITH NO LOCK
                INVALID KEY
                   IF COD1-TRANS = 2
                      MOVE "N" TO VR-COMPRA-TRANS
                      MOVE "S" TO VR-VENTA-TRANS
                   ELSE
                      MOVE "S" TO VR-COMPRA-TRANS
                   END-IF

           END-READ.

           IF  (COD-TRANS-INV = "20" OR "2A" OR "2a" OR "2b" OR "2c" OR "2d" OR "2e" OR "2f" OR "2g")
               MOVE "S" TO VR-VENTA-TRANS
           END-IF.

           IF  VR-VENTA-TRANS = "N" 
           AND VLR-VEN-INV IS NOT ZERO
               MOVE 0 TO VLR-VEN-INV
               REWRITE REG-INVENT END-REWRITE
           END-IF.

       VALORIZAR.
           IF VR-COMPRA-TRANS = "S"
              IF COD1-TRANS < 2
                 ADD CANT-INV TO SDO-CANT-W
                 ADD VLR-INV  TO SDO-VLR-W
              ELSE
                 SUBTRACT CANT-INV FROM SDO-CANT-W
                 SUBTRACT VLR-INV  FROM SDO-VLR-W
              END-IF
              GO TO SUMAR-ENTRADAS
           END-IF

           MOVE REG-INVENT TO REG-INV-W.

           IF  RETAIL-USU   = "S"
           AND TIPO-ART-INV =  0
               MOVE 0 TO PROMEDIO-W
           ELSE
               PERFORM CALCULAR-PROMEDIO
           END-IF

           MOVE COD-ART-TMP TO COD-ART
           READ MAESTRO-ARTICULOS WITH NO LOCK
                INVALID KEY  INITIALIZE DATOS-ART
           END-READ

           MOVE 0.85 TO MARGEN-W.

           IF VLR-ULT-COMPRA IS ZERO
              IF VLR-REF-ART IS ZERO
                 MOVE VLR-LISTA-COMP-ART TO VLR-ULT-COMPRA
              ELSE
                 COMPUTE VLR-ULT-COMPRA = VLR-REF-ART * 0.85
              END-IF
           END-IF.

           IF LLAVE-GRUPO-INV = "0SB" OR "0AB" OR "0EM"
              COMPUTE VLR-ULT-COMPRA-W = VLR-ULT-COMPRA * 4
           ELSE
              COMPUTE VLR-ULT-COMPRA-W = VLR-ULT-COMPRA * 2
           END-IF.

           IF  (COD-TRANS-INV = "20" OR "2A" OR "2a" OR "2b" OR "2c" OR "2d" OR "2e" OR "2f" OR "2g")
           AND LIMI-COSTO-USU = "N"
               IF VLR-VEN-INV > 0
                  MOVE VLR-VEN-INV TO VLR-VENTA-CALC
               ELSE
                  IF VR-VENTA1-ART(1) > 0
                     COMPUTE VLR-VENTA-CALC ROUNDED = CANT-INV * VR-VENTA1-ART(1)
                  ELSE
                     MOVE 0 TO VLR-VENTA-CALC
                  END-IF
               END-IF
               IF CANT-INV IS NOT = 0
                  COMPUTE VLR-EST-UNIT-W = VLR-INV / CANT-INV
               ELSE
                  INITIALIZE VLR-EST-UNIT-W
               END-IF
               IF  VLR-EST-UNIT-W > 1
               AND VLR-INV > (VLR-VENTA-CALC * 4)
                   COMPUTE VLR-INV = (VLR-VENTA-CALC * MARGEN-W)
               END-IF
           END-IF

           IF CANT-INV IS NOT = ZEROS
              COMPUTE VLR-VEN-UNIT-W = VLR-VEN-INV / CANT-INV
           ELSE
              INITIALIZE VLR-VEN-UNIT-W
           END-IF.

           IF (PROMEDIO-W > VLR-VEN-UNIT-W)
           AND VLR-VEN-UNIT-W > 1
           AND VLR-ULT-COMPRA-W > 0
           AND LIMI-COSTO-USU IS NOT = "S"
           AND ANO-INV > 2006
                IF (COD-TRANS-INV  = 21 OR "2C")
                   CONTINUE
                ELSE
                   MOVE 0 TO VLR-INV
                END-IF
           ELSE
               COMPUTE VLR-INV ROUNDED = PROMEDIO-W * CANT-INV
           END-IF.


           IF RETAIL-USU = "S"
           AND TIPO-ART-INV =  0
              MOVE 0 TO VLR-INV
           ELSE
               IF  ALM1-INV = "CN"
               AND SDO-INI-VLR-W  IS ZERO
               AND SDO-AC-VLR-ENT-W  IS ZERO
                   MOVE 0 TO VLR-INV
               ELSE
                  IF VLR-INV IS ZERO
                     IF VR-VENTA1-ART(1) IS ZERO
                        MOVE VLR-REF-ART TO VR-VENTA1-ART(1)
                     END-IF
                     IF  VR-VENTA1-ART(1) > 0
                     AND VLR-ULT-COMPRA > VR-VENTA1-ART(1)
                         INITIALIZE VLR-ULT-COMPRA
                     END-IF
                  END-IF
                  IF VLR-INV IS ZERO
                     IF VLR-ULT-COMPRA > 0
                        COMPUTE VLR-INV ROUNDED = VLR-ULT-COMPRA * CANT-INV
                     ELSE
                        IF VLR-VEN-INV > 0
                           COMPUTE VLR-INV ROUNDED = VLR-VEN-INV * MARGEN-W
                        ELSE
                           COMPUTE VLR-INV ROUNDED = (VR-VENTA1-ART(1) * MARGEN-W)
                           COMPUTE VLR-INV ROUNDED = (VR-VENTA1-ART(1) * CANT-INV)
                        END-IF
                     END-IF
                  END-IF
               END-IF
           END-IF     

           IF  CANT-INV = SDO-CANT-W
           AND COD1-TRANS > 1
               MOVE SDO-VLR-W TO VLR-INV
           END-IF
     
           IF COD1-TRANS < 2
              ADD CANT-INV TO SDO-CANT-W
              ADD VLR-INV  TO SDO-VLR-W
           ELSE
              SUBTRACT CANT-INV FROM SDO-CANT-W
              SUBTRACT VLR-INV  FROM SDO-VLR-W
           END-IF.

           IF REG-INVENT IS NOT = REG-INV-W
              REWRITE REG-INVENT END-REWRITE
           END-IF.

       SUMAR-ENTRADAS.
           IF COD1-TRANS < 2
              ADD CANT-INV TO ACUM-ENT-CANT   (DIA-INV)
              ADD VLR-INV  TO ACUM-ENT-VLR    (DIA-INV)
           ELSE
              ADD CANT-INV TO ACUM-SAL-CANT   (DIA-INV)
              ADD VLR-INV  TO ACUM-SAL-VLR    (DIA-INV)
           END-IF.

           IF (COD-TRANS-INV = "10" OR 12 OR "1A" OR "1B" OR "1C" OR "1E")
              PERFORM GRABAR-ULTIMA-COMPRA
           END-IF.

           GO TO LEER-MOVIMIENTO.

       GRABAR-SALDOS.

           REWRITE REG-SALDOS END-REWRITE.

       CERRAR-ARCHIVOS.
           CLOSE ARCHIVO-TRANSACC
                 ARCHIVO-GRUPOS
                 ARCHIVO-SALDOS
                 MOVIMIENTO-INVENT
                 MAESTRO-ARTICULOS.

       ENVIAR-DATOS.
            
           MOVE "00"             TO ESTADO-ENV.
       
       ENVIO-DATOS.
           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO      TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3
         
           MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
           PERFORM ABRIR-HTML.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.

       GRABAR-ULTIMA-COMPRA.
           MOVE COD-ART-INV TO COD-ART

           READ MAESTRO-ARTICULOS WITH NO LOCK
                INVALID KEY
                    INITIALIZE VLR-ULT-COMPRA TAB-VTA-ART
                NOT INVALID KEY
                IF FECHA-ULT-COMPRA <= FECHA-INV
                   OR VLR-ULT-COMPRA IS ZERO


                    IF CANT-INV = 0
                       INITIALIZE VLR-UNIT-W 
                    ELSE
                       COMPUTE VLR-UNIT-W ROUNDED = VLR-INV / CANT-INV
                    END-IF

                    IF VLR-UNIT-W = VLR-ULT-COMPRA
                       CONTINUE
                    ELSE
                       IF FORMA-LIQ-ART = 2
                          PERFORM CALCULAR-PRECIO-VENTA VARYING I FROM 1 BY 1
                                                               UNTIL I > 20
                       END-IF
                    END-IF

                    CLOSE      MAESTRO-ARTICULOS
                    OPEN I-O   MAESTRO-ARTICULOS
                    MOVE VLR-UNIT-W    TO VLR-ULT-COMPRA
                    MOVE FECHA-INV     TO FECHA-ULT-COMPRA
                    MOVE CANT-INV      TO ULT-CANT-COMPRA-ART 
                    MOVE NIT-INV       TO ID-ART

                    REWRITE REG-MAESTRO
                    CLOSE      MAESTRO-ARTICULOS
                    OPEN INPUT MAESTRO-ARTICULOS
                END-IF 
           END-READ.

       CALCULAR-PRECIO-VENTA.

           COMPUTE VR-VENTA1-ART (I) ROUNDED = VLR-UNIT-W  * (100 + PORC-INC-COMP-ART (I)) / 100.

       CALCULAR-PROMEDIO.
           INITIALIZE PROMEDIO-W.
          
           IF SDO-CANT-W   > 0
              COMPUTE PROMEDIO-W ROUNDED = SDO-VLR-W / SDO-CANT-W  
           END-IF.

           IF PROMEDIO-W < 0
            MOVE 0 TO PROMEDIO-W
           END-IF.
      
       COPY "..\..\FUENTES\SC-WEB19.CBL".     
