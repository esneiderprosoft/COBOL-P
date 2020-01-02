      *=================================================================
      *GRABA COMPROBANTE LIBRO VIAJES SOLO TAXMETA 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-IP LOCK MODE IS AUTOMATIC
                  ASSIGN  TO NOM-IP-W
                  ORGANIZATION LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-PLANO LOCK MODE IS AUTOMATIC
                  ASSIGN  TO NOM-PLANO-W
                  ORGANIZATION LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-PLANO2 LOCK MODE IS AUTOMATIC
                  ASSIGN  TO NOM-PLANO2-W
                  ORGANIZATION LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.

           SELECT  optional ARCHIVO-INTER
                  ASSIGN TO NOM-INTERM-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  LOCK MODE IS AUTOMATIC;
                  RECORD KEY IS LLAVE-INT
                  ALTERNATE RECORD KEY IS FECHA-PLA-INT WITH DUPLICATES
                  ALTERNATE RECORD KEY IS NUMERO-INT WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LIBRO-INT
                  ALTERNATE RECORD KEY IS PLACA-INT WITH DUPLICATES
                  FILE STATUS OTR-STAT.

           SELECT ARCHIVO-CARROS LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-CARROS-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS CODIGO-CAR  	
                  ALTERNATE RECORD KEY IS PROPIET-CAR
                  ALTERNATE RECORD KEY IS INTERNO-CAR
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-TERCEROS LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-TERCE-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY   IS COD-TERCERO
                  ALTERNATE RECORD KEY DESCRIP-TER WITH DUPLICATES
                  ALTERNATE RECORD KEY ACT-TER WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-MAESTROS LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-MAE-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-MAE
                  ALTERNATE RECORD KEY IS NOMBRE-MAE WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT MOVIMIENTO-DIARIO LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-MOV-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-MOV
                  ALTERNATE RECORD KEY LLAVE-DOC-MOV WITH DUPLICATES
                  ALTERNATE RECORD KEY LLAVE-FEC
                  ALTERNATE RECORD KEY SECUENCIA-MOV
                  ALTERNATE RECORD KEY COSTO-MOV WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-AGENCIAS  LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-AGEN-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CODIGO-AGEN
                  FILE STATUS IS OTR-STAT.
      
       DATA DIVISION.
       FILE SECTION.

       FD  ARCHIVO-IP
           LABEL RECORD STANDARD.
       01  REG-IP.
           02 DATO-IP       PIC X(25).

       FD  ARCHIVO-PLANO
           LABEL RECORD STANDARD.
       01  REG-PLANO.
           02 DATO-PLANO       PIC X(100).

       FD  ARCHIVO-PLANO2
           LABEL RECORD STANDARD.
       01  REG-PLANO2.
           02 DATO-PLANO2       PIC X(1000).


       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".

       COPY "S:\NEWCOBOL\TAX\FD-INTER.CBL".
       COPY "S:\NEWCOBOL\TAX\FD-CARRO.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-TERCE.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-MAEST.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-MOVIM.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-AGENC.CBL".
      
       WORKING-STORAGE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".

       01  DATO-ENV-W.
           02 TIPO-ENV-W        PIC 9.
           02 FILLER            PIC X VALUE "|".
           02 VALOR-ENV-W       PIC X(60).


       01  FECHA-W.
           02 ANO-W.
              03 ANO1-W         PIC 99.
           02 FECHA2-W.
              03 ANO2-W         PIC 99.
              03 MES-W          PIC 99.
              03 DIA-W          PIC 99.

       01  NOM-PLANO-W.
           02 NOM1-PLANO-W      PIC X(16).
           02 NOM2-PLANO-W      PIC X(60).

       01  NOMBRES-W.
           02 NOM-IP-W          PIC X(60).
           02 NOM-PLANO2-W      PIC X(60).
           02 NOM-INTERM-W      PIC X(60).
           02 NOM-CARROS-W      PIC X(60).
           02 NOM-TERCE-W       PIC X(60).
           02 NOM-MAE-W         PIC X(60).
           02 NOM-MOV-W         PIC X(60).
           02 NOM-AGEN-W        PIC X(60).

       01  NUMERO-INT-W         PIC 9(6).

       01  DATO-REC-W.
           02 AGEN-REC-W        PIC 99.
           02 FECHA-REC-W       PIC 9(8).
           02 LIBRO-REC-W       PIC 9(9).
           02 PLACA-REC-W       PIC X(6).
           02 BRUTO-REC-W       PIC 9(12).
           02 SEGURO-REC-W      PIC 9(12).
           02 AVANCES-REC-W     PIC 9(12).
           02 OPER-REC-W        PIC 9(4).

       LINKAGE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\ISAPICTX.CBL".
       PROCEDURE DIVISION WITH stdcall LINKAGE USING ISAPI-CTX-CNT.
       
       DECLARATIVES.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUNET.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO ERROR-W
              MOVE 1 TO TIPO-ENV-W
              MOVE NOM-USU-W        TO VALOR-ENV-W
              GO TO CERRAR-SESION
           END-IF.
       
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO ERROR-W
              MOVE 1 TO TIPO-ENV-W
              MOVE NOM-SESION-W     TO VALOR-ENV-W 
              GO TO CERRAR-SESION
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-INTER.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"  OR "35"
              CONTINUE 
           ELSE
              MOVE OTR-STAT        TO ERROR-W
              MOVE 1 TO TIPO-ENV-W
              MOVE NOM-INTERM-W    TO VALOR-ENV-W
              MOVE OTR-STAT TO REG-PLANO2
              WRITE REG-PLANO2 BEFORE 1
              MOVE REG-INT TO REG-PLANO2
              WRITE REG-PLANO2 BEFORE 1
              GO TO CERRAR-SESION
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MOVIMIENTO-DIARIO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"          
              CONTINUE 
           ELSE
              MOVE OTR-STAT        TO ERROR-W
              MOVE 1 TO TIPO-ENV-W
              MOVE NOM-MOV-W       TO VALOR-ENV-W
              MOVE OTR-STAT TO REG-PLANO2
              WRITE REG-PLANO2 BEFORE 1
              MOVE NOM-MOV-W TO REG-PLANO2
              WRITE REG-PLANO2 BEFORE 1
              GO TO CERRAR-SESION
           END-IF.
        
        END DECLARATIVES.

       ABRIR-LOG.
           MOVE "C:\PROSOFT\LOG.TXT" TO NOM-PLANO2-W.
           OPEN OUTPUT ARCHIVO-PLANO2.
       
       INICIAR-IIS.
           MOVE LOW-VALUE TO COBW3.
           MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
           CALL "COBW3_INIT" USING COBW3.
      
       LEER-DATO-HTML.
           INITIALIZE NOMBRES-W.
           MOVE "C:\PROSOFT\TEMP\" TO NOM1-PLANO-W.

           MOVE "DATO-H"          TO COBW3-SEARCH-DATA.
           CALL "COBW3_GET_VALUE" USING COBW3.
           MOVE COBW3-GET-DATA    TO NOM2-PLANO-W.


           MOVE NOM-PLANO-W TO REG-PLANO2
           WRITE REG-PLANO2 BEFORE 1.

       ABRIR-PLANO.
           MOVE "C:\PROSOFT\IP.TXT" TO NOM-IP-W.
           OPEN INPUT ARCHIVO-IP.

           IF OTR-STAT = "35"
              OPEN OUTPUT ARCHIVO-IP
              CLOSE ARCHIVO-IP
              MOVE OTR-STAT           TO ERROR-W
              MOVE "Archivo IP vacio" TO MENSAJE1-W
              INITIALIZE MENSAJE2-W
              GO TO MOSTRAR-ERROR                	
           END-IF.

           READ ARCHIVO-IP NEXT AT END
                CLOSE ARCHIVO-IP.

       ASIGNAR-NOMBRES.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           
           MOVE 2020 TO ANO-TOTAL

           MOVE DATO-IP          TO NOM-INTERM-W 
           INSPECT NOM-INTERM-W  REPLACING FIRST "    " BY ANO-TOTAL
           INSPECT NOM-INTERM-W  REPLACING FIRST "                     "
                                           BY    "\control\AR-INTER.DAT"

           MOVE DATO-IP          TO NOM-CARROS-W 
           INSPECT NOM-CARROS-W  REPLACING FIRST "    " BY ANO-TOTAL
           INSPECT NOM-CARROS-W  REPLACING FIRST "                 "
                                           BY    "\control\ARCHCARR"

           MOVE DATO-IP         TO NOM-TERCE-W 
           INSPECT NOM-TERCE-W  REPLACING FIRST "    " BY ANO-TOTAL
           INSPECT NOM-TERCE-W  REPLACING FIRST "                "
                                          BY    "\control\ARCHTER"

           MOVE DATO-IP         TO NOM-MAE-W 
           INSPECT NOM-MAE-W    REPLACING FIRST "    " BY ANO-TOTAL
           INSPECT NOM-MAE-W    REPLACING FIRST "                "
                                          BY    "\control\ARCHMAE"

           MOVE DATO-IP         TO NOM-AGEN-W 
           INSPECT NOM-AGEN-W   REPLACING FIRST "    " BY ANO-TOTAL
           INSPECT NOM-AGEN-W   REPLACING FIRST "                 "
                                          BY    "\control\ARCHAGEN"

           MOVE DATO-IP         TO NOM-MOV-W 
           INSPECT NOM-MOV-W    REPLACING FIRST "    " BY ANO-TOTAL
           INSPECT NOM-MOV-W    REPLACING FIRST "               "
                                          BY    "\MM\ARCHMOV.DAT"
           INSPECT NOM-MOV-W    REPLACING FIRST "MM" BY MES-TOTAL.


       ABRIR-PLANO.
           OPEN INPUT ARCHIVO-PLANO

           IF OTR-STAT = "35"
              OPEN OUTPUT ARCHIVO-PLANO
              CLOSE ARCHIVO-PLANO
              MOVE "Archivo Plano " TO MENSAJE1-W
              INITIALIZE MENSAJE2-W
              GO TO MOSTRAR-ERROR
           END-IF.

             
       ABRIR-ARCHIVOS.     
           OPEN INPUT ARCHIVO-CARROS
                      ARCHIVO-AGENCIAS
                      ARCHIVO-TERCEROS
                      ARCHIVO-MAESTROS.
      
           OPEN I-O  ARCHIVO-INTER.

           IF OTR-STAT = "35"
              OPEN OUTPUT  ARCHIVO-INTER
              CLOSE ARCHIVO-INTER
              OPEN I-O  ARCHIVO-INTER
           END-IF.

           PERFORM BUSCAR-NUMERO.

       REVISAR-NUMERO.
            MOVE NUMERO-INT-W TO NUMERO-INT.
            READ ARCHIVO-INTER RECORD KEY IS NUMERO-INT
                 INVALID KEY
                   GO TO LEER-PLANO 
                 NOT INVALID KEY
                   ADD 1 TO NUMERO-INT-W
                   GO TO REVISAR-NUMERO.

       LEER-PLANO.
           INITIALIZE REG-PLANO.
           READ ARCHIVO-PLANO NEXT AT END
                              MOVE REG-PLANO TO REG-PLANO2
                              WRITE REG-PLANO2 BEFORE 1

                              CLOSE ARCHIVO-PLANO
                              GO TO FIN-PLANO.

           UNSTRING REG-PLANO DELIMITED BY ","
             INTO AGEN-REC-W, FECHA-REC-W, LIBRO-REC-W, PLACA-REC-W, BRUTO-REC-W,
                  SEGURO-REC-W, AVANCES-REC-W, OPER-REC-W,
           END-UNSTRING.

           IF LIBRO-REC-W > 0
              CONTINUE
           ELSE
              GO TO LEER-PLANO
           END-IF.


       GRABAR-COMPROBANTE.
           INITIALIZE REG-INT.

           MOVE FECHA-REC-W       TO FECHA-W
              
           MOVE AGEN-REC-W        TO AGEN-INT
           MOVE FECHA2-W          TO FECHA-PLA-INT 
           MOVE NUMERO-INT-W      TO NUMERO-INT
           MOVE LIBRO-REC-W       TO LIBRO-INT

           READ ARCHIVO-INTER WITH NO LOCK
                INVALID KEY
                  MOVE FECHA2-W          TO FECHA-COMP-INT 
                  MOVE PLACA-REC-W       TO PLACA-INT
                  MOVE BRUTO-REC-W       TO VR-BRUTO-INT
                  MOVE SEGURO-REC-W      TO VR-SEGURO-INT
                  MOVE AVANCES-REC-W     TO VR-AVANCE-INT
                  ACCEPT FECHA-ELAB-INT FROM DATE
                  MOVE OPER-REC-W        TO OPER-ELAB-INT
                  WRITE REG-INT
           END-READ.


           GO TO LEER-PLANO.

       FIN-PLANO.
           MOVE 0 TO TIPO-ENV-W.
           MOVE NUMERO-INT-W  TO VALOR-ENV-W.
           PERFORM GRABAR-NUMERO.


       CERRAR-ARCHIVOS.
           CLOSE ARCHIVO-INTER
                 ARCHIVO-CARROS
                 ARCHIVO-AGENCIAS
                 ARCHIVO-TERCEROS
                 ARCHIVO-MAESTROS.

       
       CERRAR-SESION.
           IF TIPO-ENV-W > 0
              INSPECT VALOR-ENV-W REPLACING FIRST " "
                                            BY    "|"
              INSPECT VALOR-ENV-W REPLACING FIRST "  "
                                            BY   OTR-STAT
           END-IF.

           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE DATO-ENV-W       TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3.
      
           MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
           PERFORM ABRIR-HTML.

           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.
       
       COPY "P:\PROG\PROYECT\FUENTES\SC-WEB.CBL".

       BUSCAR-NUMERO.
           MOVE DATO-IP         TO NOM-MOV-W 
           INSPECT NOM-MOV-W    REPLACING FIRST "    " BY ANO-TOTAL
           INSPECT NOM-MOV-W    REPLACING FIRST "                    "
                                          BY    "\CONTROL\ARCHMOV.DAT"

           MOVE NOM-MOV-W TO REG-PLANO2.
           WRITE REG-PLANO2 BEFORE 1.

           OPEN INPUT MOVIMIENTO-DIARIO.

            INITIALIZE MOV-DIARIO.
            MOVE "9999999" TO LLAVE-COMP-MOV.
            MOVE "1I"      TO SECU-MOV.

            MOVE SECUENCIA-MOV TO REG-PLANO2.
            WRITE REG-PLANO2 BEFORE 1.


            READ MOVIMIENTO-DIARIO RECORD KEY IS SECUENCIA-MOV
                  INVALID KEY  CLOSE    MOVIMIENTO-DIARIO
                               OPEN I-O MOVIMIENTO-DIARIO
                               INITIALIZE REFER-MOV
                               MOVE "ULTIMO COMPROBANTE" TO DETALLE-MOV
                               WRITE MOV-DIARIO END-WRITE.


            IF REF1-MOV IS NOT NUMERIC
               MOVE 0 TO REF1-MOV
            END-IF.

            INSPECT REFER-MOV REPLACING ALL " " BY "0".

            MOVE REFER-MOV       TO NUMERO-INT-W
            ADD  1               TO NUMERO-INT-W.

            CLOSE MOVIMIENTO-DIARIO.


       GRABAR-NUMERO.
           MOVE DATO-IP         TO NOM-MOV-W 
           INSPECT NOM-MOV-W    REPLACING FIRST "    " BY ANO-TOTAL
           INSPECT NOM-MOV-W    REPLACING FIRST "                    "
                                          BY    "\CONTROL\ARCHMOV.DAT"

           OPEN I-O MOVIMIENTO-DIARIO.

            INITIALIZE MOV-DIARIO.
            MOVE "9999999" TO LLAVE-COMP-MOV.
            MOVE "1I"      TO SECU-MOV.

            READ MOVIMIENTO-DIARIO RECORD KEY IS SECUENCIA-MOV
                  INVALID KEY 
                    MOVE NUMERO-INT-W TO REFER-MOV
                    MOVE "ULTIMO COMPROBANTE" TO DETALLE-MOV
                    WRITE MOV-DIARIO END-WRITE
                  NOT INVALID KEY
                    MOVE NUMERO-INT-W TO REFER-MOV
                    REWRITE MOV-DIARIO END-REWRITE
             END-READ.

            CLOSE MOVIMIENTO-DIARIO.



       FIN-VALIDAR-SESION.
