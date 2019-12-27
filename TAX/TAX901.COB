      *=================================================================
      *CONTABILIZA EGRESOS E INGRESOS DESDE PLANO    
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

           SELECT ARCHIVO-LOG LOCK MODE IS AUTOMATIC
                  ASSIGN  TO NOM-LOG-W
                  ORGANIZATION LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.

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
           02 DATO-PLANO       PIC X(500).

       FD  ARCHIVO-LOG
           LABEL RECORD STANDARD.
       01  REG-LOG.
           02 DATO-LOG      PIC X(500).


       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".

       COPY "S:\NEWCOBOL\TAX\FD-CARRO.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-TERCE.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-MAEST.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-AGENC.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-MOVIM.CBL".

       WORKING-STORAGE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".

       01  SW9                  PIC 9.
       01  SECU-W               PIC 99.
       01  ERROR-STAT           PIC XX.
       01  VALOR-CIERRE-W       PIC S9(12)V99.
       01  CTA-CIERRE-W         PIC X(11).
       01  COSTO-CIERRE-W       PIC X(2).
       01  NIT-CIERRE-W         PIC 9(10).
       01  TIPO-CIERRE-W        PIC 9.
       01  CONV-CIERRE-W        PIC 99.
       01  AGEN-CIERRE-W        PIC 99.

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
           02 NOM-LOG-W         PIC X(60).
           02 NOM-INTERM-W      PIC X(60).
           02 NOM-CARROS-W      PIC X(60).
           02 NOM-TERCE-W       PIC X(60).
           02 NOM-MAE-W         PIC X(60).
           02 NOM-MOV-W         PIC X(60).
           02 NOM-AGEN-W        PIC X(60).

       01  NUMERO-CONT-W        PIC X(11).

       01  DATO-REC-W.
           02 TIPO-REC-W        PIC 9.
           02 CONV-REC-W        PIC 99.
           02 AGEN-REC-W        PIC 99.
           02 FECHA-REC-W       PIC 9(8).
           02 COMPROB-REC-W     PIC 9(5).
           02 TABLA-REC-W. 
              03 TAB-REC-W OCCURS 3.
                 05 CTA-REC-W   PIC X(11).
                 05 VALOR-REC-W PIC S9(12).
                 05 RUT-REC-W   PIC 9(10).
           02 DETALLE-REC-W     PIC X(50).
           02 CCOSTO-REC-W      PIC X(2).
           02 OPER-REC-W        PIC 9(4).
           02 CTA-CAJA-REC-W    PIC X(11).
           02 NIT-REC-W         PIC 9(10).

       01  SECUENCIA-MOV-W.
           02 LOTE-MOV-W.
              03 LOTE1-MOV-W    PIC X.
              03 LOTE2-MOV-W    PIC X.
           02 COMPROB-MOV-W.
              03 COMP1-MOV-W    PIC XX.
              03 COMP2-MOV-W    PIC 9(3).
           02 SECU-MOV-W.
              03 SECU1-MOV-W    PIC X.
              03 SECU2-MOV-W    PIC X.

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
              MOVE OTR-STAT         TO ERROR-STAT
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
              MOVE OTR-STAT         TO ERROR-STAT
              MOVE 1 TO TIPO-ENV-W
              MOVE NOM-SESION-W     TO VALOR-ENV-W 
              GO TO CERRAR-SESION
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MOVIMIENTO-DIARIO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"          
              CONTINUE 
           ELSE
              MOVE OTR-STAT        TO ERROR-STAT
              MOVE 1 TO TIPO-ENV-W
              MOVE NOM-MOV-W       TO VALOR-ENV-W
              MOVE OTR-STAT  TO REG-LOG
              WRITE REG-LOG  BEFORE 1
              MOVE NOM-MOV-W TO REG-LOG
              WRITE REG-LOG  BEFORE 1
              GO TO CERRAR-SESION
           END-IF.
        
        END DECLARATIVES.

       ABRIR-LOG.
           MOVE "C:\PROSOFT\LOG.TXT" TO NOM-LOG-W.
           OPEN OUTPUT ARCHIVO-LOG.
       
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


           MOVE NOM-PLANO-W TO REG-LOG    
           WRITE REG-LOG    BEFORE 1.

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
      
       REVISAR-NUMERO.
           INITIALIZE SECU-W SW9.

       LEER-PLANO.
           INITIALIZE REG-PLANO.

           READ ARCHIVO-PLANO NEXT AT END
                              CLOSE ARCHIVO-PLANO
                              GO TO FIN-PLANO.

           UNSTRING REG-PLANO DELIMITED BY ","
             INTO TIPO-REC-W,   CONV-REC-W,     AGEN-REC-W, FECHA-REC-W, COMPROB-REC-W,
                  CTA-REC-W(1), VALOR-REC-W(1), RUT-REC-W(1),
                  CTA-REC-W(2), VALOR-REC-W(2), RUT-REC-W(2),
                  CTA-REC-W(3), VALOR-REC-W(3), RUT-REC-W(3),
                  DETALLE-REC-W, CCOSTO-REC-W, OPER-REC-W, CTA-CAJA-REC-W, NIT-REC-W,
           END-UNSTRING.

           IF COMPROB-REC-W > 0
              CONTINUE
           ELSE
              GO TO LEER-PLANO
           END-IF.

       MONTAR-COMPROBANTE.
           MOVE FECHA-REC-W TO FECHA-W

           IF SW9 = 0
              MOVE 1 TO SW9
              PERFORM ABRIR-MOVIMIENTO
              MOVE 0 TO SECU-W VALOR-CIERRE-W
              MOVE CTA-CAJA-REC-W TO CTA-CIERRE-W
              MOVE NIT-REC-W      TO NIT-CIERRE-W
              MOVE CCOSTO-REC-W   TO COSTO-CIERRE-W
              MOVE TIPO-REC-W     TO TIPO-CIERRE-W
              MOVE CONV-REC-W     TO CONV-CIERRE-W
              MOVE AGEN-REC-W     TO AGEN-CIERRE-W
           END-IF.

           WRITE REG-LOG FROM REG-PLANO BEFORE 1

           EVALUATE TIPO-REC-W 
             WHEN 1 MOVE "2R" TO LOTE-MOV-W 
             WHEN 2 MOVE "2C" TO LOTE-MOV-W 
             WHEN OTHER GO TO LEER-PLANO
           END-EVALUATE.

           MOVE AGEN-REC-W    TO COMP1-MOV-W
           MOVE COMPROB-REC-W TO COMP2-MOV-W
           MOVE SECU-W        TO SECU-MOV-W.

           INITIALIZE MOV-DIARIO
           MOVE SECUENCIA-MOV-W TO SECUENCIA-MOV.
           PERFORM GRABAR-DATOS VARYING I FROM 1 BY 1 UNTIL I > 3.

           GO TO LEER-PLANO.


       FIN-PLANO.
           WRITE REG-LOG FROM TIPO-CIERRE-W BEFORE 1

           IF  VALOR-CIERRE-W < 0
           AND TIPO-CIERRE-W = 2
           AND CONV-CIERRE-W = 1
               MOVE CTA-CIERRE-W    TO CUENTA-MOV
               MOVE NIT-CIERRE-W    TO ID-MOV

               EVALUATE AGEN-CIERRE-W
                 WHEN 01 MOVE 9901 TO ID-MOV
                 WHEN 32 MOVE 9932 TO ID-MOV
               END-EVALUATE

               MOVE 9               TO TIP-DOC-MOV
               MOVE AGEN-REC-W      TO NRO-DOC-MOV
               MOVE FECHA2-W        TO FECHA-MOV
               MOVE COSTO-CIERRE-W  TO COSTO-MOV
               MOVE AGEN-REC-W      TO REFER-MOV
               MOVE VALOR-CIERRE-W  TO VALOR-MOV 
               MOVE DETALLE-REC-W   TO DETALLE-MOV
               MOVE FECHA2-W        TO FECHA-ELAB-MOV
               ACCEPT HORA-ELAB-MOV FROM TIME
               MOVE OPER-REC-W      TO OPER-MOV
               ADD 1 TO SECU-W
               MOVE SECU-W          TO SECU-MOV
               WRITE MOV-DIARIO END-WRITE
            END-IF.


           MOVE 0 TO TIPO-ENV-W.
           INITIALIZE SECU-MOV-W.
           MOVE SECUENCIA-MOV-W  TO VALOR-ENV-W.


       CERRAR-ARCHIVOS.
           CLOSE MOVIMIENTO-DIARIO
                 ARCHIVO-LOG.

       
       CERRAR-SESION.
           IF TIPO-ENV-W > 0
              INSPECT VALOR-ENV-W REPLACING FIRST " "
                                            BY    "|"
              INSPECT VALOR-ENV-W REPLACING FIRST "  "
                                            BY   ERROR-STAT
           END-IF.

           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE DATO-ENV-W       TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3.
      
           MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
           PERFORM ABRIR-HTML.

           CALL "COBW3_END_SESSION" using COBW3.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.

       COPY "P:\PROG\PROYECT\FUENTES\SC-WEB.CBL".

       GRABAR-DATOS.
           IF VALOR-REC-W (I) > 0
              MOVE CTA-REC-W (I)   TO CUENTA-MOV
              MOVE RUT-REC-W (I)   TO ID-MOV

              IF  TIPO-REC-W = 2
              AND CONV-REC-W = 1
                 EVALUATE AGEN-REC-W
                  WHEN 01 MOVE 9901 TO ID-MOV
                  WHEN 32 MOVE 9932 TO ID-MOV
                 END-EVALUATE
              END-IF

              MOVE 0               TO TIP-DOC-MOV
              MOVE AGEN-REC-W      TO NRO-DOC-MOV
              MOVE FECHA2-W        TO FECHA-MOV
              MOVE CCOSTO-REC-W    TO COSTO-MOV

              EVALUATE CONV-REC-W
               WHEN  2 MOVE "00"   TO COSTO-MOV
               WHEN  4 MOVE "03"   TO COSTO-MOV
              END-EVALUATE

              MOVE AGEN-REC-W      TO REFER-MOV
              MOVE VALOR-REC-W (I) TO VALOR-MOV 

              MOVE DETALLE-REC-W   TO DETALLE-MOV
              MOVE FECHA2-W        TO FECHA-ELAB-MOV
              ACCEPT HORA-ELAB-MOV FROM TIME
              MOVE OPER-REC-W      TO OPER-MOV
              ADD 1 TO SECU-W
              MOVE SECU-W          TO SECU-MOV
              SUBTRACT VALOR-MOV   FROM VALOR-CIERRE-W

              IF I = 1               
                 IF TIPO-CIERRE-W = 2
                    EVALUATE CONV-CIERRE-W 
                     WHEN 1 CONTINUE
                     WHEN 2 COMPUTE VALOR-MOV = VALOR-MOV * -1
                     WHEN 3 CONTINUE
                     WHEN 4 CONTINUE
                    END-EVALUATE
                 END-IF
              ELSE
                 IF TIPO-CIERRE-W = 1
                    COMPUTE VALOR-MOV = VALOR-MOV * -1
                 ELSE
                    EVALUATE CONV-CIERRE-W 
                     WHEN 1 CONTINUE
                     WHEN 2 CONTINUE
                     WHEN 3 COMPUTE VALOR-MOV = VALOR-MOV * -1
                     WHEN 4 COMPUTE VALOR-MOV = VALOR-MOV * -1
                    END-EVALUATE
                  END-IF                  
              END-IF

              WRITE MOV-DIARIO END-WRITE
           END-IF.

       ABRIR-MOVIMIENTO.
           MOVE DATO-IP         TO NOM-MOV-W 
           INSPECT NOM-MOV-W    REPLACING FIRST "                     "
                                          BY    "\AAAA\MMM\ARCHMOV.DAT"
           INSPECT NOM-MOV-W    REPLACING FIRST "AAAA" BY ANO-TOTAL

           EVALUATE MES-W
             WHEN 01 INSPECT NOM-MOV-W REPLACING FIRST "MMM" BY "ENE"
             WHEN 02 INSPECT NOM-MOV-W REPLACING FIRST "MMM" BY "FEB"
             WHEN 03 INSPECT NOM-MOV-W REPLACING FIRST "MMM" BY "MAR"
             WHEN 04 INSPECT NOM-MOV-W REPLACING FIRST "MMM" BY "ABR"
             WHEN 05 INSPECT NOM-MOV-W REPLACING FIRST "MMM" BY "MAY"
             WHEN 06 INSPECT NOM-MOV-W REPLACING FIRST "MMM" BY "JUN"
             WHEN 07 INSPECT NOM-MOV-W REPLACING FIRST "MMM" BY "JUL"
             WHEN 08 INSPECT NOM-MOV-W REPLACING FIRST "MMM" BY "AGT"
             WHEN 09 INSPECT NOM-MOV-W REPLACING FIRST "MMM" BY "SEP"
             WHEN 10 INSPECT NOM-MOV-W REPLACING FIRST "MMM" BY "OCT"
             WHEN 11 INSPECT NOM-MOV-W REPLACING FIRST "MMM" BY "NOV"
             WHEN 12 INSPECT NOM-MOV-W REPLACING FIRST "MMM" BY "DIC"    
           END-EVALUATE.

           OPEN I-O MOVIMIENTO-DIARIO.



       FIN-VALIDAR-SESION.
