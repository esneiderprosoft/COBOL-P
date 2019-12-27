004820*=================================================================
004830* ACUMULAR FACTURA PARA CARTERA
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "..\..\FUENTES\FS-USUNET.CBL".
004920     COPY "..\..\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-NUMERACION LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-NUMER-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-NUM
                  ALTERNATE RECORD KEY LLAVE-ELE-NUM   WITH DUPLICATES
                  ALTERNATE RECORD KEY SUCUR-NUM       WITH DUPLICATES
                  ALTERNATE RECORD KEY NIT-NUM         WITH DUPLICATES
                  ALTERNATE RECORD KEY DESCRIP-NUM     WITH DUPLICATES
                  ALTERNATE RECORD KEY NOMBRE-PAC-NUM  WITH DUPLICATES
                  ALTERNATE RECORD KEY HAB-NUM         WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-ING-NUM   WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-RET-NUM   WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT FACTURA-SERVICIOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-SAL-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-FACT
                  ALTERNATE RECORD KEY NRO-FACT   WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-FACT WITH DUPLICATES
                  ALTERNATE RECORD KEY CTA-FACT   WITH DUPLICATES
                  ALTERNATE RECORD KEY NIT-FACT   WITH DUPLICATES
                  ALTERNATE RECORD KEY ID-HISTORIA-FACT WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-ING-ESTAD WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-SAL-ESTAD WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-ENTR-RX   WITH DUPLICATES				  
                  FILE STATUS IS OTR-STAT.

005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-NUMER19.CBL".
       COPY "..\..\FUENTES\FD-SALUD.CBL".

005190 WORKING-STORAGE SECTION.
005200 COPY "..\..\FUENTES\COBW3.CBL".
005210 COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LINEA-LLEGADA-W               PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W             PIC X(15).
             03 FECHA-LLEGAD-W          PIC X(8).
             03 HORA-LLEGAD-W           PIC X(6).
          02 DIR-CONTAB-LLEGADA-W       PIC X(30).
          02 MES-CONTAB-LLEGADA-W       PIC XX.
          02 OPERADOR-LLEGADA-W         PIC X(4).
          02 LLAVE-LLEGADA-W.
             03 PREFIJO-LLEGADA-W       PIC X.
             03 NUMERO-LLEGADA-W        PIC X(6).
          02 FECHA-NUM.
             03 ANO-NUM                 PIC 99.
             03 MES-NUM                 PIC 99.
             03 DIA-NUM                 PIC 99.
          02 NIT-USU                    PIC 9(10).
          02 SAL-MIN-USU                PIC 9(9).
        

       77 MES-SAL-W                     PIC X(4).
       77 NOM-NUMER-W                   PIC X(70).
       77 NOM-SAL-W                     PIC X(70).
       77 SW9                           PIC 9.
       77 ANO-ALFA                      PIC 9(4).
       77 VLR-W                         PIC S9(11).
       77 SW-PASO                       PIC 9.
       77 SAL-MIN-W                     PIC 9(6)V99.
       77 TOPE-SOAT-W                   PIC 9(11)V99.
       77 PORC-SOAT75-W                 PIC 9(11)V99.
       77 PORC-SOAT80-W                 PIC 9(11)V99.
       77 PORC-SOAT85-W                 PIC 9(11)V99.
       77 PORC-SOAT90-W                 PIC 9(11)V99.
       77 PORC-SOAT95-W                 PIC 9(11)V99.
       77 VLR-FACT-VITUAL-W             PIC 9(11)V99.
       77 VLR-EDIT                      PIC ZZZ,ZZZ,ZZZ.ZZ.

       01 FECHA-LIMI.
          02 ANO-LIMI                   PIC 9(4).
          02 MES-LIMI                   PIC 99.
          02 DIA-LIMI                   PIC 99. 

       01 FECHA-FAC-ED.
          02 LLAVE-MES-ED.
             05 ANO-FAC-ED              PIC 9(4).
             05 MES-FAC-ED              PIC 99.
          02 DIA-FAC-ED                 PIC 99.


       01 DATOS-W.
          02 CARGOS-MES-W.
             05 TABLA-FACT-W  OCCURS 20.
                07 FECHA-FACT-W.
                   09 ANO-FACT-W        PIC 9(4).
                   09 MES-FACT-W        PIC 99.
                07 VLR-FACT-W           PIC S9(11).

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV             PIC XX.
          

006310 LINKAGE SECTION.
006320 COPY "..\..\FUENTES\ISAPICTX.CBL".
006330 PROCEDURE DIVISION WITH stdcall LINKAGE USING ISAPI-CTX-CNT.
006340
006350 DECLARATIVES.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUNET.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USU-W                TO MSJ2-HTML
              MOVE "INV020GA"               TO MSJ3-HTML
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
              MOVE "INV020GA"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-NUMERACION
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-NUMER-W              TO MSJ2-HTML
              MOVE "INV020GA"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON FACTURA-SERVICIOS.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-SAL-W                TO MSJ2-HTML
              MOVE "INV020GA"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
006720  END DECLARATIVES.
006730
006740 INICIAR-IIS.
006750     MOVE LOW-VALUE TO COBW3.
006760     MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
006770     CALL "COBW3_INIT" USING COBW3.
006780
006790 LEER-DATO-HTML.
006800     MOVE "datosh" TO COBW3-SEARCH-DATA.
006810     CALL "COBW3_GET_VALUE" USING COBW3.
006820     MOVE COBW3-GET-DATA    TO LINEA-LLEGADA-W.

           UNSTRING LINEA-LLEGADA-W DELIMITED BY "|"
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, 
                    OPERADOR-LLEGADA-W, LLAVE-LLEGADA-W, FECHA-NUM, NIT-USU, SAL-MIN-USU
           END-UNSTRING.

001855     MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.

000797 INICIAR-SESION.
000806     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.
006840
006900 ABRIR-USUARIO.
006910     INITIALIZE OTR-STAT.
006920     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
006930
006940     OPEN INPUT ARCHIVO-USUNET
006950     EVALUATE OTR-STAT
006960       WHEN "00"  CONTINUE
006970       WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
006980                  INITIALIZE REG-USUNET
006990                  WRITE REG-USUNET
007000                  CLOSE ARCHIVO-USUNET
007010                  MOVE "Validacion de Usuario"       TO MSJ1-HTML
007020                  MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007030                  MOVE "Sc"                          TO MSJ3-HTML
007040                  GO TO ENVIAR2-ERROR
007050       WHEN OTHER GO TO CERRAR-SESION
007060     END-EVALUATE.
007070
007080     INITIALIZE LLAVE-USUNET.

007100
007110 LEER-USUARIO.
007120     READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
007140     CLOSE ARCHIVO-USUNET.
007200     
007210     IF NOMBRE-USUNET = SPACES
007230        MOVE "Validacion de Sesion"        TO MSJ1-HTML
007240        MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007250        MOVE "Sc"                          TO MsJ3-HTML
007260        GO TO ENVIAR2-ERROR
007270     END-IF.
007280 
007350

        ASIGNAR-NOMBRES.
           EVALUATE MES-CONTAB-LLEGADA-W
             WHEN 01 MOVE "\ENE" TO MES-SAL-W 
             WHEN 02 MOVE "\FEB" TO MES-SAL-W 
             WHEN 03 MOVE "\MAR" TO MES-SAL-W 
             WHEN 04 MOVE "\ABR" TO MES-SAL-W 
             WHEN 05 MOVE "\MAY" TO MES-SAL-W 
             WHEN 06 MOVE "\JUN" TO MES-SAL-W 
             WHEN 07 MOVE "\JUL" TO MES-SAL-W 
             WHEN 08 MOVE "\AGT" TO MES-SAL-W 
             WHEN 09 MOVE "\SEP" TO MES-SAL-W 
             WHEN 10 MOVE "\OCT" TO MES-SAL-W 
             WHEN 11 MOVE "\NOV" TO MES-SAL-W 
             WHEN 12 MOVE "\DIC" TO MES-SAL-W 
           END-EVALUATE    

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

001614     INITIALIZE NOM-NUMER-W

           MOVE "\\" TO NOM-NUMER-W

           INSPECT NOM-NUMER-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET
001621     INSPECT NOM-NUMER-W REPLACING FIRST " "
001624                                      BY "\"
007190     INSPECT NOM-NUMER-W REPLACING FIRST "                              "
                                            BY DIR-CONTAB-LLEGADA-W

           MOVE NOM-NUMER-W TO NOM-SAL-W

           INSPECT NOM-NUMER-W REPLACING FIRST "                     "
007300                                      BY "\CONTROL\SC-NUMER.DAT".

           INSPECT NOM-SAL-W REPLACING FIRST "                     "
                                          BY "\CONTROL\SC-FACSA.DAT".

        ABRIR-ARCHIVOS.
           OPEN INPUT ARCHIVO-NUMERACION
                      FACTURA-SERVICIOS.

           IF ANO-NUM > 90
              COMPUTE ANO-ALFA = ANO-NUM + 1900
           ELSE
              COMPUTE ANO-ALFA = ANO-NUM + 2000
           END-IF.

        LEER-FACTURA.
            MOVE LLAVE-LLEGADA-W TO LLAVE-NUM
            READ ARCHIVO-NUMERACION WITH NO LOCK
                   INVALID KEY
                     INITIALIZE REG-NUM
                     MOVE "01" TO SW-INVALID-ENV
                   NOT INVALID KEY
                     MOVE "00" TO SW-INVALID-ENV
            END-READ.

            PERFORM BUSCAR-CARGOS VARYING I FROM 1 BY 1
                                  UNTIL I > 10.

            MOVE LLAVE-LLEGADA-W TO CTA-FACT.

        BUSCAR-FACTURA.
            READ FACTURA-SERVICIOS NEXT WITH NO LOCK AT END
                                   GO TO GRABAR-CARTERA.

            IF CTA-FACT IS NOT EQUAL TO LLAVE-LLEGADA-W
                               GO TO GRABAR-CARTERA.


            IF ANO-FACT > 90
               COMPUTE ANO-FAC-ED = ANO-FACT + 1900
            ELSE
               COMPUTE ANO-FAC-ED = ANO-FACT + 2000
            END-IF.
            MOVE MES-FACT TO MES-FAC-ED.
            MOVE DIA-FACT TO DIA-FAC-ED.
       
            IF (PREFIJO-FACT = "A" OR "B" OR "D" OR "F"
                 OR "G" OR "H" OR "I" OR "J" OR "K" OR "L" OR "M" OR "N"
                 OR "O" OR "Q" OR "R" OR "S" OR "V" OR "W"
                 OR "X" OR "Y" OR "Z")
               IF FECHA-FAC-ED < FECHA-RET-NUM
               OR FECHA-FAC-ED > FECHA-RET-NUM
                  IF MES-RET-NUM IS ZERO
                     MOVE FECHA-ING-NUM TO FECHA-FAC-ED
                  ELSE
                     MOVE FECHA-RET-NUM TO FECHA-FAC-ED
                  END-IF
               END-IF.

            IF (NIT-USU = 900566047)
               CONTINUE
            ELSE
               IF ANO-ALFA IS NOT = ANO-FAC-ED
                  GO TO BUSCAR-FACTURA
               END-IF
            END-IF.
               
            INITIALIZE VLR-W.
            PERFORM SUMAR-TABLA VARYING I FROM 1 BY 1
                                UNTIL I > 30.

            SUBTRACT VALOR-DES-FACT FROM VLR-W.
            ADD      VLR-IVA-FACT   TO VLR-W.

        ACUMULAR-CARTERA.
            MOVE 0 TO SW-BUSCAR.
            PERFORM UBICAR-CARGOS  VARYING I FROM 1 BY 1
                                   UNTIL I > 10 OR SW-BUSCAR > 0.

            IF SW-BUSCAR IS ZERO
               PERFORM ASIGNAR-CARGOS  VARYING I FROM 1 BY 1
                                      UNTIL I > 10 OR SW-BUSCAR > 0.

            GO TO BUSCAR-FACTURA.

        SUMAR-TABLA.
            ADD VLR-FACT  (I) TO VLR-W.

        GRABAR-CARTERA.
            COMPUTE SAL-MIN-W ROUNDED = SAL-MIN-USU / 30.
            COMPUTE TOPE-SOAT-W ROUNDED = SAL-MIN-W * 800.
            COMPUTE PORC-SOAT75-W ROUNDED = TOPE-SOAT-W * 0.75.
            COMPUTE PORC-SOAT80-W ROUNDED = TOPE-SOAT-W * 0.80.
            COMPUTE PORC-SOAT85-W ROUNDED = TOPE-SOAT-W * 0.85.
            COMPUTE PORC-SOAT90-W ROUNDED = TOPE-SOAT-W * 0.90.
            COMPUTE PORC-SOAT95-W ROUNDED = TOPE-SOAT-W * 0.95.
            INITIALIZE VLR-FACT-VITUAL-W.
            PERFORM TOTAL-FACTURA  VARYING I FROM 1 BY 1
                                       UNTIL I > 10.
    
      *      COMPUTE VLR-FACT-VITUAL-W = VLR-FACT-VITUAL-W + VLR-IPSANT-NUM.
       
            IF PREFIJO-FACT = "T"
             MOVE SAL-MIN-W TO VLR-EDIT
             MOVE TOPE-SOAT-W TO VLR-EDIT
             MOVE VLR-FACT-VITUAL-W TO VLR-EDIT
             IF VLR-FACT-VITUAL-W  > PORC-SOAT75-W
             AND VLR-FACT-VITUAL-W  < PORC-SOAT80-W
                   MOVE "8O" TO SW-INVALID-ENV
             END-IF
             IF VLR-FACT-VITUAL-W > PORC-SOAT80-W
             AND VLR-FACT-VITUAL-W  < PORC-SOAT85-W
                   MOVE "8P" TO SW-INVALID-ENV
              END-IF
             IF VLR-FACT-VITUAL-W  > PORC-SOAT85-W
             AND VLR-FACT-VITUAL-W  < PORC-SOAT90-W
                   MOVE "8Q" TO SW-INVALID-ENV
             END-IF
             IF VLR-FACT-VITUAL-W  > PORC-SOAT90-W
             AND VLR-FACT-VITUAL-W  < PORC-SOAT95-W
                   MOVE "8R" TO SW-INVALID-ENV
             END-IF
             IF VLR-FACT-VITUAL-W  > PORC-SOAT95-W
                  MOVE "8S" TO SW-INVALID-ENV
             END-IF
            END-IF.

      * INV020G

            IF   NIT-USU = 800037979
           AND  (PREFIJO-LLEGADA-W = "A" OR "B" OR "D" OR "F"
                OR "G" OR "H" OR "I" OR "J" OR "K" OR "L" OR "M" OR "N"
                OR "O" OR "Q" OR "R" OR "S" OR "V" OR "W"
                OR "X" OR "Y" OR "Z")
           AND  NUMERO-LLEGADA-W < 11202
                GO TO CERRAR-ARCHIVOS
           END-IF.

        TOTAL-FACT.
           MOVE 0 TO SW-BUSCAR.
           PERFORM COMPARAR-CARTERA VARYING I FROM 1 BY 1
                                    UNTIL I > 10.


           IF  (NIT-USU = 892000401 OR  900804411 OR 900870633)
                PERFORM BUSCAR-RETENCION VARYING I FROM 1 BY 1
                                         UNTIL I > 3
           END-IF.

       GRABAR-CARTERA2.
           IF SW-BUSCAR > 0
              CLOSE    ARCHIVO-NUMERACION
              OPEN I-O ARCHIVO-NUMERACION
              REWRITE REG-NUM.

        CERRAR-ARCHIVOS.
            CLOSE ARCHIVO-NUMERACION
                  FACTURA-SERVICIOS.

010700  PAGINA-CONFIG.
010720      MOVE "datosrecibidos"    TO COBW3-CNV-NAME
010730      MOVE DATOS-ENVIO         TO COBW3-CNV-VALUE
010740      CALL "COBW3_SET_CNV" USING COBW3
010750
010760      MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
010770      PERFORM ABRIR-HTML.
007980   
007990 CERRAR-SESION.
008000      CALL "COBW3_FREE" USING COBW3.
008010      MOVE 1 TO PROGRAM-STATUS.
008020      EXIT PROGRAM.

008030
008040 COPY "..\..\FUENTES\SC-WEB.CBL".

        BUSCAR-CARGOS.
           IF  ANO-FACT-NUM (I) IS NOT = ANO-ALFA
           AND VLR-FACT-NUM (I) IS NOT ZERO
               MOVE TABLA-FACT-NUM (I) TO TABLA-FACT-W (I)
               PERFORM VALIDAR-CARGOS VARYING J FROM 1 BY 1
                                      UNTIL J > 10.

        VALIDAR-CARGOS.
           IF MES-FACT-W (I) = 12
              COMPUTE ANO-LIMI = ANO-FACT-W (I) + 1
              MOVE 01 TO MES-LIMI
              MOVE 31 TO DIA-LIMI
           ELSE
              MOVE ANO-FACT-W (I) TO ANO-LIMI
              COMPUTE MES-LIMI = MES-FACT-W (I) + 1
              MOVE 31             TO DIA-LIMI
           END-IF.

           IF FECHA-LIMI < FECHA-ING-NUM
              INITIALIZE TABLA-FACT-W (I).

           IF   I IS NOT = J
           AND  FECHA-FACT-W (J) = FECHA-FACT-W (I)
                INITIALIZE TABLA-FACT-W (I).

        UBICAR-CARGOS.
           IF  ANO-FAC-ED = ANO-ALFA
           AND SW-BUSCAR  = 0
           AND LLAVE-MES-ED = FECHA-FACT-W (I)
               ADD VLR-W   TO VLR-FACT-W (I)
               MOVE 1      TO SW-BUSCAR
           ELSE
              INITIALIZE SW-PASO
              PERFORM VALIDAR-CARGOS2 VARYING J FROM 1 BY 1
                                      UNTIL J > 10

             IF (NIT-USU = 900566047)
             AND SW-BUSCAR  = 0
             AND SW-PASO    = 0
             AND LLAVE-MES-ED = FECHA-FACT-W (I)
                      ADD VLR-W   TO VLR-FACT-W (I)
                      MOVE 1      TO SW-BUSCAR
             END-IF
           END-IF.

       ASIGNAR-CARGOS.
           IF  ANO-FAC-ED = ANO-ALFA
           AND SW-BUSCAR  = 0
           AND MES-FACT-W (I) IS ZERO
               MOVE  LLAVE-MES-ED TO FECHA-FACT-W (I)
               MOVE VLR-W         TO VLR-FACT-W   (I)
               MOVE 1             TO SW-BUSCAR
           ELSE
              INITIALIZE SW-PASO
              PERFORM VALIDAR-CARGOS2 VARYING J FROM 1 BY 1
                                      UNTIL J > 10

             IF (NIT-USU = 900566047)
             AND SW-BUSCAR  = 0
             AND SW-PASO    = 0
             AND MES-FACT-W (I) IS ZERO
                      MOVE  LLAVE-MES-ED TO FECHA-FACT-W (I)
                      MOVE VLR-W         TO VLR-FACT-W   (I)
                      MOVE 1             TO SW-BUSCAR
             END-IF
           END-IF.

       VALIDAR-CARGOS2.
           IF  MES-FACT-W (I) IS ZERO
               CONTINUE
           ELSE
               MOVE 1 TO SW-PASO
           END-IF.

       TOTAL-FACTURA.
           IF  MES-FACT-W (I) IS ZERO
               CONTINUE
           ELSE
               ADD VLR-FACT-W   (I) TO VLR-FACT-VITUAL-W
           END-IF.

        COMPARAR-CARTERA.
           IF TABLA-FACT-W (I) IS NOT = TABLA-FACT-NUM (I)
              MOVE TABLA-FACT-W (I) TO TABLA-FACT-NUM (I)
              MOVE 1 TO SW-BUSCAR.


       BUSCAR-RETENCION.
           IF (LOTE-ABON-NUM (I) = 30 OR 40 OR 50
                                 OR "GA" OR "GP" OR "GT")
           AND SEC-ABON-NUM (I) = "ZZ"
               INITIALIZE TABLA-RBOS-NUM (I)
               MOVE 1 TO SW-BUSCAR
           END-IF. 

005210* COPY "..\..\FUENTES\WS-HCEVO.CBL".
