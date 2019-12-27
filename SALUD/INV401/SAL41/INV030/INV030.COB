004820*=================================================================
004830*   SALUD - MONTA LECTURAS DE RX EN ARCHIVO WEB
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "..\..\FUENTES\FS-USUNET.CBL".
004920     COPY "..\..\FUENTES\FS-SESION.CBL".

            SELECT  ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                    ASSIGN TO NOM-USUAR-W
                    ORGANIZATION IS INDEXED
                    ACCESS MODE IS DYNAMIC;
                    RECORD KEY IS NOMBRE-USU
                    ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                    ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                    ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                    FILE STATUS IS OTR-STAT.


            SELECT  FACTURA-SERVICIOS LOCK MODE IS AUTOMATIC
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

            SELECT  MOVIMIENTO-INVENT LOCK MODE IS AUTOMATIC
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
                   ASSIGN NOM-SALDO-W
      *"SC-SALDO.DAT"
                   ORGANIZATION INDEXED;
                   ACCESS MODE  DYNAMIC;
                   RECORD KEY LLAVE-SAL
                   ALTERNATE RECORD KEY COD-ART-SAL  WITH DUPLICATES
                   ALTERNATE RECORD KEY COD-LOTE-SAL WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.

            SELECT REGCONT LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-FECHA-W
                  FILE STATUS OTR-STAT.
				  
005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-SALUD.CBL".
       COPY "..\..\FUENTES\FD-ARTIC.CBL".
       COPY "..\..\FUENTES\FD-INVEN.CBL".
       COPY "..\..\FUENTES\FD-SALDO.CBL".
       COPY "..\..\FUENTES\FD-FECHA.CBL".

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

       01 LLAVE-FACT-W.
          03 SUC-FACT-W                 PIC XX.
          03 CL-FACT-W                  PIC 9.
          03 NRO-FACT-W.
             05 NRO-FACT1-W.
                07 NROA-FACT1-W         PIC 9.
                07 NROB-FACT1-W         PIC 9.
             05 NRO-FACT2-W             PIC 9(4).
       01 SW-RECAL                      PIC 9.


       77 NOM-SAL-W                     PIC X(70).
       77 NOM-USUAR-W                   PIC X(70).
       77 NOM-INVENT-W                  PIC X(70).
       77 NOM-FECHA-W                   PIC X(70).
       77 NOM-ART-W                     PIC X(70).
       77 NOM-SALDO-W                   PIC X(70).
       77 MES-SAL-W                     PIC X(4).
       77 ALMACEN-W                     PIC X(5).
       77 ALM-SIN-W                     PIC X(3).
       77 SECU-W                        PIC 99.
       77 SW-BORRAR                     PIC 9.
       77 TRANSAC-W                     PIC XX.
       01 COMPROB-X.
          02 COMPROB1-X                 PIC X.
          02 COMPROB2-X                 PIC 9(4).
       77 LLAVE-W                       PIC X(9).
       77 SW-EXT                        PIC 9.
       77 DIA-CORTE-W                   PIC 99.
       77 NOM-LOG-W                     PIC X(50).



       01 VARIABLES.
          02 SECU-TEM                   PIC 9(3).
          02 VLR-COSTO-W                PIC S9(12)V99.
          02 VALOR-BRUTO                PIC S9(12).
          02 FACTOR-DCTO                PIC 99V9(4).
          02 VALOR-ICAV                 PIC S9(12)V99.
          02 SW-INICIO                  PIC 9.

       01 LINEA-L3.
          02 LLAVE-SDO-L3               PIC X(30).
          02 FILLER                     PIC X VALUE ",".
          02 DIA-L3                     PIC 99.
          02 FILLER                     PIC X VALUE ",".
          02 AC-ENT-VLR-L3              PIC 9(12).
          02 FILLER                     PIC X VALUE ",".
          02 AC-ENT-CANT-L3             PIC 9(12).
          02 FILLER                     PIC X VALUE ",".
          02 AC-SAL-VLR-L3              PIC 9(12).
          02 FILLER                     PIC X VALUE ",".
          02 AC-SAL-CANT-L3             PIC 9(12).
          02 FILLER                     PIC X VALUE ",".
          02 SDO-ACT-CANT-L3            PIC 9(12).
          02 FILLER                     PIC X VALUE ",".
          02 SDO-ACT-VLR-L3             PIC 9(12).
          02 FILLER                     PIC X VALUE ",".


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
              MOVE "INV030"                 TO MSJ3-HTML
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
              MOVE "INV030"                 TO MSJ3-HTML
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
              MOVE "INV030"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MOVIMIENTO-INVENT.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-INVENT-W             TO MSJ2-HTML
              MOVE "INV030"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF. 
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MAESTRO-ARTICULOS.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-ART-W                TO MSJ2-HTML
              MOVE "INV030"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.           
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SALDOS.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-SALDO-W              TO MSJ2-HTML
              MOVE "INV030"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USUAR-W              TO MSJ2-HTML
              MOVE "INV030"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF. 
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON REGCONT.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-FECHA-W              TO MSJ2-HTML
              MOVE "INV030"                 TO MSJ3-HTML
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
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W,
                    LLAVE-FACT-W, SW-RECAL
           END-UNSTRING.

001855     MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.

000797 INICIAR-SESION.
000806     MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.
006840
006900 ABRIR-USUARIO.
006910     INITIALIZE OTR-STAT.
006920     MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
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
001614      EVALUATE MES-CONTAB-LLEGADA-W
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
            
            INITIALIZE NOM-SAL-W
                       NOM-INVENT-W
                       NOM-ART-W
                       NOM-SALDO-W
					   NOM-USUAR-W

            MOVE "\\" TO NOM-SAL-W
                         NOM-INVENT-W
                         NOM-ART-W
                         NOM-SALDO-W
						 NOM-USUAR-W

            INSPECT NOM-SAL-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET                              
001621      INSPECT NOM-SAL-W REPLACING FIRST " "
001624                                    BY "\"
007190      INSPECT NOM-SAL-W REPLACING FIRST "                              "
                                           BY DIR-CONTAB-LLEGADA-W
            INSPECT NOM-SAL-W REPLACING FIRST "                     "
007300                                     BY "\CONTROL\SC-FACSA.DAT".
            MOVE NOM-SAL-W TO NOM-INVENT-W NOM-ART-W NOM-SALDO-W NOM-USUAR-W
            INSPECT NOM-SALDO-W REPLACING FIRST "SC-FACSA.DAT"
                                             BY "SC-SALDO.DAT".
            INSPECT NOM-INVENT-W REPLACING FIRST "SC-FACSA.DAT "
                                              BY "SC-MOVINV.DAT".
            INSPECT NOM-ART-W REPLACING FIRST "SC-FACSA.DAT  "
                                           BY "SC-MAESART.DAT".
            INSPECT NOM-USUAR-W REPLACING FIRST "SC-FACSA.DAT  "
                                             BY "SC-ARCHUSU.DAT".

            IF PREFIJO-USU = "  "
               MOVE "00"  TO PREFIJO-USU.

            IF (PUC-USU = 4 OR 6)
                MOVE "DR001" TO ALMACEN-W
            ELSE
              MOVE "ALM01" TO ALMACEN-W.

	        OPEN INPUT FACTURA-SERVICIOS
                       MAESTRO-ARTICULOS
                 I-O   MOVIMIENTO-INVENT.


       LEER-FACTURA.
            MOVE LLAVE-FACT-W TO LLAVE-FACT.
            READ FACTURA-SERVICIOS WITH NO LOCK
			        INVALID KEY
					    MOVE "01"  TO SW-INVALID-ENV
                        GO TO CERRAR-ARCHIVOS
			END-READ.

            IF ANO-FACT IS NOT EQUAL TO ANO-ALFA
               MOVE "01" TO SW-INVALID-ENV
               GO TO CERRAR-ARCHIVOS
            END-IF.
			
            MOVE 0 TO SW-BORRAR.
            IF COSTO-FACT = SPACES
               MOVE "00" TO COSTO-FACT.

            IF REFACTURA-FACT > 0
               GO TO CERRAR-ARCHIVOS
            END-IF.

       ENCABEZAR-BORRAR.
            ADD 1 TO SW-BORRAR.
            EVALUATE SW-BORRAR
            WHEN 1 MOVE "12" TO COD-TRANS-INV
            WHEN 2 IF CL-FACT > 0
                      MOVE "2I"  TO COD-TRANS-INV
                   ELSE
                      IF SUC-FACT IS NOT = PREFIJO-USU
                      OR NIT-USU = 800162035
                         MOVE SUC-FACT TO COD-TRANS-INV
                         MOVE 2        TO COD1-TRANS-INV
                         IF NIT-USU = 830512772 OR 845000038
                         EVALUATE  SUC-FACT
                         WHEN 01 MOVE 20   TO COD-TRANS-INV
                         WHEN 02 MOVE 22   TO COD-TRANS-INV
                         WHEN 03 MOVE 23   TO COD-TRANS-INV
                         WHEN 04 MOVE 24   TO COD-TRANS-INV
                         WHEN 05 MOVE 25   TO COD-TRANS-INV
                         WHEN 06 MOVE 26   TO COD-TRANS-INV
                         WHEN 07 MOVE 27   TO COD-TRANS-INV
                         WHEN 08 MOVE 28   TO COD-TRANS-INV
                         WHEN 09 MOVE "2U"   TO COD-TRANS-INV
                         WHEN 10 MOVE "2a" TO COD-TRANS-INV
                         WHEN 11 MOVE "2b" TO COD-TRANS-INV
                         WHEN 12 MOVE "2c" TO COD-TRANS-INV
                         WHEN 13 MOVE "2d" TO COD-TRANS-INV
                         WHEN 14 MOVE "2e" TO COD-TRANS-INV
                         WHEN 15 MOVE "2f" TO COD-TRANS-INV
                         WHEN 16 MOVE "2g" TO COD-TRANS-INV
                         WHEN 17 MOVE "2h" TO COD-TRANS-INV
                         WHEN 18 MOVE "2i" TO COD-TRANS-INV
                         WHEN 19 MOVE "2j" TO COD-TRANS-INV
                         WHEN 20 MOVE "2k" TO COD-TRANS-INV
                         WHEN 21 MOVE "2l" TO COD-TRANS-INV
                         WHEN 22 MOVE "2m" TO COD-TRANS-INV
                         WHEN 23 MOVE "2n" TO COD-TRANS-INV
                         WHEN 24 MOVE "2o" TO COD-TRANS-INV
                         WHEN 25 MOVE "2p" TO COD-TRANS-INV
                         WHEN 26 MOVE "2r" TO COD-TRANS-INV
                         WHEN 27 MOVE "2s" TO COD-TRANS-INV
                         WHEN 28 MOVE "2t" TO COD-TRANS-INV
                         WHEN 29 MOVE "2u" TO COD-TRANS-INV
                         WHEN 30 MOVE "2v" TO COD-TRANS-INV
                         WHEN 31 MOVE "2w" TO COD-TRANS-INV
                         WHEN 32 MOVE "2x" TO COD-TRANS-INV
                         WHEN 33 MOVE "2y" TO COD-TRANS-INV
                         WHEN 34 MOVE "2z" TO COD-TRANS-INV
                         WHEN 35 MOVE "2Z" TO COD-TRANS-INV
                         WHEN 36 MOVE "2Y" TO COD-TRANS-INV
                         WHEN 37 MOVE "2X" TO COD-TRANS-INV
                         WHEN 38 MOVE "2W" TO COD-TRANS-INV
                         WHEN 39 MOVE "2Q" TO COD-TRANS-INV
                         WHEN 40 MOVE "2O" TO COD-TRANS-INV
                         WHEN 41 MOVE "2T" TO COD-TRANS-INV
                         WHEN 42 MOVE "2N" TO COD-TRANS-INV
                         WHEN 43 MOVE "2M" TO COD-TRANS-INV
                         WHEN 44 MOVE "2L" TO COD-TRANS-INV
                         WHEN 45 MOVE "2K" TO COD-TRANS-INV
                         WHEN 46 MOVE "2J" TO COD-TRANS-INV
                         WHEN 47 MOVE "2I" TO COD-TRANS-INV
                         WHEN 48 MOVE "2H" TO COD-TRANS-INV
                         WHEN 49 MOVE "2G" TO COD-TRANS-INV
                         WHEN 50 MOVE "2F" TO COD-TRANS-INV
                         WHEN OTHER MOVE "20" TO COD-TRANS-INV
                         END-EVALUATE
                         END-IF
                         IF COD-TRANS-INV = 29
                            MOVE 20 TO COD-TRANS-INV
                         END-IF
                         IF COD-TRANS-INV = 21
                            MOVE 20 TO COD-TRANS-INV
                         END-IF
                      ELSE
                         MOVE "20" TO COD-TRANS-INV
                      END-IF
                   END-IF
            WHEN OTHER GO TO FIN-BORRAR
            END-EVALUATE.
            PERFORM EDITAR-1ER-DIGITO.

            MOVE NRO-FACT2-W      TO COMPROB2-X.
            MOVE COMPROB-X        TO COMPROB-INV.
            MOVE LLAVE-SEC-INV    TO LLAVE-W.

       BUSCAR-MOVIMIENTO.
            START MOVIMIENTO-INVENT KEY = LLAVE-SEC-INV
                    INVALID KEY     
				        GO TO ENCABEZAR-BORRAR.

       LEER-INVENTARIOS.
            READ MOVIMIENTO-INVENT NEXT WITH NO LOCK AT END 
			                       GO TO ENCABEZAR-BORRAR.
								   
            IF LLAVE-SEC-INV IS NOT EQUAL TO LLAVE-W
               GO TO ENCABEZAR-BORRAR
            END-IF.

            DELETE MOVIMIENTO-INVENT.
            PERFORM ACTUALIZAR-BORRAR.

            GO TO LEER-INVENTARIOS.

       FIN-BORRAR.	         
             INITIALIZE REG-INVENT.
             MOVE LLAVE-W TO LLAVE-SEC-INV.

             MOVE NRO-FACT1-W  TO INI-FACT-INV.


             IF ANO-FACT > 90
                COMPUTE ANO-INV = ANO-FACT + 1900
             ELSE
                COMPUTE ANO-INV = ANO-FACT + 2000.

             MOVE MES-FACT          TO MES-INV.
             MOVE DIA-FACT          TO DIA-INV.

             MOVE "FACT."           TO DOCUM-INV.
             MOVE NIT-FACT          TO NIT-INV.
             MOVE COSTO-FACT        TO COSTO-INV.
             MOVE CTA-FACT          TO DETALLE-INV.
             MOVE ID-HISTORIA-FACT  TO OBSERV-INV.
             MOVE MED-OTR-FACT      TO NIT-RECIBE-INV.


       REVISAR-CODIGO.
            PERFORM REGISTRO-MOVIMIENTO	VARYING I FROM 1 BY 1
                                        UNTIL I > 140.
            GO TO CERRAR-ARCHIVOS.

       REGISTRO-MOVIMIENTO.
             IF ALM-FACT (I) = SPACES
                MOVE ALMACEN-W    TO ALMACEN-INV
             ELSE
                MOVE ALM-FACT (I) TO ALMACEN-INV
             END-IF.

             MOVE ALMACEN-INV  TO ALM-SIN-W.
             IF ALM-SIN-W = "SIN"
                MOVE "SIN99" TO ALMACEN-INV.


            IF  (NIT-USU = 892000401 OR 900648993)
            AND (ALMACEN-INV = "DR099" OR "CR001"
                            OR "UNI01" OR "ALM05")
                 MOVE "SIN99" TO ALMACEN-INV
            END-IF.

      *       MOVE HORA-ELAB-FACT TO HORA-OBSERV-INV.
             MOVE " FAC"     TO DETALLE2-INV.
             MOVE NRO-FACT-W TO DETALLE1-INV.


             IF  MACRO-FACT = 1
                 IF GRUPO-FACT (I) = "XM" OR "XP" OR "XN" OR "MQ" OR "PO" OR "NP"
                    CONTINUE
                 ELSE
                    MOVE 0 TO CANT-FACT (I)
                 END-IF
             END-IF

             IF  CANT-FACT (I) IS NOT ZERO
             AND ALMACEN-INV IS NOT = "SIN99"
                 PERFORM DESCARGAR-INVENTARIO.


       DESCARGAR-INVENTARIO.
             IF NRO-FACT = 404612 OR 404941 OR 405293 OR 405462
      *          DISPLAY  CANT-FACT (I)  LINE 29 POSITION 02
      *          DISPLAY  NRO-FACT  LINE 29 POSITION 15
      *          ACCEPT OPCION-1
                GO TO CERRAR-ARCHIVOS
             END-IF
             IF CANT-FACT (I) > 0
                IF SUC-FACT IS NOT = PREFIJO-USU
                OR NIT-USU = 800162035
                   MOVE SUC-FACT TO COD-TRANS-INV
                   MOVE 2        TO COD1-TRANS-INV
                   IF NIT-USU = 830512772 OR 845000038
                   EVALUATE  SUC-FACT
                   WHEN 01 MOVE 20   TO COD-TRANS-INV
                   WHEN 02 MOVE 22   TO COD-TRANS-INV
                   WHEN 03 MOVE 23   TO COD-TRANS-INV
                   WHEN 04 MOVE 24   TO COD-TRANS-INV
                   WHEN 05 MOVE 25   TO COD-TRANS-INV
                   WHEN 06 MOVE 26   TO COD-TRANS-INV
                   WHEN 07 MOVE 27   TO COD-TRANS-INV
                   WHEN 08 MOVE 28   TO COD-TRANS-INV
                   WHEN 09 MOVE "2U"   TO COD-TRANS-INV
                   WHEN 10 MOVE "2a" TO COD-TRANS-INV
                   WHEN 11 MOVE "2b" TO COD-TRANS-INV
                   WHEN 12 MOVE "2c" TO COD-TRANS-INV
                   WHEN 13 MOVE "2d" TO COD-TRANS-INV
                   WHEN 14 MOVE "2e" TO COD-TRANS-INV
                   WHEN 15 MOVE "2f" TO COD-TRANS-INV
                   WHEN 16 MOVE "2g" TO COD-TRANS-INV
                   WHEN 17 MOVE "2h" TO COD-TRANS-INV
                   WHEN 18 MOVE "2i" TO COD-TRANS-INV
                   WHEN 19 MOVE "2j" TO COD-TRANS-INV
                   WHEN 20 MOVE "2k" TO COD-TRANS-INV
                   WHEN 21 MOVE "2l" TO COD-TRANS-INV
                   WHEN 22 MOVE "2m" TO COD-TRANS-INV
                   WHEN 23 MOVE "2n" TO COD-TRANS-INV
                   WHEN 24 MOVE "2o" TO COD-TRANS-INV
                   WHEN 25 MOVE "2p" TO COD-TRANS-INV
                   WHEN 26 MOVE "2r" TO COD-TRANS-INV
                   WHEN 27 MOVE "2s" TO COD-TRANS-INV
                   WHEN 28 MOVE "2t" TO COD-TRANS-INV
                   WHEN 29 MOVE "2u" TO COD-TRANS-INV
                   WHEN 30 MOVE "2v" TO COD-TRANS-INV
                   WHEN 31 MOVE "2w" TO COD-TRANS-INV
                   WHEN 32 MOVE "2x" TO COD-TRANS-INV
                   WHEN 33 MOVE "2y" TO COD-TRANS-INV
                   WHEN 34 MOVE "2z" TO COD-TRANS-INV
                   WHEN 35 MOVE "2Z" TO COD-TRANS-INV
                   WHEN 36 MOVE "2Y" TO COD-TRANS-INV
                   WHEN 37 MOVE "2X" TO COD-TRANS-INV
                   WHEN 38 MOVE "2W" TO COD-TRANS-INV
                   WHEN 39 MOVE "2Q" TO COD-TRANS-INV
                   WHEN 40 MOVE "2O" TO COD-TRANS-INV
                   WHEN 41 MOVE "2T" TO COD-TRANS-INV
                   WHEN 42 MOVE "2N" TO COD-TRANS-INV
                   WHEN 43 MOVE "2M" TO COD-TRANS-INV
                   WHEN 44 MOVE "2L" TO COD-TRANS-INV
                   WHEN 45 MOVE "2K" TO COD-TRANS-INV
                   WHEN 46 MOVE "2J" TO COD-TRANS-INV
                   WHEN 47 MOVE "2I" TO COD-TRANS-INV
                   WHEN 48 MOVE "2H" TO COD-TRANS-INV
                   WHEN 49 MOVE "2G" TO COD-TRANS-INV
                   WHEN 50 MOVE "2F" TO COD-TRANS-INV
                   WHEN OTHER MOVE "20" TO COD-TRANS-INV
                   END-EVALUATE
                   END-IF
                   IF COD-TRANS-INV = 29
                      MOVE 20 TO COD-TRANS-INV
                   END-IF
                   IF COD-TRANS-INV = 21
                      MOVE 20 TO COD-TRANS-INV
                   END-IF
                ELSE
                   MOVE "20"     TO COD-TRANS-INV
                END-IF
             ELSE
                MOVE "12"        TO COD-TRANS-INV
             END-IF.

             MOVE 0                   TO TIPO-ART-INV.
             IF GRUPO1-FACT (I) = "X"
                EVALUATE GRUPO2-FACT (I)
                WHEN "M" MOVE "MQ"             TO GRUPO-ART-INV
                         MOVE COD-ART-FACT (I) TO NUMERO-ART-INV
                WHEN "N" MOVE "NP"             TO GRUPO-ART-INV
                         MOVE COD-ART-FACT (I) TO NUMERO-ART-INV
                WHEN "P" MOVE ART-FACT (I) TO COD-BARRAS-ART
                         READ MAESTRO-ARTICULOS KEY IS COD-BARRAS-ART
                              INVALID KEY
                                MOVE 99               TO GRUPO-ART-INV
                                MOVE COD-ART-FACT (I) TO NUMERO-ART-INV
                              NOT INVALID KEY
                                MOVE COD-ART TO COD-ART-INV
                         END-READ
                END-EVALUATE
             ELSE
                MOVE GRUPO-FACT     (I)   TO GRUPO-ART-INV
                MOVE COD-ART-FACT   (I)   TO NUMERO-ART-INV
                MOVE CLASE-ART-FACT (I)   TO CLASE-INV
                IF LOTE-FARM-USU = "N"
                   INITIALIZE COD-LOTE-INV
                ELSE
                   MOVE COD-LOTE-FACT (I) TO COD-LOTE-INV
                END-IF
             END-IF.

             IF CANT-FACT (I) > 0
                MOVE CANT-FACT  (I)  TO CANT-INV
             ELSE
                COMPUTE CANT-INV = CANT-FACT (I) * -1.

             IF VLR-FACT        (I) > 0
                MOVE VLR-FACT   (I)  TO VLR-VEN-INV
             ELSE
                COMPUTE VLR-VEN-INV = VLR-FACT (I) * -1.

             IF OPER-CORREC-FACT = SPACES
                MOVE OPER-ELAB-FACT      TO OPER-ELAB-INV
                MOVE FECHA-ELAB-FACT     TO FECHA-ELAB-INV
                MOVE HORA-ATEN-ESTAD     TO HORA-ELAB-INV
             ELSE
                MOVE OPER-CORREC-FACT    TO OPER-ELAB-INV
                MOVE FECHA-CORREC-FACT   TO FECHA-ELAB-INV.


             IF  GRP1-INV = 9 OR GRUPO-ART-INV = ZEROS OR GRUPO-ART-INV = SPACES OR NUMERO-ART-INV = SPACES
                 PERFORM GRABAR-MOVIMIENTO
             ELSE
                 PERFORM GRABAR-MOVIMIENTO
             END-IF.


       GRABAR-MOVIMIENTO.
            MOVE COD-ART-INV TO COD-ART.
            READ MAESTRO-ARTICULOS INVALID KEY
                PERFORM GRABAR-ARTICULO.
            OPEN I-O ARCHIVO-SALDOS.
            INITIALIZE REG-SALDOS.
            MOVE LLAVE-ART-INV TO LLAVE-ART-SAL.
            MOVE COD-LOTE-INV  TO COD-LOTE-SAL.

            IF COD-TRANS-INV < 20
                PERFORM ACTUALIZAR-ENTRADA
            ELSE
                PERFORM ACTUALIZAR-SALIDA.
				
            CLOSE ARCHIVO-SALDOS.
			 
			IF COSTO-ART-USU = "S"
			    MOVE CCOSTO-ART TO COSTO-INV
			END-IF

            MOVE  I          TO SECU-W.
            MOVE  SECU-W     TO SECU-INV.
            READ  MOVIMIENTO-INVENT RECORD KEY IS SECUENCIA-INV
                  INVALID KEY   
                    WRITE REG-INVENT
                    END-WRITE
                  NOT INVALID KEY 
                    REWRITE REG-INVENT
            END-READ.
			
			IF  ALM1-INV  = "CN"
                MOVE "TP" TO ALM1-INV
                PERFORM ACTUALIZAR-TEMPORAL
            END-IF.
      *      DISPLAY SECUENCIA-INV     LINE 01 POSITION 05.

       ACTUALIZAR-TEMPORAL.
            OPEN I-O ARCHIVO-SALDOS.
            INITIALIZE REG-SALDOS.
            MOVE LLAVE-ART-INV TO LLAVE-SAL.
            MOVE COD-LOTE-INV  TO COD-LOTE-SAL.

            IF COD1-TRANS-INV = 1
               PERFORM ACTUALIZAR-ENTRADA
            ELSE
               PERFORM ACTUALIZAR-SALIDA
            END-IF.
        
            CLOSE ARCHIVO-SALDOS.
            ADD  1        TO SECU-TEM.
            MOVE SECU-TEM TO SECU-INV.

            OPEN I-O MOVIMIENTO-INVENT.       

            WRITE REG-INVENT.
            CLOSE MOVIMIENTO-INVENT. 

       ACTUALIZAR-ENTRADA.
            READ ARCHIVO-SALDOS WITH NO LOCK
                 INVALID KEY
                    MOVE CANT-INV    TO ACUM-ENT-CANT (DIA-FACT)
                    WRITE REG-SALDOS
                    INITIALIZE VLR-INV
                 NOT INVALID KEY
                    INITIALIZE VARIABLES-SALDOS-W
                    PERFORM CALCULAR-SALDO-ACTUAL VARYING DIA-SDO-W FROM 1 BY 1 UNTIL
                                                  DIA-SDO-W > DIA-FACT             
                    IF SDO-ACT-CANT-W = 0
                       INITIALIZE VLR-INV
                    ELSE
                       COMPUTE VLR-INV ROUNDED = (SDO-ACT-VLR-W / SDO-ACT-CANT-W )
                    END-IF
                    COMPUTE VLR-INV = VLR-INV * CANT-INV
                    ADD CANT-INV TO ACUM-ENT-CANT (DIA-FACT)
                    ADD VLR-INV  TO ACUM-ENT-VLR  (DIA-FACT)
                    REWRITE REG-SALDOS
            END-READ.

       ACTUALIZAR-SALIDA.
            READ ARCHIVO-SALDOS WITH NO LOCK
                 INVALID KEY
                    MOVE CANT-INV  TO ACUM-SAL-CANT (DIA-FACT)
                    WRITE REG-SALDOS
                    INITIALIZE VLR-INV
                 NOT INVALID KEY
                    MOVE DIA-FACT TO DIA-CORTE-W
                    INITIALIZE VARIABLES-SALDOS-W
                    PERFORM CALCULAR-SALDO-ACTUAL VARYING DIA-SDO-W FROM 1 BY 1 UNTIL
                                                  DIA-SDO-W > DIA-CORTE-W

                    IF SDO-ACT-CANT-W = 0
                       INITIALIZE VLR-INV
                    ELSE
                       COMPUTE VLR-INV ROUNDED = (SDO-ACT-VLR-W / SDO-ACT-CANT-W )
                    END-IF
                    COMPUTE VLR-INV ROUNDED = VLR-INV * CANT-INV
                    ADD CANT-INV TO ACUM-SAL-CANT (DIA-FACT)
                    ADD VLR-INV  TO ACUM-SAL-VLR  (DIA-FACT)
                    REWRITE REG-SALDOS
            END-READ.



       ACTUALIZAR-BORRAR.
             MOVE LLAVE-ART-INV TO LLAVE-SAL.
             MOVE COD-LOTE-INV  TO COD-LOTE-SAL.
			
			OPEN I-O ARCHIVO-SALDOS
			
			READ ARCHIVO-SALDOS WITH NO LOCK 
			     INVALID KEY 
				    INITIALIZE REG-SALDOS
                 NOT INVALID KEY					
				    IF COD1-TRANS-INV = 2
					   SUBTRACT CANT-INV FROM ACUM-SAL-CANT (DIA-FACT)
					   SUBTRACT VLR-INV FROM ACUM-SAL-VLR (DIA-FACT)
					ELSE
                       SUBTRACT CANT-INV FROM ACUM-SAL-CANT (DIA-FACT)
                       SUBTRACT VLR-INV FROM ACUM-SAL-VLR (DIA-FACT)					   
					END-IF
                    REWRITE REG-SALDOS 					
			END-READ.
			
			CLOSE ARCHIVO-SALDOS.
      *       CALL "INV010" USING LLAVE-SAL.


       GRABAR-ARTICULO.
             CLOSE MAESTRO-ARTICULOS.
             OPEN I-O MAESTRO-ARTICULOS.
             INITIALIZE DATOS-ART.
             WRITE REG-MAESTRO.

       CERRAR-ARCHIVOS.
            MOVE "00" TO SW-INVALID-ENV
            CLOSE MOVIMIENTO-INVENT
                  MAESTRO-ARTICULOS
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
       CALCULAR-SALDO-ACTUAL.
            IF DIA-SDO-W = 1
                ADD  ACUM-ENT-CANT (32)    TO SDO-ACT-CANT-W SDO-INI-CANT-W
                ADD  ACUM-ENT-VLR  (32)    TO SDO-ACT-VLR-W  SDO-INI-VLR-W
                ADD  DEP-ENT-VLR   (32)    TO SDO-ACT-DEP-W
                ADD  INF-ENT-VLR   (32)    TO SDO-ACT-INF-W
                ADD  INFD-ENT-VLR  (32)    TO SDO-ACT-INFD-W 
              
            MOVE LLAVE-SAL                 TO LLAVE-SDO-L3
            MOVE DIA-SDO-W                 TO DIA-L3
            MOVE ACUM-ENT-CANT (32)        TO AC-ENT-CANT-L3
            MOVE ACUM-ENT-VLR  (32)        TO AC-ENT-VLR-L3
            MOVE ACUM-SAL-CANT (32)        TO AC-SAL-CANT-L3
            MOVE ACUM-SAL-VLR  (32)        TO AC-SAL-VLR-L3
            MOVE SDO-ACT-CANT-W            TO SDO-ACT-CANT-L3
            MOVE SDO-ACT-VLR-W             TO SDO-ACT-VLR-L3
     
      *     WRITE REG-LOG FROM LINEA-L3 BEFORE 1
        
            END-IF

            ADD ACUM-ENT-CANT (DIA-SDO-W)  TO SDO-AC-CANT-ENT-W SDO-ACT-CANT-W
            ADD ACUM-ENT-VLR  (DIA-SDO-W)  TO SDO-AC-VLR-ENT-W  SDO-ACT-VLR-W
            ADD ACUM-SAL-CANT (DIA-SDO-W)  TO SDO-AC-CANT-SAL-W SDO-ACT-CANT-W
            ADD ACUM-SAL-VLR  (DIA-SDO-W)  TO SDO-AC-VLR-SAL-W  SDO-ACT-VLR-W
            ADD DEP-ENT-VLR   (DIA-SDO-W)  TO SDO-ACT-DEP-W 
            ADD INF-ENT-VLR   (DIA-SDO-W)  TO SDO-ACT-INF-W
            ADD INFD-ENT-VLR  (DIA-SDO-W)  TO SDO-ACT-INFD-W.

            MOVE LLAVE-SAL                 TO LLAVE-SDO-L3
            MOVE DIA-SDO-W                 TO DIA-L3
            MOVE ACUM-ENT-CANT (DIA-SDO-W) TO AC-ENT-CANT-L3
            MOVE ACUM-ENT-VLR  (DIA-SDO-W) TO AC-ENT-VLR-L3
            MOVE ACUM-SAL-CANT (DIA-SDO-W) TO AC-SAL-CANT-L3
            MOVE ACUM-SAL-VLR  (DIA-SDO-W) TO AC-SAL-VLR-L3
            MOVE SDO-ACT-CANT-W            TO SDO-ACT-CANT-L3
            MOVE SDO-ACT-VLR-W             TO SDO-ACT-VLR-L3.

       EDITAR-1ER-DIGITO.
            IF NRO-FACT1-W < 50
               MOVE NROB-FACT1-W TO COMPROB1-X
            ELSE
              EVALUATE NRO-FACT1-W
              WHEN 50 MOVE "A" TO COMPROB1-X
              WHEN 51 MOVE "B" TO COMPROB1-X
              WHEN 52 MOVE "C" TO COMPROB1-X
              WHEN 53 MOVE "D" TO COMPROB1-X
              WHEN 54 MOVE "E" TO COMPROB1-X
              WHEN 55 MOVE "F" TO COMPROB1-X
              WHEN 56 MOVE "G" TO COMPROB1-X
              WHEN 57 MOVE "H" TO COMPROB1-X
              WHEN 58 MOVE "I" TO COMPROB1-X
              WHEN 59 MOVE "J" TO COMPROB1-X
              WHEN 60 MOVE "K" TO COMPROB1-X
              WHEN 61 MOVE "L" TO COMPROB1-X
              WHEN 62 MOVE "M" TO COMPROB1-X
              WHEN 63 MOVE "N" TO COMPROB1-X
              WHEN 64 MOVE "O" TO COMPROB1-X
              WHEN 65 MOVE "P" TO COMPROB1-X
              WHEN 66 MOVE "Q" TO COMPROB1-X
              WHEN 67 MOVE "R" TO COMPROB1-X
              WHEN 68 MOVE "S" TO COMPROB1-X
              WHEN 69 MOVE "T" TO COMPROB1-X
              WHEN 70 MOVE "U" TO COMPROB1-X
              WHEN 71 MOVE "V" TO COMPROB1-X
              WHEN 72 MOVE "W" TO COMPROB1-X
              WHEN 73 MOVE "X" TO COMPROB1-X
              WHEN 74 MOVE "Y" TO COMPROB1-X
              WHEN 75 MOVE "Z" TO COMPROB1-X
              WHEN 76 MOVE "a" TO COMPROB1-X
              WHEN 77 MOVE "b" TO COMPROB1-X
              WHEN 78 MOVE "c" TO COMPROB1-X
              WHEN 79 MOVE "d" TO COMPROB1-X
              WHEN 80 MOVE "e" TO COMPROB1-X
              WHEN 81 MOVE "f" TO COMPROB1-X
              WHEN 82 MOVE "g" TO COMPROB1-X
              WHEN 83 MOVE "h" TO COMPROB1-X
              WHEN 84 MOVE "i" TO COMPROB1-X
              WHEN 85 MOVE "j" TO COMPROB1-X
              WHEN 86 MOVE "k" TO COMPROB1-X
              WHEN 87 MOVE "l" TO COMPROB1-X
              WHEN 88 MOVE "m" TO COMPROB1-X
              WHEN 89 MOVE "n" TO COMPROB1-X
              WHEN 90 MOVE "o" TO COMPROB1-X
              WHEN 91 MOVE "p" TO COMPROB1-X
              WHEN 92 MOVE "q" TO COMPROB1-X
              WHEN 93 MOVE "r" TO COMPROB1-X
              WHEN 94 MOVE "s" TO COMPROB1-X
              WHEN 95 MOVE "t" TO COMPROB1-X
              WHEN 96 MOVE "u" TO COMPROB1-X
              WHEN 97 MOVE "v" TO COMPROB1-X
              WHEN 98 MOVE "w" TO COMPROB1-X
              WHEN 99 MOVE "x" TO COMPROB1-X
              END-EVALUATE
           END-IF.

005210* COPY "..\..\FUENTES\WS-HCEVO.CBL".
