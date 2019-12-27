004820*=================================================================
004830* FACTURACION - GENERA FACTURAS MENU PRINCIPAL.
      * ALMACEN SIN99 NO CONTABILIZA NI DESCARGA INVENTARIOS.
      * 17355476  FERRETERIA ALEXANDER SAN MARTIN
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "..\..\FUENTES\FS-USUNET.CBL".
004920     COPY "..\..\FUENTES\FS-SESION.CBL".
           COPY "..\..\FUENTES\FS-IMPRE.CBL".
           COPY "..\..\FUENTES\FS-SALUD.CBL".
		   
           SELECT ARCHIVO-SUCUR LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-SUCUR-W
      * "P:\PROG\DATOS\SC-SUCUR.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS COD-SUCUR
                  FILE STATUS IS OTR-STAT.
		   
           SELECT ARCHIVO-PREFIJOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PREFI-W 
      *"../control/SC-ARCHPREF.DAT"
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS LLAVE-PREF
                  FILE STATUS IS OTR-STAT.
				  
           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-USUAR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-LOTE-FARM LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-LOTE-LNK
                  ORGANIZATION INDEXED
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY COD-LTF
                  ALTERNATE RECORD KEY ART-LTF WITH DUPLICATES
                  ALTERNATE RECORD KEY DESCRIP-LTF WITH DUPLICATES
                  ALTERNATE RECORD KEY LOTE-LTF WITH DUPLICATES
                  ALTERNATE RECORD KEY VENCE-LTF WITH DUPLICATES
                  ALTERNATE RECORD KEY CUM-LTF WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-JSON
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.

005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-IMPRE.CBL".
       COPY "..\..\FUENTES\FD-SALUD.CBL".
       COPY "..\..\FUENTES\FD-SUCUR.CBL".
       COPY "..\..\FUENTES\FD-PREFI.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-LTFAR.CBL".
	   
       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON                PIC X(1000).

005190 WORKING-STORAGE SECTION.
005200 COPY "..\..\FUENTES\COBW3.CBL".
005210 COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-PLANO-W                   PIC x(90).
       77 NOM-USUAR-W                   PIC X(70).
       77 NOM-PRINTER                   PIC X(70).
       77 NOM-PREFI-W                   PIC X(70).
       77 NOM-SUCUR-W                   PIC X(70).
       77 MES-SAL-W                     PIC X(4).
       77 SAL-MIN-W                     PIC 9(6)V99.
       77 SW-PASO                       PIC 9.
       77 SW-PASO-ALM-W                 PIC 9.
       
       01 DEPEN-LNK.
          02 LT-CON1-USU-W              PIC X.     
          02 LT-CON2-USU-W              PIC X.
          02 LT-CON3-USU-W              PIC X.
       	   
       01 CUOTAS-MOD-W.
          02 VLR-MOD-W                 PIC 9(9) OCCURS 3.

       01 VARIABLES.
          02 VALOR-APROX              PIC S9(9).
	   
       01 DATOS-ENVIO.
          02 DAT0-ENV                   PIC XX.
		
       01 SUCURSAL.
          02 SUCURSAL-PRN-W             PIC XX.
		  
       01 DATOS-OPER-W.
          02 NOMBRE-OPER-W.
             03 NOMBRE1-OPER-W        PIC X(28).
             03 SUC-OPER-W            PIC X(2).
          02 IDENT-OPER-W             PIC 9(10).
		   
       01 DATOS-MENU.
          02  FECHA-LNK.
              05 ANO-LNK              PIC 99.
              05 MES-LNK              PIC 99.
              05 DIA-LNK              PIC 99. 
       
       01 REG-W.
          02 LLAVE-W.
             03 SUC-W                     PIC XX.
             03 CL-W                      PIC 9.
             03 NRO-W                     PIC 9(6).	  
		
       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
      *    02 IP-SERVER-W               PIC X(18).
          02 DIR-CONTAB-LLEGADA-W      PIC X(8).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 ADMIN-LLEGADA-W           PIC X(5).
          02 RX-822-W                  PIC XX.
          02 SUC-W-ENVIADA             PIC XX.

       01 LINEA-LLEGADA-W              PIC X(100).
          

       01 INVALID-W                    PIC X(80).
       01 IN-W                         PIC X(80).
       01 SW-INVALID-W                 PIC 99.
       01 MENSAJE-INDALID-W            PIC X(50).

       01 LIN-1.
          02 FILLER                    PIC X(12)   VALUE "{*SUC-FACT*:". 
          02 FILLER                    PIC X       VALUE "*".
          02 SUC-FACT-ENVIAR           PIC X(2).
          02 FILLER                    PIC XX      VALUE "*,".
          02 FILLER                    PIC X(11)   VALUE "{*ADMIN-W*:". 
          02 FILLER                    PIC X       VALUE "*".
          02 ADMIN-ENVIAR              PIC X(4).

          02 FILLER                    PIC XX      VALUE "*}".
          02 CIERRE-LIN-1              PIC X.

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
              MOVE "INV401"                 TO MSJ3-HTML
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
              MOVE "INV401"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PRINTER.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PRINTER              TO MSJ2-HTML
              MOVE "INV403"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON FACTURA-SERVICIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "INV401"                 TO MSJ2-HTML
              MOVE NOM-SALUD                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SUCUR.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "INV401"                 TO MSJ2-HTML
              MOVE NOM-SUCUR-W              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PREFIJOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "INV401"                 TO MSJ2-HTML
              MOVE NOM-PREFI-W              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-USUAR-W          TO MSJ2-HTML
              MOVE "INV401"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-LOTE-FARM.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "INV401"                 TO MSJ2-HTML
              MOVE NOM-LOTE-LNK             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "INV401"                 TO MSJ2-HTML
              MOVE NOM-PLANO-W              TO MSJ3-HTML
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
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, ADMIN-LLEGADA-W, RX-822-W, SUC-W-ENVIADA
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
007010                  MOVE "Validacion de Usuario"       TO MENSAJE1-HTML
007020                  MOVE "Falta configurar usuarios"   TO MENSAJE2-HTML
007030                  MOVE "Sc"                          TO MENSAJE3-HTML
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
007230        MOVE "Validacion de Sesion"        TO MENSAJE1-HTML
007240        MOVE "Falta configurar usuarios"   TO MENSAJE2-HTML
007250        MOVE "Sc"                          TO MENSAJE3-HTML
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

001614     INITIALIZE NOM-LOTE-LNK
                      NOM-PLANO-W
                      NOM-USUAR-W
                      NOM-PREFI-W
                      NOM-SUCUR-W

           MOVE "\\" TO NOM-USUAR-W

           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

001621     INSPECT NOM-USUAR-W REPLACING FIRST " "
001624                                       BY "\"

007190     INSPECT NOM-USUAR-W REPLACING FIRST "        "
                                            BY DIR-CONTAB-LLEGADA-W

           MOVE NOM-USUAR-W TO NOM-LOTE-LNK NOM-PREFI-W

           INSPECT NOM-USUAR-W REPLACING FIRST "                       "
007200                                      BY "\CONTROL\SC-ARCHUSU.DAT".

           INSPECT NOM-LOTE-LNK REPLACING FIRST "                     "
007200                                       BY "\CONTROL\SC-LOTEF.DAT".

           INSPECT NOM-PREFI-W  REPLACING FIRST "                        "
007200                                       BY "\CONTROL\SC-ARCHPREF.DAT".             
           

           MOVE "\\" TO NOM-PLANO-W 
007220     INSPECT NOM-PLANO-W  REPLACING FIRST "                    "
007230                                       BY IP-DATOS-USUNET
      

           INSPECT NOM-PLANO-W REPLACING FIRST "                                                           "
                                           BY  "\PROG\DATOS\JSON\JSC-SUC-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"

007281     INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
007282                                       BY LLAVE-SESION-LLEGAD-W.

           MOVE NOM-PLANO-W TO NOM-SUCUR-W
           INSPECT NOM-SUCUR-W REPLACING FIRST "JSON\JSC-SUC-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"
                                            BY "SC-SUCUR.DAT                                   ".

           MOVE ADMIN-LLEGADA-W TO ADMIN-W.

       ABRIR-ARCHIVO-USUARIOS.
           OPEN INPUT ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT AT END MOVE 0 TO OPCION-1.
           CLOSE ARCHIVO-USUARIOS.

       REVISAR-DATO.           
           IF NIT-USU = 844003225 OR 845000038
              MOVE "N" TO BARRAS-USU
           END-IF.
        
           IF LOTE-FARM-USU = "S" OR "N"
              NEXT SENTENCE
           ELSE
              IF (PUC-USU = 4 OR 6)
                 MOVE "S" TO LOTE-FARM-USU
              ELSE
                 MOVE "N" TO LOTE-FARM-USU
              END-IF.

           IF PREFIJO-USU = "  "
              MOVE "00"  TO PREFIJO-USU.

           CALL "SC-SALUD" USING NOM-SALUD-W INVALID-W.
           
      *     IF TIPO1-COMP = 3
      *        MOVE "../CONTROL/SC-FACEX.DAT" TO NOM-SALUD
      *     END-IF.
      * PARA SOLUCIONAR EL PROBLEMA SC-SALUD DEBO ASIGNAR LAS VARIABLES MANUALMENTE

           IF  (PUC-USU = 4 OR 6)
           AND CONTADO-USU  = "S"
               MOVE "N" TO CONTADO-USU.
      

           COMPUTE SAL-MIN-W ROUNDED = SAL-MIN-USU / 30.

           COMPUTE VALOR-APROX ROUNDED  = (SAL-MIN-W * 0.00117)
           COMPUTE VLR-MOD-W (1) = VALOR-APROX * 100.

           COMPUTE VALOR-APROX ROUNDED = (SAL-MIN-W * 0.004610)
           COMPUTE VLR-MOD-W (2) = VALOR-APROX * 100.

           COMPUTE VALOR-APROX ROUNDED  = (SAL-MIN-W * 0.01215)
           COMPUTE VLR-MOD-W (3)  = VALOR-APROX * 100.
           
           MOVE LT-CON1-USU TO LT-CON1-USU-W
           MOVE LT-CON2-USU TO LT-CON2-USU-W
           MOVE LT-CON3-USU TO LT-CON3-USU-W.
           
       LEER-IMPRESORA.
           CALL "CON003A" USING ADMIN-W DEPEN-LNK INVALID-W.
           IF DEPEN-LNK IS NOT NUMERIC
           OR DEPEN-LNK =  000
              MOVE "\\" TO NOM-PRINTER
              INSPECT NOM-PRINTER REPLACING FIRST "                    "
                                               BY IP-DATOS-USUNET
              INSPECT NOM-PRINTER REPLACING FIRST "                       "
                                               BY "\PROG\DATOS\PRINTER.PSC"
           ELSE
              MOVE "\\" TO NOM-PRINTER
              INSPECT NOM-PRINTER REPLACING FIRST "                    "
                                               BY IP-DATOS-USUNET
              INSPECT NOM-PRINTER REPLACING FIRST "                           "
                                               BY "\PROG\DATOS\PRINTER-XXX.PSC"
              INSPECT NOM-PRINTER REPLACING FIRST "XXX"
                                               BY DEPEN-LNK
           END-IF.

           OPEN INPUT ARCHIVO-PRINTER.
           READ ARCHIVO-PRINTER NEXT AT END MOVE 0 TO OPCION-1
           CLOSE ARCHIVO-PRINTER.   

        ABRIR-PREFIJOS.
           OPEN INPUT ARCHIVO-PREFIJOS.
           READ ARCHIVO-PREFIJOS NEXT AT END MOVE 0 TO OPCION-1.
           CLOSE ARCHIVO-PREFIJOS.

           EVALUATE SUCURSAL-PRN
           WHEN 01  IF (PUC-USU = 4 OR 6)
                       MOVE "DR001" TO ALM-PREF (1)
                    ELSE
                       IF NIT-USU = 800162035
                       AND PREFIJO-USU = 08
                          MOVE "SIN99" TO ALM-PREF (1)
                       ELSE
      * CLINICA CASANARE MANEJA ALM02 PARA FARMACIA
                          IF  NIT-USU = 891855847
                          AND FECHA-LNK > 150430
                              MOVE "ALM02" TO ALM-PREF (1)
                          ELSE
                              MOVE "ALM01" TO ALM-PREF (1)
                          END-IF
                       END-IF
                    END-IF
           WHEN 02  EVALUATE NIT-USU
                    WHEN 845000038 MOVE "DR003" TO ALM-PREF (1)
                    WHEN 800162035 MOVE "ALM02" TO ALM-PREF (1)
                    WHEN 800037979 MOVE "URG01" TO ALM-PREF (1)
                    WHEN 800037021 MOVE "DR047" TO ALM-PREF (1)
                    WHEN OTHER     MOVE "DR002" TO ALM-PREF (1)
                    END-EVALUATE
           WHEN 03  EVALUATE NIT-USU
                    WHEN 845000038 MOVE "DR002" TO ALM-PREF (1)
                    WHEN OTHER     MOVE "DR003" TO ALM-PREF (1)
                    END-EVALUATE
           WHEN 04  EVALUATE NIT-USU
                    WHEN 800162035 MOVE "ALM00" TO ALM-PREF (1)
                    WHEN OTHER     MOVE "DR004" TO ALM-PREF (1)
                    END-EVALUATE
           WHEN 05  EVALUATE NIT-USU
                    WHEN 800037021 MOVE "DR034" TO ALM-PREF (1)
                    WHEN OTHER     MOVE "DR005" TO ALM-PREF (1)
                    END-EVALUATE
           WHEN 08  MOVE "SIN98" TO ALM-PREF (1)
           WHEN 09  MOVE "SIN99" TO ALM-PREF (1)
           WHEN 10  MOVE "ALM10" TO ALM-PREF (1)
           WHEN OTHER
                   IF ADMIN-W = "RLSY"
                   OR (PUC-USU = 4 OR 6)
                      MOVE "DR001" TO ALM-PREF (1)
                   ELSE
                      MOVE "ALM01" TO ALM-PREF (1)
                   END-IF
           END-EVALUATE.

        ABRIR-LOTE.
           OPEN INPUT ARCHIVO-LOTE-FARM.
           READ ARCHIVO-LOTE-FARM NEXT AT END MOVE 0 TO OPCION-1.
                  
        DATO-SUCURSAL.
            IF PREFIJO-USU = "  "
               MOVE "00"        TO PREFIJO-USU
            END-IF.

            CALL "CON003" USING ADMIN-W NOMBRE-OPER-W IDENT-OPER-W IN-W

            IF (ADMIN-W = "GEBC" OR "ADMI")
                CONTINUE
            ELSE
               EVALUATE NIT-USU
      * ESE SALUD YOPAL
               WHEN 844003225
                    IF SUC-OPER-W = "JL" OR "CA" OR "CS"
                                 OR "PV" OR "BC" OR "LC"
                                 OR "CV" OR "HT" OR "EM"
                                 OR "HY" OR "TL" OR "MR"
                                 OR "01"
                       MOVE SUC-OPER-W TO PREFIJO-USU
                    ELSE
                       MOVE "48"                       TO MENSAJE1-HTML
                       MOVE "Sucursal no permitida "   TO MENSAJE2-HTML
                       MOVE "INV401"                   TO MENSAJE3-HTML
                       CLOSE ARCHIVO-LOTE-FARM
                       GO TO ENVIAR2-ERROR
                    END-IF
      *  SERVIMEDICOS
               WHEN 800162035
                    IF SUC-OPER-W = "01" OR "03" OR "05" OR "06"
                                 OR "07" OR "08" OR "10" OR "11"
                                 OR "12" OR "14" OR "15" OR "17"
                       MOVE SUC-OPER-W TO PREFIJO-USU
                    ELSE
                       MOVE "48"                       TO MENSAJE1-HTML
                       MOVE "Sucursal no permitida "   TO MENSAJE2-HTML
                       MOVE "INV401"                   TO MENSAJE3-HTML
                       CLOSE ARCHIVO-LOTE-FARM
                       GO TO ENVIAR2-ERROR
                    END-IF
      * MULTISALUD VILLAVICENCIO
               WHEN 830511298
                   IF SUC-OPER-W = "01" OR "02" OR "03" OR "04"
                                OR "05" OR "06" OR "07" OR "08"
                                OR "09" OR "10" OR "11" OR "12"
                                OR "13" OR "14" OR "15" OR "16"
                                OR "17" OR "18" OR "19" OR "20"

                        MOVE SUC-OPER-W TO PREFIJO-USU
                   ELSE
                        MOVE "48"                       TO MENSAJE1-HTML
                        MOVE "Sucursal no permitida "   TO MENSAJE2-HTML
                        MOVE "INV401"                   TO MENSAJE3-HTML
                        CLOSE ARCHIVO-LOTE-FARM
                        GO TO ENVIAR2-ERROR
                   END-IF

      * FAMEDIC
               WHEN 900405505
                    IF SUC-OPER-W = "01" OR "02" OR "03" OR "04"
                                 OR "05" OR "06" OR "07" OR "08"
                                 OR "09"
                       MOVE SUC-OPER-W TO PREFIJO-USU
                    ELSE
                       MOVE "48"                       TO MENSAJE1-HTML
                       MOVE "Sucursal no permitida "   TO MENSAJE2-HTML
                       MOVE "INV401"                   TO MENSAJE3-HTML
                       CLOSE ARCHIVO-LOTE-FARM
                       GO TO ENVIAR2-ERROR
                    END-IF


      * SOCIEDAD CARDIOLOGICA
               WHEN 900161116
                    IF SUC-OPER-W = "01" OR "02" OR "03" OR "04"
                       MOVE SUC-OPER-W TO PREFIJO-USU
                    ELSE
                       MOVE "48"                       TO MENSAJE1-HTML
                       MOVE "Sucursal no permitida "   TO MENSAJE2-HTML
                       MOVE "INV401"                   TO MENSAJE3-HTML
                       CLOSE ARCHIVO-LOTE-FARM
                       GO TO ENVIAR2-ERROR
                    END-IF


      * MAVEPHARMA
               WHEN 900658867
                    IF SUC-OPER-W = "01" OR "02" OR "03" OR "04"
                                 OR "05" OR "06" OR "07" OR "10"
                       MOVE SUC-OPER-W TO PREFIJO-USU
                    ELSE
                       MOVE "48"                       TO MENSAJE1-HTML
                       MOVE "Sucursal no permitida "   TO MENSAJE2-HTML
                       MOVE "INV401"                   TO MENSAJE3-HTML
                       CLOSE ARCHIVO-LOTE-FARM
                       GO TO ENVIAR2-ERROR
                    END-IF
      * ENLACE
               WHEN 900541158
                    IF SUC-OPER-W = "01" OR "02" OR "03" OR "04"
                                 OR "05" OR "06" OR "07" OR "10"
                       MOVE SUC-OPER-W TO PREFIJO-USU
                    ELSE
                       MOVE "48"                       TO MENSAJE1-HTML
                       MOVE "Sucursal no permitida "   TO MENSAJE2-HTML
                       MOVE "INV401"                   TO MENSAJE3-HTML
                       CLOSE ARCHIVO-LOTE-FARM
                       GO TO ENVIAR2-ERROR
                    END-IF


      * MAVESALUD
               WHEN 900566047
                    IF SUC-OPER-W = "01" OR "02" OR "03" OR "04"
                                 OR "05" OR "06" OR "07" OR "08"
                                 OR "09" OR "10" OR "11" OR "12"

                       MOVE SUC-OPER-W TO PREFIJO-USU
                    ELSE
                       MOVE "48"                       TO MENSAJE1-HTML
                       MOVE "Sucursal no permitida "   TO MENSAJE2-HTML
                       MOVE "INV401"                   TO MENSAJE3-HTML
                       CLOSE ARCHIVO-LOTE-FARM
                       GO TO ENVIAR2-ERROR
                    END-IF


      * ALBERGUE SUKURAME
               WHEN 900565371
                    IF SUC-OPER-W = "01" OR "02" OR "03" OR "04"
                                 OR "05" OR "06" OR "07" OR "10"
                       MOVE SUC-OPER-W TO PREFIJO-USU
                    ELSE
                       MOVE "48"                       TO MENSAJE1-HTML
                       MOVE "Sucursal no permitida "   TO MENSAJE2-HTML
                       MOVE "INV401"                   TO MENSAJE3-HTML
                       CLOSE ARCHIVO-LOTE-FARM
                       GO TO ENVIAR2-ERROR
                    END-IF



      * IMAGENES DIAGNOSTICAS VCIO
               WHEN 800156469
                    IF SUC-OPER-W = "00" OR "01" OR "02" OR "03"
                       MOVE SUC-OPER-W TO PREFIJO-USU
                    ELSE
                       MOVE "48"                       TO MENSAJE1-HTML
                       MOVE "Sucursal no permitida "   TO MENSAJE2-HTML
                       MOVE "INV401"                   TO MENSAJE3-HTML
                       CLOSE ARCHIVO-LOTE-FARM
                       GO TO ENVIAR2-ERROR
                    END-IF
      * TERAMED
               WHEN 900641654
                    IF SUC-OPER-W = "00" OR "01" OR "02" OR "03" OR "04"
                       MOVE SUC-OPER-W TO PREFIJO-USU
                    ELSE
                       MOVE "48"                       TO MENSAJE1-HTML
                       MOVE "Sucursal no permitida "   TO MENSAJE2-HTML
                       MOVE "INV401"                   TO MENSAJE3-HTML
                       CLOSE ARCHIVO-LOTE-FARM
                       GO TO ENVIAR2-ERROR
                    END-IF
      * TERAMED
               WHEN 800037979
                    IF SUC-OPER-W = "00" OR "01" OR "02" OR "03" OR "04"
                       MOVE SUC-OPER-W TO PREFIJO-USU
                    ELSE
                       MOVE 01 TO PREFIJO-USU
                    END-IF

      * SIKUANI
               WHEN 830512772
                    IF SUC-OPER-W = "01" OR "02" OR "03"
                                 OR "04" OR "05" OR "06"
                                 OR "07" OR "08" OR "09"
                                 OR "10" OR "11" OR "12"
                                 OR "13" OR "14" OR "15"
                                 OR "16" OR "17" OR "18"
                                 OR "19" OR "20" OR "21"
                                 OR "22" OR "23" OR "24"
                                 OR "25" OR "26" OR "27"
                                 OR "28" OR "29" OR "30"
                                 OR "31" OR "32" OR "33"
                                 OR "34" OR "35" OR "36"
                                 OR "37" OR "38" OR "39"
                                 OR "40" OR "41" OR "42"
                                 OR "43" OR "44" OR "45"
                                 OR "46" OR "47" OR "48"
                                 OR "49" OR "50"
                       MOVE SUC-OPER-W TO PREFIJO-USU
                    ELSE
                       MOVE "48"                       TO MENSAJE1-HTML
                       MOVE "Sucursal no permitida "   TO MENSAJE2-HTML
                       MOVE "INV401"                   TO MENSAJE3-HTML
                       CLOSE ARCHIVO-LOTE-FARM
                       GO TO ENVIAR2-ERROR
                    END-IF

                END-EVALUATE
            END-IF.
            
            IF NIT-USU = 800162035
            AND PREFIJO-USU = 08
               MOVE "SIN99" TO ALM-PREF (1)
            END-IF.

            MOVE PREFIJO-USU TO SUC-FACT
            MOVE "0830092718" TO NIT-USU.
            IF (NIT-USU = 830092718 OR 830092719 OR 900193162)
      * IMAGENES Y EQUIPOS BOGOTA     
                CALL "CON003" USING ADMIN-W NOMBRE-OPER-W IDENT-OPER-W IN-W

                IF RX-822-W = "un"
                   MOVE "00"                 TO MSJ1-HTML
                   MOVE "RX-822-W"           TO MSJ2-HTML
                   MOVE "INV401"             TO MSJ3-HTML
                   GO TO ENVIAR2-ERROR               
                ELSE
                   IF  ADMIN-W = "0101" OR "GEBC"
                   OR  SUC-OPER-W = "**"
                       MOVE RX-822-W  TO SUC-W
                       MOVE SUC-W     TO SUCURSAL-PRN SUC-FACT
                ELSE
                   IF SUC-W IS NOT = SUC-OPER-W
                      MOVE "48"                      TO MENSAJE1-HTML
                      MOVE "Sucursal no permitida "  TO MENSAJE2-HTML
                      MOVE "INV401"                  TO MENSAJE3-HTML
                      GO TO ENVIAR2-ERROR
                   ELSE
                      MOVE SUC-W TO SUCURSAL-PRN SUC-FACT
                   END-IF
                END-IF    
            END-IF.
      
         

            IF (NIT-USU = 844003225)
            AND SUC-OPER-W = SPACES
                MOVE PREFIJO-USU TO SUC-W
                IF SUC-W-ENVIADA = "un" 
                    MOVE "00"                      TO MENSAJE1-HTML
                    MOVE "SER868-F8"               TO MENSAJE2-HTML
                    MOVE "INV401"                  TO MENSAJE3-HTML
                    GO TO ENVIAR2-ERROR
                ELSE 
                    MOVE SUC-W    TO SUC-FACT
                END-IF
            END-IF.

      * SALUD YOPAL
           IF NIT-USU = 844003225
              IF SUC-FACT = "JL" OR "CA" OR "CS"
                         OR "PV" OR "BC" OR "LC"
                         OR "CV" OR "HT" OR "EM"
                         OR "HY" OR "TL" OR "MR"
                         OR "01"
                   CONTINUE
              ELSE
                   MOVE "48"                       TO MENSAJE1-HTML
                   MOVE "Sucursal no permitida "   TO MENSAJE2-HTML
                   MOVE "INV401"                   TO MENSAJE3-HTML
                   GO TO ENVIAR2-ERROR
              END-IF
           END-IF.


      * SIKUANI
           IF NIT-USU = 830512772
              IF SUC-FACT = "01" OR "02" OR "03"
                         OR "04" OR "05" OR "06"
                         OR "07" OR "08" OR "09"
                         OR "10" OR "11" OR "12"
                         OR "13" OR "14" OR "15"
                         OR "16" OR "17" OR "18"
                         OR "19" OR "20" OR "21"
                         OR "22" OR "23" OR "24"
                         OR "25" OR "26" OR "27"
                         OR "28" OR "29" OR "30"
                         OR "31" OR "32" OR "33"
                         OR "34" OR "35" OR "36"
                         OR "37" OR "38" OR "39"
                         OR "40" OR "41" OR "42"
                         OR "43" OR "44" OR "45"
                         OR "46" OR "47" OR "48"
                         OR "49" OR "50"
                   CONTINUE
              ELSE
                   MOVE "48"                       TO MENSAJE1-HTML
                   MOVE "Sucursal no permitida "   TO MENSAJE2-HTML
                   MOVE "INV401"                   TO MENSAJE3-HTML
                   GO TO ENVIAR2-ERROR
              END-IF
                   
              OPEN INPUT ARCHIVO-SUCUR
              MOVE SUC-FACT TO COD-SUCUR
              READ ARCHIVO-SUCUR INVALID KEY
                                 MOVE SPACES TO ALM-PREF (1)
              NOT INVALID KEY
                    INITIALIZE SW-PASO-ALM-W
                    IF COD-SUCUR = 01
                       MOVE 1 TO SW-PASO-ALM-W
                    ELSE
                       MOVE 0 TO SW-PASO-ALM-W
                    END-IF
                    IF ADMIN-W = "INVI" OR "VENT"
                       MOVE "SIN98" TO ALM-PREF (1)
                    ELSE
                       MOVE ALMACEN-SUCUR TO ALM-PREF (1)
                    END-IF
              END-READ
              CLOSE ARCHIVO-SUCUR
           END-IF.
          
           IF NIT-USU = 900658867
           AND PREFIJO-USU = 10
               MOVE "S" TO BARRAS-USU
           END-IF.
           
      * LEER-RESTRICCION.
      *     INITIALIZE OPC-REST
      *     OPEN INPUT ARCHIVO-RESTRICCION.
      *     MOVE OPER-LLEGAD-W    TO COD-REST

      *     READ ARCHIVO-RESTRICCION WITH NO LOCK 
      *          INVALID KEY 
      *                  MOVE "99"                  TO MENSAJE1-HTML
      *                  MOVE "Usuario no existe"   TO MENSAJE2-HTML
      *                  MOVE "INV401"              TO MENSAJE3-HTML
      *                  GO TO ENVIAR-ERROR
      *     END-READ.

      *     CLOSE ARCHIVO-RESTRICCION.
      *     MOVE "00"                 TO MSJ1-HTML
      *     MOVE SUC-FACT             TO MSJ2-HTML
      *     MOVE "INV401"             TO MSJ3-HTML
      *     GO TO ENVIAR2-ERROR.

            

       ABRIR-JSON-SUC.           
           OPEN OUTPUT ARCHIVO-JSON.
           
           MOVE "{*SUCURSAL*:[" TO DATOS-JSON
           INSPECT DATOS-JSON REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON END-WRITE
           MOVE 0 TO SW-FIN.
           
      *     MOVE "00"                 TO MSJ1-HTML
      *     MOVE ADMIN-W              TO MSJ2-HTML
      *     MOVE "INV401"             TO MSJ3-HTML
      *     GO TO ENVIAR2-ERROR.

       ENVIAR-SUC.
           INITIALIZE LIN-1
           MOVE ADMIN-W   TO ADMIN-ENVIAR
           MOVE SUC-FACT  TO SUC-FACT-ENVIAR
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE "," TO CIERRE-LIN-1.

       CERRAR-SUC.
           INITIALIZE LIN-1
           MOVE LIN-1 TO DATOS-JSON
           WRITE REG-JSON END-WRITE

           MOVE "}]" TO DATOS-JSON
           WRITE REG-JSON END-WRITE

           CLOSE ARCHIVO-JSON.

      *     MOVE "00"                 TO MSJ1-HTML
      *     MOVE "RX-822-W"           TO MSJ2-HTML
      *     MOVE "INV401"             TO MSJ3-HTML
      *     GO TO ENVIAR2-ERROR.

010700  PAGINA-CONFIG.
010720     MOVE "datosrecibidos" TO COBW3-CNV-NAME
010730     MOVE SUC-FACT         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3
010750
010760     MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
010770     PERFORM ABRIR-HTML.
007980   
007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.

008030
008040 COPY "..\..\FUENTES\SC-WEB.CBL".
005210* COPY "..\..\FUENTES\WS-HCEVO.CBL".
