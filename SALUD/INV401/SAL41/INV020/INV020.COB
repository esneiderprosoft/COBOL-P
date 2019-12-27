004820*=================================================================
004830* FACTURACION CLINICAS - CONTABILIZAR UNA FACTURA
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

           SELECT ARCHIVO-TARIFAS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TARIF-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS COD-TAR. 

           SELECT ARCHIVO-TERCEROS  LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TER-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-TERCERO
                  ALTERNATE RECORD KEY DESCRIP-TER WITH DUPLICATES
                  ALTERNATE RECORD KEY ACT-TER     WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-PACIENTES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PACIE-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PACI
                  ALTERNATE RECORD KEY  DESCRIP-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  EPS-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  ID-COTIZ-PACI WITH DUPLICATES
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

           SELECT ARCHIVO-TABLAS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TABLA-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS LLAVE-TAB
                  ALTERNATE RECORD KEY IS DESCRIP-TAB   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS COD-RIPS-TAB  WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-CUPS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-CUPS-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS LLAVE-CUP
                  ALTERNATE RECORD KEY IS DESCRIP-CUP WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LLAVE-ALT-CUP WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.  

           SELECT ARCHIVO-ESPECIALIDAD LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-ESP-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-ESP
                  ALTERNATE RECORD KEY IS NOMBRE-ESP WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.            

           SELECT ARCHIVO-GR-SER LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-GRSER-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CODIGO-GRSER
                  FILE STATUS IS OTR-STAT.

           SELECT MOVIMIENTO-DIARIO LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-MOV-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-MOV
                  ALTERNATE RECORD KEY SECUENCIA-MOV
                  ALTERNATE RECORD KEY LLAVE-FEC
                  ALTERNATE RECORD KEY LLAVE-DOC-MOV   WITH DUPLICATES
                  ALTERNATE RECORD KEY COSTO-MOV       WITH DUPLICATES
                  ALTERNATE RECORD KEY NRO-ORD-MOV     WITH DUPLICATES
                  ALTERNATE RECORD KEY LLAVE-LIBRE-MOV WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-MAESTROS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-MAE-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-MAE
                  ALTERNATE RECORD KEY IS NOMBRE-MAE    WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CTA-INTER-MAE WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-PROFESIONALES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PROF-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PROF
                  ALTERNATE RECORD KEY DESCRIP-PROF WITH DUPLICATES
                  ALTERNATE RECORD KEY ATIENDE-PROF WITH DUPLICATES
                  ALTERNATE RECORD KEY SUCURSAL-PROF WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-NUMER19.CBL".
       COPY "..\..\FUENTES\FD-SALUD.CBL".
       COPY "..\..\FUENTES\FD-TERCE.CBL".
       COPY "..\..\FUENTES\FD-TARIF.CBL".
       COPY "..\..\FUENTES\FD-PACIE.CBL".
       COPY "..\..\FUENTES\FD-ARTIC.CBL".
       COPY "..\..\FUENTES\FD-CUPS.CBL".
       COPY "..\..\FUENTES\FD-TABLA19.CBL".
       COPY "..\..\FUENTES\FD-ESPEC.CBL".
       COPY "..\..\FUENTES\FD-GRSER.CBL".
       COPY "..\..\FUENTES\FD-MOVIM.CBL".
       COPY "..\..\FUENTES\FD-MAEST.CBL".
       COPY "..\..\FUENTES\FD-PROFE.CBL".

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
             03 SUC-LLEGADA-W           PIC XX.
             03 PREFIJO-LLEGADA-W       PIC X.
             03 NUMERO-LLEGADA-W.
                05 NRO-FACT1-W          PIC 9.
                05 NRO-FACT2-W          PIC 9.
                05 NRO-FACT3-W          PIC 9(4).
          02 FECHA-NUM.
             03 ANO-NUM                 PIC 99.
             03 MES-NUM                 PIC 99.
             03 DIA-NUM                 PIC 99.
          02 NIT-USU                    PIC 9(10).
          02 SAL-MIN-USU                PIC 9(9).
          02 CONTADO-USU                PIC X.
          02 PUC-USU                    PIC X.
          02 PREFIJO-USU                PIC XX.
          02 CONTAB-POS-USU             PIC X.
          02 RET-CREE-USU               PIC X.
          02 PEDIDO-USU                 PIC X.
          02 AUTORET-USU                PIC X.
          02 RETENEDOR-USU              PIC X.
          02 NOM1-USU                   PIC X(7).
          02 SEPARA-CAJA-ALT            pic X.

       77 MES-SAL-W                     PIC X(4).
       77 NOM-NUMER-W                   PIC X(70).
       77 NOM-SAL-W                     PIC X(70).
       77 NOM-TARIF-W                   PIC X(70).
       77 NOM-TER-W                     PIC X(70).
       77 NOM-PACIE-W                   PIC X(70).
       77 NOM-ART-W                     PIC X(70).
       77 NOM-TABLA-W                   PIC X(70).
       77 NOM-CUPS-W                    PIC X(70).
       77 NOM-ESP-W                     PIC X(70).
       77 NOM-GRSER-W                   PIC X(70).
       77 NOM-PROF-W                    PIC X(70).
       77 NOM-MOV-W                     PIC X(70).
       77 NOM-MAE-W                     PIC X(70).
       77 SW-RET                        PIC 9.
       77 LLAVE-W                       PIC X(7).
       77 NOMBRE-CTA                    PIC X(30).
       77 NOMBRE-MED                    PIC X(30).
       77 ACTIV-W                       PIC 99.
       77 AUXIL-W                       PIC X(5).
       77 CL-W                          PIC 9.
       77 TIPO-L                        PIC X(11).
       77 CTA-COPAGO                    PIC X(11).
       77 ACT-PARTIC1                   PIC 99.
       77 ACT-PARTIC2                   PIC 99.
       77 PORC-RET-W                    PIC 99V99.
       77 BASE-MIN-RET-W                PIC 9(10).
       77 VALOR-EDIT                    PIC ZZZZ,ZZZ,ZZZ.
       77 ANO-ALFA                      PIC 9(4).
       77 SECU-X                        PIC 99.
       77 SECU-MOV-W                    PIC XX.

       01 NOM-UT.
          02 NOM1-UT                    PIC X(3).
          02 NOM2-UT                    PIC X.

       01 CTA-TAB-W.
          02 SUBCTA-TAB-W               PIC X(6).
          02 AUX-TAB-W                  PIC X(5).

       01 SUBCTA-TER.
          02  MAYOR-TER                 PIC X(4).
          02  SCTA-TER                  PIC 99.

       01 LLAVE-TIPO-W.
          02  COD-TAB-W                 PIC XX.
          02  TIPO-TAB-W                PIC 9.

       01 CTAS-CONTAB.
          02 CTA-CAJA.
             03 MAY-CJ-W                PIC X(6).
             03 AUX-CJ-W                PIC X(5).
          02 CTA-MEDI.
              03 MAY-MEDI               PIC X(4).
              03 SCTA-MEDI              PIC XX.

       01 NRO-COMP-W.
          02 NRO-COMP1-W                PIC 9.
          02 NRO-COMP2-W                PIC 9(4).

       01 VARIABLES.
          02 SW0                        PIC 9.
          02 SW-GRP                     PIC 9.
          02 CL-SW                      PIC 9.
          02 SEC-NUM                    PIC 9(3).
          02 ITEM                       PIC 99.
          02 VALOR-TOTAL                PIC S9(9)V99.
          02 VALOR-BRUTO                PIC S9(9).
          02 VALOR-TEMP                 PIC S9(11)V99.
          02 VALOR-APROX                PIC S9(9)V99.
          02 PORC-CIR                   PIC 9(3)V9.
          02 PORC-W                     PIC 9(3).
          02 TOT-ITEM-W                 PIC S9(11)V99.
          02 VALOR-RET                  PIC S9(11).
          02 BASE-CREE-W                PIC S9(11)V99.
          02 AUTORET-CREE-W             PIC S9(11).
                                   
       01 COD-CLIENTE.
          02 NOMBRE-CLIENTE             PIC XX.
          02 CEDULA-CLIENTE             PIC 9(3).
       01 COD-CLI-2.
          02 NOMBRE-CLI-2               PIC X(3).
          02 CEDULA-CLI-2               PIC 99.

       01 COD-MEDICO.
          02 NOMBRE-MEDICO              PIC XX.
          02 CEDULA-MEDICO              PIC 9(3).

       01 TABLA-ACUMULADOS.
          02 TABLA-ACUMULADOS-1 OCCURS 95.
             03 CL-ACUM                 PIC 9.
             03 CODIGO-ACUM             PIC X(11).
             03 COSTO-ACUM              PIC XX.
             03 VLR-ACUM                PIC S9(11)V99.
             03 MED-ACUM                PIC 9(10).

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV             PIC XX.
          02 FILLER                     PIC X VALUE "|".
          

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
              MOVE "INV020"                 TO MSJ3-HTML
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
              MOVE "INV020"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-NUMERACION.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-NUMER-W              TO MSJ2-HTML
              MOVE "INV020"                 TO MSJ3-HTML
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
              MOVE "INV020"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TARIFAS.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-TARIF-W              TO MSJ2-HTML
              MOVE "INV020"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TERCEROS.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-TER-W                TO MSJ2-HTML
              MOVE "INV020"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.   
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PACIE-W              TO MSJ2-HTML
              MOVE "INV020"                 TO MSJ3-HTML
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
              MOVE "INV020"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.   
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CUPS.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-CUPS-W               TO MSJ2-HTML
              MOVE "INV020"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.           
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TABLAS.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-TABLA-W              TO MSJ2-HTML
              MOVE "INV020"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF. 
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ESPECIALIDAD.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-ESP-W                TO MSJ2-HTML
              MOVE "INV020"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.   
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-GR-SER.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-GRSER-W              TO MSJ2-HTML
              MOVE "INV020"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF. 
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MOVIMIENTO-DIARIO.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-MOV-W                TO MSJ2-HTML
              MOVE "INV020"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-MAESTROS.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-MAE-W                TO MSJ2-HTML
              MOVE "INV020"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.   
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PROFESIONALES.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PROF-W               TO MSJ2-HTML
              MOVE "INV020"                 TO MSJ3-HTML
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
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, 
                    MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W,
                    LLAVE-LLEGADA-W, FECHA-NUM, NIT-USU, SAL-MIN-USU,
                    CONTADO-USU, PUC-USU, PREFIJO-USU, CONTAB-POS-USU,
                    RET-CREE-USU, PEDIDO-USU, AUTORET-USU, RETENEDOR-USU,
                    NOM1-USU, SEPARA-CAJA-ALT
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
           MOVE NOM-NUMER-W  TO NOM-CUPS-W
                                NOM-ESP-W
                                NOM-MOV-W
007190     INSPECT NOM-NUMER-W REPLACING FIRST "                              "
                                            BY DIR-CONTAB-LLEGADA-W
           INSPECT NOM-NUMER-W REPLACING FIRST "                     "
007300                                      BY "\CONTROL\SC-NUMER.DAT".
           MOVE NOM-NUMER-W TO NOM-SAL-W
                               NOM-TARIF-W  
                               NOM-TER-W  
                               NOM-PACIE-W
                               NOM-ART-W
                               NOM-TABLA-W
                               NOM-GRSER-W
                               NOM-MAE-W
                               NOM-PROF-W
           INSPECT NOM-SAL-W REPLACING FIRST "SC-NUMER.DAT"
007300                                    BY "SC-FACSA.DAT".
           INSPECT NOM-TARIF-W REPLACING FIRST "SC-NUMER.DAT  "
007300                                      BY "SC-ARCHTAR.DAT".
           INSPECT NOM-TER-W REPLACING FIRST "SC-NUMER.DAT  "
007300                                    BY "SC-ARCHTER.DAT".
           INSPECT NOM-PACIE-W REPLACING FIRST "SC-NUMER.DAT"
007300                                      BY "SC-PACIE.DAT".
           INSPECT NOM-ART-W REPLACING FIRST "SC-NUMER.DAT  "
007300                                    BY "SC-MAESART.DAT".
           INSPECT NOM-TABLA-W REPLACING FIRST "SC-NUMER.DAT"
007300                                      BY "SC-TABLA.DAT".
           INSPECT NOM-GRSER-W REPLACING FIRST "SC-NUMER.DAT   "
007300                                      BY "SC-GRUPOSER.DAT".
           INSPECT NOM-TABLA-W REPLACING FIRST "SC-NUMER.DAT  "
007300                                      BY "SC-ARCHMAE.DAT".
           INSPECT NOM-PROF-W REPLACING FIRST "SC-NUMER.DAT   "
007300                                     BY "SC-ARCHPROF.DAT".
           INSPECT NOM-CUPS-W REPLACING FIRST "                          "
                                           BY "PROG\DATOS\SC-ARCHCUPS.DAT".
           INSPECT NOM-ESP-W REPLACING FIRST "                         "
                                          BY "PROG\DATOS\SC-ARCHESP.DAT".
           INSPECT NOM-MOV-W REPLACING FIRST "                          "
                                          BY "PROG\DATOS\SC-ARCHLOTE.DAT".

        ABRIR-ARCHIVOS.
            MOVE "00" TO SW-INVALID-ENV
            OPEN INPUT MAESTRO-ARTICULOS
                       ARCHIVO-TABLAS
                       ARCHIVO-TARIFAS
                       ARCHIVO-TERCEROS
                       ARCHIVO-GR-SER
                       ARCHIVO-CUPS
                       ARCHIVO-ESPECIALIDAD
                       ARCHIVO-PACIENTES
                       ARCHIVO-NUMERACION
                       FACTURA-SERVICIOS.

            IF   ANO-NUM > 90
                  COMPUTE ANO-ALFA = ANO-NUM + 1900
            ELSE
                  COMPUTE ANO-ALFA = ANO-NUM + 2000.

        LEER-FACTURA.
            MOVE LLAVE-LLEGADA-W TO LLAVE-FACT
            READ FACTURA-SERVICIOS  WITH NO LOCK 
                                    INVALID KEY
                                        MOVE "01"  TO SW-INVALID-ENV
                                        GO TO CERRAR-ARCHIVOS
                                    NOT INVALID KEY
                                        CLOSE FACTURA-SERVICIOS
            END-READ.

            IF    (PUC-USU = 4 OR 6)
            AND  (PREFIJO-FACT = "A" OR "P" OR "T" OR "B" OR "D" OR "F"
                OR "G" OR "H" OR "I" OR "J" OR "K" OR "L" OR "M" OR "N"
                OR "O" OR "Q" OR "R" OR "S" OR "V" OR "W"
                OR "X" OR "Y" OR "Z")
                  GO TO CERRAR-ARCHIVOS.

            IF REFACTURA-FACT > 0
               GO TO CERRAR-ARCHIVOS
            END-IF   

            IF  PREFIJO-FACT = "E"
            AND CONTADO-USU  = "N"
            AND FECHA-FACT < 060601
               MOVE "C" TO PREFIJO-FACT
            END-IF   

            MOVE ESPEC-ELAB TO COD-ESP.
            READ ARCHIVO-ESPECIALIDAD  WITH NO LOCK
                   INVALID KEY 
                     MOVE SPACES TO COSTO-ESP
            END-READ.                          

         ASIGNAR-CAJA.
           EVALUATE PUC-USU
           WHEN 1 MOVE "11050500001" TO CTA-CAJA
                  MOVE "281510"      TO CTA-MEDI
                  MOVE 01            TO ACT-PARTIC1 ACT-PARTIC2
           WHEN 2 MOVE "11050100001" TO CTA-CAJA
                  MOVE "231010"      TO CTA-MEDI
                  MOVE "23100500010" TO CTA-COPAGO
                  MOVE 25            TO ACT-PARTIC1
                  MOVE 30            TO ACT-PARTIC2
           WHEN 3 MOVE "11050500001" TO CTA-CAJA
                  MOVE "281510"      TO CTA-MEDI
                  MOVE "28150500010" TO CTA-COPAGO
                  MOVE 25            TO ACT-PARTIC1
                  MOVE 30            TO ACT-PARTIC2
           WHEN 5 MOVE "11050500001" TO CTA-CAJA
                  MOVE "281510"      TO CTA-MEDI
                  MOVE "28150500010" TO CTA-COPAGO
                  MOVE 25            TO ACT-PARTIC1
                  MOVE 30            TO ACT-PARTIC2
           WHEN 4 MOVE "11050100001" TO CTA-CAJA
                  MOVE "290510"      TO CTA-MEDI
                  MOVE "29050500010" TO CTA-COPAGO
                  MOVE 27            TO ACT-PARTIC1
                  MOVE 27            TO ACT-PARTIC2
           WHEN 6 MOVE "11050100001" TO CTA-CAJA
                  MOVE "240710"      TO CTA-MEDI
                  MOVE "24070600010" TO CTA-COPAGO
                  MOVE 27            TO ACT-PARTIC1
                  MOVE 27            TO ACT-PARTIC2
           WHEN 7 MOVE "11010106001" TO CTA-CAJA
                  MOVE "250101"      TO CTA-MEDI
                  MOVE "25010106002" TO CTA-COPAGO
                  MOVE 25            TO ACT-PARTIC1
                  MOVE 30            TO ACT-PARTIC2
           END-EVALUATE.

      * IMAGENES DIAGNOSTICAS DEL LLANO
             IF  NIT-USU = 800156469 AND SUC-LLEGADA-W = 02
                 MOVE "11050500002"  TO CTA-CAJA.

           IF (NIT-USU = 830092718 OR 830092719)
              IF PREFIJO-USU = "TB"
                 MOVE "11050500002" TO CTA-CAJA
              ELSE
                 MOVE "11050500001" TO CTA-CAJA
              END-IF
              GO TO ASIGNAR-MEDICO
           END-IF

           EVALUATE F-PAGO-FACT
           WHEN 95     MOVE "TCR01"  TO AUX-CJ-W
           WHEN 96     MOVE "TDB01"  TO AUX-CJ-W
           WHEN OTHER  MOVE "00001"  TO AUX-CJ-W.

        ASIGNAR-MEDICO.
            IF PREFIJO-FACT = "E" OR "C"
               MOVE "CL" TO COD-TAR
               READ ARCHIVO-TARIFAS WITH NO LOCK
                      INVALID KEY
                      MOVE "SO" TO COD-TAR
               END-READ
            ELSE
               PERFORM LEER-CUENTA
               IF  (NIT-USU = 892000401 OR 900648993 OR 900755133
                           OR 900804411 OR 900870633)
               AND FECHA-ING-NUM > 20030331
                   PERFORM CAMBIAR-NIT.

            MOVE MED-OTR-FACT TO COD-TER.
            READ ARCHIVO-TERCEROS  WITH NO LOCK 
                   INVALID KEY
                   MOVE ALL "*" TO DESCRIP-TER
            END-READ
            MOVE DESCRIP-TER  TO NOMBRE-MEDICO NOMBRE-MED.
            MOVE COD-TER      TO CEDULA-MEDICO.

       ASIGNAR-CLIENTE.
            MOVE NIT-FACT TO COD-TER.
            READ ARCHIVO-TERCEROS  WITH NO LOCK 
                   INVALID KEY
                   MOVE ALL "*" TO DESCRIP-TER
            END-READ

            IF   (PUC-USU = 4 OR 6)
            AND (ACT-TER = 41 OR 42 OR 43 OR 44 OR 45)
                 MOVE 31 TO ACT-TER
            END-IF.


            MOVE DESCRIP-TER  TO NOMBRE-CLIENTE NOMBRE-CLI-2.
            MOVE DESCRIP-TER  TO NOMBRE-CTA.
            MOVE COD-TER      TO CEDULA-CLIENTE CEDULA-CLI-2.
            MOVE ACT-TER      TO ACTIV-W.

            MOVE "00001"      TO AUXIL-W.

            EVALUATE PUC-USU
            WHEN 1 MOVE "130505"          TO SUBCTA-TER
            WHEN 2 MOVE "161502"          TO SUBCTA-TER
            WHEN 3 EVALUATE ACTIV-W
                   WHEN  03   MOVE "130525"  TO SUBCTA-TER
                   WHEN  04   MOVE "130525"  TO SUBCTA-TER
                   WHEN  05   MOVE "130580"  TO SUBCTA-TER
                   WHEN  06   MOVE "130506"  TO SUBCTA-TER
                   WHEN  10   MOVE "130510"  TO SUBCTA-TER
                   WHEN  15   MOVE "130515"  TO SUBCTA-TER
                   WHEN  20   MOVE "130520"  TO SUBCTA-TER
                   WHEN  25   MOVE "130525"  TO SUBCTA-TER
                   WHEN  30   MOVE "130530"  TO SUBCTA-TER
                   WHEN  35   MOVE "130535"  TO SUBCTA-TER
                   WHEN  40   MOVE "130540"  TO SUBCTA-TER
                   WHEN  45   MOVE "130545"  TO SUBCTA-TER
                   WHEN  50   MOVE "130550"  TO SUBCTA-TER
                   WHEN  55   MOVE "130505"  TO SUBCTA-TER
                   WHEN  56   MOVE "130555"  TO SUBCTA-TER
                   WHEN  57   MOVE "130506"  TO SUBCTA-TER
                   WHEN  60   MOVE "130560"  TO SUBCTA-TER
                   WHEN  65   MOVE "130565"  TO SUBCTA-TER
                   WHEN  70   MOVE "130570"  TO SUBCTA-TER
                   WHEN  75   MOVE "130575"  TO SUBCTA-TER
                   WHEN  90   MOVE "130405"  TO SUBCTA-TER
                   WHEN OTHER MOVE "130525"  TO SUBCTA-TER
                   END-EVALUATE
            WHEN 5 EVALUATE ACTIV-W
                   WHEN  03   MOVE "130525"  TO SUBCTA-TER
                   WHEN  04   MOVE "130525"  TO SUBCTA-TER
                   WHEN  05   MOVE "130580"  TO SUBCTA-TER
                   WHEN  06   MOVE "130506"  TO SUBCTA-TER
                   WHEN  10   MOVE "130510"  TO SUBCTA-TER
                   WHEN  15   MOVE "130515"  TO SUBCTA-TER
                   WHEN  20   MOVE "130520"  TO SUBCTA-TER
                   WHEN  25   MOVE "130525"  TO SUBCTA-TER
                   WHEN  30   MOVE "130530"  TO SUBCTA-TER
                   WHEN  35   MOVE "130535"  TO SUBCTA-TER
                   WHEN  40   MOVE "130540"  TO SUBCTA-TER
                   WHEN  45   MOVE "130545"  TO SUBCTA-TER
                   WHEN  50   MOVE "130550"  TO SUBCTA-TER
                   WHEN  55   MOVE "130505"  TO SUBCTA-TER
                   WHEN  56   MOVE "130555"  TO SUBCTA-TER
                   WHEN  57   MOVE "130506"  TO SUBCTA-TER
                   WHEN  60   MOVE "130560"  TO SUBCTA-TER
                   WHEN  65   MOVE "130565"  TO SUBCTA-TER
                   WHEN  70   MOVE "130570"  TO SUBCTA-TER
                   WHEN  75   MOVE "130575"  TO SUBCTA-TER
                   WHEN  90   MOVE "130405"  TO SUBCTA-TER
                   WHEN OTHER MOVE "130525"  TO SUBCTA-TER
                   END-EVALUATE
            WHEN 4 EVALUATE ACTIV-W
                   WHEN  21   MOVE "140901"  TO SUBCTA-TER
                   WHEN  22   MOVE "140902"  TO SUBCTA-TER
                   WHEN  23   MOVE "140903"  TO SUBCTA-TER
                   WHEN  24   MOVE "140904"  TO SUBCTA-TER
                   WHEN  25   MOVE "140905"  TO SUBCTA-TER
                   WHEN  26   MOVE "140906"  TO SUBCTA-TER
                   WHEN  27   MOVE "140907"  TO SUBCTA-TER
                   WHEN  28   MOVE "140915"  TO SUBCTA-TER
                   WHEN  29   MOVE "140909"  TO SUBCTA-TER
                   WHEN  30   MOVE "140910"  TO SUBCTA-TER
                   WHEN  31   MOVE "140911"  TO SUBCTA-TER
                              MOVE 00001     TO AUXIL-W
                   WHEN  32   MOVE "140912"  TO SUBCTA-TER
                   WHEN  33   MOVE "140913"  TO SUBCTA-TER
                   WHEN  34   MOVE "140914"  TO SUBCTA-TER
                   WHEN  35   MOVE "140916"  TO SUBCTA-TER
                   WHEN  36   MOVE "140916"  TO SUBCTA-TER
                   WHEN  37   MOVE "140917"  TO SUBCTA-TER
                   WHEN  38   MOVE "140911"  TO SUBCTA-TER
                              MOVE 00002     TO AUXIL-W
                   WHEN  39   MOVE "140911"  TO SUBCTA-TER
                              MOVE 00003     TO AUXIL-W
      * SALUD PUBLICA
                   WHEN  46   MOVE "140918"  TO SUBCTA-TER
                       EVALUATE NIT-NUM
                       WHEN 0000009017  MOVE 00001 TO AUXIL-W
                       WHEN 0892099233  MOVE 00002 TO AUXIL-W
                       WHEN 0832000605  MOVE 00003 TO AUXIL-W
                       WHEN 0832000219  MOVE 00004 TO AUXIL-W
                       END-EVALUATE
      * CONVENIOS DIFERENTES A VENTAS DE SALUD R4
                   WHEN  70   MOVE "147090"  TO SUBCTA-TER
                   WHEN  72   MOVE "147090"  TO SUBCTA-TER
                   			  MOVE 00002     TO AUXIL-W
      * CONVENIOS DE APORTES DIRECTOS
                   WHEN  84   MOVE "148490"  TO SUBCTA-TER
                   			  MOVE 00007     TO AUXIL-W
                   WHEN  90   MOVE "140990"  TO SUBCTA-TER
                   WHEN OTHER MOVE "140990"  TO SUBCTA-TER
                   END-EVALUATE
            WHEN 6 EVALUATE ACTIV-W
                   WHEN  21   MOVE "131901"  TO SUBCTA-TER
                   WHEN  22   MOVE "131907"  TO SUBCTA-TER
                   WHEN  23   MOVE "131903"  TO SUBCTA-TER
                   WHEN  24   MOVE "131908"  TO SUBCTA-TER
                   WHEN  25   MOVE "131905"  TO SUBCTA-TER
                   WHEN  26   MOVE "131912"  TO SUBCTA-TER
                   WHEN  27   MOVE "131916"  TO SUBCTA-TER
                              IF NIT-USU = 845000038
                              EVALUATE C-COSTO-NUM
                              WHEN 70  MOVE "00002"      TO AUXIL-W
                              WHEN 80  MOVE "00003"      TO AUXIL-W
                              END-EVALUATE
                              END-IF
                   WHEN  28   MOVE "131928"  TO SUBCTA-TER
                   WHEN  29   MOVE "131910"  TO SUBCTA-TER
                   WHEN  30   MOVE "131914"  TO SUBCTA-TER
                   WHEN  31   MOVE "131921"  TO SUBCTA-TER
                              IF  CTA-PIC-NUM IS NUMERIC
                              AND CTA-PIC-NUM > 0
                                  MOVE CTA-PIC-NUM TO AUXIL-W
                              ELSE
                                  MOVE 00001       TO AUXIL-W
                              END-IF
                   WHEN  32   MOVE "131923"  TO SUBCTA-TER
                   WHEN  33   MOVE "131929"  TO SUBCTA-TER
                   WHEN  34   MOVE "131917"  TO SUBCTA-TER
                   WHEN  35   MOVE "131925"  TO SUBCTA-TER
                   WHEN  36   MOVE "131925"  TO SUBCTA-TER
                   WHEN  37   MOVE "131930"  TO SUBCTA-TER
                   WHEN  38   IF NIT-USU = 845000038
                                 MOVE "131919"  TO SUBCTA-TER
                                 EVALUATE NIT-NUM
                                 WHEN 0000009017  MOVE 00001 TO AUXIL-W
                                 WHEN 0892099233  MOVE 00002 TO AUXIL-W
                                 WHEN 0832000605  MOVE 00003 TO AUXIL-W
                                 WHEN 0832000219  MOVE 00004 TO AUXIL-W
                                 END-EVALUATE
                              ELSE
                                MOVE "131919"  TO SUBCTA-TER
                                MOVE 00002     TO AUXIL-W
                              END-IF
                   WHEN  39   MOVE "131921"  TO SUBCTA-TER
                             EVALUATE NIT-NUM
                             WHEN 0000009017  MOVE 00003 TO AUXIL-W
                             WHEN 0892099233  MOVE 00004 TO AUXIL-W
                             WHEN 0832000605  MOVE 00005 TO AUXIL-W
                             WHEN 0832000219  MOVE 00006 TO AUXIL-W
                             END-EVALUATE
                   WHEN  46  MOVE "131919"  TO SUBCTA-TER
                             EVALUATE NIT-NUM
                             WHEN 0000009017  MOVE 00001 TO AUXIL-W
                             WHEN 0892099233  MOVE 00002 TO AUXIL-W
                             WHEN 0832000605  MOVE 00003 TO AUXIL-W
                             WHEN 0832000219  MOVE 00004 TO AUXIL-W
                             END-EVALUATE
      * CONVENIOS DIFERENTES A VENTAS DE SALUD R4
                   WHEN  70   MOVE "138490"  TO SUBCTA-TER
                   WHEN  72   MOVE "138490"  TO SUBCTA-TER
                   			  MOVE 00002     TO AUXIL-W
      * CONVENIOS DE APORTES DIRECTOS
                   WHEN  84   MOVE "138490"  TO SUBCTA-TER
                   			  MOVE 00007     TO AUXIL-W
                   WHEN  90   MOVE "131990"  TO SUBCTA-TER
                   WHEN OTHER MOVE "131990"  TO SUBCTA-TER
                   END-EVALUATE
            WHEN 7 EVALUATE ACTIV-W
                   WHEN  03   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06003"     TO AUXIL-W
                   WHEN  04   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06004"     TO AUXIL-W
                   WHEN  05   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06005"     TO AUXIL-W
                   WHEN  06   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06006"     TO AUXIL-W
                   WHEN  10   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06010"     TO AUXIL-W
                   WHEN  15   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06015"     TO AUXIL-W
                   WHEN  20   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06020"     TO AUXIL-W
                   WHEN  25   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06025"     TO AUXIL-W
                   WHEN  30   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06030"     TO AUXIL-W
                   WHEN  35   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06035"     TO AUXIL-W
                   WHEN  40   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06040"     TO AUXIL-W
                   WHEN  45   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06045"     TO AUXIL-W
                   WHEN  50   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06050"     TO AUXIL-W
                   WHEN  55   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06055"     TO AUXIL-W
                   WHEN  56   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06056"     TO AUXIL-W
                   WHEN  57   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06057"     TO AUXIL-W
                   WHEN  60   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06060"     TO AUXIL-W
                   WHEN  65   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06065"     TO AUXIL-W
                   WHEN  70   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06070"     TO AUXIL-W
                   WHEN  75   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06075"     TO AUXIL-W
                   WHEN  90   MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06090"     TO AUXIL-W
                   WHEN OTHER MOVE "130124"  TO SUBCTA-TER
                 		     MOVE  "06025"     TO AUXIL-W
                   END-EVALUATE
               END-EVALUATE.
               PERFORM TOTAL. 

        DATOS-MOVIMIENTO.
            IF PREFIJO-FACT = "E"
               EVALUATE NIT-USU
      *        IMAGENES DIAGNOSTICAS DEL LLANO
               WHEN 800156469 IF SUC-LLEGADA-W = 02
                                 MOVE "2"  TO LOTE1-MOV
                              ELSE
                                 MOVE "1"  TO LOTE1-MOV
                              END-IF
      *        DIAGNOSTICOS E IMAGENES (SEGURA)
               WHEN 830092718 IF PREFIJO-USU = "TB"
                                 MOVE 2     TO LOTE1-MOV
                              ELSE
                                 MOVE 1     TO LOTE1-MOV
                              END-IF
      *        ESE SALUD YOPAL
               WHEN 844003225 EVALUATE SUC-FACT
                              WHEN "CV"  MOVE  1  TO LOTE1-MOV
                              WHEN "CA"  MOVE  2  TO LOTE1-MOV
                              WHEN "BC"  MOVE  3  TO LOTE1-MOV
                              WHEN "PV"  MOVE  4  TO LOTE1-MOV
                              WHEN "CS"  MOVE  5  TO LOTE1-MOV
                              WHEN "HY"  MOVE  6  TO LOTE1-MOV
                              WHEN "JL"  MOVE  7  TO LOTE1-MOV
                              WHEN "TL"  MOVE  8  TO LOTE1-MOV
                              WHEN "MR"  MOVE  9  TO LOTE1-MOV
                              WHEN OTHER MOVE "a" TO LOTE1-MOV
                              END-EVALUATE
               WHEN OTHER
                  IF CONTAB-POS-USU = 1 OR 2
                     MOVE CONTAB-POS-USU TO LOTE1-MOV
                  ELSE
                     MOVE 2   TO LOTE1-MOV
                  END-IF
               END-EVALUATE
            ELSE
               MOVE 1        TO LOTE1-MOV
            END-IF.

            MOVE "F"         TO LOTE2-MOV.

            IF FECHA-FACT > 030301
               MOVE PREFIJO-LLEGADA-W   TO NRO-COMP1-W
            ELSE
               MOVE NRO-FACT2-W TO NRO-COMP1-W.
            MOVE NRO-FACT3-W    TO NRO-COMP2-W.
            MOVE NRO-COMP-W     TO COMPROB-MOV.


            MOVE LLAVE-COMP-MOV TO LLAVE-W.
            OPEN I-O MOVIMIENTO-DIARIO.

        BUSCAR-MOVIMIENTO.
            START MOVIMIENTO-DIARIO KEY = LLAVE-COMP-MOV
                  INVALID KEY GO TO FIN-BORRAR-MOV.

       BORRAR-MOV.
            READ MOVIMIENTO-DIARIO NEXT WITH NO LOCK AT END
                                   GO TO FIN-BORRAR-MOV.
            IF LLAVE-COMP-MOV IS NOT EQUAL TO LLAVE-W
                                   GO TO FIN-BORRAR-MOV.
            DELETE MOVIMIENTO-DIARIO.
            GO TO BORRAR-MOV.
       FIN-BORRAR-MOV.
            CLOSE MOVIMIENTO-DIARIO.

            IF  (PREFIJO-FACT =  "A" OR "P" OR "T" OR "B" OR "D" OR "F"
                OR "G" OR "H" OR "I" OR "J" OR "K" OR "L" OR "M" OR "N"
                OR "O" OR "Q" OR "R" OR "S" OR "V" OR "W"
                OR "X" OR "Y" OR "Z")
                AND (FECHA-FACT > 090600 OR SEPARA-CAJA-ALT = "S")
                    GO TO CERRAR-ARCHIVOS
            END-IF.

            MOVE LLAVE-W          TO LLAVE-COMP-MOV.
            MOVE NIT-FACT         TO ID-MOV.
            MOVE 1                TO SEC-NUM.
            MOVE PREFIJO-FACT     TO SUC-MOV.
            MOVE NRO-FACT         TO DOCUM-MOV.
            MOVE NRO-CTA-FACT     TO REFER-MOV.
            MOVE CL-FACT          TO TIPO-DOC-MOV
            MOVE FECHA-FACT       TO FECHA-MOV.
            MOVE OPER-ELAB-FACT   TO OPER-ELAB-MOV.
            MOVE "S.C."           TO OPER-MODIF-MOV.
            ACCEPT FECHA-MODIF-MOV FROM DATE.

            MOVE ID-HISTORIA-FACT TO COD-PACI.
            READ ARCHIVO-PACIENTES WITH NO LOCK
                 INVALID KEY 
                    EVALUATE PREFIJO-FACT
                    WHEN "E" MOVE "CONTAD"  TO DETALLE-MOV
                    WHEN "C" MOVE "CREDIT"  TO DETALLE-MOV
                    WHEN "P" MOVE "HOSPIT"  TO DETALLE-MOV
                    WHEN "A" MOVE "AMBUL."  TO DETALLE-MOV
                    WHEN "T" MOVE "A.TRAN"  TO DETALLE-MOV
                    END-EVALUATE
                  NOT INVALID KEY
                    MOVE 1ER-APEL-PACI TO DETALLE-MOV
                    INSPECT DETALLE-MOV REPLACING FIRST "  "
                                                  BY    "**"
                    INSPECT DETALLE-MOV REPLACING
                                        FIRST "               "
                                        BY     2DO-APEL-PACI
                    INSPECT DETALLE-MOV REPLACING FIRST "  "
                                                  BY    "**"
                    INSPECT DETALLE-MOV REPLACING FIRST "            "
                                                  BY     1ER-NOM-PACI
                    INSPECT DETALLE-MOV REPLACING ALL "**"
                                                  BY  "  ".
            EVALUATE CL-FACT
            WHEN 0 MOVE " DROGUERIA"   TO TIPO-L
            WHEN 1 MOVE " CIRUGIA  "   TO TIPO-L
            WHEN 2 MOVE " LABORATOR"   TO TIPO-L
            WHEN 3 MOVE " IMAGENOL."   TO TIPO-L
            WHEN 4 MOVE " OTROS SER"   TO TIPO-L
            WHEN 5 MOVE " CONSULTAS"   TO TIPO-L
            WHEN 6 MOVE " PATOLOGIA"   TO TIPO-L
            WHEN 7 MOVE " P.Y.P.   "   TO TIPO-L.
            INSPECT DETALLE-MOV REPLACING FIRST "           "
                                          BY    TIPO-L.

            IF  (PREFIJO-FACT = "A" OR "P" OR "T" OR "B" OR "D" OR "F"
                OR "G" OR "H" OR "I" OR "J" OR "K" OR "L" OR "M" OR "N"
                OR "O" OR "Q" OR "R" OR "S" OR "V" OR "W"
                OR "X" OR "Y" OR "Z")
            AND  FECHA-FACT < 060601
                 PERFORM GRABAR-ABONO
                 GO TO CERRAR-ARCHIVOS.

            PERFORM LEER-CONVENIO.

       REVISAR-CODIGO.
            IF CL-FACT > 0
               PERFORM REGISTRO-SERVICIOS VARYING I FROM 1 BY 1
                                            UNTIL I > 30
            ELSE
               PERFORM REGISTRO-ARTICULOS VARYING I FROM 1 BY 1
                                            UNTIL I > 30.

            PERFORM CONTABILIZAR-CTAS VARYING LN FROM 1 BY 1
                                      UNTIL LN > 95.


       DATO-IVA.
            MOVE COSTO-FACT       TO COSTO-MOV.

            IF VLR-IVA-FACT IS ZERO
               GO TO AUTORETENCION-CREE
            END-IF.

      * ESTA INSTRUCCION REMEDIA ERROR EN CORRECCION QUE BORRABA LOS IVAS PARCIALES
            IF  VLR-IVA1-FACT IS ZERO
            AND VLR-IVA2-FACT IS ZERO
            AND VLR-IVA3-FACT IS ZERO
                MOVE VLR-IVA-FACT TO VLR-IVA1-FACT
            END-IF.

            IF VLR-IVA1-FACT > 0
              MOVE 24080100001    TO CUENTA-MOV
              COMPUTE VALOR-MOV = VLR-IVA1-FACT * -1
              IF VALOR-MOV IS NOT EQUAL TO ZEROS
                                PERFORM GRABAR-MOVIMIENTO
              END-IF
            END-IF.


            IF VLR-IVA2-FACT > 0
              MOVE 24080100002    TO CUENTA-MOV
              COMPUTE VALOR-MOV = VLR-IVA2-FACT * -1
              IF VALOR-MOV IS NOT EQUAL TO ZEROS
                                PERFORM GRABAR-MOVIMIENTO
              END-IF
            END-IF.

            IF VLR-IVA3-FACT > 0
              MOVE 24080100003    TO CUENTA-MOV
              COMPUTE VALOR-MOV = VLR-IVA3-FACT * -1
              IF VALOR-MOV IS NOT EQUAL TO ZEROS
                                PERFORM GRABAR-MOVIMIENTO
              END-IF
            END-IF.

       AUTORETENCION-CREE.
            IF PREFIJO-FACT = "C" OR "E"
               CONTINUE
            ELSE
               GO TO TOTAL-MOV
            END-IF.


      * NO APLICA PARA UNIONES TEMPORALES NI CONSORCIOS
            MOVE NOM1-USU TO NOM-UT.

            IF RET-CREE-USU = "N"
            OR (NOM1-USU = "CONSORC" OR "UNION T")
            OR (NOM1-UT  = "UT " OR NOM1-UT = "UT." OR NOM1-UT = "U T")
            OR (NOM-UT = "U.T.")
               GO TO TOTAL-MOV
            END-IF.




      * NO APLICA PARA PERSONAS NATURALES
            IF NIT-USU < 800000000
            OR NIT-USU > 999999999
            OR NIT-USU = 844001911
            OR (PUC-USU = 2 OR 4 OR 6)
               GO TO TOTAL-MOV
            END-IF

            IF (AUTORET-USU   = "S" AND FECHA-FACT > 130500)
            OR (RETENEDOR-USU = "S" AND FECHA-FACT > 130900)
               CONTINUE
            ELSE
                GO TO TOTAL-MOV
            END-IF.

            COMPUTE BASE-CREE-W = VALOR-BRUTO + VLR-IVA-FACT
                                - VALOR-DES-FACT.


            IF (PUC-USU = 4 OR 6)
               MOVE "14225500002"   TO CUENTA-MOV
            ELSE
               IF NIT-USU = 800162035
                  MOVE "13551900002"   TO CUENTA-MOV
               ELSE
                  MOVE "13551900020"   TO CUENTA-MOV
               END-IF
            END-IF.

      *     CALL "CON008R" USING CUENTA-MOV PORC-RET-W ARCHMAE-USU.
       CON008R.
           OPEN INPUT ARCHIVO-MAESTROS.
           MOVE CUENTA-MOV TO CTA-MAE.
           MOVE 4          TO TIPO-MAE
           READ ARCHIVO-MAESTROS WITH NO LOCK 
                                 INVALID KEY 
                                    MOVE 0 TO PORCENT-RET-MAE
           END-READ.

           MOVE PORCENT-RET-MAE TO PORC-RET-W

           IF PORC-RET-W IS ZERO
              MOVE 0.6 TO PORC-RET-W
           END-IF.

      * MULTISSALUD Y COVICOOL NO CONOCEN ESTA LEY
           IF BASE-CREE-W < 127000
              IF (NIT-USU = 900716467 OR 830511298)
                   COMPUTE AUTORET-CREE-W ROUNDED =
                           BASE-CREE-W * PORC-RET-W / 100
              ELSE
                 MOVE 0 TO AUTORET-CREE-W
              END-IF
           ELSE
              COMPUTE AUTORET-CREE-W ROUNDED =
                      BASE-CREE-W * PORC-RET-W / 100
           END-IF.

           MOVE DESCRIP-TER        TO DETALLE-MOV.
           MOVE AUTORET-CREE-W     TO VALOR-MOV.
           MOVE NIT-FACT           TO ID-MOV.
           MOVE BASE-CREE-W        TO BASE-RET-MOV.
           MOVE 70 TO I.

           PERFORM GRABAR-SECUENCIA.

           IF (PUC-USU = 4 OR 6)
              MOVE "24369600002"   TO CUENTA-MOV
           ELSE
               MOVE "23657500099"   TO CUENTA-MOV
            END-IF.

           COMPUTE VALOR-MOV = VALOR-MOV * -1.
           MOVE 71 TO I.
           PERFORM GRABAR-SECUENCIA.


       TOTAL-MOV.
           IF (PREFIJO-FACT = "E" OR "C")
           OR (ACT-TER = ACT-PARTIC1 OR ACT-PARTIC2)
              EVALUATE PUC-USU
              WHEN 1 MOVE "13050500001" TO CUENTA-MOV
              WHEN 2 MOVE "16150200001" TO CUENTA-MOV
              WHEN 4 MOVE "14090700001" TO CUENTA-MOV
              WHEN 6 MOVE "13191600001" TO CUENTA-MOV
              WHEN 3 IF PREFIJO-FACT = "E" OR "C"
                        MOVE "13052500001" TO CUENTA-MOV
                     ELSE
                        MOVE "13062500001" TO CUENTA-MOV
                     END-IF
              WHEN 5 IF PREFIJO-FACT = "E" OR "C"
                        MOVE "13012500001" TO CUENTA-MOV
                     ELSE
                        MOVE "13062500001" TO CUENTA-MOV
                     END-IF
              WHEN 7 IF PREFIJO-FACT = "E" OR "C"
                        MOVE "13012406025" TO CUENTA-MOV
                     ELSE
                        MOVE "13012506025" TO CUENTA-MOV
                     END-IF
              END-EVALUATE
           ELSE
              EVALUATE PUC-USU
              WHEN 1 MOVE "13050500001" TO CUENTA-MOV
              WHEN 3 MOVE CTA-COPAGO    TO CUENTA-MOV
              WHEN 4 MOVE CTA-COPAGO    TO CUENTA-MOV
              WHEN 5 MOVE CTA-COPAGO    TO CUENTA-MOV
              WHEN 6 MOVE CTA-COPAGO    TO CUENTA-MOV
              WHEN 7 MOVE CTA-COPAGO    TO CUENTA-MOV
              END-EVALUATE
           END-IF.


            COMPUTE VALOR-MOV = VALOR-BRUTO + VLR-IVA-FACT
                              - VALOR-DES-FACT.

            IF DETALLE-FACT IS NOT EQUAL TO SPACES
               MOVE DETALLE-FACT   TO DETALLE-MOV.

            IF VALOR-MOV IS NOT EQUAL TO ZEROS
                                PERFORM GRABAR-MOVIMIENTO.

            IF PREFIJO-FACT = "E"
               GO TO CERRAR-ARCHIVOS.

            OPEN I-O ARCHIVO-MAESTROS.
            MOVE CUENTA-MOV    TO CTA-MAE.
            MOVE 4             TO TIPO-MAE.
            MOVE NOMBRE-CTA    TO NOMBRE-MAE.

            READ  ARCHIVO-MAESTROS WITH NO LOCK
                  INVALID KEY
                    WRITE REGISTRO-MAE.

            CLOSE ARCHIVO-MAESTROS.

       CERRAR-ARCHIVOS.
            CLOSE   ARCHIVO-TERCEROS
                    MAESTRO-ARTICULOS
                    ARCHIVO-TARIFAS
                    ARCHIVO-TABLAS
                    ARCHIVO-CUPS
                    ARCHIVO-GR-SER
                    ARCHIVO-ESPECIALIDAD
                    ARCHIVO-PACIENTES
                    ARCHIVO-NUMERACION
                    ARCHIVO-PROFESIONALES.

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

       GRABAR-ABONO.
            MOVE MAYOR-TER        TO MAYOR-MOV
            MOVE SCTA-TER         TO SCTA-MOV
            MOVE AUXIL-W          TO LOCAL-MOV
            COMPUTE VALOR-MOV =   COPAGO-ESTIM-FACT * -1
            PERFORM GRABAR-MOVIMIENTO.
            PERFORM DESCARGAR-CARTERA.


            MOVE CTA-CAJA         TO CUENTA-MOV
            MOVE COPAGO-ESTIM-FACT TO VALOR-MOV
            PERFORM GRABAR-MOVIMIENTO.

       DESCARGAR-CARTERA.
             MOVE 0 TO SW-BUSCAR
             PERFORM BUSCAR-ABONO-FACT VARYING J FROM 1 BY 1
                                       UNTIL J > 5 OR SW-BUSCAR > 0.
             IF SW-BUSCAR = 0
                PERFORM ASIGNAR-ABONO-FACT VARYING J FROM 1 BY 1
                                           UNTIL J > 5 OR SW-BUSCAR > 0.

             OPEN I-O FACTURA-SERVICIOS.
             REWRITE REG-FACT.
             CLOSE FACTURA-SERVICIOS.


       BUSCAR-ABONO-FACT.
            IF  ANO-MOV       =  ANO-ABON-SALUD   (J)
            AND MES-MOV       =  MES-ABON-SALUD   (J)
            AND SECUENCIA-MOV =  SECU-ABON-SALUD  (J)
                MOVE VALOR-MOV TO VLR-ABON-SALUD  (J)
                MOVE 1 TO SW-BUSCAR.

       ASIGNAR-ABONO-FACT.
            IF VLR-ABON-SALUD (J) IS ZERO
               MOVE ANO-MOV       TO ANO-ABON-SALUD  (J)
               MOVE MES-MOV       TO MES-ABON-SALUD  (J)
               MOVE SECUENCIA-MOV TO SECU-ABON-SALUD (J)
               MOVE VALOR-MOV     TO VLR-ABON-SALUD  (J)
               MOVE 1 TO SW-BUSCAR.

       LEER-CUENTA.
           MOVE PREFIJO-FACT TO PREFIJO-NUM.
           MOVE NRO-CTA-FACT TO NRO-NUM.
           READ ARCHIVO-NUMERACION WITH NO LOCK 
                INVALID KEY
                  INITIALIZE NIT-NUM CONVENIO-NUM.

           MOVE CONVENIO-NUM   TO COD-TAR.

       LEER-CONVENIO.
           READ ARCHIVO-TARIFAS WITH NO LOCK
                INVALID KEY
                  DISPLAY "ERROR NO EXISTE CONVENIO"
                  GO TO CERRAR-ARCHIVOS.

           MOVE CL-FACT                   TO TIPO-TAB-W.

           IF CL-FACT > 0
              IF CL-FACT = 7 MOVE 5       TO CL-SW
              ELSE           MOVE CL-FACT TO CL-SW END-IF
              IF TARIF-FACT = SPACES
                 MOVE COD-TAB-TAR (CL-SW)  TO COD-TAB-W
              ELSE
                 MOVE TARIF-FACT           TO COD-TAB-W
              END-IF.

       CREAR-MEDICO.
           MOVE "2335"     TO MAYOR-MAE
           IF NIT-USU = 800156469
              MOVE "95" TO SUBCTA-MAE
           ELSE
              MOVE "90" TO SUBCTA-MAE
           END-IF
           MOVE LOCAL-MOV  TO AUXILIAR-MAE

           MOVE 4             TO TIPO-MAE.
           OPEN I-O ARCHIVO-MAESTROS.
           READ  ARCHIVO-MAESTROS WITH NO LOCK
                 INVALID KEY
                   MOVE NOMBRE-MED TO NOMBRE-MAE
                   WRITE REGISTRO-MAE.

           CLOSE ARCHIVO-MAESTROS.


           IF  FECHA-NUM > 020100  AND ANO-ALFA  > 2001
                 PERFORM GRABAR-GASTO
           ELSE
               INITIALIZE VALOR-RET.

           MOVE CTA-MAE     TO CUENTA-MOV.
           MOVE NIT-FACT    TO ID-MOV.
           COMPUTE VALOR-MOV =  VLR-ACUM (LN) - VALOR-RET.
           PERFORM GRABAR-SECUENCIA.

       GRABAR-GASTO.
           EVALUATE CL-ACUM (LN)
           WHEN 1 MOVE "61201020050" TO CUENTA-MOV
           WHEN 2 MOVE "61250320050" TO CUENTA-MOV
           WHEN 3 MOVE "61250220050" TO CUENTA-MOV
           WHEN 4 MOVE "61055620050" TO CUENTA-MOV
           WHEN 5 MOVE "61105120050" TO CUENTA-MOV
           WHEN 6 MOVE "61250420050" TO CUENTA-MOV
           WHEN 7 MOVE "61055620050" TO CUENTA-MOV
           WHEN 8 MOVE "61300720050" TO CUENTA-MOV.
           MOVE MED-ACUM (LN) TO ID-MOV.
           MOVE COSTO-FACT    TO COSTO-MOV.
           COMPUTE VALOR-MOV =  VLR-ACUM (LN) * -1.

           PERFORM GRABAR-SECUENCIA.


           IF RETENEDOR-USU = "N"
              MOVE "S" TO EXENT-RET-TER
           ELSE
              MOVE ID-MOV TO COD-TER
              READ ARCHIVO-TERCEROS WITH NO LOCK
                   INVALID KEY
                     MOVE "N" TO EXENT-RET-TER
              END-READ.

           IF EXENT-RET-TER = "S"
              MOVE 0 TO VALOR-RET
           ELSE
              IF ID-MOV > 800000000
                 MOVE "23651500002" TO CUENTA-MOV
                 COMPUTE VALOR-RET ROUNDED = VALOR-MOV * -0.11
              ELSE
                 MOVE "23651500001" TO CUENTA-MOV
                 COMPUTE VALOR-RET ROUNDED = VALOR-MOV * -0.10.

           MOVE "00"      TO COSTO-MOV.
           MOVE VALOR-RET TO VALOR-MOV.
           PERFORM GRABAR-SECUENCIA.

       CAMBIAR-CLASE.
             EVALUATE GRUPO-FACT (I)
             WHEN 90 MOVE 2 TO CL-W
             WHEN 87 MOVE 3 TO CL-W
             WHEN 88 MOVE 3 TO CL-W
             WHEN 89 IF COD1-ART-FACT (I) = 8 OR 9
                        MOVE 6 TO CL-W
                     ELSE
                        IF CL-FACT = 7
                           MOVE 5 TO CL-W
                        END-IF
                     END-IF
             WHEN 93 MOVE 8 TO CL-W.


       TOTAL.
             INITIALIZE VALOR-BRUTO.
             PERFORM SUMAR-VALORES VARYING J FROM 1 BY 1
                                             UNTIL J > 30.

             COMPUTE VALOR-TOTAL =  VALOR-BRUTO - VALOR-DES-FACT.

       SUMAR-VALORES.
             ADD VLR-FACT (J)    TO VALOR-BRUTO.


       REGISTRO-SERVICIOS.
           IF  CL-FACT = 1
           AND I = 1
               PERFORM LEER-ARCHIVO-TABLAS
               PERFORM LEER-GRUPO-SERV
               MOVE PORC-W  TO PORC-CIR.

           IF VLR-FACT (I) IS NOT ZERO
              PERFORM LEER-ITEM-SER.
       LEER-ITEM-SER.
            IF   CL-FACT = 1
                 MOVE PORC-CIR TO PORC-W
                 PERFORM BUSCAR-MEDICO
            ELSE
                 PERFORM LEER-ARCHIVO-TABLAS
                 PERFORM LEER-GRUPO-SERV.


            MOVE MED-OTR-FACT TO COD-PROF.
            READ ARCHIVO-PROFESIONALES WITH NO LOCK
                 INVALID KEY
                   MOVE 1 TO CONTRATO-P
                   MOVE 0 TO PORCENT-P.


            IF PEDIDO-USU = "N"
               MOVE 1 TO CONTRATO-P
               MOVE 0 TO PORCENT-P
               MOVE "N" TO 100-MED-CUP.



            IF CONTRATO-P = 1
               MOVE 100 TO PORC-W
               MOVE 0   TO PORC-OTR-GRSER
            ELSE
               IF PORCENT-P > 0
                  COMPUTE PORC-W = 100 - PORCENT-P - PORC-OTR-GRSER
               END-IF.

            IF MED-OTR-FACT = NIT-USU
            OR PEDIDO-USU = "N"
               COMPUTE PORC-W = 100 - PORC-OTR-GRSER
               MOVE "N" TO 100-MED-CUP
            ELSE
               EVALUATE NIT-USU
               WHEN  892000401 PERFORM COND-CLINICA-META
               WHEN  900648993 PERFORM COND-CLINICA-META
               WHEN  900755133 PERFORM COND-CLINICA-META
               WHEN  900804411 PERFORM COND-CLINICA-META
               WHEN  900870633 PERFORM COND-CLINICA-META
               WHEN  800156469 PERFORM COND-IMAGENES-DIAG
               WHEN  822003323 PERFORM COND-CARDIOPULMONAR
               END-EVALUATE.

           MOVE VLR-FACT (I) TO TOT-ITEM-W.

           IF   100-MED-CUP = "S"
                COMPUTE VALOR-TEMP = VLR-FACT (I) * -1
           ELSE
                COMPUTE VALOR-APROX ROUNDED =
                        VLR-FACT (I) * PORC-W * -1
                COMPUTE VALOR-TEMP  ROUNDED = VALOR-APROX / 100
                ADD VALOR-TEMP TO TOT-ITEM-W
           END-IF.


           IF  FECHA-NUM > 020100
           AND ANO-ALFA  > 2001
               COMPUTE VALOR-TEMP = VLR-FACT (I) * -1.

           MOVE 1 TO SW-GRP.
           MOVE 0 TO CL-W.
           PERFORM TOTALES-CONTROL.

           MOVE CL-FACT TO CL-W.
           IF CL-W > 0
              PERFORM CAMBIAR-CLASE.

           IF PORC-OTR-GRSER > 0
              MOVE 2 TO SW-GRP
              MOVE PORC-OTR-GRSER TO PORC-W
              MOVE OTR-CTA-GRSER  TO CTA-CONTAB-TAB
              COMPUTE VALOR-APROX ROUNDED =
                      VLR-FACT (I) * PORC-W * -1
              COMPUTE VALOR-TEMP  ROUNDED =
                      VALOR-APROX / 100
              ADD VALOR-TEMP TO TOT-ITEM-W
              PERFORM TOTALES-CONTROL.

            IF TOT-ITEM-W IS NOT ZERO
               MOVE 3           TO SW-GRP
               MOVE MAY-MEDI    TO MAY-CONTAB-TAB
               MOVE SCTA-MEDI   TO SCT-CONTAB-TAB
               MOVE "XXXXX"     TO AUX-CONTAB-TAB
               INSPECT AUX-CONTAB-TAB REPLACING
                       FIRST "XXXXX" BY COD-MEDICO
               COMPUTE VALOR-TEMP = TOT-ITEM-W * -1
               PERFORM TOTALES-CONTROL.


       LEER-ARCHIVO-TABLAS.
           IF  ANO-FACT > 01 AND < 90
               MOVE ART-FACT (I) TO LLAVE-CUP
               IF  GRUPO-FACT (I) = "XX"
               OR (NIT-USU = 892000401 OR 900648993 OR 900755133
                          OR 900804411 OR 900870633)
                  CONTINUE
               ELSE
                  MOVE SPACES TO NRO3-CUP
               END-IF
               READ ARCHIVO-CUPS WITH NO LOCK
                    INVALID KEY
                        INITIALIZE CTA-CONTAB-TAB PORCENT-CUP
                                   NIT-OTR-CUP 100-MED-CUP
                    NOT INVALID KEY
                        EVALUATE PUC-USU
                        WHEN 1 MOVE CTA-CONTAB-CUP (1) TO CTA-CONTAB-TAB
                        WHEN 2 MOVE CTA-CONTAB-CUP (2) TO CTA-CONTAB-TAB
                        WHEN 3 MOVE CTA-CONTAB-CUP (1) TO CTA-CONTAB-TAB
                        WHEN 4 MOVE CTA-CONTAB-CUP (3) TO CTA-CONTAB-TAB
                        WHEN 5 MOVE CTA-CONTAB-CUP (3) TO CTA-CONTAB-TAB
                        WHEN 6 MOVE CTA-CONTAB-CUP (3) TO CTA-CONTAB-TAB
                        WHEN 7 MOVE CTA-CONTAB-CUP (1) TO CTA-CONTAB-TAB
                        END-EVALUATE
              END-READ
           ELSE
              MOVE LLAVE-TIPO-W   TO LLAVE-TIPO-TAB
              MOVE ART-FACT  (I)  TO COD-SER-TAB
              READ ARCHIVO-TABLAS WITH NO LOCK
                   INVALID KEY
                     INITIALIZE CTA-CONTAB-TAB PORCENT-CUP
              END-READ
           END-IF.

            IF CTA-CONTAB-TAB = SPACES
               MOVE "4105"   TO MAY-CONTAB-TAB
               MOVE NRO-FACT TO COD-SCTA-TAB.




      ***  RECLASIFICAR UCI PUBLICO.
           IF  MAY-CONTAB-TAB = 4312
           AND SCT-CONTAB-TAB = 27
               EVALUATE UNSERV1-FACT
               WHEN 1 MOVE 30      TO SCT-CONTAB-TAB
                      MOVE "000XX" TO AUX-CONTAB-TAB
                      INSPECT AUX-CONTAB-TAB REPLACING FIRST "XX"
                                                      BY UNSERV-FACT
               WHEN 2 IF UNSERV-FACT = 21
                         MOVE 28   TO SCT-CONTAB-TAB
                      ELSE
                         MOVE 29   TO SCT-CONTAB-TAB
                      END-IF
                      MOVE "000XX" TO AUX-CONTAB-TAB
                      INSPECT AUX-CONTAB-TAB REPLACING FIRST "XX"
                                                      BY UNSERV-FACT
               WHEN 3 IF UNSERV-FACT = 31
                         MOVE 28   TO SCT-CONTAB-TAB
                      ELSE
                         MOVE 29   TO SCT-CONTAB-TAB
                      END-IF
                      MOVE "000XX" TO AUX-CONTAB-TAB
                      INSPECT AUX-CONTAB-TAB REPLACING FIRST "XX"
                                                       BY UNSERV-FACT
              WHEN 5 MOVE 31      TO SCT-CONTAB-TAB
                     MOVE "000XX" TO AUX-CONTAB-TAB
                     INSPECT AUX-CONTAB-TAB REPLACING FIRST "XX"
                                                      BY UNSERV-FACT.



      ***  RECLASIFICAR CONSULTA EXTERNA PRIVADO.
           MOVE CTA-CONTAB-TAB TO CTA-TAB-W.


           IF  SUBCTA-TAB-W = "411027"
               EVALUATE ESPEC-ELAB
               WHEN 020 MOVE 02   TO SCT-CONTAB-TAB
               WHEN 021 MOVE 02   TO SCT-CONTAB-TAB
               WHEN 022 MOVE 02   TO SCT-CONTAB-TAB
               WHEN 023 MOVE 02   TO SCT-CONTAB-TAB
               WHEN 047 MOVE 19   TO SCT-CONTAB-TAB
               WHEN 049 MOVE 45   TO SCT-CONTAB-TAB
               WHEN 053 MOVE 30   TO SCT-CONTAB-TAB
               WHEN 054 MOVE 49   TO SCT-CONTAB-TAB
               WHEN 056 MOVE 32   TO SCT-CONTAB-TAB
               WHEN 057 MOVE 32   TO SCT-CONTAB-TAB
               WHEN 058 MOVE 37   TO SCT-CONTAB-TAB
               WHEN 120 MOVE 03   TO SCT-CONTAB-TAB
               WHEN 121 MOVE 03   TO SCT-CONTAB-TAB
               WHEN 122 MOVE 03   TO SCT-CONTAB-TAB
               WHEN 130 MOVE 04   TO SCT-CONTAB-TAB
               WHEN 131 MOVE 05   TO SCT-CONTAB-TAB
               WHEN 132 MOVE 11   TO SCT-CONTAB-TAB
               WHEN 133 MOVE 06   TO SCT-CONTAB-TAB
               WHEN 134 MOVE 10   TO SCT-CONTAB-TAB
               WHEN 135 MOVE 12   TO SCT-CONTAB-TAB
               WHEN 136 MOVE 17   TO SCT-CONTAB-TAB
               WHEN 137 MOVE 02   TO SCT-CONTAB-TAB
               WHEN 138 MOVE 07   TO SCT-CONTAB-TAB
               WHEN 139 MOVE 08   TO SCT-CONTAB-TAB
               WHEN 140 MOVE 07   TO SCT-CONTAB-TAB
               WHEN 141 MOVE 39   TO SCT-CONTAB-TAB
               WHEN 142 MOVE 42   TO SCT-CONTAB-TAB
               WHEN 143 MOVE 09   TO SCT-CONTAB-TAB
               WHEN 144 MOVE 09   TO SCT-CONTAB-TAB
               WHEN 145 MOVE 09   TO SCT-CONTAB-TAB
               WHEN 146 MOVE 09   TO SCT-CONTAB-TAB
               WHEN 147 MOVE 09   TO SCT-CONTAB-TAB
               WHEN 148 MOVE 04   TO SCT-CONTAB-TAB
               WHEN 149 MOVE 07   TO SCT-CONTAB-TAB
               WHEN 160 MOVE 43   TO SCT-CONTAB-TAB
               WHEN 180 MOVE 36   TO SCT-CONTAB-TAB
               WHEN 195 MOVE 31   TO SCT-CONTAB-TAB
               WHEN 200 MOVE 12   TO SCT-CONTAB-TAB
               WHEN 201 MOVE 12   TO SCT-CONTAB-TAB
               WHEN 220 MOVE 35   TO SCT-CONTAB-TAB
               WHEN 240 MOVE 13   TO SCT-CONTAB-TAB
               WHEN 250 MOVE 35   TO SCT-CONTAB-TAB
               WHEN 280 MOVE 14   TO SCT-CONTAB-TAB
               WHEN 281 MOVE 14   TO SCT-CONTAB-TAB
               WHEN 282 MOVE 14   TO SCT-CONTAB-TAB
               WHEN 310 MOVE 17   TO SCT-CONTAB-TAB
               WHEN 311 MOVE 17   TO SCT-CONTAB-TAB
               WHEN 320 MOVE 19   TO SCT-CONTAB-TAB
               WHEN 330 MOVE 19   TO SCT-CONTAB-TAB
               WHEN 331 MOVE 19   TO SCT-CONTAB-TAB
               WHEN 340 MOVE 20   TO SCT-CONTAB-TAB
               WHEN 341 MOVE 20   TO SCT-CONTAB-TAB
               WHEN 350 MOVE 22   TO SCT-CONTAB-TAB
               WHEN 351 MOVE 22   TO SCT-CONTAB-TAB
               WHEN 352 MOVE 22   TO SCT-CONTAB-TAB
               WHEN 370 MOVE 07   TO SCT-CONTAB-TAB
               WHEN 371 MOVE 07   TO SCT-CONTAB-TAB
               WHEN 387 MOVE 28   TO SCT-CONTAB-TAB
               WHEN 388 MOVE 29   TO SCT-CONTAB-TAB
               WHEN 410 MOVE 30   TO SCT-CONTAB-TAB
               WHEN 420 MOVE 42   TO SCT-CONTAB-TAB
               WHEN 430 MOVE 31   TO SCT-CONTAB-TAB
               WHEN 431 MOVE 31   TO SCT-CONTAB-TAB
               WHEN 440 MOVE 32   TO SCT-CONTAB-TAB
               WHEN 441 MOVE 32   TO SCT-CONTAB-TAB
               WHEN 442 MOVE 32   TO SCT-CONTAB-TAB
               WHEN 443 MOVE 32   TO SCT-CONTAB-TAB
               WHEN 450 MOVE 33   TO SCT-CONTAB-TAB
               WHEN 451 MOVE 33   TO SCT-CONTAB-TAB
               WHEN 460 MOVE 35   TO SCT-CONTAB-TAB
               WHEN 461 MOVE 35   TO SCT-CONTAB-TAB
               WHEN 462 MOVE 35   TO SCT-CONTAB-TAB
               WHEN 463 MOVE 35   TO SCT-CONTAB-TAB
               WHEN 464 MOVE 35   TO SCT-CONTAB-TAB
               WHEN 480 MOVE 36   TO SCT-CONTAB-TAB
               WHEN 481 MOVE 36   TO SCT-CONTAB-TAB
               WHEN 490 MOVE 37   TO SCT-CONTAB-TAB
               WHEN 491 MOVE 37   TO SCT-CONTAB-TAB
               WHEN 492 MOVE 37   TO SCT-CONTAB-TAB
               WHEN 500 MOVE 38   TO SCT-CONTAB-TAB
               WHEN 510 MOVE 35   TO SCT-CONTAB-TAB
               WHEN 511 MOVE 39   TO SCT-CONTAB-TAB
               WHEN 512 MOVE 39   TO SCT-CONTAB-TAB
               WHEN 513 MOVE 39   TO SCT-CONTAB-TAB
               WHEN 514 MOVE 39   TO SCT-CONTAB-TAB
               WHEN 520 MOVE 41   TO SCT-CONTAB-TAB
               WHEN 521 MOVE 41   TO SCT-CONTAB-TAB
               WHEN 522 MOVE 41   TO SCT-CONTAB-TAB
               WHEN 550 MOVE 42   TO SCT-CONTAB-TAB
               WHEN 551 MOVE 42   TO SCT-CONTAB-TAB
               WHEN 560 MOVE 35   TO SCT-CONTAB-TAB
               WHEN 561 MOVE 35   TO SCT-CONTAB-TAB
               WHEN 562 MOVE 35   TO SCT-CONTAB-TAB
               WHEN 563 MOVE 35   TO SCT-CONTAB-TAB
               WHEN 570 MOVE 35   TO SCT-CONTAB-TAB
               WHEN 571 MOVE 35   TO SCT-CONTAB-TAB
               WHEN 580 MOVE 35   TO SCT-CONTAB-TAB
               WHEN 590 MOVE 45   TO SCT-CONTAB-TAB
               WHEN 620 MOVE 47   TO SCT-CONTAB-TAB
               WHEN 630 MOVE 44   TO SCT-CONTAB-TAB
               WHEN 750 MOVE 49   TO SCT-CONTAB-TAB
               WHEN 751 MOVE 49   TO SCT-CONTAB-TAB
               WHEN 760 MOVE 18   TO SCT-CONTAB-TAB
               END-EVALUATE
            ELSE
               IF CTA-TAB-W = 41100500021
                  EVALUATE UNSERV-FACT
                  WHEN 01 MOVE 4105    TO MAY-CONTAB-TAB
                          MOVE 05      TO SCT-CONTAB-TAB
                          MOVE "00EEE" TO AUX-CONTAB-TAB
                  WHEN 02 MOVE 4110    TO MAY-CONTAB-TAB
                          MOVE 05      TO SCT-CONTAB-TAB
                          MOVE "00EEE" TO AUX-CONTAB-TAB
                  WHEN 03 MOVE 4115    TO MAY-CONTAB-TAB
                          MOVE 05      TO SCT-CONTAB-TAB
                          MOVE "00EEE" TO AUX-CONTAB-TAB
                  WHEN 04 MOVE 4120    TO MAY-CONTAB-TAB
                          MOVE 05      TO SCT-CONTAB-TAB
                          MOVE "00EEE" TO AUX-CONTAB-TAB
                  WHEN 05 MOVE 4110    TO MAY-CONTAB-TAB
                          MOVE 05      TO SCT-CONTAB-TAB
                          MOVE "00EEE" TO AUX-CONTAB-TAB
                  WHEN 08 MOVE 4110    TO MAY-CONTAB-TAB
                          MOVE 05      TO SCT-CONTAB-TAB
                          MOVE "00EEE" TO AUX-CONTAB-TAB
                  WHEN 11 MOVE 4115    TO MAY-CONTAB-TAB
                          MOVE 35      TO SCT-CONTAB-TAB
                          MOVE "10EEE" TO AUX-CONTAB-TAB
                  WHEN 12 MOVE 4115    TO MAY-CONTAB-TAB
                          MOVE 35      TO SCT-CONTAB-TAB
                          MOVE "10EEE" TO AUX-CONTAB-TAB
                  WHEN 13 MOVE 4115    TO MAY-CONTAB-TAB
                          MOVE 35      TO SCT-CONTAB-TAB
                          MOVE "10EEE" TO AUX-CONTAB-TAB
                  WHEN 21 MOVE 4115    TO MAY-CONTAB-TAB
                          MOVE 30      TO SCT-CONTAB-TAB
                          MOVE "10EEE" TO AUX-CONTAB-TAB
                  WHEN 22 MOVE 4115    TO MAY-CONTAB-TAB
                          MOVE 30      TO SCT-CONTAB-TAB
                          MOVE "10EEE" TO AUX-CONTAB-TAB
                  WHEN 31 MOVE 4115    TO MAY-CONTAB-TAB
                          MOVE 25      TO SCT-CONTAB-TAB
                          MOVE "10EEE" TO AUX-CONTAB-TAB
                  WHEN 32 MOVE 4115    TO MAY-CONTAB-TAB
                          MOVE 25      TO SCT-CONTAB-TAB
                          MOVE "10EEE" TO AUX-CONTAB-TAB
                  WHEN 41 MOVE 4115    TO MAY-CONTAB-TAB
                          MOVE 05      TO SCT-CONTAB-TAB
                          MOVE "00EEE" TO AUX-CONTAB-TAB
                  WHEN 42 MOVE 4110    TO MAY-CONTAB-TAB
                          MOVE 05      TO SCT-CONTAB-TAB
                          MOVE "00EEE" TO AUX-CONTAB-TAB
                  WHEN 51 MOVE 4120    TO MAY-CONTAB-TAB
                          MOVE 05      TO SCT-CONTAB-TAB
                          MOVE "00EEE" TO AUX-CONTAB-TAB
                  WHEN 52 MOVE 4110    TO MAY-CONTAB-TAB
                          MOVE 05      TO SCT-CONTAB-TAB
                          MOVE "00EEE" TO AUX-CONTAB-TAB
                  WHEN 53 MOVE 4115    TO MAY-CONTAB-TAB
                          MOVE 05      TO SCT-CONTAB-TAB
                          MOVE "00EEE" TO AUX-CONTAB-TAB
                  WHEN 54 MOVE 4120    TO MAY-CONTAB-TAB
                          MOVE 05      TO SCT-CONTAB-TAB
                          MOVE "00EEE" TO AUX-CONTAB-TAB
                  WHEN 55 MOVE 4105    TO MAY-CONTAB-TAB
                          MOVE 05      TO SCT-CONTAB-TAB
                          MOVE "00EEE" TO AUX-CONTAB-TAB
                  WHEN 56 MOVE 4110    TO MAY-CONTAB-TAB
                          MOVE 05      TO SCT-CONTAB-TAB
                          MOVE "00EEE" TO AUX-CONTAB-TAB
                  WHEN OTHER
                          MOVE 4110    TO MAY-CONTAB-TAB
                          MOVE 05      TO SCT-CONTAB-TAB
                          MOVE "00EEE" TO AUX-CONTAB-TAB
                  END-EVALUATE
                  INSPECT AUX-CONTAB-TAB REPLACING FIRST "EEE"
                                                   BY ESPEC-ELAB
               ELSE
                  IF CTA-TAB-W = 41201000021
                     EVALUATE UNSERV-FACT
                     WHEN 01 MOVE 4105    TO MAY-CONTAB-TAB
                             MOVE 10      TO SCT-CONTAB-TAB
                             MOVE "00EEE" TO AUX-CONTAB-TAB
                     WHEN 02 MOVE 4110    TO MAY-CONTAB-TAB
                             MOVE 10      TO SCT-CONTAB-TAB
                             MOVE "00EEE" TO AUX-CONTAB-TAB
                     WHEN 03 MOVE 4115    TO MAY-CONTAB-TAB
                             MOVE 10      TO SCT-CONTAB-TAB
                             MOVE "00EEE" TO AUX-CONTAB-TAB
                     WHEN 04 MOVE 4120    TO MAY-CONTAB-TAB
                             MOVE 10      TO SCT-CONTAB-TAB
                             MOVE "00EEE" TO AUX-CONTAB-TAB
                     WHEN 05 MOVE 4110    TO MAY-CONTAB-TAB
                             MOVE 10      TO SCT-CONTAB-TAB
                             MOVE "00EEE" TO AUX-CONTAB-TAB
                     WHEN 08 MOVE 4110    TO MAY-CONTAB-TAB
                             MOVE 10      TO SCT-CONTAB-TAB
                             MOVE "00EEE" TO AUX-CONTAB-TAB
                     WHEN 11 MOVE 4115    TO MAY-CONTAB-TAB
                             MOVE 35      TO SCT-CONTAB-TAB
                             MOVE "20EEE" TO AUX-CONTAB-TAB
                     WHEN 12 MOVE 4115    TO MAY-CONTAB-TAB
                             MOVE 35      TO SCT-CONTAB-TAB
                             MOVE "20EEE" TO AUX-CONTAB-TAB
                     WHEN 13 MOVE 4115    TO MAY-CONTAB-TAB
                             MOVE 35      TO SCT-CONTAB-TAB
                             MOVE "20EEE" TO AUX-CONTAB-TAB
                     WHEN 21 MOVE 4115    TO MAY-CONTAB-TAB
                             MOVE 30      TO SCT-CONTAB-TAB
                             MOVE "20EEE" TO AUX-CONTAB-TAB
                     WHEN 22 MOVE 4115    TO MAY-CONTAB-TAB
                             MOVE 30      TO SCT-CONTAB-TAB
                             MOVE "20EEE" TO AUX-CONTAB-TAB
                     WHEN 31 MOVE 4115    TO MAY-CONTAB-TAB
                             MOVE 25      TO SCT-CONTAB-TAB
                             MOVE "20EEE" TO AUX-CONTAB-TAB
                     WHEN 32 MOVE 4115    TO MAY-CONTAB-TAB
                             MOVE 25      TO SCT-CONTAB-TAB
                             MOVE "20EEE" TO AUX-CONTAB-TAB
                     WHEN 41 MOVE 4115    TO MAY-CONTAB-TAB
                             MOVE 10      TO SCT-CONTAB-TAB
                             MOVE "20EEE" TO AUX-CONTAB-TAB
                     WHEN 42 MOVE 4110    TO MAY-CONTAB-TAB
                             MOVE 10      TO SCT-CONTAB-TAB
                             MOVE "00EEE" TO AUX-CONTAB-TAB
                     WHEN 51 MOVE 4120    TO MAY-CONTAB-TAB
                             MOVE 10      TO SCT-CONTAB-TAB
                             MOVE "00EEE" TO AUX-CONTAB-TAB
                     WHEN 52 MOVE 4110    TO MAY-CONTAB-TAB
                             MOVE 10      TO SCT-CONTAB-TAB
                             MOVE "00EEE" TO AUX-CONTAB-TAB
                     WHEN 53 MOVE 4115    TO MAY-CONTAB-TAB
                             MOVE 10      TO SCT-CONTAB-TAB
                             MOVE "00EEE" TO AUX-CONTAB-TAB
                     WHEN 54 MOVE 4120    TO MAY-CONTAB-TAB
                             MOVE 10      TO SCT-CONTAB-TAB
                             MOVE "00EEE" TO AUX-CONTAB-TAB
                     WHEN 55 MOVE 4105    TO MAY-CONTAB-TAB
                             MOVE 10      TO SCT-CONTAB-TAB
                             MOVE "00EEE" TO AUX-CONTAB-TAB
                     WHEN 56 MOVE 4110    TO MAY-CONTAB-TAB
                             MOVE 10      TO SCT-CONTAB-TAB
                             MOVE "00EEE" TO AUX-CONTAB-TAB
                     WHEN OTHER
                             MOVE 4110    TO MAY-CONTAB-TAB
                             MOVE 10      TO SCT-CONTAB-TAB
                             MOVE "00EEE" TO AUX-CONTAB-TAB
                     END-EVALUATE
                     INSPECT AUX-CONTAB-TAB REPLACING FIRST "EEE"
                                                      BY ESPEC-ELAB
                  END-IF
               END-IF
            END-IF.

      ***  RECLASIFICAR UCI HOSPITAL GRANADA.
            IF  NIT-USU = 800037021
            AND OPER-ELAB1-FACT = "UCI"
            AND GRUPO-FACT (I) IS NOT = "S1"
                MOVE "431228000XX"  TO CTA-CONTAB-TAB
                IF GRUPO-FACT (I) < 87
                   INSPECT CTA-CONTAB-TAB REPLACING FIRST "XX"
                                                       BY "80"
                ELSE
                   INSPECT CTA-CONTAB-TAB REPLACING FIRST "XX"
                                                       BY GRUPO-FACT (I)
                END-IF
             END-IF.


      ***  RECLASIFICAR IMAGENOLOGIA HOSPITAL GRANADA.
            IF  NIT-USU = 800037021
            AND OPER-ELAB1-FACT = "IMA"
                MOVE "431247000XX"  TO CTA-CONTAB-TAB
                INSPECT CTA-CONTAB-TAB REPLACING FIRST "XX"
                                                 BY GRUPO-FACT (I)
            END-IF.



      **   RECLASIFICAR PYP.
            IF  MAY-CONTAB-TAB = "4312"
            AND SCT-CONTAB-TAB = "17"
            AND CL-FACT = 7
                MOVE "20" TO SCT-CONTAB-TAB
            END-IF.





       LEER-GRUPO-SERV.
            MOVE GRUPO-FACT (I) TO CODIGO-GRSER
            READ ARCHIVO-GR-SER WITH NO LOCK
                 INVALID KEY
                   MOVE 100 TO PORC-CL-GRSER
                   MOVE   0 TO PORC-OTR-GRSER.

            IF  PORC-CL-CUP   > 0
            OR  PORC-OTR-CUP  > 0
            OR  100-MED-CUP   = "S"
                MOVE PORC-CL-CUP   TO PORC-CL-GRSER
                MOVE PORC-OTR-CUP  TO PORC-OTR-GRSER
                MOVE CTA-OTR-CUP   TO OTR-CTA-GRSER.


            MOVE PORC-CL-GRSER TO PORC-W.


       TOTALES-CONTROL.
             MOVE 0 TO SW0.
             IF VALOR-TEMP IS NOT ZERO
                PERFORM ASIGNAR-VALOR VARYING LN FROM 1 BY 1
                                      UNTIL LN > 95 OR SW0 = 1.
             IF  SW0 = 0
             AND VALOR-TEMP IS NOT EQUAL TO 0
                 ADD 1 TO ITEM
                 MOVE CL-W             TO CL-ACUM     (ITEM)
                 MOVE CTA-CONTAB-TAB   TO CODIGO-ACUM (ITEM)
                 MOVE COSTO-FACT       TO COSTO-ACUM  (ITEM)
                 MOVE VALOR-TEMP       TO VLR-ACUM    (ITEM)
                 EVALUATE SW-GRP
                 WHEN 1 MOVE 0            TO MED-ACUM (ITEM)
                 WHEN 2 MOVE NIT-OTR-CUP  TO MED-ACUM (ITEM)
                 WHEN 3 MOVE MED-OTR-FACT TO MED-ACUM (ITEM)
                 END-EVALUATE.

             MOVE 0 TO SW0.

       ASIGNAR-VALOR.
             IF  CTA-CONTAB-TAB = CODIGO-ACUM (LN)
             AND CL-W           = CL-ACUM     (LN)
             AND COSTO-FACT     = COSTO-ACUM  (LN)
                 ADD VALOR-TEMP   TO VLR-ACUM (LN)
                 MOVE 1 TO SW0.


       BUSCAR-MEDICO.
             EVALUATE I
             WHEN 2 MOVE MED-CIR-FACT  TO MED-OTR-FACT
                    EVALUATE PUC-USU
                    WHEN 3 IF (NIT-USU = 892000401 OR 900648993
                                      OR 900804411 OR 900755133
                                      OR 900870633)
                              MOVE "41201000EEE" TO CTA-CONTAB-TAB
                              INSPECT CTA-CONTAB-TAB REPLACING
                                                     FIRST "EEE"
                                                     BY ESPEC-ELAB
                              EVALUATE UNSERV-FACT
                              WHEN 01 MOVE 4105    TO MAY-CONTAB-TAB
                              WHEN 02 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 03 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 04 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 05 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 08 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 11 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 12 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 13 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 21 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 30      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 22 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 30      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 31 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 25      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 32 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 25      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 41 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 42 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 51 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 52 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 53 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 54 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 55 MOVE 4105    TO MAY-CONTAB-TAB
                              WHEN 56 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN OTHER
                                    MOVE 4120    TO MAY-CONTAB-TAB
                              END-EVALUATE
                              INSPECT AUX-CONTAB-TAB REPLACING
                                      FIRST "EEE" BY ESPEC-ELAB

                           ELSE
                             MOVE "41200200050" TO CTA-CONTAB-TAB
                             EVALUATE ESPEC-ELAB
                             WHEN 020 MOVE 00002 TO AUX-CONTAB-TAB
                             WHEN 021 MOVE 00002 TO AUX-CONTAB-TAB
                             WHEN 022 MOVE 00002 TO AUX-CONTAB-TAB
                             WHEN 023 MOVE 00002 TO AUX-CONTAB-TAB
                             WHEN 047 MOVE 00019 TO AUX-CONTAB-TAB
                             WHEN 049 MOVE 00045 TO AUX-CONTAB-TAB
                             WHEN 053 MOVE 00030 TO AUX-CONTAB-TAB
                             WHEN 054 MOVE 00049 TO AUX-CONTAB-TAB
                             WHEN 056 MOVE 00032 TO AUX-CONTAB-TAB
                             WHEN 057 MOVE 00032 TO AUX-CONTAB-TAB
                             WHEN 058 MOVE 00037 TO AUX-CONTAB-TAB
                             WHEN 120 MOVE 00003 TO AUX-CONTAB-TAB
                             WHEN 121 MOVE 00003 TO AUX-CONTAB-TAB
                             WHEN 122 MOVE 00003 TO AUX-CONTAB-TAB
                             WHEN 130 MOVE 00004 TO AUX-CONTAB-TAB
                             WHEN 131 MOVE 00005 TO AUX-CONTAB-TAB
                             WHEN 132 MOVE 00011 TO AUX-CONTAB-TAB
                             WHEN 133 MOVE 00006 TO AUX-CONTAB-TAB
                             WHEN 134 MOVE 00010 TO AUX-CONTAB-TAB
                             WHEN 135 MOVE 00012 TO AUX-CONTAB-TAB
                             WHEN 136 MOVE 00017 TO AUX-CONTAB-TAB
                             WHEN 137 MOVE 00002 TO AUX-CONTAB-TAB
                             WHEN 138 MOVE 00007 TO AUX-CONTAB-TAB
                             WHEN 139 MOVE 00008 TO AUX-CONTAB-TAB
                             WHEN 140 MOVE 00007 TO AUX-CONTAB-TAB
                             WHEN 141 MOVE 00039 TO AUX-CONTAB-TAB
                             WHEN 142 MOVE 00042 TO AUX-CONTAB-TAB
                             WHEN 143 MOVE 00009 TO AUX-CONTAB-TAB
                             WHEN 144 MOVE 00009 TO AUX-CONTAB-TAB
                             WHEN 145 MOVE 00009 TO AUX-CONTAB-TAB
                             WHEN 146 MOVE 00009 TO AUX-CONTAB-TAB
                             WHEN 147 MOVE 00009 TO AUX-CONTAB-TAB
                             WHEN 148 MOVE 00004 TO AUX-CONTAB-TAB
                             WHEN 149 MOVE 00007 TO AUX-CONTAB-TAB
                             WHEN 160 MOVE 00043 TO AUX-CONTAB-TAB
                             WHEN 180 MOVE 00036 TO AUX-CONTAB-TAB
                             WHEN 195 MOVE 00031 TO AUX-CONTAB-TAB
                             WHEN 200 MOVE 00012 TO AUX-CONTAB-TAB
                             WHEN 201 MOVE 00012 TO AUX-CONTAB-TAB
                             WHEN 220 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 240 MOVE 00013 TO AUX-CONTAB-TAB
                             WHEN 250 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 280 MOVE 00014 TO AUX-CONTAB-TAB
                             WHEN 281 MOVE 00014 TO AUX-CONTAB-TAB
                             WHEN 282 MOVE 00014 TO AUX-CONTAB-TAB
                             WHEN 310 MOVE 00017 TO AUX-CONTAB-TAB
                             WHEN 311 MOVE 00017 TO AUX-CONTAB-TAB
                             WHEN 320 MOVE 00019 TO AUX-CONTAB-TAB
                             WHEN 330 MOVE 00019 TO AUX-CONTAB-TAB
                             WHEN 331 MOVE 00019 TO AUX-CONTAB-TAB
                             WHEN 340 MOVE 00020 TO AUX-CONTAB-TAB
                             WHEN 341 MOVE 00020 TO AUX-CONTAB-TAB
                             WHEN 350 MOVE 00022 TO AUX-CONTAB-TAB
                             WHEN 351 MOVE 00022 TO AUX-CONTAB-TAB
                             WHEN 352 MOVE 00022 TO AUX-CONTAB-TAB
                             WHEN 370 MOVE 00007 TO AUX-CONTAB-TAB
                             WHEN 371 MOVE 00007 TO AUX-CONTAB-TAB
                             WHEN 387 MOVE 00028 TO AUX-CONTAB-TAB
                             WHEN 388 MOVE 00029 TO AUX-CONTAB-TAB
                             WHEN 410 MOVE 00030 TO AUX-CONTAB-TAB
                             WHEN 420 MOVE 00042 TO AUX-CONTAB-TAB
                             WHEN 430 MOVE 00031 TO AUX-CONTAB-TAB
                             WHEN 431 MOVE 00031 TO AUX-CONTAB-TAB
                             WHEN 440 MOVE 00032 TO AUX-CONTAB-TAB
                             WHEN 441 MOVE 00032 TO AUX-CONTAB-TAB
                             WHEN 442 MOVE 00032 TO AUX-CONTAB-TAB
                             WHEN 443 MOVE 00032 TO AUX-CONTAB-TAB
                             WHEN 450 MOVE 00033 TO AUX-CONTAB-TAB
                             WHEN 451 MOVE 00033 TO AUX-CONTAB-TAB
                             WHEN 460 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 461 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 462 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 463 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 464 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 480 MOVE 00036 TO AUX-CONTAB-TAB
                             WHEN 481 MOVE 00036 TO AUX-CONTAB-TAB
                             WHEN 490 MOVE 00037 TO AUX-CONTAB-TAB
                             WHEN 491 MOVE 00037 TO AUX-CONTAB-TAB
                             WHEN 492 MOVE 00037 TO AUX-CONTAB-TAB
                             WHEN 500 MOVE 00038 TO AUX-CONTAB-TAB
                             WHEN 510 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 511 MOVE 00039 TO AUX-CONTAB-TAB
                             WHEN 512 MOVE 00039 TO AUX-CONTAB-TAB
                             WHEN 513 MOVE 00039 TO AUX-CONTAB-TAB
                             WHEN 514 MOVE 00039 TO AUX-CONTAB-TAB
                             WHEN 520 MOVE 00041 TO AUX-CONTAB-TAB
                             WHEN 521 MOVE 00041 TO AUX-CONTAB-TAB
                             WHEN 522 MOVE 00041 TO AUX-CONTAB-TAB
                             WHEN 550 MOVE 00042 TO AUX-CONTAB-TAB
                             WHEN 551 MOVE 00042 TO AUX-CONTAB-TAB
                             WHEN 560 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 561 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 562 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 563 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 570 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 571 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 580 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 590 MOVE 00045 TO AUX-CONTAB-TAB
                             WHEN 620 MOVE 00047 TO AUX-CONTAB-TAB
                             WHEN 630 MOVE 00044 TO AUX-CONTAB-TAB
                             WHEN 750 MOVE 00049 TO AUX-CONTAB-TAB
                             WHEN 751 MOVE 00049 TO AUX-CONTAB-TAB
                             WHEN 760 MOVE 00018 TO AUX-CONTAB-TAB
                             END-EVALUATE
                           END-IF
                    WHEN 5 IF (NIT-USU = 892000401 OR 900648993
                                      OR 900804411 OR 900755133
                                      OR 900870633)
                              MOVE "41201000EEE" TO CTA-CONTAB-TAB
                              INSPECT CTA-CONTAB-TAB REPLACING
                                                     FIRST "EEE"
                                                     BY ESPEC-ELAB
                              EVALUATE UNSERV-FACT
                              WHEN 01 MOVE 4105    TO MAY-CONTAB-TAB
                              WHEN 02 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 03 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 04 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 05 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 08 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 11 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 12 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 13 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 21 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 30      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 22 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 30      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 31 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 25      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 32 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 25      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 41 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 42 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 51 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 52 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 53 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 54 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 55 MOVE 4105    TO MAY-CONTAB-TAB
                              WHEN 56 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN OTHER
                                    MOVE 4120    TO MAY-CONTAB-TAB
                              END-EVALUATE
                              INSPECT AUX-CONTAB-TAB REPLACING
                                      FIRST "EEE" BY ESPEC-ELAB

                           ELSE
                             MOVE "41200200050" TO CTA-CONTAB-TAB
                             EVALUATE ESPEC-ELAB
                             WHEN 020 MOVE 00002 TO AUX-CONTAB-TAB
                             WHEN 021 MOVE 00002 TO AUX-CONTAB-TAB
                             WHEN 022 MOVE 00002 TO AUX-CONTAB-TAB
                             WHEN 023 MOVE 00002 TO AUX-CONTAB-TAB
                             WHEN 047 MOVE 00019 TO AUX-CONTAB-TAB
                             WHEN 049 MOVE 00045 TO AUX-CONTAB-TAB
                             WHEN 053 MOVE 00030 TO AUX-CONTAB-TAB
                             WHEN 054 MOVE 00049 TO AUX-CONTAB-TAB
                             WHEN 056 MOVE 00032 TO AUX-CONTAB-TAB
                             WHEN 057 MOVE 00032 TO AUX-CONTAB-TAB
                             WHEN 058 MOVE 00037 TO AUX-CONTAB-TAB
                             WHEN 120 MOVE 00003 TO AUX-CONTAB-TAB
                             WHEN 121 MOVE 00003 TO AUX-CONTAB-TAB
                             WHEN 122 MOVE 00003 TO AUX-CONTAB-TAB
                             WHEN 130 MOVE 00004 TO AUX-CONTAB-TAB
                             WHEN 131 MOVE 00005 TO AUX-CONTAB-TAB
                             WHEN 132 MOVE 00011 TO AUX-CONTAB-TAB
                             WHEN 133 MOVE 00006 TO AUX-CONTAB-TAB
                             WHEN 134 MOVE 00010 TO AUX-CONTAB-TAB
                             WHEN 135 MOVE 00012 TO AUX-CONTAB-TAB
                             WHEN 136 MOVE 00017 TO AUX-CONTAB-TAB
                             WHEN 137 MOVE 00002 TO AUX-CONTAB-TAB
                             WHEN 138 MOVE 00007 TO AUX-CONTAB-TAB
                             WHEN 139 MOVE 00008 TO AUX-CONTAB-TAB
                             WHEN 140 MOVE 00007 TO AUX-CONTAB-TAB
                             WHEN 141 MOVE 00039 TO AUX-CONTAB-TAB
                             WHEN 142 MOVE 00042 TO AUX-CONTAB-TAB
                             WHEN 143 MOVE 00009 TO AUX-CONTAB-TAB
                             WHEN 144 MOVE 00009 TO AUX-CONTAB-TAB
                             WHEN 145 MOVE 00009 TO AUX-CONTAB-TAB
                             WHEN 146 MOVE 00009 TO AUX-CONTAB-TAB
                             WHEN 147 MOVE 00009 TO AUX-CONTAB-TAB
                             WHEN 148 MOVE 00004 TO AUX-CONTAB-TAB
                             WHEN 149 MOVE 00007 TO AUX-CONTAB-TAB
                             WHEN 160 MOVE 00043 TO AUX-CONTAB-TAB
                             WHEN 180 MOVE 00036 TO AUX-CONTAB-TAB
                             WHEN 195 MOVE 00031 TO AUX-CONTAB-TAB
                             WHEN 200 MOVE 00012 TO AUX-CONTAB-TAB
                             WHEN 201 MOVE 00012 TO AUX-CONTAB-TAB
                             WHEN 220 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 240 MOVE 00013 TO AUX-CONTAB-TAB
                             WHEN 250 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 280 MOVE 00014 TO AUX-CONTAB-TAB
                             WHEN 281 MOVE 00014 TO AUX-CONTAB-TAB
                             WHEN 282 MOVE 00014 TO AUX-CONTAB-TAB
                             WHEN 310 MOVE 00017 TO AUX-CONTAB-TAB
                             WHEN 311 MOVE 00017 TO AUX-CONTAB-TAB
                             WHEN 320 MOVE 00019 TO AUX-CONTAB-TAB
                             WHEN 330 MOVE 00019 TO AUX-CONTAB-TAB
                             WHEN 331 MOVE 00019 TO AUX-CONTAB-TAB
                             WHEN 340 MOVE 00020 TO AUX-CONTAB-TAB
                             WHEN 341 MOVE 00020 TO AUX-CONTAB-TAB
                             WHEN 350 MOVE 00022 TO AUX-CONTAB-TAB
                             WHEN 351 MOVE 00022 TO AUX-CONTAB-TAB
                             WHEN 352 MOVE 00022 TO AUX-CONTAB-TAB
                             WHEN 370 MOVE 00007 TO AUX-CONTAB-TAB
                             WHEN 371 MOVE 00007 TO AUX-CONTAB-TAB
                             WHEN 387 MOVE 00028 TO AUX-CONTAB-TAB
                             WHEN 388 MOVE 00029 TO AUX-CONTAB-TAB
                             WHEN 410 MOVE 00030 TO AUX-CONTAB-TAB
                             WHEN 420 MOVE 00042 TO AUX-CONTAB-TAB
                             WHEN 430 MOVE 00031 TO AUX-CONTAB-TAB
                             WHEN 431 MOVE 00031 TO AUX-CONTAB-TAB
                             WHEN 440 MOVE 00032 TO AUX-CONTAB-TAB
                             WHEN 441 MOVE 00032 TO AUX-CONTAB-TAB
                             WHEN 442 MOVE 00032 TO AUX-CONTAB-TAB
                             WHEN 443 MOVE 00032 TO AUX-CONTAB-TAB
                             WHEN 450 MOVE 00033 TO AUX-CONTAB-TAB
                             WHEN 451 MOVE 00033 TO AUX-CONTAB-TAB
                             WHEN 460 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 461 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 462 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 463 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 464 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 480 MOVE 00036 TO AUX-CONTAB-TAB
                             WHEN 481 MOVE 00036 TO AUX-CONTAB-TAB
                             WHEN 490 MOVE 00037 TO AUX-CONTAB-TAB
                             WHEN 491 MOVE 00037 TO AUX-CONTAB-TAB
                             WHEN 492 MOVE 00037 TO AUX-CONTAB-TAB
                             WHEN 500 MOVE 00038 TO AUX-CONTAB-TAB
                             WHEN 510 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 511 MOVE 00039 TO AUX-CONTAB-TAB
                             WHEN 512 MOVE 00039 TO AUX-CONTAB-TAB
                             WHEN 513 MOVE 00039 TO AUX-CONTAB-TAB
                             WHEN 514 MOVE 00039 TO AUX-CONTAB-TAB
                             WHEN 520 MOVE 00041 TO AUX-CONTAB-TAB
                             WHEN 521 MOVE 00041 TO AUX-CONTAB-TAB
                             WHEN 522 MOVE 00041 TO AUX-CONTAB-TAB
                             WHEN 550 MOVE 00042 TO AUX-CONTAB-TAB
                             WHEN 551 MOVE 00042 TO AUX-CONTAB-TAB
                             WHEN 560 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 561 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 562 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 563 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 570 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 571 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 580 MOVE 00035 TO AUX-CONTAB-TAB
                             WHEN 590 MOVE 00045 TO AUX-CONTAB-TAB
                             WHEN 620 MOVE 00047 TO AUX-CONTAB-TAB
                             WHEN 630 MOVE 00044 TO AUX-CONTAB-TAB
                             WHEN 750 MOVE 00049 TO AUX-CONTAB-TAB
                             WHEN 751 MOVE 00049 TO AUX-CONTAB-TAB
                             WHEN 760 MOVE 00018 TO AUX-CONTAB-TAB
                             END-EVALUATE
                           END-IF
                    WHEN 4
                           MOVE "43123600002" TO CTA-CONTAB-TAB
                           IF GRUPO-FACT (1) = 73 OR 74
                              MOVE 37 TO SCT-CONTAB-TAB
                           END-IF

                    WHEN 6
                           MOVE "43123600002" TO CTA-CONTAB-TAB
                           IF GRUPO-FACT (1) = 73 OR 74
                              MOVE 37 TO SCT-CONTAB-TAB
                           END-IF
                    WHEN 7 IF (NIT-USU = 892000401 OR 900648993
                                      OR 900804411 OR 900755133
                                      OR 900870633)
                              MOVE "41201000EEE" TO CTA-CONTAB-TAB
                              INSPECT CTA-CONTAB-TAB REPLACING
                                                     FIRST "EEE"
                                                     BY ESPEC-ELAB
                              EVALUATE UNSERV-FACT
                              WHEN 01 MOVE 4105    TO MAY-CONTAB-TAB
                              WHEN 02 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 03 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 04 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 05 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 08 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 11 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 12 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 13 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 21 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 30      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 22 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 30      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 31 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 25      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 32 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 25      TO SCT-CONTAB-TAB
                                      MOVE "20EEE" TO AUX-CONTAB-TAB
                              WHEN 41 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 42 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 51 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 52 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 53 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 54 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 55 MOVE 4105    TO MAY-CONTAB-TAB
                              WHEN 56 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN OTHER
                                    MOVE 4120    TO MAY-CONTAB-TAB
                              END-EVALUATE
                              INSPECT AUX-CONTAB-TAB REPLACING
                                      FIRST "EEE" BY ESPEC-ELAB

                           ELSE
                             MOVE "41010200050" TO CTA-CONTAB-TAB
                             EVALUATE ESPEC-ELAB
                             WHEN 020 MOVE 06002 TO AUX-CONTAB-TAB
                             WHEN 021 MOVE 06002 TO AUX-CONTAB-TAB
                             WHEN 022 MOVE 06002 TO AUX-CONTAB-TAB
                             WHEN 023 MOVE 06002 TO AUX-CONTAB-TAB
                             WHEN 047 MOVE 06019 TO AUX-CONTAB-TAB
                             WHEN 049 MOVE 06045 TO AUX-CONTAB-TAB
                             WHEN 053 MOVE 06030 TO AUX-CONTAB-TAB
                             WHEN 054 MOVE 06049 TO AUX-CONTAB-TAB
                             WHEN 056 MOVE 06032 TO AUX-CONTAB-TAB
                             WHEN 057 MOVE 06032 TO AUX-CONTAB-TAB
                             WHEN 058 MOVE 06037 TO AUX-CONTAB-TAB
                             WHEN 120 MOVE 06003 TO AUX-CONTAB-TAB
                             WHEN 121 MOVE 06003 TO AUX-CONTAB-TAB
                             WHEN 122 MOVE 06003 TO AUX-CONTAB-TAB
                             WHEN 130 MOVE 06004 TO AUX-CONTAB-TAB
                             WHEN 131 MOVE 06005 TO AUX-CONTAB-TAB
                             WHEN 132 MOVE 06011 TO AUX-CONTAB-TAB
                             WHEN 133 MOVE 06006 TO AUX-CONTAB-TAB
                             WHEN 134 MOVE 06010 TO AUX-CONTAB-TAB
                             WHEN 135 MOVE 06012 TO AUX-CONTAB-TAB
                             WHEN 136 MOVE 06017 TO AUX-CONTAB-TAB
                             WHEN 137 MOVE 06002 TO AUX-CONTAB-TAB
                             WHEN 138 MOVE 06007 TO AUX-CONTAB-TAB
                             WHEN 139 MOVE 06008 TO AUX-CONTAB-TAB
                             WHEN 140 MOVE 06007 TO AUX-CONTAB-TAB
                             WHEN 141 MOVE 06039 TO AUX-CONTAB-TAB
                             WHEN 142 MOVE 06042 TO AUX-CONTAB-TAB
                             WHEN 143 MOVE 06009 TO AUX-CONTAB-TAB
                             WHEN 144 MOVE 06009 TO AUX-CONTAB-TAB
                             WHEN 145 MOVE 06009 TO AUX-CONTAB-TAB
                             WHEN 146 MOVE 06009 TO AUX-CONTAB-TAB
                             WHEN 147 MOVE 06009 TO AUX-CONTAB-TAB
                             WHEN 148 MOVE 06004 TO AUX-CONTAB-TAB
                             WHEN 149 MOVE 06007 TO AUX-CONTAB-TAB
                             WHEN 160 MOVE 06043 TO AUX-CONTAB-TAB
                             WHEN 180 MOVE 06036 TO AUX-CONTAB-TAB
                             WHEN 195 MOVE 06031 TO AUX-CONTAB-TAB
                             WHEN 200 MOVE 06012 TO AUX-CONTAB-TAB
                             WHEN 201 MOVE 06012 TO AUX-CONTAB-TAB
                             WHEN 220 MOVE 06035 TO AUX-CONTAB-TAB
                             WHEN 240 MOVE 06013 TO AUX-CONTAB-TAB
                             WHEN 250 MOVE 06035 TO AUX-CONTAB-TAB
                             WHEN 280 MOVE 06014 TO AUX-CONTAB-TAB
                             WHEN 281 MOVE 06014 TO AUX-CONTAB-TAB
                             WHEN 282 MOVE 06014 TO AUX-CONTAB-TAB
                             WHEN 310 MOVE 06017 TO AUX-CONTAB-TAB
                             WHEN 311 MOVE 06017 TO AUX-CONTAB-TAB
                             WHEN 320 MOVE 06019 TO AUX-CONTAB-TAB
                             WHEN 330 MOVE 06019 TO AUX-CONTAB-TAB
                             WHEN 331 MOVE 06019 TO AUX-CONTAB-TAB
                             WHEN 340 MOVE 06020 TO AUX-CONTAB-TAB
                             WHEN 341 MOVE 06020 TO AUX-CONTAB-TAB
                             WHEN 350 MOVE 06022 TO AUX-CONTAB-TAB
                             WHEN 351 MOVE 06022 TO AUX-CONTAB-TAB
                             WHEN 352 MOVE 06022 TO AUX-CONTAB-TAB
                             WHEN 370 MOVE 06007 TO AUX-CONTAB-TAB
                             WHEN 371 MOVE 06007 TO AUX-CONTAB-TAB
                             WHEN 387 MOVE 06028 TO AUX-CONTAB-TAB
                             WHEN 388 MOVE 06029 TO AUX-CONTAB-TAB
                             WHEN 410 MOVE 06030 TO AUX-CONTAB-TAB
                             WHEN 420 MOVE 06042 TO AUX-CONTAB-TAB
                             WHEN 430 MOVE 06031 TO AUX-CONTAB-TAB
                             WHEN 431 MOVE 06031 TO AUX-CONTAB-TAB
                             WHEN 440 MOVE 06032 TO AUX-CONTAB-TAB
                             WHEN 441 MOVE 06032 TO AUX-CONTAB-TAB
                             WHEN 442 MOVE 06032 TO AUX-CONTAB-TAB
                             WHEN 443 MOVE 06032 TO AUX-CONTAB-TAB
                             WHEN 450 MOVE 06033 TO AUX-CONTAB-TAB
                             WHEN 451 MOVE 06033 TO AUX-CONTAB-TAB
                             WHEN 460 MOVE 06035 TO AUX-CONTAB-TAB
                             WHEN 461 MOVE 06035 TO AUX-CONTAB-TAB
                             WHEN 462 MOVE 06035 TO AUX-CONTAB-TAB
                             WHEN 463 MOVE 06035 TO AUX-CONTAB-TAB
                             WHEN 464 MOVE 06035 TO AUX-CONTAB-TAB
                             WHEN 480 MOVE 06036 TO AUX-CONTAB-TAB
                             WHEN 481 MOVE 06036 TO AUX-CONTAB-TAB
                             WHEN 490 MOVE 06037 TO AUX-CONTAB-TAB
                             WHEN 491 MOVE 06037 TO AUX-CONTAB-TAB
                             WHEN 492 MOVE 06037 TO AUX-CONTAB-TAB
                             WHEN 500 MOVE 06038 TO AUX-CONTAB-TAB
                             WHEN 510 MOVE 06035 TO AUX-CONTAB-TAB
                             WHEN 511 MOVE 06039 TO AUX-CONTAB-TAB
                             WHEN 512 MOVE 06039 TO AUX-CONTAB-TAB
                             WHEN 513 MOVE 06039 TO AUX-CONTAB-TAB
                             WHEN 514 MOVE 06039 TO AUX-CONTAB-TAB
                             WHEN 520 MOVE 06041 TO AUX-CONTAB-TAB
                             WHEN 521 MOVE 06041 TO AUX-CONTAB-TAB
                             WHEN 522 MOVE 06041 TO AUX-CONTAB-TAB
                             WHEN 550 MOVE 06042 TO AUX-CONTAB-TAB
                             WHEN 551 MOVE 06042 TO AUX-CONTAB-TAB
                             WHEN 560 MOVE 06035 TO AUX-CONTAB-TAB
                             WHEN 561 MOVE 06035 TO AUX-CONTAB-TAB
                             WHEN 562 MOVE 06035 TO AUX-CONTAB-TAB
                             WHEN 563 MOVE 06035 TO AUX-CONTAB-TAB
                             WHEN 570 MOVE 06035 TO AUX-CONTAB-TAB
                             WHEN 571 MOVE 06035 TO AUX-CONTAB-TAB
                             WHEN 580 MOVE 06035 TO AUX-CONTAB-TAB
                             WHEN 590 MOVE 06045 TO AUX-CONTAB-TAB
                             WHEN 620 MOVE 06047 TO AUX-CONTAB-TAB
                             WHEN 630 MOVE 06044 TO AUX-CONTAB-TAB
                             WHEN 750 MOVE 06049 TO AUX-CONTAB-TAB
                             WHEN 751 MOVE 06049 TO AUX-CONTAB-TAB
                             WHEN 760 MOVE 06018 TO AUX-CONTAB-TAB
                             END-EVALUATE
                           END-IF
                    END-EVALUATE
             WHEN 3 MOVE MED-AYU-FACT  TO MED-OTR-FACT
                    EVALUATE PUC-USU
                    WHEN 3 IF (NIT-USU = 892000401 OR 900648993
                                      OR 900755133 OR 900804411
                                      OR 900870633)
                              MOVE "41201500004" TO CTA-CONTAB-TAB
                           ELSE
                              MOVE "41200200050" TO CTA-CONTAB-TAB
                           END-IF
                    WHEN 4 MOVE "43123600010" TO CTA-CONTAB-TAB
                           IF GRUPO-FACT (1) = 73 OR 74
                              MOVE 37 TO SCT-CONTAB-TAB
                           END-IF
                    WHEN 5 IF (NIT-USU = 892000401 OR 900648993
                                      OR 900755133 OR 900804411
                                      OR 900870633)
                              MOVE "41201500004" TO CTA-CONTAB-TAB
                           ELSE
                              MOVE "41200200050" TO CTA-CONTAB-TAB
                           END-IF
                    WHEN 6 MOVE "43123600010" TO CTA-CONTAB-TAB
                           IF GRUPO-FACT (1) = 73 OR 74
                              MOVE 37 TO SCT-CONTAB-TAB
                           END-IF
                    WHEN 7 IF (NIT-USU = 892000401 OR 900648993
                                      OR 900755133 OR 900804411
                                      OR 900870633)
                              MOVE "41010206004" TO CTA-CONTAB-TAB
                           ELSE
                              MOVE "41010206050" TO CTA-CONTAB-TAB
                           END-IF

                    END-EVALUATE
             WHEN 4 MOVE MED-ANE-FACT  TO MED-OTR-FACT
                    EVALUATE PUC-USU
                    WHEN 3 IF (NIT-USU = 892000401 OR 900648993
                                      OR 900755133 OR 900804411
                                      OR 900870633)
                              MOVE "41201000021" TO CTA-CONTAB-TAB
                              EVALUATE UNSERV-FACT
                              WHEN 01 MOVE 4105    TO MAY-CONTAB-TAB
                              WHEN 02 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 03 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 04 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 05 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 08 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 11 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                              WHEN 12 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                              WHEN 13 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                              WHEN 21 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 30      TO SCT-CONTAB-TAB
                              WHEN 22 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 30      TO SCT-CONTAB-TAB
                              WHEN 31 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 25      TO SCT-CONTAB-TAB
                              WHEN 32 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 25      TO SCT-CONTAB-TAB
                              WHEN 41 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 42 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 51 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 52 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 53 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 54 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 55 MOVE 4105    TO MAY-CONTAB-TAB
                              WHEN 56 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN OTHER
                                    MOVE 4120    TO MAY-CONTAB-TAB
                              END-EVALUATE
                           ELSE
                              MOVE "41200100050" TO CTA-CONTAB-TAB
                           END-IF
                    WHEN 4 MOVE "43123600001" TO CTA-CONTAB-TAB
                           IF GRUPO-FACT (1) = 73 OR 74
                              MOVE 37 TO SCT-CONTAB-TAB
                           END-IF
                    WHEN 5 IF (NIT-USU = 892000401 OR 900648993
                                      OR 900755133 OR 900804411
                                      OR 900870633)
                              MOVE "41201000021" TO CTA-CONTAB-TAB
                              EVALUATE UNSERV-FACT
                              WHEN 01 MOVE 4105    TO MAY-CONTAB-TAB
                              WHEN 02 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 03 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 04 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 05 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 08 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 11 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                              WHEN 12 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                              WHEN 13 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                              WHEN 21 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 30      TO SCT-CONTAB-TAB
                              WHEN 22 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 30      TO SCT-CONTAB-TAB
                              WHEN 31 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 25      TO SCT-CONTAB-TAB
                              WHEN 32 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 25      TO SCT-CONTAB-TAB
                              WHEN 41 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 42 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 51 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 52 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 53 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 54 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 55 MOVE 4105    TO MAY-CONTAB-TAB
                              WHEN 56 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN OTHER
                                    MOVE 4120    TO MAY-CONTAB-TAB
                              END-EVALUATE
                           ELSE
                              MOVE "41200100050" TO CTA-CONTAB-TAB
                           END-IF
                    WHEN 6 MOVE "43123600001" TO CTA-CONTAB-TAB
                           IF GRUPO-FACT (1) = 73 OR 74
                              MOVE 37 TO SCT-CONTAB-TAB
                           END-IF
                    WHEN 7 IF (NIT-USU = 892000401 OR 900648993
                                      OR 900755133 OR 900804411
                                      OR 900870633)
                              MOVE "41010206021" TO CTA-CONTAB-TAB
                           ELSE
                              MOVE "41010206050" TO CTA-CONTAB-TAB
                           END-IF

                    END-EVALUATE
             WHEN 5 MOVE NIT-USU       TO MED-OTR-FACT
                    EVALUATE PUC-USU
                    WHEN 3 IF (NIT-USU = 892000401 OR 900648993
                                      OR 900755133 OR 900804411
                                      OR 900870633)
                              MOVE "41350500001" TO CTA-CONTAB-TAB
                           ELSE
                              MOVE "41350300050" TO CTA-CONTAB-TAB
                           END-IF
                    WHEN 4 MOVE "43126200050" TO CTA-CONTAB-TAB
                    WHEN 5 IF (NIT-USU = 892000401 OR 900648993
                                      OR 900755133 OR 900804411
                                      OR 900870633)
                              MOVE "41350500001" TO CTA-CONTAB-TAB
                           ELSE
                              MOVE "41350300050" TO CTA-CONTAB-TAB
                           END-IF
                    WHEN 6 MOVE "43126200050" TO CTA-CONTAB-TAB
                    WHEN 7 IF (NIT-USU = 892000401 OR 900648993
                                      OR 900755133 OR 900804411
                                      OR 900870633)
                              MOVE "41010706001" TO CTA-CONTAB-TAB
                           ELSE
                              MOVE "41010706050" TO CTA-CONTAB-TAB
                           END-IF
                    END-EVALUATE
             WHEN 6 MOVE NIT-USU       TO MED-OTR-FACT
                    EVALUATE PUC-USU
                    WHEN 3 IF NIT-USU = 892000401
                              MOVE "41201500002" TO CTA-CONTAB-TAB
                              EVALUATE UNSERV-FACT
                              WHEN 01 MOVE 4105    TO MAY-CONTAB-TAB
                              WHEN 02 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 03 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 04 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 05 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 08 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 11 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                              WHEN 12 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                              WHEN 13 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                              WHEN 21 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 30      TO SCT-CONTAB-TAB
                              WHEN 22 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 30      TO SCT-CONTAB-TAB
                              WHEN 31 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 25      TO SCT-CONTAB-TAB
                              WHEN 32 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 25      TO SCT-CONTAB-TAB
                              WHEN 41 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 42 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 51 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 52 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 53 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 54 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 55 MOVE 4105    TO MAY-CONTAB-TAB
                              WHEN 56 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN OTHER
                                    MOVE 4120    TO MAY-CONTAB-TAB
                              END-EVALUATE
                           ELSE
                               MOVE "41200400001" TO CTA-CONTAB-TAB
                           END-IF
                    WHEN 4 MOVE "43123600004" TO CTA-CONTAB-TAB
                           IF GRUPO-FACT (1) = 73 OR 74
                              MOVE 37 TO SCT-CONTAB-TAB
                           END-IF
                    WHEN 5 IF NIT-USU = 892000401
                              MOVE "41201500002" TO CTA-CONTAB-TAB
                              EVALUATE UNSERV-FACT
                              WHEN 01 MOVE 4105    TO MAY-CONTAB-TAB
                              WHEN 02 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 03 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 04 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 05 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 08 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN 11 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                              WHEN 12 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                              WHEN 13 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 35      TO SCT-CONTAB-TAB
                              WHEN 21 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 30      TO SCT-CONTAB-TAB
                              WHEN 22 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 30      TO SCT-CONTAB-TAB
                              WHEN 31 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 25      TO SCT-CONTAB-TAB
                              WHEN 32 MOVE 4115    TO MAY-CONTAB-TAB
                                      MOVE 25      TO SCT-CONTAB-TAB
                              WHEN 41 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 42 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 51 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 52 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 53 MOVE 4115    TO MAY-CONTAB-TAB
                              WHEN 54 MOVE 4120    TO MAY-CONTAB-TAB
                              WHEN 55 MOVE 4105    TO MAY-CONTAB-TAB
                              WHEN 56 MOVE 4110    TO MAY-CONTAB-TAB
                              WHEN OTHER
                                    MOVE 4120    TO MAY-CONTAB-TAB
                              END-EVALUATE
                           ELSE
                               MOVE "41200400001" TO CTA-CONTAB-TAB
                           END-IF
                    WHEN 6 MOVE "43123600004" TO CTA-CONTAB-TAB
                           IF GRUPO-FACT (1) = 73 OR 74
                              MOVE 37 TO SCT-CONTAB-TAB
                           END-IF
                    WHEN 7 IF NIT-USU = 892000401
                              MOVE "41010206002" TO CTA-CONTAB-TAB
                           ELSE
                              MOVE "41010206001" TO CTA-CONTAB-TAB
                           END-IF
                    END-EVALUATE
                    MOVE 100           TO PORC-W
             END-EVALUATE.
             PERFORM ASIGNAR-MEDICO.


       REGISTRO-ARTICULOS.
             MOVE 0                  TO TIPO-ART.
             MOVE GRUPO-FACT     (I) TO GRUPO-ART.
             MOVE COD-ART-FACT   (I) TO NUMERO-ART.
             MOVE CLASE-ART-FACT (I) TO CLASE-ART.
             READ MAESTRO-ARTICULOS WITH NO LOCK
                  INVALID KEY
                    MOVE SPACES  TO CTA-ART.


             MOVE CTA-ART TO CTA-CONTAB-TAB.
             IF MAY-CONTAB-TAB < "2000"
                EVALUATE NIT-USU
                  WHEN 800074996 MOVE "41350100001"  TO CTA-CONTAB-TAB
                  WHEN OTHER     MOVE "4135FXXXXXX"  TO CTA-CONTAB-TAB
                END-EVALUATE
                INSPECT CTA-CONTAB-TAB REPLACING FIRST "XXXXXX"
                                                 BY NRO-FACT.

             IF  (PREFIJO-FACT = "A" OR "P" OR "T" OR "B" OR "D" OR "F"
                OR "G" OR "H" OR "I" OR "J" OR "K" OR "L" OR "M" OR "N"
                OR "O" OR "Q" OR "R" OR "S" OR "V" OR "W"
                OR "X" OR "Y" OR "Z")
             AND PUC-USU = 3
                 MOVE "41350100002" TO CTA-CONTAB-TAB.


             IF  (PREFIJO-FACT = "A" OR "P" OR "T" OR "B" OR "D" OR "F"
                OR "G" OR "H" OR "I" OR "J" OR "K" OR "L" OR "M" OR "N"
                OR "O" OR "Q" OR "R" OR "S" OR "V" OR "W"
                OR "X" OR "Y" OR "Z")
             AND PUC-USU = 7
                 MOVE "41010706002" TO CTA-CONTAB-TAB.


             IF  NIT-USU = 800037021
             AND OPER-ELAB1-FACT = "UCI"
                 MOVE "43122800050"  TO CTA-CONTAB-TAB
             END-IF.

      * HOSPITAL DE MITU QUIERE LLEVAR DROGA PARTICULARES EN LA 4808
             IF  NIT-USU = 845000038
             AND MAY-CONTAB-TAB = 4312
             AND SCT-CONTAB-TAB = 62
                 MOVE "48089000003" TO CTA-CONTAB-TAB
             END-IF.


             COMPUTE VALOR-TEMP  ROUNDED =
                     VLR-FACT (I) * -1.

             PERFORM TOTALES-CONTROL.

       CONTABILIZAR-CTAS.
             MOVE CODIGO-ACUM (LN) TO CUENTA-MOV.
             MOVE COSTO-ACUM  (LN) TO COSTO-MOV.

             IF LOCAL-MOV = ZEROS
             OR LOCAL-MOV = SPACES MOVE COD-MEDICO TO LOCAL-MOV.

             MOVE VLR-ACUM    (LN) TO VALOR-MOV.
             IF VALOR-MOV IS NOT EQUAL TO 0
                PERFORM GRABAR-MOVIMIENTO.


       GRABAR-MOVIMIENTO.
             IF LLAVE-SUB-MOV = CTA-MEDI
             OR MAYOR-MOV = 2335
                PERFORM CREAR-MEDICO
             ELSE
                PERFORM GRABAR-SECUENCIA.

       GRABAR-SECUENCIA.
             IF VALOR-MOV IS NOT ZERO
                OPEN I-O MOVIMIENTO-DIARIO
      *          CALL "CON910" USING SEC-NUM SECU-MOV
                PERFORM EDITAR-SECUENCIA
                WRITE MOV-DIARIO
                ADD 1 TO SEC-NUM
                CLOSE MOVIMIENTO-DIARIO.

       COND-IMAGENES-DIAG.
            IF  (COD-SER-TAB   = "21210" OR "211210")
            AND FECHA-FACT < 011001
                MOVE 15 TO PORC-W
                MOVE  0 TO PORC-OTR-GRSER.


       COND-CARDIOPULMONAR.
            IF   MED-OTR-FACT = 3010401
            AND  (COD-SER-TAB = "24102" OR "24109" OR "24110")
                  MOVE 0 TO PORC-OTR-GRSER
                  MOVE 0 TO PORC-W.

       COND-CLINICA-META.
      * CONSULTA DE URGENCIAS.
            IF COD-SER-TAB  = "39145" OR "35604"
               MOVE 0   TO PORC-OTR-GRSER
               MOVE 100 TO PORC-W.


      * INTERCONSULTAS.
            IF  CL-FACT    = 5
                IF  (COD-SER-TAB   = "39140" OR "38100")
                AND (PREFIJO-FACT = "A" OR "B" OR "D" OR "F"
                OR "G" OR "H" OR "I" OR "J" OR "K" OR "L" OR "M" OR "N"
                OR "O" OR "Q" OR "R" OR "S" OR "V" OR "W"
                OR "X" OR "Y" OR "Z")
                    IF (FECHA-FACT > 010710 AND < 900101)
                        MOVE 10 TO PORC-W
                    ELSE
                        MOVE 0 TO PORC-W
                    END-IF
                ELSE
                   IF  COD-SER-TAB = "39124"
                   AND (PREFIJO-FACT = "A" OR "P" OR "T" OR "B" OR "D"
                   OR "F" OR "G" OR "H" OR "I" OR "J" OR "K" OR "L" OR
                   "M" OR "N" OR "O" OR "Q" OR "R" OR "S" OR "V" OR "W"
                   OR "X" OR "Y" OR "Z")
                   AND COD-TAB-W    = "IS"
                       IF (FECHA-FACT > 010710 AND < 900101)
                           MOVE 10 TO PORC-W
                       ELSE
                           MOVE 0 TO PORC-W
                       END-IF.

      * ANALISIS DEL LLANO.
            IF  MED-OTR-FACT = 822001157
                EVALUATE COD-TAB-W
                WHEN "SO" IF  GR-SER-TAB   = "19"
                          AND DESCRIP1-TAB = "-"
                              MOVE 0 TO PORC-W
                          END-IF
                WHEN "IS" IF  GR-SER-TAB   = "19"
                          AND DESCRIP1-TAB = "-"
                              MOVE 0 TO PORC-W
                          END-IF.


      * IMAGENES DIAGNOSTICAS.
            IF  MED-OTR-FACT = 800156479
                EVALUATE COD-TAB-W
                WHEN "SO" IF  GR-SER-TAB = 31
                          OR (GR-SER-TAB = 21 AND CD-SER1-TAB = 70)
                          OR (GR-SER-TAB = 21 AND CD-SER1-TAB = 71)
                          OR (GR-SER-TAB = 21 AND CD-SER1-TAB = 72)
                          OR (COD-SER-TAB = 21210 OR 21212)
                              MOVE 5 TO PORC-W
                          ELSE
                             IF  FECHA-FACT >= 990901 OR < 900101
                             AND PORC-W = 30
                                 MOVE 20 TO PORC-W
                             END-IF
                          END-IF
                WHEN "IS" IF GR-SER-TAB = 31
                          OR (GR-SER-TAB = 21 AND CD-SER1-TAB = 36)
                              MOVE 5 TO PORC-W
                          ELSE
                             IF  FECHA-FACT >= 990901 OR < 900101
                             AND PORC-W = 30
                                 MOVE 20 TO PORC-W
                             END-IF
                          END-IF
                WHEN OTHER IF GR-SER-TAB = 46
                              MOVE 5 TO PORC-W
                          ELSE
                             IF  FECHA-FACT >= 990901 OR < 900101
                             AND PORC-W = 30
                                 MOVE 20 TO PORC-W
                             END-IF
                           END-IF.


      ****  NIETO IVAN (NEFROLOGO).
            IF MED-OTR-FACT = 19497267
               MOVE 0 TO PORC-W.


      ****  CUMO.
            IF  MED-OTR-FACT = 822000327
            AND NIT-FACT     = 9999
                MOVE 0 TO PORC-W.

      ***** NEBULIZACIONES TERALLANOS.
            IF  (FECHA-FACT > 000601 AND < 900101)
            AND (CL-FACT = 4 OR 5)
            AND  ANO-FACT < 02
            AND (COD-SER-TAB = 27111 OR 29107 OR 29108 OR 29111 OR 29118
                            OR 29120 OR 29314 OR 29504 OR 29908 OR 35504
                            OR 294314)
                 MOVE 50 TO PORC-W.

      *** ELSA PRADA CITOLOGA.
            IF  MED-OTR-FACT = 21230571
                MOVE 30 TO PORC-W.

      * CITOLOGIA.
            IF  GR-SER-TAB  = "20"
            AND CD-SER1-TAB = "30"
            AND ANO-FACT < 02
                MOVE 0 TO PORC-OTR-GRSER.


      ***** MARCELA GOMEZ ODONTOLOGA.
            IF MED-OTR-FACT = 40391556
               MOVE 0   TO PORC-OTR-GRSER
               MOVE 100 TO PORC-W.


       CAMBIAR-NIT.
           IF MED-OTR-FACT = 40439353 OR 40375908 OR 30083045
                    OR 40442282 OR 52332379 OR 40442763 OR 52561742
                    OR 40392909 OR 40389899
              MOVE 830104629 TO MED-OTR-FACT.

       EDITAR-SECUENCIA.
           IF SEC-NUM < 100
              MOVE SEC-NUM TO SECU-X
              MOVE SECU-X TO SECU-MOV-W
           ELSE
              EVALUATE SEC-NUM
              WHEN 100 MOVE "A0" TO SECU-MOV-W
              WHEN 101 MOVE "A1" TO SECU-MOV-W
              WHEN 102 MOVE "A2" TO SECU-MOV-W
              WHEN 103 MOVE "A3" TO SECU-MOV-W
              WHEN 104 MOVE "A4" TO SECU-MOV-W
              WHEN 105 MOVE "A5" TO SECU-MOV-W
              WHEN 106 MOVE "A6" TO SECU-MOV-W
              WHEN 107 MOVE "A7" TO SECU-MOV-W
              WHEN 108 MOVE "A8" TO SECU-MOV-W
              WHEN 109 MOVE "A9" TO SECU-MOV-W
              WHEN 110 MOVE "AA" TO SECU-MOV-W
              WHEN 111 MOVE "AB" TO SECU-MOV-W
              WHEN 112 MOVE "AC" TO SECU-MOV-W
              WHEN 113 MOVE "AD" TO SECU-MOV-W
              WHEN 114 MOVE "AE" TO SECU-MOV-W
              WHEN 115 MOVE "AF" TO SECU-MOV-W
              WHEN 116 MOVE "AG" TO SECU-MOV-W
              WHEN 117 MOVE "AH" TO SECU-MOV-W
              WHEN 118 MOVE "AI" TO SECU-MOV-W
              WHEN 119 MOVE "AJ" TO SECU-MOV-W
              WHEN 120 MOVE "AK" TO SECU-MOV-W
              WHEN 121 MOVE "AL" TO SECU-MOV-W
              WHEN 122 MOVE "AM" TO SECU-MOV-W
              WHEN 123 MOVE "AN" TO SECU-MOV-W
              WHEN 124 MOVE "AO" TO SECU-MOV-W
              WHEN 125 MOVE "AP" TO SECU-MOV-W
              WHEN 126 MOVE "AQ" TO SECU-MOV-W
              WHEN 127 MOVE "AR" TO SECU-MOV-W
              WHEN 128 MOVE "AS" TO SECU-MOV-W
              WHEN 129 MOVE "AT" TO SECU-MOV-W
              WHEN 130 MOVE "AU" TO SECU-MOV-W
              WHEN 131 MOVE "AV" TO SECU-MOV-W
              WHEN 132 MOVE "AW" TO SECU-MOV-W
              WHEN 133 MOVE "AX" TO SECU-MOV-W
              WHEN 134 MOVE "AY" TO SECU-MOV-W
              WHEN 135 MOVE "AZ" TO SECU-MOV-W
              WHEN 136 MOVE "B0" TO SECU-MOV-W
              WHEN 137 MOVE "B1" TO SECU-MOV-W
              WHEN 138 MOVE "B2" TO SECU-MOV-W
              WHEN 139 MOVE "B3" TO SECU-MOV-W
              WHEN 140 MOVE "B4" TO SECU-MOV-W
              WHEN 141 MOVE "B5" TO SECU-MOV-W
              WHEN 142 MOVE "B6" TO SECU-MOV-W
              WHEN 143 MOVE "B7" TO SECU-MOV-W
              WHEN 144 MOVE "B8" TO SECU-MOV-W
              WHEN 145 MOVE "B9" TO SECU-MOV-W
              WHEN 146 MOVE "BA" TO SECU-MOV-W
              WHEN 147 MOVE "BB" TO SECU-MOV-W
              WHEN 148 MOVE "BC" TO SECU-MOV-W
              WHEN 149 MOVE "BD" TO SECU-MOV-W
              WHEN 150 MOVE "BE" TO SECU-MOV-W
              WHEN 151 MOVE "BF" TO SECU-MOV-W
              WHEN 152 MOVE "BG" TO SECU-MOV-W
              WHEN 153 MOVE "BH" TO SECU-MOV-W
              WHEN 154 MOVE "BI" TO SECU-MOV-W
              WHEN 155 MOVE "BJ" TO SECU-MOV-W
              WHEN 156 MOVE "BK" TO SECU-MOV-W
              WHEN 157 MOVE "BL" TO SECU-MOV-W
              WHEN 158 MOVE "BM" TO SECU-MOV-W
              WHEN 159 MOVE "BN" TO SECU-MOV-W
              WHEN 160 MOVE "BO" TO SECU-MOV-W
              WHEN 161 MOVE "BP" TO SECU-MOV-W
              WHEN 162 MOVE "BQ" TO SECU-MOV-W
              WHEN 163 MOVE "BR" TO SECU-MOV-W
              WHEN 164 MOVE "BS" TO SECU-MOV-W
              WHEN 165 MOVE "BT" TO SECU-MOV-W
              WHEN 166 MOVE "BU" TO SECU-MOV-W
              WHEN 167 MOVE "BV" TO SECU-MOV-W
              WHEN 168 MOVE "BW" TO SECU-MOV-W
              WHEN 169 MOVE "BX" TO SECU-MOV-W
              WHEN 170 MOVE "BY" TO SECU-MOV-W
              WHEN 171 MOVE "BZ" TO SECU-MOV-W
              WHEN 172 MOVE "C0" TO SECU-MOV-W
              WHEN 173 MOVE "C1" TO SECU-MOV-W
              WHEN 174 MOVE "C2" TO SECU-MOV-W
              WHEN 175 MOVE "C3" TO SECU-MOV-W
              WHEN 176 MOVE "C4" TO SECU-MOV-W
              WHEN 177 MOVE "C5" TO SECU-MOV-W
              WHEN 178 MOVE "C6" TO SECU-MOV-W
              WHEN 179 MOVE "C7" TO SECU-MOV-W
              WHEN 180 MOVE "C8" TO SECU-MOV-W
              WHEN 181 MOVE "C9" TO SECU-MOV-W
              WHEN 182 MOVE "CA" TO SECU-MOV-W
              WHEN 183 MOVE "CB" TO SECU-MOV-W
              WHEN 184 MOVE "CC" TO SECU-MOV-W
              WHEN 185 MOVE "CD" TO SECU-MOV-W
              WHEN 186 MOVE "CE" TO SECU-MOV-W
              WHEN 187 MOVE "CF" TO SECU-MOV-W
              WHEN 188 MOVE "CG" TO SECU-MOV-W
              WHEN 189 MOVE "CH" TO SECU-MOV-W
              WHEN 190 MOVE "CI" TO SECU-MOV-W
              WHEN 191 MOVE "CJ" TO SECU-MOV-W
              WHEN 192 MOVE "CK" TO SECU-MOV-W
              WHEN 193 MOVE "CL" TO SECU-MOV-W
              WHEN 194 MOVE "CM" TO SECU-MOV-W
              WHEN 195 MOVE "CN" TO SECU-MOV-W
              WHEN 196 MOVE "CO" TO SECU-MOV-W
              WHEN 197 MOVE "CP" TO SECU-MOV-W
              WHEN 198 MOVE "CQ" TO SECU-MOV-W
              WHEN 199 MOVE "CR" TO SECU-MOV-W
              WHEN 200 MOVE "CS" TO SECU-MOV-W
              WHEN 201 MOVE "CT" TO SECU-MOV-W
              WHEN 202 MOVE "CU" TO SECU-MOV-W
              WHEN 203 MOVE "CV" TO SECU-MOV-W
              WHEN 204 MOVE "CW" TO SECU-MOV-W
              WHEN 205 MOVE "CX" TO SECU-MOV-W
              WHEN 206 MOVE "CY" TO SECU-MOV-W
              WHEN 207 MOVE "CZ" TO SECU-MOV-W
              WHEN 208 MOVE "D0" TO SECU-MOV-W
              WHEN 209 MOVE "D1" TO SECU-MOV-W
              WHEN 210 MOVE "D2" TO SECU-MOV-W
              WHEN 211 MOVE "D3" TO SECU-MOV-W
              WHEN 212 MOVE "D4" TO SECU-MOV-W
              WHEN 213 MOVE "D5" TO SECU-MOV-W
              WHEN 214 MOVE "D6" TO SECU-MOV-W
              WHEN 215 MOVE "D7" TO SECU-MOV-W
              WHEN 216 MOVE "D8" TO SECU-MOV-W
              WHEN 217 MOVE "D9" TO SECU-MOV-W
              WHEN 218 MOVE "DA" TO SECU-MOV-W
              WHEN 219 MOVE "DB" TO SECU-MOV-W
              WHEN 220 MOVE "DC" TO SECU-MOV-W
              WHEN 221 MOVE "DD" TO SECU-MOV-W
              WHEN 222 MOVE "DE" TO SECU-MOV-W
              WHEN 223 MOVE "DF" TO SECU-MOV-W
              WHEN 224 MOVE "DG" TO SECU-MOV-W
              WHEN 225 MOVE "DH" TO SECU-MOV-W
              WHEN 226 MOVE "DI" TO SECU-MOV-W
              WHEN 227 MOVE "DJ" TO SECU-MOV-W
              WHEN 228 MOVE "DK" TO SECU-MOV-W
              WHEN 229 MOVE "DL" TO SECU-MOV-W
              WHEN 230 MOVE "DM" TO SECU-MOV-W
              WHEN 231 MOVE "DN" TO SECU-MOV-W
              WHEN 232 MOVE "DO" TO SECU-MOV-W
              WHEN 233 MOVE "DP" TO SECU-MOV-W
              WHEN 234 MOVE "DQ" TO SECU-MOV-W
              WHEN 235 MOVE "DR" TO SECU-MOV-W
              WHEN 236 MOVE "DS" TO SECU-MOV-W
              WHEN 237 MOVE "DT" TO SECU-MOV-W
              WHEN 238 MOVE "DU" TO SECU-MOV-W
              WHEN 239 MOVE "DV" TO SECU-MOV-W
              WHEN 240 MOVE "DW" TO SECU-MOV-W
              WHEN 241 MOVE "DX" TO SECU-MOV-W
              WHEN 242 MOVE "DY" TO SECU-MOV-W
              WHEN 243 MOVE "DZ" TO SECU-MOV-W
              WHEN 244 MOVE "E0" TO SECU-MOV-W
              WHEN 245 MOVE "E1" TO SECU-MOV-W
              WHEN 246 MOVE "E2" TO SECU-MOV-W
              WHEN 247 MOVE "E3" TO SECU-MOV-W
              WHEN 248 MOVE "E4" TO SECU-MOV-W
              WHEN 249 MOVE "E5" TO SECU-MOV-W
              WHEN 250 MOVE "E6" TO SECU-MOV-W
              WHEN 251 MOVE "E7" TO SECU-MOV-W
              WHEN 252 MOVE "E8" TO SECU-MOV-W
              WHEN 253 MOVE "E9" TO SECU-MOV-W
              WHEN 254 MOVE "EA" TO SECU-MOV-W
              WHEN 255 MOVE "EB" TO SECU-MOV-W
              WHEN 256 MOVE "EC" TO SECU-MOV-W
              WHEN 257 MOVE "ED" TO SECU-MOV-W
              WHEN 258 MOVE "EE" TO SECU-MOV-W
              WHEN 259 MOVE "EF" TO SECU-MOV-W
              WHEN 260 MOVE "EG" TO SECU-MOV-W
              WHEN 261 MOVE "EH" TO SECU-MOV-W
              WHEN 262 MOVE "EI" TO SECU-MOV-W
              WHEN 263 MOVE "EJ" TO SECU-MOV-W
              WHEN 264 MOVE "EK" TO SECU-MOV-W
              WHEN 265 MOVE "EL" TO SECU-MOV-W
              WHEN 266 MOVE "EM" TO SECU-MOV-W
              WHEN 267 MOVE "EN" TO SECU-MOV-W
              WHEN 268 MOVE "EO" TO SECU-MOV-W
              WHEN 269 MOVE "EP" TO SECU-MOV-W
              WHEN 270 MOVE "EQ" TO SECU-MOV-W
              WHEN 271 MOVE "ER" TO SECU-MOV-W
              WHEN 272 MOVE "ES" TO SECU-MOV-W
              WHEN 273 MOVE "ET" TO SECU-MOV-W
              WHEN 274 MOVE "EU" TO SECU-MOV-W
              WHEN 275 MOVE "EV" TO SECU-MOV-W
              WHEN 276 MOVE "EW" TO SECU-MOV-W
              WHEN 277 MOVE "EX" TO SECU-MOV-W
              WHEN 278 MOVE "EY" TO SECU-MOV-W
              WHEN 279 MOVE "EZ" TO SECU-MOV-W
              WHEN 280 MOVE "F0" TO SECU-MOV-W
              WHEN 281 MOVE "F1" TO SECU-MOV-W
              WHEN 282 MOVE "F2" TO SECU-MOV-W
              WHEN 283 MOVE "F3" TO SECU-MOV-W
              WHEN 284 MOVE "F4" TO SECU-MOV-W
              WHEN 285 MOVE "F5" TO SECU-MOV-W
              WHEN 286 MOVE "F6" TO SECU-MOV-W
              WHEN 287 MOVE "F7" TO SECU-MOV-W
              WHEN 288 MOVE "F8" TO SECU-MOV-W
              WHEN 289 MOVE "F9" TO SECU-MOV-W
              WHEN 290 MOVE "FA" TO SECU-MOV-W
              WHEN 291 MOVE "FB" TO SECU-MOV-W
              WHEN 292 MOVE "FC" TO SECU-MOV-W
              WHEN 293 MOVE "FD" TO SECU-MOV-W
              WHEN 294 MOVE "FE" TO SECU-MOV-W
              WHEN 295 MOVE "FF" TO SECU-MOV-W
              WHEN 296 MOVE "FG" TO SECU-MOV-W
              WHEN 297 MOVE "FH" TO SECU-MOV-W
              WHEN 298 MOVE "FI" TO SECU-MOV-W
              WHEN 299 MOVE "FJ" TO SECU-MOV-W
              WHEN 300 MOVE "FK" TO SECU-MOV-W
              WHEN 301 MOVE "FL" TO SECU-MOV-W
              WHEN 302 MOVE "FM" TO SECU-MOV-W
              WHEN 303 MOVE "FN" TO SECU-MOV-W
              WHEN 304 MOVE "FO" TO SECU-MOV-W
              WHEN 305 MOVE "FP" TO SECU-MOV-W
              WHEN 306 MOVE "FQ" TO SECU-MOV-W
              WHEN 307 MOVE "FR" TO SECU-MOV-W
              WHEN 308 MOVE "FS" TO SECU-MOV-W
              WHEN 309 MOVE "FT" TO SECU-MOV-W
              WHEN 310 MOVE "FU" TO SECU-MOV-W
              WHEN 311 MOVE "FV" TO SECU-MOV-W
              WHEN 312 MOVE "FW" TO SECU-MOV-W
              WHEN 313 MOVE "FX" TO SECU-MOV-W
              WHEN 314 MOVE "FY" TO SECU-MOV-W
              WHEN 315 MOVE "FZ" TO SECU-MOV-W
              WHEN 316 MOVE "G0" TO SECU-MOV-W
              WHEN 317 MOVE "G1" TO SECU-MOV-W
              WHEN 318 MOVE "G2" TO SECU-MOV-W
              WHEN 319 MOVE "G3" TO SECU-MOV-W
              WHEN 320 MOVE "G4" TO SECU-MOV-W
              WHEN 321 MOVE "G5" TO SECU-MOV-W
              WHEN 322 MOVE "G6" TO SECU-MOV-W
              WHEN 323 MOVE "G7" TO SECU-MOV-W
              WHEN 324 MOVE "G8" TO SECU-MOV-W
              WHEN 325 MOVE "G9" TO SECU-MOV-W
              WHEN 326 MOVE "GA" TO SECU-MOV-W
              WHEN 327 MOVE "GB" TO SECU-MOV-W
              WHEN 328 MOVE "GC" TO SECU-MOV-W
              WHEN 329 MOVE "GD" TO SECU-MOV-W
              WHEN 330 MOVE "GE" TO SECU-MOV-W
              WHEN 331 MOVE "GF" TO SECU-MOV-W
              WHEN 332 MOVE "GG" TO SECU-MOV-W
              WHEN 333 MOVE "GH" TO SECU-MOV-W
              WHEN 334 MOVE "GI" TO SECU-MOV-W
              WHEN 335 MOVE "GJ" TO SECU-MOV-W
              WHEN 336 MOVE "GK" TO SECU-MOV-W
              WHEN 337 MOVE "GL" TO SECU-MOV-W
              WHEN 338 MOVE "GM" TO SECU-MOV-W
              WHEN 339 MOVE "GN" TO SECU-MOV-W
              WHEN 340 MOVE "GO" TO SECU-MOV-W
              WHEN 341 MOVE "GP" TO SECU-MOV-W
              WHEN 342 MOVE "GQ" TO SECU-MOV-W
              WHEN 343 MOVE "GR" TO SECU-MOV-W
              WHEN 344 MOVE "GS" TO SECU-MOV-W
              WHEN 345 MOVE "GT" TO SECU-MOV-W
              WHEN 346 MOVE "GU" TO SECU-MOV-W
              WHEN 347 MOVE "GV" TO SECU-MOV-W
              WHEN 348 MOVE "GW" TO SECU-MOV-W
              WHEN 349 MOVE "GX" TO SECU-MOV-W
              WHEN 350 MOVE "GY" TO SECU-MOV-W
              WHEN 351 MOVE "GZ" TO SECU-MOV-W
              WHEN 352 MOVE "H0" TO SECU-MOV-W
              WHEN 353 MOVE "H1" TO SECU-MOV-W
              WHEN 354 MOVE "H2" TO SECU-MOV-W
              WHEN 355 MOVE "H3" TO SECU-MOV-W
              WHEN 356 MOVE "H4" TO SECU-MOV-W
              WHEN 357 MOVE "H5" TO SECU-MOV-W
              WHEN 358 MOVE "H6" TO SECU-MOV-W
              WHEN 359 MOVE "H7" TO SECU-MOV-W
              WHEN 360 MOVE "H8" TO SECU-MOV-W
              WHEN 361 MOVE "H9" TO SECU-MOV-W
              WHEN 362 MOVE "HA" TO SECU-MOV-W
              WHEN 363 MOVE "HB" TO SECU-MOV-W
              WHEN 364 MOVE "HC" TO SECU-MOV-W
              WHEN 365 MOVE "HD" TO SECU-MOV-W
              WHEN 366 MOVE "HE" TO SECU-MOV-W
              WHEN 367 MOVE "HF" TO SECU-MOV-W
              WHEN 368 MOVE "HG" TO SECU-MOV-W
              WHEN 369 MOVE "HH" TO SECU-MOV-W
              WHEN 370 MOVE "HI" TO SECU-MOV-W
              WHEN 371 MOVE "HJ" TO SECU-MOV-W
              WHEN 372 MOVE "HK" TO SECU-MOV-W
              WHEN 373 MOVE "HL" TO SECU-MOV-W
              WHEN 374 MOVE "HM" TO SECU-MOV-W
              WHEN 375 MOVE "HN" TO SECU-MOV-W
              WHEN 376 MOVE "HO" TO SECU-MOV-W
              WHEN 377 MOVE "HP" TO SECU-MOV-W
              WHEN 378 MOVE "HQ" TO SECU-MOV-W
              WHEN 379 MOVE "HR" TO SECU-MOV-W
              WHEN 380 MOVE "HS" TO SECU-MOV-W
              WHEN 381 MOVE "HT" TO SECU-MOV-W
              WHEN 382 MOVE "HU" TO SECU-MOV-W
              WHEN 383 MOVE "HV" TO SECU-MOV-W
              WHEN 384 MOVE "HW" TO SECU-MOV-W
              WHEN 385 MOVE "HX" TO SECU-MOV-W
              WHEN 386 MOVE "HY" TO SECU-MOV-W
              WHEN 387 MOVE "HZ" TO SECU-MOV-W
              WHEN 388 MOVE "I0" TO SECU-MOV-W
              WHEN 389 MOVE "I1" TO SECU-MOV-W
              WHEN 390 MOVE "I2" TO SECU-MOV-W
              WHEN 391 MOVE "I3" TO SECU-MOV-W
              WHEN 392 MOVE "I4" TO SECU-MOV-W
              WHEN 393 MOVE "I5" TO SECU-MOV-W
              WHEN 394 MOVE "I6" TO SECU-MOV-W
              WHEN 395 MOVE "I7" TO SECU-MOV-W
              WHEN 396 MOVE "I8" TO SECU-MOV-W
              WHEN 397 MOVE "I9" TO SECU-MOV-W
              WHEN 398 MOVE "IA" TO SECU-MOV-W
              WHEN 399 MOVE "IB" TO SECU-MOV-W
              WHEN 400 MOVE "IC" TO SECU-MOV-W
              WHEN 401 MOVE "ID" TO SECU-MOV-W
              WHEN 402 MOVE "IE" TO SECU-MOV-W
              WHEN 403 MOVE "IF" TO SECU-MOV-W
              WHEN 404 MOVE "IG" TO SECU-MOV-W
              WHEN 405 MOVE "IH" TO SECU-MOV-W
              WHEN 406 MOVE "II" TO SECU-MOV-W
              WHEN 407 MOVE "IJ" TO SECU-MOV-W
              WHEN 408 MOVE "IK" TO SECU-MOV-W
              WHEN 409 MOVE "IL" TO SECU-MOV-W
              WHEN 410 MOVE "IM" TO SECU-MOV-W
              WHEN 411 MOVE "IN" TO SECU-MOV-W
              WHEN 412 MOVE "IO" TO SECU-MOV-W
              WHEN 413 MOVE "IP" TO SECU-MOV-W
              WHEN 414 MOVE "IQ" TO SECU-MOV-W
              WHEN 415 MOVE "IR" TO SECU-MOV-W
              WHEN 416 MOVE "IS" TO SECU-MOV-W
              WHEN 417 MOVE "IT" TO SECU-MOV-W
              WHEN 418 MOVE "IU" TO SECU-MOV-W
              WHEN 419 MOVE "IV" TO SECU-MOV-W
              WHEN 420 MOVE "IW" TO SECU-MOV-W
              WHEN 421 MOVE "IX" TO SECU-MOV-W
              WHEN 422 MOVE "IY" TO SECU-MOV-W
              WHEN 423 MOVE "IZ" TO SECU-MOV-W
              WHEN 424 MOVE "J0" TO SECU-MOV-W
              WHEN 425 MOVE "J1" TO SECU-MOV-W
              WHEN 426 MOVE "J2" TO SECU-MOV-W
              WHEN 427 MOVE "J3" TO SECU-MOV-W
              WHEN 428 MOVE "J4" TO SECU-MOV-W
              WHEN 429 MOVE "J5" TO SECU-MOV-W
              WHEN 430 MOVE "J6" TO SECU-MOV-W
              WHEN 431 MOVE "J7" TO SECU-MOV-W
              WHEN 432 MOVE "J8" TO SECU-MOV-W
              WHEN 433 MOVE "J9" TO SECU-MOV-W
              WHEN 434 MOVE "JA" TO SECU-MOV-W
              WHEN 435 MOVE "JB" TO SECU-MOV-W
              WHEN 436 MOVE "JC" TO SECU-MOV-W
              WHEN 437 MOVE "JD" TO SECU-MOV-W
              WHEN 438 MOVE "JE" TO SECU-MOV-W
              WHEN 439 MOVE "JF" TO SECU-MOV-W
              WHEN 440 MOVE "JG" TO SECU-MOV-W
              WHEN 441 MOVE "JH" TO SECU-MOV-W
              WHEN 442 MOVE "JI" TO SECU-MOV-W
              WHEN 443 MOVE "JJ" TO SECU-MOV-W
              WHEN 444 MOVE "JK" TO SECU-MOV-W
              WHEN 445 MOVE "JL" TO SECU-MOV-W
              WHEN 446 MOVE "JM" TO SECU-MOV-W
              WHEN 447 MOVE "JN" TO SECU-MOV-W
              WHEN 448 MOVE "JO" TO SECU-MOV-W
              WHEN 449 MOVE "JP" TO SECU-MOV-W
              WHEN 450 MOVE "JQ" TO SECU-MOV-W
              WHEN 451 MOVE "JR" TO SECU-MOV-W
              WHEN 452 MOVE "JS" TO SECU-MOV-W
              WHEN 453 MOVE "JT" TO SECU-MOV-W
              WHEN 454 MOVE "JU" TO SECU-MOV-W
              WHEN 455 MOVE "JV" TO SECU-MOV-W
              WHEN 456 MOVE "JW" TO SECU-MOV-W
              WHEN 457 MOVE "JX" TO SECU-MOV-W
              WHEN 458 MOVE "JY" TO SECU-MOV-W
              WHEN 459 MOVE "JZ" TO SECU-MOV-W
              WHEN 460 MOVE "K0" TO SECU-MOV-W
              WHEN 461 MOVE "K1" TO SECU-MOV-W
              WHEN 462 MOVE "K2" TO SECU-MOV-W
              WHEN 463 MOVE "K3" TO SECU-MOV-W
              WHEN 464 MOVE "K4" TO SECU-MOV-W
              WHEN 465 MOVE "K5" TO SECU-MOV-W
              WHEN 466 MOVE "K6" TO SECU-MOV-W
              WHEN 467 MOVE "K7" TO SECU-MOV-W
              WHEN 468 MOVE "K8" TO SECU-MOV-W
              WHEN 469 MOVE "K9" TO SECU-MOV-W
              WHEN 470 MOVE "KA" TO SECU-MOV-W
              WHEN 471 MOVE "KB" TO SECU-MOV-W
              WHEN 472 MOVE "KC" TO SECU-MOV-W
              WHEN 473 MOVE "KD" TO SECU-MOV-W
              WHEN 474 MOVE "KE" TO SECU-MOV-W
              WHEN 475 MOVE "KF" TO SECU-MOV-W
              WHEN 476 MOVE "KG" TO SECU-MOV-W
              WHEN 477 MOVE "KH" TO SECU-MOV-W
              WHEN 478 MOVE "KI" TO SECU-MOV-W
              WHEN 479 MOVE "KJ" TO SECU-MOV-W
              WHEN 480 MOVE "KK" TO SECU-MOV-W
              WHEN 481 MOVE "KL" TO SECU-MOV-W
              WHEN 482 MOVE "KM" TO SECU-MOV-W
              WHEN 483 MOVE "KN" TO SECU-MOV-W
              WHEN 484 MOVE "KO" TO SECU-MOV-W
              WHEN 485 MOVE "KP" TO SECU-MOV-W
              WHEN 486 MOVE "KQ" TO SECU-MOV-W
              WHEN 487 MOVE "KR" TO SECU-MOV-W
              WHEN 488 MOVE "KS" TO SECU-MOV-W
              WHEN 489 MOVE "KT" TO SECU-MOV-W
              WHEN 490 MOVE "KU" TO SECU-MOV-W
              WHEN 491 MOVE "KV" TO SECU-MOV-W
              WHEN 492 MOVE "KW" TO SECU-MOV-W
              WHEN 493 MOVE "KX" TO SECU-MOV-W
              WHEN 494 MOVE "KY" TO SECU-MOV-W
              WHEN 495 MOVE "KZ" TO SECU-MOV-W
              WHEN 496 MOVE "L0" TO SECU-MOV-W
              WHEN 497 MOVE "L1" TO SECU-MOV-W
              WHEN 498 MOVE "L2" TO SECU-MOV-W
              WHEN 499 MOVE "L3" TO SECU-MOV-W
              WHEN 500 MOVE "L4" TO SECU-MOV-W
              WHEN 501 MOVE "L5" TO SECU-MOV-W
              WHEN 502 MOVE "L6" TO SECU-MOV-W
              WHEN 503 MOVE "L7" TO SECU-MOV-W
              WHEN 504 MOVE "L8" TO SECU-MOV-W
              WHEN 505 MOVE "L9" TO SECU-MOV-W
              WHEN 506 MOVE "LA" TO SECU-MOV-W
              WHEN 507 MOVE "LB" TO SECU-MOV-W
              WHEN 508 MOVE "LC" TO SECU-MOV-W
              WHEN 509 MOVE "LD" TO SECU-MOV-W
              WHEN 510 MOVE "LE" TO SECU-MOV-W
              WHEN 511 MOVE "LF" TO SECU-MOV-W
              WHEN 512 MOVE "LG" TO SECU-MOV-W
              WHEN 513 MOVE "LH" TO SECU-MOV-W
              WHEN 514 MOVE "LI" TO SECU-MOV-W
              WHEN 515 MOVE "LJ" TO SECU-MOV-W
              WHEN 516 MOVE "LK" TO SECU-MOV-W
              WHEN 517 MOVE "LL" TO SECU-MOV-W
              WHEN 518 MOVE "LM" TO SECU-MOV-W
              WHEN 519 MOVE "LN" TO SECU-MOV-W
              WHEN 520 MOVE "LO" TO SECU-MOV-W
              WHEN 521 MOVE "LP" TO SECU-MOV-W
              WHEN 522 MOVE "LQ" TO SECU-MOV-W
              WHEN 523 MOVE "LR" TO SECU-MOV-W
              WHEN 524 MOVE "LS" TO SECU-MOV-W
              WHEN 525 MOVE "LT" TO SECU-MOV-W
              WHEN 526 MOVE "LU" TO SECU-MOV-W
              WHEN 527 MOVE "LV" TO SECU-MOV-W
              WHEN 528 MOVE "LW" TO SECU-MOV-W
              WHEN 529 MOVE "LX" TO SECU-MOV-W
              WHEN 530 MOVE "LY" TO SECU-MOV-W
              WHEN 531 MOVE "LZ" TO SECU-MOV-W
              WHEN 532 MOVE "M0" TO SECU-MOV-W
              WHEN 533 MOVE "M1" TO SECU-MOV-W
              WHEN 534 MOVE "M2" TO SECU-MOV-W
              WHEN 535 MOVE "M3" TO SECU-MOV-W
              WHEN 536 MOVE "M4" TO SECU-MOV-W
              WHEN 537 MOVE "M5" TO SECU-MOV-W
              WHEN 538 MOVE "M6" TO SECU-MOV-W
              WHEN 539 MOVE "M7" TO SECU-MOV-W
              WHEN 540 MOVE "M8" TO SECU-MOV-W
              WHEN 541 MOVE "M9" TO SECU-MOV-W
              WHEN 542 MOVE "MA" TO SECU-MOV-W
              WHEN 543 MOVE "MB" TO SECU-MOV-W
              WHEN 544 MOVE "MC" TO SECU-MOV-W
              WHEN 545 MOVE "MD" TO SECU-MOV-W
              WHEN 546 MOVE "ME" TO SECU-MOV-W
              WHEN 547 MOVE "MF" TO SECU-MOV-W
              WHEN 548 MOVE "MG" TO SECU-MOV-W
              WHEN 549 MOVE "MH" TO SECU-MOV-W
              WHEN 550 MOVE "MI" TO SECU-MOV-W
              WHEN 551 MOVE "MJ" TO SECU-MOV-W
              WHEN 552 MOVE "MK" TO SECU-MOV-W
              WHEN 553 MOVE "ML" TO SECU-MOV-W
              WHEN 554 MOVE "MM" TO SECU-MOV-W
              WHEN 555 MOVE "MN" TO SECU-MOV-W
              WHEN 556 MOVE "MO" TO SECU-MOV-W
              WHEN 557 MOVE "MP" TO SECU-MOV-W
              WHEN 558 MOVE "MQ" TO SECU-MOV-W
              WHEN 559 MOVE "MR" TO SECU-MOV-W
              WHEN 560 MOVE "MS" TO SECU-MOV-W
              WHEN 561 MOVE "MT" TO SECU-MOV-W
              WHEN 562 MOVE "MU" TO SECU-MOV-W
              WHEN 563 MOVE "MV" TO SECU-MOV-W
              WHEN 564 MOVE "MW" TO SECU-MOV-W
              WHEN 565 MOVE "MX" TO SECU-MOV-W
              WHEN 566 MOVE "MY" TO SECU-MOV-W
              WHEN 567 MOVE "MZ" TO SECU-MOV-W
              WHEN 568 MOVE "N0" TO SECU-MOV-W
              WHEN 569 MOVE "N1" TO SECU-MOV-W
              WHEN 570 MOVE "N2" TO SECU-MOV-W
              WHEN 571 MOVE "N3" TO SECU-MOV-W
              WHEN 572 MOVE "N4" TO SECU-MOV-W
              WHEN 573 MOVE "N5" TO SECU-MOV-W
              WHEN 574 MOVE "N6" TO SECU-MOV-W
              WHEN 575 MOVE "N7" TO SECU-MOV-W
              WHEN 576 MOVE "N8" TO SECU-MOV-W
              WHEN 577 MOVE "N9" TO SECU-MOV-W
              WHEN 578 MOVE "NA" TO SECU-MOV-W
              WHEN 579 MOVE "NB" TO SECU-MOV-W
              WHEN 580 MOVE "NC" TO SECU-MOV-W
              WHEN 581 MOVE "ND" TO SECU-MOV-W
              WHEN 582 MOVE "NE" TO SECU-MOV-W
              WHEN 583 MOVE "NF" TO SECU-MOV-W
              WHEN 584 MOVE "NG" TO SECU-MOV-W
              WHEN 585 MOVE "NH" TO SECU-MOV-W
              WHEN 586 MOVE "NI" TO SECU-MOV-W
              WHEN 587 MOVE "NJ" TO SECU-MOV-W
              WHEN 588 MOVE "NK" TO SECU-MOV-W
              WHEN 589 MOVE "NL" TO SECU-MOV-W
              WHEN 590 MOVE "NM" TO SECU-MOV-W
              WHEN 591 MOVE "NN" TO SECU-MOV-W
              WHEN 592 MOVE "NO" TO SECU-MOV-W
              WHEN 593 MOVE "NP" TO SECU-MOV-W
              WHEN 594 MOVE "NQ" TO SECU-MOV-W
              WHEN 595 MOVE "NR" TO SECU-MOV-W
              WHEN 596 MOVE "NS" TO SECU-MOV-W
              WHEN 597 MOVE "NT" TO SECU-MOV-W
              WHEN 598 MOVE "NU" TO SECU-MOV-W
              WHEN 599 MOVE "NV" TO SECU-MOV-W
              WHEN 600 MOVE "NW" TO SECU-MOV-W
              WHEN 601 MOVE "NX" TO SECU-MOV-W
              WHEN 602 MOVE "NY" TO SECU-MOV-W
              WHEN 603 MOVE "NZ" TO SECU-MOV-W
              WHEN 604 MOVE "O0" TO SECU-MOV-W
              WHEN 605 MOVE "O1" TO SECU-MOV-W
              WHEN 606 MOVE "O2" TO SECU-MOV-W
              WHEN 607 MOVE "O3" TO SECU-MOV-W
              WHEN 608 MOVE "O4" TO SECU-MOV-W
              WHEN 609 MOVE "O5" TO SECU-MOV-W
              WHEN 610 MOVE "O6" TO SECU-MOV-W
              WHEN 611 MOVE "O7" TO SECU-MOV-W
              WHEN 612 MOVE "O8" TO SECU-MOV-W
              WHEN 613 MOVE "O9" TO SECU-MOV-W
              WHEN 614 MOVE "OA" TO SECU-MOV-W
              WHEN 615 MOVE "OB" TO SECU-MOV-W
              WHEN 616 MOVE "OC" TO SECU-MOV-W
              WHEN 617 MOVE "OD" TO SECU-MOV-W
              WHEN 618 MOVE "OE" TO SECU-MOV-W
              WHEN 619 MOVE "OF" TO SECU-MOV-W
              WHEN 620 MOVE "OG" TO SECU-MOV-W
              WHEN 621 MOVE "OH" TO SECU-MOV-W
              WHEN 622 MOVE "OI" TO SECU-MOV-W
              WHEN 623 MOVE "OJ" TO SECU-MOV-W
              WHEN 624 MOVE "OK" TO SECU-MOV-W
              WHEN 625 MOVE "OL" TO SECU-MOV-W
              WHEN 626 MOVE "OM" TO SECU-MOV-W
              WHEN 627 MOVE "ON" TO SECU-MOV-W
              WHEN 628 MOVE "OO" TO SECU-MOV-W
              WHEN 629 MOVE "OP" TO SECU-MOV-W
              WHEN 630 MOVE "OQ" TO SECU-MOV-W
              WHEN 631 MOVE "OR" TO SECU-MOV-W
              WHEN 632 MOVE "OS" TO SECU-MOV-W
              WHEN 633 MOVE "OT" TO SECU-MOV-W
              WHEN 634 MOVE "OU" TO SECU-MOV-W
              WHEN 635 MOVE "OV" TO SECU-MOV-W
              WHEN 636 MOVE "OW" TO SECU-MOV-W
              WHEN 637 MOVE "OX" TO SECU-MOV-W
              WHEN 638 MOVE "OY" TO SECU-MOV-W
              WHEN 639 MOVE "OZ" TO SECU-MOV-W
              WHEN 640 MOVE "P0" TO SECU-MOV-W
              WHEN 641 MOVE "P1" TO SECU-MOV-W
              WHEN 642 MOVE "P2" TO SECU-MOV-W
              WHEN 643 MOVE "P3" TO SECU-MOV-W
              WHEN 644 MOVE "P4" TO SECU-MOV-W
              WHEN 645 MOVE "P5" TO SECU-MOV-W
              WHEN 646 MOVE "P6" TO SECU-MOV-W
              WHEN 647 MOVE "P7" TO SECU-MOV-W
              WHEN 648 MOVE "P8" TO SECU-MOV-W
              WHEN 649 MOVE "P9" TO SECU-MOV-W
              WHEN 650 MOVE "PA" TO SECU-MOV-W
              WHEN 651 MOVE "PB" TO SECU-MOV-W
              WHEN 652 MOVE "PC" TO SECU-MOV-W
              WHEN 653 MOVE "PD" TO SECU-MOV-W
              WHEN 654 MOVE "PE" TO SECU-MOV-W
              WHEN 655 MOVE "PF" TO SECU-MOV-W
              WHEN 656 MOVE "PG" TO SECU-MOV-W
              WHEN 657 MOVE "PH" TO SECU-MOV-W
              WHEN 658 MOVE "PI" TO SECU-MOV-W
              WHEN 659 MOVE "PJ" TO SECU-MOV-W
              WHEN 660 MOVE "PK" TO SECU-MOV-W
              WHEN 661 MOVE "PL" TO SECU-MOV-W
              WHEN 662 MOVE "PM" TO SECU-MOV-W
              WHEN 663 MOVE "PN" TO SECU-MOV-W
              WHEN 664 MOVE "PO" TO SECU-MOV-W
              WHEN 665 MOVE "PP" TO SECU-MOV-W
              WHEN 666 MOVE "PQ" TO SECU-MOV-W
              WHEN 667 MOVE "PR" TO SECU-MOV-W
              WHEN 668 MOVE "PS" TO SECU-MOV-W
              WHEN 669 MOVE "PT" TO SECU-MOV-W
              WHEN 670 MOVE "PU" TO SECU-MOV-W
              WHEN 671 MOVE "PV" TO SECU-MOV-W
              WHEN 672 MOVE "PW" TO SECU-MOV-W
              WHEN 673 MOVE "PX" TO SECU-MOV-W
              WHEN 674 MOVE "PY" TO SECU-MOV-W
              WHEN 675 MOVE "PZ" TO SECU-MOV-W
              WHEN 676 MOVE "Q0" TO SECU-MOV-W
              WHEN 677 MOVE "Q1" TO SECU-MOV-W
              WHEN 678 MOVE "Q2" TO SECU-MOV-W
              WHEN 679 MOVE "Q3" TO SECU-MOV-W
              WHEN 680 MOVE "Q4" TO SECU-MOV-W
              WHEN 681 MOVE "Q5" TO SECU-MOV-W
              WHEN 682 MOVE "Q6" TO SECU-MOV-W
              WHEN 683 MOVE "Q7" TO SECU-MOV-W
              WHEN 684 MOVE "Q8" TO SECU-MOV-W
              WHEN 685 MOVE "Q9" TO SECU-MOV-W
              WHEN 686 MOVE "QA" TO SECU-MOV-W
              WHEN 687 MOVE "QB" TO SECU-MOV-W
              WHEN 688 MOVE "QC" TO SECU-MOV-W
              WHEN 689 MOVE "QD" TO SECU-MOV-W
              WHEN 690 MOVE "QE" TO SECU-MOV-W
              WHEN 691 MOVE "QF" TO SECU-MOV-W
              WHEN 692 MOVE "QG" TO SECU-MOV-W
              WHEN 693 MOVE "QH" TO SECU-MOV-W
              WHEN 694 MOVE "QI" TO SECU-MOV-W
              WHEN 695 MOVE "QJ" TO SECU-MOV-W
              WHEN 696 MOVE "QK" TO SECU-MOV-W
              WHEN 697 MOVE "QL" TO SECU-MOV-W
              WHEN 698 MOVE "QM" TO SECU-MOV-W
              WHEN 699 MOVE "QN" TO SECU-MOV-W
              WHEN 700 MOVE "QO" TO SECU-MOV-W
              WHEN 701 MOVE "QP" TO SECU-MOV-W
              WHEN 702 MOVE "QQ" TO SECU-MOV-W
              WHEN 703 MOVE "QR" TO SECU-MOV-W
              WHEN 704 MOVE "QS" TO SECU-MOV-W
              WHEN 705 MOVE "QT" TO SECU-MOV-W
              WHEN 706 MOVE "QU" TO SECU-MOV-W
              WHEN 707 MOVE "QV" TO SECU-MOV-W
              WHEN 708 MOVE "QW" TO SECU-MOV-W
              WHEN 709 MOVE "QX" TO SECU-MOV-W
              WHEN 710 MOVE "QY" TO SECU-MOV-W
              WHEN 711 MOVE "QZ" TO SECU-MOV-W
              WHEN 712 MOVE "R0" TO SECU-MOV-W
              WHEN 713 MOVE "R1" TO SECU-MOV-W
              WHEN 714 MOVE "R2" TO SECU-MOV-W
              WHEN 715 MOVE "R3" TO SECU-MOV-W
              WHEN 716 MOVE "R4" TO SECU-MOV-W
              WHEN 717 MOVE "R5" TO SECU-MOV-W
              WHEN 718 MOVE "R6" TO SECU-MOV-W
              WHEN 719 MOVE "R7" TO SECU-MOV-W
              WHEN 720 MOVE "R8" TO SECU-MOV-W
              WHEN 721 MOVE "R9" TO SECU-MOV-W
              WHEN 722 MOVE "RA" TO SECU-MOV-W
              WHEN 723 MOVE "RB" TO SECU-MOV-W
              WHEN 724 MOVE "RC" TO SECU-MOV-W
              WHEN 725 MOVE "RD" TO SECU-MOV-W
              WHEN 726 MOVE "RE" TO SECU-MOV-W
              WHEN 727 MOVE "RF" TO SECU-MOV-W
              WHEN 728 MOVE "RG" TO SECU-MOV-W
              WHEN 729 MOVE "RH" TO SECU-MOV-W
              WHEN 730 MOVE "RI" TO SECU-MOV-W
              WHEN 731 MOVE "RJ" TO SECU-MOV-W
              WHEN 732 MOVE "RK" TO SECU-MOV-W
              WHEN 733 MOVE "RL" TO SECU-MOV-W
              WHEN 734 MOVE "RM" TO SECU-MOV-W
              WHEN 735 MOVE "RN" TO SECU-MOV-W
              WHEN 736 MOVE "RO" TO SECU-MOV-W
              WHEN 737 MOVE "RP" TO SECU-MOV-W
              WHEN 738 MOVE "RQ" TO SECU-MOV-W
              WHEN 739 MOVE "RR" TO SECU-MOV-W
              WHEN 740 MOVE "RS" TO SECU-MOV-W
              WHEN 741 MOVE "RT" TO SECU-MOV-W
              WHEN 742 MOVE "RU" TO SECU-MOV-W
              WHEN 743 MOVE "RV" TO SECU-MOV-W
              WHEN 744 MOVE "RW" TO SECU-MOV-W
              WHEN 745 MOVE "RX" TO SECU-MOV-W
              WHEN 746 MOVE "RY" TO SECU-MOV-W
              WHEN 747 MOVE "RZ" TO SECU-MOV-W
              WHEN 748 MOVE "S0" TO SECU-MOV-W
              WHEN 749 MOVE "S1" TO SECU-MOV-W
              WHEN 750 MOVE "S2" TO SECU-MOV-W
              WHEN 751 MOVE "S3" TO SECU-MOV-W
              WHEN 752 MOVE "S4" TO SECU-MOV-W
              WHEN 753 MOVE "S5" TO SECU-MOV-W
              WHEN 754 MOVE "S6" TO SECU-MOV-W
              WHEN 755 MOVE "S7" TO SECU-MOV-W
              WHEN 756 MOVE "S8" TO SECU-MOV-W
              WHEN 757 MOVE "S9" TO SECU-MOV-W
              WHEN 758 MOVE "SA" TO SECU-MOV-W
              WHEN 759 MOVE "SB" TO SECU-MOV-W
              WHEN 760 MOVE "SC" TO SECU-MOV-W
              WHEN 761 MOVE "SD" TO SECU-MOV-W
              WHEN 762 MOVE "SE" TO SECU-MOV-W
              WHEN 763 MOVE "SF" TO SECU-MOV-W
              WHEN 764 MOVE "SG" TO SECU-MOV-W
              WHEN 765 MOVE "SH" TO SECU-MOV-W
              WHEN 766 MOVE "SI" TO SECU-MOV-W
              WHEN 767 MOVE "SJ" TO SECU-MOV-W
              WHEN 768 MOVE "SK" TO SECU-MOV-W
              WHEN 769 MOVE "SL" TO SECU-MOV-W
              WHEN 770 MOVE "SM" TO SECU-MOV-W
              WHEN 771 MOVE "SN" TO SECU-MOV-W
              WHEN 772 MOVE "SO" TO SECU-MOV-W
              WHEN 773 MOVE "SP" TO SECU-MOV-W
              WHEN 774 MOVE "SQ" TO SECU-MOV-W
              WHEN 775 MOVE "SR" TO SECU-MOV-W
              WHEN 776 MOVE "SS" TO SECU-MOV-W
              WHEN 777 MOVE "ST" TO SECU-MOV-W
              WHEN 778 MOVE "SU" TO SECU-MOV-W
              WHEN 779 MOVE "SV" TO SECU-MOV-W
              WHEN 780 MOVE "SW" TO SECU-MOV-W
              WHEN 781 MOVE "SX" TO SECU-MOV-W
              WHEN 782 MOVE "SY" TO SECU-MOV-W
              WHEN 783 MOVE "SZ" TO SECU-MOV-W
              WHEN 784 MOVE "T0" TO SECU-MOV-W
              WHEN 785 MOVE "T1" TO SECU-MOV-W
              WHEN 786 MOVE "T2" TO SECU-MOV-W
              WHEN 787 MOVE "T3" TO SECU-MOV-W
              WHEN 788 MOVE "T4" TO SECU-MOV-W
              WHEN 789 MOVE "T5" TO SECU-MOV-W
              WHEN 790 MOVE "T6" TO SECU-MOV-W
              WHEN 791 MOVE "T7" TO SECU-MOV-W
              WHEN 792 MOVE "T8" TO SECU-MOV-W
              WHEN 793 MOVE "T9" TO SECU-MOV-W
              WHEN 794 MOVE "TA" TO SECU-MOV-W
              WHEN 795 MOVE "TB" TO SECU-MOV-W
              WHEN 796 MOVE "TC" TO SECU-MOV-W
              WHEN 797 MOVE "TD" TO SECU-MOV-W
              WHEN 798 MOVE "TE" TO SECU-MOV-W
              WHEN 799 MOVE "TF" TO SECU-MOV-W
              WHEN 800 MOVE "TG" TO SECU-MOV-W
              WHEN 801 MOVE "TH" TO SECU-MOV-W
              WHEN 802 MOVE "TI" TO SECU-MOV-W
              WHEN 803 MOVE "TJ" TO SECU-MOV-W
              WHEN 804 MOVE "TK" TO SECU-MOV-W
              WHEN 805 MOVE "TL" TO SECU-MOV-W
              WHEN 806 MOVE "TM" TO SECU-MOV-W
              WHEN 807 MOVE "TN" TO SECU-MOV-W
              WHEN 808 MOVE "TO" TO SECU-MOV-W
              WHEN 809 MOVE "TP" TO SECU-MOV-W
              WHEN 810 MOVE "TQ" TO SECU-MOV-W
              WHEN 811 MOVE "TR" TO SECU-MOV-W
              WHEN 812 MOVE "TS" TO SECU-MOV-W
              WHEN 813 MOVE "TT" TO SECU-MOV-W
              WHEN 814 MOVE "TU" TO SECU-MOV-W
              WHEN 815 MOVE "TV" TO SECU-MOV-W
              WHEN 816 MOVE "TW" TO SECU-MOV-W
              WHEN 817 MOVE "TX" TO SECU-MOV-W
              WHEN 818 MOVE "TY" TO SECU-MOV-W
              WHEN 819 MOVE "TZ" TO SECU-MOV-W
              WHEN 820 MOVE "U0" TO SECU-MOV-W
              WHEN 821 MOVE "U1" TO SECU-MOV-W
              WHEN 822 MOVE "U2" TO SECU-MOV-W
              WHEN 823 MOVE "U3" TO SECU-MOV-W
              WHEN 824 MOVE "U4" TO SECU-MOV-W
              WHEN 825 MOVE "U5" TO SECU-MOV-W
              WHEN 826 MOVE "U6" TO SECU-MOV-W
              WHEN 827 MOVE "U7" TO SECU-MOV-W
              WHEN 828 MOVE "U8" TO SECU-MOV-W
              WHEN 829 MOVE "U9" TO SECU-MOV-W
              WHEN 830 MOVE "UA" TO SECU-MOV-W
              WHEN 831 MOVE "UB" TO SECU-MOV-W
              WHEN 832 MOVE "UC" TO SECU-MOV-W
              WHEN 833 MOVE "UD" TO SECU-MOV-W
              WHEN 834 MOVE "UE" TO SECU-MOV-W
              WHEN 835 MOVE "UF" TO SECU-MOV-W
              WHEN 836 MOVE "UG" TO SECU-MOV-W
              WHEN 837 MOVE "UH" TO SECU-MOV-W
              WHEN 838 MOVE "UI" TO SECU-MOV-W
              WHEN 839 MOVE "UJ" TO SECU-MOV-W
              WHEN 840 MOVE "UK" TO SECU-MOV-W
              WHEN 841 MOVE "UL" TO SECU-MOV-W
              WHEN 842 MOVE "UM" TO SECU-MOV-W
              WHEN 843 MOVE "UN" TO SECU-MOV-W
              WHEN 844 MOVE "UO" TO SECU-MOV-W
              WHEN 845 MOVE "UP" TO SECU-MOV-W
              WHEN 846 MOVE "UQ" TO SECU-MOV-W
              WHEN 847 MOVE "UR" TO SECU-MOV-W
              WHEN 848 MOVE "US" TO SECU-MOV-W
              WHEN 849 MOVE "UT" TO SECU-MOV-W
              WHEN 850 MOVE "UU" TO SECU-MOV-W
              WHEN 851 MOVE "UV" TO SECU-MOV-W
              WHEN 852 MOVE "UW" TO SECU-MOV-W
              WHEN 853 MOVE "UX" TO SECU-MOV-W
              WHEN 854 MOVE "UY" TO SECU-MOV-W
              WHEN 855 MOVE "UZ" TO SECU-MOV-W
              WHEN 856 MOVE "V0" TO SECU-MOV-W
              WHEN 857 MOVE "V1" TO SECU-MOV-W
              WHEN 858 MOVE "V2" TO SECU-MOV-W
              WHEN 859 MOVE "V3" TO SECU-MOV-W
              WHEN 860 MOVE "V4" TO SECU-MOV-W
              WHEN 861 MOVE "V5" TO SECU-MOV-W
              WHEN 862 MOVE "V6" TO SECU-MOV-W
              WHEN 863 MOVE "V7" TO SECU-MOV-W
              WHEN 864 MOVE "V8" TO SECU-MOV-W
              WHEN 865 MOVE "V9" TO SECU-MOV-W
              WHEN 866 MOVE "VA" TO SECU-MOV-W
              WHEN 867 MOVE "VB" TO SECU-MOV-W
              WHEN 868 MOVE "VC" TO SECU-MOV-W
              WHEN 869 MOVE "VD" TO SECU-MOV-W
              WHEN 870 MOVE "VE" TO SECU-MOV-W
              WHEN 871 MOVE "VF" TO SECU-MOV-W
              WHEN 872 MOVE "VG" TO SECU-MOV-W
              WHEN 873 MOVE "VH" TO SECU-MOV-W
              WHEN 874 MOVE "VI" TO SECU-MOV-W
              WHEN 875 MOVE "VJ" TO SECU-MOV-W
              WHEN 876 MOVE "VK" TO SECU-MOV-W
              WHEN 877 MOVE "VL" TO SECU-MOV-W
              WHEN 878 MOVE "VM" TO SECU-MOV-W
              WHEN 879 MOVE "VN" TO SECU-MOV-W
              WHEN 880 MOVE "VO" TO SECU-MOV-W
              WHEN 881 MOVE "VP" TO SECU-MOV-W
              WHEN 882 MOVE "VQ" TO SECU-MOV-W
              WHEN 883 MOVE "VR" TO SECU-MOV-W
              WHEN 884 MOVE "VS" TO SECU-MOV-W
              WHEN 885 MOVE "VT" TO SECU-MOV-W
              WHEN 886 MOVE "VU" TO SECU-MOV-W
              WHEN 887 MOVE "VV" TO SECU-MOV-W
              WHEN 888 MOVE "VW" TO SECU-MOV-W
              WHEN 889 MOVE "VX" TO SECU-MOV-W
              WHEN 890 MOVE "VY" TO SECU-MOV-W 
              WHEN 891 MOVE "VZ" TO SECU-MOV-W
              WHEN 892 MOVE "W0" TO SECU-MOV-W
              WHEN 893 MOVE "W1" TO SECU-MOV-W
              WHEN 894 MOVE "W2" TO SECU-MOV-W
              WHEN 895 MOVE "W3" TO SECU-MOV-W
              WHEN 896 MOVE "W4" TO SECU-MOV-W
              WHEN 897 MOVE "W5" TO SECU-MOV-W
              WHEN 898 MOVE "W6" TO SECU-MOV-W
              WHEN 899 MOVE "W7" TO SECU-MOV-W
              WHEN 900 MOVE "W8" TO SECU-MOV-W
              WHEN 901 MOVE "W9" TO SECU-MOV-W
              WHEN 902 MOVE "WA" TO SECU-MOV-W
              WHEN 903 MOVE "WB" TO SECU-MOV-W
              WHEN 904 MOVE "WC" TO SECU-MOV-W
              WHEN 905 MOVE "WD" TO SECU-MOV-W
              WHEN 906 MOVE "WE" TO SECU-MOV-W
              WHEN 907 MOVE "WF" TO SECU-MOV-W
              WHEN 908 MOVE "WG" TO SECU-MOV-W
              WHEN 909 MOVE "WH" TO SECU-MOV-W
              WHEN 910 MOVE "WI" TO SECU-MOV-W
              WHEN 911 MOVE "WJ" TO SECU-MOV-W
              WHEN 912 MOVE "WK" TO SECU-MOV-W
              WHEN 913 MOVE "WL" TO SECU-MOV-W
              WHEN 914 MOVE "WM" TO SECU-MOV-W
              WHEN 915 MOVE "WN" TO SECU-MOV-W
              WHEN 916 MOVE "WO" TO SECU-MOV-W
              WHEN 917 MOVE "WP" TO SECU-MOV-W
              WHEN 918 MOVE "WQ" TO SECU-MOV-W
              WHEN 919 MOVE "WR" TO SECU-MOV-W
              WHEN 920 MOVE "WS" TO SECU-MOV-W
              WHEN 921 MOVE "WT" TO SECU-MOV-W
              WHEN 922 MOVE "WU" TO SECU-MOV-W
              WHEN 923 MOVE "WV" TO SECU-MOV-W
              WHEN 924 MOVE "WW" TO SECU-MOV-W
              WHEN 925 MOVE "WX" TO SECU-MOV-W
              WHEN 926 MOVE "WY" TO SECU-MOV-W
              WHEN 927 MOVE "WZ" TO SECU-MOV-W
              WHEN 928 MOVE "X0" TO SECU-MOV-W
              WHEN 929 MOVE "X1" TO SECU-MOV-W
              WHEN 930 MOVE "X2" TO SECU-MOV-W
              WHEN 931 MOVE "X3" TO SECU-MOV-W
              WHEN 932 MOVE "X4" TO SECU-MOV-W
              WHEN 933 MOVE "X5" TO SECU-MOV-W
              WHEN 934 MOVE "X6" TO SECU-MOV-W
              WHEN 935 MOVE "X7" TO SECU-MOV-W
              WHEN 936 MOVE "X8" TO SECU-MOV-W
              WHEN 937 MOVE "X9" TO SECU-MOV-W
              WHEN 938 MOVE "XA" TO SECU-MOV-W
              WHEN 939 MOVE "XB" TO SECU-MOV-W
              WHEN 940 MOVE "XC" TO SECU-MOV-W
              WHEN 941 MOVE "XD" TO SECU-MOV-W
              WHEN 942 MOVE "XE" TO SECU-MOV-W
              WHEN 943 MOVE "XF" TO SECU-MOV-W
              WHEN 944 MOVE "XG" TO SECU-MOV-W
              WHEN 945 MOVE "XH" TO SECU-MOV-W
              WHEN 946 MOVE "XI" TO SECU-MOV-W
              WHEN 947 MOVE "XJ" TO SECU-MOV-W
              WHEN 948 MOVE "XK" TO SECU-MOV-W
              WHEN 949 MOVE "XL" TO SECU-MOV-W
              WHEN 950 MOVE "XM" TO SECU-MOV-W
              WHEN 951 MOVE "XN" TO SECU-MOV-W
              WHEN 952 MOVE "XO" TO SECU-MOV-W
              WHEN 953 MOVE "XP" TO SECU-MOV-W
              WHEN 954 MOVE "XQ" TO SECU-MOV-W
              WHEN 955 MOVE "XR" TO SECU-MOV-W
              WHEN 956 MOVE "XS" TO SECU-MOV-W
              WHEN 957 MOVE "XT" TO SECU-MOV-W
              WHEN 958 MOVE "XU" TO SECU-MOV-W
              WHEN 959 MOVE "XV" TO SECU-MOV-W
              WHEN 960 MOVE "XW" TO SECU-MOV-W
              WHEN 961 MOVE "XX" TO SECU-MOV-W
              WHEN 962 MOVE "XY" TO SECU-MOV-W
              WHEN 963 MOVE "XZ" TO SECU-MOV-W
              WHEN 964 MOVE "Y0" TO SECU-MOV-W
              WHEN 965 MOVE "Y1" TO SECU-MOV-W
              WHEN 966 MOVE "Y2" TO SECU-MOV-W
              WHEN 967 MOVE "Y3" TO SECU-MOV-W
              WHEN 968 MOVE "Y4" TO SECU-MOV-W
              WHEN 969 MOVE "Y5" TO SECU-MOV-W
              WHEN 970 MOVE "Y6" TO SECU-MOV-W
              WHEN 971 MOVE "Y7" TO SECU-MOV-W
              WHEN 972 MOVE "Y8" TO SECU-MOV-W
              WHEN 973 MOVE "Y9" TO SECU-MOV-W
              WHEN 974 MOVE "YA" TO SECU-MOV-W
              WHEN 975 MOVE "YB" TO SECU-MOV-W
              WHEN 976 MOVE "YC" TO SECU-MOV-W
              WHEN 977 MOVE "YD" TO SECU-MOV-W
              WHEN 978 MOVE "YE" TO SECU-MOV-W
              WHEN 979 MOVE "YF" TO SECU-MOV-W
              WHEN 980 MOVE "YG" TO SECU-MOV-W
              WHEN 981 MOVE "YH" TO SECU-MOV-W 
              WHEN 982 MOVE "YI" TO SECU-MOV-W
              WHEN 983 MOVE "YJ" TO SECU-MOV-W
              WHEN 984 MOVE "YK" TO SECU-MOV-W
              WHEN 985 MOVE "YL" TO SECU-MOV-W
              WHEN 986 MOVE "YM" TO SECU-MOV-W
              WHEN 987 MOVE "YN" TO SECU-MOV-W
              WHEN 988 MOVE "YO" TO SECU-MOV-W
              WHEN 989 MOVE "YP" TO SECU-MOV-W
              WHEN 990 MOVE "YQ" TO SECU-MOV-W 
              WHEN 991 MOVE "YR" TO SECU-MOV-W
              WHEN 992 MOVE "YS" TO SECU-MOV-W
              WHEN 993 MOVE "YT" TO SECU-MOV-W
              WHEN 994 MOVE "YU" TO SECU-MOV-W
              WHEN 995 MOVE "YV" TO SECU-MOV-W
              WHEN 996 MOVE "YW" TO SECU-MOV-W
              WHEN 997 MOVE "YX" TO SECU-MOV-W
              WHEN 998 MOVE "YY" TO SECU-MOV-W
              WHEN 999 MOVE "YZ" TO SECU-MOV-W.
 
005210* COPY "..\..\FUENTES\WS-HCEVO.CBL".
