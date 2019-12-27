      *============================================================
      * CREA ARCHIVO JSON PARA VENTANA CONSULTA DE ARTICULOS
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
                  ASSIGN NOM-USUAR-W
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

           SELECT ARCHIVO-LOCALIZACION LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-LOCAL-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS COD-LOCAL
                  ALTERNATE RECORD KEY IS LLAVE-ALT1-LOCAL WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LLAVE-ALT2-LOCAL WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-COSTO LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-COSTO-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS COD-COSTO
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-SALDOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-SAL-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-SAL
                  ALTERNATE RECORD KEY COD-ART-SAL  WITH DUPLICATES
                  ALTERNATE RECORD KEY COD-LOTE-SAL WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-USO LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-USO-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS LLAVE-USO
                  ALTERNATE RECORD KEY IS DESCRIP-USO WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-PRINTER
                  ASSIGN TO NOM-PRINTER-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS OTR-STAT.


    	   SELECT ARCHIVO-PREFIJOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PREFI-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS LLAVE-PREF
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-JSON LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.
                                                          
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-ARTIC.CBL".
       COPY "..\..\FUENTES\FD-LOCAL.CBL".
       COPY "..\..\FUENTES\FD-COSTO.CBL".
       COPY "..\..\FUENTES\FD-SALDO.CBL".
       COPY "..\..\FUENTES\FD-USOAR.CBL".
       COPY "..\..\FUENTES\FD-IMPRE.CBL".
       COPY "..\..\FUENTES\FD-PREFI.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-PLANO.
           02 DATOS-PLANO     PIC X(500).
                  
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LLEGADA-W                    PIC X(100).

       77 NOM-ART-W                    PIC X(70).
       77 NOM-SAL-W                    PIC X(70).
       77 NOM-USUAR-W                  PIC X(70).
       77 NOM-LOCAL-W                  PIC X(70).
       77 NOM-COSTO-W                  PIC X(70).
       77 NOM-USO-W                    PIC X(70).
       77 NOM-PRINTER-W                PIC X(70).
       77 NOM-PREFI-W                  PIC X(70).
       77 NOM-PLANO-W                  PIC X(90).
      
       01  ALMACEN-W.
           02 ALM1-W                PIC X(3).
           02 ALM2-W                PIC 99.

      
       77  SUC-TMP                   PIC X.
       77  SW-VAR                    PIC 9.
       77  K                         PIC 99.
       77  MES-SAL-W                 PIC X(4).
       77  TAR-W                     PIC 99V9.
       77  VALOR-VENTA               PIC 9(12).
       77  IVA-TMP                   PIC 9(12).
       77  EXISTENCIAS-TMP           PIC S9(12)V99.
       77  NRO-EDIT                  PIC ZZZZZZZZZZZZ.
       77  PROCENTAJE-EDIT           PIC ZZ.Z.
       77  SW-USO                    PIC XX.


       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(21).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 COD-ART-LLEGADA-W         PIC X(33).
          02 ADMIN-LLEGADA-W           PIC X(4).



       01 LIN-1.  
          02 FILLER                  PIC X(7)   VALUE "{*TIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-ART-J              PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*GRP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 GRUPO-ART-J             PIC X(2).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*NUMERO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NUMERO-ART-J            PIC X(13).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(8)   VALUE "*CLASE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLASE-ART-J             PIC X(2).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-ART-J           PIC X(50).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(7)   VALUE "*UNID*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UNIDAD-ART-J            PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*VALOR*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VALOR-ART-J             PIC X(15).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*REFER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 REFER-ART-J             PIC X(15).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*SALDO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SALDO-ART-J             PIC X(12).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*BARRA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 BARRAS-ART-J            PIC X(17).
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-LIN-1            PIC X.

      
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
              MOVE "INV803"                 TO MSJ3-HTML
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
              MOVE "INV803"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF. 

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PLANO-W              TO MSJ2-HTML
              MOVE "INV803"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
002580     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.         
002590 ESCR-EXCEPTIONES.
002600     IF OTR-STAT = "00" 
002610        CONTINUE 
002620     ELSE
002630        MOVE OTR-STAT                 TO MSJ1-HTML
002640        MOVE NOM-USUAR-W              TO MSJ2-HTML
002650        MOVE "INV803"                 TO MSJ3-HTML
002660        GO TO ENVIAR2-ERROR
002670     END-IF.

002694 I-O-TEST SECTION.
002700     USE AFTER EXCEPTION PROCEDURE ON MAESTRO-ARTICULOS.
002710 ESCR-EXCEPTIONES.
002720     IF OTR-STAT = "00" OR "35"
002730        CONTINUE 
002740     ELSE
002750        MOVE OTR-STAT                 TO MSJ1-HTML
002760        MOVE NOM-ART-W                TO MSJ2-HTML
002770        MOVE "INV803"                 TO MSJ3-HTML
002780        GO TO ENVIAR2-ERROR
002790     END-IF.
       I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-LOCALIZACION.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT                 TO MSJ1-HTML
006670        MOVE NOM-LOCAL-W              TO MSJ2-HTML
006680        MOVE "INV803"                 TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.
006590  
006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-COSTO.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT                 TO MSJ1-HTML
006670        MOVE NOM-COSTO-W              TO MSJ2-HTML
006680        MOVE "INV803"                 TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.
006590  
006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USO.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT                 TO MSJ1-HTML
006670        MOVE NOM-USO-W                TO MSJ2-HTML
006680        MOVE "INV803"                 TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.
006590  
006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PRINTER.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT                 TO MSJ1-HTML
006670        MOVE NOM-PRINTER-W            TO MSJ2-HTML
006680        MOVE "INV803"                 TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.
006590  
006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PREFIJOS.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" OR "35"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT                 TO MSJ1-HTML
006670        MOVE NOM-PREFI-W              TO MSJ2-HTML
006680        MOVE "INV803"                 TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.
006590  
006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SALDOS.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" OR "35"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT                 TO MSJ1-HTML
006670        MOVE NOM-SAL-W                TO MSJ2-HTML
006680        MOVE "INV803"                 TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, COD-ART-LLEGADA-W, ADMIN-LLEGADA-W                 
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

           INITIALIZE NOM-ART-W NOM-SAL-W NOM-PLANO-W NOM-USUAR-W NOM-LOCAL-W
                     NOM-COSTO-W NOM-USO-W NOM-PRINTER-W NOM-PREFI-W

           EVALUATE MES-TOTAL
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
007287     MOVE "\\" TO NOM-ART-W
007288     INSPECT NOM-ART-W REPLACING FIRST "                    "
007289                                    BY IP-DATOS-USUNET
007290     INSPECT NOM-ART-W REPLACING FIRST " "
007291                                    BY "\"
007292     INSPECT NOM-ART-W REPLACING FIRST "                     "
007293                                    BY DIR-CONTAB-LLEGADA-W.
007294     INSPECT NOM-ART-W REPLACING FIRST "                       "
007295                                    BY "\CONTROL\SC-MAESART.DAT".

           MOVE NOM-ART-W    TO NOM-USUAR-W NOM-LOCAL-W NOM-COSTO-W NOM-USO-W NOM-PREFI-W


           INSPECT NOM-USUAR-W REPLACING FIRST "SC-MAESART.DAT"
                                            BY "SC-ARCHUSU.DAT".

           INSPECT NOM-LOCAL-W REPLACING FIRST "SC-MAESART.DAT"
                                            BY "SC-ALMAC.DAT  ".

           INSPECT NOM-COSTO-W REPLACING FIRST "SC-MAESART.DAT"
                                            BY "SC-ARCHCOS.DAT".

           INSPECT NOM-USO-W   REPLACING FIRST "SC-MAESART.DAT"
                                            BY "SC-ARCHUSO.DAT".

           INSPECT NOM-PREFI-W REPLACING FIRST "SC-MAESART.DAT "
                                            BY "SC-ARCHPREF.DAT".  
                                                          
           INITIALIZE NOM-PLANO-W
           MOVE "\\" TO NOM-PLANO-W

           INSPECT NOM-PLANO-W REPLACING FIRST "                    "
                                         BY IP-DATOS-USUNET

           INSPECT NOM-PLANO-W REPLACING FIRST  "                 "
                                            BY  "\PROG\DATOS\JSON\" 
      
           INSPECT NOM-PLANO-W  REPLACING FIRST "                                              "
                                             BY "JSC-MAESART-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"

           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                             BY LLAVE-SESION-LLEGA-W.  

           MOVE "\\" TO NOM-SAL-W 
007288     INSPECT NOM-SAL-W REPLACING FIRST "                    "
007289                                    BY IP-DATOS-USUNET
007290     INSPECT NOM-SAL-W REPLACING FIRST " "
007291                                    BY "\"
007292     INSPECT NOM-SAL-W REPLACING FIRST "                     "
007293                                    BY DIR-CONTAB-LLEGADA-W.
007290     INSPECT NOM-SAL-W REPLACING FIRST "    "
007291                                    BY MES-SAL-W
007290     INSPECT NOM-SAL-W REPLACING FIRST "             "
007291                                    BY "\SC-SALDO.DAT".


           MOVE "C:\PROSOFT\PRINTER.PSC"     TO NOM-PRINTER-W.

       ABRIR-USUARIOS.

           OPEN INPUT ARCHIVO-USUARIOS
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.

       BUSCAR-SUSCURSAL.
           OPEN INPUT ARCHIVO-PRINTER
           READ ARCHIVO-PRINTER NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-PRINTER
           END-READ.

           IF SUCURSAL-PRN IS NOT NUMERIC
              MOVE 00 TO SUCURSAL-PRN
           END-IF.

       ABRIR-PREFIJOS.
           OPEN INPUT ARCHIVO-PREFIJOS.
           EVALUATE OTR-STAT
               WHEN "00"  CONTINUE
               WHEN "35"  INITIALIZE REG-PREF
                    MOVE  1 TO DEF-PREF
                    MOVE  PREFIJO-USU    TO COD-PREF     (1)
                    MOVE  "Principal"    TO DESCRIP-PREF (1)
                    MOVE  RESOL-DIAN-USU TO RESOL-PREF   (1)
                    MOVE  "ALM01"        TO ALM-PREF     (1)
                    OPEN  OUTPUT ARCHIVO-PREFIJOS
                    WRITE REG-PREF END-WRITE
                    CLOSE ARCHIVO-PREFIJOS
                    GO TO ABRIR-PREFIJOS
           END-EVALUATE.

       LEER-PREFIJOS.
           READ ARCHIVO-PREFIJOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-PREFIJOS
           END-READ.


       ABRIR-SALDOS.

           OPEN INPUT ARCHIVO-SALDOS.
     
           EVALUATE OTR-STAT
               WHEN  "00" GO TO ABRIR-ARCHIVOS
               WHEN  "35" OPEN OUTPUT ARCHIVO-SALDOS
                          CLOSE  ARCHIVO-SALDOS
                          GO TO ABRIR-SALDOS
               WHEN  OTHER GO TO ABRIR-SALDOS
           END-EVALUATE.


       ABRIR-ARCHIVOS.
           OPEN INPUT MAESTRO-ARTICULOS
                      ARCHIVO-USO.
     
           IF NIT-USU = 800202522
              MOVE "PTO01" TO ALMACEN-W
           ELSE
              MOVE "ALM01" TO ALMACEN-W
           END-IF.

       LEER-ALMACEN.
           OPEN INPUT ARCHIVO-LOCALIZACION.
           MOVE ALMACEN-W TO COD-LOCAL

           READ ARCHIVO-LOCALIZACION WITH NO LOCK
                INVALID KEY 
                  MOVE ALL "*" TO NOMBRE-LOCAL
           END-READ
                        
           CLOSE ARCHIVO-LOCALIZACION.

           MOVE "**"     TO SW-USO.


       ABRIR-ARTICULOS.
008123     OPEN OUTPUT ARCHIVO-JSON.
008150
008160     INITIALIZE REG-MAESTRO REG-PLANO.
008170     MOVE "{*ARTICULOS*:[" TO  DATOS-PLANO
008180     INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
008190     WRITE REG-PLANO.
008200     MOVE 0 TO SW-FIN.
008210 
008220 LEER-ARTICULOS.
008230     IF SW-FIN = 0
008240        MOVE 1 TO SW-FIN
008250     ELSE
008260        MOVE LIN-1 TO DATOS-PLANO
008270        WRITE REG-PLANO END-WRITE
008280     END-IF
008290
008300     READ MAESTRO-ARTICULOS NEXT WITH NO LOCK AT END
                GO TO CERRAR-ARTICULOS
           END-READ.
008310     
           IF COD-ART = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE
              IF SW-USO  IS NOT = "**" AND USO-ART IS NOT = SW-USO
                 MOVE 0  TO SW-FIN
                 GO TO LEER-ARTICULOS
              END-IF

              IF  DESCRIP1-ART = "*"
008382            MOVE 0 TO SW-FIN
008383        ELSE
008390            INITIALIZE LIN-1
                  MOVE GRUPO-ART       TO GRUPO-ART-J
008391            MOVE NUMERO-ART      TO NUMERO-ART-J

                  INSPECT NUMERO-ART-J REPLACING ALL "*" BY "x"
                  INSPECT NUMERO-ART-J REPLACING ALL '"' BY " "
                  INSPECT NUMERO-ART-J REPLACING ALL "'" BY " "

                  MOVE CLASE-ART        TO CLASE-ART-J

008393            MOVE DESCRIP-ART      TO DESCRIP-ART-J
008394            INSPECT DESCRIP-ART-J REPLACING ALL "*" BY "x"
                  INSPECT DESCRIP-ART-J REPLACING ALL "\" BY "/"
                  INSPECT DESCRIP-ART-J REPLACING ALL CARAC-COMILLA BY "p"

                  MOVE UNIDAD-ART       TO UNIDAD-ART-J

                  INSPECT UNIDAD-ART-J REPLACING ALL "*" BY "x"
                  INSPECT UNIDAD-ART-J REPLACING ALL '"' BY " "
                  INSPECT UNIDAD-ART-J REPLACING ALL "'" BY " "


                  IF IVA-ART = "N"   
005204               MOVE 0 TO IVA-ART
005205            END-IF

005206            IF IVA-ART = "S"   
005207               MOVE 1 TO IVA-ART
005208            END-IF
008395
008399            EVALUATE IVA-ART
                    WHEN 0 MOVE 0 TO TAR-W
008400              WHEN 1 IF IVA-USU = 0
                              MOVE 0 TO IVA-ART TAR-W
                           ELSE
                              MOVE IVA-USU TO TAR-W
                           END-IF
008401              WHEN 2 IF IVA-2-USU = 0
                              MOVE 0 TO IVA-ART TAR-W
                           ELSE
                              MOVE IVA-2-USU TO TAR-W
                           END-IF
008402              WHEN 3 IF IVA-3-USU = 0
                              MOVE 0 TO IVA-ART TAR-W
                           ELSE
                              MOVE IVA-3-USU TO TAR-W
                           END-IF
008404            END-EVALUATE

                  IF TAR-W = 1
                     MOVE 1.6 TO TAR-W
                  END-IF

                  PERFORM LEER-SALDO

                  COMPUTE VALOR-VENTA ROUNDED = VR-VENTA1-ART(1) * (100 + TAR-W) / 100

                  MOVE VALOR-VENTA TO IVA-TMP
                  MOVE IVA-TMP     TO VALOR-ART-J

                  MOVE COD-BARRAS-ART TO BARRAS-ART-J
                  MOVE REF-ART        TO REFER-ART-J

                  INSPECT REFER-ART-J REPLACING ALL "*" BY "x"
                  INSPECT REFER-ART-J REPLACING ALL '"' BY " "
                  INSPECT REFER-ART-J REPLACING ALL "'" BY " "

005883            IF EXISTENCIAS-TMP = ZERO OR ZEROS 
005884               MOVE "0" TO SALDO-ART-J
005885            ELSE
005886               MOVE EXISTENCIAS-TMP TO NRO-EDIT
005887               MOVE NRO-EDIT        TO SALDO-ART-J
005888            END-IF

008810            MOVE "," TO CIERRE-LIN-1
008830        END-IF
008840     END-IF
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
008850     GO TO LEER-ARTICULOS.
008860
008870 CERRAR-ARTICULOS.
008880     INITIALIZE CIERRE-LIN-1
008890     MOVE LIN-1 TO DATOS-PLANO
008900     WRITE REG-PLANO.
008910
008920     MOVE "]}" TO DATOS-PLANO
008930     WRITE REG-PLANO.
008940          
008950     CLOSE MAESTRO-ARTICULOS
                 ARCHIVO-SALDOS
008960           ARCHIVO-JSON.

       ENVIO-DATOS.
           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE "00"             TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3
         
           MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
           PERFORM ABRIR-HTML.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM. 

       LEER-SALDO.
           MOVE ALMACEN-W  TO COD-ALM-SAL
007971     MOVE COD-ART    TO COD-ART-SAL
007972     START ARCHIVO-SALDOS KEY = COD-ART-SAL
007973           INVALID KEY 
007974                       MOVE ZEROS TO EXISTENCIAS-TMP
007979           NOT INVALID KEY  
007980                       INITIALIZE VARIABLES-SALDOS-W
007981                       PERFORM CALCULAR-SALDO-ACTUAL 
                             VARYING DIA-SDO-W FROM 1 BY 1
                                               UNTIL DIA-SDO-W > 31  
007983                       MOVE SDO-ACT-CANT-W TO EXISTENCIAS-TMP
007984     END-START.
007990
008000 CALCULAR-SALDO-ACTUAL.
008010     IF DIA-SDO-W = 1
008020        ADD  ACUM-ENT-CANT     (32)    TO SDO-ACT-CANT-W SDO-INI-CANT-W
008030        ADD  ACUM-ENT-VLR      (32)    TO SDO-ACT-VLR-W  SDO-INI-VLR-W
008040        ADD  DEP-ENT-VLR       (32)    TO SDO-ACT-DEP-W  SDO-INI-DEP-W
008050        ADD  INF-ENT-VLR       (32)    TO SDO-ACT-INF-W  SDO-INI-INF-W
008060        ADD  INFD-ENT-VLR      (32)    TO SDO-ACT-INFD-W SDO-INI-INFD-W
008070     END-IF
008080
008090     ADD ACUM-ENT-CANT (DIA-SDO-W) TO SDO-AC-CANT-ENT-W SDO-ACT-CANT-W
008100     ADD ACUM-ENT-VLR  (DIA-SDO-W) TO SDO-AC-VLR-ENT-W  SDO-ACT-VLR-W
008110     ADD DEP-ENT-VLR   (DIA-SDO-W) TO SDO-AC-DEP-ENT-W  SDO-ACT-DEP-W
008120     ADD INF-ENT-VLR   (DIA-SDO-W) TO SDO-AC-INF-ENT-W  SDO-ACT-INF-W 
008130     ADD INFD-ENT-VLR  (DIA-SDO-W) TO SDO-AC-INFD-ENT-W SDO-ACT-INFD-W.
008140     
008150     ADD ACUM-SAL-CANT (DIA-SDO-W) TO SDO-AC-CANT-SAL-W 
008160     ADD ACUM-SAL-VLR  (DIA-SDO-W) TO SDO-AC-VLR-SAL-W  
008170     ADD DEP-SAL-VLR   (DIA-SDO-W) TO SDO-AC-DEP-SAL-W  
008180     ADD INF-SAL-VLR   (DIA-SDO-W) TO SDO-AC-INF-SAL-W  
008190     ADD INFD-SAL-VLR  (DIA-SDO-W) TO SDO-AC-INFD-SAL-W.
008200
008210     SUBTRACT ACUM-SAL-CANT (DIA-SDO-W) FROM SDO-ACT-CANT-W
008220     SUBTRACT ACUM-SAL-VLR  (DIA-SDO-W) FROM SDO-ACT-VLR-W
008230     SUBTRACT DEP-SAL-VLR   (DIA-SDO-W) FROM SDO-ACT-DEP-W
008240     SUBTRACT INF-SAL-VLR   (DIA-SDO-W) FROM SDO-ACT-INF-W 
008250     SUBTRACT INFD-SAL-VLR  (DIA-SDO-W) FROM SDO-ACT-INFD-W.

           
      
       COPY "..\..\FUENTES\SC-WEB19.CBL".
      
