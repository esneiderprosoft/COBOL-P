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
   				  
005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-INVEN.CBL".

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
       01 SW-OK                         PIC 9.

       77 MES-SAL-W                     PIC X(4).
       77  LLAVE-W                      PIC X(7).
       77  SW9                          PIC 9.
       01  COMPROB-X.
           02 COMPROB1-X                PIC X.
           02 COMPROB2-X                PIC 9(4).

       77 NOM-USUAR-W                   PIC X(70).
       77 NOM-INVENT-W                  PIC X(70).

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
              MOVE "INV030V"                TO MSJ3-HTML
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
              MOVE "INV030V"                TO MSJ3-HTML
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
              MOVE "INV030V"                TO MSJ3-HTML
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
              MOVE "INV030V"                TO MSJ3-HTML
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
                    LLAVE-FACT-W, SW-OK
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
            
            INITIALIZE NOM-INVENT-W
                       NOM-USUAR-W

            MOVE "\\" TO NOM-INVENT-W
                         NOM-USUAR-W

            INSPECT NOM-INVENT-W REPLACING FIRST "                    "
                                              BY IP-DATOS-USUNET                              
001621      INSPECT NOM-INVENT-W REPLACING FIRST " "
001624                                        BY "\"
007190      INSPECT NOM-INVENT-W REPLACING FIRST "                              "
                                              BY DIR-CONTAB-LLEGADA-W
            INSPECT NOM-INVENT-W REPLACING FIRST "                      "
007300                                        BY "\CONTROL\SC-MOVINV.DAT".
            MOVE NOM-INVENT-W TO NOM-USUAR-W
            INSPECT NOM-USUAR-W REPLACING FIRST "SC-MOVINV.DAT "
                                             BY "SC-ARCHUSU.DAT".
       ABRIR-USUARIO.
            OPEN INPUT ARCHIVO-USUARIOS.

            READ ARCHIVO-USUARIOS NEXT WITH NO LOCK
            END-READ.

            CLOSE ARCHIVO-USUARIOS.
            OPEN INPUT MOVIMIENTO-INVENT.

       LEER-FACTURA.
             IF SUC-FACT-W IS NOT = PREFIJO-USU
             OR NIT-USU = 800162035
                MOVE SUC-FACT-W TO COD-TRANS-INV
                MOVE 2          TO COD1-TRANS-INV
                IF (NIT-USU = 830512772 OR 845000038)
                EVALUATE  SUC-FACT-W
                WHEN 01 MOVE 20   TO COD-TRANS-INV
                WHEN 02 MOVE 20   TO COD-TRANS-INV
                WHEN 03 MOVE 22   TO COD-TRANS-INV
                WHEN 04 MOVE 23   TO COD-TRANS-INV
                WHEN 05 MOVE 24   TO COD-TRANS-INV
                WHEN 06 MOVE 25   TO COD-TRANS-INV
                WHEN 07 MOVE 26   TO COD-TRANS-INV
                WHEN 08 MOVE 27   TO COD-TRANS-INV
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
             END-IF.

             PERFORM EDITAR-1ER-DIGITO.

             MOVE NRO-FACT2-W      TO COMPROB2-X.
             MOVE COMPROB-X        TO COMPROB-INV.
             MOVE LLAVE-SEC-INV    TO LLAVE-W.

       BUSCAR-MOVIMIENTO.
             START MOVIMIENTO-INVENT KEY = LLAVE-SEC-INV
                   INVALID KEY
                     MOVE "01"  TO SW-INVALID-ENV
                   NOT INVALID KEY
                     MOVE "00"  TO SW-INVALID-ENV
             END-START.

       CERRAR-ARCHIVOS.
            CLOSE MOVIMIENTO-INVENT.

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