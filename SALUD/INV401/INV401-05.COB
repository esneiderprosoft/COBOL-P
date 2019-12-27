004820*=================================================================
004830* CONSULTA DE ARCHIVO TARIFAS Y NUMERACION.
      * CREACION JSON DE TABLA-TAR
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "..\..\FUENTES\FS-USUNET.CBL".
004920     COPY "..\..\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-TARIFAS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TARIF-W
      *"../CONTROL/SC-ARCHTAR.DAT"
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS COD-TAR.

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

            SELECT ARCHIVO-JSON LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.

005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-TARIF.CBL".	   
       COPY "..\..\FUENTES\FD-NUMER19.CBL".	 

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON                PIC X(500).

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
          02 COD-TAR-LLEGADA-W.
             03 COD1-TAR-LLEGADA-W      PIC X.
             03 COD2-TAR-LLEGADA-W      PIC X.
          02 LLAVE-LLEGADA-W.
             05 PREFIJO-LLEGADA-W       PIC X.
             05 NRO-LLEGADA-W           PIC X(6).

       77 MES-SAL-W                     PIC X(4).
       77 NOM-TARIF-W                   PIC X(70).
       77 NOM-NUMER-W                   PIC X(70).
       77 NOM-PLANO-W                   PIC X(70).
       01 HN-QUIR-TAR-EDIT              PIC ZZZZZZZZZ.ZZ.
       01 HN-AYUD-TAR-EDIT              PIC ZZZZZZZZZ.ZZ.
       01 HN-ANES-TAR-EDIT              PIC ZZZZZZZZZ.ZZ.
      * 01 HN-QUIR-TAR-EDIT-J            PIC ZZZZZZZZZ.ZZ.
      * 01 HN-AYUD-TAR-EDIT-J            PIC ZZZZZZZZZ.ZZ.
      * 01 HN-ANES-TAR-EDIT-J            PIC ZZZZZZZZZ.ZZ.
      * 01 MAT-QUI-TAR-EDIT-J            PIC ZZZZZZZZZ.ZZ.
      * 01 DR-SALA-TAR-EDIT-J            PIC ZZZZZZZZZ.ZZ.
      * 01 HN-OTR1-TAR-EDIT-J            PIC ZZZZZZZZZ.ZZ.
      * 01 HN-OTR2-TAR-EDIT-J            PIC ZZZZZZZZZ.ZZ.
       01 SAL-MIN-TAR-EDIT              PIC ZZZZZZ.ZZ.
       01 PORC-PO-TAR-EDIT              PIC ZZZ.ZZ.
       01 PORC-NP-TAR-EDIT              PIC ZZZ.ZZ.
       01 PORC-MO-TAR-EDIT              PIC ZZZ.ZZ.
       01 PORC-MQ-TAR-EDIT              PIC ZZZ.ZZ.
       01 PORC-TAB-TAR-EDIT             PIC ZZZ.ZZ.
       


       01 DATOS-ENVIO.
          02 SW-INVALID-ENV             PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 COD-TAR-ENV.
             03 COD1-TAR-ENV            PIC X.
             03 COD2-TAR-ENV            PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 DESCRIP-TAR-ENV            PIC X(25).
          02 FILLER                     PIC X VALUE "|".
          02 PORC-PO-TAR-ENV            PIC X(6).
          02 FILLER                     PIC X VALUE "|".
          02 PORC-NP-TAR-ENV            PIC X(6).
          02 FILLER                     PIC X VALUE "|".
          02 PORC-MO-TAR-ENV            PIC X(6).
          02 FILLER                     PIC X VALUE "|".
          02 PORC-MQ-TAR-ENV            PIC X(6).
          02 FILLER                     PIC X VALUE "|".
          02 SAL-MIN-TAR-ENV            PIC X(9).
          02 FILLER                     PIC X VALUE "|".
          02 COD1-TAB-TAR-ENV           PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 COD2-TAB-TAR-ENV           PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 COD3-TAB-TAR-ENV           PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 COD4-TAB-TAR-ENV           PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 COD5-TAB-TAR-ENV           PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 COD6-TAB-TAR-ENV           PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 FACT-CAPIT-NUM-ENV.
             03 PRE-CAPIT-NUM-ENV       PIC X.
             03 NRO-CAPIT-NUM-ENV       PIC 9(6).
          02 FILLER                     PIC X VALUE "|".
          02 NIT-NUM-ENV                PIC 9(10).
          02 FILLER                     PIC X VALUE "|".
          02 FECHA-ING-NUM-ENV.
             03 LLAVE-MES-ING-NUM-ENV.
                05 ANO-ING-NUM-ENV      PIC 9(4).
                05 MES-ING-NUM-ENV      PIC 99.
             03 DIA-ING-NUM-ENV         PIC 99.
          02 FILLER                     PIC X VALUE "|".
          02 RED-EXTER-NUM-ENV          PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 HN-QUIR-TAR-ENV            PIC X(12).
          02 FILLER                     PIC X VALUE "|".
          02 CIS-NUM-ENV                PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 PORC-TAB1-TAR              PIC X(7).
          02 FILLER                     PIC X VALUE "|".
          02 PORC-TAB2-TAR              PIC X(7).
          02 FILLER                     PIC X VALUE "|".
          02 PORC-TAB3-TAR              PIC X(7).
          02 FILLER                     PIC X VALUE "|".
          02 PORC-TAB4-TAR              PIC X(7).
          02 FILLER                     PIC X VALUE "|".
          02 PORC-TAB5-TAR              PIC X(7).
          02 FILLER                     PIC X VALUE "|".
          02 PORC-TAB6-TAR              PIC X(7).
          02 FILLER                     PIC X VALUE "|".
          02 LLAVE-NUM-ENV              PIC X(7).
          02 FILLER                     PIC X VALUE "|".
          02 HN-AYUD-TAR-ENV            PIC X(12).
          02 FILLER                     PIC X VALUE "|".
          02 HN-ANES-TAR-ENV            PIC X(12).
          02 FILLER                     PIC X VALUE "|".
          02 NOMBRE-JSON                PIC X(26).


        01 LIN-1.
          02 FILLER                     PIC X(13)  VALUE "{*HNQUIRTAR*:".
          02 FILLER                     PIC X      VALUE "*".
      *    02 HN-QUIR-TAR-J              PIC X(12).
          02 HN-QUIR-TAR-EDIT-J         PIC ZZZZZZZZZ.ZZ.
          02 FILLER                     PIC XX     VALUE "*,".
          02 FILLER                     PIC X(12)  VALUE "*HNAYUDTAR*:".
          02 FILLER                     PIC X      VALUE "*".
      *    02 HN-AYUD-TAR-J              PIC X(12).
          02 HN-AYUD-TAR-EDIT-J            PIC ZZZZZZZZZ.ZZ.
          02 FILLER                     PIC XX     VALUE "*,".
          02 FILLER                     PIC X(12)  VALUE "*HNANESTAR*:".
          02 FILLER                     PIC X      VALUE "*".
      *    02 HN-ANES-TAR-J              PIC X(12).
          02 HN-ANES-TAR-EDIT-J            PIC ZZZZZZZZZ.ZZ.
          02 FILLER                     PIC XX     VALUE "*,".
          02 FILLER                     PIC X(12)  VALUE "*MATQUITAR*:".
          02 FILLER                     PIC X      VALUE "*".
      *    02 MAT-QUI-TAR-J              PIC X(12).
          02 MAT-QUI-TAR-EDIT-J            PIC ZZZZZZZZZ.ZZ.
          02 FILLER                     PIC XX     VALUE "*,".
          02 FILLER                     PIC X(12)  VALUE "*DRSALATAR*:".
          02 FILLER                     PIC X      VALUE "*".
      *    02 DR-SALA-TAR-J              PIC X(12).
          02 DR-SALA-TAR-EDIT-J            PIC ZZZZZZZZZ.ZZ.
          02 FILLER                     PIC XX      VALUE "*,".
          02 FILLER                     PIC X(12)  VALUE "*HNOTR1TAR*:".
          02 FILLER                     PIC X      VALUE "*".
      *    02 HN-OTR1-TAR-J              PIC X(12).
          02 HN-OTR1-TAR-EDIT-J            PIC ZZZZZZZZZ.ZZ.
          02 FILLER                     PIC XX     VALUE "*,".
          02 FILLER                     PIC X(12)  VALUE "*HNOTR2TAR*:".
          02 FILLER                     PIC X      VALUE "*".
      *    02 HN-OTR2-TAR-J              PIC X(12).
          02 HN-OTR2-TAR-EDIT-J            PIC ZZZZZZZZZ.ZZ.
          02 FILLER                     PIC XX     VALUE "*}".    
          02 CIERRE-LIN-1               PIC X.

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
              MOVE "INV401-05"              TO MSJ3-HTML
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
              MOVE "INV401-05"              TO MSJ3-HTML
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
              MOVE "INV401-05"              TO MSJ3-HTML
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
              MOVE "INV401-05"              TO MSJ3-HTML
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
              MOVE "INV401-05"              TO MSJ3-HTML
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
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W, COD-TAR-LLEGADA-W, LLAVE-LLEGADA-W
           END-UNSTRING.

001855     MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.
           MOVE FUNCTION CURRENT-DATE TO FECHA-JSON-W.
           ACCEPT HORA-JSON-W FROM TIME.
           MOVE OPERADOR-LLEGADA-W TO OPER-JSON-W.

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

001614     INITIALIZE NOM-TARIF-W
                      NOM-NUMER-W
                      NOM-PLANO-W

           MOVE "\\" TO NOM-TARIF-W 
                        NOM-NUMER-W
                        NOM-PLANO-W

           INSPECT NOM-TARIF-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

001621     INSPECT NOM-TARIF-W REPLACING FIRST " "
001624                                      BY "\"

007190     INSPECT NOM-TARIF-W REPLACING FIRST "                              "
                                            BY DIR-CONTAB-LLEGADA-W

           MOVE NOM-TARIF-W TO NOM-NUMER-W

           INSPECT NOM-TARIF-W REPLACING FIRST "                       "
007300                                      BY "\CONTROL\SC-ARCHTAR.DAT".

           INSPECT NOM-NUMER-W REPLACING FIRST "                     "
007300                                      BY "\CONTROL\SC-NUMER.DAT".


           INSPECT NOM-PLANO-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-PLANO-W REPLACING FIRST  "                 "
                                            BY  "\PROG\DATOS\JSON\"  

           INSPECT NOM-PLANO-W  REPLACING FIRST "                          "
                                             BY "XXXXXXXXXXXXXXXXXXXXX.JSON"

           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXX"
                                             BY NOMBRE-JSON-W.
           MOVE NOMBRE-JSON-W TO NOMBRE-JSON.

        ABRIR-ARCHIVO-NUMERACION.
           OPEN INPUT ARCHIVO-NUMERACION.

        BUSCAR-LLAVE-NUMERACION.
           MOVE LLAVE-LLEGADA-W   TO LLAVE-NUM
           READ ARCHIVO-NUMERACION WITH NO LOCK 
                INVALID KEY
                    MOVE "01"  TO SW-INVALID-ENV
                NOT INVALID KEY
                    MOVE "00"  TO SW-INVALID-ENV
           END-READ.

        ENVIO-DATOS-NUMERACION.
           MOVE FACT-CAPIT-NUM      TO FACT-CAPIT-NUM-ENV
           MOVE NIT-NUM             TO NIT-NUM-ENV
           MOVE CIS-NUM             TO CIS-NUM-ENV
           MOVE LLAVE-NUM           TO LLAVE-NUM-ENV
           MOVE RED-EXTER-NUM       TO RED-EXTER-NUM-ENV.

        ABRIR-ARCHIVO-TARIFAS.
           OPEN INPUT ARCHIVO-TARIFAS.

        ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-JSON.

           MOVE "{*TABLA_TAR*:[" TO DATOS-JSON
           INSPECT DATOS-JSON  REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON  END-WRITE.

        BUSCAR-TARIFA.
           MOVE COD-TAR-LLEGADA-W   TO COD-TAR
           READ ARCHIVO-TARIFAS INVALID KEY
                                MOVE "01"  TO SW-INVALID-ENV
                                NOT INVALID KEY
                                MOVE "00"  TO SW-INVALID-ENV
           END-READ.

        ENVIAR-DATOS-TARIFAS.

           PERFORM MOVER-DATOS VARYING I FROM 1 BY 1
                               UNTIL I > 30.

           MOVE PORC-PO-TAR            TO PORC-PO-TAR-EDIT
           MOVE PORC-PO-TAR-EDIT       TO PORC-PO-TAR-ENV
           MOVE PORC-NP-TAR            TO PORC-NP-TAR-EDIT
           MOVE PORC-NP-TAR-EDIT       TO PORC-NP-TAR-ENV
           MOVE PORC-MO-TAR            TO PORC-MO-TAR-EDIT
           MOVE PORC-MO-TAR-EDIT       TO PORC-MO-TAR-ENV
           MOVE PORC-MQ-TAR            TO PORC-MQ-TAR-EDIT
           MOVE PORC-MQ-TAR-EDIT       TO PORC-MQ-TAR-ENV
           MOVE SAL-MIN-TAR            TO SAL-MIN-TAR-EDIT
           IF SAL-MIN-TAR-EDIT = SPACES OR LOW-VALUE
           MOVE 000000.00              TO SAL-MIN-TAR-EDIT
           MOVE SAL-MIN-TAR-EDIT       TO SAL-MIN-TAR-ENV
           ELSE          
           MOVE SAL-MIN-TAR-EDIT       TO SAL-MIN-TAR-ENV
           END-IF
           MOVE COD-TAR                TO COD-TAR-ENV
           MOVE DESCRIP-TAR            TO DESCRIP-TAR-ENV
           MOVE HN-QUIR-TAR (30)       TO HN-QUIR-TAR-EDIT
           MOVE HN-QUIR-TAR-EDIT       TO HN-QUIR-TAR-ENV 
           MOVE HN-AYUD-TAR (30)       TO HN-AYUD-TAR-EDIT
           MOVE HN-AYUD-TAR-EDIT       TO HN-AYUD-TAR-ENV
           MOVE HN-ANES-TAR (30)       TO HN-ANES-TAR-EDIT
           MOVE HN-ANES-TAR-EDIT       TO HN-ANES-TAR-ENV
           MOVE PORC-TAB-TAR (1)       TO PORC-TAB-TAR-EDIT
           MOVE PORC-TAB-TAR-EDIT      TO PORC-TAB1-TAR
           INITIALIZE PORC-TAB-TAR-EDIT
           MOVE PORC-TAB-TAR (2)       TO PORC-TAB-TAR-EDIT
           MOVE PORC-TAB-TAR-EDIT      TO PORC-TAB2-TAR
           INITIALIZE PORC-TAB-TAR-EDIT
           MOVE PORC-TAB-TAR (3)       TO PORC-TAB-TAR-EDIT
           MOVE PORC-TAB-TAR-EDIT      TO PORC-TAB3-TAR
           INITIALIZE PORC-TAB-TAR-EDIT
           MOVE PORC-TAB-TAR (4)       TO PORC-TAB-TAR-EDIT
           MOVE PORC-TAB-TAR-EDIT      TO PORC-TAB4-TAR
           INITIALIZE PORC-TAB-TAR-EDIT
           MOVE PORC-TAB-TAR (5)       TO PORC-TAB-TAR-EDIT
           MOVE PORC-TAB-TAR-EDIT      TO PORC-TAB5-TAR
           INITIALIZE PORC-TAB-TAR-EDIT
           MOVE PORC-TAB-TAR (6)       TO PORC-TAB-TAR-EDIT
           MOVE PORC-TAB-TAR-EDIT      TO PORC-TAB6-TAR
           INITIALIZE PORC-TAB-TAR-EDIT
           MOVE COD-TAB-TAR (1)        TO COD1-TAB-TAR-ENV
           MOVE COD-TAB-TAR (2)        TO COD2-TAB-TAR-ENV
           MOVE COD-TAB-TAR (3)        TO COD3-TAB-TAR-ENV
           MOVE COD-TAB-TAR (4)        TO COD4-TAB-TAR-ENV
           MOVE COD-TAB-TAR (5)        TO COD5-TAB-TAR-ENV
           MOVE COD-TAB-TAR (6)        TO COD6-TAB-TAR-ENV.
           
        CERRAR-JSON.
           INITIALIZE LIN-1
           
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-1 TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
          
           MOVE "]}" TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE.

        CERRAR-ARCHIVOS.   
           CLOSE ARCHIVO-NUMERACION
                 ARCHIVO-TARIFAS
                 ARCHIVO-JSON.

010700  PAGINA-CONFIG.
010720     MOVE "datosrecibidos"    TO COBW3-CNV-NAME
010730     MOVE DATOS-ENVIO         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3
010750
010760     MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
010770     PERFORM ABRIR-HTML.
007980   
007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.
008040 COPY "..\..\FUENTES\SC-WEB.CBL".
005210* COPY "..\..\FUENTES\WS-HCEVO.CBL".

       MOVER-DATOS.
            MOVE HN-QUIR-TAR (I)    TO HN-QUIR-TAR-EDIT-J
      *      MOVE HN-QUIR-TAR-EDIT-J TO HN-QUIR-TAR-J
            MOVE HN-AYUD-TAR (I)    TO HN-AYUD-TAR-EDIT-J
      *      MOVE HN-AYUD-TAR-EDIT-J TO HN-AYUD-TAR-J
            MOVE HN-ANES-TAR (I)    TO HN-ANES-TAR-EDIT-J
      *      MOVE HN-ANES-TAR-EDIT-J TO HN-ANES-TAR-J
            MOVE MAT-QUI-TAR (I)    TO MAT-QUI-TAR-EDIT-J
      *      MOVE MAT-QUI-TAR-EDIT-J TO MAT-QUI-TAR-J
            MOVE DR-SALA-TAR (I)    TO DR-SALA-TAR-EDIT-J
      *      MOVE DR-SALA-TAR-EDIT-J TO DR-SALA-TAR-J
            MOVE HN-OTR1-TAR (I)    TO HN-OTR1-TAR-EDIT-J
      *      MOVE HN-OTR1-TAR-EDIT-J TO HN-OTR1-TAR-J
            MOVE HN-OTR2-TAR (I)    TO HN-OTR2-TAR-EDIT-J
      *      MOVE HN-OTR2-TAR-EDIT-J TO HN-OTR2-TAR-J

            INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
            INSPECT LIN-1 REPLACING ALL LOW-VALUE BY " "
            MOVE "," TO CIERRE-LIN-1

            MOVE LIN-1 TO DATOS-JSON
            WRITE REG-JSON END-WRITE.
