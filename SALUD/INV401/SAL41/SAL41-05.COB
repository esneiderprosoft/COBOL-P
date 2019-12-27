004820*=================================================================
004830* CONSULTA DE ARCHIVO DE PACIENTES.
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "..\..\FUENTES\FS-USUNET.CBL".
004920     COPY "..\..\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-PACIENTES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PACIE-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PACI
                  ALTERNATE RECORD KEY  DESCRIP-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  EPS-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  ID-COTIZ-PACI WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-PACIE.CBL".	   

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
          02 COD-PACI-LLEGADA-W         PIC X(15).

       77 MES-SAL-W                     PIC X(4).
       77 NOM-PACIE-W                   PIC X(70).

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV             PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 EPS-PACI-ENV.
             03 EPS1-PACI-ENV           PIC X(3).
             03 EPS2-PACI-ENV           PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 DESCRIP-PACI-ENV.
             03 LLAVE-APEL-PACI-ENV.
                05 1ER-APEL-PACI-ENV.
                   07 INICIAL-PACI-ENV  PIC X.
                   07 RESTO-PACI-ENV    PIC X(14).
                05 2DO-APEL-PACI-ENV    PIC X(15).
             03 NOMBRE-PACI-ENV.
                05 1ER-NOM-PACI-ENV     PIC X(12).
                05 2DO-NOM-PACI-ENV     PIC X(12).
          02 FILLER                     PIC X VALUE "|".
          02 NACIM-PACI-ENV.
             03 ANO-PACI-ENV            PIC 9(4).
             03 MES-PACI-ENV            PIC 99.
             03 DIA-PACI-ENV            PIC 99.
          02 FILLER                     PIC X VALUE "|".
          02 TIPO-PACI-ENV              PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CONTRATO-PACI-ENV          PIC X(15).
          02 FILLER                     PIC X VALUE "|".
          02 TUTELA-PACI-ENV            PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 ALT-COS-PACI-ENV           PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 PROG-ESP-PACI-ENV          PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CRONICO-PACI-ENV           PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 MULTICONSUL-PACI-ENV       PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 SEXO-PACI-ENV              PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CLASIF-PACI-ENV            PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 CIUDAD-PACI-ENV.
             03 DPTO-PACI-ENV           PIC 99.
             03 CIUD-PACI-ENV           PIC 9(3).
          02 FILLER                     PIC X VALUE "|".
          02 TIPO-AFIL-PACI-ENV         PIC 9.
          02 FILLER                     PIC X VALUE "|".
          02 DERECHO-PACI-ENV           PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 FECHA-VENCE-PACI-ENV.
             03 ANO-VENCE-PACI-ENV      PIC 9(4).
             03 MES-VENCE-PACI-ENV      PIC 99.
             03 DIA-VENCE-PACI-ENV      PIC 99.
          02 FILLER                     PIC X VALUE "|".
          02 REST-APLI-PACI-ENV         PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 REST-DROG-PACI-ENV         PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 REST-CIRU-PACI-ENV         PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 REST-LABO-PACI-ENV         PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 REST-IMAG-PACI-ENV         PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 REST-ESTA-PACI-ENV         PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 REST-CONS-PACI-ENV         PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 REST-TERF-PACI-ENV         PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 REST-TERO-PACI-ENV         PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 REST-ODON-PACI-ENV         PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 REST-PYP-PACI-ENV          PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 NIT-FACT-PACI-ENV          PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 TIPO-ID-PACI-ENV           PIC X(3).
          02 FILLER                     PIC X VALUE "|".
          02 ESTRATO-PACI-ENV           PIC 9.
          02 FILLER                     PIC X VALUE "|".
          02 FECHA-NOTA-PAC2-ENV        PIC 9(8).
          02 FILLER                     PIC X VALUE "|".
          02 OBSERV1A-NOTA-PAC2-ENV     PIC X(40).
          02 FILLER                     PIC X VALUE "|".
          02 OBSERV1B-NOTA-PAC2-ENV     PIC X(20).
          02 FILLER                     PIC X VALUE "|".
          02 OBSERV1C-NOTA-PAC2-ENV     PIC X(20).
          02 FILLER                     PIC X VALUE "|".
          02 OBSERV2-NOTA-PAC2-ENV      PIC X(80).
          02 FILLER                     PIC X VALUE "|".
          02 OBSERV3-NOTA-PAC2-ENV      PIC X(80).
          02 FILLER                     PIC X VALUE "|".
          02 OBSERV4-NOTA-PAC2-ENV      PIC X(80).
          02 FILLER                     PIC X VALUE "|".
          02 OBSERV5-NOTA-PAC2-ENV      PIC X(80).
          02 FILLER                     PIC X VALUE "|".
          02 OPER-OBSE-NOTA-PAC2-ENV    PIC X(4).
          02 FILLER                     PIC X VALUE "|".
          02 FACT-NOTA-PAC2-ENV.
             03 PREFIJO-NOTA-PAC2-ENV   PIC X.
             03 NUMERO-NOTA-PAC2-ENV    PIC 9(6).
          02 FILLER                     PIC X VALUE "|".
          02 DIRECC-PACI-ENV            PIC X(30).  
          02 FILLER                     PIC X VALUE "|".
          02 TELEFONO-PACI-ENV          PIC X(12).
          02 FILLER                     PIC X VALUE "|".
          02 EST-CIV-PACI-ENV           PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CEL-PACI-ENV               PIC X(12).
          02 FILLER                     PIC X VALUE "|".
          02 E-MAIL-ENV                 PIC X(80).


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
              MOVE "INV401-08"              TO MSJ3-HTML
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
              MOVE "INV401-08"              TO MSJ3-HTML
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
              MOVE "INV401-08"              TO MSJ3-HTML
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
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W, COD-PACI-LLEGADA-W
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
           END-EVALUATE.
           
    

        ASIGNAR-NOMBRES. 
           INITIALIZE NOM-PACIE-W 
           MOVE "\\" TO NOM-PACIE-W
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".
           INSPECT NOM-PACIE-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET
           INSPECT NOM-PACIE-W REPLACING FIRST " "
                                            BY "\"
           INSPECT NOM-PACIE-W REPLACING FIRST "                              "
                                            BY DIR-CONTAB-LLEGADA-W.
           INSPECT NOM-PACIE-W REPLACING FIRST "                     "
                                            BY "\CONTROL\SC-PACIE.DAT".

       ABRIR-TARIFAS.
           OPEN INPUT ARCHIVO-PACIENTES.

       LEER-TARIFAS.
           MOVE COD-PACI-LLEGADA-W   TO COD-PACI
           READ ARCHIVO-PACIENTES WITH NO LOCK
                INVALID KEY 
                  MOVE "01" TO SW-INVALID-ENV               
                NOT INVALID KEY
                  MOVE "00" TO SW-INVALID-ENV
           END-READ.

           MOVE EPS-PACI             TO EPS-PACI-ENV
           MOVE DESCRIP-PACI         TO DESCRIP-PACI-ENV
           MOVE NACIM-PACI           TO NACIM-PACI-ENV
           MOVE TIPO-PACI            TO TIPO-PACI-ENV
           MOVE TELEFONO-PACI        TO TELEFONO-PACI-ENV
           MOVE CEL-PACI             TO CEL-PACI-ENV
           MOVE EST-CIV-PACI         TO EST-CIV-PACI-ENV
           MOVE DIRECC-PACI          TO DIRECC-PACI-ENV
           MOVE CONTRATO-PACI        TO CONTRATO-PACI-ENV
           MOVE TUTELA-PACI          TO TUTELA-PACI-ENV
           MOVE ALT-COS-PACI         TO ALT-COS-PACI-ENV
           MOVE PROG-ESP-PACI        TO PROG-ESP-PACI-ENV
           MOVE CRONICO-PACI         TO CRONICO-PACI-ENV
           MOVE MULTICONSUL-PACI     TO MULTICONSUL-PACI-ENV
           MOVE SEXO-PACI            TO SEXO-PACI-ENV
           MOVE CLASIF-PACI          TO CLASIF-PACI-ENV
           MOVE TIPO-AFIL-PACI       TO TIPO-AFIL-PACI-ENV
           MOVE DERECHO-PACI         TO DERECHO-PACI-ENV
           MOVE FECHA-VENCE-PACI     TO FECHA-VENCE-PACI-ENV
           MOVE REST-APLI-PACI       TO REST-APLI-PACI-ENV
           MOVE REST-DROG-PACI       TO REST-DROG-PACI-ENV
           MOVE REST-CIRU-PACI       TO REST-CIRU-PACI-ENV
           MOVE REST-LABO-PACI       TO REST-LABO-PACI-ENV
           MOVE REST-IMAG-PACI       TO REST-IMAG-PACI-ENV
           MOVE REST-ESTA-PACI       TO REST-ESTA-PACI-ENV
           MOVE REST-CONS-PACI       TO REST-CONS-PACI-ENV
           MOVE REST-TERF-PACI       TO REST-TERF-PACI-ENV
           MOVE REST-TERO-PACI       TO REST-TERO-PACI-ENV
           MOVE REST-ODON-PACI       TO REST-ODON-PACI-ENV
           MOVE REST-PYP-PACI        TO REST-PYP-PACI-ENV
           MOVE NIT-FACT-PACI        TO NIT-FACT-PACI-ENV
           MOVE CIUDAD-PACI          TO CIUDAD-PACI-ENV
           MOVE FECHA-NOTA-PAC2      TO FECHA-NOTA-PAC2-ENV
           MOVE OBSERV1A-NOTA-PAC2   TO OBSERV1A-NOTA-PAC2-ENV  
           MOVE OBSERV1B-NOTA-PAC2   TO OBSERV1B-NOTA-PAC2-ENV  
           MOVE OBSERV1C-NOTA-PAC2   TO OBSERV1C-NOTA-PAC2-ENV  
           MOVE OBSERV2-NOTA-PAC2    TO OBSERV2-NOTA-PAC2-ENV  
           MOVE OBSERV3-NOTA-PAC2    TO OBSERV3-NOTA-PAC2-ENV  
           MOVE OBSERV4-NOTA-PAC2    TO OBSERV4-NOTA-PAC2-ENV  
           MOVE OBSERV5-NOTA-PAC2    TO OBSERV5-NOTA-PAC2-ENV        
           MOVE OPER-OBSE-NOTA-PAC2  TO OPER-OBSE-NOTA-PAC2-ENV    
           MOVE ESTRATO-PACI         TO ESTRATO-PACI-ENV
           MOVE E-MAIL-PACI          TO E-MAIL-ENV
           MOVE FACT-NOTA-PAC2       TO FACT-NOTA-PAC2-ENV.

        CERRAR-TERCEROS.
           CLOSE ARCHIVO-PACIENTES.

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
