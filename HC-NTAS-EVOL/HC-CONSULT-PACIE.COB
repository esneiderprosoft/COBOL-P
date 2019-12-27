004820*=================================================================
004830* CREA JSON DE CONSULTA DE DATOS POR PACIENTE HC.
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
004920     COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-USUARIOS
                  ASSIGN NOM-USUAR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU 
                  ALTERNATE RECORD KEY IS		
                  CLAVE-USU WITH DUPLICATES
                  ALTERNATE RECORD KEY IS
                  CTL-USU WITH DUPLICATES
                  ALTERNATE RECORD KEY IS
                  CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT HISTORIA-CLINICA LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-HCAPE-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-HC
                  ALTERNATE RECORD KEY IS FOLIO-HC WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FECHA-HC WITH DUPLICATES
                  ALTERNATE RECORD KEY IS MED-HC WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ESTADO-HC WITH DUPLICATES
                  FILE STATUS OTR-STAT.

           SELECT ARCHIVO-PACIENTES
                  ASSIGN  NOM-PACIE-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PACI
                  ALTERNATE RECORD KEY  
                  DESCRIP-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  
                  EPS-PACI  WITH DUPLICATES
                  ALTERNATE RECORD KEY  
                  ID-COTIZ-PACI WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-JSON
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.

005080 DATA DIVISION.
005090 FILE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-USUAR.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-HCAPER17.CBL".
       COPY "S:\NEWCOBOL\FUENTES\FD-PACIE.CBL".
       

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-PLANO.
           02 DATOS-PLANO     PIC X(3500).

005190 WORKING-STORAGE SECTION.
005200 COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
005210 COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".
005210 COPY "P:\PROG\PROYECT\FUENTES\WS-8001.CBL".
005210 COPY "P:\PROG\PROYECT\FUENTES\WS-8031.CBL".
005220 
       77 NOM-USUAR-W                PIC X(70).
       77 NOM-HCAPE-W                PIC X(70).
       77 NOM-PACIE-W                PIC X(70).
       77 NOM-PLANO-W                PIC X(90).
       77 CONT                       PIC 99.
       77 OPER-W                     PIC X(4).
       77 SUCURSAL-W                 PIC X.
       77 SW-PYP                     PIC 999.
       77 SW-BLANCA                  PIC 999.
       77 SW-OPEN                    PIC 9.
       77 LLAVE-HC-W                 PIC X(23).


       01 ULT-FECHA-NUM.
          02 ULT-ANO-NUM             PIC 99.
          02 ULT-MES-NUM             PIC 99.
          02 ULT-DIA-NUM             PIC 99.


       01 FECHA-SIG-ACT-W.
          02 SIG-ACT-W         PIC 99.
          02 FECHA-ACT-W.
             03 ANO-ACT-W      PIC 99.
             03 MES-ACT-W      PIC 99.
             03 DIA-ACT-W      PIC 99.

       01 LINEA-LLEGAD-W             PIC X(200).

005270 01  DATO-LLEGADA-W.
           02 SESION-LLEGAD-W.
005280        03 ID-LLEGAD-W         PIC X(15).
              03 FECHA-LLEGAD-W      PIC X(8).
              03 HORA-LLEGAD-W       PIC X(6).
           02 DIRECTORIO-W           PIC X(30).
           02 MES-TRABAJO-W          PIC X(10).
           02 LLAVE-PACI-W           PIC X(15).


       01 DATOS-ENVIO.
          02 DAT0-ENV                PIC XX.
          02 FILLER                  PIC X VALUE "|".
          02 LLAVE-HC-ENV            PIC X(23).
          02 FILLER                  PIC X VALUE "|".
          02 COD-PACI-ENV            PIC X(15).
          02 FILLER                  PIC X VALUE "|".
          02 TIPO-ID-PACI-ENV        PIC X(3).
          02 FILLER                  PIC X VALUE "|".
          02 1ER-APEL-PACI-ENV       PIC X(15).
          02 FILLER                  PIC X VALUE "|".
          02 2DO-APEL-PACI-ENV       PIC X(15).
          02 FILLER                  PIC X VALUE "|".
          02 NOMBRE-PACI-ENV         PIC X(24).
          02 FILLER                  PIC X VALUE "|".
          02 NACIM-PACI-ENV          PIC X(8).
          02 FILLER                  PIC X VALUE "|".
          02 SEXO-PACI-ENV           PIC X.
          02 FILLER                  PIC X VALUE "|".
          02 CIUDAD-PACI-ENV         PIC X(5).
          02 FILLER                  PIC X VALUE "|".
          02 ZONA-PACI-ENV           PIC X.
          02 FILLER                  PIC X VALUE "|".
          02 TIPO-PACI-ENV           PIC X.
          02 FILLER                  PIC X VALUE "|".
          02 EPS-PACI-ENV            PIC X(6).
          02 FILLER                  PIC X VALUE "|".
          02 TIPO-AFIL-PACI-ENV      PIC X.
          02 FILLER                  PIC X VALUE "|".
          02 NRO-AFIL-PACI-ENV       PIC X(15).
          02 FILLER                  PIC X VALUE "|".
          02 ID-COTIZ-PACI-ENV       PIC X(15).
          02 FILLER                  PIC X VALUE "|".
          02 ESTRATO-PACI-ENV        PIC X.
          02 FILLER                  PIC X VALUE "|".
          02 ACOMPA-PACI-ENV         PIC X(30).
          02 FILLER                  PIC X VALUE "|".
          02 TELEFONO-PACI-ENV       PIC X(12).
          02 FILLER                  PIC X VALUE "|".
          02 DIRREC-PACI-ENV         PIC X(30).
          02 FILLER                  PIC X VALUE "|".
          02 OCUP-V8-PACI-ENV        PIC X(4).
          02 FILLER                  PIC X VALUE "|".
          02 DERECHO-PACI-ENV        PIC X.
          02 FILLER                  PIC X VALUE "|".
          02 CRONICO-PACI-ENV        PIC X.
          02 FILLER                  PIC X VALUE "|".
          02 PARENT-PACI-ENV         PIC XX.
          02 FILLER                  PIC X VALUE "|".
          02 OBSERV-PACI-ENV         PIC X(50).
          02 FILLER                  PIC X VALUE "|".
          02 NIT-FACT-PACI-ENV       PIC X(10).
          02 FILLER                  PIC X VALUE "|".
          02 REST-APLI-PACI-ENV      PIC X.
          02 FILLER                  PIC X VALUE "|".
          02 REST-DROG-PACI-ENV      PIC X.
          02 FILLER                  PIC X VALUE "|".
          02 REST-CIRU-PACI-ENV      PIC X.
          02 FILLER                  PIC X VALUE "|".
          02 REST-LABO-PACI-ENV      PIC X.
          02 FILLER                  PIC X VALUE "|".
          02 REST-IMAG-PACI-ENV      PIC X.
          02 FILLER                  PIC X VALUE "|".
          02 REST-ESTA-PACI-ENV      PIC X.
          02 FILLER                  PIC X VALUE "|".
          02 REST-CONS-PACI-ENV      PIC X.
          02 FILLER                  PIC X VALUE "|".
          02 REST-TERF-PACI-ENV      PIC X.
          02 FILLER                  PIC X VALUE "|".
          02 REST-TERO-PACI-ENV      PIC X.
          02 FILLER                  PIC X VALUE "|".
          02 REST-ODON-PACI-ENV      PIC X.
          02 FILLER                  PIC X VALUE "|".
          02 REST-PYP-PACI-ENV       PIC X.


006310 LINKAGE SECTION.
006320 COPY "P:\PROG\PROYECT\FUENTES\ISAPICTX.CBL".
006330 PROCEDURE DIVISION WITH stdcall LINKAGE USING ISAPI-CTX-CNT.
006340
006350 DECLARATIVES.
006360 I-O-TEST SECTION.
006370     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUNET.         
006380 ESCR-EXCEPTIONES.
006390     IF OTR-STAT = "00"
006400        CONTINUE 
006410     ELSE
006420        MOVE OTR-STAT         TO MSJ1-HTML
006430        MOVE NOM-USU-W        TO MSJ2-HTML
006440        MOVE "HC811"          TO MSJ3-HTML
006450        GO TO ENVIAR2-ERROR
006460     END-IF.
006470
006480 I-O-TEST SECTION.
006490     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.
006500 ESCR-EXCEPTIONES.
006510     IF OTR-STAT = "00" 
006520        CONTINUE 
006530     ELSE
006540        MOVE OTR-STAT         TO MSJ1-HTML
006550        MOVE NOM-SESION-W     TO MSJ2-HTML
006560        MOVE "HC811"          TO MSJ3-HTML
006570        GO TO ENVIAR2-ERROR
006580     END-IF.
006590  

006480 I-O-TEST SECTION.
006490     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
006500 ESCR-EXCEPTIONES.
006510     IF OTR-STAT = "00" 
006520        CONTINUE 
006530     ELSE
006540        MOVE OTR-STAT         TO MSJ1-HTML
006550        MOVE NOM-USUAR-W      TO MSJ2-HTML
006560        MOVE "HC811"          TO MSJ3-HTML
006570        GO TO ENVIAR2-ERROR
006580     END-IF.
006590  

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON HISTORIA-CLINICA.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-HCAPE-W      TO MSJ2-HTML
006680        MOVE "HC811"          TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR  
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" 
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-PACIE-W      TO MSJ2-HTML
006680        MOVE "HC811"          TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

       I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" OR "35"
006640        CONTINUE                                                                                      
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-PLANO-W      TO MSJ2-HTML
006680        MOVE "HC002"          TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.
006710
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
006820     MOVE COBW3-GET-DATA    TO LINEA-LLEGAD-W.


           UNSTRING LINEA-LLEGAD-W DELIMITED BY "|"
              INTO SESION-LLEGAD-W, LLAVE-PACI-W, ADMIN-W
           END-UNSTRING.

001855     MOVE SESION-LLEGAD-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.

000797 INICIAR-SESION.
000806     MOVE "D:\WEB\HC-NTAS-EVOL\DATOS\SC-SESION.DAT" TO NOM-SESION-W
000820     MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
000830     ACCEPT HORA-TOTAL FROM TIME.
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.
006840
006900 ABRIR-USUARIO.
006910     INITIALIZE OTR-STAT.
006920     MOVE "D:\WEB\HC-NTAS-EVOL\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
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
007130
007140     CLOSE ARCHIVO-USUNET.
007200     
007210     IF NOMBRE-USUNET = SPACES
007220     OR NIT-USUNET = ZEROS
007230        MOVE "Validacion de Sesion"        TO MSJ1-HTML
007240        MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007250        MOVE "Sc"                          TO MSJ3-HTML
007260        GO TO ENVIAR2-ERROR
007270     END-IF.
007280 
007350
       ASIGNAR-NOMBRES.
           ACCEPT ULT-FECHA-NUM FROM DATE.
           MOVE ULT-FECHA-NUM TO FECHA-ACT-W
           INITIALIZE NOM-USUAR-W NOM-PLANO-W
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           MOVE "\\" TO NOM-USUAR-W NOM-HCAPE-W

           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-USUAR-W REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-USUAR-W REPLACING FIRST "                              "
                                            BY DIRECTORIO-USUNET

           MOVE NOM-USUAR-W TO NOM-PACIE-W 

           INSPECT NOM-USUAR-W REPLACING FIRST  "                "
                                            BY  "\CONTROL\ARCHUSU".

           INSPECT NOM-PACIE-W REPLACING FIRST  "                     "
                                           BY   "\CONTROL\SC-PACIE.DAT".


           INSPECT NOM-HCAPE-W  REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

           INSPECT NOM-HCAPE-W REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-HCAPE-W REPLACING FIRST "                               "
                                            BY "NEWCOBOL\HC\DATOS\SC-HCAPER.S17".


       LEER-USUARIO.

           OPEN INPUT ARCHIVO-USUARIOS
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.

       LEER-PACIENTE.
           OPEN INPUT ARCHIVO-PACIENTES.
           MOVE LLAVE-PACI-W    TO  COD-PACI
           READ ARCHIVO-PACIENTES WITH NO LOCK
                INVALID KEY
                        MOVE "99"         TO MSJ1-HTML
                        MOVE "Paciente no existe" To MSJ2-HTML
                        MOVE "HC-CONSULT-PACIE"   TO MSJ3-HTML
                        GO TO ENVIAR2-ERROR
           END-READ.
           CLOSE ARCHIVO-PACIENTES.


       BUSCAR-HISTORIA.
           OPEN INPUT HISTORIA-CLINICA.

           INITIALIZE SW-OPEN CONT
           MOVE COD-PACI      TO LLAVE-HC
           START HISTORIA-CLINICA  KEY = ID-HC
                 INVALID KEY
                        MOVE LLAVE-HC   TO LLAVE-HC-W
                        GO TO HISTORIA-NUEVA
           END-START.

       LEER-HISTORIA.
           READ HISTORIA-CLINICA NEXT WITH NO LOCK AT END GO TO VERIFICAR-HISTORIA.

           IF COD-PACI = ID-HC
              CONTINUE
           ELSE 
              GO TO VERIFICAR-HISTORIA
           END-IF 

           IF ESTADO-HC = 1
              MOVE 1 TO SW-OPEN
              GO TO VERIFICAR-HISTORIA
           END-IF

           MOVE LLAVE-HC   TO LLAVE-HC-W

           GO TO LEER-HISTORIA.

       VERIFICAR-HISTORIA.
           CLOSE HISTORIA-CLINICA.
           IF SW-OPEN  = ZERO
              CONTINUE
           ELSE
              MOVE "99"               TO MSJ1-HTML
              MOVE "Ya existe historia clinica!"  TO MSJ2-HTML
              MOVE LLAVE-HC   TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       HISTORIA-NUEVA.

           MOVE 00                 TO DAT0-ENV
           MOVE LLAVE-HC-W         TO LLAVE-HC-ENV
           MOVE COD-PACI           TO COD-PACI-ENV
           MOVE TIPO-ID-PACI       TO TIPO-ID-PACI-ENV
           MOVE 1ER-APEL-PACI      TO 1ER-APEL-PACI-ENV
           MOVE 2DO-APEL-PACI      TO 2DO-APEL-PACI-ENV
           MOVE NOMBRE-PACI        TO NOMBRE-PACI-ENV
           MOVE NACIM-PACI         TO NACIM-PACI-ENV
           MOVE SEXO-PACI          TO SEXO-PACI-ENV
           MOVE CIUDAD-PACI        TO CIUDAD-PACI-ENV
           MOVE ZONA-PACI          TO ZONA-PACI-ENV
           MOVE TIPO-PACI          TO TIPO-PACI-ENV
           MOVE EPS-PACI           TO EPS-PACI-ENV
           MOVE TIPO-AFIL-PACI     TO TIPO-AFIL-PACI-ENV
           MOVE NRO-AFIL-PACI      TO NRO-AFIL-PACI-ENV
           MOVE ID-COTIZ-PACI      TO ID-COTIZ-PACI-ENV
           MOVE ESTRATO-PACI       TO ESTRATO-PACI-ENV
           MOVE ACOMPA-PACI        TO ACOMPA-PACI-ENV
           MOVE TELEFONO-PACI      TO TELEFONO-PACI-ENV
           MOVE DIRECC-PACI        TO DIRREC-PACI-ENV
           MOVE OCUP-V8-PACI       TO OCUP-V8-PACI-ENV
           MOVE DERECHO-PACI       TO DERECHO-PACI-ENV
           MOVE CRONICO-PACI       TO CRONICO-PACI-ENV
           MOVE PARENT-PACI        TO PARENT-PACI-ENV
           MOVE OBSERV-PACI        TO OBSERV-PACI-ENV
           MOVE NIT-FACT-PACI      TO NIT-FACT-PACI-ENV
           MOVE REST-APLI-PACI     TO REST-APLI-PACI-ENV
           MOVE REST-DROG-PACI     TO REST-DROG-PACI-ENV
           MOVE REST-CIRU-PACI     TO REST-CIRU-PACI-ENV
           MOVE REST-LABO-PACI     TO REST-LABO-PACI-ENV
           MOVE REST-IMAG-PACI     TO REST-IMAG-PACI-ENV
           MOVE REST-ESTA-PACI     TO REST-ESTA-PACI-ENV
           MOVE REST-CONS-PACI     TO REST-CONS-PACI-ENV
           MOVE REST-TERF-PACI     TO REST-TERF-PACI-ENV
           MOVE REST-TERO-PACI     TO REST-TERO-PACI-ENV
           MOVE REST-ODON-PACI     TO REST-ODON-PACI-ENV
           MOVE REST-PYP-PACI      TO REST-PYP-PACI-ENV.

010700 PAGINA-CONFIG.

010720     MOVE "datosrecibidos"  TO COBW3-CNV-NAME
010730     MOVE DATOS-ENVIO       TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV"   USING COBW3
010750
010760     MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
010770     PERFORM ABRIR-HTML.
007980   
007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.

008040 COPY "P:\PROG\PROYECT\FUENTES\SC-WEB.CBL".
