      *=================================================================
      * FACTURACION - CONSULTA DATOS DE NUMERACION 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\..\FUENTES\FS-USUNET.CBL".
           COPY "..\..\FUENTES\FS-SESION.CBL".
  
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

           SELECT ARCHIVO-TARIFAS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TAR-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS COD-TAR
                  FILE STATUS IS OTR-STAT.
      *"../CONTROL/SC-ARCHTAR.DAT"

           SELECT ARCHIVO-SERV-HOSP LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-SERHO-W
      *"/PROG/DATOS/SC-SERVHOS.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CODIGO-SERHO
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-COSTO LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-COSTO-W
      *"../control/SC-ARCHCOS.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS COD-COSTO
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-DIVISION LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-DIV-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS LLAVE-DIV
                  ALTERNATE RECORD KEY IS DESCRIP-DIV WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-COD-IPS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-CODIP-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-IPS
                  ALTERNATE RECORD KEY IS
                  NOMBRE-IPS WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-CIUDAD  LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-CIU-W
      *"P:\PROG\DATOS\SC-ARCHCIUD.DAT";
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-CIU
                  ALTERNATE RECORD KEY IS NOMBRE-CIU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

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


       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-NUMER19.CBL".
       COPY "..\..\FUENTES\FD-TARIF.CBL".
       COPY "..\..\FUENTES\FD-SERHO.CBL".
       COPY "..\..\FUENTES\FD-COSTO.CBL".
       COPY "..\..\FUENTES\FD-DIVIS.CBL".
       COPY "..\..\FUENTES\FD-CODIP.CBL".
       COPY "..\..\FUENTES\FD-CIUDA.CBL".
       COPY "..\..\FUENTES\FD-TERCE.CBL".
       COPY "..\..\FUENTES\FD-PACIE.CBL".

005190 WORKING-STORAGE SECTION.
005200 COPY "..\..\FUENTES\COBW3.CBL".
005210 COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-NUMER-W                     PIC X(70).
       77 NOM-TAR-W                       PIC X(70).
       77 NOM-SERHO-W                     PIC X(70).
       77 NOM-COSTO-W                     PIC X(70).
       77 NOM-DIV-W                       PIC X(70).
       77 NOM-CODIP-W                     PIC X(70).
       77 NOM-CIU-W                       PIC X(70).
       77 NOM-TER-W                       PIC X(70).
       77 NOM-PACIE-W                     PIC X(70).
       01 LINEA-LLEGADA-W                 PIC X(100).


       01 PORCENT-COPAGO-NUM-EDIT         PIC ZZ.ZZ.
  
       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W               PIC X(15).
             03 FECHA-LLEGAD-W            PIC X(8).
             03 HORA-LLEGAD-W             PIC X(6).
          02 DIR-CONTAB-LLEGADA-W         PIC X(30).
          02 MES-CONTAB-LLEGADA-W         PIC XX.
          02 LLAVE-NUM-W.
             05 PREFIJO-W                 PIC X.
             05 NRO-W                     PIC 9(6).
          02 OPERADOR-LLEGADA-W           PIC X(4).

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV               PIC XX.
          02 FILLER                       PIC X      VALUE "|".
          02 LLAVE-NUM-ENV.
	     03 PREFIJO-NUM-ENV           PIC X.
             03 NRO-NUM-ENV               PIC 9(6).
          02 FILLER                       PIC X      VALUE "|".
          02 NIT-NUM-ENV                      PIC 9(10).
          02 FILLER                       PIC X      VALUE "|".
          02 DESCRIP-NUM-ENV.
             03 DESCRIP1-NUM-ENV          PIC X(30).
             03 DESCRIP2-NUM-ENV          PIC X(10).
          02 FILLER                       PIC X      VALUE "|".
          02 CONVENIO-NUM-ENV             PIC XX.
          02 FILLER                       PIC X      VALUE "|". 
          02 DESCRIP-TAR-ENV              PIC X(25).
          02 FILLER                       PIC X      VALUE "|".
          02 ESTADO-NUM-ENV               PIC X.
          02 FILLER                       PIC X      VALUE "|".
          02 PORC-RETENC-NUM-ENV          PIC 99.
          02 FILLER                       PIC X      VALUE "|".
          02 SEG-RIPS-NUM-ENV             PIC X.
          02 FILLER                       PIC X      VALUE "|".
          02 CTA-PIC-NUM-ENV              PIC 9(5).
          02 FILLER                       PIC X      VALUE "|".
          02 ID-PAC-NUM-ENV               PIC X(15).
          02 FILLER                       PIC X      VALUE "|".
          02 DESCRIP-PACI-ENV.
             03 LLAVE-APEL-PACI.
                05 1ER-APEL-PACI.
                   07 INICIAL-PACI        PIC X.
                   07 RESTO-PACI          PIC X(14).
                05 2DO-APEL-PACI          PIC X(15).
             03 NOMBRE-PACI.
                05 1ER-NOM-PACI           PIC X(12).
                05 2DO-NOM-PACI           PIC X(12).
          02 FILLER                       PIC X      VALUE "|".
          02 TIPO-FAC-NUM-ENV             PIC 9.
          02 FILLER                       PIC X      VALUE "|".
          02 HAB-NUM-ENV                  PIC X(4).
          02 FILLER                       PIC X      VALUE "|".
          02 PORCENT-COPAGO-NUM-ENV       PIC 99V99.
          02 FILLER                       PIC X      VALUE "|".
          02 FECHA-ING-NUM-ENV.
             05 LLAVE-MES-ING-NUM-ENV.
                07 ANO-ING-NUM-ENV        PIC 9(4).
                07 MES-ING-NUM-ENV        PIC 99.
             05 DIA-ING-NUM-ENV           PIC 99.
          02 FILLER                       PIC X      VALUE "|".
          02 FECHA-RET-NUM-ENV.
             05 LLAVE-MES-RET-NUM-ENV.
                07 ANO-RET-NUM-ENV        PIC 9(4).
                07 MES-RET-NUM-ENV        PIC 99.
             05 DIA-RET-NUM-ENV           PIC 99.
          02 FILLER                       PIC X      VALUE "|".
          02 HORA-ING-NUM-ENV.
             05 HR-ING-NUM-ENV            PIC 99.
             05 MN-ING-NUM-ENV            PIC 99.
          02 FILLER                       PIC X      VALUE "|".
          02 HORA-RET-NUM-ENV.
             05 HR-RET-NUM-ENV            PIC 99.
             05 MN-RET-NUM-ENV            PIC 99. 
          02 FILLER                       PIC X      VALUE "|".
          02 SERVICIO-NUM-ENV             PIC XX.
          02 FILLER                       PIC X      VALUE "|".
          02 DESCRIP-SERHO-ENV            PIC X(25).
          02 FILLER                       PIC X      VALUE "|".
          02 RED-EXTER-NUM-ENV            PIC X.
          02 FILLER                       PIC X      VALUE "|".
          02 CONTRATO-NUM-ENV             PIC X(20).
          02 FILLER                       PIC X      VALUE "|".
          02 DIVISION-NUM-ENV             PIC XX.
          02 FILLER                       PIC X      VALUE "|".
      *    02 DESCRIP-DIV-ENV              PIC X(20).
      *    02 FILLER                       PIC X      VALUE "|".
          02 FACT-CAPIT-NUM-ENV.
             05 PRE-CAPIT-NUM-ENV         PIC X.
             05 NRO-CAPIT-NUM-ENV         PIC 9(6).
          02 FILLER                       PIC X      VALUE "|".
          02 FORMA-COPAG-NUM-ENV          PIC 9.
          02 FILLER                       PIC X      VALUE "|".
          02 C-COSTO-NUM-ENV.
             05 C-COSTO1-NUM-ENV          PIC X.
	     05 C-COSTO2-NUM-ENV          PIC X.
             05 C-COSTO4-NUM-ENV          PIC X(2).
          02 FILLER                       PIC X      VALUE "|".
      *    02 NOMBRE-COSTO-ENV.
      *        05 NOMBRE1-COSTO            PIC X.
      *        05 NOMBRE2-COSTO            PIC X(14).
      *        05 NOMBRE3-COSTO            PIC X(5).
      *    02 FILLER                       PIC X      VALUE "|".
          02 ENVIO-NUM-ENV                PIC 9(6).
          02 FILLER                       PIC X      VALUE "|".
          02 CONTROL-CAP-NUM-ENV          PIC 9(4).
          02 FILLER                       PIC X      VALUE "|".
          02 OBSERV-NUM-ENV.
             05 OBSERV1-NUM-ENV.
                07 OBSERV1A-NUM-ENV           PIC X(40).
                07 OBSERV1B-NUM-ENV           PIC X(20).
             05 OBSERV2-NUM-ENV.
                07 OBSERV2A-NUM-ENV           PIC X(40).
                07 OBSERV2B-NUM-ENV           PIC X(20).
             05 OBSERV3-NUM-ENV.
                07 OBSERV3A-NUM-ENV           PIC X(40).
                07 OBSERV3B-NUM-ENV           PIC X(20).
             05 OBSERV4-NUM-ENV.
                07 OBSERV4A-NUM-ENV           PIC X(40).
                07 OBSERV4B-NUM-ENV           PIC X(20).          
          02 FILLER                           PIC X      VALUE "|".
          02 TIPO-PACI-NUM-ENV                PIC X.
          02 FILLER                           PIC X      VALUE "|".
          02 DET-CARTERA-NUM-ENV              PIC X(50).
          02 FILLER                           PIC X      VALUE "|".
          02 CTL-NROPACI-NUM-ENV              PIC X.
          02 FILLER                           PIC X      VALUE "|".
          02 CIS-NUM-ENV                      PIC X.
          02 FILLER                           PIC X      VALUE "|".
          02 MYT-NUM-ENV                      PIC X.
          02 FILLER                           PIC X      VALUE "|".
          02 CONTROL-X-SERV-NUM-ENV           PIC X.
          02 FILLER                           PIC X      VALUE "|".
          02 CONTROL-CL-0-NUM-ENV             PIC X.
          02 FILLER                           PIC X      VALUE "|".
          02 CONTROL-CL-1-NUM-ENV             PIC X.
          02 FILLER                           PIC X      VALUE "|".
          02 CONTROL-CL-2-NUM-ENV             PIC X.
          02 FILLER                           PIC X      VALUE "|".
          02 CONTROL-CL-3-NUM-ENV             PIC X.
          02 FILLER                           PIC X      VALUE "|".
          02 CONTROL-CL-4-NUM-ENV             PIC X.
          02 FILLER                           PIC X      VALUE "|".
          02 CONTROL-CL-5-NUM-ENV             PIC X.
          02 FILLER                           PIC X      VALUE "|".
          02 CONTROL-CL-6-NUM-ENV             PIC X.
          02 FILLER                           PIC X      VALUE "|".
          02 CONTROL-CL-7-NUM-ENV             PIC X.
          02 FILLER                           PIC X      VALUE "|".
          02 ART-IVA-NUM-ENV                  PIC X.
          02 FILLER                           PIC X      VALUE "|".
          02 NRO-POL-NUM-ENV                  PIC X(30).
          02 FILLER                           PIC X      VALUE "|".
          02 DIV-NUM-ENV                      PIC XX.
          02 FILLER                           PIC X      VALUE "|".
          02 DIAS-EST-NUM-ENV                 PIC 9(3).
          02 FILLER                           PIC X      VALUE "|".
          02 CLASIF-NUM-ENV                   PIC 9.
          02 FILLER                           PIC X      VALUE "|".
          02 ENTRA-REMIT-NUM-ENV              PIC X.
          02 FILLER                           PIC X      VALUE "|".
          02 ORIG-REMIT-NUM-ENV               PIC X(12).
          02 FILLER                           PIC X      VALUE "|".
          02 NOMBRE-IPS-ENV.
             05 NOMBRE1-IPS                   PIC X(40).
             05 NOMBRE2-IPS                   PIC X(40).
          02 FILLER                           PIC X      VALUE "|".
          02 TIPO-EVENTO-NUM-ENV              PIC 99.
          02 FILLER                           PIC X      VALUE "|".
          02 CIUDAD-NUM-ENV                   PIC 9(5).
          02 FILLER                           PIC X      VALUE "|".
          02 NOMBRE-CIU-ENV.
             05 NOMBRE1-CIU                   PIC X(16).
             05 NOMBRE2-CIU                   PIC X(4).
          02 FILLER                           PIC X      VALUE "|".
          02 FUNC-AUTOR-ING-NUM-ENV           PIC 9(10).
          02 FILLER                           PIC X      VALUE "|".
          02 DESCRIP-TER-ENV.
             03 DESCRIP1-TER.
                05 INI1-TER             PIC X.
                05 INI2-TER             PIC X(29).
             03 DESCRIP2-TER            PIC X(20).
          02 FILLER                           PIC X      VALUE "|".
          02 NRO-AUTORI-NUM-ENV               PIC X(15).
          02 FILLER                           PIC X      VALUE "|".
          02 OBSERV-CRE-NUM-ENV               PIC X(50).
          02 FILLER                           PIC X      VALUE "|".
          02 DATO-OPER-NUM-ENV.
             05 OPER-NUM                   PIC X(4).
             05 FECHA-CRE-NUM.
                07 ANO-CRE-NUM             PIC 9(4).
	        07 MES-CRE-NUM             PIC 99.
	        07 DIA-CRE-NUM             PIC 99.
             05 FECHA-MOD-NUM.
                07 ANO-MOD-NUM             PIC 9(4).
                07 MES-MOD-NUM             PIC 99.
                07 DIA-MOD-NUM             PIC 99.
             05 OPER-BLOQ-NUM              PIC X(4).
          02 FILLER                       PIC X      VALUE "|".
          02 ZONA-TER-ENV.
             03 ZONA1-TER                 PIC X.
             03 ZONA2-TER                 PIC X.
          02 FILLER                       PIC X      VALUE "|".
          02 OPER-MOD-NUM-ENV             PIC X(4).
          02 FILLER                       PIC X      VALUE "|".
          02 NACIM-PACI-ENV.
             03 ANO-PACI                    PIC 9(4).
             03 MES-PACI                    PIC 99.
             03 DIA-PACI                    PIC 99.
            


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
              MOVE "SER108-03"              TO MSJ3-HTML
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
              MOVE "SER108-03"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-NUMERACION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-NUMER-W          TO MSJ2-HTML
              MOVE "SER108-03"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TARIFAS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-TAR-W            TO MSJ2-HTML
              MOVE "SER108-03"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF. 
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SERV-HOSP.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-SERHO-W          TO MSJ2-HTML
              MOVE "SER108-03"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-COSTO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-COSTO-W          TO MSJ2-HTML
              MOVE "SER108-03"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-DIVISION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-DIV-W          TO MSJ2-HTML
              MOVE "SER108-03"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-COD-IPS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-CODIP-W          TO MSJ2-HTML
              MOVE "SER108-03"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CIUDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-CIU-W            TO MSJ2-HTML
              MOVE "SER108-03"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TERCEROS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-TER-W            TO MSJ2-HTML
              MOVE "SER108-03"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-PACIE-W          TO MSJ2-HTML
              MOVE "SER108-03"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

006720 END DECLARATIVES.
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
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, LLAVE-NUM-W, OPERADOR-LLEGADA-W
           END-UNSTRING.

001855     MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.

000797 INICIAR-SESION.
000806     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W

      *     MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W

000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.
006840
006900 ABRIR-USUARIO.
006910     INITIALIZE OTR-STAT.
006920     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W

      *     MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W

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
007230        MOVE "Validacion de Sesion"                  TO MSJ1-HTML
007240        MOVE "Falta configurar usuarios"             TO MSJ2-HTML
007250        MOVE "Sc"                                    TO MSJ3-HTML
007260        GO TO ENVIAR2-ERROR
007270     END-IF.
007280 
007350

        ASIGNAR-NOMBRES. 

           INITIALIZE NOM-NUMER-W NOM-TAR-W NOM-TER-W NOM-PACIE-W

           MOVE "\\" TO NOM-NUMER-W NOM-TAR-W NOM-TER-W NOM-PACIE-W

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-NUMER-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-NUMER-W REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-NUMER-W REPLACING FIRST "                              " 
                                            BY DIR-CONTAB-LLEGADA-W.

           MOVE NOM-NUMER-W TO NOM-TAR-W
                               NOM-COSTO-W
                               NOM-DIV-W
                               NOM-TER-W
                               NOM-PACIE-W

           INSPECT NOM-NUMER-W REPLACING FIRST "                     "
                                            BY "\CONTROL\SC-NUMER.DAT".

           INSPECT NOM-TAR-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-ARCHTAR.DAT".

           INSPECT NOM-COSTO-W REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHCOS.DAT".

           INSPECT NOM-DIV-W REPLACING FIRST "                     "
                                          BY "\CONTROL\SC-DIVIS.DAT".

           INSPECT NOM-TER-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-ARCHTER.DAT".

           INSPECT NOM-PACIE-W REPLACING FIRST "                     "
                                          BY "\CONTROL\SC-PACIE.DAT".

           INITIALIZE NOM-SERHO-W NOM-CODIP-W NOM-CIU-W

           MOVE "\\" TO NOM-SERHO-W NOM-CODIP-W NOM-CIU-W

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-SERHO-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           MOVE NOM-SERHO-W TO NOM-CODIP-W
                               NOM-CIU-W

           INSPECT NOM-SERHO-W REPLACING FIRST "                          "
                                            BY "\PROG\DATOS\SC-SERVHOS.DAT"

           INSPECT NOM-CODIP-W REPLACING FIRST "                        "
                                            BY "\PROG\DATOS\SC-CODIP.DAT"

           INSPECT NOM-CIU-W REPLACING FIRST "                           "
                                          BY "\PROG\DATOS\SC-ARCHCIUD.DAT".

       ABRIR-ARCHIVOS.
        
           OPEN INPUT ARCHIVO-NUMERACION
                      ARCHIVO-TARIFAS
                      ARCHIVO-SERV-HOSP
                      ARCHIVO-COSTO
                      ARCHIVO-DIVISION
                      ARCHIVO-COD-IPS
                      ARCHIVO-CIUDAD 
                      ARCHIVO-TERCEROS
                      ARCHIVO-PACIENTES.

           MOVE LLAVE-NUM-W TO LLAVE-NUM

           READ ARCHIVO-NUMERACION 
                INVALID KEY 
                     MOVE "01" TO SW-INVALID-ENV
                NOT INVALID KEY
                     MOVE "00" TO SW-INVALID-ENV
           END-READ.

           MOVE LLAVE-NUM  TO LLAVE-NUM-ENV

           MOVE NIT-NUM            TO NIT-NUM-ENV

           MOVE NIT-NUM-ENV        TO COD-TERCERO
           READ ARCHIVO-TERCEROS WITH NO LOCK
                INVALID KEY 
                   MOVE COD-TERCERO   TO ZONA-TER
           END-READ
           MOVE ZONA-TER           TO ZONA-TER-ENV                   
           MOVE DESCRIP-NUM        TO DESCRIP-NUM-ENV                  
           MOVE CONVENIO-NUM       TO CONVENIO-NUM-ENV

           MOVE CONVENIO-NUM-ENV   TO COD-TAR
           READ ARCHIVO-TARIFAS WITH NO LOCK
                INVALID KEY 
                   MOVE COD-TAR    TO DESCRIP-TAR
           END-READ

           MOVE DESCRIP-TAR        TO DESCRIP-TAR-ENV           
           MOVE ESTADO-NUM         TO ESTADO-NUM-ENV
           MOVE PORC-RETENC-NUM    TO PORC-RETENC-NUM-ENV
           MOVE SEG-RIPS-NUM       TO SEG-RIPS-NUM-ENV
           MOVE CTA-PIC-NUM        TO CTA-PIC-NUM-ENV
           MOVE ID-PAC-NUM         TO ID-PAC-NUM-ENV

           MOVE ID-PAC-NUM-ENV     TO COD-PACI
           READ ARCHIVO-PACIENTES WITH NO LOCK
                INVALID KEY 
                   MOVE COD-PACI   TO DESCRIP-PACI
                   MOVE COD-PACI   TO NACIM-PACI
           END-READ
           MOVE DESCRIP-PACI       TO DESCRIP-PACI-ENV
           MOVE NACIM-PACI         TO NACIM-PACI-ENV 
           MOVE TIPO-FAC-NUM       TO TIPO-FAC-NUM-ENV 
           MOVE HAB-NUM            TO HAB-NUM-ENV
           MOVE PORCENT-COPAGO-NUM TO PORCENT-COPAGO-NUM-EDIT
           MOVE PORCENT-COPAGO-NUM-EDIT TO PORCENT-COPAGO-NUM-ENV
           MOVE FECHA-ING-NUM      TO FECHA-ING-NUM-ENV
           MOVE FECHA-RET-NUM      TO FECHA-RET-NUM-ENV
           MOVE HORA-ING-NUM       TO HORA-ING-NUM-ENV
           MOVE HORA-RET-NUM       TO HORA-RET-NUM-ENV
           MOVE SERVICIO-NUM       TO SERVICIO-NUM-ENV

           MOVE SERVICIO-NUM-ENV   TO CODIGO-SERHO
           READ ARCHIVO-TARIFAS WITH NO LOCK
                INVALID KEY 
                   MOVE CODIGO-SERHO TO DESCRIP-SERHO
           END-READ

           MOVE DESCRIP-SERHO      TO DESCRIP-SERHO-ENV  
           MOVE RED-EXTER-NUM      TO RED-EXTER-NUM-ENV
           MOVE CONTRATO-NUM       TO CONTRATO-NUM-ENV
           MOVE DIVISION-NUM       TO DIVISION-NUM-ENV

      *     MOVE DIVISION-NUM-ENV    TO LLAVE-DIV
      *     READ ARCHIVO-DIVISION WITH NO LOCK
      *          INVALID KEY 
      *             MOVE LLAVE-DIV TO DESCRIP-DIV
      *     END-READ
      *     MOVE DESCRIP-DIV        TO DESCRIP-DIV-ENV   
           MOVE FACT-CAPIT-NUM     TO FACT-CAPIT-NUM-ENV
           MOVE FORMA-COPAG-NUM    TO FORMA-COPAG-NUM-ENV
           MOVE C-COSTO-NUM        TO C-COSTO-NUM-ENV

      *     MOVE C-COSTO-NUM-ENV    TO COD-COSTO
      *     READ ARCHIVO-COSTO WITH NO LOCK
      *          INVALID KEY 
      *             MOVE COD-COSTO TO NOMBRE-COSTO
      *     END-READ
      *     MOVE NOMBRE-COSTO       TO NOMBRE-COSTO-ENV                                     
           MOVE ENVIO-NUM          TO ENVIO-NUM-ENV
           MOVE CONTROL-CAP-NUM    TO CONTROL-CAP-NUM-ENV
           MOVE OBSERV-NUM         TO OBSERV-NUM-ENV
           MOVE TIPO-PACI-NUM      TO TIPO-PACI-NUM-ENV 
           MOVE DET-CARTERA-NUM    TO DET-CARTERA-NUM-ENV
           MOVE CTL-NROPACI-NUM    TO CTL-NROPACI-NUM-ENV
           MOVE CIS-NUM            TO CIS-NUM-ENV
           MOVE MYT-NUM            TO MYT-NUM-ENV
           MOVE CONTROL-X-SERV-NUM TO CONTROL-X-SERV-NUM-ENV
           MOVE CONTROL-CL-0-NUM   TO CONTROL-CL-0-NUM-ENV
           MOVE CONTROL-CL-1-NUM   TO CONTROL-CL-1-NUM-ENV
           MOVE CONTROL-CL-2-NUM   TO CONTROL-CL-2-NUM-ENV
           MOVE CONTROL-CL-3-NUM   TO CONTROL-CL-3-NUM-ENV
           MOVE CONTROL-CL-4-NUM   TO CONTROL-CL-4-NUM-ENV
           MOVE CONTROL-CL-5-NUM   TO CONTROL-CL-5-NUM-ENV
           MOVE CONTROL-CL-6-NUM   TO CONTROL-CL-6-NUM-ENV
           MOVE CONTROL-CL-7-NUM   TO CONTROL-CL-7-NUM-ENV
           MOVE ART-IVA-NUM        TO ART-IVA-NUM-ENV
           MOVE NRO-POL-NUM        TO NRO-POL-NUM-ENV
           MOVE DIV-NUM            TO DIV-NUM-ENV 
           MOVE DIAS-EST-NUM       TO DIAS-EST-NUM-ENV
           MOVE CLASIF-NUM         TO CLASIF-NUM-ENV
           MOVE ENTRA-REMIT-NUM    TO ENTRA-REMIT-NUM-ENV
           MOVE ORIG-REMIT-NUM     TO ORIG-REMIT-NUM-ENV
 
           MOVE ORIG-REMIT-NUM-ENV TO COD-IPS
           READ ARCHIVO-COSTO WITH NO LOCK
                INVALID KEY 
                   MOVE COD-IPS    TO NOMBRE-IPS
           END-READ
           MOVE NOMBRE-IPS         TO NOMBRE-IPS-ENV   
           MOVE TIPO-EVENTO-NUM    TO TIPO-EVENTO-NUM-ENV
           MOVE CIUDAD-NUM         TO CIUDAD-NUM-ENV

           MOVE CIUDAD-NUM-ENV     TO COD-CIU
           READ ARCHIVO-CIUDAD WITH NO LOCK
                INVALID KEY 
                   MOVE COD-CIU    TO DESCRIP-TER
           END-READ
           MOVE DESCRIP-TER        TO DESCRIP-TER-ENV 
  
           MOVE FUNC-AUTOR-ING-NUM TO FUNC-AUTOR-ING-NUM-ENV

           MOVE FUNC-AUTOR-ING-NUM-ENV  TO COD-TERCERO
           READ ARCHIVO-TERCEROS WITH NO LOCK
                INVALID KEY 
                   MOVE COD-TERCERO TO DESCRIP-TER
           END-READ
           MOVE DESCRIP-TER        TO DESCRIP-TER-ENV 
           MOVE NRO-AUTORI-NUM     TO NRO-AUTORI-NUM-ENV
           MOVE OBSERV-CRE-NUM     TO OBSERV-CRE-NUM-ENV
           MOVE DATO-OPER-NUM      TO DATO-OPER-NUM-ENV
           MOVE OPER-MOD-NUM       TO OPER-MOD-NUM-ENV.

           CLOSE ARCHIVO-NUMERACION
                 ARCHIVO-TARIFAS
                 ARCHIVO-SERV-HOSP
                 ARCHIVO-COSTO
                 ARCHIVO-DIVISION
                 ARCHIVO-COD-IPS
                 ARCHIVO-CIUDAD 
                 ARCHIVO-TERCEROS
                 ARCHIVO-PACIENTES.


010700  PAGINA-CONFIG.
010720     MOVE "datosrecibidos" TO COBW3-CNV-NAME
010730     MOVE DATOS-ENVIO       TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV"  USING COBW3
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
