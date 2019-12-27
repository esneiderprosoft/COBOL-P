      *=================================================================
      * GRABAR ACTUALIZACION MAESTRO PACIENTES - SAL7767 - ANTERIOR SER110C
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

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
       COPY "..\..\FUENTES\FD-PACIE.CBL".
     
       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LLEGADA-W                    PIC X(1000).
       77 NOM-PACIE-W                  PIC X(70).
     
       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(30).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 NOV-LLEGADA-W             PIC X.
        
       01 REG-PACI-W.
          02 COD-PACI-W                          PIC X(15).
          02 TIPO-ID-PACI-W                      PIC X(3).
          02 DESCRIP-PACI-W.
             03 LLAVE-APEL-PACI-W.
                05 1ER-APEL-PACI-W.
                   07 INICIAL-PACI-W             PIC X.
                   07 RESTO-PACI-W               PIC X(14).
                05 2DO-APEL-PACI-W               PIC X(15).
             03 NOMBRE-PACI-W.
                05 1ER-NOM-PACI-W                PIC X(12).
                05 2DO-NOM-PACI-W                PIC X(12).
          02 DATOS-PACI-W.
             03 NACIM-PACI-W.
                05 ANO-PACI-W                    PIC 9(4).
                05 MES-PACI-W                    PIC 99.
                05 DIA-PACI-W                    PIC 99.
             03 SEXO-PACI-W                      PIC X.
             03 CIUDAD-PACI-W.
                05 DPTO-PACI-W                   PIC 99.
                05 CIUD-PACI-W                   PIC 9(3).
             03 ZONA-PACI-W                      PIC X.
             03 TIPO-PACI-W                      PIC X.
             03 EPS-PACI-W.
                05 EPS1-PACI-W                   PIC X(3).
                05 EPS2-PACI-W                   PIC X(3).
             03 TIPO-AFIL-PACI-W                 PIC 9.
             03 NRO-AFIL-PACI-W                  PIC X(15).
             03 ID-COTIZ-PACI-W                  PIC X(15).
             03 CONTRATO-PACI-W                  PIC X(15).
             03 FICHA-PACI-W                     PIC X(6).
             03 ESTRATO-PACI-W                   PIC 9.
             03 ACOMPA-PACI-W                    PIC X(30).
             03 CLASIF-PACI-W                    PIC XX.
             03 EST-CIV-PACI-W                   PIC X.
             03 TELEFONO-PACI-W                  PIC X(12).
             03 DIRECC-PACI-W                    PIC X(30).
             03 OCUP-PACI-W                      PIC 9(3).
             03 DERECHO-PACI-W                   PIC X.
             03 FILLER                         PIC 99.
             03 CRONICO-PACI-W                   PIC X.
             03 PARENT-PACI-W                    PIC 99.
             03 EMPRESA-PACI-W                   PIC X(30).
             03 OBSERV-PACI-W                    PIC X(50).
             03 NOV-PACI-W                       PIC 9.
             03 FECHA-NOV-PACI-W.
                05 ANO-NOV-PACI-W                PIC 9(4).
                05 MES-NOV-PACI-W                PIC 99.
                05 DIA-NOV-PACI-W                PIC 99.
             03 FECHA-VENCE-PACI-W.
                05 ANO-VENCE-PACI-W              PIC 9(4).
                05 MES-VENCE-PACI-W              PIC 99.
                05 DIA-VENCE-PACI-W              PIC 99.
             03 FECHA-AFIL-PACI-W.
                05 ANO-AFIL-PACI-W               PIC 9(4).
                05 MES-AFIL-PACI-W               PIC 99.
                05 DIA-AFIL-PACI-W               PIC 99.
             03 RESTRIC-PACI-W.
                05 REST-APLI-PACI-W              PIC X.
                05 REST-DROG-PACI-W              PIC X.
                05 REST-CIRU-PACI-W              PIC X.
                05 REST-LABO-PACI-W              PIC X.
                05 REST-IMAG-PACI-W              PIC X.
                05 REST-ESTA-PACI-W              PIC X.
                05 REST-CONS-PACI-W              PIC X.
                05 REST-TERF-PACI-W              PIC X.
                05 REST-TERO-PACI-W              PIC X.
                05 REST-ODON-PACI-W              PIC X.
                05 REST-PYP-PACI-W               PIC X.
             03 HEMOCLAS-PACI-W.
                05 GRP-SANG-PACI-W               PIC XX.
                05 RH-PACI-W                     PIC X.
             03 ULT-MAMO-PACI-W.
                05 ANO-MAMO-PACI-W               PIC 9(4).
                05 MES-MAMO-PACI-W               PIC 99.
             03 NIT-FACT-PACI-W                  PIC 9(10).
             03 FECHA-NIT-PACI-W.    
                05 ANO-NIT-PACI-W                PIC 9(4).
                05 MES-NIT-PACI-W                PIC 99.
                05 DIA-NIT-PACI-W                PIC 99.
             03 ANTECED-CANCER-PACI-W            PIC X.
             03 ETNIA-PACI-W                     PIC X.
             03 PADRE-PACI-W                     PIC X(30).
             03 MADRE-PACI-W                     PIC X(30).
             03 CEL-PACI-W                       PIC X(12).
             03 TEL-ACOM-PACI-W                  PIC X(10).
             03 COPAGO-PACI-W                    PIC X.
             03 ALT-COS-PACI-W                   PIC X.
             03 EMB-ALTO-RIESG-PACI-W            PIC X.
             03 NIV-ESTUD-PACI-W                 PIC X.
             03 CERT-ESTUD-PACI-W.
                05 ANO-CERT-ESTUD-PACI-W         PIC 9(4).
                05 MES-CERT-ESTUD-PACI-W         PIC 99.
             03 FECHA-RET-PACI-W.
                05 ANO-RET-PACI-W                PIC 9(4).
                05 MES-RET-PACI-W                PIC 99.
                05 DIA-RET-PACI-W                PIC 99.
             03 E-MAIL-PACI-W                    PIC X(80).
             03 MED-FAMI-PACI-W                  PIC 9(10).
             03 INSTITUTO-PACI-W                 PIC X(12).
             03 PATOL-CRONIC-PACI-W              PIC 9(3).
             03 PERI-ESTUD-PACI-W                PIC 9.
             03 CERT-ECONO-PACI-W.
                05 ANO-CERT-ECO-PACI-W           PIC 9(4).
                05 MES-CERT-ECO-PACI-W           PIC 99.
             03 PERI-ECO-PACI-W                  PIC 9.
             03 ETNIA-IND-PACI-W                 PIC XX.
             03 TIPO-POBL-PACI-W                 PIC XX.
             03 DISCAP-PACI-W                    PIC X.
             03 CABEZA-FAM-PACI-W                PIC 9.
             03 TUTELA-PACI-W                    PIC X.
             03 OCUP-V8-PACI-W                   PIC 9(4).
             03 PROG-ESP-PACI-W                  PIC X.
             03 MULTICONSUL-PACI-W               PIC X.
             03 COD-ORIGEN-PACI-W                PIC X(15).
             03 DEMAN-INDU-PACI-W                PIC X.
             03 FECHA-DEMAN-INDU-PACI-W.
                05 ANO-DEMAN-INDU-PACI-W         PIC 9(4).
                05 MES-DEMAN-INDU-PACI-W         PIC 99.
                05 DIA-DEMAN-INDU-PACI-W         PIC 99.
             03 VICTI-CONFLICTO-PACI-W           PIC X.
             03 BLOQUEO-HC-PACI-W                PIC X.
             03 NOM-COMUNIDAD-PACI-W             PIC 9(3).
             03 NOM-RESGUARDO-PACI-W             PIC 9(3).
             03 PAIS-ORIG-PACI-W                 PIC X(3).
             03 CONT-WEB-PACI-W                  PIC X(8).
             03 REGIS-WEB-PACI-W                 PIC 9.
             03 PORTABILIDAD-PACI-W              PIC X.
             03 CIUDAD-ASEG-PACI-W               PIC 9(5).
             03 VICT-ABUSO-SEX-PACI-W            PIC X.
             03 LUGAR-ID-PACI-W                  PIC X(5).
             03 FILLER                         PIC X(2994).
             03 NOTAS-PACI-W.
                05 FECHA-NOTA-PAC2-W.
                   07 ANO-NOTA-PAC2-W            PIC 9(4).
                   07 MES-NOTA-PAC2-W            PIC 99.
                   07 DIA-NOTA-PAC2-W            PIC 99.
                05 OBSERV-NOTA-PAC2-W.
                   07 OBSERV1-NOTA-PAC2-W.
                      10 OBSERV1A-NOTA-PAC2-W    PIC X(40).
                      10 OBSERV1B-NOTA-PAC2-W    PIC X(20).
                      10 OBSERV1C-NOTA-PAC2-W    PIC X(20).
                   07 OBSERV2-NOTA-PAC2-W        PIC X(80).
                   07 OBSERV3-NOTA-PAC2-W        PIC X(80).
                   07 OBSERV4-NOTA-PAC2-W        PIC X(80).
                   07 OBSERV5-NOTA-PAC2-W        PIC X(80).
                05 OPER-OBSE-NOTA-PAC2-W         PIC X(4).
                05 FACT-NOTA-PAC2-W.
                   07 PREFIJO-NOTA-PAC2-W        PIC X.
                   07 NUMERO-NOTA-PAC2-W         PIC 9(6).
                05 FILLER                      PIC X(2000).
             03 DATO-ACT-PACI-W.
                05 OPER-CREA-PACI-W              PIC X(4).
                05 FECHA-CREA-PACI-W.
                   07 ANO-CREA-PACI-W            PIC 9(4).
                   07 MES-CREA-PACI-W            PIC 99.
                   07 DIA-CREA-PACI-W            PIC 99.
                05 HORA-CREA-PACI-W.
                   07 HR-CREA-PACI-W             PIC 99.
                   07 MN-CREA-PACI-W             PIC 99.
                05 OPER-CORR-PACI-W              PIC X(4).
                05 FECHA-CORR-PACI-W.
                   07 ANO-CORR-PACI-W            PIC 9(4).
                   07 MES-CORR-PACI-W            PIC 99.
                   07 DIA-CORR-PACI-W            PIC 99.
                05 HORA-CORR-PACI-W.
                   07 HR-CORR-PACI-W             PIC 99.
                   07 MN-CORR-PACI-W             PIC 99.

       01 DATOS-ENVIO.
          02 DAT0-ENV                           PIC XX.
          02 FILLER                             PIC X VALUE "|".
          02 DAT1-ENV                           PIC X(50).   
               
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
              MOVE "SAL7767-13"             TO MSJ3-HTML
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
              MOVE "SAL7767-13"             TO MSJ3-HTML
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
              MOVE "SAL7767-13"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       END DECLARATIVES.
       
       INICIAR-IIS.
           MOVE LOW-VALUE TO COBW3.
           MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
           CALL "COBW3_INIT" USING COBW3.
         
       LEER-DATO-HTML.
           MOVE "datosh" TO COBW3-SEARCH-DATA.
           CALL "COBW3_GET_VALUE" USING COBW3.
           MOVE COBW3-GET-DATA    TO LLEGADA-W.

           INITIALIZE REG-PACI-W.

           UNSTRING LLEGADA-W DELIMITED BY "|"
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, NOV-LLEGADA-W, COD-PACI-W, TIPO-ID-PACI-W, LUGAR-ID-PACI-W, DESCRIP-PACI-W, NACIM-PACI-W, 
                   HEMOCLAS-PACI-W, SEXO-PACI-W, EST-CIV-PACI-W, NIV-ESTUD-PACI-W, ZONA-PACI-W, PADRE-PACI-W, MADRE-PACI-W, DIRECC-PACI-W, TELEFONO-PACI-W,
                   CEL-PACI-W, CIUDAD-PACI-W, OCUP-V8-PACI-W, PAIS-ORIG-PACI-W, ESTRATO-PACI-W, COPAGO-PACI-W, TIPO-PACI-W, INSTITUTO-PACI-W, ETNIA-PACI-W,
                   TIPO-AFIL-PACI-W, PORTABILIDAD-PACI-W, CIUDAD-ASEG-PACI-W, EPS-PACI-W, CONTRATO-PACI-W, FECHA-AFIL-PACI-W, FICHA-PACI-W, NRO-AFIL-PACI-W,
                   FECHA-VENCE-PACI-W, FECHA-DEMAN-INDU-PACI-W, DEMAN-INDU-PACI-W, ID-COTIZ-PACI-W, PARENT-PACI-W, VICTI-CONFLICTO-PACI-W, PROG-ESP-PACI-W,
                   ALT-COS-PACI-W, TUTELA-PACI-W, EMPRESA-PACI-W, CRONICO-PACI-W, PATOL-CRONIC-PACI-W, CLASIF-PACI-W, ACOMPA-PACI-W, TEL-ACOM-PACI-W, CERT-ESTUD-PACI, 
                   PERI-ESTUD-PACI-W, ULT-MAMO-PACI-W, CERT-ECONO-PACI-W, PERI-ECO-PACI-W, MULTICONSUL-PACI-W, RESTRIC-PACI-W, VICT-ABUSO-SEX-PACI-W, DERECHO-PACI-W, OBSERV-PACI-W,
                   DISCAP-PACI-W, EMB-ALTO-RIESG-PACI-W, NIT-FACT-PACI-W, MED-FAMI-PACI-W, E-MAIL-PACI-W, OPER-CREA-PACI-W, FECHA-CREA-PACI-W, HORA-CREA-PACI-W, OPER-CORR-PACI-W,
                   FECHA-CORR-PACI-W, HORA-CORR-PACI-W
 
           END-UNSTRING.
            MOVE 99                    TO MSJ1-HTML
            MOVE LLEGADA-W              TO MSJ2-HTML
            MOVE "SAL7767-13"             TO MSJ3-HTML
            GO TO ENVIAR2-ERROR.
	OTR.	  
			  
           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.


       INICIAR-SESION.
           MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W
          
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.

       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
      
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
         
           INITIALIZE NOM-PACIE-W
                    
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
 
           MOVE "\\" TO NOM-PACIE-W 
                       
           INSPECT NOM-PACIE-W REPLACING FIRST "                    "
                                           BY IP-DATOS-USUNET 
           INSPECT NOM-PACIE-W REPLACING FIRST " "
                                           BY "\"
           INSPECT NOM-PACIE-W REPLACING FIRST "                              "
                                          BY DIR-CONTAB-LLEGADA-W.             
           INSPECT NOM-PACIE-W REPLACING FIRST "                     "
                                            BY "\CONTROL\SC-PACIE.DAT".
          
       ABRIR-ARCHIVO.

           OPEN INPUT ARCHIVO-PACIENTES.
                     
       LEER-MAESTRO.

           MOVE COD-PACI-W TO COD-PACI
           READ ARCHIVO-PACIENTES WITH NO LOCK
                INVALID KEY 
                  MOVE 01 TO SW-INVALID
           END-READ.

           IF NOV-LLEGADA-W = 7 AND SW-INVALID = 01 GO TO NUEVO.
           IF NOV-LLEGADA-W = 7 AND SW-INVALID = 00 GO TO ERROR-1.
           IF NOV-LLEGADA-W = 8 AND SW-INVALID = 00 GO TO CAMBIO.
           IF NOV-LLEGADA-W = 8 AND SW-INVALID = 01 GO TO ERROR-1.
           IF NOV-LLEGADA-W = 9 AND SW-INVALID = 00 GO TO RETIRO.
           IF NOV-LLEGADA-W = 9 AND SW-INVALID = 01 GO TO ERROR-1.

       NUEVO.
        
           MOVE REG-PACI-W TO REG-PACI 

           CLOSE ARCHIVO-PACIENTES.
           OPEN I-O ARCHIVO-PACIENTES.

           WRITE REG-PACI END-WRITE
           CLOSE ARCHIVO-PACIENTES.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO GRABADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       CAMBIO.
            
           CLOSE ARCHIVO-PACIENTES.

           OPEN I-O ARCHIVO-PACIENTES.

           MOVE REG-PACI-W TO REG-PACI 

           REWRITE REG-PACI END-REWRITE
           CLOSE ARCHIVO-PACIENTES.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO MODIFICADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       RETIRO.
           CLOSE ARCHIVO-PACIENTES.

           OPEN I-O ARCHIVO-PACIENTES.

           DELETE ARCHIVO-PACIENTES END-DELETE

           CLOSE ARCHIVO-PACIENTES.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO BORRADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       ERROR-1.
           MOVE 99 TO DAT0-ENV
           MOVE "ERROR" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       ENVIO-DATOS.

           MOVE 00         TO DAT0-ENV
           MOVE "CORRECTO" TO DAT1-ENV.
              
   

       PAGINA-CONFIG.	 
           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO      TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3
         
           MOVE "..\..\FRAMEWORKS\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
           PERFORM ABRIR-HTML.

       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.

              
       COPY "..\..\FUENTES\SC-WEB19.CBL".
