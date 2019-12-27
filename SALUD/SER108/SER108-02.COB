      *=================================================================
      *GRABAR ARCHIVO-APERTURA DE FACTURACION- SALUD
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
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-NUMER19.CBL".

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".
  
       77 NOM-NUMER-W                   PIC X(70).
       01 LINEA-LLEGADA-W               PIC X(500).


       01 PORCENT-COPAGO-EDIT.
          02 ENT-PORCENT-COPAG-EDIT     PIC 9(2).
          02 DEC-PORCENT-COPAG-EDIT     PIC 9(2).     
                                          
       01  DATO-LLEGADA-W.
           02 LLAVE-SESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 DIR-CONTAB-LLEGADA-W      PIC X(30).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 NOV-LLEGADA-W             PIC X.

       01 REG-NUM-W.
          02 LLAVE-NUM-W.
	     03 PREFIJO-NUM-W         PIC X.
             03 NRO-NUM-W             PIC 9(6).
	  02 LLAVE-ELE-NUM-W.
	     03 PREFIJO-ELE-NUM-W     PIC XX.
             03 NRO-ELE-NUM-W         PIC 9(7).
	  02 SUCUR-NUM-W              PIC X(2).
          02 CONVENIO-NUM-W           PIC XX.
          02 ESTADO-NUM-W             PIC X.
          02 NIT-NUM-W                PIC 9(10).
          02 DESCRIP-NUM-W.
             03 DESCRIP1-NUM-W        PIC X(30).
             03 DESCRIP2-NUM-W        PIC X(10).
          02 DATOS-PAC-NUM-W.
             03 NOMBRE-PAC-NUM-W.
                05 NOMBRE1-NUM-W.
                   07 INIC-NOM-NUM-W  PIC X(5).
                   07 REST-NOM-NUM-W  PIC X(25).
                05 NOMBRE2-NUM-W      PIC X(10).
             03 ID-PAC-NUM-W          PIC X(15).
             03 HAB-NUM-W             PIC X(4).
             03 CLASIF-NUM-W          PIC 9.
             03 FECHA-ING-NUM-W.
                05 LLAVE-MES-ING-NUM-W.
                   07 ANO-ING-NUM-W   PIC 9(4).
                   07 MES-ING-NUM-W   PIC 99.
                05 DIA-ING-NUM-W      PIC 99.
             03 FECHA-RET-NUM-W.
                05 LLAVE-MES-RET-NUM-W.
                   07 ANO-RET-NUM-W   PIC 9(4).
                   07 MES-RET-NUM-W   PIC 99.
                05 DIA-RET-NUM-W      PIC 99.
             03 FECHA-PRE-NUM-W.
                05 ANO-PRE-NUM-W      PIC 9(4).
                05 MES-PRE-NUM-W      PIC 99.
                05 DIA-PRE-NUM-W      PIC 99.
             03 OBSERV-NUM-W.
                05 OBSERV1-NUM-W.
                   07 OBSERV1A-NUM-W  PIC X(40).
                   07 OBSERV1B-NUM-W  PIC X(20).
                05 OBSERV2-NUM-W.
                   07 OBSERV2A-NUM-W  PIC X(40).
                   07 OBSERV2B-NUM-W  PIC X(20).
                05 OBSERV3-NUM-W.
                   07 OBSERV3A-NUM-W  PIC X(40).
                   07 OBSERV3B-NUM-W  PIC X(20).
                05 OBSERV4-NUM-W.
                   07 OBSERV4A-NUM-W  PIC X(40).
                   07 OBSERV4B-NUM-W  PIC X(20).
             03 ANEXOS-NUM-W.
                05 ANEXO1-NUM-W.
                   07 ANEXO1A-NUM-W   PIC X(40).
                   07 ANEXO1B-NUM-W   PIC X(20).
                05 ANEXO2-NUM-W.
                   07 ANEXO2A-NUM-W   PIC X(40).
                   07 ANEXO2B-NUM-W   PIC X(20).
                05 ANEXO3-NUM-W.
                   07 ANEXO3A-NUM-W   PIC X(40).
                   07 ANEXO3B-NUM-W   PIC X(20).
             03 DATOS-ESTAD-NUM-W.
                05 NRO-HIST-NUM-W     PIC X(16).
                05 RADICACION-NUM-W   PIC 9(5).
                05 EST-COB-NUM-W      PIC 99.
                05 C-COSTO-NUM-W.
                   07 C-COSTO1-NUM-W  PIC X.
	           07 C-COSTO2-NUM-W  PIC X.
		   07 C-COSTO4-NUM-W  PIC X(2).
                05 SERVICIO-NUM-W     PIC XX.
                05 DIVISION-NUM-W     PIC XX.
                05 HORA-ING-NUM-W.
                   07 HR-ING-NUM-W    PIC 99.
                   07 MN-ING-NUM-W    PIC 99.
                05 DIAS-EST-NUM-W     PIC 9(3).
                05 DIAG-ING-NUM-W     PIC X(4).
                05 DIAG-SAL-NUM-W     PIC X(4).
                05 ESTAD-SAL-NUM-W    PIC 9.
                05 REMITE-NUM-W       PIC X(30).
                05 CIU-REMITE-NUM-W   PIC 9(5).
                05 DIAG-MUER-NUM-W    PIC X(4).
                05 HORA-RET-NUM-W.
                   07 HR-RET-NUM-W    PIC 99.
                   07 MN-RET-NUM-W    PIC 99.
             03 CTL-NROPACI-NUM-W     PIC X.
             03 FILLER              PIC X.
             03 PER-CONT-RAD-NUM-W.
                05 ANO-CONT-RAD-NUM-W PIC 99.
                05 MES-CONT-RAD-NUM-W PIC 99.
             03 CTA-PIC-NUM-W         PIC 9(5).
             03 DIV-NUM-W             PIC XX.
             03 FORMA-COPAG-NUM-W     PIC 9.
             03 RED-EXTER-NUM-W       PIC X.
             03 TIPO-EVENTO-NUM-W     PIC 99.
             03 BOL-SAL-NUM-W.
                05 OPER-BOL-NUM-W     PIC X(4).
                05 FECH-BOL-NUM-W.
                   07 ANO-BOL-NUM-W   PIC 9(4).
		   07 MES-BOL-NUM-W   PIC 99.
		   07 DIA-BOL-NUM-W   PIC 99.
                05 HORA-BOL-NUM-W.
                   07 HR-BOL-NUM-W    PIC X(2).
                   07 MN-BOL-NUM-W    PIC X(2).
             03 FARMA-NUM-W.
                05 FECHA-FARMA-NUM-W.
                   07 ANO-FARMA-NUM-W          PIC 9(4).
                   07 MES-FARMA-NUM-W          PIC 99.
                   07 DIA-FARMA-NUM-W          PIC 99.
                05 HORA-FARMA-NUM-W.
                   07 HR-FARMA-NUM-W           PIC 99.
                   07 MN-FARMA-NUM-W           PIC 99.
                05 OPER-FARMA-NUM-W            PIC X(4).
             03 TIPO-PACI-NUM-W                PIC X.
             03 CONTRATO-ANT-NUM-W             PIC X(9).
             03 CONTROL-CAP-NUM-W              PIC 9(4).
             03 FACT-CAPIT-NUM-W.
                05 PRE-CAPIT-NUM-W             PIC X.
                05 NRO-CAPIT-NUM-W             PIC 9(6).
             03 FILLER                       PIC X(4).
             03 GLOSA-NUM-W                    PIC 9(9).
             03 RESP-GLOSA-NUM-W               PIC 9(9).
             03 GLOSA-DISC-NUM-W               PIC S9(9).
             03 TRASL-GLOSA-NUM-W              PIC 9(9).
             03 CARTA-GLO-NUM-W                PIC X.
             03 ENTRA-REMIT-NUM-W              PIC X.
             03 ORIG-REMIT-NUM-W               PIC X(12).
             03 TIPO-FAC-NUM-W                 PIC 9.
             03 FILLER                       PIC X(4).
	     03 PORC-RETENC-NUM-W              PIC 99.
             03 ENVIO-NUM-W                    PIC 9(6).
             03 SEG-RIPS-NUM-W                 PIC X.
             03 DETALLE-NUM-W                  PIC X(50).
             03 DET-CARTERA-NUM-W              PIC X(50).
             03 COPAGO-EST-NUM-W               PIC 9(9).
             03 PORCENT-COPAGO-NUM-W           PIC 99V99.
             03 CO-PAGOS-NUM-W                 PIC 9(9).
             03 ENVIO-FURIPS-NUM-W             PIC 9(6).
             03 FECHA-GLOSA-NUM-W.
                05 ANO-GLOSA-NUM-W             PIC 9(4).
                05 MES-GLOSA-NUM-W             PIC 9(2).
                05 DIA-GLOSA-NUM-W             PIC 9(2).
             03 CONTRATO-NUM-W                 PIC X(20).
             03 CIS-NUM-W                      PIC X.
             03 NRO-AUTORI-NUM-W               PIC X(15).
             03 FECHA-REPRE-NUM-W.
                05 ANO-REPRE-NUM-W             PIC 9(4).
                05 MES-REPRE-NUM-W             PIC 99.
                05 DIA-REPRE-NUM-W             PIC 99.
             03 MYT-NUM-W                      PIC X.
             03 CIUDAD-NUM-W                   PIC 9(5).
	     03 FECHA-RADGLO-NUM-W.
                05 ANO-RADGLO-NUM-W            PIC 9(4).
                05 MES-RADGLO-NUM-W            PIC 99.
                05 DIA-RADGLO-NUM-W            PIC 99.
             03 RADICADO-EXTER-NUM-W           PIC X(15).
             03 CONTROL-X-SERV-NUM-W           PIC X.
             03 CONTROL-CL-0-NUM-W             PIC X.
             03 CONTROL-CL-1-NUM-W             PIC X.
             03 CONTROL-CL-2-NUM-W             PIC X.
             03 CONTROL-CL-3-NUM-W             PIC X.
             03 CONTROL-CL-4-NUM-W             PIC X.
             03 CONTROL-CL-5-NUM-W             PIC X.
             03 CONTROL-CL-6-NUM-W             PIC X.
             03 CONTROL-CL-7-NUM-W             PIC X.
             03 ART-IVA-NUM-W                  PIC X.
             03 LLAVE-SALID-NUM-W.
                05 PER-SALID-NUM-W             PIC 9(4).
                05 NRO-SALID-NUM-W             PIC 9(6).
             03 FILLER                       PIC X(232).
             03 LEVANT-GLOSA-NUM-W             PIC X(10).
             03 VLR-LEV-GLOSA-NUM-W            PIC S9(12)V99.
             03 FECHA-LEV-GLOSA-NUM-W.
                05 ANO-LEV-GLOSA-NUM-W         PIC 9(4).
                05 MES-LEV-GLOSA-NUM-W         PIC 99.
                05 DIA-LEV-GLOSA-NUM-W         PIC 99.
             03 HORA-FACT-ARMA-NUM-W.
                05 HR-FACT-ARMA-NUM-W          PIC 99.
                05 MN-FACT-ARMA-NUM-W          PIC 99.
             03 DOCUM-FACT-ARMA-NUM-W          PIC 9(6).
             03 FECHA-FACT-ARMA-NUM-W.
                05 ANO-FACT-ARMA-NUM-W         PIC 9(4).
                05 MES-FACT-ARMA-NUM-W         PIC 9(2).
                05 DIA-FACT-ARMA-NUM-W         PIC 9(2).
             03 DOCUM-ARMA-FACT-NUM-W          PIC 9(6).
             03 FECHA-ARMA-FACT-NUM-W.
                05 ANO-ARMA-FACT-NUM-W         PIC 9(4).
                05 MES-ARMA-FACT-NUM-W         PIC 9(2).
                05 DIA-ARMA-FACT-NUM-W         PIC 9(2).
             03 DOCUM-ARMA-RADI-NUM-W          PIC 9(6).
             03 FECHA-ARMA-RADI-NUM-W.
                05 ANO-ARMA-RADI-NUM-W         PIC 9(4).
                05 MES-ARMA-RADI-NUM-W         PIC 9(2).
                05 DIA-ARMA-RADI-NUM-W         PIC 9(2).
             03 DOCUM-RADI-ARMA-NUM-W          PIC 9(6).
             03 FECHA-RADI-ARMA-NUM-W.
                05 ANO-RADI-ARMA-NUM-W         PIC 9(4).
                05 MES-RADI-ARMA-NUM-W         PIC 9(2).
                05 DIA-RADI-ARMA-NUM-W         PIC 9(2).
             03 NRO-POL-NUM-W                  PIC X(30).
             03 FILLER                       PIC X(300).
             03 COPAGO-TIPO1-NUM-W             PIC 9(9).
             03 COPAGO-TIPO2-NUM-W             PIC 9(9).
             03 OPER-MOD-NUM-W                 PIC X(4).
             03 FUNC-AUTOR-ING-NUM-W           PIC 9(10).
             03 OBSERV-CRE-NUM-W               PIC X(50).
             03 HORA-CRE-NUM-W.
                05 HR-CRE-NUM-W                PIC X(2).
                05 MN-CRE-NUM-W                PIC X(2).
             03 TABLA-NUM-W.
                05 TABLA-FACT-NUM-W OCCURS 20.
                   07 FECHA-FACT-NUM-W.
                      09 ANO-FACT-NUM-W         PIC 9(4).
                      09 MES-FACT-NUM-W         PIC 99.
                   07 VLR-FACT-NUM-W            PIC S9(12)V99.
                05 TABLA-RBOS-NUM-W OCCURS 50.
                   06 LLAVE-ABON-NUM-W.
                      07 FECHA-ABON-NUM-W.
                         09 ANO-ABON-NUM-W      PIC 9(4).
                         09 MES-ABON-NUM-W      PIC 99.
                         09 DIA-ABON-NUM-W      PIC 99.
                      07 SECU-ABON-NUM-W.
                         09 LOTE-ABON-NUM-W.
                            11 LOTE1-ABON-NUM-W PIC X.
                            11 LOTE2-ABON-NUM-W PIC X.
                         09 NRO-ABON-NUM-W      PIC 9(5).
                         09 SEC-ABON-NUM-W      PIC XX.
                   06 VLR-ABON-NUM-W            PIC S9(12)V99.
             03 DATO-OPER-NUM-W.
                05 OPER-NUM-W                   PIC X(4).
                05 FECHA-CRE-NUM-W.
                   07 ANO-CRE-NUM-W             PIC 9(4).
                   07 MES-CRE-NUM-W             PIC 99.
		   07 DIA-CRE-NUM-W             PIC 99.
                05 FECHA-MOD-NUM-W.
                   07 ANO-MOD-NUM-W             PIC 9(4).
                   07 MES-MOD-NUM-W             PIC 99.
                   07 DIA-MOD-NUM-W             PIC 99.
                05 OPER-BLOQ-NUM-W              PIC X(4).
	  02 DATOS-NUM2-W.
             03 TABLA-NUM2-W OCCURS 150.
                05 CONDIC1-NUM2-W               PIC X(50).
                05 CONDIC2-NUM2-W               PIC X(45).
          02 FILLER                             PIC X(2000).  

       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.
          02 FILLER                    PIC X VALUE "|".
          02 DAT1-ENV                  PIC X(30).

      
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
              MOVE "SER108-2"               TO MSJ3-HTML
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
              MOVE "SER108-2"               TO MSJ3-HTML
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
              MOVE "SER108-2"               TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO LINEA-LLEGADA-W.

           INITIALIZE REG-NUM-W.

           UNSTRING LINEA-LLEGADA-W DELIMITED BY "|"
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, NOV-LLEGADA-W,
                   LLAVE-NUM-W, NIT-NUM-W, DESCRIP-NUM-W, CONVENIO-NUM-W, ESTADO-NUM-W, PORC-RETENC-NUM-W, SEG-RIPS-NUM-W         
                   CTA-PIC-NUM-W, ID-PAC-NUM-W, TIPO-FAC-NUM-W, HAB-NUM-W, PORCENT-COPAGO-EDIT, FECHA-ING-NUM-W, FECHA-RET-NUM-W             
                   HORA-ING-NUM-W, HORA-RET-NUM-W, SERVICIO-NUM-W, RED-EXTER-NUM-W, CONTRATO-NUM-W, DIVISION-NUM-W, FACT-CAPIT-NUM-W
                   FORMA-COPAG-NUM-W, C-COSTO-NUM-W, ENVIO-NUM-W, CONTROL-CAP-NUM-W, OBSERV-NUM-W, TIPO-PACI-NUM-W, DET-CARTERA-NUM-W                                                 
                   CTL-NROPACI-NUM-W, CIS-NUM-W, MYT-NUM-W, CONTROL-X-SERV-NUM-W, CONTROL-CL-0-NUM-W, CONTROL-CL-1-NUM-W,                                                                                         
                   CONTROL-CL-2-NUM-W, CONTROL-CL-3-NUM-W, CONTROL-CL-4-NUM-W, CONTROL-CL-5-NUM-W, CONTROL-CL-6-NUM-W, CONTROL-CL-7-NUM-W                  
                   ART-IVA-NUM-W, NRO-POL-NUM-W, DIV-NUM-W, DIAS-EST-NUM-W, CLASIF-NUM-W, ENTRA-REMIT-NUM-W, ORIG-REMIT-NUM-W,           
                   TIPO-EVENTO-NUM-W, CIUDAD-NUM-W, FUNC-AUTOR-ING-NUM-W, NRO-AUTORI-NUM-W, OBSERV-CRE-NUM-W, OPER-NUM-W                   
                   FECHA-CRE-NUM-W, FECHA-MOD-NUM-W, OPER-MOD-NUM-W, OPER-BLOQ-NUM-W         
           END-UNSTRING.
           MOVE PORCENT-COPAGO-EDIT TO PORCENT-COPAGO-NUM-W.
           
           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.

       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.

       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
       
           OPEN INPUT ARCHIVO-USUNET
           EVALUATE OTR-STAT
             WHEN "00"  CONTINUE
             WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
                        CLOSE       ARCHIVO-USUNET
                        OPEN I-O    ARCHIVO-USUNET
                        INITIALIZE REG-USUNET
                        MOVE "." TO NOMBRE-USUNET
                        WRITE REG-USUNET
                        CLOSE       ARCHIVO-USUNET
                        MOVE "Validacion de usuarios"      TO MSJ1-HTML
                        MOVE "Falta configurar usuario"    TO MSJ2-HTML
                        MOVE "Sc"                          TO MSJ3-HTML
                        GO TO ENVIAR2-ERROR
             WHEN OTHER GO TO CERRAR-SESION
           END-EVALUATE.
       
           INITIALIZE LLAVE-USUNET.
      
       LEER-USUARIO.
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
           CLOSE ARCHIVO-USUNET.
        
           IF NOMBRE-USUNET = SPACES
              MOVE "Validacion de usuarios"                TO MSJ1-HTML
              MOVE "Falta configurar usuario"              TO MSJ2-HTML
              MOVE "Sc"                                    TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.

           INITIALIZE NOM-NUMER-W

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           MOVE "\\" TO NOM-NUMER-W

           INSPECT NOM-NUMER-W REPLACING FIRST "                    "
                                         BY IP-DATOS-USUNET

           INSPECT NOM-NUMER-W REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-NUMER-W   REPLACING FIRST "                              "
                                           BY     DIR-CONTAB-LLEGADA-W.
           
           INSPECT NOM-NUMER-W   REPLACING FIRST "                     "
                                            BY   "\CONTROL\SC-NUMER.DAT".

       ABRIR-NUMERACION.

           OPEN INPUT ARCHIVO-NUMERACION. 

       LEER-NUMERACION.

           MOVE LLAVE-NUM-W         TO LLAVE-NUM
           READ ARCHIVO-NUMERACION WITH NO LOCK
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

           MOVE REG-NUM-W         TO REG-NUM
          
           CLOSE ARCHIVO-NUMERACION.
           OPEN I-O ARCHIVO-NUMERACION.

           WRITE REG-NUM END-WRITE

           CLOSE ARCHIVO-NUMERACION.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO GRABADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       CAMBIO.
            
           CLOSE ARCHIVO-NUMERACION.

           OPEN I-O ARCHIVO-NUMERACION.

           MOVE REG-NUM-W          TO REG-NUM
          
           REWRITE REG-NUM END-REWRITE

           CLOSE ARCHIVO-NUMERACION.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO MODIFICADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       RETIRO.
           CLOSE ARCHIVO-NUMERACION.

           OPEN I-O ARCHIVO-NUMERACION.

           DELETE ARCHIVO-NUMERACION END-DELETE

           CLOSE ARCHIVO-NUMERACION.

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

           MOVE "datosrecibidos"  TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO       TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"   USING COBW3

           MOVE "..\..\FRAMEWORKS\PAGINAS\RECIBIDOS.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.
        
       COPY "..\..\FUENTES\SC-WEB19.CBL".
      
