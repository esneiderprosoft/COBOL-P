      *=================================================================
      *GRABAR ACTUALIZACION MAESTRO DE ARTICULOS 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
       

           SELECT ARCHIVO-TERCEROS  LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TER-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-TERCERO
                  ALTERNATE RECORD KEY DESCRIP-TER WITH DUPLICATES
                  ALTERNATE RECORD KEY ACT-TER     WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-TEXTO LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.

      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-TERCE.CBL".

       FD  ARCHIVO-TEXTO
           LABEL RECORD STANDARD.
       01  REG-PLANO.
           02 DATOS-PLANO            PIC X(500).

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".
  
       77 NOM-TER-W                            PIC X(70).
       77 NOM-PLANO-W   	               PIC X(90).
       77 CONT-W                               PIC 99.

       01 LINEA-LLEGADA-W                      PIC X(650).

       01 NOMBRE-TXT                           PIC X(50).
       
       01 FACTOR-EDIT.
          02 ENT-FACTOR-EDIT                   PIC 9(3).
          02 DEC-FACTOR-EDIT                   PIC 9(2).

       01 PORC-ICA-EDIT.
          02 ENT-PORC-ICA-EDIT                 PIC 9(2).
          02 DEC-PORC-ICA-EDIT                 PIC 9(3). 

       01 PORC-RET-EDIT. 
          02 ENT-PORC-RET-EDIT                 PIC 9(2). 
          02 DEC-PORC-RET-EDIT                 PIC 9.  
 
       01  DATO-LLEGADA-W.
           02 LLAVE-SESION-LLEGA-W.
              03 ID-LLEGAD-W                   PIC X(15).
              03 FECHA-LLEGAD-W                PIC X(8).
              03 HORA-LLEGAD-W                 PIC X(6).
           02 DIR-CONTAB-LLEGADA-W             PIC X(30).
           02 MES-CONTAB-LLEGADA-W             PIC XX.
           02 NOV-LLEGADA-W                    PIC X.

       01 REG-TERCERO-W.
          02 COD-TERCERO-W.
             03 COD-TER-W                 PIC 9(10).
          02 DESCRIP-TER-W.
             03 DESCRIP1-TER-W.
                05 INI1-TER-W             PIC X.
                05 INI2-TER-W             PIC X(29).
             03 DESCRIP2-TER-W            PIC X(20).
          02 DATOS-TERCERO-W.
             03 DIRECC-TER-W.
                05 DIRECC1-TER-W          PIC X(25).
                05 DIRECC2-TER-W          PIC X(20).
             03 DV-TER-W                  PIC X.
             03 ZONA-TER-W.
                05 ZONA1-TER-W            PIC X.
                05 ZONA2-TER-W            PIC X.
             03 RUTA-TER-W                PIC XX.
             03 ORDEN-TER-W               PIC 9(3).
             03 BLOQ-TER-W                PIC X.
             03 TIPO-ID-TER-W             PIC X(2).
             03 GRADO-TER-W               PIC X.
             03 CALIFIC-TER-W             PIC X.
             03 REG-IVA-TER-W             PIC X.
             03 RUT-TER-W                 PIC X.
             03 CONVENIO-TER-W.
                05 CONVENIO1-TER-W        PIC X.
                05 CONVENIO2-TER-W        PIC X.
             03 PLAZO-TER-W               PIC 9(3).
             03 TELEFONO-TER-W            PIC 9(12).
             03 NIT-TER-W                 PIC 9(10).
             03 ACT-TER-W                 PIC 99.
             03 VENDEDOR-TER-W            PIC X(5).
             03 ENTIDAD-TER-W             PIC X(6).
             03 RETENEDOR-TER-W           PIC X.
             03 FACTOR-TER-W              PIC 9(3)V99.
             03 COD-CIU-TER-W.
                05 DPTO-CIU-TER-W         PIC 99.
                05 CIUD-CIU-TER-W         PIC 9(3).
             03 PAGO-TER-W                PIC 99.
             03 EXENT-RET-TER-W           PIC X.
             03 RET-IVA-TER-W             PIC X.
             03 DATO-VEH-TER-W.
                05 MARCA-VEH-W             PIC X(10).
                05 EMPRESA-VEH-W           PIC X(10).
                05 NRO-VEH-W               PIC X(4).
                05 PLACA-VEH-W.
                   07 PLACA1-VEH-W         PIC XX.
                   07 PLACA2-VEH-W         PIC X(4).
                05 FECHA-ALIN-VEH-W        PIC 9(6).
                05 FECHA-CUMP-VEH-W.
                   07 ANO-NAC-TER2-W       PIC 9(4).
                   07 MES-CUMP-VEH-W       PIC 99.
                   07 DIA-CUMP-VEH-W       PIC 99.
             03 DATOS-REF-TER-W.
                05 CANT-TER-W              PIC 9(4)V99.
                05 CUPO-TER-W              PIC 9(11).
                05 REFER1-TER-W            PIC X(40).
                05 REFER2-TER-W            PIC X(40).
                05 REFER3-TER-W            PIC X(20).
                05 CARGO-TER-W             PIC X(10).
                05 SEGURO-TER-W            PIC X.
                05 DATACRE-TER-W           PIC X.
                05 FECHA-CRE-TER-W.
                   07 ANO-CRE-TER-W        PIC 9(4).
                   07 MES-CRE-TER-W        PIC 99.
                   07 DIA-CRE-TER-W        PIC 99.
          02 DESCRIP-TER2-W.
             03 APEL1-TER2-W               PIC X(20).
             03 APEL2-TER2-W               PIC X(20).
             03 NOMB1-TER2-W               PIC X(30).
          02 REF1-TER2-W.
             03 NOM-REF1-TER2-W            PIC X(30).
             03 DIR-REF1-TER2-W            PIC X(30).
             03 TEL-REF1-TER2-W            PIC X(15).
             03 REL-REF1-TER2-W            PIC X(10).
          02 REF2-TER2-W.
             03 NOM-REF2-TER2-W            PIC X(30).
             03 DIR-REF2-TER2-W            PIC X(30).
             03 TEL-REF2-TER2-W            PIC X(15).
             03 REL-REF2-TER2-W            PIC X(10).
          02 REF3-TER2-W.
             03 NOM-REF3-TER2-W            PIC X(30).
             03 DIR-REF3-TER2-W            PIC X(30).
             03 TEL-REF3-TER2-W            PIC X(15).
             03 REL-REF3-TER2-W            PIC X(10).
          02 TRAB-TER2-W.
             03 NOM-TRAB-TER2-W            PIC X(30).
             03 DIR-TRAB-TER2-W            PIC X(30).
             03 TEL-TRAB-TER2-W            PIC X(15).
             03 CAR-TRAB-TER2-W            PIC X(15).
             03 SUE-TRAB-TER2-W            PIC 9(10).
             03 ANT-TRAB-TER2-W            PIC X(10).
          02 CIU-EXP-TER2-W.
             03 DPTO-EXP-TER2-W            PIC 99.
             03 CIUD-EXP-TER2-W            PIC 9(3).
          02 ENTID-AFI-TER2-W              PIC 9(10).
          02 FECHA-AFI-TER2-W.
             03 ANO-AFI-TER2-W             PIC 9(4).
             03 MES-AFI-TER2-W             PIC 9(2).
             03 DIA-AFI-TER2-W             PIC 9(2).
          02 E-MAIL-TER2-W                 PIC X(50).
          02 GRAN-CONTRIB-TER-W            PIC X.
          02 RET-IVA-COMPRA-W              PIC X.
          02 ACTIV-ICA-W                   PIC 9(3).
          02 EMBARGOS-TER2-W.
             03 EMBARG1-TER2-W             PIC X(3).
             03 EMBARG2-TER2-W             PIC X(27).
          02 PORC-ICA-TER2-W               PIC 9(2)V999.
          02 EXTRA-CUPO-TER2-W             PIC 9(11).
          02 ACUE-PAGO-TER2-W              PIC X.
          02 REPR-TER-W.
             03 ID-REPR-LEGAL-TER-W        PIC 9(10).
             03 REPR-LEGAL-TER-W           PIC X(30).
          02 TESOR-TER-W.
             03 ID-TESOR-TER-W             PIC 9(10).
             03 TESORERO-TER-W             PIC X(30).
          02 E-MAIL-REP-TER2-W             PIC X(50).
          02 E-MAIL-TES-TER2-W             PIC X(50).
          02 CAPITADO-TER2-W               PIC X.
          02 TIPO-CUPO-TER2-W              PIC 9.
          02 INDICATIVO-TER-W              PIC X(3).
          02 ASESOR-TER-W                  PIC X(2).
          02 NIT1-CLI-TER-W                PIC X.
          02 RET-ICAV-TER-W                PIC X.
          02 MODIFICA-TER-W.
             03 ADMI-MOD-TER-W             PIC X(4).
             03 FECHA-MOD-TER-W.
                05 ANO-MOD-TER-W           PIC 9(4).
                05 MES-MOD-TER-W           PIC 99.
                05 DIA-MOD-TER-W           PIC 99.
             03 PROCESO-MOD-TER-W          PIC X(10).
             03 ADMI-CRE-TER-W             PIC X(4).
          02 REG-WEB-TER-W                 PIC 9.
          02 PORC-ESTAMP-TER-W             PIC 9(3)V99.
          02 E-MAIL-TER-W                  PIC X(60).
          02 CONTACT-TER-W.
             03 DESCRIP-CONT-TER-W         PIC X(50).
             03 TEL-CONT-TER-W             PIC X(12).
             03 E-MAIL-CONT-TER-W          PIC X(60).
          02 DATOS-HOTEL-TER-W.
             03 DATO-HOT-TER-W.
                05 PROCED-TER-W            PIC X(5).
                05 DESTINO-TER-W           PIC X(5).
                05 NACIONAL-TER-W          PIC X(5).
                05 HABIT-TER-W             PIC 9(4).
                05 PASAPORT1-TER-W         PIC X(10).
                05 PASAPORT2-TER-W         PIC X(5).
                05 FILLER                PIC X(3).
             03 DATOS-ACOMP-TER-W.
                05 TABLA-ACOMP-TER-W OCCURS 3.
                   07 NOMBRE-ACOMP-TER-W   PIC X(25).
                   07 TIPO-ID-ACOMP-TER-W  PIC 9.
                   07 EXP-ID-ACOMP-TER-W   PIC X(4).
                   07 IDENT-ACOMP-TER-W    PIC 9(10).
                   07 NACION-ACOMP-TER-W   PIC X(5).
	  02 DATOS-DIRECCIONES-TER-W.
             03 TABLA-DIRECCIONES-TER-W OCCURS 20.
                05 DIRECCION-EXT-TER-W     PIC X(60).
                05 COD-CIU-EXT-TER-W.
                   07 DPTO-CIU-EXT-TER-W   PIC 99.
                   07 CIUD-CIU-EXT-TER-W   PIC 9(3).
                05 TELEFONO-EXT-TER-W      PIC 9(12).
                05 BARRIO-TER-W            PIC 9(8).
          02 NOM-COMER-TER-W               PIC X(50).
          02 ESTADO-TER-W                  PIC 9.
          02 EXIVA-TER-W                   PIC X.
          02 ULT-FACT-TER2-W.
	    03 FECHA-UFACT-TER2-W.
                05 ANO-UFACT-TER2-W        PIC 9(4).
                05 MES-UFACT-TER2-W        PIC 9(2).
                05 DIA-UFACT-TER2-W        PIC 9(2).
	    03 SUC-UFACT-TER2-W            PIC 99.
	    03 NRO-UFACT-TER2-W            PIC 9(6).
          02 PORC-RET-TER-W                PIC 9(2)V9.
          02 VLR-BASE-RET-TER-W            PIC 9(12).
	  02 COD-VTAS-TER-W                PIC 9(4).
          02 CLASIFICACION-TER-W           PIC X.
          02 FILLER                        PIC X(2637).
                
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
              MOVE "CON110C_14"                 TO MSJ3-HTML
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
              MOVE "CON110C_14"                 TO MSJ3-HTML
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
              MOVE "CON110C_14"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TEXTO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PLANO-W              TO MSJ2-HTML
              MOVE "CON110C_14"                 TO MSJ3-HTML
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

           INITIALIZE REG-TERCERO-W.

           UNSTRING LINEA-LLEGADA-W DELIMITED BY "|"
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, NOV-LLEGADA-W,
                   COD-TERCERO-W, DV-TER-W, FECHA-CUMP-VEH-W, APEL1-TER2-W, APEL2-TER2-W, NOMB1-TER2-W,                            
                   DIRECC-TER-W, COD-CIU-TER-W, INDICATIVO-TER-W, TELEFONO-TER-W, NIT-TER-W, TIPO-ID-TER-W, ENTIDAD-TER-W,
                   ACT-TER-W, CONVENIO-TER-W, RUT-TER-W, NOM-COMER-TER-W, REFER1-TER-W, REFER2-TER-W,
                   REFER3-TER-W, CARGO-TER-W, E-MAIL-TER-W, ASESOR-TER-W, TIPO-CUPO-TER2-W, FECHA-CRE-TER-W,
                   ADMI-CRE-TER-W, FECHA-MOD-TER-W, ADMI-MOD-TER-W, FACTOR-EDIT, CUPO-TER-W, VENDEDOR-TER-W,
                   PAGO-TER-W, PLAZO-TER-W, ZONA-TER-W, RUTA-TER-W, ORDEN-TER-W, ACTIV-ICA-W,  PORC-ICA-EDIT,        
                   PORC-RET-EDIT, GRADO-TER-W, CLASIFICACION-TER-W, REG-IVA-TER-W, CALIFIC-TER-W, GRAN-CONTRIB-TER-W,    
                   RETENEDOR-TER-W, VLR-BASE-RET-TER-W, RET-IVA-COMPRA-W, RET-IVA-TER-W, EXENT-RET-TER-W,       
                   SEGURO-TER-W, DATACRE-TER-W, ACUE-PAGO-TER2-W, CAPITADO-TER2-W, NIT1-CLI-TER-W, RET-ICAV-TER-W,      
                   BLOQ-TER-W, EXIVA-TER-W, MARCA-VEH-W, EMPRESA-VEH-W, NRO-VEH-W, PLACA-VEH-W, ID-REPR-LEGAL-TER-W,       
                   REPR-LEGAL-TER-W, E-MAIL-REP-TER2-W, ID-TESOR-TER-W, TESORERO-TER-W, E-MAIL-TES-TER2-W,      
                   NOM-REF1-TER2-W, DIR-REF1-TER2-W, TEL-REF1-TER2-W, REL-REF1-TER2-W, NOM-REF2-TER2-W,           
                   DIR-REF2-TER2-W, TEL-REF2-TER2-W, REL-REF2-TER2-W, NOM-REF3-TER2-W, DIR-REF3-TER2-W,           
                   TEL-REF3-TER2-W, REL-REF3-TER2-W, NOM-TRAB-TER2-W, DIR-TRAB-TER2-W, TEL-TRAB-TER2-W,           
                   CAR-TRAB-TER2-W, SUE-TRAB-TER2-W, ANT-TRAB-TER2-W, FECHA-CUMP-VEH-W, EMBARGOS-TER2-W,
                   CIU-EXP-TER2-W, ENTID-AFI-TER2-W, FECHA-AFI-TER2-W, NOMBRE-TXT            
           END-UNSTRING.

           MOVE FACTOR-EDIT    TO FACTOR-TER-W 
           MOVE PORC-ICA-EDIT  TO PORC-ICA-TER2-W 
           MOVE PORC-RET-EDIT  TO PORC-RET-TER-W.

           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.

       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W

      *     MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.

       ABRIR-USUARIO.
           INITIALIZE OTR-STAT

           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W

      *     MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
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

           INITIALIZE NOM-TER-W 
           
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           MOVE "\\" TO NOM-TER-W 


           INSPECT NOM-TER-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET

           INSPECT NOM-TER-W REPLACING FIRST " "
                                          BY "\"

           INSPECT NOM-TER-W REPLACING FIRST "                              "
                                          BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT NOM-TER-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-ARCHTER.DAT".

       ABRIR-TABLA.
                                                   
           MOVE NOMBRE-TXT  TO NOM-PLANO-W
		
           OPEN INPUT ARCHIVO-TEXTO.

           INITIALIZE CONT-W DATOS-DIRECCIONES-TER-W.


       LEER-TABLA.
           READ ARCHIVO-TEXTO NEXT WITH NO LOCK AT END GO TO CERRAR-TABLA.

           IF DATOS-PLANO = SPACES OR LOW-VALUES
              GO TO LEER-TABLA
           ELSE
           
	      ADD 1 TO CONT-W

              IF CONT-W > 20
                 GO TO CERRAR-TABLA
              END-IF

              UNSTRING DATOS-PLANO DELIMITED BY "|"
                  INTO DIRECCION-EXT-TER-W (CONT-W), COD-CIU-EXT-TER-W (CONT-W), TELEFONO-EXT-TER-W (CONT-W), BARRIO-TER-W (CONT-W)
              END-UNSTRING
           END-IF.


           GO TO LEER-TABLA. 
        
       CERRAR-TABLA.
           CLOSE ARCHIVO-TEXTO.

       ABRIR-MAESTRO.

           OPEN INPUT ARCHIVO-TERCEROS. 
       
       LEER-MAESTRO.
           MOVE COD-TERCERO-W TO COD-TERCERO
           READ ARCHIVO-TERCEROS WITH NO LOCK
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
        
           MOVE REG-TERCERO-W TO REG-TERCERO

           CLOSE ARCHIVO-TERCEROS.
           OPEN I-O ARCHIVO-TERCEROS.

           WRITE REG-TERCERO END-WRITE
           CLOSE ARCHIVO-TERCEROS.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO GRABADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       CAMBIO.
            
           CLOSE ARCHIVO-TERCEROS.

           OPEN I-O ARCHIVO-TERCEROS.

           MOVE REG-TERCERO-W TO REG-TERCERO 

           REWRITE REG-TERCERO END-REWRITE
           CLOSE ARCHIVO-TERCEROS.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO MODIFICADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       RETIRO.
           CLOSE ARCHIVO-TERCEROS.

           OPEN I-O ARCHIVO-TERCEROS.

           DELETE ARCHIVO-TERCEROS END-DELETE

           CLOSE ARCHIVO-TERCEROS.

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