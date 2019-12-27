      *============================================================
      * GRABAR DATOS  SAL718-G  SALUD
      *============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.                 
       CONFIGURATION SECTION.           
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL". 

             SELECT ARCHIVO-CUPS LOCK MODE IS AUTOMATIC
                    ASSIGN ARCHCUPS-W
                    ORGANIZATION IS INDEXED;
                    ACCESS MODE DYNAMIC;
                    RECORD KEY  IS LLAVE-CUP
                    ALTERNATE RECORD KEY IS DESCRIP-CUP WITH DUPLICATES
                    ALTERNATE RECORD KEY IS LLAVE-ALT-CUP WITH DUPLICATES
                    FILE STATUS IS OTR-STAT.


       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-CUPS19.CBL".


       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 ARCHCUPS-W                    PIC X(60).
 

       01 LLEGADA-W                     PIC X(200).
       01 FECHA-W                       PIC X(8).
       01 OPER-W                        PIC X(4).

       77 MES-SAL-W                     PIC X(4).

       01 REG-LLEGADA-W.
          02 LLAVESESION-LLEGA-W       PIC X(29).
          02 DIR-CONTAB-LLEGADA-W      PIC X(30).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 NOVEDAD-LLEGADA-W         PIC X.



       01  REG-CUPS-W.
           02 LLAVE-CUP-W.
              03 GRP-CUP-W.
                 05 GRP1-CUP-W         PIC X.
                 05 GRP2-CUP-W         PIC X.
              03 NRO-CUP-W.
                 05 NRO1-CUP-W         PIC XX.
                 05 NRO2-CUP-W         PIC XX.
                 05 NRO3-CUP-W         PIC X(6).
           02 DATOS-CUP-W.
              03 DESCRIP-CUP-W.
                 05 DESCRIP1-CUP-W.
                    07 OCULTAR-CUP-W   PIC X.
                    07 FILLER          PIC X(29).
                 05 DESCRIP2-CUP-W     PIC X(50).
              03 LLAVE-ALT-CUP-W.
                 05 TIPO-CUP-W         PIC 9.
                 05 ABREV-CUP-W        PIC X(5).
              03 CTA-CONTAB-CUP-W OCCURS 3.
                 05 MAY-CUP-W          PIC X(4).
                 05 SCTA-CUP-W         PIC X(2).
                 05 AUXIL-CUP-W        PIC X(5).
              03 DIV2-CUP-W            PIC X(2).
              03 COPAG-MOD-CUP-W       PIC X.
              03 NOPOS-CUP-W           PIC X.
              03 DURACION-CUP-W        PIC 9(3).
              03 NIVEL-CUP-W           PIC X.
              03 DIVISION-CUP-W        PIC XX.
              03 NIT-OTR-CUP-W         PIC 9(10).
              03 100-MED-CUP-W         PIC X.
              03 CTA-OTR-CUP-W         PIC X(11).
              03 DIAGN-CUP-W           PIC X.
              03 SEXO-CUP-W            PIC X.
              03 EDAD-MIN-CUP-W        PIC 9(3).
              03 EDAD-MAX-CUP-W        PIC 9(3).
              03 UNID-EDAD-CUP-W       PIC X.
              03 PORC-CL-CUP-W         PIC 9(3).
              03 COSTO-CUP-W           PIC X(4).
              03 CIS-CUP-W             PIC X.
              03 PORC-OTR-CUP-W        PIC 99V99.
           02 FILLER                   PIC X(2000).
           02 OPER-ELAB-CUP-W          PIC X(4).
           02 FECHA-ELAB-CUP-W.
              03 ANO-ELAB-CUP-W        PIC 9(4).
              03 MES-ELAB-CUP-W        PIC 99.
              03 DIA-ELAB-CUP-W        PIC 99.
           02 HORA-ELAB-CUP-W          PIC X(4).
           02 OPER-MOD-CUP-W           PIC X(4).
           02 FECHA-MOD-CUP-W.
              03 ANO-MOD-CUP-W         PIC 9(4).
              03 MES-MOD-CUP-W         PIC 99.
              03 DIA-MOD-CUP-W         PIC 99.
           02 HORA-MOD-CUP-W           PIC X(4).
           02 FILLER                   PIC X(1000).
					  

       01 DATOS-ENVIO.
          02 DAT0-ENV                         PIC XX.
          02 FILLER                           PIC X VALUE "|".
          02 DAT1-ENV                         PIC X(30).


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
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-USU-W                 TO MSJ2-HTML
              MOVE "SAL718-G"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-SESION-W              TO MSJ2-HTML
              MOVE "SAL718-G"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.


       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CUPS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE ARCHCUPS-W                 TO MSJ2-HTML
              MOVE "SAL718-G-CUPS"            TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO LLEGADA-W.
           
           INITIALIZE REG-CUPS-W

           UNSTRING LLEGADA-W DELIMITED BY "|"
               INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, NOVEDAD-LLEGADA-W, LLAVE-CUP-W,
                    DESCRIP-CUP-W, TIPO-CUP-W, ABREV-CUP-W, NIVEL-CUP-W, COPAG-MOD-CUP-W, NOPOS-CUP-W, CIS-CUP-W, COSTO-CUP-W,
                    EDAD-MIN-CUP-W, EDAD-MAX-CUP-W, UNID-EDAD-CUP-W, SEXO-CUP-W, DIAGN-CUP-W, 100-MED-CUP-W, PORC-CL-CUP-W,
                    PORC-OTR-CUP-W, CTA-OTR-CUP-W, NIT-OTR-CUP-W, CTA-CONTAB-CUP-W (1), CTA-CONTAB-CUP-W (2), CTA-CONTAB-CUP-W (3),
                    DIVISION-CUP-W, DIV2-CUP-W 
           END-UNSTRING.


           IF TIPO-CUP-W = 6
              MOVE "41250400050" TO CTA-CONTAB-CUP-W (1)
              MOVE "43124800001" TO CTA-CONTAB-CUP-W (2)
           END-IF


           MOVE LLAVESESION-LLEGA-W    TO LLAVE-SESION-W.
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
              MOVE "Validacion de usuarios"      TO MSJ1-HTML
              MOVE "Falta configurar usuario"    TO MSJ2-HTML
              MOVE "Sc"                          TO MSJ3-HTML
              GO TO ENVIAR-ERROR
           END-IF.

 
       ASIGNAR-NOMBRES.
           INITIALIZE ARCHCUPS-W
		   
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".
		   
           MOVE "\\" TO ARCHCUPS-W
		   
           INSPECT ARCHCUPS-W REPLACING FIRST "                    "
                                           BY IP-DATOS-USUNET

           INSPECT ARCHCUPS-W REPLACING FIRST " "
                                           BY "\"

           INSPECT ARCHCUPS-W REPLACING FIRST "                          "
                                           BY "PROG\DATOS\SC-ARCHCUPS.DAT".

         
       ABRIR-ARCHIVOS.
           OPEN INPUT ARCHIVO-CUPS.
                     

       NOVEDAD.
           INITIALIZE SW-INVALID 

           MOVE LLAVE-CUP-W TO LLAVE-CUP
           READ ARCHIVO-CUPS WITH NO LOCK
                  INVALID KEY
                     MOVE 01 TO SW-INVALID
           END-READ.


           IF NOVEDAD-LLEGADA-W = 7 AND SW-INVALID = 01 GO TO NUEVO.
           IF NOVEDAD-LLEGADA-W = 7 AND SW-INVALID = 00 GO TO ERROR-1.
           IF NOVEDAD-LLEGADA-W = 8 AND SW-INVALID = 00 GO TO CAMBIO.
           IF NOVEDAD-LLEGADA-W = 8 AND SW-INVALID = 01 GO TO ERROR-1.
           IF NOVEDAD-LLEGADA-W = 9 AND SW-INVALID = 00 GO TO RETIRO.
           IF NOVEDAD-LLEGADA-W = 9 AND SW-INVALID = 01 GO TO ERROR-1.


        NUEVO.    
           MOVE REG-CUPS-W TO REG-CUPS
      
           CLOSE      ARCHIVO-CUPS
           OPEN I-O   ARCHIVO-CUPS

           WRITE REG-CUPS END-WRITE       
           CLOSE      ARCHIVO-CUPS.
           
           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO CREADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       CAMBIO.
           CLOSE      ARCHIVO-CUPS
           OPEN I-O   ARCHIVO-CUPS

           MOVE REG-CUPS-W  TO REG-CUPS

           REWRITE REG-CUPS END-REWRITE. 
           CLOSE   ARCHIVO-CUPS.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO MODIFICADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       RETIRO.
           CLOSE      ARCHIVO-CUPS
           OPEN I-O   ARCHIVO-CUPS

           DELETE     ARCHIVO-CUPS END-DELETE
           CLOSE      ARCHIVO-CUPS.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO ELIMINADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       ERROR-1.
           MOVE 99 TO DAT0-ENV
           MOVE SW-INVALID TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       ENVIO-DATOS.
           MOVE "datosrecibidos"  TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO       TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"   USING COBW3
           MOVE "../PAGINAS/RECIBIDOS.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML.

       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.

       COPY "..\..\FUENTES\SC-WEB.CBL".
