      *============================================================
      * GRABAR DATOS  SAL714-02  SALUD
      *============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.                 
       CONFIGURATION SECTION.           
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  
           
           SELECT ARCHIVO-SISVAN LOCK MODE IS AUTOMATIC
                  ASSIGN SISVAN-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-SIS
                  ALTERNATE RECORD KEY FICHA-SIS     WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-REG-SIS WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.
                                                          
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-SISVA.CBL".


       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".
       
       77 SISVAN-W                  PIC X(70).
	   
       77 CONT-W                    PIC 99.
       
       77 ESP-W                     PIC X(3).
       77 NOMBRE-ESP-W              PIC X(70).
 
       77 MES-SAL-W                 PIC X(4).

       77 NOMBRE-W                  PIC X(50).


       01 LINEA-LLEGADA-W           PIC X(200).

       01  REG-LLEGADA-W.
           02 LLAVESESION-LLEGA-W             PIC X(29).
           02 DIR-CONTAB-LLEGADA-W            PIC X(8).
           02 MES-CONTAB-LLEGADA-W            PIC XX.
           02 OPERADOR-LLEGADA-W              PIC X(4).
           02 NOVEDAD-LLEGAD-W                PIC X.
		   
       01 REG-SISVAN-W.
          02 COD-SIS-W.
             03 IDENT-SIS-W                   PIC X(15).
          02 DATOS-SIS-W.
             03 FICHA-SIS-W                   PIC X(6).
             03 FECHA-REG-SIS-W.
                05 ANO-REG-SIS-W              PIC 9(4).
                05 MES-REG-SIS-W              PIC 99.
                05 DIA-REG-SIS-W              PIC 99.
             03 CENT-SAL-SIS-W                PIC X(20).
             03 BARRIO-SIS-W                  PIC X(20).
             03 PROG-NUT-SIS-W                PIC XX.
             03 LACT-ACTUAL-SIS-W             PIC X.
             03 LACT-EXCLUS-SIS-W             PIC X.
             03 MESES-LACT-EXCLUS-SIS-W       PIC X(2).
             03 DATOS-OPER-SIS-W.
                05 OPER-CRE-SIS-W             PIC X(4).
                05 FECH-CRE-SIS-W             PIC X(6).
                05 OPER-MOD-SIS-W             PIC X(4).
                05 FECH-MOD-SIS-W             PIC X(6).
          02 FILLER                           PIC X(5000).
		  

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
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USU-W                TO MSJ2-HTML
              MOVE "SAL714-02"              TO MSJ3-HTML
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
              MOVE "SAL714-02"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SISVAN.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE SISVAN-W             TO MSJ2-HTML
              MOVE "SAL714-02"          TO MSJ3-HTML
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

           INITIALIZE REG-SISVAN-W

           UNSTRING LINEA-LLEGADA-W DELIMITED BY "|" 
                INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W, NOVEDAD-LLEGAD-W, COD-SIS-W, 
                     FICHA-SIS-W, FECHA-REG-SIS-W, CENT-SAL-SIS-W, BARRIO-SIS-W, PROG-NUT-SIS-W, LACT-ACTUAL-SIS-W, LACT-EXCLUS-SIS-W, 
                     MESES-LACT-EXCLUS-SIS-W, OPER-CRE-SIS-W, FECH-CRE-SIS-W, OPER-MOD-SIS-W, FECH-MOD-SIS-W 
           END-UNSTRING.


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
           INITIALIZE SISVAN-W 

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           
           MOVE "\\" TO  SISVAN-W

           INSPECT SISVAN-W REPLACING FIRST "                    "
                                           BY IP-DATOS-USUNET 
                                           
           INSPECT SISVAN-W REPLACING FIRST "                            "
                                         BY "\PROG\HC\DATOS\SC-SISVAN.DAT".

	   
       ABRIR-SISVAN.
           OPEN INPUT ARCHIVO-SISVAN.
                     

       NOVEDAD.
      *     INITIALIZE SW-INVALID
           MOVE 00 TO SW-INVALID
           MOVE COD-SIS-W TO COD-SIS
           READ ARCHIVO-SISVAN WITH NO LOCK
                 INVALID KEY
                     MOVE 01  TO SW-INVALID
           END-READ

           IF NOVEDAD-LLEGAD-W = 7 AND SW-INVALID = 01 GO TO NUEVO.
           IF NOVEDAD-LLEGAD-W = 7 AND SW-INVALID = 00 GO TO ERROR-1.
           IF NOVEDAD-LLEGAD-W = 8 AND SW-INVALID = 00 GO TO CAMBIO.
           IF NOVEDAD-LLEGAD-W = 8 AND SW-INVALID = 01 GO TO ERROR-1.
           IF NOVEDAD-LLEGAD-W = 9 AND SW-INVALID = 00 GO TO RETIRO.
           IF NOVEDAD-LLEGAD-W = 9 AND SW-INVALID = 01 GO TO ERROR-1.

       NUEVO.		
      *     MOVE REG-SISVAN-W TO REG-SISVAN

            MOVE COD-SIS-W                  TO COD-SIS
            MOVE FICHA-SIS-W                TO FICHA-SIS
            MOVE FECHA-REG-SIS-W            TO FECHA-REG-SIS
            MOVE CENT-SAL-SIS-W             TO CENT-SAL-SIS
            MOVE BARRIO-SIS-W               TO BARRIO-SIS
            MOVE PROG-NUT-SIS-W             TO PROG-NUT-SIS
            MOVE LACT-ACTUAL-SIS-W          TO LACT-ACTUAL-SIS
            MOVE LACT-EXCLUS-SIS-W          TO LACT-EXCLUS-SIS
            MOVE MESES-LACT-EXCLUS-SIS-W    TO MESES-LACT-EXCLUS-SIS
            MOVE OPER-CRE-SIS-W             TO OPER-CRE-SIS
            MOVE FECH-CRE-SIS-W             TO FECH-CRE-SIS
            MOVE OPER-MOD-SIS-W             TO OPER-MOD-SIS
            MOVE FECH-MOD-SIS-W             TO FECH-MOD-SIS


           CLOSE     ARCHIVO-SISVAN 
           OPEN I-O  ARCHIVO-SISVAN

           WRITE REG-SISVAN END-WRITE. 
           CLOSE      ARCHIVO-SISVAN.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO CREADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.
                      
       CAMBIO.
           CLOSE      ARCHIVO-SISVAN
           OPEN I-O   ARCHIVO-SISVAN

      *     MOVE REG-SISVAN-W TO REG-SISVAN

            MOVE COD-SIS-W                  TO COD-SIS
            MOVE FICHA-SIS-W                TO FICHA-SIS
            MOVE FECHA-REG-SIS-W            TO FECHA-REG-SIS
            MOVE CENT-SAL-SIS-W             TO CENT-SAL-SIS
            MOVE BARRIO-SIS-W               TO BARRIO-SIS
            MOVE PROG-NUT-SIS-W             TO PROG-NUT-SIS
            MOVE LACT-ACTUAL-SIS-W          TO LACT-ACTUAL-SIS
            MOVE LACT-EXCLUS-SIS-W          TO LACT-EXCLUS-SIS
            MOVE MESES-LACT-EXCLUS-SIS-W    TO MESES-LACT-EXCLUS-SIS
            MOVE OPER-CRE-SIS-W             TO OPER-CRE-SIS
            MOVE FECH-CRE-SIS-W             TO FECH-CRE-SIS
            MOVE OPER-MOD-SIS-W             TO OPER-MOD-SIS
            MOVE FECH-MOD-SIS-W             TO FECH-MOD-SIS


           REWRITE REG-SISVAN END-REWRITE.
           CLOSE      ARCHIVO-SISVAN.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO MODIFICADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       RETIRO.
           CLOSE      ARCHIVO-SISVAN
           OPEN I-O   ARCHIVO-SISVAN

           DELETE     ARCHIVO-SISVAN END-DELETE.
           CLOSE      ARCHIVO-SISVAN.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO ELIMINADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       ERROR-1.
           MOVE 99 TO DAT0-ENV
           MOVE "ERROR" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       ENVIO-DATOS.
           MOVE 00 TO DAT0-ENV
           MOVE "CORRECTO" TO DAT1-ENV


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