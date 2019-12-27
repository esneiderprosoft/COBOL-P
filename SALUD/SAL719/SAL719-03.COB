      *============================================================
      * GRABAR DATOS  SAL719-03  SALUD ANTES SER119-03
      *============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc". 
       ENVIRONMENT DIVISION.                 
       CONFIGURATION SECTION.           
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL". 
       COPY "..\..\FUENTES\FS-PROFE.CBL".
           
      *     SELECT ARCHIVO-PROFESIONALES LOCK MODE IS AUTOMATIC
      *            ASSIGN PROFESIONALES-W
      *            ORGANIZATION IS INDEXED
      *            ACCESS MODE  IS DYNAMIC
      *            RECORD KEY   IS COD-PROF
      *            ALTERNATE RECORD KEY DESCRIP-PROF WITH DUPLICATES
      *            FILE STATUS IS OTR-STAT.


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
       COPY "..\..\FUENTES\FD-PROFE.CBL".
       COPY "..\..\FUENTES\FD-TERCE.CBL".

       FD  ARCHIVO-TEXTO
           LABEL RECORD STANDARD.
       01  REG-PLANO.
           02 DATOS-PLANO            PIC X(150). 

       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".


       77 PROFESIONALES-W           PIC X(70).
       77 NOM-TER-W                 PIC X(70).
       77 NOM-PLANO-W   	    PIC X(90).
       77 CONT-W                    PIC 99.
       77 ESP-W                     PIC X(3).
       77 NOMBRE-ESP-W              PIC X(70).
       77 SUCU-INI1-PROF-W          PIC XX.
       77 SUCU-INI2-PROF-W          PIC XX.

       01 LINEA-LLEGADA-W           PIC X(1000).

       01  REG-LLEGADA-W.
           02 LLAVESESION-LLEGA-W             PIC X(29).
           02 DIR-CONTAB-LLEGADA-W            PIC X(30).
           02 MES-CONTAB-LLEGADA-W            PIC XX.
           02 NOVEDAD-LLEGAD-W                PIC X.

       01 FECHA-COMPLETA-INICIO-W.
          02 FECHA-INI-W           PIC X(8).
          02 HRA-INI-W             PIC X(4).

       01 FECHA-COMPLETA-FIN-W.
          02 FECHA-FIN-W           PIC X(8).
          02 HRA-FIN-W             PIC X(4).

            
        01 REG-PROF-W.
          02 COD-PROF-W                       PIC X(10).
          02 DESCRIP-PROF-W.
             03 INI1-PROF-W                   PIC X.
             03 INI2-PROF-W                   PIC X(29).
          02 DATOS-PROF-W.
             03 REG-MED-PROF-W                PIC X(10).
             03 TABLA-CL-EDIF-W.
                05 CL-PROF-W                  PIC X OCCURS 7.
             03 TABLA-ESP-PROF-W.
                05 ESP-PROF-W                 PIC X(3) OCCURS 10.
             03 FILLER                        PIC X.
             03 TABLA-MIN2-PROF-W.
                05 INT-MIN2-TAB-PROF-W        PIC 99 OCCURS 7.
             03 OPER-AUT-PROF-W               PIC X(4).
             03 OPER-AUT-CIRU-PROF-W          PIC X(4).
             03 OPER-AUT-OTRO-PROF-W          PIC X(4).
             03 RANGO-BLOQ-PROF-W             PIC 99.
             03 CTA-RET-PROF-W.
                05 MAY-RET-PROF-W             PIC X(4).
                05 SCTA-RET-PROF-W            PIC XX.
                05 AUX-RET-PROF-W             PIC X(5).
             03 CANTD-CITAS-PROF-W            PIC X(3).
             03 DIV2-PROF-W                   PIC XX.
             03 TABLA-TURNO-PROF-W.
                05 INT-MIN-PROF-W             PIC XX.
                05 LLAVE-TURNO-PROF-W OCCURS 7.
                   07 HORA-INI1-PROF-W.
                      09 HR-INI1-PROF-W       PIC XX.
                      09 MN-INI1-PROF-W       PIC XX.
                   07 HORA-FIN1-PROF-W.
                      09 HR-FIN1-PROF-W       PIC XX.
                      09 MN-FIN1-PROF-W       PIC XX.
                   07 HORA-INI2-PROF-W.
                      09 HR-INI2-PROF-W       PIC XX.
                      09 MN-INI2-PROF-W       PIC XX.
                   07 HORA-FIN2-PROF-W.
                      09 HR-FIN2-PROF-W       PIC XX.
                      09 MN-FIN2-PROF-W       PIC XX.
                05 TABLA-MIN-PROF-W.
                   07 INT-MIN-TABLA-PROF-W    PIC XX OCCURS 7.
             03 DESHABIL-PROF-W.
                05 FECHA-INI-DES-PROF-W.
                   07 ANO-INI-DES-PROF-W      PIC X(4).
                   07 MES-INI-DES-PROF-W      PIC XX.
                   07 DIA-INI-DES-PROF-W      PIC XX.
                05 HORA-INI-DES-PROF-W        PIC X(4).
                05 FECHA-FIN-DES-PROF-W.
                   07 ANO-FIN-DES-PROF-W      PIC X(4).
                   07 MES-FIN-DES-PROF-W      PIC XX.
                   07 DIA-FIN-DES-PROF-W      PIC XX.
                05 HORA-FIN-DES-PROF-W        PIC X(4).
             03 SOBREAGE-PROF-W               PIC X.
             03 ASOC-PROF-W                   PIC X.
             03 FORMA-AGE-PROF-W              PIC X.
             03 ESTADO-PROF-W                 PIC X.
             03 DIV-PROF-W                    PIC XX.
             03 ATIENDE-PROF-W                PIC X.
             03 CONTRATO-PROF-W               PIC X.
             03 PORCENT-PROF-W                PIC X(3).
             03 IMP-BIRAD-PROF-W              PIC X.
             03 IMP-NORM-PROF-W               PIC X.
             03 IMP-DVD-PROF-W                PIC X.
             03 OPER-AUT-4-PROF-W             PIC X(4).
             03 OPER-AUT-5-PROF-W             PIC X(4).
             03 OPERADOR-ASIGNA-PROF-W        PIC X(4).
             03 FILLER                        PIC X.
             03 DETALLE-PROF-W                PIC X(30).
             03 OPER-PROF-W                   PIC X(4).                                             
             03 FECHA-PROF-W.   
                05 ANO-PROF-W                 PIC X(4).
                05 MES-PROF-W                 PIC XX.
                05 DIA-PROF-W                 PIC XX.
             03 SUCURSAL-PROF-W.
                05 SUCU-PROF-W                PIC XX.
                05 CIUD-SUCU-PROF-W           PIC X(5).
		  02 DATOS-PROF2-W.
             03 HORA-INI-PROF2-W.
                05 HR-INI-PROF2-W             PIC XX.
                05 MN-INI-PROF2-W             PIC XX.
             03 HORA-FIN-PROF2-W.
                05 HR-FIN-PROF2-W             PIC XX.
                05 MN-FIN-PROF2-W             PIC XX.
             03 OBSERV-PROF2-W                PIC X(50).			 
             03 FILLER                        PIC X(500).
             03 OPER-MOD-PROF2-W              PIC X(4).
             03 FECHA-MOD-PROF2-W             PIC X(6).


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
              MOVE "SAL719-03"              TO MSJ3-HTML
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
              MOVE "SAL719-03"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PROFESIONALES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-PROF-LNK      TO MSJ2-HTML
              MOVE "SAL719-03-PROF"     TO MSJ3-HTML
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
              MOVE "SAL719-03-TER"      TO MSJ3-HTML
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

           INITIALIZE REG-PROF-W.

           UNSTRING LINEA-LLEGADA-W DELIMITED BY "|" 
                INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, NOVEDAD-LLEGAD-W, COD-PROF-W, 
	             REG-MED-PROF-W, CL-PROF-W (1), CL-PROF-W (2), CL-PROF-W (3), CL-PROF-W (4), CL-PROF-W(5), CL-PROF-W(6), 
                     CL-PROF-W(7), ESP-PROF-W (1), ESP-PROF-W (2), ESP-PROF-W (3), ESP-PROF-W (4), ESP-PROF-W (5), 
                     INT-MIN-PROF-W, OPER-AUT-PROF-W, OPER-AUT-CIRU-PROF-W, OPER-AUT-OTRO-PROF-W, RANGO-BLOQ-PROF-W, 
                     CTA-RET-PROF-W, CANTD-CITAS-PROF-W, DIV2-PROF-W, FECHA-COMPLETA-INICIO-W, FECHA-COMPLETA-FIN-W, 
                     SOBREAGE-PROF-W, ASOC-PROF-W, FORMA-AGE-PROF-W, ESTADO-PROF-W, DIV-PROF-W, ATIENDE-PROF-W,
                     CONTRATO-PROF-W, PORCENT-PROF-W, IMP-BIRAD-PROF-W, IMP-NORM-PROF-W, IMP-DVD-PROF-W, OPER-AUT-4-PROF-W,
                     OPER-AUT-5-PROF-W, DETALLE-PROF-W, SUCU-PROF-W                        
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
           INITIALIZE  NOM-TER-W NOM-PROF-LNK
      
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           
           MOVE "\\" TO  NOM-TER-W NOM-PROF-LNK

           INSPECT NOM-TER-W REPLACING FIRST "                    "
                                           BY IP-DATOS-USUNET
                                          
           INSPECT NOM-TER-W REPLACING FIRST " "
                                           BY "\"           
           
           INSPECT NOM-TER-W REPLACING FIRST "                              "
                                           BY DIR-CONTAB-LLEGADA-W         

           MOVE NOM-TER-W TO NOM-PROF-LNK
           INSPECT NOM-TER-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-ARCHTER.DAT".
            
           INSPECT NOM-PROF-LNK   REPLACING FIRST "                        "
                                                  BY "\CONTROL\SC-ARCHPROF.DAT".


      * ABRIR-TABLA-FECHA.
      *     MOVE "C:\PROSOFT\TEMP\DATOS-TABLA-SAL719-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.TXT"  TO NOM-PLANO-W
      *
      *     INSPECT NOM-PLANO-W REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
      *                                      BY LLAVESESION-LLEGA-W.

      *     OPEN INPUT ARCHIVO-TEXTO.
      *     INITIALIZE CONT-W TABLA-TURNO-PROF-W.

      * LEER-TABLA.
      *     READ ARCHIVO-TEXTO NEXT WITH NO LOCK AT END GO TO CERRAR-TABLA.
      *
      *     IF DATOS-PLANO = SPACES
      *        GO TO CERRAR-TABLA
      *     END-IF.
      *     ADD 1 TO CONT-W
      *
      *     IF CONT-W > 7               
      *        GO TO CERRAR-TABLA
      *     END-IF
      *
      *     UNSTRING DATOS-PLANO DELIMITED BY "|"
      *         INTO SUCU-INI1-PROF-W, HORA-INI1-PROF-W (CONT-W), HORA-FIN1-PROF-W (CONT-W), INT-MIN-TABLA-PROF-W (CONT-W),
      *              SUCU-INI2-PROF-W, HORA-INI2-PROF-W (CONT-W), HORA-FIN2-PROF-W (CONT-W), INT-MIN2-TAB-PROF-W (CONT-W)
      *     END-UNSTRING

      *     GO TO LEER-TABLA.      

      * CERRAR-TABLA.
      *     CLOSE ARCHIVO-TEXTO.


       ABRIR-PROFESIONALES.
           OPEN INPUT ARCHIVO-PROFESIONALES
                      ARCHIVO-TERCEROS. 


           MOVE COD-PROF-W TO COD-TER
           READ ARCHIVO-TERCEROS WITH NO LOCK
                 INVALID KEY 
                   MOVE SPACES TO DESCRIP1-TER
           END-READ.
           CLOSE ARCHIVO-TERCEROS.
           MOVE DESCRIP1-TER   TO DESCRIP-PROF-W.
           

       NOVEDAD.
           INITIALIZE SW-INVALID.

           MOVE COD-PROF-W TO COD-PROF
           READ ARCHIVO-PROFESIONALES WITH NO LOCK
                  INVALID KEY
                     MOVE 01 TO SW-INVALID
                  NOT INVALID KEY
                     MOVE 00  TO SW-INVALID
           END-READ.

           IF NOVEDAD-LLEGAD-W = 7 AND SW-INVALID = 01 GO TO NUEVO.
           IF NOVEDAD-LLEGAD-W = 7 AND SW-INVALID = 00 GO TO ERROR-1.
           IF NOVEDAD-LLEGAD-W = 8 AND SW-INVALID = 00 GO TO CAMBIO.
           IF NOVEDAD-LLEGAD-W = 8 AND SW-INVALID = 01 GO TO ERROR-1.
           IF NOVEDAD-LLEGAD-W = 9 AND SW-INVALID = 00 GO TO RETIRO.
           IF NOVEDAD-LLEGAD-W = 9 AND SW-INVALID = 01 GO TO ERROR-1.


       NUEVO.    
           MOVE REG-PROF-W TO REG-PROF      

           MOVE "00"                 TO MSJ1-HTML
           MOVE REG-PROF             TO MSJ2-HTML
           MOVE "SAL719-03"          TO MSJ3-HTML
           GO TO ENVIAR2-ERROR.

       OTR.
           CLOSE      ARCHIVO-PROFESIONALES
           OPEN I-O   ARCHIVO-PROFESIONALES
      
           WRITE  REG-PROF END-WRITE 
           CLOSE  ARCHIVO-PROFESIONALES.
           
           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO CREADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.


       CAMBIO.
           CLOSE      ARCHIVO-PROFESIONALES
           OPEN I-O   ARCHIVO-PROFESIONALES
      
           MOVE REG-PROF-W TO REG-PROF

           REWRITE REG-PROF END-REWRITE. 
           CLOSE      ARCHIVO-PROFESIONALES.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO MODIFICADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.


       RETIRO.
           CLOSE ARCHIVO-PROFESIONALES
           OPEN I-O ARCHIVO-PROFESIONALES
     
           DELETE ARCHIVO-PROFESIONALES END-DELETE
           CLOSE ARCHIVO-PROFESIONALES

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO BORRADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.


       ERROR-1.
           MOVE 99 TO DAT0-ENV
           MOVE "ERROR" TO DAT1-ENV.

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