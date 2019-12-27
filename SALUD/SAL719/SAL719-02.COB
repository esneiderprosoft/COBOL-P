      *============================================================
      * GUARDAR DATOS -ARCHIVO-HORARIO-PROF- SER119-02  SALUD
      *============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.             
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  



      * "\PROG\HC\DATOS\SC-HORAR.DAT"
           SELECT ARCHIVO-HORARIO-PROF LOCK MODE IS AUTOMATIC
                  ASSIGN HORARI-PROF-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE IS DYNAMIC RECORD KEY IS LLAVE-HOR-P
                  ALTERNATE RECORD KEY FECHA-HOR-P WITH DUPLICATES 
                  FILE STATUS IS OTR-STAT.
                            
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-HORAR.CBL".

       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 HORARI-PROF-W             PIC X(60).
       77 MES-SAL-W                 PIC X(4).
       01 TABLA-LLEGADA             PIC X(250).

       01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 DIR-CONTAB-LLEGADA-W      PIC X(8).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 NOVED-LLEGADA-W           PIC X.
           02 COD-HRP-W                 PIC X(10).
           02 FECHA-HRP-W.
              03 ANO-HRP-W              PIC XX.
              03 MES-HRP-W              PIC XX.
              03 DIA-HRP-W              PIC XX.
           02 OBSERV-HORA-HRP-W         PIC X(50).
      *     02 SUCUR-UNO-HRP-W          PIC XX.
           02 HORA-INIC1-HRP-W.
              03 HR-INIC1-HRP-W         PIC XX. 
              03 MN-INIC1-HRP-W         PIC XX.
           02 HORA-SALD1-HRP-W.
              03 HR-SALD1-HRP-W         PIC XX. 
              03 MN-SALD1-HRP-W         PIC XX.
           02 FRECU-UNO-HRP-W           PIC XX.
      *     02 SUCUR-DOS-HRP-W          PIC XX.
           02 HORA-INIC2-HRP-W.
              03 HR-INIC2-HRP-W         PIC XX. 
              03 MN-INIC2-HRP-W         PIC XX.
           02 HORA-SALD2-HRP-W.
              03 HR-SALD2-HRP-W         PIC XX. 
              03 MN-SALD2-HRP-W         PIC XX.
           02 FRECU-DOS-HRP-W           PIC XX.
      *     02 SUCUR-TRES-HRP-W          PIC XX.
      *     02 HORA-INIC3-HRP-W.
      *        03 HR-INIC3-HRP-W         PIC XX. 
      *        03 MN-INIC3-HRP-W         PIC XX.
      *     02 HORA-SALD3-HRP-W.
      *        03 HR-SALD3-HRP-W         PIC XX. 
      *        03 MN-SALD3-HRP-W         PIC XX.
      *     02 FRECU-TRES-HRP-W          PIC XX.
      *     02 SUCUR-CUAT-HRP-W          PIC XX.
      *     02 HORA-INIC4-HRP-W.
      *        03 HR-INIC4-HRP-W         PIC XX. 
      *        03 MN-INIC4-HRP-W         PIC XX.
      *     02 HORA-SALD4-HRP-W.
      *        03 HR-SALD4-HRP-W         PIC XX. 
      *        03 MN-SALD4-HRP-W         PIC XX.
      *     02 FRECU-CUAT-HRP-W          PIC XX.
                


       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.
          02 FILLER                    PIC X VALUE "|".
          02 DAT1-ENV                  PIC X(25).
          02 FILLER                    PIC X VALUE "|".


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
              MOVE "SER119-02"              TO MSJ3-HTML
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
              MOVE "SER119-02"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-HORARIO-PROF.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "SER119-02"              TO MSJ2-HTML
              MOVE HORARI-PROF-W            TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO TABLA-LLEGADA

           UNSTRING TABLA-LLEGADA DELIMITED BY "|" 
             INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, NOVED-LLEGADA-W,
                  OBSERV-HORA-HRP-W, HORA-INIC1-HRP-W, HORA-SALD1-HRP-W, FRECU-UNO-HRP-W,
                  HORA-INIC2-HRP-W, HORA-SALD2-HRP-W, FRECU-DOS-HRP-W
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
              GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.
           INITIALIZE HORARI-PROF-W.

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           MOVE "\\" TO HORARI-PROF-W 

           INSPECT HORARI-PROF-W REPLACING FIRST "                    "
                                              BY IP-DATOS-USUNET

           INSPECT HORARI-PROF-W REPLACING FIRST " "
                                              BY "\"

           INSPECT HORARI-PROF-W REPLACING FIRST "        "
                                            BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT HORARI-PROF-W REPLACING FIRST "                     "
                                              BY "\CONTROL\SC-HORAR.DAT".


       ABRIR-HORARIO-PROF.
           OPEN INPUT ARCHIVO-HORARIO-PROF. 

       LEER-HORARIO.

           MOVE COD-HRP-W TO COD-HOR-P

           READ ARCHIVO-HORARIO-PROF WITH NO LOCK
                INVALID KEY 
                  MOVE 1 TO SW-INVALID
           END-READ.


           IF NOVED-LLEGADA-W = 7 AND SW-INVALID = 1 GO TO NUEVO.
           IF NOVED-LLEGADA-W = 7 AND SW-INVALID = 0 GO TO ERROR-1.
           IF NOVED-LLEGADA-W = 8 AND SW-INVALID = 0 GO TO CAMBIO.
           IF NOVED-LLEGADA-W = 8 AND SW-INVALID = 1 GO TO ERROR-1.
           IF NOVED-LLEGADA-W = 9 AND SW-INVALID = 0 GO TO RETIRO.
           IF NOVED-LLEGADA-W = 9 AND SW-INVALID = 1 GO TO ERROR-1.


       NUEVO.
           INITIALIZE REG-HOR-P.

           MOVE COD-HRP-W          TO COD-HOR-P   
           MOVE FECHA-HRP-W        TO FECHA-HOR-P     
           MOVE OBSERV-HORA-HRP-W  TO OBSERV-HOR-P
      *    MOVE SUCUR-UNO-HRP-W    TO SUCUR-UNO-HOR-P    
           MOVE HORA-INIC1-HRP-W   TO HORA-INI1-HOR-P 
           MOVE HORA-SALD1-HRP-W   TO HORA-FIN1-HOR-P
           MOVE FRECU-UNO-HRP-W    TO INT-HOR1-P
           MOVE HORA-INIC2-HRP-W   TO HORA-INI2-HOR-P
           MOVE HORA-SALD2-HRP-W   TO HORA-FIN2-HOR-P
           MOVE FRECU-DOS-HRP-W    TO INT-HOR2-P 
      *    MOVE SUCUR-DOS-HRP-W    TO SUCUR-DOS-HOR-P

           CLOSE ARCHIVO-HORARIO-PROF. 

           WRITE REG-HOR-P END-WRITE
           CLOSE ARCHIVO-HORARIO-PROF.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO GRABADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.           

       
       CAMBIO.
           CLOSE ARCHIVO-HORARIO-PROF.
           OPEN I-O ARCHIVO-HORARIO-PROF.

           MOVE COD-HRP-W          TO COD-HOR-P   
           MOVE FECHA-HRP-W        TO FECHA-HOR-P     
           MOVE OBSERV-HORA-HRP-W  TO OBSERV-HOR-P
      *    MOVE SUCUR-UNO-HRP-W    TO SUCUR-UNO-HOR-P    
           MOVE HORA-INIC1-HRP-W   TO HORA-INI1-HOR-P 
           MOVE HORA-SALD1-HRP-W   TO HORA-FIN1-HOR-P
           MOVE FRECU-UNO-HRP-W    TO INT-HOR1-P
           MOVE HORA-INIC2-HRP-W   TO HORA-INI2-HOR-P
           MOVE HORA-SALD2-HRP-W   TO HORA-FIN2-HOR-P
           MOVE FRECU-DOS-HRP-W    TO INT-HOR2-P 
      *    MOVE SUCUR-DOS-HRP-W    TO SUCUR-DOS-HOR-P    

           REWRITE REG-HOR-P END-REWRITE
           CLOSE ARCHIVO-HORARIO-PROF.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO MODIFICADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.           

       RETIRO.
           CLOSE ARCHIVO-HORARIO-PROF.
           OPEN I-O ARCHIVO-HORARIO-PROF.

           DELETE ARCHIVO-HORARIO-PROF END-DELETE
           CLOSE  ARCHIVO-HORARIO-PROF.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO BORRADO" TO DAT1-ENV

           GO TO ENVIO-DATOS.


       ERROR-1.
           MOVE 99      TO DAT0-ENV
           MOVE "ERROR" TO DAT1-ENV.

           GO TO ENVIO-DATOS.


       ENVIO-DATOS.
           MOVE "datosrecibidos"  TO COBW3-CNV-NAME
           MOVE 00                TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"   USING COBW3

           MOVE "..\..\FRAMEWORKS\PAGINAS\RECIBIDOS.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.

       COPY "..\..\FUENTES\SC-WEB19.CBL".



