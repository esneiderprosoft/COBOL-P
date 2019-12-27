      *=================================================================
      *CREACION DE ARCHIVO JSON CON801 F8 DE CENTROS DE COSTO
      *=================================================================  
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".  
                                                                                      
           SELECT ARCHIVO-MAESTROS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-MAE-LNK
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-MAE
                  ALTERNATE RECORD KEY IS NOMBRE-MAE    WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CTA-INTER-MAE WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.  
                                          
      
       DATA DIVISION.
       FILE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-MAEST.CBL".

      
       WORKING-STORAGE  SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC19.CBL".

       77 NOM-PLANO-W                   PIC X(90).
      
       77 MES-SAL-W                     PIC X(4).
       77 COMPARA-LLAVE-MAE             PIC X(12).

       77 ESPEJO-PORCENT                PIC ZZ,ZZZ.

       77 DATOS-PLANO-W                 PIC X(200).
        
       01 LLEGADA-W                     PIC X(100).

       01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 DIR-CONTAB-LLEGADA-W      PIC X(30).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 OPERADOR-LLEGADA-W        PIC X(4).
          

       01 LIN-1.            
          02 FILLER                  PIC X(11)  VALUE "{*CTA_MAY*:".
          02 FILLER                  PIC X      VALUE "*".
          02 MAY-MAE-J  	         PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*SUB_CTA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SUB-CTA-J  	         PIC X(2).    
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*AUX_MAE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 AUX-MAE-J  	         PIC X(5).                   
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*TIPO_MAE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-MAE-J              PIC 9.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*NOMBRE_MAE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOMBRE-MAE-J            PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*PORCENT_RET*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PORCENT-RET-MAE-J       PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*FUENTE_FIN*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FTE-FIN-MAE-J           PIC X(4).
          02 FILLER                  PIC XX     VALUE "*}".    
          02 CIERRE-LIN-1            PIC X.

        
       LINKAGE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\ISAPICTX.CBL".
      
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
              MOVE "CON801"                 TO MSJ3-HTML
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
              MOVE "CON801"                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-MAESTROS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE NOM-MAE-LNK                TO MSJ2-HTML
              MOVE "CON801"                   TO MSJ3-HTML
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

           UNSTRING LLEGADA-W DELIMITED BY "|"
              INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W
           END-UNSTRING. 

           MOVE LLAVESESION-LLEGA-W TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-JSON-W.
           ACCEPT HORA-JSON-W FROM TIME.
           MOVE OPERADOR-LLEGADA-W TO OPER-JSON-W.

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
           INITIALIZE NOM-MAE-LNK
            
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"  
           MOVE "\\" TO NOM-MAE-LNK

           INSPECT NOM-MAE-LNK REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET

           INSPECT NOM-MAE-LNK REPLACING FIRST " "
                                            BY "\"

 
           INSPECT NOM-MAE-LNK   REPLACING FIRST "                              "  
                                              BY DIR-CONTAB-LLEGADA-W.

           
           INSPECT NOM-MAE-LNK   REPLACING FIRST "                       "
                                            BY   "\CONTROL\SC-ARCHMAE.DAT".


       ABRIR-NOMBRE-MAE.
           OPEN INPUT ARCHIVO-MAESTROS.

           EVALUATE OTR-STAT
             WHEN "00" CONTINUE
             WHEN "35" OPEN OUTPUT ARCHIVO-MAESTROS
                       CLOSE       ARCHIVO-MAESTROS
                       GO TO ABRIR-NOMBRE-MAE
           END-EVALUATE.

 
       ABRIR-JSON.
           INITIALIZE REGISTRO-MAE.
           MOVE "{*MAESTROS*:[" TO  DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM ENVIO-DATOS
           MOVE 0 TO SW-FIN.

       LEER-NOMBRE-MAE.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-PLANO-W
              PERFORM ENVIO-DATOS
           END-IF

           READ ARCHIVO-MAESTROS NEXT WITH NO LOCK AT END 
                GO TO CERRAR-NOMBRE-MAE
           END-READ.

           IF LLAVE-MAE = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE
              IF LLAVE-MAE = COMPARA-LLAVE-MAE
                 MOVE 0 TO SW-FIN
              ELSE
                 INITIALIZE LIN-1
                 MOVE LLAVE-MAE     TO COMPARA-LLAVE-MAE
                 MOVE MAYOR-MAE     TO MAY-MAE-J
                 MOVE SUBCTA-MAE    TO SUB-CTA-J
                 MOVE AUXILIAR-MAE  TO AUX-MAE-J
                 MOVE TIPO-MAE      TO TIPO-MAE-J
                 MOVE NOMBRE-MAE    TO NOMBRE-MAE-J
                 MOVE PORCENT-RET-MAE TO ESPEJO-PORCENT
                 MOVE ESPEJO-PORCENT TO PORCENT-RET-MAE-J
                 MOVE FTE-FIN-MAE TO FTE-FIN-MAE-J

                 INSPECT NOMBRE-MAE-J REPLACING ALL '"' BY " "
                 INSPECT NOMBRE-MAE-J REPLACING ALL "*" BY " "
                 INSPECT NOMBRE-MAE-J REPLACING ALL "Ñ" BY CARAC-ENE-MAY 
                 INSPECT NOMBRE-MAE-J REPLACING ALL "ñ" BY CARAC-ENE-MIN

                 INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
                 MOVE "," TO CIERRE-LIN-1          
              END-IF
           END-IF


           GO TO LEER-NOMBRE-MAE.

       CERRAR-NOMBRE-MAE.
           INITIALIZE LIN-1
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM ENVIO-DATOS

           MOVE "]}" TO DATOS-PLANO-W
           PERFORM ENVIO-DATOS

                
           CLOSE ARCHIVO-MAESTROS.    

       PAGINA-CONFIG.  
           MOVE "statuscode"     TO COBW3-CNV-NAME
           MOVE "00"             TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"  USING COBW3

           MOVE "programa-id"    TO COBW3-CNV-NAME
           MOVE "SAL711"         TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"  USING COBW3

           MOVE "..\PAGINAS\RECIBIDOSLISTADO.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML.


       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.


       ENVIO-DATOS.
010720     MOVE "datosrecibidos"   TO COBW3-CNV-NAME
010730     MOVE DATOS-PLANO-W      TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_REPEAT" USING COBW3. 
      
      
       COPY "P:\PROG\PROYECT\FUENTES\SC-WEB19.CBL".          
