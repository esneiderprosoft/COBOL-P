      *=================================================================
      *CREACION DE ARCHIVO JSON INV809-01  SALUD
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
       

           SELECT ARCHIVO-DIVISION
                  ASSIGN NOM-DIV-LNK
                  ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS LLAVE-DIV
                  ALTERNATE RECORD KEY IS DESCRIP-DIV WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-JSON
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT. 
                                          
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-DIVIS.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-PLANO.
           02 DATOS-PLANO     PIC X(500).

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".



       77 NOM-PLANO-W               PIC X(90).
       77 NOM-DIV-LNK               PIC X(60).
       77 MES-SAL-W                 PIC X(4).

       01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 IP-SERVER-W               PIC X(13).
           02 DIR-CONTAB-LLEGADA-W      PIC X(6).
           02 MES-CONTAB-LLEGADA-W      PIC XX.

       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.

       01 LIN-1.  
          02 FILLER                  PIC X(7)   VALUE "{*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-DIV-J               PIC X(2).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)   VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-DIV-J           PIC X(20).
          02 FILLER                  PIC XX     VALUE "*}".    
          02 CIERRE-LIN-1            PIC X.
        
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
              MOVE "INV809"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-DIVISION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE                    
           ELSE
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-DIV-LNK               TO MSJ2-HTML
              MOVE "INV809"                  TO MSJ3-HTML
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
              MOVE "INV809"                 TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO DATO-LLEGADA-W.

           MOVE LLAVESESION-LLEGA-W    TO LLAVE-SESION-W.
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
           INITIALIZE NOM-DIV-LNK

           EVALUATE MES-CONTAB-LLEGADA-W
             WHEN 01 MOVE "\ENE" TO MES-SAL-W 
             WHEN 02 MOVE "\FEB" TO MES-SAL-W 
             WHEN 03 MOVE "\MAR" TO MES-SAL-W 
             WHEN 04 MOVE "\ABR" TO MES-SAL-W 
             WHEN 05 MOVE "\MAY" TO MES-SAL-W 
             WHEN 06 MOVE "\JUN" TO MES-SAL-W 
             WHEN 07 MOVE "\JUL" TO MES-SAL-W 
             WHEN 08 MOVE "\AGT" TO MES-SAL-W 
             WHEN 09 MOVE "\SEP" TO MES-SAL-W 
             WHEN 10 MOVE "\OCT" TO MES-SAL-W 
             WHEN 11 MOVE "\NOV" TO MES-SAL-W 
             WHEN 12 MOVE "\DIC" TO MES-SAL-W 
           END-EVALUATE

      *     INSPECT IP-SERVER-W REPLACING FIRST "/" BY "\"
           INITIALIZE NOM-DIV-LNK
           MOVE "\\" TO NOM-DIV-LNK
           INSPECT NOM-DIV-LNK REPLACING FIRST "             "
                                          BY IP-SERVER-W

           INSPECT NOM-DIV-LNK REPLACING FIRST "     "
                                          BY    "\PSC$"

           INSPECT NOM-DIV-LNK REPLACING FIRST " "
                                          BY "\"
 
           INSPECT NOM-DIV-LNK   REPLACING FIRST "      "
                                            BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT NOM-DIV-LNK   REPLACING FIRST "                     "
                                            BY   "\CONTROL\SC-DIVIS.DAT".

           INITIALIZE NOM-PLANO-W
           MOVE "\\" TO NOM-PLANO-W 
           INSPECT NOM-PLANO-W  REPLACING FIRST "             "
                                             BY IP-SERVER-W
      
           INSPECT NOM-PLANO-W REPLACING FIRST "     "
                                          BY    "\PSC$"
      
      
           INSPECT NOM-PLANO-W  REPLACING FIRST " "
                                             BY "\" 
      
           INSPECT NOM-PLANO-W  REPLACING FIRST "                                                            "
                                             BY "PROG\DATOS\JSON\JSC-DIVIS-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"

           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                             BY LLAVESESION-LLEGA-W.  


       ABRIR-DIVIS.
           OPEN INPUT ARCHIVO-DIVISION

           EVALUATE OTR-STAT
             WHEN "00" CONTINUE
             WHEN "35" OPEN OUTPUT ARCHIVO-DIVISION
                       CLOSE       ARCHIVO-DIVISION
                       GO TO ABRIR-DIVIS
           END-EVALUATE.
 
       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-DIV.
           MOVE "{*CODIGOS*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.
           MOVE 0 TO SW-FIN.

       LEER-DIV.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-PLANO
              WRITE REG-PLANO END-WRITE
           END-IF

           READ ARCHIVO-DIVISION NEXT WITH NO LOCK AT END 
                GO TO CERRAR-DIV
           END-READ.

           IF COD-DIV = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE
              IF COD-DIV = COD-DIV-J
                 MOVE 0 TO SW-FIN
              ELSE
                 INITIALIZE LIN-1
                 MOVE COD-DIV     TO COD-DIV-J
                 MOVE DESCRIP-DIV TO DESCRIP-DIV-J
                 INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
                 MOVE "," TO CIERRE-LIN-1          
              END-IF
           END-IF

           GO TO LEER-DIV.

       CERRAR-DIV.
           INITIALIZE LIN-1
           MOVE LIN-1 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE
         
           MOVE "]}" TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE
                
           CLOSE ARCHIVO-DIVISION 
                 ARCHIVO-JSON.
      
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