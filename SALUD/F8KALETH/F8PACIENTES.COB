      *=================================================================
      *CREACION DE ARCHIVO JSON CON803 F8 DE CENTROS DE COSTO
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
                  ASSIGN NOM-PACIE-LNK
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PACI
                  ALTERNATE RECORD KEY  DESCRIP-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  EPS-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  ID-COTIZ-PACI WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-JSON
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT. 
                                          
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-PACIE.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-PLANO.
           02 DATOS-PLANO     PIC X(500).

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-PLANO-W                   PIC X(90).
       77 NOM-PACIE-LNK                 PIC X(90).
      
       77 MES-SAL-W                     PIC X(4).
       77 COMPARA-LLAVE-MAE             PIC X(12).
        
       01 LLEGADA-W                     PIC X(100).
      
       01  VAR-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 IP-SERVER-W               PIC X(18).
           02 DIR-CONTAB-LLEGADA-W      PIC X(8).
           02 MES-CONTAB-LLEGADA-W      PIC XX.


       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.


       01 LIN-1.            
          02 FILLER                  PIC X(15)  VALUE "{*CEDULA_PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-PACI-J  	     PIC X(15).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*TIPO_ID_PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-ID-PACI-J  	     PIC X(3).    
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*1ER_APEL_PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 1ER-APEL-PACI-J  	     PIC X(15).                   
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*2DO_APEL_PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 2DO-APEL-PACI-J         PIC X(15).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*NOMBRE_PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOMBRE-PACI-J           PIC X(24).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*EPS_PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EPS1-PACI-J             PIC X(3).
          02 EPS2-PACI-J             PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*DERECHO_PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DERECHO-PACI-J          PIC X.
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
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-USU-W                 TO MSJ2-HTML
              MOVE "F8KALETH"                TO MSJ3-HTML
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
              MOVE "F8KALETH"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE NOM-PACIE-LNK              TO MSJ2-HTML
              MOVE "F8KALETH"                 TO MSJ3-HTML
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
              INTO LLAVESESION-LLEGA-W, IP-SERVER-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W
           END-UNSTRING. 

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
           INITIALIZE NOM-PACIE-LNK
           MOVE "\\" TO NOM-PACIE-LNK
           INSPECT NOM-PACIE-LNK REPLACING FIRST "                  "
                                          BY IP-SERVER-W

           INSPECT NOM-PACIE-LNK REPLACING FIRST "      "
                                         BY    "\PSC$\"

 
           INSPECT NOM-PACIE-LNK   REPLACING FIRST "        "
                                           BY DIR-CONTAB-LLEGADA-W.
                                                        
           INSPECT NOM-PACIE-LNK   REPLACING FIRST "                     "
                                             BY    "\CONTROL\SC-PACIE.DAT".

           INITIALIZE NOM-PLANO-W.
           MOVE "\\" TO NOM-PLANO-W 
           INSPECT NOM-PLANO-W  REPLACING FIRST "                  "
                                             BY IP-SERVER-W
      
           INSPECT NOM-PLANO-W REPLACING FIRST "                "
                                         BY    "\PSCELECT$\TEMP\" 
      
           INSPECT NOM-PLANO-W  REPLACING FIRST "                                                "
                                             BY "JSC-ARCHPACIE-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"
 
           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                             BY LLAVESESION-LLEGA-W.  


       ABRIR-NOMBRE-PACIE.
           OPEN INPUT ARCHIVO-PACIENTES.

           EVALUATE OTR-STAT
             WHEN "00" CONTINUE
             WHEN "35" OPEN OUTPUT ARCHIVO-PACIENTES
                       CLOSE       ARCHIVO-PACIENTES
                       GO TO ABRIR-NOMBRE-PACIE
           END-EVALUATE.
 
       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-PACI.
           MOVE "{*CODIGOS*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.
           MOVE 0 TO SW-FIN.

       LEER-NOMBRE-PACIE.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-PLANO
              WRITE REG-PLANO END-WRITE
           END-IF

           READ ARCHIVO-PACIENTES NEXT WITH NO LOCK AT END 
                GO TO CERRAR-NOMBRE-PACIE
           END-READ.

           IF COD-PACI = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE
              INITIALIZE LIN-1
              MOVE COD-PACI           TO COD-PACI-J
              MOVE TIPO-ID-PACI       TO TIPO-ID-PACI-J
              MOVE 1ER-APEL-PACI      TO 1ER-APEL-PACI-J
              MOVE 2DO-APEL-PACI      TO 2DO-APEL-PACI-J
              MOVE NOMBRE-PACI        TO NOMBRE-PACI-J
              MOVE EPS1-PACI          TO EPS1-PACI-J
              MOVE EPS2-PACI          TO EPS2-PACI-J
              MOVE DERECHO-PACI       TO DERECHO-PACI-J
              

              INSPECT NOMBRE-PACI REPLACING ALL '"' BY " "
              INSPECT NOMBRE-PACI REPLACING ALL "*" BY " "
              INSPECT NOMBRE-PACI REPLACING ALL "Ñ" BY CARAC-ENE-MAY 
              INSPECT NOMBRE-PACI REPLACING ALL "ñ" BY CARAC-ENE-MIN

              INSPECT 1ER-APEL-PACI REPLACING ALL '"' BY " "
              INSPECT 1ER-APEL-PACI REPLACING ALL "*" BY " "
              INSPECT 1ER-APEL-PACI REPLACING ALL "Ñ" BY CARAC-ENE-MAY 
              INSPECT 1ER-APEL-PACI REPLACING ALL "ñ" BY CARAC-ENE-MIN

              INSPECT 2DO-APEL-PACI REPLACING ALL '"' BY " "
              INSPECT 2DO-APEL-PACI REPLACING ALL "*" BY " "
              INSPECT 2DO-APEL-PACI REPLACING ALL "Ñ" BY CARAC-ENE-MAY 
              INSPECT 2DO-APEL-PACI REPLACING ALL "ñ" BY CARAC-ENE-MIN


              INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
              MOVE "," TO CIERRE-LIN-1          
           END-IF


           GO TO LEER-NOMBRE-PACIE.

       CERRAR-NOMBRE-PACIE.
           INITIALIZE LIN-1
           MOVE LIN-1 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE
      
           MOVE "]}" TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE
                
           CLOSE ARCHIVO-PACIENTES
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