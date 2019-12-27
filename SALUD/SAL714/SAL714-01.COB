      *============================================================
      * CREACION DE ARCHIVO JSON SAL714-01  SALUD
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  


           SELECT ARCHIVO-PACIENTES LOCK MODE IS AUTOMATIC
                  ASSIGN TO PACIE-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PACI
                  ALTERNATE RECORD KEY  DESCRIP-PACI  WITH DUPLICATES
                  ALTERNATE RECORD KEY  EPS-PACI      WITH DUPLICATES
                  ALTERNATE RECORD KEY  ID-COTIZ-PACI WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-CIUDAD  LOCK MODE IS AUTOMATIC
                  ASSIGN TO CIUD-W 
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-CIU
                  ALTERNATE RECORD KEY IS NOMBRE-CIU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-SISVAN LOCK MODE IS AUTOMATIC
                  ASSIGN TO SISVAN-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-SIS
                  ALTERNATE RECORD KEY FICHA-SIS     WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-REG-SIS WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

                                                          
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-PACIE.CBL".
       COPY "..\..\FUENTES\FD-CIUDA.CBL".
       COPY "..\..\FUENTES\FD-SISVA.CBL".

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 TABLA-LLEGADA-W.
          02 DATO-LLEGADA-W PIC X OCCURS 200.
       
       01 LLEGADA-W         PIC X(100).

       01 TABLA-TEMP.
          02 CARAC-TEMP     PIC X OCCURS 50.


       77 NOM-PLANO-W               PIC x(90).
       77 USU-W                     PIC X(60).
       77 PACIE-W                   PIC X(60).
       77 CIUD-W                    PIC X(60).
       77 SISVAN-W                  PIC X(60).   
       77 MES-SAL-W                 PIC X(4).
       77 CONT                      PIC 9999.
       
       01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 DIR-CONTAB-LLEGADA-W      PIC X(30).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 OPERADOR-LLEGADA-W        PIC X(4).
           02 COD-SISVAN-LLEGADA-W      PIC X(15).
             


       01 DATOS-ENVIO.
          02 SWINVALID-ENV              PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 SWINVALID1-ENV             PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 SWINVALID2-ENV             PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 COD-SISVAN-ENV             PIC X(15).
          02 FILLER                     PIC X VALUE "|".
          02 NOMBRE-SISVAN-ENV          PIC X(54).
          02 FILLER                     PIC X VALUE "|".
          02 FICHA-SISVAN-ENV           PIC X(6).
          02 FILLER                     PIC X VALUE "|".
          02 FECHA-SISVAN-ENV           PIC X(8).
          02 FILLER                     PIC X VALUE "|".
          02 FECHA-NACI-PACI-ENV        PIC X(8).
          02 FILLER                     PIC X VALUE "|".
          02 SEXO-PACI-ENV              PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 DIRECC-PACI-ENV            PIC X(30).
          02 FILLER                     PIC X VALUE "|".
          02 CIUDAD-ENV                 PIC X(20).
          02 FILLER                     PIC X VALUE "|".
          02 TELFN-PACI-ENV             PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 ZONA-PACI-ENV              PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 REGIMEN-PACI-ENV           PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 ETNIA-PACI-ENV             PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 CNTSALUD-SISVAN-ENV        PIC X(20).
          02 FILLER                     PIC X VALUE "|".
          02 BARRIO-SISVAN-ENV          PIC X(20).
          02 FILLER                     PIC X VALUE "|".
          02 PROGRM-SISVAN-ENV          PIC X(2).
          02 FILLER                     PIC X VALUE "|".
          02 LACT-SISVAN-ENV            PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 EXCLUS-SISVAN-ENV          PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 MESLACT-SISVAN-ENV         PIC X(2).
          02 FILLER                     PIC X VALUE "|".
          02 OPERCRE-SISVAN-ENV         PIC X(4).
          02 FILLER                     PIC X VALUE "|".
          02 FECHACRE-SISVAN-ENV        PIC X(6).
          02 FILLER                     PIC X VALUE "|".
          02 OPERMDF-SISVAN-ENV         PIC X(4).
          02 FILLER                     PIC X VALUE "|".
          02 FECHAMDF-SISVAN-ENV        PIC X(6).


           
      
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
              MOVE "ARCHIVO-USUNET"         TO MSJ2-HTML
              MOVE "SER134-01"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
          
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "ARCHIVO-SESION"         TO MSJ2-HTML
              MOVE "SER134-01"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"         
              CONTINUE            
           ELSE
              MOVE OTR-STAT                      TO MSJ1-HTML
              MOVE "ARCHIVO-PACIENTES-SER134-01" TO MSJ2-HTML
              MOVE PACIE-W                       TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CIUDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"         
              CONTINUE            
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE "ARCHIVO-CIUDAD-SER134-01" TO MSJ2-HTML
              MOVE CIUD-W                     TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF. 

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SISVAN.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"         
              CONTINUE            
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE "ARCHIVO-SISVAN-SER134-01" TO MSJ2-HTML
              MOVE SISVAN-W                   TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO LLEGADA-W


           UNSTRING LLEGADA-W DELIMITED BY "|"
              INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W, COD-SISVAN-LLEGADA-W
           END-UNSTRING.

           MOVE LLAVESESION-LLEGA-W   TO LLAVE-SESION-W.
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
           INITIALIZE PACIE-W CIUD-W SISVAN-W NOM-PLANO-W.

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           MOVE "\\" TO PACIE-W CIUD-W SISVAN-W NOM-PLANO-W.

           INSPECT PACIE-W REPLACING FIRST "                    "
                                        BY IP-DATOS-USUNET
           INSPECT PACIE-W REPLACING FIRST " "
                                        BY "\"


           MOVE PACIE-W TO CIUD-W SISVAN-W
           INSPECT CIUD-W REPLACING FIRST "                          "
                                       BY "PROG\DATOS\SC-ARCHCIUD.DAT"
           INSPECT SISVAN-W REPLACING FIRST "                           "
                                         BY "PROG\HC\DATOS\SC-SISVAN.DAT".



           INSPECT PACIE-W REPLACING FIRST "                              "          
                                        BY DIR-CONTAB-LLEGADA-W.

           INSPECT PACIE-W REPLACING FIRST "         "
                                      BY "\CONTROL\".

           INSPECT PACIE-W REPLACING FIRST "            "
                                        BY "SC-PACIE.DAT".		   



       ABRIR-USUARIOS.
           OPEN INPUT ARCHIVO-PACIENTES
                      ARCHIVO-CIUDAD
                      ARCHIVO-SISVAN.


       LEER-SISVAN.
           MOVE COD-SISVAN-LLEGADA-W TO COD-SIS
           READ ARCHIVO-SISVAN WITH NO LOCK
                INVALID KEY 
                  MOVE "01" TO SWINVALID-ENV               
                NOT INVALID KEY
                  MOVE "00" TO SWINVALID-ENV
           END-READ.
                
              MOVE COD-SIS               TO COD-SISVAN-ENV
              MOVE FICHA-SIS             TO FICHA-SISVAN-ENV
              MOVE FECHA-REG-SIS         TO FECHA-SISVAN-ENV
              MOVE CENT-SAL-SIS          TO CNTSALUD-SISVAN-ENV
              MOVE BARRIO-SIS            TO BARRIO-SISVAN-ENV 
              MOVE PROG-NUT-SIS          TO PROGRM-SISVAN-ENV
              MOVE LACT-ACTUAL-SIS       TO LACT-SISVAN-ENV
              MOVE LACT-EXCLUS-SIS       TO EXCLUS-SISVAN-ENV 
              MOVE MESES-LACT-EXCLUS-SIS TO MESLACT-SISVAN-ENV
                   
              MOVE OPER-CRE-SIS          TO OPERCRE-SISVAN-ENV
              MOVE FECH-CRE-SIS          TO FECHACRE-SISVAN-ENV
              MOVE OPER-MOD-SIS          TO OPERMDF-SISVAN-ENV
              MOVE FECH-MOD-SIS          TO FECHAMDF-SISVAN-ENV.

       LEER-PACIENTE.
           MOVE COD-SIS TO COD-PACI
           READ ARCHIVO-PACIENTES WITH NO LOCK
                INVALID KEY 
                  MOVE "01" TO SWINVALID1-ENV               
                NOT INVALID KEY
                  MOVE "00" TO SWINVALID1-ENV
           END-READ.

           MOVE DESCRIP-PACI  TO NOMBRE-SISVAN-ENV
           MOVE NACIM-PACI    TO FECHA-NACI-PACI-ENV
           MOVE SEXO-PACI     TO SEXO-PACI-ENV
           MOVE ZONA-PACI     TO ZONA-PACI-ENV
           MOVE TIPO-PACI     TO REGIMEN-PACI-ENV
           MOVE TELEFONO-PACI TO TELFN-PACI-ENV
           MOVE DIRECC-PACI   TO DIRECC-PACI-ENV
           MOVE ETNIA-PACI    TO ETNIA-PACI-ENV.



       LEER-CIUDAD.
           MOVE CIUDAD-PACI TO COD-CIU
           READ ARCHIVO-CIUDAD WITH NO LOCK
                INVALID KEY 
                  MOVE "01" TO SWINVALID2-ENV               
                NOT INVALID KEY
                  MOVE "00" TO SWINVALID2-ENV
           END-READ.

           MOVE NOMBRE-CIU TO CIUDAD-ENV.



       CERRAR-ARCHIVOS.                
           CLOSE ARCHIVO-PACIENTES
                 ARCHIVO-CIUDAD
                 ARCHIVO-SISVAN.    

       ENVIO-DATOS.
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