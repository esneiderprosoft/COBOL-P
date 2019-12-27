      *============================================================
      * CREACION DE ARCHIVO JSON SAL714-TB  SALUD
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
                  ASSIGN TO SISVAN-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-SIS
                  ALTERNATE RECORD KEY FICHA-SIS     WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-REG-SIS WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-JSON LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.
                                                          
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-SISVA.CBL".


       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON      PIC X(500).

      
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


       01 DATOS-ENVIO.
          02 DAT0-ENV                   PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 NOMBRE-ENV                 PIC X(28).


       01 LIN-1.  
          02 FILLER                   PIC X(9)   VALUE "{*FECHA*:".
          02 FILLER                   PIC X      VALUE "*".
          02 FECHA-SIS-J              PIC X(8).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*EDAD_MES*:".
          02 FILLER                   PIC X      VALUE "*".
          02 EDADMES-SIS-J            PIC 9(3).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(7)   VALUE "*PESO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 PESO-SIS-J               PIC ZZZZ.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(8)   VALUE "*TALLA*:".
          02 FILLER                   PIC X      VALUE "*".
          02 TALLA-SIS-J              PIC ZZZ.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(6)   VALUE "*IMC*:".
          02 FILLER                   PIC X      VALUE "*".
          02 IMC-SIS-J                PIC ZZ.Z.
          02 FILLER                   PIC XX     VALUE "*}".    
          02 CIERRE-LIN-1             PIC X.

           
      
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
              MOVE "SAL714-TB"              TO MSJ3-HTML
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
              MOVE "SAL714-TB"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.


       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SISVAN.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"         
              CONTINUE            
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE "ARCHIVO-SISVAN-SAL714-TB" TO MSJ2-HTML
              MOVE SISVAN-W                   TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  		   

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "ARCHIVO-JSON-SAL714-TB" TO MSJ2-HTML
              MOVE NOM-PLANO-W              TO MSJ3-HTML
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
              INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W
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
           INITIALIZE SISVAN-W NOM-PLANO-W.

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           MOVE "\\" TO SISVAN-W NOM-PLANO-W.

           INSPECT SISVAN-W REPLACING FIRST "                    "
                                        BY IP-DATOS-USUNET

           INSPECT SISVAN-W REPLACING FIRST " "
                                        BY "\"

           INSPECT SISVAN-W REPLACING FIRST "                           "
                                         BY "PROG\HC\DATOS\SC-SISVAN.DAT".




           INSPECT NOM-PLANO-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-PLANO-W  REPLACING FIRST "                                           "
                                             BY "\PROG\DATOS\JSON\XXXXXXXXXXXXXXXXXXXXX.JSON"

           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXX"
                                             BY NOMBRE-JSON-W.

           MOVE NOMBRE-JSON-W TO NOMBRE-ENV.


       ABRIR-USUARIOS.
           OPEN INPUT ARCHIVO-SISVAN.

           MOVE "00" TO DAT0-ENV.
 
       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.

           MOVE "{*TABLA*:[" TO  DATOS-JSON   
           INSPECT DATOS-JSON  REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON  END-WRITE
           MOVE 0 TO SW-FIN.

       LEER-DATOS.
                
              MOVE FECHA-CONTROL-SIS (I)   TO FECHA-SIS-J
              MOVE EDAD-MESES-SIS    (I)   TO EDADMES-SIS-J
              MOVE PESO-CONTROL-SIS  (I)   TO PESO-SIS-J
              MOVE TALLA-CONTROL-SIS (I)   TO TALLA-SIS-J
              MOVE IMC-CONTROL-SIS   (I)   TO IMC-SIS-J


              INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
              MOVE "," TO CIERRE-LIN-1.           


       CERRAR-ARCHIVOS.
           INITIALIZE LIN-1	  
           MOVE LIN-1 TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
          
           MOVE "]}" TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
                
           CLOSE ARCHIVO-SISVAN  
                 ARCHIVO-JSON.      

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