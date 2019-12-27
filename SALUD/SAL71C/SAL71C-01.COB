      *============================================================
      * CREACION DE ARCHIVO JSON SAL71C-01  SALUD
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.    
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

      * "../control/SC-GRUPOCAP.DAT"
           SELECT ARCHIVO-GR-CAP LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-GRPCAP-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CODIGO-GRCAP
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-JSON LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.
                                                          
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-GRCAP.CBL".

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
       77 NOM-GRPCAP-W              PIC X(60).
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

       01 LIN-1.  
          02 FILLER                   PIC X(10)  VALUE "{*CODIGO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CODIGO-GRCAP-J           PIC X(2).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)   VALUE "*DESCRIP*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DESCRIP-GRCAP-J          PIC X(25).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*PROCED*:".
          02 FILLER                   PIC X      VALUE "*".
          02 PROCD-QUIR-J             PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)  VALUE "*LABORAT*:".
          02 FILLER                   PIC X      VALUE "*".
          02 LABOR-CLIN-J             PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*IMAGEN*:".
          02 FILLER                   PIC X      VALUE "*".
          02 IMAGEN-J                 PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*OTRS_SER*:".
          02 FILLER                   PIC X      VALUE "*".
          02 OTR-SERV-J               PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)  VALUE "*CONSULT*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CONSUL-TERP-J            PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*PATOLG*:".
          02 FILLER                   PIC X      VALUE "*".
          02 PATOLG-CITOL-J           PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*PROMYPREV*:".
          02 FILLER                   PIC X      VALUE "*".
          02 PROMOC-PREV-J            PIC X.               
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*MEDICM*:".
          02 FILLER                   PIC X      VALUE "*".
          02 MEDICAM-J                PIC X.
          02 FILLER                   PIC XX     VALUE "*}".    
          02 CIERRE-LIN-1             PIC X.


       01 DATOS-ENVIO.
          02 DAT0-ENV                   PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 NOMBRE-JSON-ENV            PIC X(28).

      
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
              MOVE "SAL71C"                 TO MSJ3-HTML
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
              MOVE "SAL71C"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-GR-CAP.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"         
              CONTINUE            
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE "SAL71C-01"                TO MSJ2-HTML
              MOVE NOM-GRPCAP-W               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "SAL71C-ARCHIVO-JSON"    TO MSJ2-HTML
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
      *     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
           MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W
          
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.
 
             
       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
      *     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
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
              GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.
           INITIALIZE NOM-GRPCAP-W

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           MOVE "\\" TO NOM-GRPCAP-W

           INSPECT NOM-GRPCAP-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

           INSPECT NOM-GRPCAP-W REPLACING FIRST " "
                                             BY "\"

           INSPECT NOM-GRPCAP-W   REPLACING FIRST "                              "
                                               BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT NOM-GRPCAP-W   REPLACING FIRST "                        "
                                               BY "\CONTROL\SC-GRUPOCAP.DAT".
                                                            

           INITIALIZE NOM-PLANO-W

           MOVE OPERADOR-LLEGADA-W TO OPER-JSON-W
           MOVE FUNCTION CURRENT-DATE TO FECHA-JSON-W
           ACCEPT HORA-JSON-W FROM TIME


           MOVE NOMBRE-JSON-W  TO NOM-PLANO-W.
           MOVE NOM-JSON-W     TO NOMBRE-JSON-ENV.



       ABRIR-GRUPO-SER.
           OPEN INPUT ARCHIVO-GR-CAP.
           MOVE "00" TO DAT0-ENV. 
 
       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.

           MOVE "{*DIVISIONES*:[" TO  DATOS-JSON   
           INSPECT DATOS-JSON  REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON  END-WRITE
           MOVE 0 TO SW-FIN.

       LEER-GRUPOCAP.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-JSON  
              WRITE REG-JSON  END-WRITE
           END-IF

           READ ARCHIVO-GR-CAP NEXT WITH NO LOCK AT END 
                GO TO CERRAR-GRUPOCAP
           END-READ
          
           
           IF CODIGO-GRCAP = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE
              MOVE CODIGO-GRCAP   TO CODIGO-GRCAP-J
              MOVE DESCRIP-GRCAP  TO DESCRIP-GRCAP-J
              MOVE CLASE-GRCAP(1) TO PROCD-QUIR-J 
              MOVE CLASE-GRCAP(2) TO LABOR-CLIN-J 
              MOVE CLASE-GRCAP(3) TO IMAGEN-J
              MOVE CLASE-GRCAP(4) TO OTR-SERV-J
              MOVE CLASE-GRCAP(5) TO CONSUL-TERP-J
              MOVE CLASE-GRCAP(6) TO PATOLG-CITOL-J
              MOVE CLASE-GRCAP(7) TO PROMOC-PREV-J
              MOVE CLASE-GRCAP(8) TO MEDICAM-J


              INSPECT DESCRIP-GRCAP-J REPLACING ALL "*" BY "x"
              INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
              MOVE "," TO CIERRE-LIN-1           
           END-IF

           GO TO LEER-GRUPOCAP.

       CERRAR-GRUPOCAP.
           INITIALIZE LIN-1	  
           MOVE LIN-1 TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
          
           MOVE "]}" TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
                
           CLOSE ARCHIVO-GR-CAP  
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