      *============================================================
      * CREACION DE ARCHIVO JSON SER134-01  SALUD
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

           SELECT ARCHIVO-JSON LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.
                                                          
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-PACIE.CBL".
       COPY "..\..\FUENTES\FD-CIUDA.CBL".
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
          02 FILLER                   PIC X(10)  VALUE "{*CEDULA*:".
          02 FILLER                   PIC X      VALUE "*".
          02 COD-REGIS-J              PIC X(15).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*NOMBRE*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DESCRIP-REGIS-J          PIC X(54).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(8)   VALUE "*FICHA*:".
          02 FILLER                   PIC X      VALUE "*".
          02 FICHA-REGIS-J            PIC X(6).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(8)   VALUE "*FECHA*:".
          02 FILLER                   PIC X      VALUE "*".
          02 FECHA-REGIS-J.
             05 ANO-REGIS-J           PIC 9(4).
             05 MES-REGIS-J           PIC 99.
             05 DIA-REGIS-J           PIC 99.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(7)   VALUE "*EDAD*:".
          02 FILLER                   PIC X      VALUE "*".
          02 NACI-REGIS-J             PIC X(8).            
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(7)   VALUE "*SEXO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 SEXO-REGIS-J             PIC X.            
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*DIRECC*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DIRECC-REGIS-J           PIC X(30).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*CIUDAD*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CIUDAD-REGIS-J           PIC X(20).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*TELEFONO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 TELEF-REGIS-J            PIC X(10).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(7)   VALUE "*ZONA*:".
          02 FILLER                   PIC X      VALUE "*".          
          02 ZONA-REGIS-J             PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(13)  VALUE "*REGIMEN*:".
          02 FILLER                   PIC X      VALUE "*".          
          02 REGIMEN-REGIS-J          PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(8)   VALUE "*ETNIA*:".
          02 FILLER                   PIC X      VALUE "*".
          02 ETNIA-REGIS-J            PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(13)  VALUE "*CENTRO_SAL*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CENT-SAL-SIS-J           PIC X(20).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*BARRIO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 BARRIO-SIS-J             PIC X(20).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(8)   VALUE "*LACTA*:".
          02 FILLER                   PIC X      VALUE "*".
          02 LACTA-SIS-J              PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*EXCLUS*:".
          02 FILLER                   PIC X      VALUE "*".
          02 EXCLUS-SIS-J             PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*MESLAC*:".
          02 FILLER                   PIC X      VALUE "*".
          02 MESLAC-SIS-J             PIC X(2).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*PROGRM*:".
          02 FILLER                   PIC X      VALUE "*".
          02 PROGRM-SIS-J             PIC X(2).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*OPER_CREA*:".
          02 FILLER                   PIC X      VALUE "*".
          02 OPER-CRE-REGIS-J         PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*FECH_CREA*:".
          02 FILLER                   PIC X      VALUE "*".
          02 FECH-CRE-REGIS-J         PIC X(6).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*OPER_MODF*:".
          02 FILLER                   PIC X      VALUE "*".
          02 OPER-MOD-REGIS-J         PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*FECH_MODF*:".
          02 FILLER                   PIC X      VALUE "*".
          02 FECH-MOD-REGIS-J         PIC X(6).
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

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "ARCHIVO-JSON-SER134-01" TO MSJ2-HTML
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


           INSPECT NOM-PLANO-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-PLANO-W  REPLACING FIRST "                                           "
                                             BY "\PROG\DATOS\JSON\XXXXXXXXXXXXXXXXXXXXX.JSON"

           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXX"
                                             BY NOMBRE-JSON-W.

           MOVE NOMBRE-JSON-W TO NOMBRE-ENV.


       ABRIR-USUARIOS.
           OPEN INPUT ARCHIVO-PACIENTES
                      ARCHIVO-CIUDAD
                      ARCHIVO-SISVAN.

           MOVE "00" TO DAT0-ENV.
 
       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.

           MOVE "{*REGISTRO*:[" TO  DATOS-JSON   
           INSPECT DATOS-JSON  REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON  END-WRITE
           MOVE 0 TO SW-FIN.

       LEER-DATOS.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-JSON  
              WRITE REG-JSON  END-WRITE
           END-IF

           READ ARCHIVO-PACIENTES NEXT WITH NO LOCK AT END 
                GO TO CERRAR-ARCHIVOS
           END-READ
           READ ARCHIVO-CIUDAD NEXT WITH NO LOCK AT END 
                GO TO CERRAR-ARCHIVOS
           END-READ


           MOVE COD-PACI TO COD-SIS
           READ ARCHIVO-SISVAN WITH NO LOCK
                INVALID KEY 
                  MOVE SPACES TO DESCRIP-REGIS-J
           END-READ
           
           IF COD-SIS = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE
                
              MOVE COD-SIS               TO COD-REGIS-J
              MOVE DESCRIP-PACI          TO DESCRIP-REGIS-J
              MOVE FICHA-PACI            TO FICHA-REGIS-J
              MOVE FECHA-REG-SIS         TO FECHA-REGIS-J
              MOVE NACIM-PACI            TO NACI-REGIS-J
              MOVE SEXO-PACI             TO SEXO-REGIS-J
              MOVE DIRECC-PACI           TO DIRECC-REGIS-J
              MOVE NOMBRE-CIU            TO CIUDAD-REGIS-J             
              MOVE TELEFONO-PACI         TO TELEF-REGIS-J
              MOVE ZONA-PACI             TO ZONA-REGIS-J
              MOVE TIPO-PACI             TO REGIMEN-REGIS-J
              MOVE ETNIA-PACI            TO ETNIA-REGIS-J
              MOVE CENT-SAL-SIS          TO CENT-SAL-SIS-J
              MOVE BARRIO-SIS            TO BARRIO-SIS-J 
              MOVE LACT-ACTUAL-SIS       TO LACTA-SIS-J
              MOVE LACT-EXCLUS-SIS       TO EXCLUS-SIS-J
              MOVE MESES-LACT-EXCLUS-SIS TO MESLAC-SIS-J
              MOVE PROG-NUT-SIS          TO PROGRM-SIS-J     
              MOVE OPER-CRE-SIS          TO OPER-CRE-REGIS-J
              MOVE FECH-CRE-SIS          TO FECH-CRE-REGIS-J
              MOVE OPER-MOD-SIS          TO OPER-MOD-REGIS-J
              MOVE FECH-MOD-SIS          TO FECH-MOD-REGIS-J

              INSPECT DESCRIP-REGIS-J REPLACING ALL "*" BY "x"
              INSPECT DESCRIP-REGIS-J REPLACING ALL "¥" BY "N"

              INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
              MOVE "," TO CIERRE-LIN-1           
           END-IF

           GO TO LEER-DATOS.

       CERRAR-ARCHIVOS.
           INITIALIZE LIN-1	  
           MOVE LIN-1 TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
          
           MOVE "]}" TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
                
           CLOSE ARCHIVO-PACIENTES
                 ARCHIVO-CIUDAD
                 ARCHIVO-SISVAN  
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