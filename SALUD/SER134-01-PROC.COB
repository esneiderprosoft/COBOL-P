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


           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN TO USU-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

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
                  ASSIGN SISVAN-W
                  ORGANIZATION IS INDEXED
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
       COPY "..\..\FUENTES\FD-USUAR.CBL".
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


       77 NOM-PLANO-W               PIC X(90).
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
           02 DIR-CONTAB-LLEGADA-W      PIC X(8).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
		   02 IDENTIF-LLEGAD-W          PIC X(15).


       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.


       01 LIN-1.  
          02 FILLER                   PIC X(9)   VALUE "{*FICHA*:".
          02 FILLER                   PIC X      VALUE "*".
          02 FICHA-REGIS-J            PIC X(6).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*FECHA_REG*:".
          02 FILLER                   PIC X      VALUE "*".
          02 FECHA-REG-REGIS-J        PIC X(8).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*CEDULA*:".
          02 FILLER                   PIC X      VALUE "*".
          02 COD-REGIS-J              PIC X(15).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*BARRIO*:".
          02 FILLER                   PIC X      VALUE "*".  
          02 BARRIO-REGIS-J           PIC X(20). 
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(13)  VALUE "*CNTR_SALUD*:".
          02 FILLER                   PIC X      VALUE "*".	
          02 CNT-SALD-REGIS-J         PIC X(20).		  		  
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*LACT_ACT*:".
          02 FILLER                   PIC X      VALUE "*".		  
          02 LACT-ACT-REGIS-J         PIC X.		  
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*EXCLUSIVA*:".
          02 FILLER                   PIC X      VALUE "*".		  
          02 EXCLUS-REGIS-J           PIC X.		  
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(13)  VALUE "*MESES_LACT*:".
          02 FILLER                   PIC X      VALUE "*".		  
          02 MESES-LACT-REGIS-J       PIC X(2).		  
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*PROGRAMA*:".
          02 FILLER                   PIC X      VALUE "*".		  
          02 PROGRA-REGIS-J           PIC XX.
          02 FILLER                   PIC XX     VALUE "*}".    
          02 CIERRE-LIN-1             PIC X.

       01 LIN-2.  
          02 FILLER                   PIC X(10)  VALUE "{*NOMBRE*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DESCRIP-REGIS-J          PIC X(54).
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
          02 FILLER                   PIC XX     VALUE "*}".    
          02 CIERRE-LIN-2             PIC X.
	  
		  
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
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"         
              CONTINUE            
           ELSE
              MOVE OTR-STAT                     TO MSJ1-HTML
              MOVE USU-W                        TO MSJ2-HTML
              MOVE "ARCHIVO-USUARIOS-SER134-01" TO MSJ3-HTML
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
        
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SISVAN.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE SISVAN-W             TO MSJ2-HTML
              MOVE "SER134-0"           TO MSJ3-HTML
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
              INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, IDENTIF-LLEGAD-W
           END-UNSTRING.

           MOVE LLAVESESION-LLEGA-W   TO LLAVE-SESION-W.
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
              GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.
           INITIALIZE USU-W PACIE-W CIUD-W SISVAN-W NOM-PLANO-W.

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           MOVE "\\" TO USU-W PACIE-W CIUD-W NOM-PLANO-W.

           INSPECT USU-W REPLACING FIRST "                    "
                                        BY IP-DATOS-USUNET
           INSPECT USU-W REPLACING FIRST " "
                                        BY "\"


           MOVE USU-W TO CIUD-W SISVAN-W
           INSPECT CIUD-W REPLACING FIRST "                          "
                                       BY "PROG\DATOS\SC-ARCHCIUD.DAT"
           INSPECT SISVAN-W REPLACING FIRST "                           "
                                         BY "PROG\HC\DATOS\SC-SISVAN.DAT".

           INSPECT USU-W REPLACING FIRST "        "
                                        BY DIR-CONTAB-LLEGADA-W.
           INSPECT USU-W REPLACING FIRST "         "
                                      BY "\CONTROL\".

           MOVE USU-W TO PACIE-W
           INSPECT USU-W REPLACING FIRST "              "
                                      BY "SC-ARCHUSU.DAT"           
           INSPECT PACIE-W REPLACING FIRST "            "
                                        BY "SC-PACIE.DAT".		   


           INSPECT NOM-PLANO-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET
           INSPECT NOM-PLANO-W  REPLACING FIRST "                                                                "
                                             BY "\PROG\DATOS\JSON\JSC-REGISTRO-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"
           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                             BY LLAVESESION-LLEGA-W.



       ABRIR-USUARIOS.
           OPEN INPUT ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT AT END MOVE 0 TO OPCION-1.
           CLOSE ARCHIVO-USUARIOS.

           IF PREFIJO-USU = "  "
              MOVE "00"  TO PREFIJO-USU
           END-IF.

           OPEN INPUT ARCHIVO-PACIENTES
                      ARCHIVO-CIUDAD
                      ARCHIVO-SISVAN.


           MOVE CIUDAD-PACI   TO COD-CIU.
           READ ARCHIVO-CIUDAD  WITH NO LOCK
                INVALID KEY
                    MOVE CIUDAD-PACI TO NOMBRE-CIU
           END-READ.
      *     MOVE COD-SIS TO COD-PACI
      *     READ ARCHIVO-PACIENTES WITH NO LOCK                  
      *           INVALID KEY
      *              MOVE COD-SIS TO DESCRIP-PACI     
      *     END-READ.	   

					  
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
                      
           READ ARCHIVO-SISVAN NEXT WITH NO LOCK AT END 
                GO TO CERRAR-ARCHIVOS
           END-READ
		   
           IF COD-SIS = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE              
              MOVE COD-SIS               TO COD-REGIS-J
              MOVE FICHA-SIS             TO FICHA-REGIS-J
              MOVE FECHA-REG-SIS         TO FECHA-REG-REGIS-J	  
              MOVE BARRIO-SIS            TO BARRIO-REGIS-J
              MOVE CENT-SAL-SIS          TO CNT-SALD-REGIS-J
              MOVE LACT-ACTUAL-SIS       TO LACT-ACT-REGIS-J
              MOVE LACT-EXCLUS-SIS       TO EXCLUS-REGIS-J
              MOVE MESES-LACT-EXCLUS-SIS TO MESES-LACT-REGIS-J
              MOVE PROG-NUT-SIS          TO PROGRA-REGIS-J

              INSPECT DESCRIP-REGIS-J REPLACING ALL "*" BY "x"
              INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
              MOVE "," TO CIERRE-LIN-1           
           END-IF

           GO TO LEER-DATOS.

       CERRAR-ARCHIVOS.
           INITIALIZE LIN-1	  
           MOVE LIN-1 TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
          

       LEER-DATOS-LINEA2.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-JSON  
              WRITE REG-JSON  END-WRITE
           END-IF
                      
	       READ ARCHIVO-PACIENTES NEXT WITH NO LOCK AT END 
                GO TO CERRAR-ARCHIVOS
           END-READ
		   
           IF COD-SIS = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE              
              MOVE COD-SIS               TO COD-REGIS-J
              MOVE FICHA-SIS             TO FICHA-REGIS-J
              MOVE FECHA-REG-SIS         TO FECHA-REG-REGIS-J
      *        MOVE DESCRIP-PACI          TO DESCRIP-REGIS-J
      *        MOVE NACIM-PACI            TO NACI-REGIS-J
      *        MOVE SEXO-PACI             TO SEXO-REGIS-J
      *        MOVE DIRECC-PACI           TO DIRECC-REGIS-J
      *        MOVE NOMBRE-CIU            TO CIUDAD-REGIS-J 
              MOVE BARRIO-SIS            TO BARRIO-REGIS-J			  
      *        MOVE TELEFONO-PACI         TO TELEF-REGIS-J
      *        MOVE ZONA-PACI             TO ZONA-REGIS-J
      *        MOVE TIPO-PACI             TO REGIMEN-REGIS-J
      *        MOVE ETNIA-PACI            TO ETNIA-REGIS-J
              MOVE BARRIO-SIS            TO BARRIO-REGIS-J
              MOVE CENT-SAL-SIS          TO CNT-SALD-REGIS-J
              MOVE LACT-ACTUAL-SIS       TO LACT-ACT-REGIS-J
              MOVE LACT-EXCLUS-SIS       TO EXCLUS-REGIS-J
              MOVE MESES-LACT-EXCLUS-SIS TO MESES-LACT-REGIS-J
              MOVE PROG-NUT-SIS          TO PROGRA-REGIS-J

              INSPECT DESCRIP-REGIS-J REPLACING ALL "*" BY "x"
              INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
              MOVE "," TO CIERRE-LIN-1           
           END-IF

           GO TO LEER-DATOS-LINEA2.

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
           MOVE "00"              TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"   USING COBW3

           MOVE "..\..\FRAMEWORKS\PAGINAS\RECIBIDOS.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.
      
       COPY "..\..\FUENTES\SC-WEB19.CBL".      
