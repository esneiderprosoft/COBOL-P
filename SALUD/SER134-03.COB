      *============================================================
      * CREACION DE ARCHIVO JSON SER134-03  SALUD
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
          02 FILLER                   PIC X(9)   VALUE "*CEDULA*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CEDULA-REGIS-J           PIC X(15). 
      *    02 FILLER                   PIC XX     VALUE "*,".
      *    02 FILLER                   PIC X(9)   VALUE "*NOMBRE*:".
      *    02 FILLER                   PIC X      VALUE "*".
      *    02 NOMBRE-REGIS-J           PIC X(54).
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
           INITIALIZE PACIE-W SISVAN-W NOM-PLANO-W.

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           MOVE "\\" TO USU-W PACIE-W CIUD-W NOM-PLANO-W.

           INSPECT PACIE-W REPLACING FIRST "                    "
                                        BY IP-DATOS-USUNET
           INSPECT PACIE-W REPLACING FIRST " "
                                        BY "\"

           MOVE PACIE-W TO SISVAN-W
           INSPECT SISVAN-W REPLACING FIRST "                           "
                                         BY "PROG\HC\DATOS\SC-SISVAN.DAT".



           INSPECT PACIE-W REPLACING FIRST "        "
                                        BY DIR-CONTAB-LLEGADA-W.
           INSPECT PACIE-W REPLACING FIRST "                     "
                                        BY "\CONTROL\SC-PACIE.DAT".

           INSPECT NOM-PLANO-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET
           INSPECT NOM-PLANO-W  REPLACING FIRST "                                                              "
                                             BY "\PROG\DATOS\JSON\JSC-SISVAN-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"
           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                             BY LLAVESESION-LLEGA-W.



       ABRIR-USUARIOS.      
           OPEN INPUT ARCHIVO-SISVAN
                      ARCHIVO-PACIENTES.
 
       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.

           MOVE "{*SISVAN*:[" TO  DATOS-JSON   
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

	   
           READ ARCHIVO-SISVAN NEXT WITH NO LOCK AT END 
                GO TO CERRAR-ARCHIVOS
           END-READ	
           
           IF COD-SIS = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE
      *        MOVE COD-SIS               TO COD-PACI 
              MOVE FICHA-SIS             TO FICHA-REGIS-J  
              MOVE COD-PACI              TO CEDULA-REGIS-J   
      *        MOVE DESCRIP-PACI          TO NOMBRE-REGIS-J
              MOVE BARRIO-SIS            TO BARRIO-REGIS-J
              MOVE CENT-SAL-SIS          TO CNT-SALD-REGIS-J
              MOVE LACT-ACTUAL-SIS       TO LACT-ACT-REGIS-J
              MOVE LACT-EXCLUS-SIS       TO EXCLUS-REGIS-J
              MOVE MESES-LACT-EXCLUS-SIS TO MESES-LACT-REGIS-J
              MOVE PROG-NUT-SIS          TO PROGRA-REGIS-J

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
                
           CLOSE ARCHIVO-SISVAN
                 ARCHIVO-PACIENTES
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