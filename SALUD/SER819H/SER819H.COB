      *============================================================
      * CREA JSON DATOS -ARCHIVO-HORARIO-PROF- SER819H  SALUD
      *============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL". 

           SELECT ARCHIVO-HORARIO-PROF LOCK MODE IS AUTOMATIC
                  ASSIGN HORARI-PROF-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE IS DYNAMIC RECORD KEY IS LLAVE-HOR-P
                  ALTERNATE RECORD KEY FECHA-HOR-P WITH DUPLICATES 
                  FILE STATUS IS OTR-STAT.
                                 

           SELECT ARCHIVO-PROFESIONALES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PROF-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PROF
                  ALTERNATE RECORD KEY DESCRIP-PROF WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-JSON
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.        


       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-HORAR.CBL".
       COPY "..\..\FUENTES\FD-PROFE.CBL".


       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON                PIC X(1000).




       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 HORARI-PROF-W             PIC X(60).
       77 NOM-PROF-W                PIC X(60).
       77 NOM-PLANO-W               PIC X(60).
       77 MES-SAL-W                 PIC X(4).

       01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W                PIC X(15).
              03 FECHA-LLEGAD-W             PIC X(8).                
              03 HORA-LLEGAD-W              PIC X(6).
           02 DIR-CONTAB-LLEGADA-W          PIC X(30).
           02 MES-CONTAB-LLEGADA-W          PIC XX.
           02 OPERADOR-LLEGADA-W            PIC X(4).            
           02 COD-LLEGADA-W                 PIC X(10).


       01 DATOS-ENVIO.
          02 DAT0-ENV                   PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 NOMBRE-JSON-ENV            PIC X(28).
         

       01 LIN-1.  
          02 FILLER                   PIC X(9)   VALUE "{*FECHA*:".
          02 FILLER                   PIC X      VALUE "*".
          02 FECHA-HRP-J              PIC X(6).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(14)  VALUE "*OBSERVACION*:".
          02 FILLER                   PIC X      VALUE "*".
          02 OBSERV-HORA-HRP-J        PIC X(50).
      *    02 FILLER                   PIC XX     VALUE "*,".
      *    02 FILLER                   PIC X(12)  VALUE "*SUCUR_UNO*:".
      *    02 FILLER                   PIC X      VALUE "*".
      *    02 SUCUR-UNO-HRP-J          PIC X(2).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(16)  VALUE "*HORA_INGRESO1*:".
          02 FILLER                   PIC X      VALUE "*".
          02 HORA-INIC1-HRP-J         PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(15)  VALUE "*HORA_SALIDA1*:".
          02 FILLER                   PIC X      VALUE "*".
          02 HORA-SALD1-HRP-J         PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*FRECU_UNO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 FRECU-UNO-HRP-J          PIC 99.
      *    02 FILLER                   PIC XX     VALUE "*,".
      *    02 FILLER                   PIC X(12)  VALUE "*SUCUR_DOS*:".
      *    02 FILLER                   PIC X      VALUE "*".
      *    02 SUCUR-DOS-HRP-J          PIC X(2).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(16)  VALUE "*HORA_INGRESO2*:".
          02 FILLER                   PIC X      VALUE "*".
          02 HORA-INIC2-HRP-J          PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(15)  VALUE "*HORA_SALIDA2*:".
          02 FILLER                   PIC X      VALUE "*".
          02 HORA-SALD2-HRP-J        PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*FRECU_DOS*:".
          02 FILLER                   PIC X      VALUE "*".
          02 FRECU-DOS-HRP-J          PIC 99.
                     
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
              MOVE NOM-USU-W                TO MSJ2-HTML
              MOVE "SER819H"                TO MSJ3-HTML
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
              MOVE "SER819H"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-HORARIO-PROF.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "SER819H"                TO MSJ2-HTML
              MOVE HORARI-PROF-W            TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PROFESIONALES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "SER819H"                TO MSJ2-HTML
              MOVE NOM-PROF-W               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
		   
		   
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-PLANO-W          TO MSJ2-HTML
              MOVE "SER868"             TO MSJ3-HTML
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

           UNSTRING DATO-LLEGADA-W DELIMITED BY "|" 
             INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W, COD-LLEGADA-W
           END-UNSTRING.

           MOVE LLAVESESION-LLEGA-W    TO LLAVE-SESION-W.


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
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INITIALIZE HORARI-PROF-W NOM-PROF-W.

           MOVE "\\" TO HORARI-PROF-W 

           INSPECT HORARI-PROF-W REPLACING FIRST "                    "
                                              BY IP-DATOS-USUNET

           INSPECT HORARI-PROF-W REPLACING FIRST " "
                                              BY "\"

           INSPECT HORARI-PROF-W REPLACING FIRST "                       "
                                              BY "PROG\DATOS\SC-HORAR.DAT".


           MOVE "\\" TO NOM-PROF-W 

           INSPECT NOM-PROF-W REPLACING FIRST "                    "
                                              BY IP-DATOS-USUNET

           INSPECT NOM-PROF-W REPLACING FIRST " "
                                           BY "\"
                      
           INSPECT NOM-PROF-W REPLACING FIRST "                              "
                                            BY DIR-CONTAB-LLEGADA-W.

           INSPECT NOM-PROF-W   REPLACING FIRST "                        "
                                             BY "\CONTROL\SC-ARCHPROF.DAT".


           INITIALIZE NOM-PLANO-W

           MOVE OPERADOR-LLEGADA-W TO OPER-JSON-W
           MOVE FUNCTION CURRENT-DATE TO FECHA-JSON-W
           ACCEPT HORA-JSON-W FROM TIME


           MOVE NOMBRE-JSON-W  TO NOM-PLANO-W.
           MOVE NOM-JSON-W     TO NOMBRE-JSON-ENV.

 
									

       ABRIR-PROFES.
           OPEN INPUT ARCHIVO-PROFESIONALES.                      

           MOVE COD-LLEGADA-W TO COD-PROF
           READ ARCHIVO-PROFESIONALES WITH NO LOCK
                INVALID KEY 
                  MOVE COD-PROF TO DESCRIP-PROF
           END-READ.
           CLOSE ARCHIVO-PROFESIONALES.	
	   
           OPEN INPUT ARCHIVO-HORARIO-PROF.

           MOVE "00" TO DAT0-ENV.

       ABRIR-JSON-SERV.	       
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-HOR-P.

           MOVE "{*AGENDA*:[" TO  DATOS-JSON
           INSPECT DATOS-JSON REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON END-WRITE
           MOVE 0 TO SW-FIN.

       LEER-HORARIO.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN                 
           ELSE
              MOVE LIN-1 TO DATOS-JSON
              WRITE REG-JSON END-WRITE
           END-IF

           READ ARCHIVO-HORARIO-PROF  NEXT WITH NO LOCK AT END 
                GO TO CERRAR-HORARIO
           END-READ.

           IF COD-HOR-P = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE             
              INITIALIZE LIN-1

              MOVE FECHA-HOR-P      TO FECHA-HRP-J
              MOVE OBSERV-HOR-P     TO OBSERV-HORA-HRP-J
      *        MOVE SUCUR-UNO-HOR-J  TO SUCUR-UNO-HRP-J      
              MOVE HORA-INI1-HOR-P  TO HORA-INIC1-HRP-J      
              MOVE HORA-FIN1-HOR-P  TO HORA-SALD1-HRP-J           
              MOVE HORA-INI2-HOR-P  TO HORA-INIC2-HRP-J      
              MOVE HORA-FIN2-HOR-P  TO HORA-SALD2-HRP-J
              MOVE INT-HOR1-P       TO FRECU-UNO-HRP-J 
              MOVE INT-HOR2-P       TO FRECU-DOS-HRP-J 
      *        MOVE SUCUR-UNO-HOR-J  TO SUCUR-UNO-HRP-J


              INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
              MOVE "," TO CIERRE-LIN-1           
              
           END-IF

           GO TO LEER-HORARIO.

       CERRAR-HORARIO.
           INITIALIZE LIN-1
           MOVE LIN-1 TO DATOS-JSON
           WRITE REG-JSON END-WRITE
      
           MOVE "]}" TO DATOS-JSON
           WRITE REG-JSON END-WRITE
                
           CLOSE ARCHIVO-HORARIO-PROF  
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
