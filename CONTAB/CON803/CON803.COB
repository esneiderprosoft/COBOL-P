      *============================================================
      * CREACION DE ARCHIVO JSON CON803 F8 DE CENTROS DE COSTO
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".  


     
           SELECT ARCHIVO-COSTO LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-ARCH-COSTO-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS COD-COSTO
                  FILE STATUS IS OTR-STAT.           

           SELECT ARCHIVO-JSON LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.
                                                          
      
       DATA DIVISION.
       FILE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-COSTO.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON      PIC X(500).

      
       WORKING-STORAGE  SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".

       77 K                 PIC 9(3).
       01 TABLA-LLEGADA-W.
          02 DATO-LLEGADA-W PIC X OCCURS 200.
       
       01 LLEGADA-W         PIC X(100).

       01 TABLA-TEMP.
          02 CARAC-TEMP     PIC X OCCURS 50.


       77 NOM-PLANO-W               PIC x(90).
       77 NOM-ARCH-COSTO-W              PIC X(60).
       77 MES-SAL-W                 PIC X(4).
       77 CONT                      PIC 9999.
       
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
          02 FILLER                  PIC X(7)   VALUE "{*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-COST-J              PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*NOMBRE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOMBRE-COST-J           PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-COST-J          PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*CUENTA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CODIGO-COST-J           PIC X(11).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*TABLA_COST*:[".             
          02 TABLA-COST-J  OCCURS 7.         
             05 FILLER               PIC X      VALUE "*".
             05 MAY-COST-J           PIC X(4).
             05 FILLER               PIC X      VALUE "*".
             05 COMA-COST-J          PIC X.
          02 FILLER                  PIC X      VALUE "]".
          02 FILLER                  PIC X      VALUE ",".
          02 FILLER                  PIC X(11)  VALUE "*DIVISION*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIV-COST-J              PIC X(2).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*OPER_CREA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OPER-ELAB-J             PIC X(4).          
          02 FILLER                  PIC XX      VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*FECHA_CREA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-ELAB-J            PIC X(6).          
          02 FILLER                  PIC XX      VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*OPER_MODI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OPER-MODI-J             PIC X(4).          
          02 FILLER                  PIC XX      VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*FECHA_MODI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-MODI-J            PIC X(6).                   
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
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "ARCHIVO-USUNET"         TO MSJ2-HTML
              MOVE "CON803"                 TO MSJ3-HTML
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
              MOVE "CON803"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-COSTO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE "CON803-ARCHIVO-COSTO"     TO MSJ2-HTML
              MOVE NOM-ARCH-COSTO-W           TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "CON803-ARCHIVO-JSON"    TO MSJ2-HTML
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
              GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.
           INITIALIZE NOM-ARCH-COSTO-W.

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
       
           INITIALIZE NOM-ARCH-COSTO-W
           MOVE "\\" TO NOM-ARCH-COSTO-W

           INSPECT NOM-ARCH-COSTO-W REPLACING FIRST "                  "
                                          BY IP-SERVER-W

           INSPECT NOM-ARCH-COSTO-W REPLACING FIRST "     "
                                              BY    "\PSC$"

           INSPECT NOM-ARCH-COSTO-W REPLACING FIRST " "
                                              BY    "\"
       
           INSPECT NOM-ARCH-COSTO-W   REPLACING FIRST "        "
                                                BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT NOM-ARCH-COSTO-W   REPLACING FIRST "                       "
                                                BY    "\CONTROL\SC-ARCHCOS.DAT".
                                                            

           INITIALIZE NOM-PLANO-W
           MOVE "\\" TO NOM-PLANO-W

           INSPECT NOM-PLANO-W REPLACING FIRST "                  "
                                          BY IP-SERVER-W

           INSPECT NOM-PLANO-W REPLACING FIRST  "                "
                                          BY    "\PSCELECT$\TEMP\"

      
           INSPECT NOM-PLANO-W  REPLACING FIRST "                                                "
                                             BY "JSC-ARCHCOSTO-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"

           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                             BY LLAVESESION-LLEGA-W.  


       ABRIR-NOMBRE-COSTO.
           OPEN INPUT ARCHIVO-COSTO.

           
           EVALUATE OTR-STAT
             WHEN "00" CONTINUE
             WHEN "35" OPEN OUTPUT ARCHIVO-COSTO
                       CLOSE       ARCHIVO-COSTO
                       GO TO ABRIR-NOMBRE-COSTO
           END-EVALUATE.
 
       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-COSTO.

           MOVE "{*CODIGOS*:[" TO  DATOS-JSON   
           INSPECT DATOS-JSON  REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON  END-WRITE
           MOVE 0 TO SW-FIN.

       LEER-NOMBRE-COSTO.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-JSON  
              WRITE REG-JSON  END-WRITE
           END-IF


           READ ARCHIVO-COSTO NEXT WITH NO LOCK AT END 
                GO TO CERRAR-NOMBRE-COSTO
           END-READ.
           
           IF COD-COSTO = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE
             INITIALIZE LIN-1
             MOVE COD-COSTO     TO COD-COST-J
             MOVE NOMBRE-COSTO  TO NOMBRE-COST-J

             INSPECT DESCRIP-COSTO REPLACING ALL '"' BY " "
             INSPECT DESCRIP-COSTO REPLACING ALL "*" BY " "


             MOVE DESCRIP-COSTO   TO DESCRIP-COST-J
             MOVE CODIGO-COSTO    TO CODIGO-COST-J
                                                                       
             PERFORM LEER-MAY-COST VARYING I FROM 1 BY 1 
                                             UNTIL I > 7

             MOVE SPACES TO COMA-COST-J (7)

             MOVE DIV-COSTO         TO DIV-COST-J

             IF OPER-CREA-COSTO IS NOT = SPACES OR LOW-VALUES
                MOVE OPER-CREA-COSTO   TO OPER-ELAB-J
             END-IF

             IF FECHA-CREA-COSTO IS NOT = SPACES OR LOW-VALUES
                MOVE FECHA-CREA-COSTO  TO FECHA-ELAB-J
             END-IF

             IF OPER-MODI-COSTO IS NOT = SPACES OR LOW-VALUES
                MOVE OPER-MODI-COSTO   TO OPER-MODI-J
             END-IF

             IF FECHA-MODI-COSTO IS NOT = SPACES OR LOW-VALUES
                MOVE FECHA-MODI-COSTO  TO FECHA-MODI-J
             END-IF
                                                                 
             INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
             MOVE "," TO CIERRE-LIN-1          
           END-IF
         

           GO TO LEER-NOMBRE-COSTO.

        CERRAR-NOMBRE-COSTO.
           INITIALIZE LIN-1
           PERFORM LEER-COMAS-COST VARYING I FROM 1 BY 1 
                                         UNTIL I > 7

           MOVE SPACES TO COMA-COST-J (7)

           MOVE LIN-1 TO DATOS-JSON
           WRITE REG-JSON END-WRITE
      
           MOVE "]}" TO DATOS-JSON
           WRITE REG-JSON END-WRITE
           
           CLOSE ARCHIVO-COSTO
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

       LEER-MAY-COST.
           IF MAY-COSTO (I) IS NOT = SPACES OR LOW-VALUES
              PERFORM LLENAR-MAY-COST              
           END-IF.
         
       LLENAR-MAY-COST.  
           MOVE MAY-COSTO (I) TO MAY-COST-J (I).
           MOVE ","           TO COMA-COST-J (I).

       LEER-COMAS-COST.
           MOVE ","           TO COMA-COST-J (I).
      
      
       COPY "P:\PROG\PROYECT\FUENTES\SC-WEB19.CBL".
      


