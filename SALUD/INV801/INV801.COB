      *============================================================
      * CREACION DE ARCHIVO JSON PARA VENTANA CONSULTA DE LOCALIZACION
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
                  ASSIGN NOM-USUAR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-LOCALIZACION LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-LOCAL-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS COD-LOCAL
                  ALTERNATE RECORD KEY IS LLAVE-ALT1-LOCAL WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LLAVE-ALT2-LOCAL WITH DUPLICATES
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
       COPY "..\..\FUENTES\FD-LOCAL.CBL".


       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON      PIC X(500).

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LLEGADA-W                    PIC X(100).

       77 NOM-USUAR-W                  PIC X(70).
       77 NOM-LOCAL-W                  PIC X(70).
       77 NOM-PLANO-W                  PIC X(90).
       77 CONT                         PIC 9(4).
      
       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(21).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 COD-LOCAL-LLEGADA-W       PIC X(5).
          
       01 LIN-1.
          02 FILLER                  PIC X(7)   VALUE "{*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-LOC-J               PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-LOC-J           PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(11)  VALUE "*DESCRIP2*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP2-LOC-J          PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(7)   VALUE "*COST*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COSTO-LOC-J             PIC X(4).
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
              MOVE "INV801"                 TO MSJ3-HTML
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
              MOVE "INV801"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USUAR-W              TO MSJ2-HTML
              MOVE "INV801"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR    
           END-IF. 

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-LOCALIZACION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-LOCAL-W              TO MSJ2-HTML
              MOVE "INV801"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR    
           END-IF.   

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PLANO-W              TO MSJ2-HTML
              MOVE "SER813"                 TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, COD-LOCAL-LLEGADA-W   
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.

       INICIAR-SESION.
           MOVE "D:\PROGELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W 
         
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.
              
       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\PROGELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
     
           OPEN INPUT ARCHIVO-USUNET                         
      
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

           INITIALIZE NOM-LOCAL-W NOM-USUAR-W
           
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".
 
           MOVE "\\" TO NOM-LOCAL-W NOM-USUAR-W 

           INSPECT NOM-LOCAL-W REPLACING FIRST "                    "
                                           BY IP-DATOS-USUNET
           INSPECT NOM-LOCAL-W REPLACING FIRST " "
                                            BY "\"
           INSPECT NOM-LOCAL-W REPLACING FIRST "                     "
                                             BY DIR-CONTAB-LLEGADA-W.      
           MOVE NOM-LOCAL-W TO NOM-USUAR-W

           INSPECT NOM-LOCAL-W REPLACING FIRST "                     "
                                            BY "\CONTROL\SC-ALMAC.DAT".
           INSPECT NOM-USUAR-W REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHUSU.DAT".
                                                             
           INITIALIZE NOM-PLANO-W
           MOVE "\\" TO NOM-PLANO-W

           INSPECT NOM-PLANO-W REPLACING FIRST "                    "
                                         BY IP-DATOS-USUNET

           INSPECT NOM-PLANO-W REPLACING FIRST  "                 "
                                            BY  "\PROG\DATOS\JSON\" 
      
           INSPECT NOM-PLANO-W  REPLACING FIRST "                                              "
                                             BY "JSC-ARCHLOC-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"

           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                             BY LLAVE-SESION-LLEGA-W.  

       ABRIR-USUARIOS.

           OPEN INPUT ARCHIVO-USUARIOS
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.

       ABRIR-LOCAL.

           OPEN INPUT ARCHIVO-LOCALIZACION.
           
       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-LOCAL.

           MOVE "{*LOCAL*:[" TO  DATOS-JSON   
           INSPECT DATOS-JSON  REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON  END-WRITE
           MOVE 0 TO SW-FIN.

       LEER-LOCAL.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-JSON  
              WRITE REG-JSON  END-WRITE
           END-IF

           READ ARCHIVO-LOCALIZACION NEXT WITH NO LOCK AT END 
                GO TO CERRAR-LOCAL
           END-READ.
           
           IF COD-LOCAL = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE 
              IF COD-LOCAL = COD-LOC-J
                 MOVE 0 TO SW-FIN
              ELSE
                 ADD 1 TO CONT 
                 INITIALIZE LIN-1

                 MOVE COD-LOCAL  TO COD-LOC-J
             
                 INSPECT NOMBRE-LOCAL REPLACING ALL '"' BY " "
                 INSPECT NOMBRE-LOCAL REPLACING ALL "*" BY " "
                 INSPECT NOMBRE-LOCAL REPLACING ALL "Ñ" BY CARAC-ENE-MAY 
                 INSPECT NOMBRE-LOCAL REPLACING ALL "ñ" BY CARAC-ENE-MIN

                 INSPECT NOMBRE-RESP-LOCAL REPLACING ALL '"' BY " "
                 INSPECT NOMBRE-RESP-LOCAL REPLACING ALL "*" BY " "
                 INSPECT NOMBRE-RESP-LOCAL REPLACING ALL "Ñ" BY CARAC-ENE-MAY 
                 INSPECT NOMBRE-RESP-LOCAL REPLACING ALL "ñ" BY CARAC-ENE-MIN


                 MOVE NOMBRE-LOCAL        TO DESCRIP-LOC-J
                 MOVE NOMBRE-RESP-LOCAL   TO DESCRIP2-LOC-J  
                 MOVE C-COSTO-LOCAL       TO COSTO-LOC-J  
                            
                 INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
                 MOVE "," TO CIERRE-LIN-1 
              END-IF
           END-IF
           
           GO TO LEER-LOCAL.
                                    
        CERRAR-LOCAL.

           INITIALIZE LIN-1
           
           IF CONT = ZEROS 
              INITIALIZE COD-LOC-J
                         DESCRIP-LOC-J
                         DESCRIP2-LOC-J   
                         COSTO-LOC-J  
                       
              INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
           END-IF.

           MOVE LIN-1 TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
          
           MOVE "]}" TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
           
           CLOSE ARCHIVO-LOCALIZACION
                 ARCHIVO-JSON.      

       ENVIO-DATOS.
           MOVE "datosrecibidos"   TO COBW3-CNV-NAME
           MOVE "00"               TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3
         
           MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
           PERFORM ABRIR-HTML.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.            
      
       COPY "..\..\FUENTES\SC-WEB19.CBL".
      
