      *============================================================
      * CREACION DE ARCHIVO JSON SER106-01  SALUD
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  
                      

           SELECT ARCHIVO-NOMBRE-TAR LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-GRUTAR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CODIGO-NOMTR
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-JSON LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.
                                                          
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-NOMTR.CBL".

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
       77 NOM-GRUTAR-W              PIC X(60).
       77 MES-SAL-W                 PIC X(4).
       77 CONT                      PIC 9999.
       
       01  VAR-LLEGADA-W.
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
          02 FILLER                  PIC X(7)   VALUE "{*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-SERV-J              PIC X(2).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)   VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-SERV-J          PIC X(25).
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
              MOVE "ARCHIVO-USUNET"         TO MSJ2-HTML
              MOVE "SER106"                 TO MSJ3-HTML
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
              MOVE "SER106"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-NOMBRE-TAR.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE                    
           ELSE
              MOVE OTR-STAT                       TO MSJ1-HTML
              MOVE "SER106-01-ARCHIVO-NOMBRE-TAR" TO MSJ2-HTML
              MOVE NOM-GRUTAR-W                   TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.                                                                                           

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "SER106-ARCHIVO-JSON"    TO MSJ2-HTML
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

           MOVE LLAVESESION-LLEGA-W    TO LLAVE-SESION-W.
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
           INITIALIZE NOM-GRUTAR-W
           
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\". 

           MOVE "\\" TO NOM-GRUTAR-W

           INSPECT NOM-GRUTAR-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

           INSPECT NOM-GRUTAR-W REPLACING FIRST " "
                                          BY    "\"
       
           INSPECT NOM-GRUTAR-W   REPLACING FIRST "                              "
                                               BY DIR-CONTAB-LLEGADA-W.

           INSPECT NOM-GRUTAR-W   REPLACING FIRST "                        "
                                             BY   "\CONTROL\SC-GRUPOTAR.DAT"
                                                            
 

           INITIALIZE NOM-PLANO-W

           MOVE "\\" TO NOM-PLANO-W

           INSPECT NOM-PLANO-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-PLANO-W REPLACING FIRST  "                 "
                                            BY  "\PROG\DATOS\JSON\"  

           INSPECT NOM-PLANO-W  REPLACING FIRST "                          "
                                             BY "XXXXXXXXXXXXXXXXXXXXX.JSON"

           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXX"
                                             BY NOMBRE-JSON-W. 

           MOVE NOMBRE-JSON-W TO NOMBRE-ENV.  


       ABRIR-NOMBRE-TAR.
           OPEN INPUT ARCHIVO-NOMBRE-TAR.
           MOVE "00" TO DAT0-ENV.
 
       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-NOMTR.

           MOVE "{*CODIGOS*:[" TO  DATOS-JSON   
           INSPECT DATOS-JSON  REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON  END-WRITE
           MOVE 0 TO SW-FIN.

       LEER-NOMBRE-TAR.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-JSON  
              WRITE REG-JSON  END-WRITE
           END-IF


           READ ARCHIVO-NOMBRE-TAR NEXT WITH NO LOCK AT END 
                GO TO CERRAR-NOMBRE-TAR
           END-READ.
           
           IF CODIGO-NOMTR = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE
              IF CODIGO-NOMTR = COD-SERV-J
                 MOVE 0 TO SW-FIN
              ELSE
                 INITIALIZE LIN-1
                 MOVE CODIGO-NOMTR  TO COD-SERV-J
                 MOVE DESCRIP-NOMTR TO DESCRIP-SERV-J
                 INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
                 MOVE "," TO CIERRE-LIN-1          
              END-IF
           END-IF

           GO TO LEER-NOMBRE-TAR.


       CERRAR-NOMBRE-TAR.
           INITIALIZE LIN-1
           MOVE LIN-1 TO DATOS-JSON
           WRITE REG-JSON END-WRITE
         
           MOVE "]}" TO DATOS-JSON
           WRITE REG-JSON END-WRITE           
        
           CLOSE ARCHIVO-NOMBRE-TAR  
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