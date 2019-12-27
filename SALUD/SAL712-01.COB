      *============================================================
      * CREACION DE ARCHIVO JSON SAL712-01  SALUD
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


       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-NOMTR.CBL".


      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".

       01 TABLA-LLEGADA-W.
          02 DATO-LLEGADA-W PIC X OCCURS 200.
       
       01 LLEGADA-W         PIC X(100).

       01 TABLA-TEMP.
          02 CARAC-TEMP     PIC X OCCURS 50.

       77 DATOS-PLANO-W             PIC X(200).
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
           02 COD-NOMTR-LLEGADA-W       PIC X(2).


       01 LIN-1.  
          02 FILLER                   PIC X(7)   VALUE "{*COD*:".
          02 FILLER                   PIC X      VALUE "*".
          02 COD-NOMTR-J              PIC X(2).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)   VALUE "*DESCRIP*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DESCRIP-NOMTR-J          PIC X(25).          
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
              MOVE "SAL712-01"              TO MSJ3-HTML
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
              MOVE "SAL712-01"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-NOMBRE-TAR.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE                    
           ELSE
              MOVE OTR-STAT                       TO MSJ1-HTML
              MOVE "SAL712-01-ARCHIVO-NOMBRE-TAR" TO MSJ2-HTML
              MOVE NOM-GRUTAR-W                   TO MSJ3-HTML
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
                                             BY   "\CONTROL\SC-GRUPOTAR.DAT".


       ABRIR-NOMBRE-TAR.
           OPEN INPUT ARCHIVO-NOMBRE-TAR.


       LEER-NOMTR.
           INITIALIZE REG-NOMTR
           MOVE "{*NOMTAR*:[" TO  DATOS-PLANO-W   
           INSPECT DATOS-PLANO-W  REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM ENVIO-DATOS
           MOVE 0 TO SW-FIN.

       LEER-NOMTAR.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-PLANO-W  
              INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
              INSPECT DATOS-PLANO-W REPLACING ALL LOW-VALUES BY SPACES
              PERFORM ENVIO-DATOS
           END-IF

           READ ARCHIVO-NOMBRE-TAR NEXT WITH NO LOCK AT END 
                GO TO CERRAR-GRUPO-SER
           END-READ
                    
           IF CODIGO-NOMTR = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE
              MOVE CODIGO-NOMTR   TO COD-NOMTR-J
              MOVE DESCRIP-NOMTR  TO DESCRIP-NOMTR-J

              INSPECT DESCRIP-NOMTR-J REPLACING ALL "*" BY "x"
              INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
              INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES
              MOVE "," TO CIERRE-LIN-1           
           END-IF

           GO TO LEER-NOMTAR.

       CERRAR-GRUPO-SER.
           INITIALIZE LIN-1	  
           MOVE LIN-1 TO DATOS-PLANO-W 
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES

           PERFORM ENVIO-DATOS
          
           MOVE "]}" TO DATOS-PLANO-W  
           PERFORM ENVIO-DATOS
                
           CLOSE ARCHIVO-NOMBRE-TAR.

       PAGINA-CONFIG.  
           MOVE "statuscode"     TO COBW3-CNV-NAME
           MOVE "00"             TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"  USING COBW3

           MOVE "programa-id"    TO COBW3-CNV-NAME
           MOVE "SAL712"         TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"  USING COBW3

           MOVE "..\PAGINAS\RECIBIDOSLISTADO.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML.


       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.


       ENVIO-DATOS.
           MOVE "datosrecibidos"   TO COBW3-CNV-NAME
           MOVE DATOS-PLANO-W      TO COBW3-CNV-VALUE
           CALL "COBW3_SET_REPEAT" USING COBW3. 
      
      
       COPY "..\..\FUENTES\SC-WEB19.CBL".         


