      *=================================================================
      *CONSULTA ARCHIVO LOCALIZACION
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
       

           SELECT ARCHIVO-LOCALIZACION LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-LOCAL-W
      * "..\control\SC-ALMAC.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS COD-LOCAL
                  ALTERNATE RECORD KEY IS LLAVE-ALT1-LOCAL WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LLAVE-ALT2-LOCAL WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.
 
              
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-LOCAL.CBL".
      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".
  
       77 NOM-LOCAL-W                          PIC X(70).
       01 LINEA-LLEGADA-W                      PIC X(500).
       
                                        
       01  DATO-LLEGADA-W.
           02 LLAVE-SESION-LLEGA-W.
              03 ID-LLEGAD-W                   PIC X(15).
              03 FECHA-LLEGAD-W                PIC X(8).
              03 HORA-LLEGAD-W                 PIC X(6).
           02 DIR-CONTAB-LLEGADA-W             PIC X(30).
           02 MES-CONTAB-LLEGADA-W             PIC XX.
           02 COD-LOCAL-LLEGADA-W.
              03 LLAVE3-LOC-W.
                 05 LLAVE1-LOC-W.
                    07 COD1-LOCAL-W            PIC X.
                    07 COD2-LOCAL-W            PIC X.
                 05 COD3-LOCAL-W               PIC X.
              03 COD4-LOCAL-W                  PIC XX.    
           02 OPERADOR-LLEGADA-W               PIC X(4).
                                       
       01 DATOS-ENVIO.
          02 SW-INVALID-ENV               PIC XX.
          02 FILLER                       PIC X      VALUE "|".
          02 NOMBRE-LOCAL-ENV             PIC X(30).


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
              MOVE "INV103-21"              TO MSJ3-HTML
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
              MOVE "INV103-21"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-LOCALIZACION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-LOCAL-W              TO MSJ2-HTML
              MOVE "INV103-21"              TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO LINEA-LLEGADA-W.

           UNSTRING LINEA-LLEGADA-W DELIMITED BY "|"
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, COD-LOCAL-LLEGADA-W,
                   OPERADOR-LLEGADA-W

           END-UNSTRING.

           MOVE LINEA-LLEGADA-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.
           MOVE FUNCTION CURRENT-DATE TO FECHA-JSON-W.
           ACCEPT HORA-JSON-W FROM TIME.
           MOVE OPERADOR-LLEGADA-W TO OPER-JSON-W.


       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W

      *     MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.

       ABRIR-USUARIO.
           INITIALIZE OTR-STAT

           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W

      *     MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
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
              MOVE "Validacion de usuarios"                TO MSJ1-HTML
              MOVE "Falta configurar usuario"              TO MSJ2-HTML
              MOVE "Sc"                                    TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.

           INITIALIZE NOM-LOCAL-W 
           
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           MOVE "\\" TO NOM-LOCAL-W 


           INSPECT NOM-LOCAL-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET

           INSPECT NOM-LOCAL-W REPLACING FIRST " "
                                          BY "\"

           INSPECT NOM-LOCAL-W REPLACING FIRST "                              "
                                          BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT NOM-LOCAL-W REPLACING FIRST "                     "
                                            BY "\CONTROL\SC-ALMAC.DAT".
                                                    
       ABRIR-ARCHIVO-LOCALIZACION.
           OPEN INPUT ARCHIVO-LOCALIZACION.                   
           MOVE "00"  TO SW-INVALID-ENV.

       LEER-ARCHIVO-LOCALIZACION.
           MOVE COD-LOCAL-LLEGADA-W   TO COD-LOCAL
           READ ARCHIVO-LOCALIZACION WITH NO LOCK
                INVALID KEY 
                  MOVE "01" TO SW-INVALID-ENV               
           END-READ.

           MOVE NOMBRE-LOCAL TO NOMBRE-LOCAL-ENV.

       CERRAR-ARCHIVO-LOCALIZACION.            
         
           CLOSE ARCHIVO-LOCALIZACION.
               
       PAGINA-CONFIG.

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