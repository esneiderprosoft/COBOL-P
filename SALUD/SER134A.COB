      *============================================================
      * CREACION DE ARCHIVO JSON SER134A  SALUD
      *============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.             
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

            SELECT ARCHIVO-JSON
                   ASSIGN NOM-PLANO-W
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS OTR-STAT.
                                                          
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON      PIC X(1500).
 
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-PLANO-W               PIC x(90).

       01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).           
           02 DIR-CONTAB-LLEGADA-W      PIC X(8).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
        
       01 DATO-IMPORTAR                 PIC X(100). 
        
       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.


       01 LIN-1.  
          02 FILLER                   PIC X(10)  VALUE "{*CODIGO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CODIGO-PROGRAMA-J        PIC XX.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(14)  VALUE "*DESCRIPCION*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DESCRIPCION-PROGRAMA-J   PIC X(25).
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
              MOVE "SER134A"                TO MSJ3-HTML
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
              MOVE "SER134A"                TO MSJ3-HTML
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
              MOVE "SER134A"                TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO DATO-IMPORTAR.

           UNSTRING DATO-IMPORTAR DELIMITED BY "|" 
             INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W
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

           INITIALIZE NOM-PLANO-W
           MOVE "\\" TO NOM-PLANO-W 
      
           INSPECT NOM-PLANO-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET
           INSPECT NOM-PLANO-W  REPLACING FIRST "                                                                "
                                             BY "\PROG\DATOS\JSON\JSC-PROGRAMA-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"
           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                             BY LLAVESESION-LLEGA-W.


       ABRIR-JSON-PROGRAMA.
           OPEN OUTPUT ARCHIVO-JSON.

           MOVE "{*PROGRAMA*:[" TO  DATOS-JSON
           INSPECT DATOS-JSON REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON END-WRITE
           MOVE 0 TO SW-FIN.

       CREAR-PROGRAMA.

           MOVE "DI"                          TO CODIGO-PROGRAMA-J
           MOVE "DESAYUNO INFANTIL"           TO DESCRIPCION-PROGRAMA-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "RE"                          TO CODIGO-PROGRAMA-J
           MOVE "RESTAURANTE ESCOLAR"         TO DESCRIPCION-PROGRAMA-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "RN"                          TO CODIGO-PROGRAMA-J
           MOVE "RECUPERACION NUTRICIONAL"    TO DESCRIPCION-PROGRAMA-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "RF"                          TO CODIGO-PROGRAMA-J
           MOVE "REFRIGERIOS"                 TO DESCRIPCION-PROGRAMA-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "FA"                          TO CODIGO-PROGRAMA-J
           MOVE "FAMILIAS EN ACCION"          TO DESCRIPCION-PROGRAMA-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "HI"                          TO CODIGO-PROGRAMA-J
           MOVE "HOGAR INFANTIL"              TO DESCRIPCION-PROGRAMA-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "RU"                          TO CODIGO-PROGRAMA-J
           MOVE "RED UNIDOS"                  TO DESCRIPCION-PROGRAMA-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "NS"                          TO CODIGO-PROGRAMA-J
           MOVE "NO SABE"                     TO DESCRIPCION-PROGRAMA-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "NI"                          TO CODIGO-PROGRAMA-J
           MOVE "NINGUNO"                     TO DESCRIPCION-PROGRAMA-J
           PERFORM ESCRIBIR-ULTIMA-LINEA.


           GO TO CERRAR-PROGRAMA.
           
       ESCRIBIR-LINEA.
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE ","      TO CIERRE-LIN-1
           MOVE LIN-1    TO DATOS-JSON
           WRITE REG-JSON END-WRITE.

       ESCRIBIR-ULTIMA-LINEA.
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE SPACES      TO CIERRE-LIN-1
           MOVE LIN-1 TO DATOS-JSON
           WRITE REG-JSON END-WRITE.
                      
       CERRAR-PROGRAMA.
           MOVE "]}" TO DATOS-JSON
           WRITE REG-JSON END-WRITE
            
           CLOSE ARCHIVO-JSON.   

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

       COPY "..\..\FUENTES\SC-WEB19.CBL".



