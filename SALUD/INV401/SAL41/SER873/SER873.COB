      *============================================================
      * CREACION DE ARCHIVO JSON SER873  SALUD
      *============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

           SELECT ARCHIVO-UNID-SERV   LOCK MODE IS AUTOMATIC
                  ASSIGN UNID-SERV-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CODIGO-UNID-SERV
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-JSON
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.
                                                          
000390
000400 DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-UNSER.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON                PIC X(1000).
000470
000540 WORKING-STORAGE  SECTION.
000550 COPY "..\..\FUENTES\COBW3.CBL".
000560 COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-PLANO-W                   PIC x(90).
       77 UNID-SERV-W                   PIC X(60).
       77 MES-SAL-W                     PIC X(4).

       01  CUOTAS-MOD-W.
           02 VLR-MOD-W                 PIC 9(9) OCCURS 3.

       01  VARIABLES.
           02  VALOR-APROX              PIC S9(9).

005270 01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 DIR-CONTAB-LLEGADA-W      PIC X(30).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 OPERADOR-LLEGADA-W        PIC X(4).
        
       01 DATO-IMPORTAR-WK              PIC X(100). 

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV             PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 NOMBRE-JSON-ENV            PIC X(28).

001812 01 LIN-1.  
001820    02 FILLER                   PIC X(10)  VALUE "{*codigo*:".
001830    02 FILLER                   PIC X      VALUE "*".
001840    02 COD-UNIATEN-J            PIC 99.
001850    02 FILLER                   PIC XX     VALUE "*,".
001860    02 FILLER                   PIC X(14)  VALUE "*descripcion*:".
001870    02 FILLER                   PIC X      VALUE "*".
001880    02 DESCRIP-UNIATEN-J        PIC X(30).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(14)  VALUE "*UNIDEDADMIN*:".
          02 FILLER                   PIC X      VALUE "*".
          02 UNIDEDADMIN-UNIATEN-J    PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)  VALUE "*EDADMIN*:".
          02 FILLER                   PIC X      VALUE "*".
          02 EDADMIN-UNIATEN-J        PIC 9(3).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(14)  VALUE "*UNIDEDADMAX*:".
          02 FILLER                   PIC X      VALUE "*".
          02 UNIDEDADMAX-UNIATEN-J    PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)  VALUE "*EDADMAX*:".
          02 FILLER                   PIC X      VALUE "*".
          02 EDADMAX-UNIATEN-J        PIC 9(3).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*CENCOS*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CENCOS-UNIATEN-J         PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*estado*:".
001830    02 FILLER                   PIC X      VALUE "*".
001840    02 ACTIVAR-UNIATEN-J        PIC X(8).

001970    02 FILLER                   PIC XX     VALUE "*}".    
001980    02 CIERRE-LIN-1             PIC X.

       01 LIN-2.  
001820    02 FILLER                   PIC X(7)   VALUE "{*COD*:".
001830    02 FILLER                   PIC X      VALUE "*".
001840    02 COD-COSTO-UNIATEN-J      PIC X(4).
001850    02 FILLER                   PIC XX     VALUE "*,".
001860    02 FILLER                   PIC X(9)  VALUE "*NOMBRE*:".
001870    02 FILLER                   PIC X      VALUE "*".
001880    02 NOMBRE-COSTO-UNIATEN-J   PIC X(20).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DESCRIP-COSTO-UNIATEN-J  PIC X(30).
        
001970    02 FILLER                   PIC XX     VALUE "*}".    
001980    02 CIERRE-LIN-2             PIC X.
    
       01 LIN-3.
          02 FILLER                   PIC X(10)  VALUE "{*NOMBRE*:".
          02 FILLER                   PIC X      VALUE "*".
          02 NOMBRE-USU-UNIATEN-J     PIC X(50).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*NOMBREA*:".
          02 FILLER                   PIC X      VALUE "*".   
          02 NIT-USU-UNIATEN-J        PIC 9(10).
          
          02 FILLER                   PIC XX     VALUE "*}".
          02 CIERRE-LIN-3             PIC X.

       01 INVALID-W                   PIC X(80).

000791 LINKAGE SECTION.
000792 COPY "..\..\FUENTES\ISAPICTX.CBL".

       01 DATOS-MENU.
          02 DATOS-W.
             05 ADMIN1-W              PIC X(3).
             05 ADMIN2-W              PIC X.   
000793                                       
000794 PROCEDURE DIVISION WITH stdcall LINKAGE USING ISAPI-CTX-CNT.
000795
000810 DECLARATIVES.
000811 I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUNET.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USU-W                TO MSJ2-HTML
              MOVE "SER873"                 TO MSJ3-HTML
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
              MOVE "SER873"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-COSTO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE COSTO-W              TO MSJ2-HTML
              MOVE "SER873"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-UNID-SERV.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE UNID-SERV-W          TO MSJ2-HTML
              MOVE "SER873"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE USUAR-W              TO MSJ2-HTML
              MOVE "SER873"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "SER873"                 TO MSJ2-HTML
              MOVE NOM-PLANO-W              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

001150 END DECLARATIVES.
001160
001161 INICIAR-IIS.
001162     MOVE LOW-VALUE TO COBW3.
001163     MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
001164     CALL "COBW3_INIT" USING COBW3.
001220
       LEER-DATO-HTML.
000788     MOVE "datosh" TO COBW3-SEARCH-DATA.
000790     CALL "COBW3_GET_VALUE" USING COBW3.
000791     MOVE COBW3-GET-DATA TO DATO-IMPORTAR-WK.

           UNSTRING DATO-IMPORTAR-WK DELIMITED BY "|" 
             INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W
           END-UNSTRING.

           MOVE LLAVESESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
000830     ACCEPT HORA-TOTAL FROM TIME.

001238 INICIAR-SESION.
001245     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W

           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.
       
001303 ABRIR-USUARIO.
           INITIALIZE OTR-STAT

001304     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
001305
001306     OPEN INPUT ARCHIVO-USUNET
001307     EVALUATE OTR-STAT
001308       WHEN "00"  CONTINUE
001309       WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
001310                  CLOSE       ARCHIVO-USUNET
001311                  OPEN I-O    ARCHIVO-USUNET
001312                  INITIALIZE REG-USUNET
001313                  MOVE "." TO NOMBRE-USUNET
001314                  WRITE REG-USUNET
001315                  CLOSE       ARCHIVO-USUNET
001316                  MOVE "Validacion de usuarios"      TO MSJ1-HTML
001317                  MOVE "Falta configurar usuario"    TO MSJ2-HTML
001318                  MOVE "Sc"                          TO MSJ3-HTML
001319                  GO TO ENVIAR2-ERROR
001320       WHEN OTHER GO TO CERRAR-SESION
001321     END-EVALUATE.                          
001322
001332     INITIALIZE LLAVE-USUNET.

001334 LEER-USUARIO.
001335     READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
001345     CLOSE ARCHIVO-USUNET.
001336
001337     IF NOMBRE-USUNET = SPACES
001339        MOVE "Validacion de usuarios"      TO MSJ1-HTML
001340        MOVE "Falta configurar usuario"    TO MSJ2-HTML
001341        MOVE "Sc"                          TO MSJ3-HTML
001342        GO TO ENVIAR2-ERROR
001343     END-IF.

       ASIGNAR-NOMBRES.
           INITIALIZE UNID-SERV-W
                      COSTO-W
                      USUAR-W.

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

           INSPECT IP-SERVER-W REPLACING FIRST "/" BY "\"
001614     INITIALIZE UNID-SERV-W
           MOVE "\\" TO UNID-SERV-W
001617     INSPECT UNID-SERV-W REPLACING FIRST "                    "
                                           BY  IP-DATOS-USUNET

001621     INSPECT UNID-SERV-W REPLACING FIRST " "
001624                                      BY "\"

007190     INSPECT UNID-SERV-W REPLACING FIRST "                              "
                                            BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT UNID-SERV-W REPLACING FIRST "                     "
007200                                      BY "\CONTROL\SC-UNSER.DAT".


007210     INITIALIZE NOM-PLANO-W

           MOVE OPERADOR-LLEGADA-W TO OPER-JSON-W
           MOVE FUNCTION CURRENT-DATE TO FECHA-JSON-W
           ACCEPT HORA-JSON-W FROM TIME


           MOVE NOMBRE-JSON-W  TO NOM-PLANO-W.
           MOVE NOM-JSON-W     TO NOMBRE-JSON-ENV.
           
           MOVE "00"          TO SW-INVALID-ENV.

       ABRIR-ARCHIVOS.
           OPEN INPUT ARCHIVO-UNID-SERV.

       ABRIR-JSON-UNIDAD.
           OPEN OUTPUT ARCHIVO-JSON.

           MOVE "{*UNIDSERV*:[" TO  DATOS-JSON
           INSPECT DATOS-JSON REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON END-WRITE
           MOVE 0 TO SW-FIN.
        
       ESCRIBIR-UNIDAD.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-JSON
              WRITE REG-JSON END-WRITE
           END-IF

           READ ARCHIVO-UNID-SERV NEXT WITH NO LOCK AT END 
                GO TO CERRAR-GRUPO-SERV
           END-READ

           IF CODIGO-UNID-SERV = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE
              MOVE CODIGO-UNID-SERV   TO COD-UNIATEN-J 
              MOVE DESCRIP-UNID-SERV  TO DESCRIP-UNIATEN-J
              MOVE UND-EDAD-MIN-SERV  TO UNIDEDADMIN-UNIATEN-J
              MOVE VLR-EDAD-MIN-SERV  TO EDADMIN-UNIATEN-J
              MOVE UND-EDAD-MAX-SERV  TO UNIDEDADMAX-UNIATEN-J
              MOVE VLR-EDAD-MAX-SERV  TO EDADMAX-UNIATEN-J
              MOVE C-COSTO-SERV       TO CENCOS-UNIATEN-J
              IF ACTIVAR-SERV = "S"
                 MOVE "ACTIVA" TO ACTIVAR-UNIATEN-J
              ELSE 
                 MOVE "INACTIVA" TO ACTIVAR-UNIATEN-J
              END-IF

              INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
              MOVE "," TO CIERRE-LIN-1
           END-IF

           GO TO ESCRIBIR-UNIDAD.

       CERRAR-GRUPO-SERV.
           INITIALIZE LIN-1
           MOVE LIN-1 TO DATOS-JSON
           WRITE REG-JSON END-WRITE

008060     MOVE "]}" TO DATOS-JSON
008070     WRITE REG-JSON END-WRITE
008080          
008090     CLOSE ARCHIVO-UNID-SERV
                 ARCHIVO-JSON.

002147 ENVIO-DATOS.
           MOVE "datosrecibidos"  TO COBW3-CNV-NAME
004393     MOVE DATOS-ENVIO       TO COBW3-CNV-VALUE
004403     CALL "COBW3_SET_CNV"   USING COBW3

004413     MOVE "..\..\FRAMEWORKS\PAGINAS\RECIBIDOS.ASPX"  TO SALIDA-HTML
004430     PERFORM ABRIR-HTML.
002544
002545 CERRAR-SESION.
002546     CALL "COBW3_FREE" USING COBW3.
002547     MOVE 1 TO PROGRAM-STATUS.
002548     EXIT PROGRAM.

       COPY "..\..\FUENTES\SC-WEB19.CBL".
