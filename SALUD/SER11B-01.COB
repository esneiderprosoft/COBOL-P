      *============================================================
      * CREACION DE ARCHIVO JSON SER11B-01  SALUD
      *============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  



      * "../control/SC-ARCHCOS.DAT"
           SELECT ARCHIVO-COSTO LOCK MODE IS AUTOMATIC
                  ASSIGN COSTO-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS COD-COSTO
                  FILE STATUS IS OTR-STAT.
           
           SELECT ARCHIVO-UNID-SERV   LOCK MODE IS AUTOMATIC
                  ASSIGN UNID-SERV-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CODIGO-UNID-SERV
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN TO USUAR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
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
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-UNSER.CBL".
       COPY "..\..\FUENTES\FD-COSTO.CBL".



       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON      PIC X(500).
000470
000540 WORKING-STORAGE  SECTION.
000550 COPY "..\..\FUENTES\COBW3.CBL".
000560 COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-PLANO-W               PIC x(90).
       77 COSTO-W                   PIC X(60).
       77 UNID-SERV-W               PIC X(60).
       77 USUAR-W                   PIC X(60).
       77 MES-SAL-W                 PIC X(4).

005270 01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 IP-SERVER-W               PIC X(18).
           02 DIR-CONTAB-LLEGADA-W      PIC X(8).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
        
       01 DATO-IMPORTAR-WK              PIC X(100). 
        
       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.


001812 01 LIN-1.  
001820    02 FILLER                   PIC X(7)   VALUE "{*COD*:".
001830    02 FILLER                   PIC X      VALUE "*".
001840    02 COD-UNIATEN-J            PIC 99.
001850    02 FILLER                   PIC XX     VALUE "*,".
001860    02 FILLER                   PIC X(10)  VALUE "*DESCRIP*:".
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
          02 FILLER                   PIC X(9)  VALUE "*ESTADO*:".
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
000790
000791 LINKAGE SECTION.
000792 COPY "..\..\FUENTES\ISAPICTX.CBL".
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
              MOVE "SER11B"                 TO MSJ3-HTML
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
              MOVE "SER11B"             TO MSJ3-HTML
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
              MOVE "SER11B"             TO MSJ3-HTML
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
              MOVE "SER11B"             TO MSJ3-HTML
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
              MOVE "SER11B"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "SER11B-ARCHIVO-JSON"    TO MSJ2-HTML
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
             INTO LLAVESESION-LLEGA-W, IP-SERVER-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W
           END-UNSTRING.

           MOVE LLAVESESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
000830     ACCEPT HORA-TOTAL FROM TIME.

001238 INICIAR-SESION.
001245     MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W

           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.
       
001303 ABRIR-USUARIO.
           INITIALIZE OTR-STAT

001304     MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
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

      *     INSPECT IP-SERVER-W REPLACING FIRST "/" BY "\"
001614     INITIALIZE UNID-SERV-W
           MOVE "\\" TO UNID-SERV-W
001617     INSPECT UNID-SERV-W REPLACING FIRST "                  "
                                          BY IP-SERVER-W

           INSPECT UNID-SERV-W REPLACING FIRST "     "
                                     BY    "\PSC$"

001621     INSPECT UNID-SERV-W REPLACING FIRST " "
001624                                    BY "\"

007190     INSPECT UNID-SERV-W REPLACING FIRST "        "
                                            BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT UNID-SERV-W REPLACING FIRST "                     "
007200                                      BY "\CONTROL\SC-UNSER.DAT".

           
           MOVE UNID-SERV-W TO COSTO-W
           INSPECT COSTO-W REPLACING FIRST "SC-UNSER.DAT  "
007200                                  BY "SC-ARCHCOS.DAT".

           MOVE UNID-SERV-W TO USUAR-W.
           INSPECT USUAR-W REPLACING FIRST "SC-UNSER.DAT  "
007200                                  BY "SC-ARCHUSU.DAT".


007210     INITIALIZE NOM-PLANO-W
           MOVE "\\" TO NOM-PLANO-W 
007220     INSPECT NOM-PLANO-W  REPLACING FIRST "                  "
007230                                       BY IP-SERVER-W
      
           INSPECT NOM-PLANO-W REPLACING FIRST  "                "
                                          BY    "\PSCELECT$\TEMP\"
      
007260     INSPECT NOM-PLANO-W  REPLACING FIRST "                                              "
007280                                       BY "JSC-UNIATEN-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"

007281     INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
007282                                       BY LLAVESESION-LLEGA-W.  


       ABRIR-ARCHIVOS.

           OPEN INPUT ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT AT END MOVE 0 TO OPCION-1.
           CLOSE ARCHIVO-USUARIOS.

           OPEN INPUT  ARCHIVO-UNID-SERV.
           OPEN INPUT  ARCHIVO-COSTO.

           EVALUATE OTR-STAT
001308       WHEN "00"  GO TO ABRIR-JSON-SERV 
001309       WHEN "35"  OPEN OUTPUT ARCHIVO-UNID-SERV
                        PERFORM CREAR-UNIDADES
                        CLOSE       ARCHIVO-UNID-SERV
                        GO TO ABRIR-ARCHIVOS
001316       WHEN OTHER GO TO CERRAR-SESION
001321     END-EVALUATE.  

       CREAR-UNIDADES.
           INITIALIZE REG-UNID-SERV.

           MOVE 01 TO CODIGO-UNID-SERV.
           MOVE "URGENCIAS             " TO DESCRIP-UNID-SERV
           MOVE "N" TO ACTIVAR-SERV
           WRITE REG-UNID-SERV END-WRITE
           MOVE 02 TO CODIGO-UNID-SERV.
           MOVE "CONSULTA EXTERNA      " TO DESCRIP-UNID-SERV
           WRITE REG-UNID-SERV END-WRITE
           MOVE 03 TO CODIGO-UNID-SERV.
           MOVE "HOSPITALIZACION       " TO DESCRIP-UNID-SERV
           WRITE REG-UNID-SERV END-WRITE
           MOVE 04 TO CODIGO-UNID-SERV.
           MOVE "CIRUGIA               " TO DESCRIP-UNID-SERV
           WRITE REG-UNID-SERV END-WRITE
           MOVE 05 TO CODIGO-UNID-SERV.
           MOVE "OBSERVACION           " TO DESCRIP-UNID-SERV
           WRITE REG-UNID-SERV END-WRITE
           MOVE 06 TO CODIGO-UNID-SERV.
           MOVE "ACTIV. EXTRAMULALES   " TO DESCRIP-UNID-SERV
           WRITE REG-UNID-SERV END-WRITE
           MOVE 07 TO CODIGO-UNID-SERV.
           MOVE "CONSULTA PRIORITARIA  " TO DESCRIP-UNID-SERV
           WRITE REG-UNID-SERV END-WRITE
           MOVE 08 TO CODIGO-UNID-SERV.
           MOVE "PROMOCION Y PREVENCION" TO DESCRIP-UNID-SERV
           WRITE REG-UNID-SERV END-WRITE
           MOVE 09 TO CODIGO-UNID-SERV.
           IF NIT-USU = 900565371
               MOVE "HOGAR DE PASO     "  TO DESCRIP-UNID-SERV
           ELSE
               MOVE "HOSPICASA         " TO DESCRIP-UNID-SERV
           END-IF.
           WRITE REG-UNID-SERV END-WRITE
           MOVE 11 TO CODIGO-UNID-SERV.
           MOVE "UCI NEONATAL CRITICO  " TO DESCRIP-UNID-SERV
           WRITE REG-UNID-SERV END-WRITE
           MOVE 12 TO CODIGO-UNID-SERV.
           MOVE "UCI NEONATAL INTERMEDIO" TO DESCRIP-UNID-SERV
           WRITE REG-UNID-SERV END-WRITE
           MOVE 13 TO CODIGO-UNID-SERV.
           MOVE "UCI NEONATAL BASICA    " TO DESCRIP-UNID-SERV
           WRITE REG-UNID-SERV END-WRITE
           MOVE 21 TO CODIGO-UNID-SERV.
           MOVE "UCI PEDIATRICA INTERMED" TO DESCRIP-UNID-SERV
           WRITE REG-UNID-SERV END-WRITE
           MOVE 22 TO CODIGO-UNID-SERV.
           MOVE "UCI PEDIATRICA INTERMED" TO DESCRIP-UNID-SERV
           WRITE REG-UNID-SERV END-WRITE
           MOVE 31 TO CODIGO-UNID-SERV.
           MOVE "UCI ADULTO CRITICA     " TO DESCRIP-UNID-SERV
           WRITE REG-UNID-SERV END-WRITE
           MOVE 32 TO CODIGO-UNID-SERV.
           MOVE "UCI ADULTO INTERMEDIA  " TO DESCRIP-UNID-SERV
		   WRITE REG-UNID-SERV END-WRITE
		   MOVE 51 TO CODIGO-UNID-SERV.
           MOVE "UNIDAD DE SALUD MENTAL " TO DESCRIP-UNID-SERV
           WRITE REG-UNID-SERV END-WRITE.


       ABRIR-JSON-SERV.

           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-UNID-SERV.

           MOVE "{*UNIDSERV*:[" TO  DATOS-JSON
           INSPECT DATOS-JSON REPLACING ALL "*" BY CARAC-COMILLA
007640     WRITE REG-JSON END-WRITE
007650     MOVE 0 TO SW-FIN.

       LEER-GRUPO-SERV.
           IF SW-FIN = 0
007690        MOVE 1 TO SW-FIN
007700     ELSE
007710        MOVE LIN-1 TO DATOS-JSON
007720        WRITE REG-JSON END-WRITE
007730     END-IF

           READ ARCHIVO-UNID-SERV NEXT WITH NO LOCK AT END 
                GO TO CERRAR-GRUPO-SERV
           END-READ.

           IF CODIGO-UNID-SERV = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE             
              INITIALIZE LIN-1
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

           GO TO LEER-GRUPO-SERV.

       CERRAR-GRUPO-SERV.
008020     INITIALIZE LIN-1
008030     MOVE LIN-1 TO DATOS-JSON
           WRITE REG-JSON END-WRITE
008050
008060     MOVE "]}" TO DATOS-JSON
008070     WRITE REG-JSON END-WRITE
008080          
008090     CLOSE ARCHIVO-UNID-SERV  
008100           ARCHIVO-JSON.      


        ABRIR-JSON-COSTO.
           INSPECT NOM-PLANO-W  REPLACING FIRST "UNIATEN"
007280                                       BY "ARCHCOS".

           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-COSTO.

           MOVE "{*COSTO*:[" TO  DATOS-JSON
           INSPECT DATOS-JSON REPLACING ALL "*" BY CARAC-COMILLA
007640     WRITE REG-JSON END-WRITE
007650     MOVE 0 TO SW-FIN.

       LEER-GRUPO-COSTO.
           IF SW-FIN = 0
007690        MOVE 1 TO SW-FIN
007700     ELSE
007710        MOVE LIN-2 TO DATOS-JSON
007720        WRITE REG-JSON END-WRITE
007730     END-IF

           READ ARCHIVO-COSTO NEXT WITH NO LOCK AT END 
                GO TO CERRAR-GRUPO-COSTO
           END-READ.

           IF COD-COSTO = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE
              IF COD-COSTO = COD-COSTO-UNIATEN-J
                 MOVE 0 TO SW-FIN
              ELSE
                 INITIALIZE LIN-2
                 MOVE COD-COSTO     TO COD-COSTO-UNIATEN-J 
                 MOVE NOMBRE-COSTO  TO NOMBRE-COSTO-UNIATEN-J
                 MOVE DESCRIP-COSTO TO DESCRIP-COSTO-UNIATEN-J
                                
                 INSPECT LIN-2 REPLACING ALL "*" BY CARAC-COMILLA 
                 MOVE "," TO CIERRE-LIN-2           
              END-IF
           END-IF

           GO TO LEER-GRUPO-COSTO.

       CERRAR-GRUPO-COSTO.
008020     INITIALIZE LIN-2
008030     MOVE LIN-2 TO DATOS-JSON
           WRITE REG-JSON END-WRITE
008050
008060     MOVE "]}" TO DATOS-JSON
008070     WRITE REG-JSON END-WRITE
008080          
008090     CLOSE ARCHIVO-COSTO  
008100           ARCHIVO-JSON. 
  

002147 ENVIO-DATOS.
           MOVE "datosrecibidos"  TO COBW3-CNV-NAME
004393     MOVE 00                TO COBW3-CNV-VALUE
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