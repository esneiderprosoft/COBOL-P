      *============================================================
      * CREACION DE ARCHIVO JSON CON823 SALUD
      *============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".            
       COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".  

           SELECT ARCHIVO-SUCUR LOCK MODE IS AUTOMATIC
                  ASSIGN SUCUR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS COD-SUCUR
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-JSON
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.
                                                          
000390
000400 DATA DIVISION.
       FILE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SUCUR.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON      PIC X(500).
000470
000540 WORKING-STORAGE  SECTION.
000550 COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
000560 COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".

       77 NOM-PLANO-W               PIC x(90).
       
       77 SUCUR-W                   PIC X(60).

       77 MES-SAL-W                 PIC X(4).

       01  DATO-LLEGADA-W.                                                                                 
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).     
           02 DIR-CONTAB-LLEGADA-W      PIC X(30).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 OPERADOR-LLEGADA-W        PIC X(4).

        
       01 DATO-IMPORTAR-WK              PIC X(100). 
        
       01 DATOS-ENVIO.
          02 DAT0-ENV                   PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 NOMBRE-JSON-ENV            PIC X(28).

       01 LIN-1.  
          02 FILLER                   PIC X(10)  VALUE "{*CODIGO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CODIGO-SUCUR-J           PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(17)  VALUE "*DESCRIPCION*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DESCRIPCION-SUCUR-J      PIC X(30).  
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)  VALUE "*ALMACEN*:".
          02 FILLER                   PIC X      VALUE "*".
          02 ALMACEN-SUCUR-J          PIC X(5).  

          02 FILLER                   PIC XX     VALUE "*}".    
          02 CIERRE-LIN-1             PIC X.

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
              MOVE NOM-USU-W                TO MSJ2-HTML
              MOVE "CON823"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SUCUR.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT              TO MSJ1-HTML
              MOVE SUCUR-W               TO MSJ2-HTML
              MOVE "CON823"              TO MSJ3-HTML
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
              MOVE "CON823"                 TO MSJ3-HTML
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
              MOVE "CON823"                 TO MSJ3-HTML
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
001314                  WRITE REG-USUNET
001315                  CLOSE       ARCHIVO-USUNET
001316                  MOVE "Validacion de usuarios"      TO MENSAJE1-HTML
001317                  MOVE "Falta configurar usuario"    TO MENSAJE2-HTML
001318                  MOVE "Sc"                          TO MENSAJE3-HTML
001319                  GO TO ENVIAR-ERROR
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
           INITIALIZE SUCUR-W 

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

           MOVE "\\" TO SUCUR-W 
001617     INSPECT SUCUR-W REPLACING FIRST "                    "
                                        BY IP-DATOS-USUNET


001621     INSPECT SUCUR-W REPLACING FIRST " "
001624                                  BY "\"

           INSPECT SUCUR-W REPLACING FIRST "                       "
                                        BY "PROG\DATOS\SC-SUCUR.DAT".


           INITIALIZE NOM-PLANO-W

           MOVE OPERADOR-LLEGADA-W TO OPER-JSON-W
           MOVE FUNCTION CURRENT-DATE TO FECHA-JSON-W
           ACCEPT HORA-JSON-W FROM TIME


           MOVE NOMBRE-JSON-W  TO NOM-PLANO-W.
           MOVE NOM-JSON-W     TO NOMBRE-JSON-ENV.
 

       ABRIR-ARCHIVOS.
           OPEN INPUT ARCHIVO-SUCUR.

           MOVE "00" TO DAT0-ENV.


       ABRIR-JSON-PROF.
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-SUCUR.

           MOVE "{*SUCURSAL*:[" TO  DATOS-JSON
           INSPECT DATOS-JSON REPLACING ALL "*" BY CARAC-COMILLA
007640     WRITE REG-JSON END-WRITE
007650     MOVE 0 TO SW-FIN.

       LEER-SUCURSAL.
           IF SW-FIN = 0
007690        MOVE 1 TO SW-FIN
007700     ELSE
007710        MOVE LIN-1 TO DATOS-JSON
007720        WRITE REG-JSON END-WRITE
007730     END-IF

           READ ARCHIVO-SUCUR NEXT WITH NO LOCK AT END GO TO CERRAR-SUCURSAL.

           MOVE COD-SUCUR TO CODIGO-SUCUR-J
           MOVE DESCRIP-SUCUR TO DESCRIPCION-SUCUR-J. 
           MOVE ALMACEN-SUCUR TO ALMACEN-SUCUR-J

            INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
            MOVE ","      TO CIERRE-LIN-1

           GO TO LEER-SUCURSAL.
           
       CERRAR-SUCURSAL.
           INITIALIZE LIN-1
           MOVE LIN-1 TO DATOS-JSON
           WRITE REG-JSON END-WRITE

           MOVE "]}" TO DATOS-JSON
           WRITE REG-JSON END-WRITE
            
           CLOSE ARCHIVO-SUCUR.

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

       COPY "P:\PROG\PROYECT\FUENTES\SC-WEB19.CBL".




