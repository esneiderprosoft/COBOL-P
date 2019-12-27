      *============================================================
      * VENTANA DE CONSULTA DE TIPOS DE SERVICIO SER821  SALUD 
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


       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON      PIC X(100).
000470
000540 WORKING-STORAGE  SECTION.
000550 COPY "..\..\FUENTES\COBW3.CBL".
000560 COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-PLANO-W                   PIC x(90).
       77 USUAR-W                       PIC X(60).
       77 MES-SAL-W                     PIC X(4).

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
          02 ESTADO-ENVIO               PIC XX.

       01  TIPO-SERVICIO.
           02 CODIGO-SERVICIO           PIC 9.
           02 DESCRIP-SERVICIO          PIC X(30).
           02 CANT-MAX                  PIC 9(4).

       01 LIN-1.
001820    02 FILLER                     PIC X(7)   VALUE "{*COD*:".
001830    02 FILLER                     PIC X      VALUE "*".
001840    02 CODIGO-SERVICIO-E          PIC X(2).
001850    02 FILLER                     PIC XX     VALUE "*,".
001860    02 FILLER                     PIC X(10)  VALUE "*DESCRIP*:".
001870    02 FILLER                     PIC X      VALUE "*".
001880    02 DESCRIP-SERVICIO-E         PIC X(30).
          02 FILLER                     PIC XX     VALUE "*,".
          02 FILLER                     PIC X(10)  VALUE "*CANT-M*:".
          02 FILLER                     PIC X      VALUE "*".
          02 CANT-MAX-E                 PIC X(30).
001970    02 FILLER                     PIC XX     VALUE "*}".    
001980    02 CIERRE-LIN-1               PIC X.
    

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
              MOVE "SER821"                 TO MSJ3-HTML
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
              MOVE "SER821"             TO MSJ3-HTML
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
              MOVE "SER821"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "SER821-ARCHIVO-JSON"    TO MSJ2-HTML
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
             INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W
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
001614     INITIALIZE USUAR-W
           MOVE "\\" TO USUAR-W
001617     INSPECT USUAR-W REPLACING FIRST "                    "
                                        BY  IP-DATOS-USUNET

001621     INSPECT USUAR-W REPLACING FIRST " "
001624                                  BY "\"

007190     INSPECT USUAR-W REPLACING FIRST "        "
                                        BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT USUAR-W REPLACING FIRST "                       "
007200                                  BY "\CONTROL\SC-ARCHUSU.DAT".


007210     INITIALIZE NOM-PLANO-W
           MOVE "\\" TO NOM-PLANO-W 
007220     INSPECT NOM-PLANO-W  REPLACING FIRST "                    "
007230                                       BY IP-DATOS-USUNET
      
           INSPECT NOM-PLANO-W REPLACING FIRST  "                 "
                                            BY  "\PROG\DATOS\JSON\"
      
007260     INSPECT NOM-PLANO-W  REPLACING FIRST "                                              "
007280                                       BY "JSC-TIPOSER-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"

007281     INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
007282                                       BY LLAVESESION-LLEGA-W.  


       ABRIR-ARCHIVOS.

           OPEN INPUT ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT AT END MOVE 0 TO OPCION-1.
           CLOSE ARCHIVO-USUARIOS.

        ABRIR-JSON-SERVICIO.
           OPEN OUTPUT ARCHIVO-JSON.

           MOVE "{*SERVICIOS*:[" TO  DATOS-JSON
           INSPECT DATOS-JSON REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON END-WRITE.

       CREAR-SERVICIO.

           MOVE 0 TO CODIGO-SERVICIO
           MOVE "DROGUERIA             " TO DESCRIP-SERVICIO
           MOVE 9999                     TO CANT-MAX

           MOVE "," TO CIERRE-LIN-1
           PERFORM ESCRIBIR-SERVICIO

           MOVE 1 TO CODIGO-SERVICIO.
           MOVE "CIRUGIA               " TO DESCRIP-SERVICIO
           MOVE 1                        TO CANT-MAX

           MOVE "," TO CIERRE-LIN-1
           PERFORM ESCRIBIR-SERVICIO

           IF NIT-USU = 800156469
              MOVE 2 TO CODIGO-SERVICIO
              MOVE "ECOGRAFIAS            " TO DESCRIP-SERVICIO
           ELSE
              MOVE 2 TO CODIGO-SERVICIO
              MOVE "LABORATORIO CLIN      " TO DESCRIP-SERVICIO
           END-IF.
           MOVE 10                       TO CANT-MAX

           MOVE "," TO CIERRE-LIN-1
           PERFORM ESCRIBIR-SERVICIO

           MOVE 3 TO CODIGO-SERVICIO
           MOVE "RX - IMAGENOLOGIA     " TO DESCRIP-SERVICIO
           MOVE 9                        TO CANT-MAX

           MOVE "," TO CIERRE-LIN-1
           PERFORM ESCRIBIR-SERVICIO

           IF NIT-USU = 800156469
              MOVE 4 TO CODIGO-SERVICIO
              MOVE "DOPPLER               " TO DESCRIP-SERVICIO

           ELSE
              MOVE 4 TO CODIGO-SERVICIO
              MOVE "OTROS SERVICIOS       " TO DESCRIP-SERVICIO
           END-IF.

           MOVE "," TO CIERRE-LIN-1
           PERFORM ESCRIBIR-SERVICIO


           IF NIT-USU = 800156469
              MOVE 5 TO CODIGO-SERVICIO
              MOVE "T.A.C.                " TO DESCRIP-SERVICIO

           ELSE
              MOVE 5 TO CODIGO-SERVICIO
              MOVE "CONSULTAS-TERAP       " TO DESCRIP-SERVICIO
           END-IF.

           IF (NIT-USU = 822007038 OR 900641654 OR 900541158)
              MOVE 1000                     TO CANT-MAX
           ELSE
              MOVE 9                        TO CANT-MAX
           END-IF.

           MOVE "," TO CIERRE-LIN-1
           PERFORM ESCRIBIR-SERVICIO

           MOVE 6 TO CODIGO-SERVICIO
           MOVE "RESONANCIA NUCLEAR       " TO DESCRIP-SERVICIO
           MOVE 1                           TO CANT-MAX

           MOVE "," TO CIERRE-LIN-1
           PERFORM ESCRIBIR-SERVICIO

           MOVE 7 TO CODIGO-SERVICIO
           MOVE "PROMOC. Y PREV.          " TO DESCRIP-SERVICIO
           IF NIT-USU = 900004057
               MOVE 999                     TO CANT-MAX
           ELSE 
               MOVE 20                      TO CANT-MAX
           END-IF.  

           MOVE SPACE TO CIERRE-LIN-1
           PERFORM ESCRIBIR-SERVICIO
           GO TO CERRAR-JSON.

       ESCRIBIR-SERVICIO.

           MOVE CODIGO-SERVICIO     TO CODIGO-SERVICIO-E 
           MOVE DESCRIP-SERVICIO    TO DESCRIP-SERVICIO-E
           MOVE CANT-MAX            TO CANT-MAX-E
                                
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 

           MOVE LIN-1 TO DATOS-JSON
           WRITE REG-JSON END-WRITE.
           
       CERRAR-JSON.

008060     MOVE "]}" TO DATOS-JSON
008070     WRITE REG-JSON END-WRITE
008080          
008090     CLOSE ARCHIVO-JSON. 

           MOVE "00"              TO ESTADO-ENVIO.

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