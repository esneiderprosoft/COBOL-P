      *=================================================================
      * CONSULTA DE TERCEROS EN ORDEN ALFABETICO - CON802S
      * MUESTRA SOLO ENTIDADES
      *=================================================================
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

           SELECT ARCHIVO-TERCEROS  LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TER-LNK
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-TERCERO
                  ALTERNATE RECORD KEY DESCRIP-TER WITH DUPLICATES
                  ALTERNATE RECORD KEY ACT-TER     WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-CIUDAD  LOCK MODE IS AUTOMATIC
                  ASSIGN  NOM-CIUDA-W
      * "P:\PROG\DATOS\SC-ARCHCIUD.DAT";
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-CIU
                  ALTERNATE RECORD KEY IS NOMBRE-CIU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-ACTIVIDAD LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-ACTIV-W
      * "/PROG/DATOS/SCARCHACT.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY  IS COD-ACT
                  FILE STATUS IS OTR-STAT.

           SELECT EXTENSION-TERCEROS
                  ASSIGN ARCHTER2-USU
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-TER2
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-JSON
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.



       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-TERCE.CBL".
       COPY "..\..\FUENTES\FD-CIUDA.CBL".
       COPY "..\..\FUENTES\FD-ACTIV.CBL".
       COPY "..\..\FUENTES\FD-TERC2.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON           PIC X(1000).

       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-USUAR-W                  PIC X(70).
       77 OPC-SEGU                     PIC X(6).
       77 NOM-PLANO-W                  PIC X(90).
       77 NOM-ACTIV-W                  PIC X(70).
       77 NOM-CIUDA-W                  PIC X(70).
       77 ARCHTER2-USU                 PIC X(35).


       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(20).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 ADMIN-LLEGADA-W           PIC X(4).

       01 LLEGADA-W                    PIC X(100).                   

       01 DATOS-ENVIO.
          02 ESTADO-ENVIO              PIC X(2).
          02 FILLER                    PIC X VALUE "|".
          02 MENSAJE-ENVIO             PIC X(8). 

       01 LIN-1.  
001820    02 FILLER                   PIC X(10)  VALUE "{*NOMBRE*:".
001830    02 FILLER                   PIC X      VALUE "*".
001840    02 NOMBRE-TER-J             PIC X(50).
001850    02 FILLER                   PIC XX     VALUE "*,".
001860    02 FILLER                   PIC X(10)  VALUE "*NIT-COD*:".
001870    02 FILLER                   PIC X      VALUE "*".
001880    02 NIT-TER-J                PIC 9(10).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*TELEFONO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 TELEFONO-TER-J           PIC 9(12).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*CIUDAD*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CIUDAD-TER-J             PIC 9(7).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(16)  VALUE "*NOMBRE-CIUDAD*:".
          02 FILLER                   PIC X      VALUE "*".
          02 NOMBRE-ACT-J             PIC X(25).
          
001970    02 FILLER                   PIC XX     VALUE "*}".    
001980    02 CIERRE-LIN-1             PIC X.
               
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
              MOVE "CON802S"                TO MSJ3-HTML
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
              MOVE "CON802S"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MENSAJE1-HTML
              MOVE NOM-USUAR-W              TO MENSAJE2-HTML
              MOVE "CON802S"                TO MENSAJE3-HTML
              GO TO ENVIAR-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TERCEROS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MENSAJE1-HTML
              MOVE NOM-TER-LNK                TO MENSAJE2-HTML
              MOVE "CON802S"                TO MENSAJE3-HTML
              GO TO ENVIAR-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CIUDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MENSAJE1-HTML
              MOVE NOM-CIUDAD-W             TO MENSAJE2-HTML
              MOVE "CON802S"                TO MENSAJE3-HTML
              GO TO ENVIAR-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ACTIVIDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MENSAJE1-HTML
              MOVE NOM-ACTIV-W              TO MENSAJE2-HTML
              MOVE "CON802S"                TO MENSAJE3-HTML
              GO TO ENVIAR-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON EXTENSION-TERCEROS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MENSAJE1-HTML
              MOVE ARCHTER2-USU             TO MENSAJE2-HTML
              MOVE "CON802S"                TO MENSAJE3-HTML
              GO TO ENVIAR-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MENSAJE1-HTML
              MOVE ARCHTER2-USU             TO MENSAJE2-HTML
              MOVE "CON802S"                TO MENSAJE3-HTML
              GO TO ENVIAR-ERROR
           END-IF.

       END DECLARATIVES.
       
       INICIAR-IIS.
           MOVE LOW-VALUE TO COBW3.
           MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
           CALL "COBW3_INIT" USING COBW3.
         
       LEER-DATO-HTML.
           MOVE "datosh" TO COBW3-SEARCH-DATA.
           CALL "COBW3_GET_VALUE" USING COBW3.
           MOVE COBW3-GET-DATA    TO LLEGADA-W.

           UNSTRING LLEGADA-W DELIMITED BY "|"
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, ADMIN-LLEGADA-W
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
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
      
           INITIALIZE LLAVE-USUNET.

       LEER-USUARIO.           
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
           CLOSE ARCHIVO-USUNET.
      
           IF NOMBRE-USUNET = SPACES
              MOVE "Validacion de usuarios"      TO MENSAJE1-HTML
              MOVE "Falta configurar usuario"    TO MENSAJE2-HTML
              MOVE "Sc"                          TO MENSAJE3-HTML
              GO TO ENVIAR-ERROR
           END-IF.
 
       ASIGNAR-NOMBRES.                  
         
           INITIALIZE NOM-USUAR-W
                      NOM-TER-LNK
                      NOM-CIUDA-W
                      NOM-ACTIV-W
                      ARCHTER2-USU



           MOVE "\\" TO NOM-USUAR-W 

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET
           INSPECT NOM-USUAR-W REPLACING FIRST " "
                                            BY "\"

           MOVE NOM-USUAR-W TO NOM-CIUDA-W NOM-ACTIV-W NOM-PLANO-W.

           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                            BY DIR-CONTAB-LLEGADA-W.
           INSPECT NOM-USUAR-W REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHUSU.DAT".

           INSPECT NOM-CIUDA-W REPLACING FIRST "                     "
                                            BY "\DATOS\SC-ARCHCIU.DAT".

           INSPECT NOM-ACTIV-W REPLACING FIRST "                    "
                                            BY "\DATOS\SCARCHACT.DAT".


           INSPECT NOM-PLANO-W REPLACING FIRST "               "
                                            BY "PROG\DATOS\JSON"

           INSPECT NOM-PLANO-W REPLACING FIRST "                                              "
                                            BY "JSC-UNISERV-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"

           INSPECT NOM-PLANO-W REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                            BY LLAVE-SESION-LLEGA-W.


           MOVE ADMIN-LLEGADA-W TO ADMIN-W.


       ABRIR-ARCHIVOS.    
           OPEN INPUT ARCHIVO-USUARIOS.
                     
      *     IF (PUC-USU = 4 OR 6)
      *        MOVE "SC-ACTIOFIT.DAT" TO NOM-ACTIV
      *     ELSE
      *        MOVE "ARCHACT.DAT"     TO NOM-ACTIV
      *     END-IF

            MOVE ARCHTER-USU         TO ARCHTER2-USU.
            INSPECT ARCHTER2-USU  REPLACING FIRST "ARCHTER"
                                            BY    "AR-TER2".
            OPEN INPUT ARCHIVO-TERCEROS
                       ARCHIVO-ACTIVIDAD
                       EXTENSION-TERCEROS
                       ARCHIVO-CIUDAD.
       ABRIR-JSON.
            OPEN OUTPUT ARCHIVO-JSON.

            MOVE "{*ARCH-TER*:[" TO DATOS-JSON
            INSPECT DATOS-JSON REPLACING ALL "*" BY CARAC-COMILLA
            WRITE REG-JSON END-WRITE
            MOVE 0 TO SW-FIN.

       ESCRIBIR-DATOS.
            IF SW-FIN = 0
               MOVE 1 TO SW-FIN
            ELSE
               MOVE LIN-1 TO DATOS-JSON
               WRITE REG-JSON END-WRITE
            END-IF


            READ ARCHIVO-TERCEROS NEXT WITH NO LOCK AT END 
                 GO TO CERRAR-ARCHIVOS
            END-READ

            IF NIT-TER = SPACES OR LOW-VALUES 
               MOVE 0 TO SW-FIN
            ELSE
      *         IF NIT-TER > 0
      *            MOVE NIT-TER   TO COD-TER2
      *            READ EXTENSION-TERCEROS INVALID KEY
      *                                    MOVE DESCRIP-TER2  TO NOMBRE-TER-J
      *            END-READ
      *         ELSE
      *            INITIALIZE DESCRIP-TER2
      *         END-IF

               MOVE COD-TER        TO NIT-TER-J 
               MOVE DESCRIP-TER    TO NOMBRE-TER-J
               MOVE ACT-TER        TO COD-ACT
               READ ARCHIVO-ACTIVIDAD INVALID KEY
                                      MOVE COD-ACT  TO NOMBRE-ACT
               END-READ
               MOVE NOMBRE-ACT     TO NOMBRE-ACT-J
               READ ARCHIVO-CIUDAD INVALID KEY
                                   MOVE COD-CIU   TO NOMBRE-CIU
               END-READ
               MOVE NOMBRE-CIU     TO CIUDAD-TER-J
               MOVE TELEFONO-TER   TO TELEFONO-TER-J

               INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
               MOVE "," TO CIERRE-LIN-1
            END-IF

            GO TO ESCRIBIR-DATOS.

       CERRAR-ARCHIVOS.
            CLOSE ARCHIVO-TERCEROS
                  ARCHIVO-USUARIOS
                  ARCHIVO-ACTIVIDAD
                  EXTENSION-TERCEROS
                  ARCHIVO-CIUDAD.

       ENVIO-DATOS.
            MOVE "00"            TO ESTADO-ENVIO
            MOVE "EXITOSO"       TO MENSAJE-ENVIO.

       PAGINA-CONFIG.
	 
           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO      TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3
         
           MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
           PERFORM ABRIR-HTML.

       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.

              
       COPY "..\..\FUENTES\SC-WEB19.CBL".
