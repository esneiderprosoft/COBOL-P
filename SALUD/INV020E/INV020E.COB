      *=================================================================
      * SALUD- ANULA CONTAB DE FACTURA P Y T - INV020E 
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
                  ASSIGN TO NOM-USUAR-W
      *"../CONTROL/SC-ARCHUSU.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT MOVIMIENTO-DIARIO LOCK MODE IS AUTOMATIC
                  ASSIGN  NOM-MOV-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-MOV
                  ALTERNATE RECORD KEY SECUENCIA-MOV
                  ALTERNATE RECORD KEY LLAVE-FEC
                  ALTERNATE RECORD KEY LLAVE-DOC-MOV   WITH DUPLICATES
                  ALTERNATE RECORD KEY COSTO-MOV       WITH DUPLICATES
                  ALTERNATE RECORD KEY NRO-ORD-MOV     WITH DUPLICATES
                  ALTERNATE RECORD KEY LLAVE-LIBRE-MOV WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-MOVIM.CBL".


       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LLEGADA-W                    PIC X(100).                                      
  
       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(21).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 LLAVE-LLEGADA-W.
             03 PREFIJO-LLEGADA-W      PIC X.
             03 NRO-NUM-LLEGADA-W.
                05 NRO1-NUM-W          PIC 9.
                05 NRO2-NUM-W          PIC 9(5). 

       77 NOM-USUAR-W                  PIC X(70). 
       77 NOM-MOV-W                    PIC X(70).       
                     
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
              MOVE "SER108F"                TO MSJ3-HTML
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
              MOVE "SER108F"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USUAR-W              TO MSJ2-HTML
              MOVE "SER108F"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
 
        I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MOVIMIENTO-DIARIO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-MOV-W                TO MSJ2-HTML
              MOVE "SER108F"                TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA    TO LLEGADA-W.

           UNSTRING LLEGADA-W DELIMITED BY "|"
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, LLAVE-LLEGADA-W
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
              MOVE "Validacion de usuarios"      TO MSJ1-HTML
              MOVE "Falta configurar usuario"    TO MSJ2-HTML
              MOVE "Sc"                          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
 
       ASIGNAR-NOMBRES. 
          
           OPEN INPUT  ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS  NEXT WITH NO LOCK AT END
                MOVE 0 TO OPCION-1
           END-READ

           CLOSE ARCHIVO-USUARIOS.                 
         
           INITIALIZE NOM-MOV-W
           MOVE "\\" TO NOM-MOV-W

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-MOV-W REPLACING FIRST "                    "
                                           BY IP-DATOS-USUNET

           INSPECT NOM-MOV-W REPLACING FIRST " "
                                           BY "\"

           INSPECT NOM-MOV-W REPLACING FIRST "                     "
                                           BY DIR-CONTAB-LLEGADA-W

           INSPECT NOM-MOV-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-ARCHMOV.DAT".

       DATOS-MOVIMIENTO.
           OPEN I-O MOVIMIENTO-DIARIO.

               EVALUATE PREFIJO-LLEGADA-W 
               WHEN  "A" MOVE "50" TO LOTE-MOV
               WHEN  "B" MOVE "5B" TO LOTE-MOV
               WHEN  "D" MOVE "5D" TO LOTE-MOV
               WHEN  "F" MOVE "5F" TO LOTE-MOV
               WHEN  "G" MOVE "5G" TO LOTE-MOV
               WHEN  "H" MOVE "5H" TO LOTE-MOV
               WHEN  "I" MOVE "5I" TO LOTE-MOV
               WHEN  "J" MOVE "5J" TO LOTE-MOV
               WHEN  "K" MOVE "5K" TO LOTE-MOV
               WHEN  "L" MOVE "5L" TO LOTE-MOV
               WHEN  "M" MOVE "5M" TO LOTE-MOV
               WHEN  "N" MOVE "5N" TO LOTE-MOV
               WHEN  "O" MOVE "5O" TO LOTE-MOV
               WHEN  "Q" MOVE "5Q" TO LOTE-MOV
               WHEN  "R" MOVE "5R" TO LOTE-MOV
               WHEN  "S" MOVE "5S" TO LOTE-MOV
               WHEN  "V" MOVE "5V" TO LOTE-MOV
               WHEN  "W" MOVE "5W" TO LOTE-MOV
               WHEN  "X" MOVE "5X" TO LOTE-MOV
               WHEN  "Y" MOVE "5Y" TO LOTE-MOV
               WHEN  "Z" MOVE "5Z" TO LOTE-MOV
               WHEN  "P" MOVE "40" TO LOTE-MOV
               WHEN  "T" MOVE "30" TO LOTE-MOV.

            IF (NIT-USU = 800162035 OR 845000038)
               MOVE NRO1-NUM-W  TO LOTE2-MOV
            END-IF.

            MOVE NRO2-NUM-W     TO COMPROB-MOV.
            MOVE LLAVE-COMP-MOV TO LLAVE-LLEGADA-W.

       BUSCAR-MOVIMIENTO.
            START MOVIMIENTO-DIARIO KEY = LLAVE-COMP-MOV
                  INVALID KEY
                  GO TO CERRAR-ARCHIVOS
            END-START.

       BORRAR-MOV.
            READ MOVIMIENTO-DIARIO NEXT WITH NO LOCK AT END
                 GO TO CERRAR-ARCHIVOS          
            END-READ

            IF LLAVE-COMP-MOV IS NOT EQUAL TO LLAVE-LLEGADA-W   
               GO TO CERRAR-ARCHIVOS
            END-IF

            DELETE MOVIMIENTO-DIARIO.

            GO TO CERRAR-ARCHIVOS.

       CERRAR-ARCHIVOS.
           CLOSE MOVIMIENTO-DIARIO. 

       PAGINA-CONFIG.	 
           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE "00"             TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3
         
           MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
           PERFORM ABRIR-HTML.

       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.

              
       COPY "..\..\FUENTES\SC-WEB19.CBL".
