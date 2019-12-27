      *=================================================================
      * BUSQUEDA Y CONSULTA DE MAESTRO ARTICULOS 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
           COPY "..\..\FUENTES\FS-USUNET.CBL".
           COPY "..\..\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-CLASCLIENTE LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-CLAS-W
      *"\PROG\DATOS\SC-CLASC.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS LLAVE-CLASC
                  ALTERNATE RECORD KEY IS DESCRIP-CLASC WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-JSON LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-CLASC.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON      PIC X(500).

       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-CLAS-W                             PIC X(70).
       77 NOM-PLANO-W                            PIC X(90).

       01 TABLA-DESCRIP-W.
          02 TAB-DESCRIP-W  OCCURS 20            PIC X(15).

       01 LLEGADA-W                              PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W                      PIC X(15).
             03 FECHA-LLEGAD-W                   PIC X(8).
             03 HORA-LLEGAD-W                    PIC X(6).
          02 DIR-CONTAB-LLEGADA-W                PIC X(30).
          02 MES-CONTAB-LLEGADA-W                PIC XX.
          02 OPERADOR-LLEGADA-W                  PIC X(4).

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV                      PIC XX.
          02 FILLER                              PIC X   VALUE "|".    
          02 DATO-J                              PIC X(28).

       01 LIN-1.
          02 FILLER                              PIC X(14)  VALUE "{°TB_DESCRIP°:".
          02 FILLER                              PIC X      VALUE "°".
          02 TAB-DESCRIP-W-J                     PIC X(15).
          02 FILLER                              PIC XX     VALUE "°}".    
          02 CIERRE-LIN-1                        PIC X.

                       
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
              MOVE "INV103-15"              TO MSJ3-HTML
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
              MOVE "INV103-15"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CLASCLIENTE.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-CLAS-W               TO MSJ2-HTML
              MOVE "INV103-15"              TO MSJ3-HTML
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
              MOVE "INV103-15"              TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
          
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
         
           INITIALIZE NOM-CLAS-W
 
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
 
           MOVE "\\" TO NOM-CLAS-W

           INSPECT NOM-CLAS-W REPLACING FIRST "                    "
                                           BY IP-DATOS-USUNET
           INSPECT NOM-CLAS-W REPLACING FIRST " "
                                           BY "\"          
           INSPECT NOM-CLAS-W REPLACING FIRST "           "
                                           BY "PROG\DATOS\"
           INSPECT NOM-CLAS-W REPLACING FIRST "            "
                                           BY "SC-CLASC.DAT".

           INITIALIZE NOM-PLANO-W

           MOVE OPERADOR-LLEGADA-W TO OPER-JSON-W
           MOVE FUNCTION CURRENT-DATE TO FECHA-JSON-W
           ACCEPT HORA-JSON-W FROM TIME


           MOVE NOMBRE-JSON-W  TO NOM-PLANO-W.
           MOVE NOM-JSON-W     TO DATO-J.

       ABRIR-ARCHIVOS.
           OPEN INPUT ARCHIVO-CLASCLIENTE.

           MOVE "00" TO SW-INVALID-ENV.

       LEER-CLASIFICACION.
           ADD 1 TO I

           READ ARCHIVO-CLASCLIENTE NEXT WITH NO LOCK AT END
                                    GO TO ABRIR-JSON
           END-READ.

           MOVE DESCRIP-CLASC    TO TAB-DESCRIP-W (I)

           GO TO LEER-CLASIFICACION.

       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-JSON.

           MOVE "{*TABLA*:[" TO  DATOS-JSON
           INSPECT DATOS-JSON  REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON  END-WRITE
           MOVE 0 TO SW-FIN.

       ENVIO-DATOS-JSON.
           PERFORM MOVER-VRART VARYING I FROM 1 BY 1
                               UNTIL I > 20.

       CERRAR-JSON.
           INITIALIZE LIN-1

           INITIALIZE TABLA-DESCRIP-W
                                                                 
           INSPECT LIN-1 REPLACING ALL "°" BY CARAC-COMILLA
           MOVE LIN-1 TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
          
           MOVE "]}" TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE.

       CERRAR-MAESTRO-ARTICULOS.           
           CLOSE ARCHIVO-CLASCLIENTE
                 ARCHIVO-JSON.

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

       MOVER-VRART.
           INITIALIZE LIN-1
         
           MOVE TAB-DESCRIP-W (I)     TO TAB-DESCRIP-W-J

           INSPECT LIN-1 REPLACING ALL "°" BY CARAC-COMILLA 
           INSPECT LIN-1 REPLACING ALL LOW-VALUE BY " "
           MOVE "," TO CIERRE-LIN-1

           MOVE LIN-1 TO DATOS-JSON
           WRITE REG-JSON END-WRITE.       
              
       COPY "..\..\FUENTES\SC-WEB19.CBL".
