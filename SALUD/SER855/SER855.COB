      *=================================================================
      *CREACION DE ARCHIVO JSON SER855 F8 DE ESPECIALIDADES  -SALUD
      *=================================================================  
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  
                                                                                      
           SELECT ARCHIVO-ESPECIALIDAD LOCK MODE IS AUTOMATIC
	          ASSIGN NOM-ARCH-ESP-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-ESP
                  ALTERNATE RECORD KEY IS NOMBRE-ESP WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-ESPEC.CBL".

       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".

       01 LLEGADA-W                     PIC X(100).

       77 NOM-ARCH-ESP-W                PIC X(60).
       01 DATOS-PLANO-W                 PIC X(1000).

       01 LIN-1.  
          02 FILLER                  PIC X(10)  VALUE "{*CODIGO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-ESP-J               PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*NOMBRE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOMBRE-ESP-J            PIC X(70).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*COSTO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COSTO-ESP-J             PIC X(4).          
          02 FILLER                  PIC XX     VALUE "*}".    
          02 CIERRE-LIN-1            PIC X.


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
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-USU-W                 TO MSJ2-HTML
              MOVE "SER855"                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-SESION-W              TO MSJ2-HTML
              MOVE "SER855"                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ESPECIALIDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-ARCH-ESP-W            TO MSJ2-HTML
              MOVE "SER855"                  TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO LLEGADA-W.

           UNSTRING LLEGADA-W DELIMITED BY "|"
               INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W
           END-UNSTRING.

           MOVE SESION-LLEGAD-W   TO LLAVE-SESION-W.

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
           INITIALIZE NOM-ARCH-ESP-W

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           MOVE "\\" TO NOM-ARCH-ESP-W

           INSPECT NOM-ARCH-ESP-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

           INSPECT NOM-ARCH-ESP-W REPLACING FIRST " "
                                             BY    "\"

           INSPECT NOM-ARCH-ESP-W   REPLACING FIRST "                         "
                                             BY     "PROG\DATOS\SC-ARCHESP.DAT".

       ABRIR-ARCHIVO.
           OPEN INPUT ARCHIVO-ESPECIALIDAD
           MOVE "{*ESPECIALIDADES*:[" TO  DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO.

       CONSULTAR-ARCHIVO.
           READ ARCHIVO-ESPECIALIDAD NEXT WITH NO LOCK AT END 
                GO TO CERRAR-ARCHIVO
           END-READ.

           INITIALIZE LIN-1
           MOVE COD-ESP       TO COD-ESP-J

           INSPECT NOMBRE-ESP REPLACING ALL '"' BY " "
           INSPECT NOMBRE-ESP REPLACING ALL '*' BY " "
           INSPECT NOMBRE-ESP REPLACING ALL 'Ñ' BY CARAC-ENE-MAY
           INSPECT NOMBRE-ESP REPLACING ALL 'ñ' BY CARAC-ENE-MIN
           MOVE NOMBRE-ESP    TO NOMBRE-ESP-J

           MOVE COSTO-ESP     TO COSTO-ESP-J

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES
           MOVE "," TO CIERRE-LIN-1
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           GO TO CONSULTAR-ARCHIVO.

       CERRAR-ARCHIVO.
           INITIALIZE LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
      
           MOVE "]}" TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
                
           CLOSE ARCHIVO-ESPECIALIDAD.

       PAGINA-CONFIG.
           MOVE "statuscode" TO COBW3-CNV-NAME
           MOVE "00"         TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3

           MOVE "programa-id" TO COBW3-CNV-NAME
           MOVE "SER802"      TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3

           MOVE "..\PAGINAS\RECIBIDOSLISTADO.ASPX" TO SALIDA-HTML
           PERFORM ABRIR-HTML.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.

       DATOS-ENVIO.
           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE DATOS-PLANO-W    TO COBW3-CNV-VALUE
           CALL "COBW3_SET_REPEAT" USING COBW3.

       COPY "..\..\FUENTES\SC-WEB19.CBL".
        
