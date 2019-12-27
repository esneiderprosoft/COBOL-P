      *============================================================
      * MUESTRA DE PROTOCOLOS DE EXAMENES - SER210A
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-MACRO-EVOL LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-MACRO-W;
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-MACROEVOL
                  ALTERNATE RECORD KEY DETALLE-MACROEVOL WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-MACREV.CBL".
  
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".

       01 LLEGADA-W                    PIC X(200).

       77 NOM-MACRO-W                  PIC X(70).
       01 DATOS-PLANO-W                PIC X(1024).

   
       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(8).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 LLAVE-MACROEV-W           PIC X(7).
          02 NOMBRE-PACI-W             PIC X(40).
          02 NOMBRE-EXA-W              PIC X(60).
          02 TIPO-W                    PIC X.

       01 TABLA-EDIT.
          02 TABLA-EDIT-W              PIC X(1000)  OCCURS 29.

       01 LIN-1.  
          02 FILLER                  PIC X(15)  VALUE "{*NOMBRE_PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOMBRE-PACI-J           PIC X(40). 
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*LLAVE_MACRO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LLAVE-MACROEVOL-J       PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*NOMBRE_EXA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOMBRE-EXA-J            PIC X(60).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*RENG_MACRO*:".
          02 FILLER                  PIC X      VALUE "*".

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
              MOVE "SER210A"                TO MSJ3-HTML
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
              MOVE "SER210A"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-MACRO-EVOL.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-MACRO-W              TO MSJ2-HTML
              MOVE "SER210A"                TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO LLEGADA-W

           UNSTRING LLEGADA-W DELIMITED BY "|"
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W,
                   LLAVE-MACROEV-W, NOMBRE-PACI-W, NOMBRE-EXA-W, TIPO-W
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.

       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
         
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.
              
       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
     
           OPEN INPUT ARCHIVO-USUNET
      
           INITIALIZE LLAVE-USUNET.

       LEER-USUARIO.
           
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
           CLOSE ARCHIVO-USUNET.
      
           IF NOMBRE-USUNET = SPACES
              MOVE "SC-2"                        TO MSJ1-HTML
              MOVE "Falta configurar usuario"    TO MSJ2-HTML
              MOVE "SER210A"                     TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.


       ASIGNAR-NOMBRES.
           INITIALIZE NOM-MACRO-W 

           MOVE "\\" TO NOM-MACRO-W

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\"

           INSPECT NOM-MACRO-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET
           
           INSPECT NOM-MACRO-W REPLACING FIRST "                           "
                                           BY  "\PROG\DATOS\SC-MACRO-EV.DAT"

              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE LLAVE-MACROEV-W          TO MSJ2-HTML
              MOVE "SER210A"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR.

       ABRIR-ARCHIVOS.
           OPEN INPUT ARCHIVO-MACRO-EVOL
           MOVE LLAVE-MACROEV-W    TO LLAVE-MACROEVOL
           READ ARCHIVO-MACRO-EVOL WITH NO LOCK
                INVALID KEY
                  INITIALIZE LLAVE-MACROEVOL
                  INITIALIZE NOMBRE-PACI-W
                  INITIALIZE NOMBRE-EXA-W
           END-READ

           MOVE "{*MACRO*:["       TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO

           MOVE LLAVE-MACROEVOL    TO LLAVE-MACROEVOL-J
           MOVE NOMBRE-PACI-W      TO NOMBRE-PACI-J
           MOVE NOMBRE-EXA-W       TO NOMBRE-EXA-J

           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES
           INSPECT LIN-1 REPLACING ALL "*"        BY CARAC-COMILLA

           STRING LIN-1, '"', TABLA-MACROEVOL, '"},' DELIMITED BY SIZE
                  INTO TABLA-EDIT
           END-STRING
           PERFORM MONTAR-TABLA VARYING I FROM 1 BY 1 UNTIL I > 28.

        CERRAR-ARCHIVO.
           INITIALIZE LIN-1
           STRING LIN-1, "**}]}" DELIMITED BY SIZE INTO DATOS-PLANO-W
           END-STRING

           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO
           CLOSE ARCHIVO-MACRO-EVOL.

        PAGINA-CONFIG.
           MOVE "statuscode" TO COBW3-CNV-NAME
           MOVE "00"         TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3

           MOVE "programa-id" TO COBW3-CNV-NAME
           MOVE "SER210A"     TO COBW3-CNV-VALUE
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

       MONTAR-TABLA.
           INITIALIZE DATOS-PLANO-W
           INSPECT TABLA-EDIT-W (I) REPLACING ALL "*"         BY CARAC-COMILLA
           INSPECT TABLA-EDIT-W (I) REPLACING ALL "\.br\"     BY SPACES
           INSPECT TABLA-EDIT-W (I) REPLACING ALL LOW-VALUES  BY SPACES
           MOVE TABLA-EDIT-W    (I) TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO.

       COPY "..\..\FUENTES\SC-WEB19.CBL".
      
