      *=================================================================
      *CREA, ENVIA OBJETO OCUPACIONES - SER854
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
      
           SELECT ARCHIVO-OCUPACIONES
                  ASSIGN NOM-OCUP-W 
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-OCU
                  ALTERNATE RECORD KEY IS NOMBRE-OCU WITH DUPLICATES
                  FILE STATUS OTR-STAT. 
                  
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-OCUPA.CBL".
      
       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".
       
       77 NOM-OCUP-W                   PIC X(100).
       77 CONT                         PIC 9(4).
       77 DATOS-PLANO-W                PIC X(200).

       01 LLEGADA-W PIC X(100).

       01  DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
            03 ID-LLEGAD-W            PIC X(15).
            03 FECHA-LLEGAD-W         PIC X(8).
            03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W     PIC X(20).
          02 MES-CONTAB-LLEGADA-W     PIC XX.

       01 LIN-1.
          02 FILLER                  PIC X(11)  VALUE "{*CODOCU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-OCU-J               PIC X(4).

          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*NOMBREOCU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOMBRE-OCU-J            PIC X(120).

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
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-USU-W        TO MSJ2-HTML
              MOVE "SER854"         TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" 
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-SESION-W     TO MSJ2-HTML
              MOVE "SER854"         TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

        I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-OCUPACIONES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-OCUP-W               TO MSJ2-HTML
              MOVE "SER854"                 TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W
           END-UNSTRING.  
          
           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.

       INICIAR-SESION.
000806     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.
006840
006900 ABRIR-USUARIO.
006910     INITIALIZE OTR-STAT.
006920     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
006930
006940     OPEN INPUT ARCHIVO-USUNET
006950     EVALUATE OTR-STAT
006960       WHEN "00"  CONTINUE
006970       WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
006980                  INITIALIZE REG-USUNET
006990                  WRITE REG-USUNET
007000                  CLOSE ARCHIVO-USUNET
007010                  MOVE "SC-2"                       TO MSJ1-HTML
007020                  MOVE "Falta configurar usuarios"  TO MSJ2-HTML
007030                  MOVE "SER854"                     TO MSJ3-HTML
007040                  GO TO ENVIAR2-ERROR
007050      WHEN OTHER GO TO CERRAR-SESION
007060     END-EVALUATE.

007110 LEER-USUARIO.
007120     READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
007140     CLOSE ARCHIVO-USUNET.
007200     
           IF NOMBRE-USUNET = SPACES
007220     OR NIT-USUNET = ZEROS
007230        MOVE "SC-2"      TO MSJ1-HTML
007240        MOVE "Falta configurar usuarios" TO MSJ2-HTML
007250        MOVE "SER854"    TO MSJ3-HTML
007260        GO TO ENVIAR2-ERROR
007270     END-IF.

       ASIGNAR-NOMBRES.

           INITIALIZE NOM-OCUP-W
           MOVE "\\" TO NOM-OCUP-W
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

           INSPECT NOM-OCUP-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

           INSPECT NOM-OCUP-W REPLACING FIRST " "
                                             BY "\"
           INSPECT NOM-OCUP-W REPLACING FIRST  "                         "
                                            BY "PROG\DATOS\SC-ARCHOCU.DAT".
           
       ABRIR-ARCHIVOS.
           OPEN INPUT ARCHIVO-OCUPACIONES.

       CREAR-OBJ-ENTIDADES. 
007610     INITIALIZE REG-OCU.

007620     MOVE "{*OCUPACIONES*:[" TO  DATOS-PLANO-W 
007630     INSPECT DATOS-PLANO-W  REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO
007650     MOVE 0 TO SW-FIN.
007660
007670 LEER-ENTIDADES.
007680     IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-PLANO-W
              PERFORM DATOS-ENVIO
           END-IF
           READ ARCHIVO-OCUPACIONES NEXT WITH NO LOCK AT END
                GO TO CERRAR-OCUPACIONES
           END-READ.

           IF COD-OCU = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE
              IF COD-OCU = COD-OCU-J
                 MOVE 0 TO SW-FIN 
              ELSE
                 ADD 1 TO CONT 
                 INITIALIZE LIN-1
           
                 INSPECT COD-OCU REPLACING ALL '-' BY "0"
                 MOVE COD-OCU    TO COD-OCU-J
           
                 INSPECT NOMBRE-OCU REPLACING ALL '"'  BY " "
                 INSPECT NOMBRE-OCU REPLACING ALL "*"  BY " "
                 INSPECT NOMBRE-OCU REPLACING ALL "Ñ"  BY CARAC-ENE-MAY
                 INSPECT NOMBRE-OCU REPLACING ALL "ñ"  BY CARAC-ENE-MIN

                 MOVE NOMBRE-OCU      TO NOMBRE-OCU-J

                 INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
                 INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES
                 MOVE ","               TO CIERRE-LIN-1
              END-IF   
           END-IF   
           GO TO LEER-ENTIDADES.

       CERRAR-OCUPACIONES.
           INITIALIZE LIN-1
           MOVE LIN-1 TO DATOS-PLANO-W
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO

           MOVE "]}" TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           CLOSE ARCHIVO-OCUPACIONES.

010700 PAGINA-CONFIG.
010720     MOVE "statuscode" TO COBW3-CNV-NAME
010730     MOVE "00"         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010720     MOVE "programa-id" TO COBW3-CNV-NAME
010730     MOVE "SER854"      TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010760     MOVE "..\PAGINAS\RECIBIDOSLISTADO.ASPX" TO SALIDA-HTML
010770     PERFORM ABRIR-HTML.

007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.

       VENTANA-OTROS-DATOS.


       DATOS-ENVIO.
010720     MOVE "datosrecibidos"   TO COBW3-CNV-NAME
010730     MOVE DATOS-PLANO-W      TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_REPEAT" USING COBW3.  

008040 COPY "..\..\FUENTES\SC-WEB19.CBL".
