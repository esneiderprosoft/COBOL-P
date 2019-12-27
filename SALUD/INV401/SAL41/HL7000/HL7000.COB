004820*=================================================================
004830* ACTUALIZA ARCHIVO CONTROL ENVIOS HL7 EN COMPROBANTES
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "..\..\FUENTES\FS-USUNET.CBL".
004920     COPY "..\..\FUENTES\FS-SESION.CBL".


005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".

      

005190 WORKING-STORAGE SECTION.
005200 COPY "..\..\FUENTES\COBW3.CBL".
005210 COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LINEA-LLEGADA-W               PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W             PIC X(15).
             03 FECHA-LLEGAD-W          PIC X(8).
             03 HORA-LLEGAD-W           PIC X(6).
          02 DIR-CONTAB-LLEGADA-W       PIC X(30).
          02 MES-CONTAB-LLEGADA-W       PIC XX.
          02 OPERADOR-LLEGADA-W         PIC X(4).

       01 LLAVE-W.          
          03 SUC-W                      PIC XX.
          03 CL-W                       PIC 9.
          03 NRO-W                      PIC 9(6).
       01 L                             PIC 99.
       01 ESTADO-W                      PIC 9.
       01 FECHA-MSG-W                   PIC 9(6).
       01 NOM-SALIDA                    PIC X(50).
       01 TIPO-W                        PIC X(4).


       01 DATOS-ENVIO.
          02 SW-INVALID-ENV             PIC XX.
          

006310 LINKAGE SECTION.
006320 COPY "..\..\FUENTES\ISAPICTX.CBL".
006330 PROCEDURE DIVISION WITH stdcall LINKAGE USING ISAPI-CTX-CNT.
006340
006350 DECLARATIVES.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUNET.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USU-W                TO MSJ2-HTML
              MOVE "HL7000"                 TO MSJ3-HTML
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
              MOVE "HL7000"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SALIDA.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-SALIDA               TO MSJ2-HTML
              MOVE "HL7000"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.      
006720  END DECLARATIVES.
006730
006740 INICIAR-IIS.
006750     MOVE LOW-VALUE TO COBW3.
006760     MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
006770     CALL "COBW3_INIT" USING COBW3.
006780
006790 LEER-DATO-HTML.
006800     MOVE "datosh" TO COBW3-SEARCH-DATA.
006810     CALL "COBW3_GET_VALUE" USING COBW3.
006820     MOVE COBW3-GET-DATA    TO LINEA-LLEGADA-W.

           UNSTRING LINEA-LLEGADA-W DELIMITED BY "|"
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W,
                    LLAVE-W, L, ESTADO-W, FECHA-MSG-W, NOM-SALIDA, TIPO-W
           END-UNSTRING.

001855     MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.

000797 INICIAR-SESION.
000806     MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.
006840
006900 ABRIR-USUARIO.
006910     INITIALIZE OTR-STAT.
006920     MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
006930
006940     OPEN INPUT ARCHIVO-USUNET
006950     EVALUATE OTR-STAT
006960       WHEN "00"  CONTINUE
006970       WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
006980                  INITIALIZE REG-USUNET
006990                  WRITE REG-USUNET
007000                  CLOSE ARCHIVO-USUNET
007010                  MOVE "Validacion de Usuario"       TO MSJ1-HTML
007020                  MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007030                  MOVE "Sc"                          TO MSJ3-HTML
007040                  GO TO ENVIAR2-ERROR
007050       WHEN OTHER GO TO CERRAR-SESION
007060     END-EVALUATE.
007070
007080     INITIALIZE LLAVE-USUNET.

007100
007110 LEER-USUARIO.
007120     READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
007140     CLOSE ARCHIVO-USUNET.
007200     
007210     IF NOMBRE-USUNET = SPACES
007230        MOVE "Validacion de Sesion"        TO MSJ1-HTML
007240        MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007250        MOVE "Sc"                          TO MsJ3-HTML
007260        GO TO ENVIAR2-ERROR
007270     END-IF.
007280 
007350

        ASIGNAR-NOMBRES.           
001614     


        LEER-CONTROL.
            MOVE LLAVE-W    TO LLAVE-HL7C
            MOVE L          TO ITEM-HL7C
            MOVE TIPO-W     TO TIPO-MSG-HL7C

            READ ARCHIVO-CTL-HL7C WITH NO LOCK
                 INVALID KEY
                  IF ESTADO-W = 0
                     MOVE ESTADO-W    TO ESTADO-HL7C
                     MOVE FECHA-MSG-W TO F-COMPR-HL7C
                     MOVE NOM-SALIDA  TO NOMBRE-MSG-HL7C
                     WRITE REG-HL7C END-WRITE
                  ELSE
                     MOVE 01 TO SW-INVALID-ENV
                     GO TO CERRAR-ARCHIVOS
                  END-IF
                NOT INVALID KEY
                     MOVE ESTADO-W  TO ESTADO-HL7C
                     EVALUATE ESTADO-W
                       WHEN 1 MOVE FECHA-MSG-W TO F-ENVIO-HL7C
                       WHEN 2 MOVE FECHA-MSG-W TO F-RECEP-HL7C
                     END-EVALUATE
                     REWRITE REG-HL7C END-REWRITE.

        CERRAR-ARCHIVOS.
           CLOSE ARCHIVO-CTL-HL7C.

010700  PAGINA-CONFIG.
010720      MOVE "datosrecibidos"    TO COBW3-CNV-NAME
010730      MOVE DATOS-ENVIO         TO COBW3-CNV-VALUE
010740      CALL "COBW3_SET_CNV" USING COBW3
010750
010760      MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
010770      PERFORM ABRIR-HTML.
007980   
007990 CERRAR-SESION.
008000      CALL "COBW3_FREE" USING COBW3.
008010      MOVE 1 TO PROGRAM-STATUS.
008020      EXIT PROGRAM.

008030
008040 COPY "..\..\FUENTES\SC-WEB.CBL".

       ELIMINAR-BLANCOS.
           MOVE 0 TO SW-FIN K NRO-CAR-W.
           PERFORM BUSCAR-BLANCO VARYING Y FROM 1 BY 1
                                 UNTIL Y > 599 OR SW-FIN = 1.

       BUSCAR-BLANCO.
           IF  CAR-LIN-A (Y) = SPACES
           AND CAR-LIN-A (Y + 1) = SPACES
           AND CAR-LIN-A (Y + 2) = SPACES
              CONTINUE
           ELSE
              IF  CAR-LIN-A (Y) = SPACES
              AND CAR-LIN-A (Y + 1) = SPACES
                  CONTINUE
              ELSE
                 IF CAR-LIN-A (Y) = SPACES
                 AND (CAR-LIN-A (Y + 1) = "|" OR
                     CAR-LIN-A (Y + 1) = "^")
                     CONTINUE
                 ELSE
                     ADD 1 TO K NRO-CAR-W
                     MOVE CAR-LIN-A (Y) TO CAR-LIN-B (K)
                 END-IF
              END-IF
           END-IF.

           IF CAR-LIN-B (K) = "*"
              MOVE 1 TO SW-FIN
           END-IF.


       ELIMINAR-CEROS.
           MOVE 0 TO SW-FIN K NRO-CAR-W SW-INI.
           PERFORM BUSCAR-CEROS VARYING Y FROM 1 BY 1
                                UNTIL Y > 15 OR SW-FIN = 1.

       BUSCAR-CEROS.
           IF CAR-PACI-A (Y) = 0
              IF SW-INI > 0
                 ADD 1 TO K NRO-CAR-W
                 MOVE CAR-PACI-A (Y) TO CAR-PACI-B (K)
              END-IF
           ELSE
              MOVE CAR-PACI-A (Y) TO SW-INI
              ADD 1 TO K NRO-CAR-W
              MOVE CAR-PACI-A (Y) TO CAR-PACI-B (K)
           END-IF.


005210* COPY "..\..\FUENTES\WS-HCEVO.CBL".
