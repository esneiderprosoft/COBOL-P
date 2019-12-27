      *=================================================================
      * FACTURACION - CONSULTA DATOS DE TARIFAS 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
  
           SELECT ARCHIVO-TARIFAS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TARIF-W
      *"../CONTROL/SC-ARCHTAR.DAT"
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS COD-TAR.
       
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-TARIF.CBL".

005190 WORKING-STORAGE SECTION.
005200 COPY "..\..\FUENTES\COBW3.CBL".
005210 COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LINEA-LLEGADA-W                 PIC X(100).
       01 NOM-TARIF-W                     PIC X(70).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W               PIC X(15).
             03 FECHA-LLEGAD-W            PIC X(8).
             03 HORA-LLEGAD-W             PIC X(6).
          02 DIR-CONTAB-LLEGADA-W         PIC X(30).
          02 MES-CONTAB-LLEGADA-W         PIC XX.
          02 COD-TAR-LLEGADA-W.                      
             03 COD1-TAR-LLEGADA-W        PIC X.
             03 COD2-TAR-LLEGADA-W        PIC X.
          02 OPERADOR-LLEGADA-W           PIC X(4).

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV               PIC XX.
          02 FILLER                       PIC X      VALUE "|".
          02 DESCRIP-TAR-ENV              PIC X(25).

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
              MOVE "SER108-06"              TO MSJ3-HTML
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
              MOVE "SER108-06"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TARIFAS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-TARIF-W              TO MSJ2-HTML
              MOVE "SER108-06"              TO MSJ3-HTML
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
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W,
                    COD-TAR-LLEGADA-W, OPERADOR-LLEGADA-W
           END-UNSTRING.

001855     MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.
       
000797 INICIAR-SESION.
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
007230        MOVE "Validacion de Sesion"                  TO MSJ1-HTML
007240        MOVE "Falta configurar usuarios"             TO MSJ2-HTML
007250        MOVE "Sc"                                    TO MSJ3-HTML
007260        GO TO ENVIAR2-ERROR
007270     END-IF.

        ASIGNAR-NOMBRES. 
           INITIALIZE NOM-TARIF-W 

           MOVE "\\" TO NOM-TARIF-W

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-TARIF-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-TARIF-W REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-TARIF-W REPLACING FIRST "                              "
                                            BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT NOM-TARIF-W REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHTAR.DAT".

       ABRIR-TARIFAS.
           OPEN INPUT ARCHIVO-TARIFAS.
           MOVE "00"                 TO SW-INVALID-ENV.
       LEER-TARIFAS.
           MOVE COD-TAR-LLEGADA-W    TO COD-TAR
           READ ARCHIVO-TARIFAS WITH NO LOCK
                INVALID KEY 
                  MOVE "01"          TO SW-INVALID-ENV               
           END-READ

           MOVE DESCRIP-TAR          TO DESCRIP-TAR-ENV.

        CERRAR-TARIFAS.
           CLOSE ARCHIVO-TARIFAS.

010700  PAGINA-CONFIG.           
010720     MOVE "datosrecibidos" TO COBW3-CNV-NAME
010730     MOVE DATOS-ENVIO       TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV"  USING COBW3
010750
010760     MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
010770     PERFORM ABRIR-HTML.
007980   
007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.

008030
008040 COPY "..\..\FUENTES\SC-WEB.CBL".
