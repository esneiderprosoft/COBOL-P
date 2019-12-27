      *=================================================================
      * CONSULTA ARCHIVO DE PACIENTES - SER110C 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
  
         
       DATA DIVISION.
       FILE SECTION.

       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".

005190 WORKING-STORAGE SECTION.
005200 COPY "..\..\FUENTES\COBW3.CBL".
005210 COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 ACUM-W                          PIC 9(8).
       77 DIVI-W                          PIC 99V99.
       77 FACTOR-W                        PIC 99.
       01 NIT-TAB.
          02 CAR-TAB                      PIC 9 OCCURS 10.

       01 RESID-W.
          02 ENT-W                        PIC 99V.
          02 DEC1-W                       PIC 9.
          02 DEC2-W                       PIC 9.


       01 LINEA-LLEGADA-W                 PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W               PIC X(15).
             03 FECHA-LLEGAD-W            PIC X(8).
             03 HORA-LLEGAD-W             PIC X(6).
          02 DIR-CONTAB-LLEGADA-W         PIC X(8).
          02 MES-CONTAB-LLEGADA-W         PIC XX.
          02 OPERADOR-LLEGADA-W        PIC X(4).
          02 COD-TERCERO-LLEGADA-W.
             03 COD-TER-W                 PIC 9(10).
          02 DVTERLLEGADAW                PIC 9.

  
       01 DATOS-ENVIO.
          02 DVTERLLEGADAW-ENVIO          PIC X.         
      
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
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-USU-W                 TO MSJ2-HTML
              MOVE "SER110C-01"              TO MSJ3-HTML
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
              MOVE "SER110C-01"              TO MSJ3-HTML
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
                    OPERADOR-LLEGADA-W, COD-TERCERO-LLEGADA-W, DVTERLLEGADAW 
           END-UNSTRING.

001855     MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.

000797 INICIAR-SESION.
000806     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W

      *     MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.
006840
006900 ABRIR-USUARIO.
006910     INITIALIZE OTR-STAT.
006920     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W

      *     MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
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
007250        MOVE "Sc"                          TO MSJ3-HTML
007260        GO TO ENVIAR2-ERROR
007270     END-IF.
007280 
007350

        ASIGNAR-NOMBRES. 

      *     INITIALIZE NOM-PACIE-W 
           
      *     INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".
           
      *     MOVE "\\" TO NOM-PACIE-W 

      *     INSPECT NOM-PACIE-W REPLACING FIRST "                    "
      *                                      BY IP-DATOS-USUNET

      *     INSPECT NOM-PACIE-W REPLACING FIRST " "
      *                                      BY "\"

      *     INSPECT NOM-PACIE-W REPLACING FIRST "        "
      *                                      BY DIR-CONTAB-LLEGADA-W

      *     INSPECT NOM-PACIE-W REPLACING FIRST "                     "
      *                                      BY "\CONTROL\SC-PACIE.DAT".

       ABRIR-ARTICULOS.
            MOVE COD-TERCERO-LLEGADA-W   TO NIT-TAB.
000037        COMPUTE ACUM-W = (CAR-TAB (10) * 3)
000038                     + (CAR-TAB (9)  * 7)
000039                     + (CAR-TAB (8)  * 13)
000040                     + (CAR-TAB (7)  * 17)
000041                     + (CAR-TAB (6)  * 19)
000042                     + (CAR-TAB (5)  * 23)
000043                     + (CAR-TAB (4)  * 29)
000044                     + (CAR-TAB (3)  * 37)
000045                     + (CAR-TAB (2)  * 41)
000046                     + (CAR-TAB (1)  * 43).
000047
000048
000049      COMPUTE DIVI-W ROUNDED = ACUM-W / 11.
000050
000051      MOVE DIVI-W      TO RESID-W.
000052
000053      IF DEC2-W > 0
000054         COMPUTE FACTOR-W = DEC1-W + 1
000055      ELSE
000056         MOVE DEC1-W TO FACTOR-W
000057      END-IF.
000058
000059      IF (FACTOR-W = 0 OR 1)
000060         MOVE FACTOR-W TO DVTERLLEGADAW
000061      ELSE
000062         COMPUTE DVTERLLEGADAW = 11 - FACTOR-W.

            MOVE DVTERLLEGADAW  TO DVTERLLEGADAW-ENVIO.

       PAGINA-CONFIG.
           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO       TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"  USING COBW3
      
           MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
           PERFORM ABRIR-HTML.
         
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.

      
       COPY "..\..\FUENTES\SC-WEB.CBL".
      * COPY "..\..\FUENTES\WS-HCEVO.CBL".


