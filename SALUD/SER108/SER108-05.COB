      *=================================================================
      * FACTURACION - CONSULTA DATOS DE TECEROS 
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
                  ASSIGN NOM-TER-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-TERCERO
                  ALTERNATE RECORD KEY DESCRIP-TER WITH DUPLICATES
                  ALTERNATE RECORD KEY ACT-TER     WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-TERCE.CBL".

005190 WORKING-STORAGE SECTION.
005200 COPY "..\..\FUENTES\COBW3.CBL".
005210 COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LINEA-LLEGADA-W                 PIC X(100).

       01 NOM-USUAR-W                     PIC X(70).
       01 NOM-TER-W                       PIC X(70).
       01 NOM-BUSCAR-W                    PIC X(12).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W               PIC X(15).
             03 FECHA-LLEGAD-W            PIC X(8).
             03 HORA-LLEGAD-W             PIC X(6).
          02 DIR-CONTAB-LLEGADA-W         PIC X(30).
          02 MES-CONTAB-LLEGADA-W         PIC XX.
          02 COD-TERCERO-LLEGADA-W.
             03 COD-TER-W                 PIC 9(10).
          02 OPERADOR-LLEGADA-W           PIC X(4).

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV                PIC XX.         
          02 FILLER                       PIC X      VALUE "|".
          02 COD-TERCERO-ENV.
             03 COD-TER-W                   PIC 9(10).         
          02 FILLER                       PIC X      VALUE "|".
          02 DESCRIP-TER-ENV.
             03 DESCRIP1-TER.
                05 INI1-TER               PIC X.
                05 INI2-TER               PIC X(29).
             03 DESCRIP2-TER              PIC X(20).          
          02 FILLER                       PIC X      VALUE "|".
          02 ACT-TER-ENV                   PIC 99.
          02 FILLER                       PIC X      VALUE "|".
          02 DESCRIP-TER2-ENV.
             03 APEL1-TER2                PIC X(20).
             03 APEL2-TER2                PIC X(20).
             03 NOMB1-TER2                PIC X(30).          
          02 FILLER                       PIC X      VALUE "|".
          02 CONVENIO-TER-ENV.
             03 CONVENIO1-TER             PIC X.
             03 CONVENIO2-TER             PIC X.          
          02 FILLER                       PIC X      VALUE "|".
          02 ZONA-TER-ENV.
             03 ZONA1-TER                 PIC X.
             03 ZONA2-TER                 PIC X.        
          02 FILLER                       PIC X      VALUE "|".
          02 NIT-TER-ENV                  PIC 9(10).
          02 FILLER                       PIC X      VALUE "|".
          02 ENTIDAD-TER-ENV              PIC X(6).
          02 FILLER                       PIC X      VALUE "|".
          02 REFER1-TER-ENV               PIC X(40).

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
              MOVE "SER108-05"              TO MSJ3-HTML
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
              MOVE "SER108-05"              TO MSJ3-HTML
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
              MOVE "SER108-05"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TERCEROS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" 
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-TER-W                TO MSJ2-HTML
              MOVE "SER108-05"              TO MSJ3-HTML
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
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, COD-TERCERO-LLEGADA-W, OPERADOR-LLEGADA-W  
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
           INITIALIZE NOM-TER-W 

           MOVE "\\" TO NOM-TER-W

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-TER-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-TER-W REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-TER-W REPLACING FIRST "                              "
                                            BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT NOM-TER-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-ARCHTER.DAT".

       ABRIR-TERCEROS.
           OPEN INPUT ARCHIVO-TERCEROS.

           MOVE "00"  TO SW-INVALID-ENV.
         
       LEER-TERCERO.

           MOVE COD-TERCERO-LLEGADA-W TO COD-TERCERO

           READ ARCHIVO-TERCEROS WITH NO LOCK
                INVALID KEY
                  MOVE "01"  TO SW-INVALID-ENV                 
           END-READ.

           MOVE COD-TERCERO    TO COD-TERCERO-ENV 
           MOVE DESCRIP-TER    TO DESCRIP-TER-ENV
           MOVE ACT-TER        TO ACT-TER-ENV           
           MOVE DESCRIP-TER2   TO DESCRIP-TER2-ENV
           MOVE CONVENIO-TER   TO CONVENIO-TER-ENV
           MOVE ZONA-TER       TO ZONA-TER-ENV
           MOVE NIT-TER        TO NIT-TER-ENV
           MOVE ENTIDAD-TER    TO ENTIDAD-TER-ENV
           MOVE REFER1-TER     TO REFER1-TER-ENV.
        CERRAR-TERCEROS.
           CLOSE ARCHIVO-TERCEROS.

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