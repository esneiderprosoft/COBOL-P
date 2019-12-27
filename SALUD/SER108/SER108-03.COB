      *=================================================================
      * FACTURACION - CONSULTA DATOS DE PREFIJOS
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

           SELECT ARCHIVO-PREFIJOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PREFI-W 
      * "../control/SC-ARCHPREF.DAT"
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS LLAVE-PREF
                  FILE STATUS IS OTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-PREFI.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".

005190 WORKING-STORAGE SECTION.
005200 COPY "..\..\FUENTES\COBW3.CBL".
005210 COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-USUAR-W                     PIC X(70).
       77 NOM-PREFI-W                     PIC X(70).

       01 SW-PARE                         PIC 9.

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W               PIC X(15).
             03 FECHA-LLEGAD-W            PIC X(8).
             03 HORA-LLEGAD-W             PIC X(6).
          02 DIR-CONTAB-LLEGADA-W         PIC X(30).
          02 MES-CONTAB-LLEGADA-W         PIC XX.
          02 OPERADOR-LLEGADA-W           PIC X(4).
          02 PREFIJO-W                    PIC X.
       
       01 DATOS-ENVIO.
          02 SW-INVALID-ENV               PIC X(2).            
          02 FILLER                       PIC X VALUE "|".
          02 SW-PARE-ENV                  PIC 9.
          02 FILLER                       PIC X      VALUE "|".
          02 CLAVE-MEN-INV-ENV            PIC X(6).         
          02 FILLER                       PIC X      VALUE "|".
          02 CLAVE-MEN-USU-ENV            PIC X(6).

       01 LINEA-LLEGADA-W                 PIC X(100).

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
              MOVE "SER108-03"              TO MSJ3-HTML
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
              MOVE "SER108-03"              TO MSJ3-HTML
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
              MOVE "SER108-03"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PREFIJOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PREFI-W              TO MSJ2-HTML
              MOVE "SER108-03"              TO MSJ3-HTML
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
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W, PREFIJO-W
           END-UNSTRING.

001855     MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.

000797 INICIAR-SESION.

           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W

      *     MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W

000806    
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.
006840
006900 ABRIR-USUARIO.
006910     INITIALIZE OTR-STAT.
006920     
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W

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

           INITIALIZE NOM-USUAR-W NOM-PREFI-W 

           MOVE "\\" TO NOM-USUAR-W NOM-PREFI-W

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-USUAR-W REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-USUAR-W REPLACING FIRST "                              " 
                                           BY DIR-CONTAB-LLEGADA-W.
           
           MOVE NOM-USUAR-W    TO NOM-PREFI-W

           INSPECT NOM-USUAR-W REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHUSU.DAT"

           INSPECT NOM-PREFI-W REPLACING FIRST "                        "
                                            BY "\CONTROL\SC-ARCHPREF.DAT".

       ABRIR-ARCHIVOS.

           OPEN INPUT ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.

           OPEN INPUT ARCHIVO-PREFIJOS.

           MOVE "00"  TO SW-INVALID-ENV.

           READ ARCHIVO-PREFIJOS NEXT WITH NO LOCK AT END
                CLOSE ARCHIVO-PREFIJOS           
           END-READ.

           INITIALIZE SW-PARE.
           PERFORM ASIGNAR-RESOL VARYING J FROM 1 BY 1
                   UNTIL J > 8 OR SW-PARE = 1.

       ASIGNAR-RESOL.
           IF COD1-PREF (J) IS NOT = SPACES
           AND COD1-PREF (J) = PREFIJO-W
               MOVE 1 TO SW-PARE.

           
       DATOS.
          
           MOVE CLAVE-MEN-INV  TO CLAVE-MEN-INV-ENV
           MOVE CLAVE-MEN-USU  TO CLAVE-MEN-USU-ENV
           MOVE SW-PARE        TO SW-PARE-ENV.

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
005210* COPY "..\..\FUENTES\WS-HCEVO.CBL".


