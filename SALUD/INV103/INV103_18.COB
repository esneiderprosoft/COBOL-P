      *=================================================================
      *CONSULTA MAESTRO DE ARTICULOS 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
       

           SELECT MAESTRO-ARTICULOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-ART-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY COD-ART
                  ALTERNATE RECORD KEY CLASE-ART       WITH DUPLICATES
                  ALTERNATE RECORD KEY COD-BARRAS-ART  WITH DUPLICATES
                  ALTERNATE RECORD KEY DESCRIP-ART     WITH DUPLICATES
                  ALTERNATE RECORD KEY USO-ART         WITH DUPLICATES
                  ALTERNATE RECORD KEY CL-ROTAC-ART    WITH DUPLICATES
                  ALTERNATE RECORD KEY UBICAC-ART      WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-VENCE-ART WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.
              
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-ARTIC.CBL".
      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".
  
       77 NOM-ART-W                            PIC X(70).

       01 LINEA-LLEGADA-W                      PIC X(500).
                                        
       01  DATO-LLEGADA-W.
           02 LLAVE-SESION-LLEGA-W.
              03 ID-LLEGAD-W                   PIC X(15).
              03 FECHA-LLEGAD-W                PIC X(8).
              03 HORA-LLEGAD-W                 PIC X(6).
           02 DIR-CONTAB-LLEGADA-W             PIC X(30).
           02 MES-CONTAB-LLEGADA-W             PIC XX.
           02 COD-ART-LLEGADA-W.
              03 LLAVE-NRO-ART-W.
                 05 LLAVE-GRUPO-ART-W.
                    07 TIPO-ART-W                PIC 9.
                    07 GRUPO-ART-W.
                       09 GRUPO1-ART-W           PIC X.
                       09 GRUPO2-ART-W           PIC X.
                 05 NUMERO-ART-W.
                    07 NRO1-ART-W                PIC XX.
                    07 NRO2-ART-W                PIC X(11).
              03 CLASE-ART-W                     PIC XX.            
           02 OPERADOR-LLEGADA-W               PIC X(4).
                                       
       01 DATOS-ENVIO.
          02 SW-INVALID-ENV              PIC X(2).
          02 FILLER                      PIC X VALUE "|".
          02 DESCRIP-ART-ENV.
             03 DESCRIP-ART1.
                05 INICIAL-ART.
                   07 DESCRIP1-ART       PIC X.
                   07 DESCRIP2-ART       PIC X(3).
                05 RESTO-ART             PIC X(26).
             03 DESCRIP-ART2             PIC X(20).

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
              MOVE "INV103"                 TO MSJ3-HTML
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
              MOVE "INV103"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MAESTRO-ARTICULOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-ART-W                TO MSJ2-HTML
              MOVE "INV103"                 TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO LINEA-LLEGADA-W.

           UNSTRING LINEA-LLEGADA-W DELIMITED BY "|"
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, COD-ART-LLEGADA-W,
                   OPERADOR-LLEGADA-W

           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.

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
              MOVE "Validacion de usuarios"                TO MSJ1-HTML
              MOVE "Falta configurar usuario"              TO MSJ2-HTML
              MOVE "Sc"                                    TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.

           INITIALIZE NOM-ART-W 
           
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           MOVE "\\" TO NOM-ART-W 


           INSPECT NOM-ART-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET

           INSPECT NOM-ART-W REPLACING FIRST " "
                                          BY "\"

           INSPECT NOM-ART-W REPLACING FIRST "                              "
                                          BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT NOM-ART-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-MAESART.DAT".
       ABRIR-MAESTRO-ARTICULOS.
           OPEN INPUT MAESTRO-ARTICULOS.
           MOVE "00"  TO SW-INVALID-ENV.

       LEER-MAESTRO-ARTICULOS.
           MOVE COD-ART-LLEGADA-W  TO COD-ART
           READ MAESTRO-ARTICULOS WITH NO LOCK 
                INVALID KEY
                  MOVE "01"  TO SW-INVALID-ENV
           END-READ.

       CERRAR-MAESTRO-ARTICULOS.            
           MOVE DESCRIP-ART  TO DESCRIP-ART-ENV

           CLOSE MAESTRO-ARTICULOS.

       PAGINA-CONFIG.

           MOVE "datosrecibidos"  TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO       TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"   USING COBW3

           MOVE "..\..\FRAMEWORKS\PAGINAS\RECIBIDOS.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.
        
       COPY "..\..\FUENTES\SC-WEB19.CBL".
      
