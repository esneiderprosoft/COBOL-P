      *============================================================
      * CONSULTA SALDO DE ORDENES DE COMPRA POR LISTAR 9-8-1-5-4 
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  


      * TO "../CONTROL/SC-ARCHUSU.DAT"
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

      *  "../control/SC-ARCHCOS.DAT"
           SELECT ARCHIVO-COSTO LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-COSTO-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS COD-COSTO
                  FILE STATUS IS OTR-STAT.

      * "../control/SC-ORDEN.DAT"
            SELECT ARCHIVO-ORDEN LOCK MODE IS AUTOMATIC
                  ASSIGN  NOM-ORDEN-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-ORD
                  FILE STATUS IS OTR-STAT.

      * "SC-MOVINV.DAT"
           SELECT MOVIMIENTO-INVENT LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-INV-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-INV
                  ALTERNATE RECORD KEY COD-ART-INV   WITH DUPLICATES
                  ALTERNATE RECORD KEY SECUENCIA-INV
                  ALTERNATE RECORD KEY NIT-INV           WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-INV         WITH DUPLICATES
                  ALTERNATE RECORD KEY LLAVE-DOC-CTL-INV WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


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

      * "../control/SC-ORDEN2.DAT"
           SELECT ARCHIVO-ORDEN2 LOCK MODE IS AUTOMATIC
                  ASSIGN  NOM-ORDEN2-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-ORD2
                  FILE STATUS IS OTR-STAT.

                                                        
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-TERCE.CBL".
       COPY "..\..\FUENTES\FD-COSTO.CBL".
       COPY "..\..\FUENTES\FD-INVEN.CBL".
       COPY "..\..\FUENTES\FD-ORDEN2.CBL".
       COPY "..\..\FUENTES\FD-ORDEN.CBL".
       COPY "..\..\FUENTES\FD-ARTIC.CBL".

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 TABLA-LLEGADA-W.
          02 DATO-LLEGADA-W PIC X OCCURS 200.
       
       01 LLEGADA-W         PIC X(100).

       01 TABLA-TEMP.
          02 CARAC-TEMP     PIC X OCCURS 50.


       77 NOM-USUAR-W               PIC X(60).
       77 NOM-TER-W                 PIC X(60).
       77 NOM-COSTO-W               PIC X(60).
       77 NOM-ORDEN-W               PIC X(60).
       77 NOM-INV-W                 PIC X(60).
       77 NOM-ART-W                 PIC X(60).
       77 NOM-ORDEN2-W              PIC X(60).

       77 MES-SAL-W                 PIC X(4).
       77 CONT                      PIC 9999.


       77  ORD-INI-W                  PIC 9(6).
       77  ORD-FIN-W                  PIC 9(6).
       77  SW-LN                      PIC 99.
       77  TAR-EDIT                   PIC ZZ.
       77  COSTO-W                    PIC XX.

       77  CANT2-L                  PIC Z,ZZZ,ZZZ,ZZZ.ZZ.
       77  CANT2-CSV                PIC -ZZZZZZZZZZZZZZZ.
       77  CANT2-EDIT               PIC X(16).

       01  VARIABLES.
           02 SW9                     PIC 9.
           02 ORD-STAT                PIC XX.
           02 VLR-EDIT                PIC ZZZ,ZZZ,ZZZ-.
           02 VALOR-T-EDIT            PIC ZZZZ,ZZZ,ZZZ-.
           02 VALOR-UNIT-EDIT         PIC ZZZ,ZZZ,ZZZ.ZZ.
           02 CANT-EDIT               PIC -ZZ,ZZZ,ZZZ.ZZ.
           02 CONTA-LN                PIC 99.
           02 TOTAL-ORDEN             PIC S9(11).
           02 TOTAL-W                 PIC S9(11).
           02 VLR-IVA-W               PIC S9(11)V99.
           02 TAR-IVA-W               PIC 99.
           02 CANT-TOT                PIC S9(7)V99.
           02 VALOR-TOT               PIC S9(9).
           02 VALOR-UNIT              PIC S9(9)V99.

       
       01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 DIR-CONTAB-LLEGADA-W      PIC X(30).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 OPERADOR-LLEGADA-W        PIC X(4). 



       01 DATOS-ENVIO.
          02 SW-INVALID                 PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 NOMBRE-ENV                 PIC X(28).

      
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
              MOVE "ARCHIVO-USUNET"         TO MSJ2-HTML
              MOVE "SER101"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
          
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "ARCHIVO-SESION"         TO MSJ2-HTML
              MOVE "SER101"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-COSTO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"         
              CONTINUE            
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE "INV495U-01"               TO MSJ2-HTML
              MOVE NOM-COSTO-W                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF. 


       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ORDEN.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"         
              CONTINUE            
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE "INV495U-01"               TO MSJ2-HTML
              MOVE NOM-ORDEN-W                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"         
              CONTINUE            
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE "INV495U-01"               TO MSJ2-HTML
              MOVE NOM-USUAR-W                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TERCEROS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"         
              CONTINUE            
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE "INV495U-01"               TO MSJ2-HTML
              MOVE NOM-TER-W                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MOVIMIENTO-INVENT.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"         
              CONTINUE            
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE "INV495U-01"               TO MSJ2-HTML
              MOVE NOM-INV-W                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MAESTRO-ARTICULOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"         
              CONTINUE            
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE "INV495U-01"               TO MSJ2-HTML
              MOVE NOM-ART-W                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.   

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ORDEN2.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"         
              CONTINUE            
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE "INV495U-01"               TO MSJ2-HTML
              MOVE NOM-ORDEN2-W               TO MSJ3-HTML
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
              INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W
           END-UNSTRING.

           MOVE LLAVESESION-LLEGA-W   TO LLAVE-SESION-W.

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
           INITIALIZE NOM-USUAR-W NOM-TER-W NOM-COSTO-W NOM-ORDEN-W NOM-INV-W NOM-ART-W NOM-ORDEN2-W

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           MOVE "\\" TO NOM-USUAR-W NOM-TER-W NOM-COSTO-W NOM-ORDEN-W NOM-INV-W NOM-ART-W NOM-ORDEN2-W

           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET
           INSPECT NOM-USUAR-W REPLACING FIRST " "
                                             BY "\"

           INSPECT NOM-USUAR-W   REPLACING FIRST "                              "
                                               BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT NOM-USUAR-W   REPLACING FIRST "         "
                                              BY "\CONTROL\".
                                                            

           MOVE NOM-USUAR-W TO NOM-TER-W NOM-COSTO-W NOM-ORDEN-W NOM-INV-W NOM-ART-W NOM-ORDEN2-W

           INSPECT NOM-USUAR-W   REPLACING FIRST "             "
                                              BY "SC-ARCHUSU.DAT"

           INSPECT NOM-TER-W   REPLACING FIRST "              "
                                            BY "SC-ARCHTER.DAT"

           INSPECT NOM-COSTO-W   REPLACING FIRST "              "
                                              BY "SC-ARCHCOS.DAT"

           INSPECT NOM-ORDEN-W   REPLACING FIRST "            "
                                              BY "SC-ORDEN.DAT"

           INSPECT NOM-ORDEN2-W   REPLACING FIRST "             "
                                               BY "SC-ORDEN2.DAT"

           INSPECT NOM-INV-W   REPLACING FIRST "             "
                                            BY "SC-MOVINV.DAT"

           INSPECT NOM-ART-W   REPLACING FIRST "              "
                                            BY "SC-MAESART.DAT".



       INICIAR.
           OPEN INPUT ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT AT END MOVE 0 TO I.
           CLOSE ARCHIVO-USUARIOS.


       ABRIR-ARCHIVOS.
           OPEN INPUT  ARCHIVO-ORDEN
                       ARCHIVO-ORDEN2
                       MAESTRO-ARTICULOS
                       ARCHIVO-TERCEROS
                       ARCHIVO-COSTO
                OUTPUT LISTADO.
 


       LEER-GRUPO-SER.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-JSON  
              WRITE REG-JSON  END-WRITE
           END-IF

           READ ARCHIVO-GR-SER NEXT WITH NO LOCK AT END 
                GO TO CERRAR-GRUPO-SER
           END-READ
          
           
           IF CODIGO-GRSER = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE
              MOVE CODIGO-GRSER   TO COD-GRSER-J
              MOVE DESCRIP-GRSER  TO DESCRIP-GRSER-J
              MOVE PORC-CL-GRSER  TO PORC-CL-GRSER-J
              MOVE PORC-OTR-GRSER TO PORC-OTR-GRSER-J
              MOVE OTR-CTA-GRSER  TO OTR-CT-GRSER-J       
           END-IF.



       CERRAR-ARCHIVOS.               
           CLOSE ARCHIVO-ORDEN
                 ARCHIVO-ORDEN2
                 MAESTRO-ARTICULOS
                 ARCHIVO-TERCEROS
                 ARCHIVO-COSTO
                 LISTADO.      

       ENVIO-DATOS.
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