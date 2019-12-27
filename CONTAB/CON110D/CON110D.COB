      *=================================================================
      *GRABAR CON110D  CONTAB
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".
       
      * "../control/SC-ARCHCOS.DAT"

           SELECT ARCHIVO-COSTO LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-COSTO-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS COD-COSTO
                  FILE STATUS IS OTR-STAT.

      
       DATA DIVISION.
       FILE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-COSTO.CBL".

      
       WORKING-STORAGE  SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".

       77 NOM-PLANO-W               PIC x(90).
       77 NOM-COSTO-W               PIC X(60).
       77 MES-SAL-W                 PIC X(4).
       77 K                         PIC 9(4).
       77 M                         PIC 9(4).
       77 CONT-T                    PIC 9(4).
       77 CONT                      PIC 9(4).
       77 SW-VAR                    PIC 99.

       01 LLEGADA-W              PIC X(500).

       01 TABLA-LLEGADA-W.
          02 DATO-LLEGADA-W      PIC X OCCURS 200.

       01 TABLA-TEMP.
          02 CARAC-TEMP     PIC X OCCURS 50.

       01  VAR-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 IP-SERVER-W               PIC X(13).
           02 DIR-CONTAB-LLEGADA-W      PIC X(8).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 NOV-LLEGADA-W             PIC X.
           02 COD-COST-LLEGADA-W        PIC X(4).
           02 NOMBRE-COST-LLEGADA-W     PIC X(16).
           02 DESCRIP-COST-LLEGADA-W    PIC X(30).
           02 CODIGO-COST-LLEGADA-W     PIC X(11).
           02 DIV-COST-LLEGADA-W        PIC XX.
           02 TABLA-COST-LLEGADA-W.
              05 MAY-COST-LLEGADA-W         OCCURS 7.
                 07 MAY1-COST-LLEGADA-W     PIC XX.  
                 07 MAY2-COST-LLEGADA-W     PIC XX.
           02 OPER-COST-LLEGADA-W.
              05 OPER-CREA-COST-LLEGADA-W   PIC X(4).
              05 FECHA-CREA-COST-LLEGADA-W  PIC X(6).
              05 OPER-MODI-COST-LLEGADA-W   PIC X(4).
              05 FECHA-MODI-COST-LLEGADA-W  PIC X(6).

       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.
          02 FILLER                    PIC X VALUE "|".
          02 DAT1-ENV                  PIC X(30).
          02 FILLER                    PIC X VALUE "|".
          02 DAT2-ENV                  PIC X(20).
      
       LINKAGE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\ISAPICTX.CBL".
       
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
              MOVE "CON110D"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-COSTO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" 
              CONTINUE                    
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE NOM-COSTO-W                TO MSJ2-HTML
              MOVE "CON110D"                  TO MSJ3-HTML
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
              MOVE "CON110D"                 TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO TABLA-LLEGADA-W.
                            
           INITIALIZE SW-FIN J K TABLA-TEMP.
           PERFORM RECORRER-LLEGADA VARYING I FROM 1 BY 1 
                                    UNTIL I > 200.       
      
           IF TABLA-TEMP IS NOT = SPACES OR LOW-VALUES
              PERFORM MOVER-TEMP
           END-IF.
    
           MOVE LLAVESESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.
                                                               
       INICIAR-SESION.
           MOVE "D:\PROGELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W

           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.


       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\PROGELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
       
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
              GO TO ENVIAR-ERROR
           END-IF.

       ASIGNAR-NOMBRES.
           INITIALIZE NOM-COSTO-W.

           EVALUATE MES-CONTAB-LLEGADA-W
             WHEN 01 MOVE "\ENE" TO MES-SAL-W 
             WHEN 02 MOVE "\FEB" TO MES-SAL-W 
             WHEN 03 MOVE "\MAR" TO MES-SAL-W 
             WHEN 04 MOVE "\ABR" TO MES-SAL-W 
             WHEN 05 MOVE "\MAY" TO MES-SAL-W 
             WHEN 06 MOVE "\JUN" TO MES-SAL-W 
             WHEN 07 MOVE "\JUL" TO MES-SAL-W 
             WHEN 08 MOVE "\AGT" TO MES-SAL-W 
             WHEN 09 MOVE "\SEP" TO MES-SAL-W 
             WHEN 10 MOVE "\OCT" TO MES-SAL-W 
             WHEN 11 MOVE "\NOV" TO MES-SAL-W 
             WHEN 12 MOVE "\DIC" TO MES-SAL-W 
           END-EVALUATE

           INITIALIZE NOM-COSTO-W
           MOVE "\\" TO NOM-COSTO-W
           INSPECT NOM-COSTO-W REPLACING FIRST "             "
                                          BY IP-SERVER-W

           INSPECT NOM-COSTO-W REPLACING FIRST "     "
                                          BY    "\PSC$"

           INSPECT NOM-COSTO-W REPLACING FIRST " "
                                          BY "\"

           INSPECT NOM-COSTO-W   REPLACING FIRST "        "
                                            BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT NOM-COSTO-W   REPLACING FIRST  "                       "
                                             BY   "\CONTROL\SC-ARCHCOS.DAT".

       ABRIR-NOMBRE-COST.
           OPEN INPUT ARCHIVO-COSTO.

           EVALUATE OTR-STAT
             WHEN "00" CONTINUE
             WHEN "35" OPEN OUTPUT ARCHIVO-COSTO
                       CLOSE       ARCHIVO-COSTO
                       GO TO ABRIR-NOMBRE-COST
           END-EVALUATE.
 

       LEER-NOMBRE-COST.
           MOVE 00 TO SW-INVALID
           MOVE COD-COST-LLEGADA-W TO COD-COSTO
           READ ARCHIVO-COSTO  WITH NO LOCK
                INVALID KEY 
                  MOVE 01 TO SW-INVALID
           END-READ.

           IF NOV-LLEGADA-W = 7 AND SW-INVALID = 01 GO TO NUEVO.
           IF NOV-LLEGADA-W = 7 AND SW-INVALID = 00 GO TO ERROR-1.
           IF NOV-LLEGADA-W = 8 AND SW-INVALID = 00 GO TO CAMBIO.
           IF NOV-LLEGADA-W = 8 AND SW-INVALID = 01 GO TO ERROR-1.
           IF NOV-LLEGADA-W = 9 AND SW-INVALID = 00 GO TO RETIRO.
           IF NOV-LLEGADA-W = 9 AND SW-INVALID = 01 GO TO ERROR-1.

       NUEVO.
           INITIALIZE REG-COSTO.
           CLOSE ARCHIVO-COSTO.

           OPEN I-O ARCHIVO-COSTO.

           MOVE COD-COST-LLEGADA-W     TO COD-COSTO
           MOVE NOMBRE-COST-LLEGADA-W  TO NOMBRE-COSTO
           MOVE DESCRIP-COST-LLEGADA-W  TO DESCRIP-COSTO
           MOVE CODIGO-COST-LLEGADA-W   TO CODIGO-COSTO

           PERFORM LLENAR-COSTO VARYING M FROM 1 BY 1 
                                UNTIL M > 7.

           MOVE DIV-COST-LLEGADA-W     TO DIV-COSTO
           MOVE OPER-COST-LLEGADA-W    TO OPER-COSTO

           CLOSE ARCHIVO-COSTO.

           OPEN I-O ARCHIVO-COSTO.

           WRITE REG-COSTO END-WRITE

           CLOSE ARCHIVO-COSTO.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO GRABADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       CAMBIO.
           CLOSE ARCHIVO-COSTO.

           OPEN I-O ARCHIVO-COSTO.

           MOVE COD-COST-LLEGADA-W     TO COD-COSTO
           MOVE NOMBRE-COST-LLEGADA-W  TO NOMBRE-COSTO
           MOVE DESCRIP-COST-LLEGADA-W  TO DESCRIP-COSTO
           MOVE CODIGO-COST-LLEGADA-W   TO CODIGO-COSTO

           PERFORM LLENAR-COSTO VARYING M FROM 1 BY 1 
                                UNTIL M > 7.
           
           MOVE DIV-COST-LLEGADA-W     TO DIV-COSTO
           MOVE OPER-COST-LLEGADA-W    TO OPER-COSTO
         
           REWRITE REG-COSTO END-REWRITE

           CLOSE ARCHIVO-COSTO. 
           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO MODIFICADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       RETIRO.
           CLOSE ARCHIVO-COSTO.

           OPEN I-O ARCHIVO-COSTO.

           DELETE ARCHIVO-COSTO END-DELETE

           CLOSE ARCHIVO-COSTO.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO BORRADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       ERROR-1.
           EVALUATE NOV-LLEGADA-W
             WHEN 7 MOVE "88"                           TO DAT0-ENV
                    MOVE "Error - Dato ya Esta Creado"  TO DAT1-ENV
                    MOVE "CON110D"                      TO DAT2-ENV
             WHEN 8 MOVE "88"                           TO DAT0-ENV
                    MOVE "Error - Dato NO Esta Creado"  TO DAT1-ENV
                    MOVE "CON110D"                      TO DAT2-ENV
             WHEN 9 MOVE "88"                           TO DAT0-ENV
                    MOVE "Error - Dato NO Esta Creado"  TO DAT1-ENV
                    MOVE "CON110D"                      TO DAT2-ENV
           END-EVALUATE

           GO TO ENVIO-DATOS.   


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
        
       COPY "P:\PROG\PROYECT\FUENTES\SC-WEB19.CBL".


       RECORRER-LLEGADA.
           IF (DATO-LLEGADA-W(I) = "|")
               PERFORM MOVER-TEMP               
               INITIALIZE J TABLA-TEMP
           ELSE
               ADD 1 TO J
               IF J < 50
                     MOVE DATO-LLEGADA-W(I) TO CARAC-TEMP(J)  
               END-IF
           END-IF.
      
      
       MOVER-TEMP.
           ADD 1 TO K
           EVALUATE K
             WHEN  1 MOVE TABLA-TEMP TO LLAVESESION-LLEGA-W
             WHEN  2 MOVE TABLA-TEMP TO IP-SERVER-W
             WHEN  3 MOVE TABLA-TEMP TO DIR-CONTAB-LLEGADA-W
             WHEN  4 MOVE TABLA-TEMP TO MES-CONTAB-LLEGADA-W
             WHEN  5 MOVE TABLA-TEMP TO NOV-LLEGADA-W
             WHEN  6 MOVE TABLA-TEMP TO COD-COST-LLEGADA-W
             WHEN  7 MOVE TABLA-TEMP TO NOMBRE-COST-LLEGADA-W
             WHEN  8 MOVE TABLA-TEMP TO DESCRIP-COST-LLEGADA-W
             WHEN  9 MOVE TABLA-TEMP TO CODIGO-COST-LLEGADA-W
             WHEN 10 MOVE TABLA-TEMP TO DIV-COST-LLEGADA-W
             WHEN 11 MOVE TABLA-TEMP TO MAY-COST-LLEGADA-W(1)
             WHEN 12 MOVE TABLA-TEMP TO MAY-COST-LLEGADA-W(2)
             WHEN 13 MOVE TABLA-TEMP TO MAY-COST-LLEGADA-W(3)
             WHEN 14 MOVE TABLA-TEMP TO MAY-COST-LLEGADA-W(4)
             WHEN 15 MOVE TABLA-TEMP TO MAY-COST-LLEGADA-W(5)
             WHEN 16 MOVE TABLA-TEMP TO MAY-COST-LLEGADA-W(6)
             WHEN 17 MOVE TABLA-TEMP TO MAY-COST-LLEGADA-W(7)
             WHEN 18 MOVE TABLA-TEMP TO OPER-CREA-COST-LLEGADA-W
             WHEN 19 MOVE TABLA-TEMP TO FECHA-CREA-COST-LLEGADA-W
             WHEN 20 MOVE TABLA-TEMP TO OPER-MODI-COST-LLEGADA-W
             WHEN 21 MOVE TABLA-TEMP TO FECHA-MODI-COST-LLEGADA-W
                     MOVE 1 TO SW-FIN
           END-EVALUATE.

       LLENAR-COSTO.
           IF MAY-COST-LLEGADA-W(M) IS NOT = SPACES OR LOW-VALUES
              MOVE MAY-COST-LLEGADA-W(M)  TO MAY-COSTO (M)
           END-IF.

                                                                    
