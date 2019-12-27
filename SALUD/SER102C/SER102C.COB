      *=================================================================
      *GRABAR SER102C  SALUD
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".

       
      * "/PROG/DATOS/SC-ARCHCUPS.DAT"

           SELECT ARCHIVO-CUPS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-ARCH-CUPS-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS LLAVE-CUP
                  ALTERNATE RECORD KEY IS DESCRIP-CUP WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LLAVE-ALT-CUP WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-CUPS.CBL".


       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-PLANO-W               PIC x(90).
       77 NOM-ARCH-CUPS-W           PIC X(60).
       77 MES-SAL-W                 PIC X(4).
       77 K                         PIC 9(4).
       77 M                         PIC 9(4).
       77 CONT-T                    PIC 9(4).
       77 CONT                      PIC 9(4).
       77 SW-VAR                    PIC 99.

       01 LLEGADA-W              PIC X(500).

       01 TABLA-LLEGADA-W.
          02 DATO-LLEGADA-W      PIC X OCCURS 1000.

       01 TABLA-TEMP.
          02 CARAC-TEMP     PIC X OCCURS 1000.

       01  VAR-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W                PIC X(15).
              03 FECHA-LLEGAD-W             PIC X(8).
              03 HORA-LLEGAD-W              PIC X(6).
           02 DIR-CONTAB-LLEGADA-W          PIC X(8).
           02 MES-CONTAB-LLEGADA-W          PIC XX.
           02 NOV-LLEGADA-W                 PIC X.
           02 GRP-CUP-LLEGADA-W             PIC X(2).
           02 NRO-CUP-LLEGADA-W             PIC X(10).
           02 DESCRIP-CUP-LLEGADA-W         PIC X(80).
           02 TIPO-CUP-LLEGADA-W            PIC 9.
           02 ABREV-CUP-LLEGADA-W           PIC X(5).
           02 TABLA-CTA-LLEGADA-W.
              05 CTA-CUP-LLEGADA-W          OCCURS 3.
                 07 MAY-CUP-LLEGADA-W       PIC X(4).  
                 07 SCTA-CUP-LLEGADA-W      PIC X(2).
                 07 AUXL-CUP-LLEGADA-W      PIC X(5).
           02 DIV2-CUP-LLEGADA-W            PIC X(2).
           02 COPAG-CUP-LLEGADA-W           PIC X.
           02 NOPOS-CUP-LLEGADA-W           PIC X.
           02 DURACION-CUP-LLEGADA-W        PIC X(3).
           02 NIVEL-CUP-LLEGADA-W           PIC X.
           02 DIVISION-CUP-LLEGADA-W        PIC X(2).
           02 NIT-OTR-CUP-LLEGADA-W         PIC X(10).
           02 100-MED-CUP-LLEGADA-W         PIC X.
           02 CTA-OTR-CUP-LLEGADA-W         PIC X(11).
           02 DIAGN-CUP-LLEGADA-W           PIC X.
           02 SEXO-CUP-LLEGADA-W            PIC X.
	   02 EDAD-MIN-CUP-LLEGADA-W        PIC X(3).
           02 EDAD-MAX-CUP-LLEGADA-W        PIC X(3).
           02 UNIDAD-EDAD-CUP-LLEGADA-W     PIC X.
           02 PORC-CL-CUP-LLEGADA-W         PIC X(3).
           02 COSTO-CUP-LLEGADA-W           PIC X(4).
           02 CIS-CUP-LLEGADA-W             PIC X.               
           02 PORC-OTR-CUP-LLEGADA-W        PIC X(4).
           02 OPER-CREA-CUP-LLEGADA-W       PIC X(4).
           02 FECHA-CREA-CUP-LLEGADA-W      PIC X(6).
           02 HORA-ELAB-CUP-LLEGADA-W       PIC X(4).
           02 OPER-MODI-CUP-LLEGADA-W       PIC X(4).
           02 FECHA-MODI-CUP-LLEGADA-W      PIC X(6).
           02 HORA-MODI-CUP-LLEGADA-W       PIC X(4).

       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.
          02 FILLER                    PIC X VALUE "|".
          02 DAT1-ENV                  PIC X(30).
          02 FILLER                    PIC X VALUE "|".
          02 DAT2-ENV                  PIC X(20).
      
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
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-USU-W                 TO MSJ2-HTML
              MOVE "SER102C"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CUPS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" 
              CONTINUE                    
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE NOM-ARCH-CUPS-W            TO MSJ2-HTML
              MOVE "SER102C"                  TO MSJ3-HTML
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
              MOVE "SER102C"                 TO MSJ3-HTML
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
                                    UNTIL I > 500.   

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
           INITIALIZE NOM-ARCH-CUPS-W
           
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

           MOVE "\\" TO NOM-ARCH-CUPS-W

           INSPECT NOM-ARCH-CUPS-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET


           INSPECT NOM-ARCH-CUPS-W REPLACING FIRST " "
                                                BY "\"

           INSPECT NOM-ARCH-CUPS-W REPLACING FIRST  "                          "
                                                BY  "PROG\DATOS\SC-ARCHCUPS.DAT".

       ABRIR-NOMBRE-CUPS.
           OPEN INPUT ARCHIVO-CUPS.

           EVALUATE OTR-STAT
             WHEN "00" CONTINUE
             WHEN "35" OPEN OUTPUT ARCHIVO-CUPS
                       CLOSE       ARCHIVO-CUPS
                       GO TO ABRIR-NOMBRE-CUPS
           END-EVALUATE.
 

       LEER-NOMBRE-CUPS.
  
           MOVE 00 TO SW-INVALID
           MOVE GRP-CUP-LLEGADA-W TO GRP-CUP
           MOVE NRO-CUP-LLEGADA-W TO NRO-CUP
           READ ARCHIVO-CUPS  WITH NO LOCK
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
           INITIALIZE REG-CUPS.
           CLOSE ARCHIVO-CUPS.

           OPEN I-O ARCHIVO-CUPS.

           MOVE GRP-CUP-LLEGADA-W          TO GRP-CUP
           MOVE NRO-CUP-LLEGADA-W          TO NRO-CUP
           MOVE DESCRIP-CUP-LLEGADA-W      TO DESCRIP-CUP
           MOVE TIPO-CUP-LLEGADA-W         TO TIPO-CUP
           MOVE ABREV-CUP-LLEGADA-W        TO ABREV-CUP

           PERFORM LLENAR-CUP VARYING M FROM 1 BY 1 
                                UNTIL M > 3.

           MOVE DIV2-CUP-LLEGADA-W         TO DIV2-CUP
           MOVE COPAG-CUP-LLEGADA-W        TO COPAG-MOD-CUP
           MOVE NOPOS-CUP-LLEGADA-W        TO NOPOS-CUP
           MOVE DURACION-CUP-LLEGADA-W     TO DURACION-CUP
           MOVE NIVEL-CUP-LLEGADA-W        TO NIVEL-CUP           
           MOVE DIVISION-CUP-LLEGADA-W     TO DIVISION-CUP
           MOVE NIT-OTR-CUP-LLEGADA-W      TO NIT-OTR-CUP
           MOVE 100-MED-CUP-LLEGADA-W      TO 100-MED-CUP
           MOVE CTA-OTR-CUP-LLEGADA-W      TO CTA-OTR-CUP
           MOVE DIAGN-CUP-LLEGADA-W        TO DIAGN-CUP
           MOVE SEXO-CUP-LLEGADA-W         TO SEXO-CUP
           MOVE EDAD-MIN-CUP-LLEGADA-W     TO EDAD-MIN-CUP
           MOVE EDAD-MAX-CUP-LLEGADA-W     TO EDAD-MAX-CUP
           MOVE UNIDAD-EDAD-CUP-LLEGADA-W  TO UNID-EDAD-CUP
           MOVE PORC-CL-CUP-LLEGADA-W      TO PORC-CL-CUP
           MOVE COSTO-CUP-LLEGADA-W        TO COSTO-CUP
           MOVE CIS-CUP-LLEGADA-W          TO CIS-CUP
           MOVE PORC-OTR-CUP-LLEGADA-W     TO PORC-OTR-CUP
           MOVE OPER-CREA-CUP-LLEGADA-W    TO OPER-ELAB-CUP
           MOVE FECHA-CREA-CUP-LLEGADA-W   TO FECHA-ELAB-CUP
           MOVE HORA-ELAB-CUP-LLEGADA-W    TO HORA-ELAB-CUP

           CLOSE ARCHIVO-CUPS.

           OPEN I-O ARCHIVO-CUPS.

           WRITE REG-CUPS END-WRITE

           CLOSE ARCHIVO-CUPS.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO GRABADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       CAMBIO.
           CLOSE ARCHIVO-CUPS.

           OPEN I-O ARCHIVO-CUPS.

           MOVE GRP-CUP-LLEGADA-W          TO GRP-CUP
           MOVE NRO-CUP-LLEGADA-W          TO NRO-CUP
           MOVE DESCRIP-CUP-LLEGADA-W      TO DESCRIP-CUP
           MOVE TIPO-CUP-LLEGADA-W         TO TIPO-CUP
           MOVE ABREV-CUP-LLEGADA-W        TO ABREV-CUP

           PERFORM LLENAR-CUP VARYING M FROM 1 BY 1 
                                UNTIL M > 3.

           MOVE DIV2-CUP-LLEGADA-W         TO DIV2-CUP
           MOVE COPAG-CUP-LLEGADA-W        TO COPAG-MOD-CUP
           MOVE NOPOS-CUP-LLEGADA-W        TO NOPOS-CUP
           MOVE DURACION-CUP-LLEGADA-W     TO DURACION-CUP
           MOVE NIVEL-CUP-LLEGADA-W        TO NIVEL-CUP           
           MOVE DIVISION-CUP-LLEGADA-W     TO DIVISION-CUP
           MOVE NIT-OTR-CUP-LLEGADA-W      TO NIT-OTR-CUP
           MOVE 100-MED-CUP-LLEGADA-W      TO 100-MED-CUP
           MOVE CTA-OTR-CUP-LLEGADA-W      TO CTA-OTR-CUP
           MOVE DIAGN-CUP-LLEGADA-W        TO DIAGN-CUP
           MOVE SEXO-CUP-LLEGADA-W         TO SEXO-CUP
           MOVE EDAD-MIN-CUP-LLEGADA-W     TO EDAD-MIN-CUP
           MOVE EDAD-MAX-CUP-LLEGADA-W     TO EDAD-MAX-CUP
           MOVE UNIDAD-EDAD-CUP-LLEGADA-W  TO UNID-EDAD-CUP
           MOVE PORC-CL-CUP-LLEGADA-W      TO PORC-CL-CUP
           MOVE COSTO-CUP-LLEGADA-W        TO COSTO-CUP
           MOVE CIS-CUP-LLEGADA-W          TO CIS-CUP
           MOVE PORC-OTR-CUP-LLEGADA-W     TO PORC-OTR-CUP
      *     MOVE OPER-CREA-CUP-LLEGADA-W    TO OPER-ELAB-CUP
      *     MOVE FECHA-CREA-CUP-LLEGADA-W   TO FECHA-ELAB-CUP
      *     MOVE HORA-ELAB-CUP-LLEGADA-W    TO HORA-ELAB-CUP
           MOVE OPER-MODI-CUP-LLEGADA-W    TO OPER-MOD-CUP
           MOVE FECHA-MODI-CUP-LLEGADA-W   TO FECHA-MOD-CUP
           MOVE HORA-MODI-CUP-LLEGADA-W    TO HORA-MOD-CUP

           CLOSE ARCHIVO-CUPS.

           OPEN I-O ARCHIVO-CUPS.
           REWRITE REG-CUPS END-REWRITE
           CLOSE ARCHIVO-CUPS.
        
           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO MODIFICADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       RETIRO.
           CLOSE ARCHIVO-CUPS.

           OPEN I-O ARCHIVO-CUPS.

           DELETE ARCHIVO-CUPS END-DELETE

           CLOSE ARCHIVO-CUPS.

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
        
       COPY "..\..\FUENTES\SC-WEB19.CBL".


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
             WHEN  2 MOVE TABLA-TEMP TO DIR-CONTAB-LLEGADA-W
             WHEN  3 MOVE TABLA-TEMP TO MES-CONTAB-LLEGADA-W
             WHEN  4 MOVE TABLA-TEMP TO NOV-LLEGADA-W
             WHEN  5 MOVE TABLA-TEMP TO GRP-CUP-LLEGADA-W
             WHEN  6 MOVE TABLA-TEMP TO NRO-CUP-LLEGADA-W
             WHEN  7 MOVE TABLA-TEMP TO DESCRIP-CUP-LLEGADA-W
             WHEN  8 MOVE TABLA-TEMP TO TIPO-CUP-LLEGADA-W
             WHEN  9 MOVE TABLA-TEMP TO ABREV-CUP-LLEGADA-W
             WHEN 10 MOVE TABLA-TEMP TO CTA-CUP-LLEGADA-W(1)           
             WHEN 11 MOVE TABLA-TEMP TO CTA-CUP-LLEGADA-W(2)
             WHEN 12 MOVE TABLA-TEMP TO CTA-CUP-LLEGADA-W(3)
             WHEN 13 MOVE TABLA-TEMP TO DIV2-CUP-LLEGADA-W
             WHEN 14 MOVE TABLA-TEMP TO COPAG-CUP-LLEGADA-W
             WHEN 15 MOVE TABLA-TEMP TO NOPOS-CUP-LLEGADA-W
             WHEN 16 MOVE TABLA-TEMP TO DURACION-CUP-LLEGADA-W
             WHEN 17 MOVE TABLA-TEMP TO NIVEL-CUP-LLEGADA-W
             WHEN 18 MOVE TABLA-TEMP TO DIVISION-CUP-LLEGADA-W
             WHEN 19 MOVE TABLA-TEMP TO NIT-OTR-CUP-LLEGADA-W
             WHEN 20 MOVE TABLA-TEMP TO 100-MED-CUP-LLEGADA-W
             WHEN 21 MOVE TABLA-TEMP TO CTA-OTR-CUP-LLEGADA-W
             WHEN 22 MOVE TABLA-TEMP TO DIAGN-CUP-LLEGADA-W
             WHEN 23 MOVE TABLA-TEMP TO SEXO-CUP-LLEGADA-W
             WHEN 24 MOVE TABLA-TEMP TO EDAD-MIN-CUP-LLEGADA-W
             WHEN 25 MOVE TABLA-TEMP TO EDAD-MAX-CUP-LLEGADA-W
             WHEN 26 MOVE TABLA-TEMP TO UNIDAD-EDAD-CUP-LLEGADA-W
             WHEN 27 MOVE TABLA-TEMP TO PORC-CL-CUP-LLEGADA-W
             WHEN 28 MOVE TABLA-TEMP TO COSTO-CUP-LLEGADA-W
             WHEN 29 MOVE TABLA-TEMP TO CIS-CUP-LLEGADA-W
             WHEN 30 MOVE TABLA-TEMP TO PORC-OTR-CUP-LLEGADA-W
             WHEN 31 MOVE TABLA-TEMP TO OPER-CREA-CUP-LLEGADA-W
             WHEN 32 MOVE TABLA-TEMP TO FECHA-CREA-CUP-LLEGADA-W
             WHEN 33 MOVE TABLA-TEMP TO HORA-ELAB-CUP-LLEGADA-W
             WHEN 34 MOVE TABLA-TEMP TO OPER-MODI-CUP-LLEGADA-W
             WHEN 35 MOVE TABLA-TEMP TO FECHA-MODI-CUP-LLEGADA-W
             WHEN 36 MOVE TABLA-TEMP TO HORA-MODI-CUP-LLEGADA-W
                     MOVE 1 TO SW-FIN
           END-EVALUATE.


       LLENAR-CUP.
           IF CTA-CUP-LLEGADA-W(M) IS NOT = SPACES OR LOW-VALUES
              MOVE CTA-CUP-LLEGADA-W(M)  TO CTA-CONTAB-CUP (M)
           END-IF.

                                                                    
