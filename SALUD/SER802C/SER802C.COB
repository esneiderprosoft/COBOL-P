      *=================================================================
      *CREACION DE ARCHIVO JSON SER802C F8 DE CUPS -SALUD
      *=================================================================  
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  
                                                                                      
           SELECT ARCHIVO-CUPS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-ARCH-CUPS-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS LLAVE-CUP
                  ALTERNATE RECORD KEY IS DESCRIP-CUP WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LLAVE-ALT-CUP WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.
                     
           SELECT ARCHIVO-JSON
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT. 
                                          
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-CUPS.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-PLANO.
           02 DATOS-PLANO     PIC X(1000).

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-PLANO-W                   PIC X(90).
       77 NOM-ARCH-CUPS-W               PIC X(60).

       01 LLEGADA-W                     PIC X(100).

       77 MES-SAL-W                     PIC X(4).

        01  VAR-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 IP-SERVER-W               PIC X(18).
           02 DIR-CONTAB-LLEGADA-W      PIC X(8).
           02 MES-CONTAB-LLEGADA-W      PIC XX.

       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.

       01 LIN-1.  
          02 FILLER                  PIC X(7)   VALUE "{*GRP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 GRP-CUPS-J              PIC X(2).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-CUPS-J              PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)   VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-CUPS-J          PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)   VALUE "*DESCRIP2*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP2-CUPS-J         PIC X(50).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)   VALUE "*LLAVE_ALT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LLAVE-ALT-CUPS-J        PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*TABLA_CTA_CUP*:[".             
          02 TABLA-CONTAB-CUPS-J OCCURS 3.         
             05 FILLER               PIC X      VALUE "*".
             05 CTA-CONTAB-CUPS-J   PIC X(11).
             05 FILLER               PIC X      VALUE "*".
             05 COMA-CUPS-J          PIC X.
          02 FILLER                  PIC X      VALUE "]".
          02 FILLER                  PIC XX     VALUE ",".
          02 FILLER                  PIC X(10)   VALUE "*DIV_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIV2-CUPS-J             PIC X(2).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*COPAGO_MOD_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COPAGO-CUPS-J           PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*NOPOS_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOPOS-CUPS-J            PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)   VALUE "*DURACION_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DURACION-CUPS-J         PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)   VALUE "*NIVEL_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NIVEL-CUPS-J            PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*DIVISION_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIVISION-CUPS-J         PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*NIT_OTR_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NIT-OTR-CUPS-J          PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*MED_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 100-MED-CUPS-J          PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*CTA_OTR_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CTA-OTR-CUPS-J          PIC X(11).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*DIAGN_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIAGN-CUPS-J            PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*SEXO_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SEXO-CUPS-J             PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*EDAD_MIN_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EDAD-MIN-CUPS-J         PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*EDAD_MAX_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EDAD-MAX-CUPS-J         PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*UNID_EDAD_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UNID-EDAD-CUPS-J        PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*DATO_SEP_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DATO-SEP-CUPS-J         PIC X(12).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*PORC_CL_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PORC-CL-CUPS-J          PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*COSTO_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COSTO-CUPS-J            PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*CIS_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CIS-CUPS-J              PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*PORC_OTR_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PORC-OTR-CUPS-J         PIC 99V99.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*OPER_ELAB_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OPER-ELAB-CUPS-J        PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*FECHA_ELAB_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-ELAB-CUPS-J       PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*HORA_ELAB_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HORA-ELAB-CUPS-J        PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*OPER_MOD_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OPER-MOD-CUPS-J         PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*FECHA_MOD_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-MOD-CUPS-J        PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*HORA_MOD_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HORA-MOD-CUPS-J         PIC X(4).
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
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-USU-W                 TO MSJ2-HTML
              MOVE "SER102C"                 TO MSJ3-HTML
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
       END DECLARATIVES.
       
       INICIAR-IIS.
           MOVE LOW-VALUE TO COBW3.
           MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
           CALL "COBW3_INIT" USING COBW3.
      
       LEER-DATO-HTML.
           MOVE "datosh" TO COBW3-SEARCH-DATA.
           CALL "COBW3_GET_VALUE" USING COBW3.
           MOVE COBW3-GET-DATA TO LLEGADA-W.

           UNSTRING LLEGADA-W DELIMITED BY "|"
               INTO LLAVESESION-LLEGA-W, IP-SERVER-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W 
           END-UNSTRING.

           MOVE LLAVESESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.

      *     MOVE "KK"                   TO MSJ1-HTML
      *     MOVE NOM-ARCH-CUPS-W               TO MSJ2-HTML
      *     MOVE "KALETH"                  TO MSJ3-HTML
      *     GO TO ENVIAR2-ERROR.


       INICIAR-SESION.
           MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W

           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.

       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
         
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
           INITIALIZE NOM-ARCH-CUPS-W NOM-PLANO-W.
		   
		   INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".
		   
           MOVE "\\" TO NOM-ARCH-CUPS-W NOM-PLANO-W
		   
           INSPECT NOM-ARCH-CUPS-W REPLACING FIRST "                    "
                                                BY IP-DATOS-USUNET

           INSPECT NOM-ARCH-CUPS-W REPLACING FIRST " "
                                             BY    "\"

           INSPECT NOM-ARCH-CUPS-W REPLACING FIRST "        "
                                                BY DIR-CONTAB-LLEGADA-W.
											   
           INSPECT NOM-ARCH-CUPS-W   REPLACING FIRST "                          "
                                                BY   "PROG\DATOS\SC-ARCHCUPS.DAT".

           INSPECT NOM-PLANO-W  REPLACING FIRST "                    "
                                                BY IP-DATOS-USUNET
 
           INSPECT NOM-PLANO-W  REPLACING FIRST "                                                                "
                                             BY "\PROG\DATOS\JSON\JSC-ARCHCUPS-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"
 
           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                             BY LLAVESESION-LLEGA-W.  


       ABRIR-NOMBRE-CUPS.
           OPEN INPUT ARCHIVO-CUPS.

           EVALUATE OTR-STAT
             WHEN "00" CONTINUE
             WHEN "35" OPEN OUTPUT ARCHIVO-CUPS
                       CLOSE       ARCHIVO-CUPS
                       GO TO ABRIR-NOMBRE-CUPS
           END-EVALUATE.
 
       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-CUPS.
           MOVE "{*CODIGOS*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.
           MOVE 0 TO SW-FIN.

       LEER-NOMBRE-CUPS.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-PLANO
              WRITE REG-PLANO END-WRITE
           END-IF

           READ ARCHIVO-CUPS NEXT WITH NO LOCK AT END 
                GO TO CERRAR-NOMBRE-CUPS
           END-READ.

           IF GRP-CUP = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE
               INITIALIZE LIN-1
               MOVE GRP-CUP       TO GRP-CUPS-J
               MOVE NRO-CUP       TO COD-CUPS-J

               INSPECT DESCRIP-CUP REPLACING ALL '"' BY " "
               INSPECT DESCRIP-CUP REPLACING ALL "*" BY " "
               
               INSPECT DESCRIP2-CUP REPLACING ALL '"' BY " "
               INSPECT DESCRIP2-CUP REPLACING ALL "*" BY " "

               MOVE DESCRIP-CUP    TO DESCRIP-CUPS-J
               MOVE DESCRIP2-CUP   TO DESCRIP2-CUPS-J
               MOVE LLAVE-ALT-CUP  TO LLAVE-ALT-CUPS-J
              
               PERFORM LEER-CTA-CUP  VARYING I FROM 1 BY 1
                                               UNTIL I > 3

               MOVE SPACES TO COMA-CUPS-J (3)
               
               MOVE DIV2-CUP       TO  DIV2-CUPS-J
               MOVE COPAG-MOD-CUP  TO  COPAGO-CUPS-J
               MOVE NOPOS-CUP      TO  NOPOS-CUPS-J
               MOVE DURACION-CUP   TO  DURACION-CUPS-J
               MOVE NIVEL-CUP      TO  NIVEL-CUPS-J
               MOVE DIVISION-CUP   TO  DIVISION-CUPS-J
               MOVE NIT-OTR-CUP    TO  NIT-OTR-CUPS-J
               MOVE 100-MED-CUP    TO  100-MED-CUPS-J
               MOVE CTA-OTR-CUP    TO  CTA-OTR-CUPS-J
               MOVE DIAGN-CUP      TO  DIAGN-CUPS-J
      *         MOVE PORCENT-CUP   TO  PORCENT-CUPS-J
               MOVE SEXO-CUP       TO  SEXO-CUPS-J
               MOVE EDAD-MIN-CUP   TO  EDAD-MIN-CUPS-J
               MOVE EDAD-MAX-CUP   TO  EDAD-MAX-CUPS-J
               MOVE UNID-EDAD-CUP  TO  UNID-EDAD-CUPS-J
	       MOVE DATOS-SEP-CUP  TO  DATO-SEP-CUPS-J
               MOVE PORC-CL-CUP    TO  PORC-CL-CUPS-J 
               MOVE COSTO-CUP      TO  COSTO-CUPS-J
               MOVE CIS-CUP        TO  CIS-CUPS-J
               MOVE PORC-OTR-CUP   TO  PORC-OTR-CUPS-J
               MOVE OPER-ELAB-CUP  TO  OPER-ELAB-CUPS-J
               MOVE FECHA-ELAB-CUP TO  FECHA-ELAB-CUPS-J
               MOVE HORA-ELAB-CUP  TO  HORA-ELAB-CUPS-J
               MOVE OPER-MOD-CUP   TO  OPER-MOD-CUPS-J
               MOVE FECHA-MOD-CUP  TO  FECHA-MOD-CUPS-J
               MOVE HORA-MOD-CUP   TO  HORA-MOD-CUPS-J
               


               INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
               MOVE "," TO CIERRE-LIN-1          
           END-IF

           GO TO LEER-NOMBRE-CUPS.

       CERRAR-NOMBRE-CUPS.
           INITIALIZE LIN-1

           PERFORM LEER-COMAS-CUP VARYING I FROM 1 BY 1 
                                         UNTIL I > 3
           MOVE SPACES TO COMA-CUPS-J (3)

           MOVE LIN-1 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE
      
           MOVE "]}" TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE
                
           CLOSE ARCHIVO-CUPS
                 ARCHIVO-JSON.      

       ENVIO-DATOS.
           MOVE "datosrecibidos"  TO COBW3-CNV-NAME
           MOVE 00                TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"   USING COBW3

           MOVE "..\..\FRAMEWORKS\PAGINAS\RECIBIDOS.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.


       LEER-CTA-CUP.
           IF CTA-CONTAB-CUP (I) IS NOT = SPACES OR LOW-VALUES
              PERFORM LLENAR-CTA-CUP              
           END-IF.
         
       LLENAR-CTA-CUP.  
           MOVE CTA-CONTAB-CUP (I) TO CTA-CONTAB-CUPS-J (I).
           MOVE ","           TO COMA-CUPS-J (I).

       LEER-COMAS-CUP.                                               
           MOVE ","           TO COMA-CUPS-J (I).
      



       
       COPY "..\..\FUENTES\SC-WEB19.CBL".
        
