      *=================================================================
      *ENVIAR COMPLETO ARCHIVO-ENFERMEDADES  SALUD
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".                                                         
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
       COPY "..\..\FUENTES\FS-ENFER.CBL".  
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-ENFER.CBL".

       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".

       01 DATOS-PLANO-W              PIC X(1024).

       01 TIPO-ENF-STOCK           PIC 9.

       01 LIN-1.
          02 FILLER                  PIC X(12)  VALUE "{*TIPO_ENF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-ENF-J              PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*COD_ENF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD_ENF-J               PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*NOMBRE_ENF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOMBRE-ENF-J            PIC X(78).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*SEXO_ENF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SEXO-ENF-J              PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*EDAD_MIN_ENF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EDAD-MIN-ENF-J          PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*EDAD_MAX_ENF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EDAD-MAX-ENF-J          PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*UNID_EDAD_ENF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UNID-EDAD-ENF-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*GRUPO_ENF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 GRUPO-ENF-J             PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*SUBGRUPO_ENF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SUB-GRUPO-ENF-J         PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*COD_MORB_ENF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD_MORB_ENF-J          PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*SUB_MORB_ENF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SUB_MORB_ENF-J          PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*CONF_ENF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CONF_ENF-J              PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*MOV_SN_ENF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 MOV_SN_ENF-J            PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*HUERFA_ENF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HUERFA-ENF-J            PIC X.
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-LIN-1            PIC X.       

       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.
          02 FILLER                    PIC X VALUE "|".
          02 DAT1-ENV                  PIC X(500).

      
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
              MOVE "SER851"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ENFERMEDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-ENFER-LNK               TO MSJ2-HTML
              MOVE "SER851"                TO MSJ3-HTML
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
              MOVE "SER851"                TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO LINEA-LLEGAD-W.

           UNSTRING LINEA-LLEGAD-W DELIMITED BY "|"
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W, TIPO-ENF-STOCK
           END-UNSTRING.

           MOVE SESION-LLEGAD-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.
        
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
                        MOVE "SC-2"                      TO MSJ1-HTML
                        MOVE "Falta configurar usuario"  TO MSJ2-HTML
                        MOVE "SER851"                   TO MSJ3-HTML
                        GO TO ENVIAR2-ERROR
             WHEN OTHER GO TO CERRAR-SESION
           END-EVALUATE.
      
           INITIALIZE LLAVE-USUNET.

       LEER-USUARIO.
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
           CLOSE ARCHIVO-USUNET.
      
           IF NOMBRE-USUNET = SPACES
              MOVE "SC-2"                      TO MSJ1-HTML
              MOVE "Falta configurar usuario"  TO MSJ2-HTML
              MOVE "SER851"                   TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.
           INITIALIZE NOM-ENFER-LNK

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           
           MOVE "\\" TO NOM-ENFER-LNK

           INSPECT NOM-ENFER-LNK REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET        

           INSPECT NOM-ENFER-LNK REPLACING FIRST "                          "
                                           BY "\PROG\DATOS\SC-ARCHENF.DAT".
                                                                                                                                                                                                            

       ABRIR-RX.
           OPEN INPUT ARCHIVO-ENFERMEDAD.
           INITIALIZE REG-ENF.
           MOVE "{*ENFERMEDADES*:["       TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO.
           MOVE 0 TO SW-FIN.
           MOVE 2 TO TIPO-ENF-STOCK.

       LEER-ENFERMEDAD.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-PLANO-W
              PERFORM DATOS-ENVIO
           END-IF
           
           READ ARCHIVO-ENFERMEDAD NEXT WITH NO LOCK AT END GO TO CERRAR-PLANO.
        

           IF COD-ENF = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
              GO TO LEER-ENFERMEDAD
           END-IF 


           IF TIPO-ENF IS NOT = TIPO-ENF-STOCK
              MOVE 0 TO SW-FIN
              GO TO LEER-ENFERMEDAD
           END-IF

           MOVE TIPO-ENF      TO TIPO-ENF-J
           MOVE COD-ENF       TO COD_ENF-J

           INSPECT NOMBRE-ENF REPLACING ALL "*" BY " "
           INSPECT NOMBRE-ENF REPLACING ALL '"' BY " "
           INSPECT NOMBRE-ENF REPLACING ALL "'" BY "x"
           INSPECT NOMBRE-ENF REPLACING ALL "*" BY "x"

           MOVE NOMBRE-ENF    TO NOMBRE-ENF-J
           MOVE SEXO-ENF      TO SEXO-ENF-J
           MOVE EDAD-MIN-ENF  TO EDAD-MIN-ENF-J
           MOVE EDAD-MAX-ENF  TO EDAD-MAX-ENF-J
           MOVE UNID-EDAD-ENF TO UNID-EDAD-ENF-J
           MOVE GRUPO-ENF     TO GRUPO-ENF-J
           MOVE SUBGRUPO-ENF  TO SUB-GRUPO-ENF-J
           MOVE COD-MORB-ENF  TO COD_MORB_ENF-J
           MOVE SUB-MORB-ENF  TO SUB_MORB_ENF-J
           MOVE CONF-ENF      TO CONF_ENF-J
           MOVE MOV-SN-ENF    TO MOV_SN_ENF-J
           MOVE HUERFA-ENF    TO HUERFA-ENF-J

           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES

           MOVE "," TO CIERRE-LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           GO TO LEER-ENFERMEDAD.

       CERRAR-PLANO.
           INITIALIZE LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-1 TO DATOS-PLANO-W
008900     PERFORM DATOS-ENVIO.
008910
008920     MOVE "]}" TO DATOS-PLANO-W
008930     PERFORM DATOS-ENVIO.

           CLOSE ARCHIVO-ENFERMEDAD.

       PAGINA-CONFIG.
           MOVE "statuscode" TO COBW3-CNV-NAME
           MOVE "00"         TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3

           MOVE "programa-id" TO COBW3-CNV-NAME
           MOVE "RX-421W"     TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3

           MOVE "..\PAGINAS\RECIBIDOSLISTADO.ASPX" TO SALIDA-HTML
           PERFORM ABRIR-HTML.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.

       DATOS-ENVIO.
           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE DATOS-PLANO-W    TO COBW3-CNV-VALUE
           CALL "COBW3_SET_REPEAT" USING COBW3.
      
       COPY "..\..\FUENTES\SC-WEB19.CBL".
      

