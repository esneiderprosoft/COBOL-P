      *=================================================================
      *ENVIAR COMPLETO ARCHIVO-RESULTADOS-RX  SALUD
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".                                                         
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
       COPY "..\..\FUENTES\FS-RESRX.CBL".  
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-RESRX.CBL".

       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".

       01 DATOS-PLANO-W              PIC X(1024).
       01 ID-HISTORIA-LLEGADA-W      PIC X(15).
       
       01 CEDULA-Z                  PIC ZZZZZZZZZZ.

       01 TABLA-EDIT.
          02 TABLA-EDIT-W            PIC X(1024)  OCCURS 12.

       01 LIN-1.
          02 FILLER                  PIC X(14)  VALUE "{*FECHA_FACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-FACT-J            PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*SUC_FACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SUC-FACT-J              PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*CL_FACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLASE-FACT-J            PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*NRO_FACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NRO-FACT-J              PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*GRUPO_FACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 GRUPO-FACT-J            PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*COD_ART_FACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-ART-FACT-J          PIC X(13).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*CLASE_ART_FACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLASE-ART-FACT-J        PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*ITEM_FACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ITEM-FACT-J             PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*DESCRIP_CUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-CUP-J           PIC X(60).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*NIT_USU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NIT-USU-J               PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(19)  VALUE "*ID_HISTORIA_FACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ID-HISTORIA-FACT-J      PIC X(15).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*DESCRIP_PACI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-PACI-J          PIC X(54).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*ID_RADIOLOGO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ID-RADIOLOGO-J          PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*NOM_MEDICO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOM-MEDICO-J            PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*REG_MEDICO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 REG-MEDICO-J            PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*ID_TECNOLOGO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ID-TECNOLOGO-J          PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*NOM_TECNOLOGO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOM-TECNOLOGO-J         PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*SALA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SALA-J                  PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*EDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EDAD-J                  PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*CTA_NUM*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CTA-NUM-J               PIC X(7).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*ID_ENTIDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ID-ENTIDAD-J            PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(5)   VALUE "*DX*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DX-J                    PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*TIPO_DX*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-DX-J               PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*DESCRIP_DX*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-DX-J            PIC X(80).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*ADMI_MODIF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ADMI-MODIF-J            PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*FECHA_MODIF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-MODIF-J           PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*HORA_MODIF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HORA-MODIF-J            PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*ADMI_TRANS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ADMI-TRANS-J            PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*FECHA_TRANS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-TRANS-J           PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*HORA_TRANS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HORA-TRANS-J            PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*BIRADS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 BIRADS-J                PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*COMPLEJIDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COMPLEJIDAD-J           PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*NORMALIDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NORMALIDAD-J            PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*EMAIL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EMAIL-J                 PIC X(60).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*FECHA_EMAIL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-EMAIL-J           PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*HORA_EMAIL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HORA-EMAIL-J            PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ARCHIVO_MSG*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ARCHIVO-MSG-J           PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*RESULTADO_PPAL*:".

       

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
              MOVE "RX-421W"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON RESULTADOS-RX.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-RES-RX               TO MSJ2-HTML
              MOVE "RX-421W"                TO MSJ3-HTML
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
              MOVE "RX-421W"                TO MSJ3-HTML
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
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W, ADMIN-W, ID-HISTORIA-LLEGADA-W
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
                        MOVE "RX-421W"                   TO MSJ3-HTML
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
              MOVE "RX-421W"                   TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.
           INITIALIZE NOM-RES-RX

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           
           MOVE "\\" TO NOM-RES-RX

           INSPECT NOM-RES-RX REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET        

           INSPECT NOM-RES-RX REPLACING FIRST "                        "
                                           BY "\PROG\DATOS\SC-RESRX.DAT".
                                                                                                                                                                                                            

       ABRIR-RX.
           OPEN INPUT RESULTADOS-RX.

           MOVE ID-HISTORIA-LLEGADA-W TO ID-HISTORIA-FACT-RX
           START RESULTADOS-RX KEY = ID-HISTORIA-FACT-RX
                INVALID KEY
                   MOVE "SC-1"     TO MSJ1-HTML
                   MOVE "01"       TO MSJ2-HTML
                   MOVE "RX-421W"  TO MSJ3-HTML
                   GO TO ENVIAR2-ERROR
           END-START.

           MOVE "{*RESULTADOS-RX*:["       TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO.

       LEER-RX.
           READ RESULTADOS-RX NEXT WITH NO LOCK AT END GO TO CERRAR-PLANO.

           IF ID-HISTORIA-LLEGADA-W = ID-HISTORIA-FACT-RX
              CONTINUE
           ELSE
              GO TO CERRAR-PLANO
           END-IF
           INITIALIZE LIN-1
        
           MOVE FECHA-FACT-RX      TO FECHA-FACT-J
           MOVE SUC-FACT-RX        TO SUC-FACT-J
           MOVE CL-FACT-RX         TO CLASE-FACT-J
           MOVE NRO-FACT-RX        TO NRO-FACT-J
           MOVE GRUPO-FACT-RX      TO GRUPO-FACT-J



           MOVE COD-ART-FACT-RX    TO COD-ART-FACT-J
           MOVE CLASE-ART-FACT-RX  TO CLASE-ART-FACT-J
           MOVE ITEM-FACT-RX       TO ITEM-FACT-J
           MOVE DESCRIP-CUP-RX     TO DESCRIP-CUP-J
           MOVE NIT-USU-RX         TO NIT-USU-J
           MOVE ID-HISTORIA-FACT-RX TO ID-HISTORIA-FACT-J
           MOVE DESCRIP-PACI-RX    TO DESCRIP-PACI-J
           INITIALIZE CEDULA-Z
           MOVE ID-RADIOLOGO-RX    TO CEDULA-Z
           MOVE CEDULA-Z           TO ID-RADIOLOGO-J
           MOVE NOM-MEDICO-RX      TO NOM-MEDICO-J
           MOVE REG-MEDICO-RX      TO REG-MEDICO-J
           INITIALIZE CEDULA-Z
           MOVE ID-TECNOLOGO-RX    TO CEDULA-Z
           MOVE CEDULA-Z           TO ID-TECNOLOGO-J
           MOVE NOM-TECNOLOGO-RX   TO NOM-TECNOLOGO-J
           MOVE SALA-RX            TO SALA-J
           MOVE EDAD-RX            TO EDAD-J
           MOVE CTA-NUM-RX         TO CTA-NUM-J
           MOVE ID-ENTIDAD-RX      TO ID-ENTIDAD-J
           MOVE DX-RX              TO DX-J
           MOVE TIPO-DX-RX         TO TIPO-DX-J
           MOVE DESCRIP-DX-RX      TO DESCRIP-DX-J
           MOVE ADMI-MODIF-RX      TO ADMI-MODIF-J
           MOVE FECHA-MODIF-RX     TO FECHA-MODIF-J
           MOVE HORA-MODIF-RX      TO HORA-MODIF-J
           MOVE ADMI-TRANSC-RX      TO ADMI-TRANS-J
           MOVE FECHA-TRANSC-RX    TO FECHA-TRANS-J
           MOVE HORA-TRANSC-RX     TO HORA-TRANS-J
           MOVE BIRADS-RX          TO BIRADS-J
           MOVE COMPLEJIDAD-RX     TO COMPLEJIDAD-J
           MOVE NORMALIDAD-RX      TO NORMALIDAD-J
           MOVE EMAIL-RX           TO EMAIL-J
           MOVE FECHA-EMAIL-RX     TO FECHA-EMAIL-J
           MOVE HORA-EMAIL-RX      TO HORA-EMAIL-J
           MOVE ARCHIVO-MSG-RX     TO ARCHIVO-MSG-J

           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
      *     MOVE LIN-1              TO DATOS-PLANO-W
      *     PERFORM DATOS-ENVIO

           STRING LIN-1, '"', TAB-RESULTADO-PPAL-RX, '",'  DELIMITED BY SIZE 
                 INTO TABLA-EDIT
           END-STRING
           PERFORM MONTAR-TABLA VARYING I FROM 1 BY 1 UNTIL I > 12

           INITIALIZE TABLA-EDIT
           STRING '"RESULTADO_COMP":"', TAB-COMP-PPAL-RX, '",'  DELIMITED BY SIZE 
                 INTO TABLA-EDIT
           END-STRING
           PERFORM MONTAR-TABLA VARYING I FROM 1 BY 1 UNTIL I > 11


           INITIALIZE TABLA-EDIT
           STRING '"RESULTADO_ADIC":"', TAB-ADIC-PPAL-RX, '"},'  DELIMITED BY SIZE 
                 INTO TABLA-EDIT
           END-STRING
           PERFORM MONTAR-TABLA VARYING I FROM 1 BY 1 UNTIL I > 11


           GO TO LEER-RX.

       CERRAR-PLANO.
           INITIALIZE LIN-1 TABLA-EDIT
           STRING LIN-1, "**}]}" DELIMITED BY SIZE 
                  INTO TABLA-EDIT
           END-STRING

           PERFORM MONTAR-TABLA VARYING I FROM 1 BY 1 UNTIL I > 2

           CLOSE RESULTADOS-RX.

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

       MONTAR-TABLA.
           IF TABLA-EDIT-W (I) = SPACES OR LOW-VALUES
              CONTINUE
           ELSE
              INITIALIZE DATOS-PLANO-W
              INSPECT TABLA-EDIT-W (I) REPLACING ALL "*"        BY CARAC-COMILLA
              INSPECT TABLA-EDIT-W (I) REPLACING ALL "\.br\"    BY SPACES
              INSPECT TABLA-EDIT-W (I) REPLACING ALL LOW-VALUES BY SPACES
              MOVE TABLA-EDIT-W (I) TO DATOS-PLANO-W
              PERFORM DATOS-ENVIO
           END-IF.
      
       COPY "..\..\FUENTES\SC-WEB19.CBL".
      

