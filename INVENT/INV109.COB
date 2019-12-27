      *=================================================================
      *ACTUALIZA LOS PREFIJOS AUTORIZADOS INV109-01  
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".     


*******"SC-USUARIO.DAT"
           SELECT ARCHIVO-USUARPQ LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-USUPQ-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU-PQ
                  FILE STATUS IS OTR-STAT.

*******"P:\PROG\DATOS\SC-SUCUR.DAT"
           SELECT ARCHIVO-SUCUR LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-SUCUR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS COD-SUCUR
                  FILE STATUS IS OTR-STAT.

*******"../control/SC-ARCHCOS.DAT"
           SELECT ARCHIVO-COSTO LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-COSTO-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS COD-COSTO
                  FILE STATUS IS OTR-STAT.

********"..\control\SC-ALMAC.DAT"
           SELECT ARCHIVO-LOCALIZACION LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-LOCAL-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS COD-LOCAL
                  ALTERNATE RECORD KEY IS LLAVE-ALT1-LOCAL WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LLAVE-ALT2-LOCAL WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

*********"../control/SC-ARCHPREF.DAT"
           SELECT ARCHIVO-PREFIJOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PREF-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS LLAVE-PREF
                  FILE STATUS IS OTR-STAT.
      
       DATA DIVISION.
       FILE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-PREFI.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-LOCAL.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-COSTO.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SUCUR.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUARPQ.CBL".

       WORKING-STORAGE  SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".

       77 DATOS-PLANO-W              PIC X(1024).

       77 NOM-PREF-W                   PIC X(90).
       77 NOM-LOCAL-W                  PIC X(90).
       77 NOM-COSTO-W                  PIC X(90).
       77 NOM-SUCUR-W                  PIC X(90).
       77 NOM-USUPQ-W                  PIC X(90).
       
       01 LINEA-LLEGAD-W            PIC X(150).

       01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W           PIC X(15).
              03 FECHA-LLEGAD-W        PIC X(8).
              03 HORA-LLEGAD-W         PIC X(6).
           02 DIR-CONTAB-LLEGADA-W     PIC X(30).
           02 MES-CONTAB-LLEGADA-W     PIC XX.

       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.

       01 LIN-1.  
          02 FILLER                   PIC X(13)  VALUE "{*FPREF_NUM*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DEF-PREF-J               PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(18)  VALUE "*PROV_FACT_ELECT*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 PROV-FACT-PREF-J         PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(16)  VALUE "*CLIENTE_TOKEN*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 CLIENTE-TOKEN-PREF-J     PIC X(80).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(15)  VALUE "*ACCESO_TOKEN*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 ACCESS-TOKEN-PREF-J      PIC X(80).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(15)  VALUE "*PRUEBA_TOKEN*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 PRUEBA-TOKEN-PREF-J      PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*TABLA*:[". 
         
       01 LIN-2.
          02 FILLER                   PIC X(7)   VALUE "{*NRO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 NRO-PREF-J               PIC X(6).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)  VALUE "*PREFIJO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 COD-PREF-J               PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*DESC_PREF*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 DESCRIP-PREF-J           PIC X(15).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*AUT_DIAN*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 RESOL1-PREF-J            PIC X(15).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(8)   VALUE "*FECHA*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 FECHA-RES-PREF-J         PIC X(8).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*DESDE_NRO*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 INI-RES-PREF-J           PIC X(9).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*HASTA_NRO*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 FIN-RES-PREF-J           PIC X(9).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*SUCURSAL*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 SUC-PREF-J               PIC XX.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(15)  VALUE "*CENTRO_COSTO*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 C-COSTO-PREF-J           PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)  VALUE "*ALMACEN*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 ALM-PREF-J               PIC X(5).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*LISTA_SUC*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 LISTA-SUC-PREF-J         PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*VIGENCIA*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 VIGENCIA-PREF-J          PIC X(3).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(6)  VALUE "*POS*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 POS-PREF-J               PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*ESTADO*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 ESTADO-PREF-J            PIC X.
       
          02 FILLER                   PIC XX   VALUE "*}".    
          02 CIERRE-LIN-2             PIC X.

      
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
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USU-W                TO MSJ2-HTML
              MOVE "INV109"                TO MSJ3-HTML
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
              MOVE "INV109"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PREFIJOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT          TO MSJ1-HTML
              MOVE NOM-PREF-W        TO MSJ2-HTML
              MOVE "INV109"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-LOCALIZACION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT          TO MSJ1-HTML
              MOVE NOM-LOCAL-W        TO MSJ2-HTML
              MOVE "INV109"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-COSTO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT          TO MSJ1-HTML
              MOVE NOM-COSTO-W        TO MSJ2-HTML
              MOVE "INV109"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

        I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SUCUR.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT          TO MSJ1-HTML
              MOVE NOM-SUCUR-W        TO MSJ2-HTML
              MOVE "INV109"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARPQ.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT          TO MSJ1-HTML
              MOVE NOM-USUPQ-W        TO MSJ2-HTML
              MOVE "INV109"          TO MSJ3-HTML
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
              INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W
           END-UNSTRING.

           MOVE LLAVESESION-LLEGA-W    TO LLAVE-SESION-W.

           
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
                        MOVE "SC-2"                        TO MSJ1-HTML
                        MOVE "Falta configurar usuario"    TO MSJ2-HTML
                        MOVE "INV109"                      TO MSJ3-HTML
                        GO TO ENVIAR2-ERROR
             WHEN OTHER GO TO CERRAR-SESION
           END-EVALUATE.
      
           INITIALIZE LLAVE-USUNET.

       LEER-USUARIO.
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
           CLOSE ARCHIVO-USUNET.
      
           IF NOMBRE-USUNET = SPACES
              MOVE "SC-2"                        TO MSJ1-HTML
              MOVE "Falta configurar usuario"    TO MSJ2-HTML
              MOVE "INV109"                      TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.
           INITIALIZE NOM-PREF-W
                      NOM-LOCAL-W
                      NOM-COSTO-W
                      NOM-SUCUR-W
                      NOM-USUPQ-W

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           
           MOVE "\\" TO NOM-PREF-W

           INSPECT NOM-PREF-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

           INSPECT NOM-PREF-W REPLACING FIRST " "
                                        BY "\"

           MOVE NOM-PREF-W TO NOM-SUCUR-W

           INSPECT NOM-PREF-W REPLACING FIRST "                              "
                                        BY DIR-CONTAB-LLEGADA-W.

           INSPECT NOM-PREF-W REPLACING FIRST "         "
                                       BY "\CONTROL\".

           MOVE NOM-PREF-W TO NOM-LOCAL-W NOM-COSTO-W NOM-USUPQ-W


           INSPECT NOM-USUPQ-W REPLACING FIRST "               "
                                            BY "\SC-USUARIO.DAT".
                                         
           INSPECT NOM-PREF-W REPLACING FIRST "               "
                                           BY "SC-ARCHPREF.DAT".

           INSPECT NOM-LOCAL-W REPLACING FIRST "            "
                                           BY "SC-ALMAC.DAT".

           INSPECT NOM-COSTO-W REPLACING FIRST "              "
                                           BY "SC-ARCHCOS.DAT".

           INITIALIZE NOM-SUCUR-W
           MOVE "\\" TO NOM-SUCUR-W 
           INSPECT NOM-SUCUR-W  REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET
      
           INSPECT NOM-SUCUR-W REPLACING FIRST  "                        "
                                          BY    "\PROG\DATOS\SC-SUCUR.DAT".

       ABRIR-ARCHIVOS.
           OPEN INPUT  ARCHIVO-PREFIJOS.
           
       BUSCAR-PREFIJOS.
           INITIALIZE REG-PREF
           READ  ARCHIVO-PREFIJOS NEXT WITH NO LOCK AT END MOVE 1 TO OPCION-1.

        ABRIR-JSON-PREFIJO.
           MOVE "{*PREFIJOS*:[" TO  DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO.
           
       LEER-PREFIJOS.
           MOVE DEF-PREF TO DEF-PREF-J
           MOVE PROV-FACT-PREF TO PROV-FACT-PREF-J
           IF PROV-FACT-PREF-J = 1 OR 4 OR 5
              MOVE CLIENT-TOKEN-PREF TO CLIENTE-TOKEN-PREF-J
              MOVE ACCESS-TOKEN-PREF TO ACCESS-TOKEN-PREF-J
              IF PRUEBA-TOKEN-PREF = SPACES OR LOW-VALUES OR ZEROS
                 MOVE "N" TO PRUEBA-TOKEN-PREF-J
              ELSE 
                 MOVE PRUEBA-TOKEN-PREF TO PRUEBA-TOKEN-PREF-J
              END-IF
           ELSE 
              MOVE SPACES TO CLIENTE-TOKEN-PREF-J
              MOVE SPACES TO ACCESS-TOKEN-PREF-J
              MOVE SPACES TO PRUEBA-TOKEN-PREF-J
           END-IF.

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA  
           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES           
              
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           INITIALIZE J

           PERFORM LLENAR-DATOS-PREFIJOS VARYING J FROM 1 BY 1
                                         UNTIL J > 98.

           GO TO CERRAR-PREFIJOS.           

       LLENAR-DATOS-PREFIJOS.
           IF J < 98
                 MOVE "," TO CIERRE-LIN-2
           ELSE
              MOVE SPACE TO CIERRE-LIN-2
           END-IF

           IF DESCRIP-PREF  (J) = SPACES
           OR DESCRIP-PREF  (J) = LOW-VALUES
           OR DESCRIP-PREF  (J) = ZEROS
              CONTINUE
           ELSE
              MOVE J                    TO NRO-PREF-J
              MOVE COD-PREF         (J) TO COD-PREF-J 
              MOVE DESCRIP-PREF     (J) TO DESCRIP-PREF-J 
              MOVE RESOL1-PREF      (J) TO RESOL1-PREF-J 
              MOVE FECHA-RES-PREF   (J) TO FECHA-RES-PREF-J 
              MOVE INI-RES-PREF     (J) TO INI-RES-PREF-J 
              MOVE FIN-RES-PREF     (J) TO FIN-RES-PREF-J
              MOVE SUC-PREF         (J) TO SUC-PREF-J 
              MOVE C-COSTO-PREF     (J) TO C-COSTO-PREF-J 
              
              MOVE ALM-PREF         (J) TO ALM-PREF-J 
              INSPECT ALM-PREF-J REPLACING ALL "*" BY "X"

              MOVE LISTA-SUC-PREF   (J) TO LISTA-SUC-PREF-J 
              MOVE VIGENCIA-PREF    (J) TO VIGENCIA-PREF-J
           
              IF POS-PREF (J) = SPACES OR LOW-VALUES
                 MOVE "N" TO POS-PREF-J
              ELSE 
                 MOVE POS-PREF (J) TO POS-PREF-J
              END-IF

              MOVE 1 TO ESTADO-PREF-J
              
              INSPECT LIN-2 REPLACING ALL "*" BY CARAC-COMILLA
              INSPECT LIN-2 REPLACING ALL LOW-VALUES BY SPACES  
              MOVE LIN-2 TO DATOS-PLANO-W
              PERFORM DATOS-ENVIO
            END-IF.

       CERRAR-PREFIJOS.
           INITIALIZE LIN-2
           MOVE LIN-2 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           MOVE "]}]}" TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           CLOSE ARCHIVO-PREFIJOS.

       PAGINA-CONFIG.
           MOVE "statuscode" TO COBW3-CNV-NAME
           MOVE "00"         TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3


           MOVE "programa-id" TO COBW3-CNV-NAME
           MOVE "INV109"      TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3

           MOVE "..\PAGINAS\RECIBIDOSLISTADO.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML.

       DATOS-ENVIO.
           MOVE "datosrecibidos"   TO COBW3-CNV-NAME
           MOVE DATOS-PLANO-W      TO COBW3-CNV-VALUE
           CALL "COBW3_SET_REPEAT" USING COBW3.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.
      
       COPY "P:\PROG\PROYECT\FUENTES\SC-WEB19.CBL".
      

