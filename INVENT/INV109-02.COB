      *=================================================================
      *GRABAR ARCHIVO-PREFIJOS INV109-02  SALUD
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".                                                         
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".
                                                                                     
*********"../control/SC-ARCHPREF.DAT"
           SELECT ARCHIVO-PREFIJOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PREF-LNK
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS LLAVE-PREF
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-PLANO
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.

      
       DATA DIVISION.
       FILE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-PREFI.CBL".

       FD  ARCHIVO-PLANO
           LABEL RECORD STANDARD.
       01  REG-PLANO.
           02 DATOS-PLANO      PIC X(1500).
      
       WORKING-STORAGE  SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC19.CBL".

       77 NOM-PLANO-W                  PIC X(60).

       77 LINEA-EDIT                    PIC X(1000).


       01  DATO-LLEGADA-W.
           02 DEF-PREF-LLEGADA-W        PIC X.
           02 PROV-FACT-PREF-LLEGADA-W  PIC X.
           02 NOMBRETXT-LLEGADA-W       PIC X(60).
           02 CLIENTE-LLEGADA-W         PIC X(80).
           02 ACCESO-LLEGADA-W          PIC X(80).
           02 PRUEBA-LLEGADA-W          PIC X.
           02 OPERADOR-LLEGADA-W        PIC X(4).

       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.

      
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
              MOVE "INV109-02"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PREFIJOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PREF-LNK               TO MSJ2-HTML
              MOVE "INV109-02"              TO MSJ3-HTML
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
              MOVE "INV109-02"              TO MSJ3-HTML
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
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W, 
              DEF-PREF-LLEGADA-W, PROV-FACT-PREF-LLEGADA-W, NOMBRETXT-LLEGADA-W, CLIENTE-LLEGADA-W, ACCESO-LLEGADA-W, PRUEBA-LLEGADA-W, OPERADOR-LLEGADA-W
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
                        MOVE "SC-2"                        TO MSJ1-HTML
                        MOVE "Falta configurar usuario"    TO MSJ2-HTML
                        MOVE "INV109-02"                   TO MSJ3-HTML
                        GO TO ENVIAR2-ERROR
             WHEN OTHER GO TO CERRAR-SESION
           END-EVALUATE.
      
           INITIALIZE LLAVE-USUNET.

       LEER-USUARIO.
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
           CLOSE ARCHIVO-USUNET.
      
           IF NOMBRE-USUNET = SPACES
              MOVE "SC-2"      TO MSJ1-HTML
              MOVE "Falta configurar usuario"    TO MSJ2-HTML
              MOVE "INV109-02"                   TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.
           INITIALIZE NOM-PREF-LNK

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           
           MOVE "\\" TO NOM-PREF-LNK

           INSPECT NOM-PREF-LNK REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET
           
           INSPECT NOM-PREF-LNK REPLACING FIRST " "
                                        BY "\"

           INSPECT NOM-PREF-LNK REPLACING FIRST "                              "
                                          BY DIRECTORIO-LLEGAD-W.          

           INSPECT NOM-PREF-LNK REPLACING FIRST "         "
                                           BY "\CONTROL\".

           INSPECT NOM-PREF-LNK REPLACING FIRST "               "
                                           BY "SC-ARCHPREF.DAT".                                       

                                                                                                                                                                                                            

       ABRIR-PLANO.
           MOVE NOMBRETXT-LLEGADA-W TO NOM-PLANO-W
           OPEN INPUT ARCHIVO-PLANO.
           OPEN I-O ARCHIVO-PREFIJOS.

           READ ARCHIVO-PREFIJOS NEXT WITH NO LOCK AT END 
                CONTINUE
           END-READ.
           
           INITIALIZE DATOS-PREF.

           MOVE DEF-PREF-LLEGADA-W TO DEF-PREF
           MOVE PROV-FACT-PREF-LLEGADA-W TO PROV-FACT-PREF.
           IF PROV-FACT-PREF-LLEGADA-W = 1 OR 4 OR 5
              MOVE CLIENTE-LLEGADA-W TO CLIENT-TOKEN-PREF
              MOVE ACCESO-LLEGADA-W TO ACCESS-TOKEN-PREF
              MOVE PRUEBA-LLEGADA-W TO PRUEBA-TOKEN-PREF
           END-IF.

       LEER-PLANO.

           READ ARCHIVO-PLANO NEXT WITH NO LOCK AT END GO TO CERRAR-PLANO.

           IF DATOS-PLANO = SPACES OR LOW-VALUES
              GO TO CERRAR-PLANO
           END-IF

           UNSTRING DATOS-PLANO DELIMITED BY "|"
              INTO J,  LINEA-EDIT
           END-UNSTRING

           UNSTRING LINEA-EDIT DELIMITED BY ";"
              INTO COD-PREF (J), DESCRIP-PREF (J), RESOL1-PREF (J), 
                   FECHA-RES-PREF (J), INI-RES-PREF (J), 
                   FIN-RES-PREF (J), VIGENCIA-PREF (J), SUC-PREF (J),
                   C-COSTO-PREF (J), ALM-PREF (J), LISTA-SUC-PREF (J), POS-PREF (J)
           END-UNSTRING

           REWRITE REG-PREF END-REWRITE.

           GO TO LEER-PLANO.
 
       CERRAR-PLANO.
           CLOSE ARCHIVO-PLANO.
           CLOSE ARCHIVO-PREFIJOS.

       ENVIO-DATOS.
            MOVE "statuscode" TO COBW3-CNV-NAME
            MOVE "00"         TO COBW3-CNV-VALUE
            CALL "COBW3_SET_CNV" USING COBW3

           MOVE "datosrecibidos"                 TO COBW3-CNV-NAME
           MOVE "REGISTRO GUARDADO EXITOSAMENTE" TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"                  USING COBW3

           MOVE "programa-id"   TO COBW3-CNV-NAME
           MOVE "INV109-02"     TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3

           MOVE "..\PAGINAS\RECIBIDOSPLANO.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.
      
       COPY "..\..\FUENTES\SC-WEB19.CBL".
      

