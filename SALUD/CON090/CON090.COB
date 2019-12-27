      *=================================================================
      * CARGA CAMBIOS EN EL LOG DE AUDITORIA - CON090
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

           SELECT ARCHIVO-AUD  LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-AUD-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY IS LLAVE-AUD
                  ALTERNATE RECORD KEY IS FECHA-AUD  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ADMI-AUD  WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.  
  
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-AULOG.CBL".
      


       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-AUD-W                    PIC X(70).
  
       01 LLEGADA-W                    PIC X(100).                                      
  

       01  FECHA-ACT.
           02 ANO-ACT                  PIC 99.
           02 MES-ACT                  PIC 99.
           02 DIA-ACT                  PIC 99.

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(21).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 NIT-LNK.
             03 NIT1-LNK              PIC 9(6).
             03 NIT2-LNK              PIC 9(4).
          02 ANO-LNK                  PIC 99.
          02 ADMIN-LLEGA-W            PIC X(4).
          02 REG-AUD-LNK.
             03 TIPO-AUD-LNK          PIC X(6).
             03 SUC-AUD-LNK           PIC XX.
             03 NOVED-AUD-LNK         PIC X.         
             03 DATO-AUD-LNK          PIC X(50000).

                              
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
              MOVE "CON090"                 TO MSJ3-HTML
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
              MOVE "CON090"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-AUD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-AUD-W                TO MSJ2-HTML
              MOVE "CON090"                 TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA    TO LLEGADA-W.

           UNSTRING LLEGADA-W DELIMITED BY "|"
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, NIT-LNK, ANO-LNK,
                   ADMIN-LLEGA-W, REG-AUD-LNK
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
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

       VALIDAR-ANO.
            ACCEPT FECHA-ACT FROM DATE.

            IF ANO-LNK IS ZERO
               MOVE ANO-ACT TO ANO-LNK
            END-IF.

 
       ASIGNAR-NOMBRES.                  
         
           INITIALIZE NOM-AUD-W  
         
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           MOVE "\\" TO NOM-AUD-W 


           INSPECT NOM-AUD-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

           INSPECT NOM-AUD-W REPLACING FIRST " "
                                             BY "\"
                   
           INSPECT NOM-AUD-W REPLACING FIRST "                          "           
                                          BY "PROG\DATOS\AUD\N1N2-A1.DAT". 

           INSPECT NOM-AUD-W REPLACING FIRST "N1N2" BY NIT2-LNK.

           INSPECT NOM-AUD-W REPLACING FIRST "A1"   BY ANO-LNK.

       ABRIR-ARCHIVO.
           OPEN I-O ARCHIVO-AUD.
           EVALUATE OTR-STAT
             WHEN "00" GO TO GRABAR-REGISTRO
             WHEN "35" OPEN OUTPUT ARCHIVO-AUD
                       CLOSE       ARCHIVO-AUD
                       GO TO ABRIR-ARCHIVO
            WHEN OTHER GO TO CERRAR-SESION
           END-EVALUATE.

       GRABAR-REGISTRO.
           COMPUTE ANO-AUD = ANO-ACT + 2000.
           MOVE MES-ACT        TO MES-AUD.
           MOVE DIA-ACT        TO DIA-AUD.
           ACCEPT  HORA-AUD FROM TIME.
           MOVE ADMIN-LLEGA-W  TO ADMI-AUD.
           READ ARCHIVO-AUD
                INVALID KEY
                   MOVE REG-AUD-LNK  TO DATOS-AUD
                   WRITE REG-AUD
           END-READ.

       CERRAR-ARCHIVO.
           CLOSE ARCHIVO-AUD.

       PAGINA-CONFIG.	 
           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE "00"             TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3
         
           MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
           PERFORM ABRIR-HTML.

       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.

              
       COPY "..\..\FUENTES\SC-WEB19.CBL".
