      *=================================================================
      * MUESTRA MENSAJE SOBRE PACIENTES - SER810B 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

           SELECT ARCHIVO-PACIENTES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PACIE-W 
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PACI
                  ALTERNATE RECORD KEY  DESCRIP-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  EPS-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  ID-COTIZ-PACI WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-USUAR-W
      * "../CONTROL/SC-ARCHUSU.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-PACIE.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".


       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-PACIE-W                  PIC X(70).
       77 NOM-USUAR-W                  PIC X(70).
       
       01 LLEGADA-W                    PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(20).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 ID-HISTORIA-FACT          PIC X(15).
         

       01 DATOS-ENVIO.
          02 COD-ENV                   PIC X(2).
          02 FILLER                    PIC X VALUE "|".          
          02 ANO-PAC2-ENV              PIC 9(4).
          02 FILLER                    PIC X VALUE "|". 
          02 MES-PAC2-ENV              PIC XX.
          02 FILLER                    PIC X VALUE "|". 
          02 DIA-PAC2-ENV              PIC XX.
          02 FILLER                    PIC X VALUE "|". 
          02 OPER-OBSE-PAC2-ENV        PIC X(4).
          02 FILLER                    PIC X VALUE "|".
          02 OBSERV1-PAC2-ENV          PIC X(80).
          02 FILLER                    PIC X VALUE "|".
          02 OBSERV2-PAC2-ENV          PIC X(80).
          02 FILLER                    PIC X VALUE "|".
          02 OBSERV3-PAC2-ENV          PIC X(80).
          02 FILLER                    PIC X VALUE "|".
          02 OBSERV4-PAC2-ENV          PIC X(80).
          02 FILLER                    PIC X VALUE "|".
          02 OBSERV5-PAC2-ENV          PIC X(80).
       
               
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
              MOVE "SER810B"                TO MSJ3-HTML
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
              MOVE "SER810B"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PACIE-W              TO MSJ2-HTML
              MOVE "SER810B"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USUAR-W              TO MSJ2-HTML
              MOVE "SER810B"                TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, ID-HISTORIA-FACT 
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
 
       ASIGNAR-NOMBRES.                  
         
           INITIALIZE NOM-PACIE-W
           MOVE "\\" TO NOM-PACIE-W
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

           INSPECT NOM-PACIE-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

           INSPECT NOM-PACIE-W REPLACING FIRST " "
                                             BY "\"

           INSPECT NOM-PACIE-W REPLACING FIRST "                    "
                                           BY DIR-CONTAB-LLEGADA-W.

           INSPECT NOM-PACIE-W REPLACING FIRST "                     "
                                            BY "\CONTROL\SC-PACIE.DAT".
         
       INICIO.
           OPEN INPUT  ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END
                MOVE 0 TO OPCION-1
           END-READ. 

           CLOSE ARCHIVO-USUARIOS.

           IF PREFIJO-USU = "  "
              MOVE "00" TO PREFIJO-USU
           END-IF 

           IF PREFIJO-USU = "  "
              MOVE "00"  TO PREFIJO-USU
           END-IF.

       ABRIR-PACIENTES.

           OPEN INPUT ARCHIVO-PACIENTES.
           MOVE ID-HISTORIA-FACT TO COD-PACI.
           READ ARCHIVO-PACIENTES WITH NO LOCK
                INVALID KEY
                   CLOSE ARCHIVO-PACIENTES
                      GO TO CERRAR-SESION
           END-READ    
          

           IF OBSERV-NOTA-PAC2 = SPACES
              GO TO CERRAR-VENTANA.


           MOVE "00"                TO  COD-ENV.
           MOVE ANO-NOTA-PAC2       TO  ANO-PAC2-ENV 
           MOVE MES-NOTA-PAC2       TO  MES-PAC2-ENV 
           MOVE DIA-NOTA-PAC2       TO  DIA-PAC2-ENV 
           MOVE OPER-OBSE-NOTA-PAC2 TO  OPER-OBSE-PAC2-ENV
           MOVE OBSERV1-NOTA-PAC2   TO  OBSERV1-PAC2-ENV
           MOVE OBSERV2-NOTA-PAC2   TO  OBSERV2-PAC2-ENV
           MOVE OBSERV3-NOTA-PAC2   TO  OBSERV3-PAC2-ENV
           MOVE OBSERV4-NOTA-PAC2   TO  OBSERV4-PAC2-ENV
           MOVE OBSERV5-NOTA-PAC2   TO  OBSERV5-PAC2-ENV.                        

       CERRAR-VENTANA.

           CLOSE ARCHIVO-PACIENTES.


       PAGINA-CONFIG.
	 
           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO      TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3
         
           MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
           PERFORM ABRIR-HTML.

       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.

              
       COPY "..\..\FUENTES\SC-WEB19.CBL".
