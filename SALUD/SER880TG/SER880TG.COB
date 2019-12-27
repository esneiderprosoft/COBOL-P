      *=================================================================
      * BUSCA TRIAGE DE UN PACIENTE POR FECHA Y GRABA EL NRO COMPROBANTE 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

           SELECT ARCHIVO-TRIAGE LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TRIA-W
      * "\PROG\HC\DATOS\SC-TRIAG.S17"
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-TRIA
                  ALTERNATE RECORD KEY IS ID-TRIA WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FOLIO-CONSUL-TRIA WITH DUPLICATES
                  FILE STATUS OTR-STAT.


       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-TRIAG17.CBL".
            

       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-TRIA-W                   PIC X(70).

       01 NIT-TR-W                     PIC 9(10).
       01 ANO-TR-W                     PIC 99.
       01 ADMIN-X                      PIC X(4).
       01 FECHA-INI-W.
          03 ANO-INI-W                 PIC 9(4).
          03 MES-INI-W                 PIC 99.
          03 DIA-INI-W                 PIC 99.
       01 FECHA-FIN-W.
          03 ANO-FIN-W                 PIC 9(4).
          03 MES-FIN-W                 PIC 99.
          03 DIA-FIN-W                 PIC 99.

       01 REG-AUD-W.
          02 TIPO-AUD-W                PIC X(6).
          02 SUC-AUD-W                 PIC XX.
          02 NOVED-AUD-W               PIC X.
          02 DATO-AUD-W                PIC X(50000).    
                              
       01 LLEGADA-W                    PIC X(100).
                               
       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(21).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 LLAVE-FACT-LLEGA.
             03 SUC-FACT-LLEGA         PIC XX.
             03 CL-FACT-LLEGA          PIC 9.
             03 NRO-FACT-LLEGA         PIC 9(6).
          02 FECHA-FACT-LLEGA.
             03 ANO-FACT-LLEGA         PIC 99.
             03 MES-FACT-LLEGA         PIC 99.
             03 DIA-FACT-LLEGA         PIC 99.
          02 ID-HISTORIA-FACT-LLEGA    PIC X(15).
           

       01 DATOS-ENVIO.
          02 ESTADO-ENV                PIC XX.           
          02 FILLER                    PIC X VALUE "|".
          02 INVALID-ENV               PIC XX. 
          02 FILLER                    PIC X VALUE "|".
          02 NIT-ENV                   PIC 9(10).
          02 FILLER                    PIC X VALUE "|".
          02 ANO-ENV                   PIC 99.
          02 FILLER                    PIC X VALUE "|".
          02 ADMIN-ENV                 PIC X(4).
          02 FILLER                    PIC X VALUE "|".
          02 REG-ENV                   PIC X(50009).
        
                                    
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
              MOVE "SER880TG"               TO MSJ3-HTML
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
              MOVE "SER880TG"               TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TRIAGE.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-TRIA-W               TO MSJ2-HTML
              MOVE "SER880TG"               TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, LLAVE-FACT-LLEGA, FECHA-FACT-LLEGA,
                   ID-HISTORIA-FACT-LLEGA
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
         
           INITIALIZE NOM-TRIA-W

           MOVE "\\" TO NOM-TRIA-W

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-TRIA-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-TRIA-W REPLACING FIRST " "
                                            BY "\"
           INSPECT NOM-TRIA-W REPLACING FIRST "                          "
                                            BY "PROG\HC\DATOS\SC-TRIAG.S17".

       ABRIR-TRIAGE.
           OPEN INPUT ARCHIVO-TRIAGE.

       DATO-FECHA.
           COMPUTE ANO-INI-W = ANO-FACT-LLEGA + 2000.
           MOVE MES-FACT-LLEGA TO MES-INI-W.
           MOVE DIA-FACT-LLEGA TO DIA-INI-W.
           MOVE FECHA-INI-W TO FECHA-FIN-W.

           ACCEPT HORA-SIST FROM TIME.

       BUSCAR-MOVIMIENTO.

           IF HR-SIST > 03
              CONTINUE
           ELSE
              IF DIA-INI-W > 1
                 SUBTRACT 1 FROM DIA-INI-W
              ELSE
                 IF MES-INI-W > 1
                    SUBTRACT 1 FROM MES-INI-W
                    IF MES-INI-W = 02
                       MOVE 28 TO DIA-INI-W
                    ELSE
                       MOVE 30 TO DIA-INI-W
                    END-IF
                 ELSE
                    SUBTRACT 1 FROM ANO-INI-W
                    MOVE 12    TO   MES-INI-W
                    MOVE 31    TO   DIA-INI-W
                 END-IF
              END-IF
           END-IF

           INITIALIZE LLAVE-TRIA.
           MOVE FECHA-INI-W   TO FECHA-ING-TRIA.
           START ARCHIVO-TRIAGE KEY >= FECHA-ING-TRIA
                 INVALID KEY MOVE 08 TO SW-INVALID.
                          

       LEER-TRIAGE.
           READ ARCHIVO-TRIAGE NEXT WITH NO LOCK AT END
                        GO TO CERRAR-ARCHIVOS
           END-READ

           IF FECHA-ING-TRIA > FECHA-FIN-W
              GO TO CERRAR-ARCHIVOS
           END-IF

   
           IF ID-TRIA IS NOT = ID-HISTORIA-FACT-LLEGA
              GO TO LEER-TRIAGE
           END-IF

           IF  NRO-FACT-TRIA IS ZERO
               CLOSE ARCHIVO-TRIAGE
               OPEN I-O ARCHIVO-TRIAGE
               MOVE LLAVE-FACT-LLEGA TO COMP-SERV-TRIA
               REWRITE REG-TRIA

                   MOVE "TRIA18"    TO TIPO-AUD-W
                   MOVE "R"         TO NOVED-AUD-W
                   MOVE REG-TRIA    TO DATO-AUD-W

                   MOVE "80TG"      TO ADMIN-X

                   MOVE 9999999999 TO NIT-TR-W
                   MOVE 99         TO ANO-TR-W

           ELSE
               GO TO LEER-TRIAGE
           END-IF.

       CERRAR-ARCHIVOS.
           CLOSE ARCHIVO-TRIAGE.
 
       ENVIO-DATOS.

           MOVE "00"          TO ESTADO-ENV
           MOVE SW-INVALID    TO INVALID-ENV
           MOVE NIT-TR-W      TO NIT-ENV
           MOVE ANO-TR-W      TO ANO-ENV
           MOVE ADMIN-X       TO ADMIN-ENV
           MOVE REG-AUD-W     TO REG-ENV.
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
