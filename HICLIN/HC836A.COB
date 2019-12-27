      *==================================================================
      * SALUD - BUSQUEDA DE LA ULTIMA CONSULTA DE UN PACIENTE.
      *==================================================================
      * PO - PABLO OLGUIN 21/10/2019 CREACION.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".

            SELECT FACTURA-SERVICIOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-SAL-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-FACT
                  ALTERNATE RECORD KEY NRO-FACT   WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-FACT WITH DUPLICATES
                  ALTERNATE RECORD KEY CTA-FACT   WITH DUPLICATES
                  ALTERNATE RECORD KEY NIT-FACT   WITH DUPLICATES
                  ALTERNATE RECORD KEY ID-HISTORIA-FACT WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-ING-ESTAD WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-SAL-ESTAD WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-ENTR-RX   WITH DUPLICATES				  
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-USUAR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.   

       DATA DIVISION.
       FILE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUAR.CBL".       
       COPY "P:\PROG\PROYECT\FUENTES\FD-SALUD.CBL".
       
       WORKING-STORAGE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".

       77 NOM-USUAR-W            PIC X(100).
       77 NOM-SAL-W              PIC X(70).
       77 PARE-W                 PIC 9.

       01  DATO-LLEGADA-W.
           02 SESION-W               PIC X(29).
           02 DIRECTORIO-LLEGAD-W    PIC X(30).
           02 MES-TRABAJO-W          PIC X(10).
           02 PACIENTE-W             PIC X(15).
           02 FECHA-W.
              05 ANO-W            PIC 99.
              05 MES-W            PIC 99.
              05 DIA-W            PIC 99. 

       01 LINEA-LLEGADA-W     PIC X(300).

       01 DATOS-ENVIO.
          02 DATO-ENV                PIC XX.
          02 FILLER                  PIC X VALUE "|".
          02 FECHA-ULT-CONSUL-ENV    PIC X(6).
          02 FILLER                  PIC X VALUE "|".
          02 LLAVE-ULT-COMP-W        PIC X(9).
          02 FILLER                  PIC X VALUE "|".
          
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
              MOVE "HC836A"                 TO MSJ3-HTML
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
              MOVE "HC836A"                 TO MSJ3-HTML
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
              MOVE "HC836A"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON FACTURA-SERVICIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-SAL-W                TO MSJ2-HTML
              MOVE "HC836A"                 TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO LINEA-LLEGADA-W.

           INITIALIZE DATO-LLEGADA-W
           UNSTRING LINEA-LLEGADA-W DELIMITED BY "|"
           INTO SESION-W, DIRECTORIO-LLEGAD-W, MES-TRABAJO-W, PACIENTE-W, FECHA-W
           END-UNSTRING
           MOVE SESION-W TO LLAVE-SESION-W
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL
           ACCEPT HORA-TOTAL FROM TIME.

       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.


       ABRIR-USUARIO.
           
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
           OPEN INPUT ARCHIVO-USUNET
           EVALUATE OTR-STAT
               WHEN "00"  CONTINUE
               WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
                          CLOSE ARCHIVO-USUNET
                          OPEN I-O ARCHIVO-USUNET
                          INITIALIZE REG-USUNET
                          WRITE REG-USUNET
                          CLOSE   ARCHIVO-USUNET
                          MOVE "Validacion de usuarios"      TO MSJ1-HTML
                          MOVE "Falta configurar usuario"    TO MSJ2-HTML
                          MOVE "Sc"    TO MSJ3-HTML
                          GO TO ENVIAR2-ERROR
               WHEN OTHER GO CERRAR-SESION
            END-EVALUATE.

            INITIALIZE REG-USUNET.

       LEER-USUARIO.
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
       
            CLOSE ARCHIVO-USUNET.
            
            IF NOMBRE-USUNET = SPACES
            OR NIT-USUNET = ZEROS
               MOVE "Validacion de Sesion"        TO MSJ1-HTML
               MOVE "Falta configurar usuarios"   TO MSJ2-HTML
               MOVE "Sc"                          TO MSJ3-HTML
               GO TO ENVIAR2-ERROR
            END-IF.
       
       ASIGNAR-NOMBRES-ARCHIVOS.

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           INITIALIZE NOM-USUAR-W, NOM-SAL-W
           MOVE "\\" TO NOM-USUAR-W NOM-SAL-W

           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                           BY  IP-DATOS-USUNET
           INSPECT NOM-USUAR-W REPLACING FIRST " "
                                            BY "\"
           INSPECT NOM-USUAR-W REPLACING FIRST "                              "
                                           BY DIRECTORIO-LLEGAD-W

           INSPECT NOM-USUAR-W   REPLACING FIRST "                       "
                                              BY "\CONTROL\SC-ARCHUSU.DAT".

           INSPECT NOM-SAL-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET
           INSPECT NOM-SAL-W REPLACING FIRST " "
                                            BY "\"
           INSPECT NOM-SAL-W REPLACING FIRST "                              "
                                            BY DIRECTORIO-LLEGAD-W
           INSPECT NOM-SAL-W REPLACING FIRST "                     "
                                            BY "\CONTROL\SC-FACSA.DAT".
       INICIO.
           IF  PACIENTE-W IS NUMERIC AND PACIENTE-W < 000000000000100
               GO TO CERRAR-SESION
            END-IF
           OPEN INPUT FACTURA-SERVICIOS.

       ABRIR-ARCHIVOS.
           OPEN INPUT  ARCHIVO-USUARIOS
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK
           END-READ
           CLOSE ARCHIVO-USUARIOS

           INITIALIZE REG-FACT.
           MOVE PACIENTE-W TO ID-HISTORIA-FACT.

           START FACTURA-SERVICIOS KEY = ID-HISTORIA-FACT
                 INVALID KEY 
                           GO TO CERRAR-ARCHIVOS  
           END-START.

       LEER-FACTURAS.
           READ FACTURA-SERVICIOS NEXT WITH NO LOCK AT END GO TO CERRAR-ARCHIVOS.

           IF PACIENTE-W IS NOT EQUAL TO ID-HISTORIA-FACT
                                   GO TO CERRAR-ARCHIVOS. 

               EVALUATE CL-FACT
               WHEN 1 IF FECHA-ING-ESTAD > FECHA-W AND (GRUPO-FACT(1) = 31)
                      MOVE FECHA-ING-ESTAD TO FECHA-W
                      MOVE LLAVE-FACT      TO LLAVE-ULT-COMP-W
                      END-IF
               WHEN 3 IF FECHA-ING-ESTAD > FECHA-W
                     MOVE FECHA-ING-ESTAD TO FECHA-W
                     MOVE LLAVE-FACT      TO LLAVE-ULT-COMP-W
                  END-IF
               WHEN 4 IF FECHA-ING-ESTAD > FECHA-W
                     MOVE FECHA-ING-ESTAD TO FECHA-W
                     MOVE LLAVE-FACT      TO LLAVE-ULT-COMP-W
                  END-IF
               WHEN 5 IF  FECHA-ING-ESTAD > FECHA-W
                  AND (GRUPO-FACT (1) = 89
                      OR GRUPO-FACT (2) = 89
                      OR GRUPO-FACT (1) = 95
                      OR GRUPO-FACT (1) = "XX")
                      MOVE FECHA-ING-ESTAD TO FECHA-W
                      MOVE LLAVE-FACT      TO LLAVE-ULT-COMP-W
                 END-IF
               WHEN 5 IF  (FECHA-ING-ESTAD > FECHA-W)
                  AND (   (GRUPO-FACT (1) = 89)   OR (GRUPO-FACT (2) = 89)
                       OR (GRUPO-FACT (1) = 95)   OR (GRUPO-FACT(1) = 69)
                       OR (GRUPO-FACT (1) = "A1") OR (GRUPO-FACT(1) = "A1")
                       OR (ART-FACT   (1) = 990201)
                       OR (ART-FACT   (2) = 990204)
                      )
                      MOVE FECHA-ING-ESTAD TO FECHA-W
                      MOVE LLAVE-FACT      TO LLAVE-ULT-COMP-W
                 END-IF
               END-EVALUATE
               MOVE FECHA-W         TO FECHA-ULT-CONSUL-ENV

               GO TO LEER-FACTURAS.

       CERRAR-ARCHIVOS.
           CLOSE FACTURA-SERVICIOS.
           MOVE "00" TO DATO-ENV.

010700 PAGINA-CONFIG.
010720     MOVE "datosrecibidos"  TO COBW3-CNV-NAME
010730     MOVE DATOS-ENVIO       TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV"   USING COBW3
010750
010760     MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
010770     PERFORM ABRIR-HTML.
007980   
               
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.
           
       COPY "P:\PROG\PROYECT\FUENTES\SC-WEB.CBL".
