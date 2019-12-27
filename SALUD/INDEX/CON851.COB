      *VENTANA DE ERRORES DEL SISTEMA CON851
       IDENTIFICATION DIVISION.
      * PROGRAM-ID. "CON851".
      
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.   
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
     
      *     SELECT ARCHIVO-RESTRICCION LOCK MODE IS AUTOMATIC
      *            ASSIGN TO NOM-REST-W
      *            ORGANIZATION INDEXED;
      *            ACCESS MODE DYNAMIC;
      *            RECORD KEY LLAVE-REST
      *            ALTERNATE RECORD KEY NOMBRE-REST WITH DUPLICATES
      *            FILE STATUS OTR-STAT.
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
      * COPY "..\..\FUENTES\FD-RESTR.CBL".   
        
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".              

       01 DATO-LLEGADA-W.
          02 USU-LLEGAD-W.
             03 USU1-LLEGAD-W    PIC X(2).
             03 USU2-LLEGAD-W    PIC X(2).
          02 CLAVE-LLEGAD-W      PIC X(8).
          02 DIR-CONTA-W         PIC X(6).
          02 MES-CONTAB-W        PIC XX.
          02 IP-SERVER-W         PIC X(20).

       01 IP-TEMP                PIC X(20).

       01 DATOS-ENVIO.
          02 DAT0-ENV               PIC XX.
          02 FILLER                 PIC X VALUE "|".
          02 DAT1-ENV               PIC X(29).
          02 FILLER                 PIC X VALUE "|".
          02 DATO2-ENV              PIC X(20).

      
       LINKAGE SECTION.
       COPY "..\..\FUENTES\ISAPICTX.CBL".
       
       PROCEDURE DIVISION WITH stdcall LINKAGE USING ISAPI-CTX-CNT.
       
       DECLARATIVES.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUNET.          
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                TO MSJ1-HTML
              MOVE NOM-USU-W               TO MSJ2-HTML
              MOVE "INDEX"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
        
      * I-O-TEST SECTION.
      *     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-RESTRICCION.
      * ESCR-EXCEPTIONES.
      *     IF OTR-STAT = "00"
      *        CONTINUE                    
      *     ELSE
      *        MOVE OTR-STAT                TO MSJ1-HTML
      *        MOVE NOM-REST-W              TO MSJ2-HTML
      *        MOVE "INDEX"                 TO MSJ3-HTML
      *        GO TO ENVIAR2-ERROR
      *     END-IF.
       
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE
           ELSE
              MOVE OTR-STAT                TO MSJ1-HTML
              MOVE NOM-SESION-W            TO MSJ2-HTML
              MOVE "INDEX"                 TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO DATO-LLEGADA-W.

       INICIAR-SESION.
           MOVE "D:\WEB\SALUD\DATOS\SC-SESION.DAT" TO NOM-SESION-W
      
           OPEN I-O ARCHIVO-SESION
        
           EVALUATE OTR-STAT
             WHEN "00"  CONTINUE
             WHEN "35"  OPEN OUTPUT ARCHIVO-SESION
                        CLOSE ARCHIVO-SESION
                        GO TO INICIAR-SESION
             WHEN OTHER GO TO CERRAR-SESION
           END-EVALUATE.

       ABRIR-USUARIO.
           MOVE "D:\WEB\SALUD\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
      
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
      
           INITIALIZE REG-USUNET.

       LEER-USUARIO.
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
           CLOSE ARCHIVO-USUNET.

      *     IF USU-LLEGAD-W = 99
      *     AND CLAVE-LLEGAD-W  = 641218
      *         GO TO PAGINA-CONFIG
      *     END-IF.
            
      *     IF USU-LLEGAD-W = NIT-USUNET
      *     AND CLAVE-LLEGAD-W   = "@dm1"  
      *         GO TO PAGINA-CONFIG
      *     END-IF.
       
           IF NOMBRE-USUNET = SPACES
              MOVE "Validacion de usuarios"      TO MSJ1-HTML
              MOVE "Falta configurar usuario"    TO MSJ2-HTML
              MOVE "Sc"                          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

      * INICIAR-RESTRICION.
      *     MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
      *
      *     INITIALIZE NOM-REST-W
      *     MOVE "\\" TO NOM-REST-W
      * 
      *     INSPECT NOM-REST-W REPLACING FIRST "                    "
      *                                  BY IP-SERVER-W
      *
      *     INSPECT NOM-REST-W REPLACING FIRST "     "
      *                                  BY    "\PSC$"
      * 
      *     INSPECT NOM-REST-W REPLACING FIRST " "
      *                                    BY "\"
      *
      *     INSPECT NOM-REST-W   REPLACING FIRST "                         "
      *                                       BY "PROG\DATOS\SC-ARCHREST.SC".
      *
      *     
      *     OPEN INPUT ARCHIVO-RESTRICCION.
      *     INITIALIZE LLAVE-REST
      *     MOVE USU-LLEGAD-W TO COD-REST
      *     READ ARCHIVO-RESTRICCION WITH NO LOCK
      *          INVALID KEY
      *                  MOVE "99"                   TO MSJ1-HTML
      *                  MOVE "No existe el usuario" TO MSJ2-HTML
      *                  MOVE "ARCHIVO-RESTRICCION"  TO MSJ3-HTML
      *                  GO TO ENVIAR2-ERROR
      *     END-READ.
      
       BORRAR-SESION-ANTERIOR.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.
       
           INITIALIZE LLAVE-SESION
           MOVE FECHA-TOTAL TO FECHA-SESION.
           MOVE HORA-TOTAL  TO HORA-SESION.
           START ARCHIVO-SESION KEY >= LLAVE-SESION
                 INVALID KEY
                    GO TO ASIGNAR-SESION
           END-START.
                    
       LEER-SESION-ANTERIOR.
           READ ARCHIVO-SESION NEXT WITH NO LOCK AT END GO TO ASIGNAR-SESION.
        
           IF FECHA-SESION < FECHA-TOTAL
              DELETE ARCHIVO-SESION
           END-IF.
       
           IF (HR-SESION + 3) < HR-TOTAL
              DELETE ARCHIVO-SESION
           END-IF
              
           GO TO LEER-SESION-ANTERIOR.
       
       ASIGNAR-SESION.
      *     MOVE ID-REST      TO NIT-SESION
      *     MOVE FECHA-TOTAL  TO FECHA-SESION
      *     MOVE HORA-TOTAL   TO HORA-SESION.
      *     INITIALIZE NOMBRE-SESION
      *  
           READ ARCHIVO-SESION WITH NO LOCK
                INVALID KEY
                  MOVE IP-CLIENTE-W TO IP-SESION
                  WRITE REG-SESION  END-WRITE
                NOT INVALID KEY
                   ADD 1 TO SG-TOTAL
                   GO TO ASIGNAR-SESION
           END-READ.
        
           MOVE 00           TO DAT0-ENV
           MOVE LLAVE-SESION TO DAT1-ENV
           MOVE ADMIN-W      TO COD-SESION-W
           CLOSE ARCHIVO-SESION.
           
        
       FIN-VALIDAR-SESION.


       ENVIO-DATOS.
      *     CLOSE ARCHIVO-RESTRICCION.

           MOVE "datosrecibidos"  TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO       TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"   USING COBW3
           MOVE "../PAGINAS/RECIBIDOS.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML.
           GO TO CERRAR-SESION.

       
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.
       
       COPY "..\..\FUENTES\SC-WEB.CBL".
         
