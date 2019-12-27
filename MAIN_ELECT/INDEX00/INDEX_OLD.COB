      *VALIDA EL USUARIO Y CONTRASEÑA DE LA PAGINA DE INICIO 
       IDENTIFICATION DIVISION.
      * PROGRAM-ID. "INDEX".
                                                       
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-RESTRICCION LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-REST-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY LLAVE-REST  
                  ALTERNATE RECORD KEY NOMBRE-REST WITH DUPLICATES
                  FILE STATUS OTR-STAT.

           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-USUAR-W
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
       COPY "P:\PROG\PROYECT\FUENTES\FD-RESTR.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUAR.CBL".

       WORKING-STORAGE  SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".

       01 NOM-USUAR-W           PIC X(70).
       01 ORD-DOS.
          02 COMANDO-DOS   PIC X(80).
      
       01 LINEA-LLEGAD-W        PIC X(100).

       01 DATO-LLEGADA-W.
          02 IP-DATOS-LLEGAD-W     PIC X(20).
          02 IP-DESCARGA-LLEGAD-W  PIC X(20).
          02 DIR-LLEGAD-W          PIC X(30).
          02 FECHA-NUM-LLEGAD-W    PIC X(6).
          02 USU-LLEGAD-W          PIC X(4).
          02 CLAVE-LLEGAD-W        PIC X(8).

       01 DATOS-ENVIO.
          02 ESTADO-ENV         PIC XX.
          02 FILLER             PIC X VALUE "|".
          02 OPC-ENV            PIC XX.
          02 FILLER             PIC X VALUE "|".
          02 COD-ENV            PIC X(4).
          02 FILLER             PIC X VALUE "|".
          02 NOMBRE-ENV         PIC X(30).
          02 FILLER             PIC X VALUE "|".
          02 SESION-ENV         PIC X(29).
          02 FILLER             PIC X VALUE "|".
          02 DETALLE-ENV        PIC X(50).
          02 FILLER             PIC X VALUE "|".
          02 NIT-ENV            PIC X(10).
          02 FILLER             PIC X VALUE "|".
          02 CLAVE-ENV          PIC X(6).
          02 FILLER             PIC X VALUE "|".
          02 CLAVE2-ENV         PIC X(6).
          02 FILLER             PIC X VALUE "|".
          02 PESOS-ENV          PIC X(12).
          02 FILLER             PIC X VALUE "|".
          02 ALM-ENV            PIC X(5).
          02 FILLER             PIC X VALUE "|".
          02 C-COST-ENV         PIC X(4).
          02 FILLER             PIC X VALUE "|".
          02 SUCURSAL-ENV       PIC X.
          02 FILLER             PIC X VALUE "|".
          02 INVENT-ENV         PIC X.
	  02 FILLER             PIC X VALUE "|".
          02 NOMBRE-ALTE-ENV    PIC X(50).      
	  02 FILLER             PIC X VALUE "|".
          02 UNIDAD-PROG-ENV    PIC X.


       LINKAGE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\ISAPICTX.CBL".
       
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
         
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-RESTRICCION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE                    
           ELSE
              MOVE OTR-STAT                TO MSJ1-HTML
              MOVE NOM-REST-W              TO MSJ2-HTML
              MOVE "INDEX"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
        
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
       
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                TO MSJ1-HTML
              MOVE NOM-USUAR-W             TO MSJ2-HTML
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
           MOVE COBW3-GET-DATA TO LINEA-LLEGAD-W.

           UNSTRING LINEA-LLEGAD-W DELIMITED BY "|"
              INTO IP-DATOS-LLEGAD-W, IP-DESCARGA-LLEGAD-W, DIR-LLEGAD-W, FECHA-NUM-LLEGAD-W, USU-LLEGAD-W, CLAVE-LLEGAD-W
           END-UNSTRING.

       ABRIR-USUARIO.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
      
           OPEN I-O ARCHIVO-USUNET
           EVALUATE OTR-STAT
             WHEN "00"  CONTINUE
             WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
                        CLOSE       ARCHIVO-USUNET
                        OPEN I-O    ARCHIVO-USUNET
                        INITIALIZE REG-USUNET
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
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN
                CLOSE ARCHIVO-USUNET
           END-READ

         
           IF NOMBRE-USUNET = SPACES
              MOVE "Validacion de usuarios"      TO MSJ1-HTML
              MOVE "Falta configurar usuario"    TO MSJ2-HTML
              MOVE "Sc"                          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

           MOVE IP-DATOS-LLEGAD-W        TO IP-DATOS-USUNET
           MOVE IP-DESCARGA-LLEGAD-W     TO IP-DESCARGA-USUNET
           MOVE DIR-LLEGAD-W             TO DIRECTORIO-USUNET
           REWRITE REG-USUNET END-REWRITE.

       ASIGNAR-NOMBRES.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.

           INITIALIZE NOM-USUAR-W NOM-REST-W
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           
           MOVE "\\" TO NOM-USUAR-W
       
           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET

           INSPECT NOM-USUAR-W REPLACING FIRST " "
                                           BY "\"

           MOVE NOM-USUAR-W    TO NOM-REST-W

           INSPECT NOM-USUAR-W REPLACING FIRST "                              "
                                           BY DIRECTORIO-USUNET

           INSPECT NOM-USUAR-W   REPLACING FIRST "                       "
                                              BY "\CONTROL\SC-ARCHUSU.DAT".
                                                                                 

           INSPECT NOM-REST-W   REPLACING FIRST "                         "
                                             BY "PROG\DATOS\SC-ARCHREST.SC".

       ABRIR-RESTRICCION.
           OPEN INPUT ARCHIVO-RESTRICCION.
           INITIALIZE LLAVE-REST
     
           MOVE USU-LLEGAD-W TO COD-REST
           READ ARCHIVO-RESTRICCION WITH NO LOCK
                INVALID KEY
                  MOVE "99"                   TO MSJ1-HTML
                  MOVE "No existe el usuario" TO MSJ2-HTML
                  MOVE "ARCHIVO-RESTRICCION"  TO MSJ3-HTML
                  GO TO ENVIAR2-ERROR
           END-READ.

           CLOSE ARCHIVO-RESTRICCION.

           INSPECT CLAVE-LLEGAD-W CONVERTING "0A1EIO23U45e6i8o79u" TO CARAC-SENC

           IF CLAVE-LLEGAD-W = CLAVE-REST
              CONTINUE
           ELSE
              MOVE "26"                   TO MSJ1-HTML
              MOVE "Clave invalida"       TO MSJ2-HTML
              MOVE "INDEX"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       ABRIR-USUARIOS.
           OPEN INPUT ARCHIVO-USUARIOS.

           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.

       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
 
           OPEN I-O ARCHIVO-SESION
      
           EVALUATE OTR-STAT
             WHEN "00"  CONTINUE
             WHEN "35"  OPEN OUTPUT ARCHIVO-SESION
                        CLOSE ARCHIVO-SESION
                        GO TO INICIAR-SESION
             WHEN OTHER GO TO CERRAR-SESION
           END-EVALUATE.

        
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
           MOVE ID-REST      TO NIT-SESION
           MOVE FECHA-TOTAL  TO FECHA-SESION
           MOVE HORA-TOTAL   TO HORA-SESION.
           INITIALIZE NOMBRE-SESION
       
           READ ARCHIVO-SESION WITH NO LOCK
                INVALID KEY
                  MOVE IP-CLIENTE-W TO IP-SESION
                  WRITE REG-SESION  END-WRITE
                NOT INVALID KEY
                   ADD 1 TO SG-TOTAL               
                   GO TO ASIGNAR-SESION
           END-READ.

           CLOSE ARCHIVO-SESION.

       ENVIO-DATOS.                                

           INITIALIZE DATOS-ENVIO

           IF NOMBRE-ALTERNO = SPACES OR NOMBRE-ALTERNO = NOMBRE-USU
                MOVE 1 TO SUCURSAL-ENV
           END-IF

           MOVE 00                TO ESTADO-ENV
           MOVE LLAVE-SESION      TO SESION-ENV
           MOVE COD-REST          TO COD-ENV 
           MOVE NOMBRE-REST       TO NOMBRE-ENV
           MOVE 01                TO OPC-ENV

           MOVE NOMBRE-USU       TO DETALLE-ENV
           MOVE NIT-USU          TO NIT-ENV
           MOVE CLAVE-USU        TO CLAVE-ENV
           MOVE CLAVE-2-USU      TO CLAVE2-ENV
           MOVE INVENT-USU       TO INVENT-ENV
	   MOVE NOMBRE-ALTERNO   TO NOMBRE-ALTE-ENV
           MOVE "P"              TO UNIDAD-PROG-ENV

           MOVE "datosrecibidos"  TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO       TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"   USING COBW3
           MOVE "../PAGINAS/RECIBIDOS.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML. 
       
       CERRAR-SESION.
           CALL "COBW3_END_SESSION" using COBW3.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.
        
       FIN-VALIDAR-SESION.

       COPY "P:\PROG\PROYECT\FUENTES\SC-WEB.CBL".   

