      *CONSULTA DATOS DEL USUARIO
       IDENTIFICATION DIVISION.
      * PROGRAM-ID. "INDEX00".
      
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".

           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-USUAR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;        
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT MOVIMIENTO-DIARIO LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-MOV-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-MOV
                  ALTERNATE RECORD KEY SECUENCIA-MOV
                  ALTERNATE RECORD KEY LLAVE-FEC
                  ALTERNATE RECORD KEY LLAVE-DOC-MOV   WITH DUPLICATES
                  ALTERNATE RECORD KEY COSTO-MOV       WITH DUPLICATES
                  ALTERNATE RECORD KEY NRO-ORD-MOV     WITH DUPLICATES
                  ALTERNATE RECORD KEY LLAVE-LIBRE-MOV WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUAR.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-MOVIM.CBL".

       WORKING-STORAGE  SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".

       01 NOM-SURT-W             PIC X(70).
       01 NOM-USUAR-W            PIC X(70).
       01 NOM-MOV-W              PIC X(70).

       01  LINEA-LLEGAD-W               PIC X(100).

       01  DATO-LLEGADA-W.
           02 SESION-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 CONTAB-LLEGADA-W          PIC X(30).
           02 MES-CONTAB-LLEGADA-W      PIC XX.

       01 DATOS-ENVIO.
          02 ESTADO-ENV         PIC XX.
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
          02 TIPO-EMPR-ENV      PIC X.
          02 FILLER             PIC X VALUE "|".
          02 MENU-SUC-ENV       PIC X.
          02 FILLER             PIC X VALUE "|".
          02 PRESUP-ENV         PIC X.
          02 FILLER             PIC X VALUE "|".
          02 COD-CIU-ENV        PIC X(5).
          02 FILLER             PIC X VALUE "|".
          02 SEG-W-ENV          PIC 9.
          02 FILLER             PIC X VALUE "|".
          02 CLAVE-PRE-MOV-ENV  PIC X(6).
       
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
              MOVE "INDEX00"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                TO MSJ1-HTML
              MOVE NOM-SESION-W            TO MSJ2-HTML
              MOVE "INDEX00"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-USUAR-W      TO MSJ2-HTML
              MOVE "INDEX00"         TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MOVIMIENTO-DIARIO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-MOV-W        TO MSJ2-HTML
              MOVE "INDEX00"        TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA    TO LINEA-LLEGAD-W.

           UNSTRING LINEA-LLEGAD-W DELIMITED BY "|"
              INTO SESION-W, CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W     
           END-UNSTRING.

           MOVE SESION-W   TO LLAVE-SESION-W.

       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
       
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.
           GO TO VALIDAR-SESION.
         
       FIN-VALIDAR-SESION.

       ABRIR-USUARIO.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
      
           OPEN INPUT ARCHIVO-USUNET
           EVALUATE OTR-STAT
             WHEN "00"  CONTINUE
             WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
                        CLOSE       ARCHIVO-USUNET
                        OPEN I-O    ARCHIVO-USUNET
                        INITIALIZE REG-USUNET
                        WRITE REG-USUNET
                        CLOSE       ARCHIVO-USUNET
                        MOVE "99"                          TO MSJ1-HTML
                        MOVE "Falta configurar usuario"    TO MSJ2-HTML
                        MOVE "Menu de Configuracion"       TO MSJ3-HTML
                        GO TO ENVIAR2-ERROR
             WHEN OTHER GO TO CERRAR-SESION
           END-EVALUATE.
      
           INITIALIZE REG-USUNET.

       LEER-USUARIO.
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN. 
       
           CLOSE ARCHIVO-USUNET.
           
           IF NOMBRE-USUNET = SPACES
           OR NIT-USUNET = ZEROS
              MOVE "99"                                          TO MSJ1-HTML
              MOVE "Falta configurar Nombre o Nit de usuarios"   TO MSJ2-HTML
              MOVE "Menu de Configuracion"                       TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       INICIAR-RESTRICION.
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           INITIALIZE NOM-USUAR-W
           MOVE "\\" TO NOM-USUAR-W
         
           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET
           INSPECT NOM-USUAR-W REPLACING FIRST " "
                                           BY "\"

           INSPECT NOM-USUAR-W REPLACING FIRST "                              "
                                           BY CONTAB-LLEGADA-W

           MOVE NOM-USUAR-W TO NOM-MOV-W
           INSPECT NOM-USUAR-W  REPLACING FIRST "                       "
                                             BY "\CONTROL\SC-ARCHUSU.DAT".

       ABRIR-USUARIOS.
           OPEN INPUT ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.

       ABRIR-MOVIMIENTO.
           EVALUATE MES-CONTAB-LLEGADA-W 
             WHEN "01" INSPECT NOM-MOV-W REPLACING FIRST "                   "
                                                 BY    "\ENE\SC-ARCHMOV.DAT"
             WHEN "02" INSPECT NOM-MOV-W REPLACING FIRST "                   "
                                                 BY    "\FEB\SC-ARCHMOV.DAT"
             WHEN "03" INSPECT NOM-MOV-W REPLACING FIRST "                   "
                                                 BY    "\MAR\SC-ARCHMOV.DAT"
             WHEN "04" INSPECT NOM-MOV-W REPLACING FIRST "                   "
                                                 BY    "\ABR\SC-ARCHMOV.DAT"
             WHEN "05" INSPECT NOM-MOV-W REPLACING FIRST "                   "
                                                 BY    "\MAY\SC-ARCHMOV.DAT"
             WHEN "06" INSPECT NOM-MOV-W REPLACING FIRST "                   "
                                                 BY    "\JUN\SC-ARCHMOV.DAT"
             WHEN "07" INSPECT NOM-MOV-W REPLACING FIRST "                   "
                                                 BY    "\JUL\SC-ARCHMOV.DAT"
             WHEN "08" INSPECT NOM-MOV-W REPLACING FIRST "                   "
                                                 BY    "\AGT\SC-ARCHMOV.DAT"
             WHEN "09" INSPECT NOM-MOV-W REPLACING FIRST "                   "
                                                 BY    "\SEP\SC-ARCHMOV.DAT"
             WHEN "10" INSPECT NOM-MOV-W REPLACING FIRST "                   "
                                                 BY    "\OCT\SC-ARCHMOV.DAT"
             WHEN "11" INSPECT NOM-MOV-W REPLACING FIRST "                   "
                                                 BY    "\NOV\SC-ARCHMOV.DAT"
             WHEN "12" INSPECT NOM-MOV-W REPLACING FIRST "                   "
                                                 BY    "\DIC\SC-ARCHMOV.DAT"
             WHEN "13" INSPECT NOM-MOV-W REPLACING FIRST "                       "
                                                 BY    "\CONTROL\SC-ARCHMOV.DAT"
             WHEN "14" INSPECT NOM-MOV-W REPLACING FIRST "                   "
                                                 BY    "\CIE\SC-ARCHMOV.DAT"
           END-EVALUATE.


       BUSCAR-BLOQUEO.
           OPEN INPUT MOVIMIENTO-DIARIO.
           MOVE "99999999999" TO SECUENCIA-MOV.
           READ MOVIMIENTO-DIARIO RECORD KEY IS SECUENCIA-MOV
                INVALID KEY 
                  MOVE 0 TO COST1-MOV
           END-READ
           CLOSE MOVIMIENTO-DIARIO.

       ENVIO-DATOS.
           INITIALIZE DATOS-ENVIO

           IF NOMBRE-ALTERNO = SPACES OR NOMBRE-ALTERNO = NOMBRE-USU
                MOVE 1 TO SUCURSAL-ENV
           END-IF.

           MOVE 00               TO ESTADO-ENV
           MOVE NOMBRE-USU       TO DETALLE-ENV
           MOVE NIT-USU          TO NIT-ENV
           MOVE CLAVE-USU        TO CLAVE-ENV
           MOVE CLAVE-2-USU      TO CLAVE2-ENV
           MOVE INVENT-USU       TO INVENT-ENV
           MOVE TIPO-EMPR-USU    TO TIPO-EMPR-ENV
           MOVE MENU-SUC-USU     TO MENU-SUC-ENV
           MOVE PRESUP-USU       TO PRESUP-ENV
           MOVE COD-CIU-USU      TO COD-CIU-ENV
           MOVE COST1-MOV        TO SEG-W-ENV
      *     MOVE CLAVE-PRE-MOV    TO CLAVE-PRE-MOV-ENV

           MOVE "datosrecibidos"  TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO       TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"   USING COBW3
           MOVE "../PAGINAS/RECIBIDOS.ASPX"  TO SALIDA-HTML
      *     MOVE "..\FRAMEWORKS\PAGINAS\RECIBIDOS.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML.

       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.                

       COPY "P:\PROG\PROYECT\FUENTES\SC-WEB.CBL".
      * COPY "P:\PROG\PROYECT\FUENTES\SC-WEB19.CBL".
       
