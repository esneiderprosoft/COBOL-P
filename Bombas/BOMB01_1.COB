000012* BOMBAS - GUARDA COMPROBANTE DE COMBUSTIBLES. 
000020 IDENTIFICATION DIVISION. 
000021* PROGRAM-ID. "BOMB01_1".
000022
000023 PROGRAM-ID. "HttpExtensionProc".
000024 ENVIRONMENT DIVISION.
000025 CONFIGURATION SECTION.
000026 INPUT-OUTPUT SECTION.
000027 FILE-CONTROL.
000080 COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
000150 COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".


           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-USUAR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-CL-TANQUES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-CLTAN-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY COD-CL-TANQ 
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-TEXTO
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.


000400 DATA DIVISION.
000410 FILE SECTION.
000420 COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
000421 COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
000450 COPY "P:\PROG\PROYECT\FUENTES\FD-USUAR.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-CLTAN.CBL".

       FD  ARCHIVO-TEXTO
           LABEL RECORD STANDARD.
       01  REG-PLANO.
           02 DATOS-PLANO            PIC X(6). 

000540 WORKING-STORAGE  SECTION.
000550 COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
000560 COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".

       77 NOM-USUAR-W            PIC X(70).
       77 NOM-CLTAN-W            PIC X(70).
       77 NOM-PLANO-W            PIC X(90).
       77 SUCURSAL-W             PIC X.


       01 VARIABLES.
          02 LECTURA-ACTUAL      PIC 9(6)  OCCURS 300.



       01 LINEA-LLEGAD-W             PIC X(100).

       01 DATO-LLEGADA-W.
          02 SESION-LLEGAD-W        PIC X(29).
          02 DIRECTORIO-LLEGAD-W    PIC X(30).
          02 CARPTA-LLEGAD-W        PIC X(9).
          02 NOVEDAD-LLEGAD-W       PIC X.
          02 COD-CL-W               PIC XX.
          02 DESCRIP-CL-W           PIC X(30).


       01 DATOS-ENVIO.
          02 DAT0-ENV            PIC XX.

000791 LINKAGE SECTION.
000792 COPY "P:\PROG\PROYECT\FUENTES\ISAPICTX.CBL".
000793
000794 PROCEDURE DIVISION WITH stdcall LINKAGE USING ISAPI-CTX-CNT.
000795
000810 DECLARATIVES.
000811 I-O-TEST SECTION.
000812     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUNET.         
000813 ESCR-EXCEPTIONES.
000814     IF OTR-STAT = "00"
000815        CONTINUE 
000816     ELSE
000817        MOVE OTR-STAT                TO MSJ1-HTML
000818        MOVE NOM-USU-W               TO MSJ2-HTML
000819        MOVE "BOMB01_1"              TO MSJ3-HTML
000820        GO TO ENVIAR2-ERROR
000821     END-IF.
001030
001040 I-O-TEST SECTION.
001050     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.         
001060 ESCR-EXCEPTIONES.
001070     IF OTR-STAT = "00"
001080        CONTINUE
001090     ELSE
001100        MOVE OTR-STAT                TO MSJ1-HTML
001101        MOVE NOM-SESION-W            TO MSJ2-HTML
001102        MOVE "BOMB01_1"              TO MSJ3-HTML
001103        GO TO ENVIAR2-ERROR
001104     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" 
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-USUAR-W      TO MSJ2-HTML
006680        MOVE "BOMB01_1"       TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CL-TANQUES.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" 
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-CLTAN-W      TO MSJ2-HTML
006680        MOVE "BOMB01_1"       TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TEXTO.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-PLANO-W      TO MSJ2-HTML
006680        MOVE "BOMB01_1"       TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.
001105
001150 END DECLARATIVES.
001160
001161 INICIAR-IIS.
001162     MOVE LOW-VALUE TO COBW3.
001163     MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
001164     CALL "COBW3_INIT" USING COBW3.
001220
001221 LEER-DATO-HTML.
001852     MOVE "datosh" TO COBW3-SEARCH-DATA.
001853     CALL "COBW3_GET_VALUE" USING COBW3.
001854     MOVE COBW3-GET-DATA    TO LINEA-LLEGAD-W.

           INITIALIZE DATO-LLEGADA-W

           UNSTRING LINEA-LLEGAD-W DELIMITED BY "|"
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W, NOVEDAD-LLEGAD-W, COD-CL-W, DESCRIP-CL-W
           END-UNSTRING.

001855     MOVE SESION-LLEGAD-W    TO LLAVE-SESION-W.

001238 INICIAR-SESION.
001245     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
001246
000820     MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
000830     ACCEPT HORA-TOTAL FROM TIME.
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.

001303 ABRIR-USUARIO.
001304     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
001305
001306     OPEN INPUT ARCHIVO-USUNET
001307     EVALUATE OTR-STAT
001308       WHEN "00"  CONTINUE
001309       WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
001310                  CLOSE       ARCHIVO-USUNET
001311                  OPEN I-O    ARCHIVO-USUNET
001312                  INITIALIZE REG-USUNET
001314                  WRITE REG-USUNET
001315                  CLOSE       ARCHIVO-USUNET
001316                  MOVE "Validacion de usuarios"      TO MSJ1-HTML
001317                  MOVE "Falta configurar usuario"    TO MSJ2-HTML
001318                  MOVE "Sc"                          TO MSJ3-HTML
001319                  GO TO ENVIAR2-ERROR
001320       WHEN OTHER GO TO CERRAR-SESION
001321     END-EVALUATE.
001322
001332     INITIALIZE REG-USUNET.

007110 LEER-USUARIO.
007120     READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
007130
007140     CLOSE ARCHIVO-USUNET.
007200     
007210     IF NOMBRE-USUNET = SPACES
007220     OR NIT-USUNET = ZEROS
007230        MOVE "Validacion de Sesion"        TO MSJ1-HTML
007240        MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007250        MOVE "Sc"                          TO MSJ3-HTML
007260        GO TO ENVIAR2-ERROR
007270     END-IF.

       INICIAR-RESTRICION.
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
001614     INITIALIZE NOM-USUAR-W NOM-CLTAN-W

001615     MOVE "\\" TO NOM-USUAR-W NOM-CLTAN-W

001617     INSPECT NOM-USUAR-W REPLACING FIRST "                    "
001618                                    BY IP-DATOS-USUNET
001621     INSPECT NOM-USUAR-W REPLACING FIRST " "
001624                                     BY "\"
007292     INSPECT NOM-USUAR-W REPLACING FIRST "                              "
007293                                     BY DIRECTORIO-LLEGAD-W

007190     INSPECT NOM-USUAR-W  REPLACING FIRST "                       "
007200                                       BY "\CONTROL\SC-ARCHUSU.DAT".

001617     INSPECT NOM-CLTAN-W REPLACING FIRST "                    "
001618                                      BY IP-DATOS-USUNET
001621     INSPECT NOM-CLTAN-W REPLACING FIRST " "
001624                                      BY "\"
007292     INSPECT NOM-CLTAN-W REPLACING FIRST "                         "
007293                                      BY "PROG\DATOS\SC-ARCLTAN.DAT".

       LEER-USUARIO.

           OPEN INPUT ARCHIVO-USUARIOS
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.


       LEER-CLTANQUES.
           MOVE NOVEDAD-LLEGAD-W   TO NOVEDAD-W
           OPEN INPUT ARCHIVO-CL-TANQUES.
           MOVE 0 TO SW-INVALID
           MOVE COD-CL-W      TO COD-CL-TANQ


           READ ARCHIVO-CL-TANQUES WITH NO LOCK
                INVALID KEY 
                           MOVE 1 TO SW-INVALID
           END-READ.
           CLOSE ARCHIVO-CL-TANQUES.

           
           IF NOVEDAD-W = 7 AND SW-INVALID = 0 GO TO ERROR-1.
           IF NOVEDAD-W = 8 AND SW-INVALID = 1 GO TO ERROR-1.
           IF NOVEDAD-W = 9 AND SW-INVALID = 0 GO TO RETIRO.
           IF NOVEDAD-W = 9 AND SW-INVALID = 1 GO TO ERROR-1.


          
       ABRIR-TABLA1.
           MOVE "C:\PROSOFT\TEMP\CLTAN-TBL-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.TXT"  TO NOM-PLANO-W

           INSPECT NOM-PLANO-W REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                            BY SESION-LLEGAD-W.

           OPEN INPUT ARCHIVO-TEXTO.
           INITIALIZE I VARIABLES.
       LEER-TABLA.
           READ ARCHIVO-TEXTO NEXT WITH NO LOCK AT END GO TO CERRAR-TABLA.

           ADD 1 TO I

           IF I > 300
              GO TO CERRAR-TABLA
           ELSE
              MOVE DATOS-PLANO TO LECTURA-ACTUAL (I)
           END-IF.

           GO TO LEER-TABLA.

       CERRAR-TABLA.
           CLOSE ARCHIVO-TEXTO.

           IF NOVEDAD-W = 7 AND SW-INVALID = 1 GO TO NUEVO.
           IF NOVEDAD-W = 8 AND SW-INVALID = 0 GO TO CAMBIO.


       NUEVO.
           OPEN I-O  ARCHIVO-CL-TANQUES.

           MOVE COD-CL-W       TO COD-CL-TANQ
           MOVE DESCRIP-CL-W   TO DESCRIP-CL-TANQ
           MOVE VARIABLES      TO TABLA-CL-TANQ
           WRITE REG-CL-TANQ   END-WRITE
           GO TO CERRAR-ARCHIVO.

       CAMBIO.
           OPEN I-O  ARCHIVO-CL-TANQUES.

           MOVE COD-CL-W       TO COD-CL-TANQ
           MOVE DESCRIP-CL-W   TO DESCRIP-CL-TANQ
           MOVE VARIABLES      TO TABLA-CL-TANQ
           REWRITE REG-CL-TANQ   END-REWRITE
           GO TO CERRAR-ARCHIVO.

       RETIRO.
           OPEN I-O  ARCHIVO-CL-TANQUES.
           DELETE    ARCHIVO-CL-TANQUES.

       CERRAR-ARCHIVO.
           CLOSE ARCHIVO-CL-TANQUES.

002147 ENVIO-DATOS.
004378     MOVE "datosrecibidos"  TO COBW3-CNV-NAME
004393     MOVE 00     TO COBW3-CNV-VALUE
004403     CALL "COBW3_SET_CNV"   USING COBW3
004413     MOVE "../PAGINAS/RECIBIDOS.ASPX"  TO SALIDA-HTML
004430     PERFORM ABRIR-HTML.

002545 CERRAR-SESION.
002546     CALL "COBW3_FREE" USING COBW3.
002547     MOVE 1 TO PROGRAM-STATUS.
002548     EXIT PROGRAM.

       ERROR-1.
           MOVE "99"            TO MSJ1-HTML
           MOVE SW-INVALID      TO MSJ2-HTML
           MOVE "BOMB01_1"      TO MSJ3-HTML
           GO TO ENVIAR2-ERROR.

002566 COPY "P:\PROG\PROYECT\FUENTES\SC-WEB.CBL".
002567
