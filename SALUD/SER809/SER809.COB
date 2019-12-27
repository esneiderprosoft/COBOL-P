      *==================================================================
      *CREA JSON CON MEDICAMENTOS|FARMACOS
      *==================================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

           SELECT ARCHIVO-JSON LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-POS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-POS-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-POS
                  ALTERNATE RECORD KEY IS
                  NOMBRE-POS WITH DUPLICATES
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
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-CODPO.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL". 

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON      PIC X(500).

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".


       77 NOM-PLANO-W PIC X(100).
       77 NOM-POS-W   PIC X(100).
       77 NOM-USUAR-W        PIC X(100).

       01 LINEA-LLEGAD-W                PIC X(400).
       01  DATO-LLEGADA-W.
           02 SESION-LLEGAD-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 DIRECTORIO-LLEGAD-W       PIC X(30).
           02 MES-TRABAJO-W             PIC XX.


       01 DATOS-ENVIO.
          02 DATO-ENV                   PIC XX.
          02 DATO1-ENV                  PIC X(100).


      *   *******VARIABLES JSON MEDICAMENTOS
       01 LIN-CODPOS.
          02 FILLER                  PIC X(7)   VALUE "{*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-POS-J               PIC X(13).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-POS-J           PIC X(57).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*ORDEN*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ORD-DOSIS-POS-J         PIC X(12).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*CANT-DOSIS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CANT-DOSIS-POS-J        PIC X(9).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*UNID-DOSIS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UNID-DOSIS-POS-J        PIC X(13).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*CADA-DOSIS*:".
          02 FILLER                  PIC XX     VALUE "*,".
          02 CADA-DOSIS-POS-J        PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*CANT-FREC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CANT-FREC-POS-J         PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*UNID-FREC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UNID-FREC-POS-J         PIC X(7).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*VIA-DOSIS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VIA-DOSIS-POS-J         PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*CANT-DOSIF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CANT-DOSIF-POS-J        PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*TIPO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-POS-J              PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)   VALUE "*TIPO-CUM*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-CUM-J              PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER            PIC X(19)  VALUE "*CANT-FREC-DOSISF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CANT-FREC-DOSISF-POS-J  PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*FREC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FREC-POS-J              PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*VIA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VIA-DOSISF-POS-J        PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*UNSERV*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UNSERV-POS-J            PIC XX.
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-LIN-CODPOS            PIC X.


      
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
              MOVE "ARCHIVO-USUNET"         TO MSJ2-HTML
              MOVE "SER809"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
          
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "ARCHIVO-SESION"         TO MSJ2-HTML
              MOVE "SER809"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-POS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"         
              CONTINUE            
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE "SER809- ARCHIVO FARMA"    TO MSJ2-HTML
              MOVE NOM-POS-W                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "SER809-ARCHIVO-JSON"    TO MSJ2-HTML
              MOVE NOM-PLANO-W              TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO LINEA-LLEGAD-W

           INITIALIZE DATO-LLEGADA-W
           UNSTRING LINEA-LLEGAD-W DELIMITED BY "|"
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, MES-TRABAJO-W
           END-UNSTRING.

           MOVE SESION-LLEGAD-W   TO LLAVE-SESION-W.

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
              MOVE "Validacion de usuarios"      TO MSJ1-HTML
              MOVE "Falta configurar usuario"    TO MSJ2-HTML
              MOVE "Sc"                          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           INITIALIZE NOM-USUAR-W 
           MOVE "\\" TO NOM-USUAR-W 

           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                           BY  IP-DATOS-USUNET
           INSPECT NOM-USUAR-W REPLACING FIRST " "
                                            BY "\"

           MOVE NOM-USUAR-W TO NOM-POS-W NOM-PLANO-W 

           INSPECT NOM-USUAR-W REPLACING 
           FIRST "                              "
               BY DIRECTORIO-LLEGAD-W


           INSPECT NOM-USUAR-W   REPLACING FIRST "                       "
                                              BY "\CONTROL\SC-ARCHUSU.DAT".
           INSPECT NOM-POS-W REPLACING FIRST   "                       "
                                            BY "PROG\DATOS\SC-FARMA.DAT"

           MOVE NOM-PLANO-JSON-W  TO NOM-PLANO-W.

           INSPECT NOM-PLANO-W REPLACING FIRST "                                         "
                                            BY "SER809-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"

           INSPECT NOM-PLANO-W REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                           BY SESION-LLEGAD-W

           MOVE NOM-PLANO-W TO DATO1-ENV.


       ABRIR-FARMA.
           OPEN OUTPUT ARCHIVO-JSON
           OPEN INPUT ARCHIVO-POS

           INITIALIZE REG-JSON.
           MOVE "{*SER809*:[" TO  DATOS-JSON
           INSPECT DATOS-JSON REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON.

       
       LEER-FARMA.
           READ ARCHIVO-POS NEXT WITH NO LOCK AT END GO TO CERRAR-FARMA.

           IF INICIAL-POS = "*"
              GO TO LEER-FARMA
           END-IF

           MOVE LLAVE-POS               TO COD-POS-J
           MOVE NOMBRE-POS              TO DESCRIP-POS-J
           MOVE GRUPO-POS               TO TIPO-CUM-J

           MOVE ORDEN-DOSIS-POS         TO ORD-DOSIS-POS-J
           INSPECT ORD-DOSIS-POS-J  REPLACING ALL "'"  BY " ".

           MOVE CANT-DOSIS-POS          TO CANT-DOSIS-POS-J
           MOVE UNID-DOSIS-POS          TO UNID-DOSIS-POS-J
           MOVE CADA-DOSIS-POS          TO CADA-DOSIS-POS-J
           MOVE CANT-FREC-POS           TO CANT-FREC-POS-J
           MOVE UNID-FREC-POS           TO UNID-FREC-POS-J
           MOVE VIA-DOSIS-POS           TO VIA-DOSISF-POS-J

           MOVE CANT-DOSIF-POS          TO CANT-DOSIF-POS-J
           MOVE TIPO-DOSIF-POS          TO TIPO-POS-J
           MOVE CANT-FREC-DOSIF-POS     TO CANT-FREC-DOSISF-POS-J
           MOVE FREC-DOSIF-POS          TO FREC-POS-J
           MOVE VIA-DOSIF-POS           TO VIA-DOSISF-POS-J
           MOVE UNSERV-POS              TO UNSERV-POS-J
           MOVE EPS-POS                 TO EPS-POS-J

           INSPECT DESCRIP-POS-J REPLACING ALL "*" BY "X"

           INSPECT LIN-CODPOS REPLACING ALL "*" BY CARAC-COMILLA
 
           MOVE "," TO CIERRE-LIN-CODPOS
           MOVE LIN-CODPOS TO DATOS-JSON
           WRITE REG-JSON

           GO TO LEER-FARMA.

       CERRAR-FARMA.

           INITIALIZE CIERRE-LIN-CODPOS
008030     MOVE LIN-CODPOS TO DATOS-JSON
008040     WRITE REG-JSON END-WRITE
008050
008060     MOVE "]}" TO DATOS-JSON
008070     WRITE REG-JSON END-WRITE
008090     CLOSE ARCHIVO-POS
           CLOSE ARCHIVO-JSON.


       ENVIO-DATOS.
           MOVE "00" TO DATO-ENV
           MOVE "datosrecibidos"  TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO        TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"   USING COBW3
           MOVE "../PAGINAS/RECIBIDOS.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.
      
       COPY "..\..\FUENTES\SC-WEB.CBL".      
