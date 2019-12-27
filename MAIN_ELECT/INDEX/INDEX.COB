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

           SELECT ARCHIVO-MODULO   LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-MODULO-LNK
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-MODUL
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-JSON
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.
      
       DATA DIVISION.
       FILE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-RESTR.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-MODUL.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-PLANO.
           02 DATOS-PLANO     PIC X(700).

       WORKING-STORAGE  SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".
       77 CONT-W                PIC 99.
       77 NOM-PLANO-W           PIC X(90).
       77 NOM-USUAR-W           PIC X(70).
       77 NOM-MODULO-LNK        PIC X(70).

       01 LINEA-LLEGAD-W        PIC X(100).

       01 DATO-LLEGADA-W.
          02 USU-LLEGAD-W          PIC X(4).
          02 CLAVE-LLEGAD-W        PIC X(8).
          02 MODULO-LLEGAD-W       PIC X(3).

       01 CLAVE-GBC.
          02 LETRAS-GBC         PIC X(2) VALUE "SC".
          02 FECHA-GBC.
             03 ANO-GBC         PIC 99.
             03 MES-GBC         PIC 99.
             03 DIA-GBC         PIC 99.

      *Entero 2 digitos edit
       01 E2-EDIT.
          02 E2-EDIT1 PIC 9.
          02 E2-EDIT2 PIC 9.
     
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

       01 LIN-1.  
          02 FILLER                  PIC X(6)   VALUE "{*ID*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ID-USU-J                PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*NOMUSU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOM-USU-J               PIC X(60).
          02 FILLER                  PIC XX     VALUE "*,".    
          02 FILLER                  PIC X(9)   VALUE "*NITUSU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NIT-USU-J               PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*DIRUSU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIRECC-USU-J            PIC X(60).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*EMAILUSU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EMAIL-USU-J             PIC X(60).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*CLAVEUSU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLAVE-USU-J             PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*SERVIDORUSU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SERV-EMAIL-USU-J        PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*PUERTOUSU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PUERTO-EMAIL-USU-J      PIC X(4).         
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*SSLUSU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SSL-EMAIL-USU-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*IPUSU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 IP-DATOS-USU-J          PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*IP-DESCARGA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 IP-DESC-USU-J           PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*DIR-CONT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIRECTORIO-USU-J        PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*UNID-PROG*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UNIDAD-PROGRAM-USU-J    PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*CONTAB*:[".

       01 LIN-2.
          02 FILLER                  PIC X(7)   VALUE "{*DIR*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIR-J                   PIC X(20).
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-LIN-02           PIC X.

       01 LIN-4.
          02 FILLER                  PIC X(7)   VALUE "{*NOM*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOM-J                   PIC X(5).
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-LIN-04           PIC X.


       01 LIN-3.
          02 FILLER                  PIC X(8)   VALUE "{*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CODIGO-MODUL-J          PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-MODUL-J         PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*ACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ACTIVAR-MODUL-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*}".
          02 CIERRE-LIN-03           PIC X.


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
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-MODULO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE
           ELSE
              MOVE OTR-STAT                TO MSJ1-HTML
              MOVE NOM-MODULO-LNK          TO MSJ2-HTML
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
              INTO USU-LLEGAD-W, CLAVE-LLEGAD-W, MODULO-LLEGAD-W
           END-UNSTRING.

       ABRIR-MODULO.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHMODUL.DAT" TO NOM-MODULO-LNK

           OPEN INPUT ARCHIVO-MODULO.
           EVALUATE OTR-STAT
               WHEN "00" CLOSE ARCHIVO-MODULO
               WHEN "35" OPEN OUTPUT ARCHIVO-MODULO
                         CLOSE ARCHIVO-MODULO
                         OPEN I-O ARCHIVO-MODULO
                         PERFORM ESCRIBIR-MODUL
                         CLOSE ARCHIVO-MODULO
                         MOVE "99"                       TO MSJ1-HTML
                         MOVE "Falta configurar Modulo"  TO MSJ2-HTML       
                         MOVE "Sc"                       TO MSJ3-HTML
                         GO TO ENVIAR2-ERROR
               WHEN OTHER GO TO CERRAR-SESION
           END-EVALUATE.

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
                        MOVE "99"      TO MSJ1-HTML
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

           IF USU-LLEGAD-W = 0099
           AND CLAVE-LLEGAD-W  = 641218
               GO TO PAGINA-CONFIG
           END-IF.
         
           IF USU-LLEGAD-W = NIT-USUNET
           AND CLAVE-LLEGAD-W   = "@dm1"  
               GO TO PAGINA-CONFIG 
           END-IF.

           IF NOMBRE-USUNET = SPACES
              MOVE "99"   TO MSJ1-HTML
              MOVE "Falta configurar usuario" TO MSJ2-HTML
              MOVE "Sc"   TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           INITIALIZE NOM-REST-W
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           
           MOVE "\\" TO NOM-REST-W
       
           INSPECT NOM-REST-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET

           INSPECT NOM-REST-W REPLACING FIRST "                          "
                                           BY "\PROG\DATOS\SC-ARCHREST.SC".

           IF USU-LLEGAD-W = "GEBC"
              MOVE "ADMI" TO COD-REST
           END-IF.

       ABRIR-RESTRICCION.
           INITIALIZE LLAVE-REST
           OPEN INPUT ARCHIVO-RESTRICCION.

           IF USU-LLEGAD-W = "GEBC"
              MOVE "ADMI" TO COD-REST
           ELSE
              MOVE USU-LLEGAD-W TO COD-REST
           END-IF.

           READ ARCHIVO-RESTRICCION WITH NO LOCK
                INVALID KEY
                   MOVE "99"                   TO MSJ1-HTML
                   MOVE "No existe el usuario" TO MSJ2-HTML
                   MOVE "ARCHIVO-RESTRICCION"  TO MSJ3-HTML
                   GO TO ENVIAR2-ERROR
           END-READ

           CLOSE ARCHIVO-RESTRICCION.

       VALIDAR-CLAVE-REST.
           IF USU-LLEGAD-W = "GEBC"
              ACCEPT FECHA-GBC FROM DATE

      *        MOVE ANO-GBC TO E2-EDIT
      *        COMPUTE ANO-GBC = E2-EDIT1 + E2-EDIT2

              MOVE MES-GBC TO E2-EDIT
              COMPUTE MES-GBC = E2-EDIT1 + E2-EDIT2

              MOVE DIA-GBC TO E2-EDIT
              COMPUTE DIA-GBC = E2-EDIT1 + E2-EDIT2

              INSPECT CLAVE-GBC    CONVERTING "0A1EIO23U45e6i8o79u" TO CARAC-SENC
              MOVE CLAVE-GBC       TO CLAVE-REST
           END-IF.

           INSPECT CLAVE-LLEGAD-W CONVERTING "0A1EIO23U45e6i8o79u" TO CARAC-SENC


           IF CLAVE-LLEGAD-W = CLAVE-REST
              CONTINUE
           ELSE
              MOVE "26"                   TO MSJ1-HTML
              MOVE "Clave invalida"       TO MSJ2-HTML
              MOVE "INDEX"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.


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


       LEER-MODULO.
           OPEN INPUT ARCHIVO-MODULO

           MOVE MODULO-LLEGAD-W  TO LLAVE-MODUL
           READ ARCHIVO-MODULO WITH NO LOCK
                INVALID KEY
                        MOVE "99"      TO MSJ1-HTML
                        MOVE "No existe modulo"  TO MSJ2-HTML
                        MOVE "INDEX"   TO MSJ3-HTML
                        GO TO ENVIAR2-ERROR
           END-READ

           CLOSE ARCHIVO-MODULO.

           IF ACTIVAR-MODUL = "N"
              MOVE "99"       TO MSJ1-HTML
              MOVE "Modulo desactivado" TO MSJ2-HTML
              MOVE "INDEX"    TO MSJ2-HTML
           END-IF.

       ENVIO-DATOS.                                
           INITIALIZE DATOS-ENVIO

           MOVE 00                    TO ESTADO-ENV
           MOVE 01                    TO OPC-ENV
           MOVE LLAVE-SESION          TO SESION-ENV

           IF USU-LLEGAD-W = "GEBC"
              MOVE "GEBC"                      TO COD-ENV
              MOVE "ADMINISTRADOR DEL SISTEMA" TO NOMBRE-ENV
           ELSE
              MOVE COD-REST              TO COD-ENV 
              MOVE NOMBRE-REST           TO NOMBRE-ENV
           END-IF

           MOVE "datosrecibidos"  TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO       TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"   USING COBW3
           MOVE "../PAGINAS/RECIBIDOS.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML

           GO TO CERRAR-SESION.

       PAGINA-CONFIG.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-USU-NET.JSON" TO NOM-PLANO-W

           OPEN OUTPUT ARCHIVO-JSON.

           MOVE "{*Usunet*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.

           MOVE USU-LLEGAD-W          TO ID-USU-J
           MOVE NOMBRE-USUNET         TO NOM-USU-J
           MOVE NIT-USUNET            TO NIT-USU-J
           MOVE DIRECC-USUNET         TO DIRECC-USU-J
           MOVE EMAIL-USUNET          TO EMAIL-USU-J
           MOVE CLAVE-EMAIL-USUNET    TO CLAVE-USU-J
           MOVE SERV-EMAIL-USUNET     TO SERV-EMAIL-USU-J
           MOVE PUERTO-EMAIL-USUNET   TO PUERTO-EMAIL-USU-J
           MOVE SSL-EMAIL-USUNET      TO SSL-EMAIL-USU-J

           MOVE IP-DATOS-USUNET       TO IP-DATOS-USU-J
           INSPECT IP-DATOS-USU-J REPLACING ALL "\" BY "/"

           MOVE IP-DESCARGA-USUNET    TO IP-DESC-USU-J
           INSPECT IP-DESC-USU-J REPLACING ALL "\" BY "/"

           MOVE DIRECTORIO-USUNET     TO DIRECTORIO-USU-J

           IF UNIDAD-PROGRAM-USUNET = ZEROS OR LOW-VALUES
              MOVE ZEROS TO UNIDAD-PROGRAM-USUNET
           END-IF

           MOVE UNIDAD-PROGRAM-USUNET TO UNIDAD-PROGRAM-USU-J

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-1        TO DATOS-PLANO.
           WRITE REG-PLANO

           PERFORM LLENAR-TABLA-DIR VARYING I FROM 1 BY 1
                                             UNTIL I > 50.

           IF CONT-W > 0
              MOVE SPACE TO CIERRE-LIN-02
              MOVE LIN-2        TO DATOS-PLANO
              INSPECT LIN-2 REPLACING ALL "*" BY CARAC-COMILLA
              WRITE REG-PLANO END-WRITE
           END-IF

           MOVE "],*NOMINA*:["   TO DATOS-PLANO 
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO END-WRITE

           INITIALIZE CONT-W

           PERFORM LLENAR-TABLA-NOM VARYING I FROM 1 BY 1
                                             UNTIL I > 30.

           IF CONT-W > 0
              MOVE SPACE TO CIERRE-LIN-04
              MOVE LIN-4        TO DATOS-PLANO
              INSPECT LIN-4 REPLACING ALL "*" BY CARAC-COMILLA
              WRITE REG-PLANO END-WRITE
           END-IF

           OPEN INPUT ARCHIVO-MODULO
           INITIALIZE CONT-W REG-MODUL.


           MOVE "]}],*MODULOS*:[" TO DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO END-WRITE.

       LEER-MODULOS.
           READ ARCHIVO-MODULO NEXT WITH NO LOCK AT END GO TO CERRAR-MODULOS.

           MOVE LLAVE-MODUL         TO CODIGO-MODUL-J
           MOVE DESCRIP-MODUL       TO DESCRIP-MODUL-J
           MOVE ACTIVAR-MODUL       TO ACTIVAR-MODUL-J

           MOVE ","      TO CIERRE-LIN-03
           INSPECT LIN-3 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-3    TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           GO TO LEER-MODULOS.

       CERRAR-MODULOS.

           INITIALIZE CODIGO-MODUL-J DESCRIP-MODUL-J CIERRE-LIN-03
           INSPECT LIN-3 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-3    TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           MOVE "]}"      TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           CLOSE ARCHIVO-MODULO
                 ARCHIVO-JSON.

           MOVE "datosrecibidos"  TO COBW3-CNV-NAME
           MOVE "00|02"           TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"   USING COBW3
           MOVE "../PAGINAS/RECIBIDOS.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML. 
       
       CERRAR-SESION.
           CALL "COBW3_END_SESSION" using COBW3.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.
        
       LLENAR-TABLA-DIR.
           IF DIR-DIR-NET (I) = SPACES OR LOW-VALUES
              CONTINUE
           ELSE
               MOVE DIR-DIR-NET (I)    TO DIR-J 
               INSPECT DIR-J REPLACING ALL "\" BY "/"
               MOVE ","      TO CIERRE-LIN-02
               INSPECT LIN-2 REPLACING ALL "*" BY CARAC-COMILLA
               MOVE LIN-2        TO DATOS-PLANO
               WRITE REG-PLANO END-WRITE
               ADD 1 TO CONT-W
           END-IF.

       LLENAR-TABLA-NOM.
           IF DIR-NOM-NET (I) = SPACES OR LOW-VALUES
              CONTINUE
           ELSE
              MOVE DIR-NOM-NET (I)  TO NOM-J
              MOVE ","          TO CIERRE-LIN-04
              INSPECT LIN-4 REPLACING ALL "*" BY CARAC-COMILLA
              MOVE LIN-4  TO DATOS-PLANO
              WRITE REG-PLANO END-WRITE
              ADD 1 TO CONT-W
           END-IF.


       ESCRIBIR-MODUL.
           MOVE "NEW"                            TO LLAVE-MODUL
           MOVE "Contabilidad e Inventarios   "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE

           MOVE "CER"                            TO LLAVE-MODUL
           MOVE "Cereales                     "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE

           MOVE "OLI"                            TO LLAVE-MODUL
           MOVE "Olivos                       "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE

           MOVE "LAB"                            TO LLAVE-MODUL
           MOVE "Laboratorios                 "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE

           MOVE "SAL"                            TO LLAVE-MODUL
           MOVE "Salud                        "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE

           MOVE "HIC"                            TO LLAVE-MODUL
           MOVE "Historias clinicas           "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE

           MOVE "BOM"                            TO LLAVE-MODUL
           MOVE "Estaciones de servicio       "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE

           MOVE "HOT"                            TO LLAVE-MODUL
           MOVE "Hoteleria                    "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE

           MOVE "BAR"                            TO LLAVE-MODUL
           MOVE "Bares y Restaurante          "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE

           MOVE "TAL"                            TO LLAVE-MODUL
           MOVE "Talleres                     "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE

           MOVE "ALQ"                            TO LLAVE-MODUL
           MOVE "Alquiler maquinaria          "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE

           MOVE "TEL"                            TO LLAVE-MODUL
           MOVE "Telecomunicaciones           "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE

           MOVE "NOM"                            TO LLAVE-MODUL
           MOVE "Nomina                       "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE

           MOVE "SEP"                            TO LLAVE-MODUL
           MOVE "Servicio Publico             "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE

           MOVE "SED"                            TO LLAVE-MODUL
           MOVE "servicio Domiciliario        "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE

           MOVE "COS"                            TO LLAVE-MODUL
           MOVE "Costos                       "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE

           MOVE "PAR"                            TO LLAVE-MODUL
           MOVE "Parqueo                      "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE

           MOVE "PRS"                            TO LLAVE-MODUL
           MOVE "Presupuesto                  "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE

           MOVE "PRD"                            TO LLAVE-MODUL
           MOVE "Predial                      "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE

           MOVE "RX "                            TO LLAVE-MODUL
           MOVE "Rayos X                      "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE

           MOVE "TAX"                            TO LLAVE-MODUL
           MOVE "TAX                          "  TO DESCRIP-MODUL
           MOVE "N"                              TO ACTIVAR-MODUL
           WRITE REG-MODUL END-WRITE.

       FIN-VALIDAR-SESION.

       COPY "P:\PROG\PROYECT\FUENTES\SC-WEB.CBL".   

