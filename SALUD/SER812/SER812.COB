      *=================================================================
      * CREACION DE JSON SERVICIOS HOSPITALARIOS - SER812
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

           SELECT ARCHIVO-SERV-HOSP LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-SERHO-W
      *  "/PROG/DATOS/SC-SERVHOS.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CODIGO-SERHO
                  FILE STATUS IS OTR-STAT.

     
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-SERHO.CBL".   

       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".
                                      
       77 NOM-SERHO-W                  PIC X(70).
       77 DATOS-PLANO-W                PIC X(90).
         
       01 LLEGADA-W                    PIC X(100). 
       
       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(30).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 CODIGO-LLEGADA-W          PIC 99.
          02 OPERADOR-LLEGADA-W        PIC X(4).          

       01 LIN-1.
          02 FILLER                    PIC X(6) VALUE "{*ID*:".           
          02 FILLER                    PIC X VALUE "*".
          02 CODIGO-SERHO-J            PIC 99.
          02 FILLER                    PIC XX     VALUE "*,".       
          02 FILLER                    PIC X(14)   VALUE "*DESCRIPCION*:".
          02 FILLER                    PIC X      VALUE "*".
          02 DESCRIP-SERHO-J           PIC X(25).
          02 FILLER                    PIC XX     VALUE "*}".  
          02 CIERRE-LIN-1              PIC X.


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
              MOVE "SER812"                 TO MSJ3-HTML
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
              MOVE "SER812"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SERV-HOSP.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-SERHO-W              TO MSJ2-HTML
              MOVE "SER812"                 TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, CODIGO-LLEGADA-W,
                   OPERADOR-LLEGADA-W     
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.


       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
          
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.

       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
      
           OPEN INPUT ARCHIVO-USUNET
           EVALUATE OTR-STAT
006960     WHEN "00"  CONTINUE
006970     WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
00698                 INITIALIZE REG-USUNET
                      WRITE REG-USUNET
007000                CLOSE ARCHIVO-USUNET
007010                  MOVE "Validacion de Usuario"       TO MSJ1-HTML
007020                  MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007030                  MOVE "Sc"                          TO MSJ3-HTML
007040                  GO TO ENVIAR2-ERROR
007050       WHEN OTHER GO TO CERRAR-SESION
007060     END-EVALUATE.
      
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
         
           INITIALIZE NOM-SERHO-W 

           MOVE "\\" TO NOM-SERHO-W 

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-SERHO-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-SERHO-W REPLACING FIRST " "
                                            BY "\"
           
           INSPECT NOM-SERHO-W REPLACING FIRST "                         "
                                            BY "PROG\DATOS\SC-SERVHOS.DAT".

       ABRIR-SERVICIOS.
           OPEN INPUT ARCHIVO-SERV-HOSP.
         
           INITIALIZE REG-SERHO.

           MOVE "{*SERVICIO*:[" TO  DATOS-PLANO-W
           INSPECT DATOS-PLANO-W  REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO.
           MOVE 0 TO SW-FIN.

       LEER-SERVICIOS.

           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE 
               MOVE LIN-1 TO DATOS-PLANO-W
               PERFORM DATOS-ENVIO           
           END-IF
        
           READ ARCHIVO-SERV-HOSP NEXT WITH NO LOCK AT END 
                GO TO CERRRAR-SERVICIOS
           END-READ. 
           
           MOVE CODIGO-SERHO TO CODIGO-SERHO-J

           INSPECT DESCRIP-SERHO REPLACING ALL '"' BY " "
           INSPECT DESCRIP-SERHO REPLACING ALL "*" BY " "
           INSPECT DESCRIP-SERHO REPLACING ALL "Ñ" BY CARAC-ENE-MAY 
           INSPECT DESCRIP-SERHO REPLACING ALL "ñ" BY CARAC-ENE-MIN
              
           MOVE DESCRIP-SERHO TO DESCRIP-SERHO-J

           MOVE ","               TO CIERRE-LIN-1

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
           GO TO LEER-SERVICIOS.

       CERRRAR-SERVICIOS.

           INITIALIZE CIERRE-LIN-1
008890     MOVE LIN-1 TO DATOS-PLANO-W
008900     PERFORM DATOS-ENVIO
008910
008920     MOVE "]}" TO DATOS-PLANO-W
008930     PERFORM DATOS-ENVIO.   

           CLOSE ARCHIVO-SERV-HOSP.
                 
       PAGINA-CONFIG.  
010720     MOVE "statuscode" TO COBW3-CNV-NAME
010730     MOVE "00"         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010720     MOVE "programa-id" TO COBW3-CNV-NAME
010730     MOVE "SER812"      TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010760     MOVE "..\PAGINAS\RECIBIDOSLISTADO.ASPX" TO SALIDA-HTML
010770     PERFORM ABRIR-HTML.

007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.

       DATOS-ENVIO.
010720     MOVE "datosrecibidos" TO COBW3-CNV-NAME
010730     MOVE DATOS-PLANO-W    TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_REPEAT" USING COBW3.

008040 COPY "..\..\FUENTES\SC-WEB19.CBL".

