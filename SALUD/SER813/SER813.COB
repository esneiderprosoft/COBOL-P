      *============================================================
      * CREACION DE ARCHIVO JSON CONSULTA DE CODIGOS IPS - SER813
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

           SELECT ARCHIVO-COD-IPS LOCK MODE IS AUTOMATIC
                    ASSIGN NOM-CODIP-W
                    ORGANIZATION IS INDEXED;
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS COD-IPS
                    ALTERNATE RECORD KEY IS
                    NOMBRE-IPS WITH DUPLICATES
                    FILE STATUS IS OTR-STAT. 

           SELECT ARCHIVO-CIUDAD LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-CIUDAD-W
      * "P:\PROG\DATOS\SC-ARCHCIUD.DAT";
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-CIU
                  ALTERNATE RECORD KEY IS NOMBRE-CIU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.                
                                                                     
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-CODIP.CBL".
       COPY "..\..\FUENTES\FD-CIUDA.CBL".

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".

       01 LLEGADA-W                    PIC X(100).

       77 NOM-CODIP-W                  PIC X(70).
       77 NOM-CIUDAD-W                 PIC X(70).
       77 DATOS-PLANO-W                PIC X(500).
        
       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(30).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 COD-IPS-LLEGADA-W         PIC X(12).
          02 OPERADOR-LLEGADA-W        PIC X(4).

       01 LIN-1.  
          02 FILLER                  PIC X(7)   VALUE "{*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-IPS-J               PIC X(12).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOMBRE-IPS-J            PIC X(80). 
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)  VALUE "*TEL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TEL-IPS-J               PIC X(15).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*FUNCIONARIO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FUNCIONARIO-IPS-J       PIC X(30).  
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)   VALUE "*CODCIUDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CODIGO-CIUDAD-J         PIC X(5).  
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*CIUDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CIUDAD-IPS-J            PIC X(20).   
          02 FILLER                  PIC XX     VALUE "*}".    
          02 CIERRE-LIN-1            PIC X.

      
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
              MOVE "SER813"                 TO MSJ3-HTML
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
              MOVE "SER813"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-COD-IPS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-CODIP-W              TO MSJ2-HTML
              MOVE "SER813"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR    
           END-IF. 

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CIUDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-CIUDAD-W             TO MSJ2-HTML
              MOVE "SER813"                 TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO LLEGADA-W


           UNSTRING LLEGADA-W DELIMITED BY "|"
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, COD-IPS-LLEGADA-W,
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
006960       WHEN "00"  CONTINUE
006970       WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
006980                  INITIALIZE REG-USUNET
006990                  WRITE REG-USUNET
007000                  CLOSE ARCHIVO-USUNET
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

           INITIALIZE NOM-CODIP-W NOM-CIUDAD-W
           
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".
 
           MOVE "\\" TO NOM-CODIP-W NOM-CIUDAD-W 

           INSPECT NOM-CODIP-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-CODIP-W REPLACING FIRST " "
                                            BY "\"

           MOVE NOM-CODIP-W TO NOM-CIUDAD-W


           INSPECT NOM-CODIP-W REPLACING FIRST "                       "           
                                           BY  "PROG\DATOS\SC-CODIP.DAT".

           INSPECT NOM-CIUDAD-W REPLACING FIRST "                          "           
                                             BY "PROG\DATOS\SC-ARCHCIUD.DAT".  
                                                             

       ABRIR-ARCHIVO-IPS.

           OPEN INPUT ARCHIVO-COD-IPS
                      ARCHIVO-CIUDAD.

           INITIALIZE REG-IPS.
           MOVE "{*IPS*:[" TO  DATOS-PLANO-W   
           INSPECT DATOS-PLANO-W  REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO.
           MOVE 0 TO SW-FIN.

       LEER-ARCHIVO-IPS.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-PLANO-W  
              PERFORM DATOS-ENVIO
           END-IF

           READ ARCHIVO-COD-IPS NEXT WITH NO LOCK AT END 
                GO TO CERRAR-COD-IPS
           END-READ.
           
           MOVE COD-IPS         TO COD-IPS-J
           MOVE NOMBRE-IPS      TO NOMBRE-IPS-J
           MOVE TEL-IPS         TO TEL-IPS-J
           MOVE FUNCIONARIO-IPS TO FUNCIONARIO-IPS-J
           MOVE CIUDAD-IPS      TO CIUDAD-IPS-J
           MOVE CIUDAD-IPS-J    TO COD-CIU
           READ ARCHIVO-CIUDAD WITH NO LOCK
                INVALID KEY
                  MOVE COD-CIU  TO NOMBRE-CIU
           END-READ

           MOVE NOMBRE-CIU      TO CIUDAD-IPS-J

           MOVE ","               TO CIERRE-LIN-1

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES
           
           GO TO LEER-ARCHIVO-IPS.
                                    
        CERRAR-COD-IPS.

           INITIALIZE LIN-1
008890     MOVE LIN-1 TO DATOS-PLANO-W
008900     PERFORM DATOS-ENVIO
008910
008920     MOVE "]}" TO DATOS-PLANO-W
008930     PERFORM DATOS-ENVIO.
           
           CLOSE ARCHIVO-COD-IPS
                 ARCHIVO-CIUDAD.
 
       PAGINA-CONFIG.  
010720     MOVE "statuscode" TO COBW3-CNV-NAME
010730     MOVE "00"         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010720     MOVE "programa-id" TO COBW3-CNV-NAME
010730     MOVE "SER813"      TO COBW3-CNV-VALUE
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