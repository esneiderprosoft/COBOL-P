      *============================================================
      * SER810H  CONSULTA DE PACIENTES POR NOMBRE
      * SI COINCIDE FECHA NACIMIENTO MUESTRA ERROR..
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

           SELECT ARCHIVO-PACIENTES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PACIE-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PACI
                  ALTERNATE RECORD KEY  DESCRIP-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  EPS-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  ID-COTIZ-PACI WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.
           
           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-USUAR-W 
      *"../CONTROL/SC-ARCHUSU.DAT"
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
       COPY "..\..\FUENTES\FD-PACIE.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".

       77 NOM-PACIE-W                  PIC X(70).
       77 NOM-USUAR-W                  PIC X(70).
       77 DATOS-PLANO-W                PIC X(200).
       77 1ER-NOM-EDIT                PIC X(12).
       77 2DO-NOM-EDIT                PIC X(12).


       01 LLEGADA-W                    PIC X(300).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(30).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 LLAVE-APEL-PACI-W.
             05 1ER-APEL-PACI-W.
                07 INICIAL-PACI-W             PIC X.
                07 RESTO-PACI-W               PIC X(14).
             05 2DO-APEL-PACI-W               PIC X(15).
          02 NOMBRE-PACI-W.
             05 1ER-NOM-PACI-W                PIC X(12).
             05 2DO-NOM-PACI-W                PIC X(12).
          02 DESCRIP-PACI-LLEGA.
             03 LLAVE-APEL-PACI-LLEGA.
                05 1ER-APEL-PACI-LLEGA.
                   07 INICIAL-PACI-LLEGA             PIC X.
                   07 RESTO-PACI-LLEGA               PIC X(14).
                05 2DO-APEL-PACI-LLEGA               PIC X(15).
             03 NOMBRE-PACI-LLEGA.
                05 1ER-NOM-PACI-LLEGA                PIC X(12).
                05 2DO-NOM-PACI-LLEGA                PIC X(12).
          02 COD-PACI-LLEGADA-W                  PIC X(15).
          02 NACIM-PACI-LLEGADA-W.
             05 ANO-PACI-W                       PIC 9(4).
             05 MES-PACI-W                       PIC 99.
             05 DIA-PACI-W                       PIC 99.
          02 OPERADOR-LLEGADA-W                  PIC 99. 

       01 DATOS-ENVIO-N.
          02 SW-INVALID-ENV                      PIC XX. 
         
       01 LIN-1.  
          02 FILLER                  PIC X(11)   VALUE "{*NOMBRES*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-PACI-J.
             03 LLAVE-APEL-PACI-J.
                05 1ER-APEL-PACI-J.
                   07 INICIAL-PACI-J  PIC X.
                   07 RESTO-PACI-J    PIC X(14).
                05 2DO-APEL-PACI-J    PIC X(15).
             03 NOMBRE-PACI-J.
                05 1ER-NOM-PACI-J     PIC X(12).
                05 2DO-NOM-PACI-J     PIC X(12).          
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*CEDULA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-PACI-J              PIC X(15).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*EPS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EPS-PACI-J.
             03 EPS1-PACI-J          PIC X(3).
             03 EPS2-PACI-J          PIC X(3).         
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FECHA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NACIM-PACI-J.
             03 ANO-PACI-J           PIC 9(4).
             03 MES-PACI-J           PIC 99.
             03 DIA-PACI-J           PIC 99.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*NOMBRE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CIUDAD-PACI-J.
             03 DPTO-PACI-J          PIC 99.
             03 CIUD-PACI-J          PIC 9(3).  
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
              MOVE "ARCHIVO-USUNET"         TO MSJ2-HTML
              MOVE "CON809"                 TO MSJ3-HTML
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
              MOVE "SER810H"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USUAR-W              TO MSJ2-HTML
              MOVE "SER810H"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR    
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                TO MSJ1-HTML
              MOVE NOM-PACIE-W             TO MSJ2-HTML
              MOVE "SER810H"               TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, LLAVE-APEL-PACI-W, NOMBRE-PACI-W, DESCRIP-PACI-LLEGA
                   COD-PACI-LLEGADA-W, NACIM-PACI-LLEGADA-W, OPERADOR-LLEGADA-W  
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
      
           INITIALIZE LLAVE-USUNET.

       LEER-USUARIO.
                                                              
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
           CLOSE ARCHIVO-USUNET.
      
           IF NOMBRE-USUNET = SPACES
              MOVE "SC-2"                      TO MSJ1-HTML
007240        MOVE "Falta configurar usuarios" TO MSJ2-HTML
007250        MOVE "SER810H"                   TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.

           INITIALIZE NOM-PACIE-W
           
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           MOVE "\\" TO NOM-PACIE-W


           INSPECT NOM-PACIE-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

           INSPECT NOM-PACIE-W REPLACING FIRST " "
                                             BY "\" 
           INSPECT NOM-PACIE-W REPLACING FIRST "                              "
                                             BY DIR-CONTAB-LLEGADA-W
           MOVE NOM-PACIE-W TO NOM-USUAR-W 
        
           INSPECT NOM-PACIE-W REPLACING FIRST "                     "
                                            BY "\CONTROL\SC-PACIE.DAT".
           INSPECT NOM-USUAR-W REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHUSU.DAT".

       ABRIR-PACIENTES.
           OPEN INPUT ARCHIVO-PACIENTES.

           MOVE LLAVE-APEL-PACI-W TO LLAVE-APEL-PACI.

           START ARCHIVO-PACIENTES KEY = LLAVE-APEL-PACI
                 INVALID KEY GO TO CERRAR-PACIENTES
           END-START.


           INITIALIZE REG-PACI. 

           MOVE "{*DUPLICADO*:[" TO  DATOS-PLANO-W  
           INSPECT DATOS-PLANO-W  REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO.
           MOVE 0 TO SW-FIN.
         
       LEER-PACIENTES.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-PLANO-W
              PERFORM DATOS-ENVIO
           END-IF
           
           READ ARCHIVO-PACIENTES NEXT WITH NO LOCK AT END
                GO TO CERRAR-PACIENTES
           END-READ


              MOVE "SC-1"          TO MSJ1-HTML
              MOVE LLAVE-APEL-PACI    TO MSJ2-HTML
              MOVE NOMBRE-PACI  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR.
       OTR.

           IF LLAVE-APEL-PACI IS NOT = LLAVE-APEL-PACI-W
              MOVE 0 TO SW-FIN
              GO TO LEER-PACIENTES
           END-IF


           IF COD-PACI = COD-PACI-LLEGADA-W
               MOVE 0 TO SW-FIN
               GO TO LEER-PACIENTES
           END-IF

           MOVE 1ER-NOM-PACI TO 1ER-NOM-EDIT
           MOVE 2DO-NOM-PACI TO 2DO-NOM-EDIT




           IF  1ER-NOM-EDIT = 1ER-NOM-PACI-W
           AND 2DO-NOM-EDIT = 2DO-NOM-PACI-W
              CONTINUE
           ELSE
              MOVE 0 TO SW-FIN
              GO TO LEER-PACIENTES
           END-IF


           IF  ANO-PACI =  ANO-PACI-W 
           AND MES-PACI =  MES-PACI-W
               MOVE "2B" TO SW-INVALID-ENV
           ELSE
              MOVE 0 TO SW-FIN
              GO TO LEER-PACIENTES
           END-IF

           MOVE COD-PACI     TO COD-PACI-J
           MOVE DESCRIP-PACI   TO DESCRIP-PACI-J
           MOVE EPS-PACI     TO EPS-PACI-J
           MOVE NACIM-PACI   TO NACIM-PACI-J
           MOVE CIUDAD-PACI  TO CIUDAD-PACI-J

           MOVE ","               TO CIERRE-LIN-1

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES

           GO TO LEER-PACIENTES.

       CERRAR-PACIENTES.
           INITIALIZE LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA

008890     MOVE LIN-1 TO DATOS-PLANO-W
008900     PERFORM DATOS-ENVIO
008910
008920     MOVE "]}" TO DATOS-PLANO-W
008930     PERFORM DATOS-ENVIO.

           CLOSE ARCHIVO-PACIENTES.
               
       PAGINA-CONFIG.  
010720     MOVE "statuscode" TO COBW3-CNV-NAME
010730     MOVE "00"         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010720     MOVE "programa-id" TO COBW3-CNV-NAME
010730     MOVE "SER810H"      TO COBW3-CNV-VALUE
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