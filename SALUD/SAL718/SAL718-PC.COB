      *=================================================================
      *CREACION DE ARCHIVO JSON SER11A F8 DE CUPS -SALUD
      *=================================================================  
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  


           SELECT ARCHIVO-ESPEC-CUP LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-ESPCUPS-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-ESP-CUP
                  FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-CUPS LOCK MODE IS AUTOMATIC
                  ASSIGN ARCHCUPS-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS LLAVE-CUP
                  ALTERNATE RECORD KEY IS DESCRIP-CUP WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LLAVE-ALT-CUP WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.                     


           SELECT ARCHIVO-JSON
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT. 
                                          
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-CUPS.CBL".
       COPY "..\..\FUENTES\FD-ESCUP.CBL".
     

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-PLANO.
           02 DATOS-PLANO     PIC X(1000).

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-PLANO-W                   PIC X(90).
       77 NOM-ESPCUPS-W                 PIC X(60).
       77 ARCHCUPS-W                    PIC X(60). 

       01 LLEGADA-W                     PIC X(100).

       77 MES-SAL-W                     PIC X(4).

        01 VAR-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 DIR-CONTAB-LLEGADA-W      PIC X(30).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 OPERADOR-LLEGADA-W        PIC X(4).


       01 DATOS-ENVIO.
          02 DAT0-ENV                   PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 NOMBRE-ENV                 PIC X(28). 



     

       01 LIN-1.  
          02 FILLER                  PIC X(10)  VALUE "{*CODIGO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-ESP-CUPS-J.
             05 GRUPO-ESP-CUP-J      PIC XX.
             05 NUMERO-ESP-CUP-J     PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-CUPS-J          PIC X(50).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*ESPECIALD1*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA1-CUP-J      PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*ESPECIALD2*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA2-CUP-J      PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*ESPECIALD3*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA3-CUP-J      PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*ESPECIALD4*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA4-CUP-J      PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*ESPECIALD5*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA5-CUP-J      PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*ESPECIALD6*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA6-CUP-J      PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*ESPECIALD7*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA7-CUP-J      PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*ESPECIALD8*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA8-CUP-J      PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*ESPECIALD9*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA9-CUP-J      PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESPECIALD10*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA10-CUP-J     PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESPECIALD11*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA11-CUP-J     PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESPECIALD12*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA12-CUP-J     PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESPECIALD13*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA13-CUP-J     PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESPECIALD14*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA14-CUP-J     PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESPECIALD15*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA15-CUP-J     PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESPECIALD16*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA16-CUP-J     PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESPECIALD17*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA17-CUP-J     PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESPECIALD18*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA18-CUP-J     PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESPECIALD19*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA19-CUP-J     PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESPECIALD20*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA20-CUP-J     PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESPECIALD21*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA21-CUP-J     PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESPECIALD22*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA22-CUP-J     PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESPECIALD23*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA23-CUP-J     PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESPECIALD24*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA24-CUP-J     PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESPECIALD25*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA25-CUP-J     PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESPECIALD26*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA26-CUP-J     PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESPECIALD27*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA27-CUP-J     PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESPECIALD28*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA28-CUP-J     PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*ESPECIALD29*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPECIALIDA29-CUP-J     PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*OPER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OPER-ESP-CUP-J          PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*FECHA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-ESP-CUP-J         PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*SEXO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SEXO-ESP-CUPS-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*ATIENDE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ATIENDE-ESP-CUP-J       PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*PYP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PYP-CUPS-J              PIC X.          
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*TIP_PROC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-CUPS-J             PIC 9.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*FINALIDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FINALID-CUPS-J          PIC 99.
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
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-USU-W                 TO MSJ2-HTML
              MOVE "SER11A"                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                  TO MSJ1-HTML
              MOVE NOM-SESION-W              TO MSJ2-HTML
              MOVE "SER11A"                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.


       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ESPEC-CUP.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE NOM-ESPCUPS-W              TO MSJ2-HTML
              MOVE "SAL71A"                   TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CUPS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE ARCHCUPS-W                 TO MSJ2-HTML
              MOVE "SAL71A-CUPS"              TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO LLEGADA-W.

           UNSTRING LLEGADA-W DELIMITED BY "|"
               INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W 
           END-UNSTRING.

           MOVE LLAVESESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-JSON-W.
           ACCEPT HORA-JSON-W FROM TIME.
           MOVE OPERADOR-LLEGADA-W TO OPER-JSON-W.


      *     MOVE "KK"                   TO MSJ1-HTML
      *     MOVE NOM-ARCH-CUPS-W               TO MSJ2-HTML
      *     MOVE "KALETH"                  TO MSJ3-HTML
      *     GO TO ENVIAR2-ERROR.


       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W

           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.

       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
         
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
           INITIALIZE NOM-PLANO-W ARCHCUPS-W NOM-ESPCUPS-W.
		   
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".
		   
           MOVE "\\" TO NOM-PLANO-W ARCHCUPS-W NOM-ESPCUPS-W
		   
           INSPECT ARCHCUPS-W REPLACING FIRST "                    "
                                              BY IP-DATOS-USUNET

           INSPECT ARCHCUPS-W REPLACING FIRST " "
                                              BY "\"

           MOVE ARCHCUPS-W TO NOM-ESPCUPS-W
           INSPECT NOM-ESPCUPS-W REPLACING FIRST "                         "
                                              BY "PROG\DATOS\SC-ESP-CUP.DAT".

           INSPECT ARCHCUPS-W REPLACING FIRST "                          "
                                           BY "PROG\DATOS\SC-ARCHCUPS.DAT".


           INSPECT NOM-PLANO-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-PLANO-W REPLACING FIRST  "                 "
                                            BY  "\PROG\DATOS\JSON\"  

           INSPECT NOM-PLANO-W  REPLACING FIRST "                          "
                                             BY "XXXXXXXXXXXXXXXXXXXXX.JSON"

           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXX"
                                             BY NOMBRE-JSON-W. 

           MOVE NOMBRE-JSON-W TO NOMBRE-ENV.


       ABRIR-NOMBRE-ESPCUPS.
           OPEN INPUT ARCHIVO-CUPS
                      ARCHIVO-ESPEC-CUP.

           EVALUATE OTR-STAT
             WHEN "00" CONTINUE
             WHEN "35" OPEN OUTPUT ARCHIVO-CUPS
                       CLOSE       ARCHIVO-CUPS
                       GO TO ABRIR-NOMBRE-ESPCUPS
           END-EVALUATE.

           MOVE "00" TO DAT0-ENV.

       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-CUPS.
           MOVE "{*CUPS*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO.
           MOVE 0 TO SW-FIN.

       LEER-NOMBRE-ESPCUPS.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-PLANO
              WRITE REG-PLANO END-WRITE
           END-IF

           READ ARCHIVO-CUPS NEXT WITH NO LOCK AT END 
                GO TO CERRAR-NOMBRE-ESPCUPS
           END-READ.

           READ ARCHIVO-ESPEC-CUP NEXT WITH NO LOCK AT END
                GO TO CERRAR-NOMBRE-ESPCUPS
           END-READ.

           IF COD-ESP-CUP = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE
               MOVE COD-ESP-CUP     TO COD-ESP-CUPS-J
               MOVE DESCRIP-CUP     TO DESCRIP-CUPS-J

               MOVE ESP-CUP(1)      TO ESPECIALIDA1-CUP-J
               MOVE ESP-CUP(2)      TO ESPECIALIDA2-CUP-J
               MOVE ESP-CUP(3)      TO ESPECIALIDA3-CUP-J
               MOVE ESP-CUP(4)      TO ESPECIALIDA4-CUP-J
               MOVE ESP-CUP(5)      TO ESPECIALIDA5-CUP-J
               MOVE ESP-CUP(6)      TO ESPECIALIDA6-CUP-J
               MOVE ESP-CUP(7)      TO ESPECIALIDA7-CUP-J
               MOVE ESP-CUP(8)      TO ESPECIALIDA8-CUP-J
               MOVE ESP-CUP(9)      TO ESPECIALIDA9-CUP-J
               MOVE ESP-CUP(10)     TO ESPECIALIDA10-CUP-J
               MOVE ESP-CUP(11)     TO ESPECIALIDA11-CUP-J
               MOVE ESP-CUP(12)     TO ESPECIALIDA12-CUP-J
               MOVE ESP-CUP(13)     TO ESPECIALIDA13-CUP-J
               MOVE ESP-CUP(14)     TO ESPECIALIDA14-CUP-J
               MOVE ESP-CUP(15)     TO ESPECIALIDA15-CUP-J
               MOVE ESP-CUP(16)     TO ESPECIALIDA16-CUP-J
               MOVE ESP-CUP(17)     TO ESPECIALIDA17-CUP-J
               MOVE ESP-CUP(18)     TO ESPECIALIDA18-CUP-J
               MOVE ESP-CUP(19)     TO ESPECIALIDA19-CUP-J
               MOVE ESP-CUP(20)     TO ESPECIALIDA20-CUP-J  
               MOVE ESP-CUP(21)     TO ESPECIALIDA21-CUP-J  
               MOVE ESP-CUP(22)     TO ESPECIALIDA22-CUP-J  
               MOVE ESP-CUP(23)     TO ESPECIALIDA23-CUP-J  
               MOVE ESP-CUP(24)     TO ESPECIALIDA24-CUP-J  
               MOVE ESP-CUP(25)     TO ESPECIALIDA25-CUP-J  
               MOVE ESP-CUP(26)     TO ESPECIALIDA26-CUP-J  
               MOVE ESP-CUP(27)     TO ESPECIALIDA27-CUP-J  
               MOVE ESP-CUP(28)     TO ESPECIALIDA28-CUP-J  
               MOVE ESP-CUP(29)     TO ESPECIALIDA29-CUP-J  
               MOVE OPER-ESP-CUP    TO OPER-ESP-CUP-J
               MOVE FECHA-ESP-CUP   TO FECHA-ESP-CUP-J
               MOVE SEXO-ESP-CUP    TO SEXO-ESP-CUPS-J 
               MOVE ATIENDE-ESP-CUP TO ATIENDE-ESP-CUP-J
               MOVE PYP-CUP         TO PYP-CUPS-J
               MOVE TIPO-PROC-CUP   TO TIPO-CUPS-J
               MOVE FINALID-CUP     TO FINALID-CUPS-J  


               INSPECT DESCRIP-CUPS-J REPLACING ALL '"' BY " "
               INSPECT DESCRIP-CUPS-J REPLACING ALL "*" BY " "
               
               INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
               MOVE "," TO CIERRE-LIN-1          
           END-IF


           GO TO LEER-NOMBRE-ESPCUPS.

       CERRAR-NOMBRE-ESPCUPS.
           INITIALIZE LIN-1

           MOVE LIN-1 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE
      
           MOVE "]}" TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE
                
           CLOSE ARCHIVO-CUPS
                 ARCHIVO-ESPEC-CUP
                 ARCHIVO-JSON.      

       ENVIO-DATOS.
           MOVE "datosrecibidos"  TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO       TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"   USING COBW3

           MOVE "..\..\FRAMEWORKS\PAGINAS\RECIBIDOS.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.

       
       COPY "..\..\FUENTES\SC-WEB19.CBL".
        
