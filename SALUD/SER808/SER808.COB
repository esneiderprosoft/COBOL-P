      *=================================================================
      * CREACION DE ARCHIVO JS SER808 SALUD
      * VENTANA DE CONSULTA DE FACTURAS ACTIVAS 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
       COPY "..\..\FUENTES\FS-NUMER19.CBL".
       	
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-NUMER19.CBL".
           
       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".

       01 LLEGADA-W                   PIC X(100).
       01 DATOS-PLANO-W               PIC X(1000).
       01 PREFIJO-LLEGADA-W           PIC X.

       01 LIN-1.  
          02 FILLER                   PIC X(12)  VALUE "{*COD*:".
          02 FILLER                   PIC X      VALUE "*".
          02 LLAVE-NUM-J.
             03 PREF-NUM-J            PIC X.
             03 NRO-NUM-J             PIC 9(6).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)  VALUE "*HABITAC*:".
          02 FILLER                   PIC X      VALUE "*".
          02 HAB-NUM-J                PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*FECHA_ING*:".
          02 FILLER                   PIC X      VALUE "*".
          02 FECHA-ING-NUM-J          PIC X(8).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(14)  VALUE "*DESCRIP*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DESCRIP-NUM-J            PIC X(40).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)  VALUE "*NOM_PAC*:".
          02 FILLER                   PIC X      VALUE "*".
          02 NOMBRE-PAC-NUM-J         PIC X(40).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*CONVENIO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CONVENIO-NUM-J           PIC XX.
          02 FILLER                   PIC XX     VALUE "*}".    
          02 CIERRE-LIN-1             PIC X.

        
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
              MOVE "SER808"                 TO MSJ3-HTML
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
              MOVE "SER808"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-NUMERACION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-NUMER-LNK            TO MSJ2-HTML
              MOVE "SER808"                 TO MSJ3-HTML
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
           INITIALIZE PREFIJO-LLEGADA-W.

           UNSTRING LLEGADA-W DELIMITED BY "|"
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W,
                   PREFIJO-LLEGADA-W
           END-UNSTRING.

           MOVE SESION-LLEGAD-W    TO LLAVE-SESION-W.
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
           INITIALIZE NOM-NUMER-LNK

           MOVE "\\" TO NOM-NUMER-LNK

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-NUMER-LNK REPLACING FIRST "                    "
                                              BY IP-DATOS-USUNET

           INSPECT NOM-NUMER-LNK REPLACING FIRST " "
                                              BY "\"

           INSPECT NOM-NUMER-LNK REPLACING FIRST "                              "
                                              BY DIRECTORIO-LLEGAD-W.
   
           INSPECT NOM-NUMER-LNK REPLACING FIRST "                     "
                                              BY "\CONTROL\SC-NUMER.DAT".

       ABRIR-ARCHIVO.
           OPEN INPUT ARCHIVO-NUMERACION.
           MOVE "{*NUMERACION*:[" TO  DATOS-PLANO-W
           INSPECT DATOS-PLANO-W  REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO.
   
       CONSULTAR-ARCHIVO.
           READ ARCHIVO-NUMERACION NEXT WITH NO LOCK AT END GO TO CERRAR-N.

           IF PREFIJO-LLEGADA-W = SPACES
              CONTINUE
           ELSE
              IF PREFIJO-NUM NOT = PREFIJO-LLEGADA-W
                 GO TO CONSULTAR-ARCHIVO
              END-IF
           END-IF



           MOVE LLAVE-NUM TO  LLAVE-NUM-J
                
           INSPECT DESCRIP-NUM REPLACING ALL '"' BY " "
           INSPECT DESCRIP-NUM REPLACING ALL "*" BY " "
           INSPECT DESCRIP-NUM REPLACING ALL "Ñ" BY CARAC-ENE-MAY 
           INSPECT DESCRIP-NUM REPLACING ALL "ñ" BY CARAC-ENE-MIN
  
           INSPECT NOMBRE-PAC-NUM REPLACING ALL '"' BY " "
           INSPECT NOMBRE-PAC-NUM REPLACING ALL "*" BY " "
           INSPECT NOMBRE-PAC-NUM REPLACING ALL "Ñ" BY CARAC-ENE-MAY
           INSPECT NOMBRE-PAC-NUM REPLACING ALL "ñ" BY CARAC-ENE-MIN
              
           MOVE HAB-NUM            TO HAB-NUM-J
           MOVE FECHA-ING-NUM      TO FECHA-ING-NUM-J
           MOVE DESCRIP-NUM        TO DESCRIP-NUM-J
           MOVE NOMBRE-PAC-NUM     TO NOMBRE-PAC-NUM-J
           MOVE CONVENIO-NUM       TO CONVENIO-NUM-J

           MOVE ","               TO CIERRE-LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           GO TO CONSULTAR-ARCHIVO.
   
       CERRAR-N.
           INITIALIZE LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
008890     MOVE LIN-1 TO DATOS-PLANO-W
008900     PERFORM DATOS-ENVIO
008910
008920     MOVE "]}" TO DATOS-PLANO-W
008930     PERFORM DATOS-ENVIO
           
           CLOSE ARCHIVO-NUMERACION.
                
       PAGINA-CONFIG.  
010720     MOVE "statuscode" TO COBW3-CNV-NAME
010730     MOVE "00"         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010720     MOVE "programa-id" TO COBW3-CNV-NAME
010730     MOVE "SER808"      TO COBW3-CNV-VALUE
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