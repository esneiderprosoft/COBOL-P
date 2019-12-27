      *=================================================================
      * VENTANA DE CONSULTA DE PAIS RIPS 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

           SELECT ARCHIVO-PAISES-RIPS LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-PAIRIP-W
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC;
                   RECORD KEY IS COD-PAIR
                   FILE STATUS IS OTR-STAT.


       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-PAIRIP.CBL".
      
       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LLEGADA-W                    PIC X(100).                                      
       77 NOM-PAIRIP-W                 PIC X(70).
       77 DATOS-PLANO-W                PIC X(200).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(30).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 COD-PAIR-LLEGADA-W        PIC X(3). 
          02 OPERADOR-LLEGADA-W        PIC X(4).

      * 01 DATOS-ENVIO.
      *    02 ESTADO-ENV                PIC XX.
      *    02 FILLER                    PIC X VALUE "|".
      *    02 LLAVE-ZONA-ENV.                    
      *	      05 TIPO-ZONA-ENV         PIC 9.
      *	      05 COD-ZONA-ENV          PIC X(2). 
      *    02 FILLER                    PIC X VALUE "|".
      *    02 DESCRIP-ZONA-ENV          PIC X(20).
           
       01 LIN-1.  
          02 FILLER                  PIC X(10)   VALUE "{*CODIGO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-PAIR-J              PIC X(3).          
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)   VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-PAIR-J.
             03 DESCRIP1-PAIR-J      PIC X(5).
             03 DESCRIP2-PAIR-J      PIC X(25).
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
              MOVE "CON810"                 TO MSJ3-HTML
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
              MOVE "CON810"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PAISES-RIPS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PAIRIP-W             TO MSJ2-HTML
              MOVE "CON810"                 TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, COD-PAIR-LLEGADA-W,
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

           INITIALIZE NOM-PAIRIP-W
           MOVE "\\" TO NOM-PAIRIP-W

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-PAIRIP-W REPLACING FIRST "                    "
                                         BY IP-DATOS-USUNET

           INSPECT NOM-PAIRIP-W REPLACING FIRST " "
                                            BY "\"
           INSPECT NOM-PAIRIP-W REPLACING FIRST "                         "           
                                            BY  "\PROG\DATOS\SC-PAIRIP.DAT".  
                                                           
       ABRIR-ZONAS.

           OPEN INPUT ARCHIVO-PAISES-RIPS.

           INITIALIZE REG-PAIR.
           MOVE "{*PAISESRIPS*:[" TO DATOS-PLANO-W   
           INSPECT DATOS-PLANO-W  REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO.
           MOVE 0 TO SW-FIN.

       LEER-PAISES-RIPS.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-PLANO-W  
              PERFORM DATOS-ENVIO
           END-IF

           READ ARCHIVO-PAISES-RIPS NEXT WITH NO LOCK AT END 
                GO TO CERRAR-PAISES-RIPS
           END-READ.
           
           MOVE COD-PAIR       TO COD-PAIR-J
           MOVE DESCRIP-PAIR   TO DESCRIP-PAIR-J

           MOVE ","               TO CIERRE-LIN-1

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES
           
           GO TO LEER-PAISES-RIPS.
                                    
        CERRAR-PAISES-RIPS.

           INITIALIZE LIN-1
008890     MOVE LIN-1 TO DATOS-PLANO-W
008900     PERFORM DATOS-ENVIO
008910
008920     MOVE "]}" TO DATOS-PLANO-W
008930     PERFORM DATOS-ENVIO.
           
           CLOSE ARCHIVO-PAISES-RIPS.
                    
       PAGINA-CONFIG.  
010720     MOVE "statuscode" TO COBW3-CNV-NAME
010730     MOVE "00"         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010720     MOVE "programa-id" TO COBW3-CNV-NAME
010730     MOVE "SER888"      TO COBW3-CNV-VALUE
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