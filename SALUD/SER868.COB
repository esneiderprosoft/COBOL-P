      *============================================================
      * CREACION DE ARCHIVO JSON CODIGOS DE CLASFICACION F8-SER868
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  
    
           SELECT ARCHIVO-CLASIPACI LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-CLASIPACI-W
      *"../CONTROL/SC-CLASPACI.DAT"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC;
                   RECORD KEY IS COD-CLASP
                   FILE STATUS IS OTR-STAT.
           
    
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-CLASPACI.CBL".
      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LLEGADA-W                    PIC X(100).

       77 NOM-CLASIPACI-W              PIC X(70).
       77 DATOS-PLANO-W                PIC X(90).
       77 MAX-W                        PIC 99.
    
       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(21).
          02 MES-CONTAB-LLEGADA-W      PIC XX.


       01 DATOS-ENVIO.
          02 ESTADO-ENVIO              PIC XX.           
          02 FILLER                    PIC X VALUE "|".
          02 LN-ENVIO                  PIC XX.
                                       
       01 LIN-1.  
          02 FILLER                  PIC X(7)   VALUE "{*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-CLASP-J             PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*NOMBRE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-CLASP-J         PIC X(30).
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
              MOVE "SER868"                 TO MSJ3-HTML
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
              MOVE "SER868"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CLASIPACI.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-CLASIPACI-W          TO MSJ2-HTML
              MOVE "SER868"                 TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W                
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
              MOVE "Validacion de usuarios"      TO MSJ1-HTML
              MOVE "Falta configurar usuario"    TO MSJ2-HTML
              MOVE "Sc"                          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.

           INITIALIZE NOM-CLASIPACI-W
           MOVE "\\" TO NOM-CLASIPACI-W

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-CLASIPACI-W REPLACING FIRST "                    "
                                         BY IP-DATOS-USUNET

           INSPECT NOM-CLASIPACI-W REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-CLASIPACI-W REPLACING FIRST "                     "
                                                BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT NOM-CLASIPACI-W REPLACING FIRST "                        "           
                                                BY "\CONTROL\SC-CLASPACI.DAT".  
                                                            
       ABRIR-ARCHIVO-CLASIPACI.

           OPEN INPUT ARCHIVO-CLASIPACI.

           INITIALIZE REG-CLASP.

           MOVE "{*CLASIFICACION*:[" TO DATOS-PLANO-W  
           INSPECT DATOS-PLANO-W  REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO.
           MOVE 0 TO SW-FIN.


      * VENTANA.

      *     EVALUATE NIT-USU

      * ESE SALUD YOPAL
      *           WHEN 844003225 MOVE 09 TO MAX-W

      * CLINICA CASANARE
      *           WHEN 891855847 MOVE 15 TO MAX-W

      * SERVIMEDICOS
      *           WHEN 800162035 MOVE 24 TO MAX-W
      *           WHEN OTHER     MOVE 09 TO MAX-W
      *     END-EVALUATE.

      *     COMPUTE LN = MAX-W + 2.
           
          
       LEER-ARCHIVO-CLASIPACI.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-PLANO-W  
              PERFORM DATOS-ENVIO
           END-IF

           READ ARCHIVO-CLASIPACI NEXT WITH NO LOCK AT END 
                GO TO CERRAR-ARCHIVO-CLASIPACI
           END-READ.

           MOVE COD-CLASP  TO COD-CLASP-J
             
           INSPECT DESCRIP-CLASP REPLACING ALL '"' BY " "
           INSPECT DESCRIP-CLASP REPLACING ALL "*" BY " "
           INSPECT DESCRIP-CLASP REPLACING ALL "Ñ" BY CARAC-ENE-MAY 
           INSPECT DESCRIP-CLASP REPLACING ALL "ñ" BY CARAC-ENE-MIN

           MOVE DESCRIP-CLASP   TO DESCRIP-CLASP-J

           MOVE ","               TO CIERRE-LIN-1

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES
           
           GO TO LEER-ARCHIVO-CLASIPACI.
                                    
       CERRAR-ARCHIVO-CLASIPACI.

           INITIALIZE LIN-1
           
           MOVE LIN-1 TO DATOS-PLANO-W
008900     PERFORM DATOS-ENVIO
008910
008920     MOVE "]}" TO DATOS-PLANO-W
008930     PERFORM DATOS-ENVIO. 
           
           CLOSE ARCHIVO-CLASIPACI.
                  
      
       PAGINA-CONFIG.  
010720     MOVE "statuscode" TO COBW3-CNV-NAME
010730     MOVE "00"         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010720     MOVE "programa-id" TO COBW3-CNV-NAME
010730     MOVE "SER858"      TO COBW3-CNV-VALUE
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