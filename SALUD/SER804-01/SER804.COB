      *============================================================
      * CREACION DE ARCHIVO JSON SER804 F8 DE TARIFAS
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

     
           SELECT ARCHIVO-TARIFAS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TARF-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS COD-TAR
                  FILE STATUS IS OTR-STAT.           
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-TARIF.CBL".
      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".

       01 LLEGADA-W                    PIC X(100).

       77 DATOS-PLANO-W                PIC X(90).
       77 NOM-TARF-W                   PIC X(70).
      
       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(30).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 COD-TAR-W                 PIC X(2).
          02 OPERADOR-LLEGADA-W           PIC X(4). 

       01 LIN-1.  
          02 FILLER                  PIC X(7)   VALUE "{*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-TAR-J               PIC X(2).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-TAR-J           PIC X(25).
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
              MOVE "SER804-01"              TO MSJ3-HTML
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
              MOVE "SER804-01"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TARIFAS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                   TO MSJ1-HTML
              MOVE NOM-TARF-W                 TO MSJ2-HTML
              MOVE "SER804-01"                TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, COD-TAR-W, 
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

           INITIALIZE NOM-TARF-W  
                   
           MOVE "\\" TO NOM-TARF-W   

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-TARF-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-TARF-W REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-TARF-W REPLACING FIRST "                              "
                                            BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT NOM-TARF-W REPLACING FIRST "                       "           
                                           BY "\CONTROL\SC-ARCHTAR.DAT". 
 
       ABRIR-ARCHIVO-TARIFAS.
           OPEN INPUT ARCHIVO-TARIFAS.
  
           INITIALIZE REG-TAR
           MOVE "{*TARIFAS*:["   TO DATOS-PLANO-W   
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO. 
           MOVE 0 TO SW-FIN.
 
       LEER-NOMBRE-TARIFAS.

           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-PLANO-W 
              PERFORM DATOS-ENVIO
           END-IF

           READ ARCHIVO-TARIFAS NEXT WITH NO LOCK AT END 
                GO TO CERRAR-NOMBRE-TARIFAS
           END-READ.
           
           MOVE COD-TAR      TO COD-TAR-J 
           MOVE DESCRIP-TAR  TO DESCRIP-TAR-J

           MOVE ","               TO CIERRE-LIN-1

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES
           GO TO LEER-NOMBRE-TARIFAS.
                                    
        CERRAR-NOMBRE-TARIFAS.

           INITIALIZE LIN-1
          
           MOVE LIN-1 TO DATOS-PLANO-W  
           PERFORM DATOS-ENVIO
          
           MOVE "]}" TO DATOS-PLANO-W  
           PERFORM DATOS-ENVIO

           CLOSE ARCHIVO-TARIFAS.
                    
       PAGINA-CONFIG.  
           MOVE "statuscode" TO COBW3-CNV-NAME
           MOVE "00"         TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3

           MOVE "programa-id" TO COBW3-CNV-NAME
           MOVE "SER804"      TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3

           MOVE "..\PAGINAS\RECIBIDOSLISTADO.ASPX" TO SALIDA-HTML
           PERFORM ABRIR-HTML.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.            
       DATOS-ENVIO.
           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE DATOS-PLANO-W    TO COBW3-CNV-VALUE
           CALL "COBW3_SET_REPEAT" USING COBW3.

       COPY "..\..\FUENTES\SC-WEB19.CBL".
      
