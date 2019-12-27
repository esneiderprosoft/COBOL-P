      *============================================================
      * CREACION DE ARCHIVO JSON HC837 F8 DE MACROS PARA FORMULACION
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  
     
           SELECT ARCHIVO-MACRO-HIS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-MACRO-W 
      *"\PROG\HC\DATOS\SC-MACDR.DAT"
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-MACROH
                  ALTERNATE RECORD KEY IS NOMBRE-MACROH WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.           

           SELECT ARCHIVO-JSON LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.
                                                          
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-MACR1.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON      PIC X(500).

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LLEGADA-W                    PIC X(100).

       77 NOM-MACRO-W                  PIC X(70).
       77 NOM-PLANO-W                  PIC X(90).
       77 CONT                         PIC 9(4).
      
       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(21).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 COD-COSTO-LLEGADA-W       PIC X(4). 
          02 LLAVE-MACROH-W.
             03 TIPO-MACROH            PIC 9.
             03 COD-MACROH             PIC 99.

       01 LIN-1.  
          02 FILLER                  PIC X(7)   VALUE "{*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LLAVE-MACROH-J          PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*NOMBRE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOMBRE-MACROH-J         PIC X(40).
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
              MOVE "HC837"                  TO MSJ3-HTML
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
              MOVE "HC837"                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-MACRO-HIS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-MACRO-W              TO MSJ2-HTML
              MOVE "HC837"                  TO MSJ3-HTML
              GO TO ENVIAR2-ERROR    
           END-IF.  

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PLANO-W              TO MSJ2-HTML
              MOVE "HC837"                  TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, COD-COSTO-LLEGADA-W   
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.

       INICIAR-SESION.
           MOVE "D:\PROGELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W 
         
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.
              
       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\PROGELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
     
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

           INITIALIZE NOM-MACRO-W
           MOVE "\\" TO NOM-MACRO-W 

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-MACRO-W REPLACING FIRST "                    "
                                         BY IP-DATOS-USUNET

           INSPECT NOM-MACRO-W REPLACING FIRST " "
                                            BY "\"
           
           INSPECT NOM-MACRO-W   REPLACING FIRST "                          "           
                                           BY    "PROG\HC\DATOS\SC-MACDR.DAT".  
                                                            
           INITIALIZE NOM-PLANO-W
           MOVE "\\" TO NOM-PLANO-W

           INSPECT NOM-PLANO-W REPLACING FIRST "                    "
                                         BY IP-DATOS-USUNET

           INSPECT NOM-PLANO-W REPLACING FIRST  "                 "
                                            BY  "\PROG\DATOS\JSON\" 
      
           INSPECT NOM-PLANO-W  REPLACING FIRST "                                                "
                                             BY "JSC-ARCHCOSTO-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"

           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                             BY LLAVE-SESION-LLEGA-W.  


       ABRIR-MACRO-HIS.

           OPEN INPUT ARCHIVO-MACRO-HIS.
           
       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-MACROH.

           MOVE "{*MACRO*:[" TO  DATOS-JSON   
           INSPECT DATOS-JSON  REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON  END-WRITE
           MOVE 0 TO SW-FIN.

       LEER-MACRO-HIS.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-JSON  
              WRITE REG-JSON  END-WRITE
           END-IF

           READ ARCHIVO-MACRO-HIS NEXT WITH NO LOCK AT END 
                GO TO CERRAR-MACRO-HIS
           END-READ.
           
           IF LLAVE-MACROH = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE 
              IF LLAVE-MACROH = LLAVE-MACROH-J
                 MOVE 0 TO SW-FIN
           ELSE
             ADD 1 TO CONT 
             INITIALIZE LIN-1
             MOVE LLAVE-MACROH  TO LLAVE-MACROH-J
             
             INSPECT NOMBRE-MACROH REPLACING ALL '"' BY " "
             INSPECT NOMBRE-MACROH REPLACING ALL "*" BY " "
             INSPECT NOMBRE-MACROH REPLACING ALL "Ñ" BY CARAC-ENE-MAY 
             INSPECT NOMBRE-MACROH REPLACING ALL "ñ" BY CARAC-ENE-MIN

             MOVE NOMBRE-MACROH    TO NOMBRE-MACROH-J
           
             INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
             MOVE "," TO CIERRE-LIN-1 
             END-IF
           END-IF
           
           GO TO LEER-MACRO-HIS.
                                    
        CERRAR-MACRO-HIS.

           INITIALIZE LIN-1
           
           IF CONT = ZEROS 
              INITIALIZE LLAVE-MACROH-J
                         NOMBRE-MACROH-J

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
		END-IF.
           MOVE LIN-1 TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
          
           MOVE "]}" TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
           
           CLOSE ARCHIVO-MACRO-HIS
                 ARCHIVO-JSON.      

       ENVIO-DATOS.
           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE "00"             TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3
         
           MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
           PERFORM ABRIR-HTML.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.            
      
       COPY "..\..\FUENTES\SC-WEB19.CBL".
      
