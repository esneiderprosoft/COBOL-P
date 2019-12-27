      *============================================================
      * CREACION DE ARCHIVO JSON F8 CONSULTA DE TABLA CUPS - SER802D
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

           SELECT ARCHIVO-CUPS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-CUPS-W
      *"/PROG/DATOS/SC-ARCHCUPS.DAT"
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS LLAVE-CUP
                  ALTERNATE RECORD KEY IS DESCRIP-CUP WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LLAVE-ALT-CUP WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.
           

           SELECT ARCHIVO-JSON LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.
                                                          
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-CUPS.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON      PIC X(500).

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LLEGADA-W                    PIC X(100).

       77 NOM-CUPS-W                   PIC X(70).
       77 NOM-PLANO-W                  PIC X(90).
       77 CONT                         PIC 9(4).
      
       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(21).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 TIPO-W                    PIC 9.
          02 LLAVE-W.
             03 GRUPO-W                PIC X(2).
             03 ART-W                  PIC X(10). 


       01 LIN-1.  
          02 FILLER                  PIC X(9)   VALUE "*NOMBRE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LLAVE-CUP-J             PIC X(12).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-CUP-J           PIC X(80).   
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
              MOVE "SER802D"                TO MSJ3-HTML
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
              MOVE "SER802D"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CUPS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-CUPS-W               TO MSJ2-HTML
              MOVE "SER802D"                TO MSJ3-HTML
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
              MOVE "SER802D"                TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, TIPO-W, LLAVE-W 
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

           INITIALIZE NOM-CUPS-W
           MOVE "\\" TO NOM-CUPS-W 

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-CUPS-W REPLACING FIRST "                    "
                                         BY IP-DATOS-USUNET

           INSPECT NOM-CUPS-W REPLACING FIRST " "
                                            BY "\"
           
           INSPECT NOM-CUPS-W REPLACING FIRST "                          "           
                                           BY "PROG/DATOS/SC-ARCHCUPS.DAT".  
                                                            
           INITIALIZE NOM-PLANO-W
           MOVE "\\" TO NOM-PLANO-W

           INSPECT NOM-PLANO-W REPLACING FIRST "                    "
                                         BY IP-DATOS-USUNET

           INSPECT NOM-PLANO-W REPLACING FIRST  "                 "
                                            BY  "\PROG\DATOS\JSON\" 
      
           INSPECT NOM-PLANO-W  REPLACING FIRST "                                               "
                                             BY "JSC-ARCHCUPS-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"

           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                             BY LLAVE-SESION-LLEGA-W.  


       ABRIR-ARCHIVO-CUPS.

           OPEN INPUT ARCHIVO-CUPS.
           
       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-CUPS.

           MOVE "{*CUPS*:[" TO  DATOS-JSON   
           INSPECT DATOS-JSON  REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON  END-WRITE
           MOVE 0 TO SW-FIN.

       LEER-ARCHIVO-CUPS.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-JSON  
              WRITE REG-JSON  END-WRITE
           END-IF

           READ ARCHIVO-CUPS NEXT WITH NO LOCK AT END 
                GO TO CERRAR-ARCHIVO-CUPS
           END-READ.
           
           IF LLAVE-CUP = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE 
              IF LLAVE-CUP = LLAVE-CUP-J
                 MOVE 0 TO SW-FIN
              ELSE
                 ADD 1 TO CONT 
                 INITIALIZE LIN-1
                 MOVE LLAVE-CUP  TO LLAVE-CUP-J
             
                 INSPECT DESCRIP-CUP REPLACING ALL '"' BY " "
                 INSPECT DESCRIP-CUP REPLACING ALL "*" BY " "
                 INSPECT DESCRIP-CUP REPLACING ALL "Ñ" BY CARAC-ENE-MAY 
                 INSPECT DESCRIP-CUP REPLACING ALL "ñ" BY CARAC-ENE-MIN

                 MOVE DESCRIP-CUP   TO DESCRIP-CUP-J

                 INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
                 MOVE "," TO CIERRE-LIN-1 
              END-IF
           END-IF
           
           GO TO LEER-ARCHIVO-CUPS.
                                    
        CERRAR-ARCHIVO-CUPS.

           INITIALIZE LIN-1
           
           IF CONT = ZEROS 
              INITIALIZE LLAVE-CUP-J
                         DESCRIP-CUP-J

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
		END-IF.
           MOVE LIN-1 TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
          
           MOVE "]}" TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
           
           CLOSE ARCHIVO-CUPS
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