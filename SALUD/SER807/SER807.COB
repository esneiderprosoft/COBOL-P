      *============================================================
      * CREACION DE ARCHIVO JSON DROGA SUMINISTRADA A UN PACIENTE 
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  


     
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
         
           SELECT TEMPORAL-DROGA
                  ASSIGN TO NOMBRE-TEM
	          ORGANIZATION IS INDEXED
		  ACCESS MODE DYNAMIC
		  RECORD KEY IS ART-TEM
                  ALTERNATE RECORD KEY IS COD-BARRAS-TEM
                                          WITH DUPLICATES.


           SELECT ARCHIVO-JSON LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.
                                                          
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".


       FD  TEMPORAL-DROGA
           LABEL RECORD STANDARD.
       01  REG-TEM.
           02 ART-TEM.
              07 GRUPO-TEM              PIC XX.
              07 COD-ART-TEM            PIC X(13).
              07 CLASE-TEM              PIC XX.
              07 COD-LOTE-TEM           PIC X(4).
           02 DESCRIP-TEM               PIC X(30).
           02 REF-TEM                   PIC X(10).
           02 CANT-TEM                  PIC 9(4)V99.
           02 VLR-TEM                   PIC 9(10).
           02 COD-BARRAS-TEM            PIC X(15).


       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON      PIC X(500).

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LLEGADA-W                    PIC X(100).

       77 NOM-USUAR-W                  PIC X(70).
       77 NOM-PLANO-W                  PIC X(90).
       77 CONT                         PIC 9(4).
      
       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(21).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 NOMBRE-TEM-LLEGA          PIC X(35).
          02 REG-LLEGA-W.
             03 ART-W                  PIC X(17).
             03 COD-LOTE-W             PIC X(4).
             03 DESCRIP-W              PIC X(30).
             03 REF-W                  PIC X(10).
             03 CANT-W                 PIC 9(4)V99.
             03 VLR-W                  PIC 9(10).
          02 ADMIN-LLEGA-W             PIC X(4).

       01 LIN-1.  
          02 FILLER                  PIC X(11)   VALUE "{*COD-ART*:".
          02 FILLER                  PIC X       VALUE "*".
          02 COD-ART-J               PIC X(17).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*COD-LOT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-LOTE-J              PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-J               PIC X(30). 
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*REF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 REF-J                   PIC X(10). 
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*CANT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CANT-J                  PIC 9(4)V99.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*VALOR*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VLR-J                   PIC 9(10).  
          02 FILLER                  PIC XX     VALUE "*}".    
          02 CIERRE-LIN-1            PIC X.

       01 DATOS-VENTANA.
          02 DATOS-1 OCCURS 15.
             03 REG-VEN.
                05 COD-ART-VEN  PIC X(17).
                05 COD-LOTE-VEN PIC X(4).
                05 DESCRIP-VEN  PIC X(30).
                05 REF-VEN      PIC X(10).
                05 CANT-VEN     PIC 9(4)V99.
                05 VLR-VEN      PIC 9(10).


      
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
              MOVE "SER807"                 TO MSJ3-HTML
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
              MOVE "SER807"                 TO MSJ3-HTML
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
              MOVE "SER807"                 TO MSJ3-HTML
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
              MOVE "SER807"                 TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, NOMBRE-TEM-LLEGA, 
                   REG-LLEGA-W, ADMIN-LLEGA-W   
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

           INITIALIZE NOM-USUAR-W
           MOVE "\\" TO NOM-USUAR-W 

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                         BY IP-DATOS-USUNET

           INSPECT NOM-USUAR-W REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-USUAR-W REPLACING FIRST "                     "
                                            BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT NOM-USUAR-W REPLACING FIRST "                       "           
                                            BY "\CONTROL\SC-ARCHUSU.DAT".  
                                                                      
           INITIALIZE NOM-PLANO-W
           MOVE "\\" TO NOM-PLANO-W

           INSPECT NOM-PLANO-W REPLACING FIRST "                    "
                                         BY IP-DATOS-USUNET

           INSPECT NOM-PLANO-W REPLACING FIRST  "                 "
                                            BY  "\PROG\DATOS\JSON\" 
      
           INSPECT NOM-PLANO-W  REPLACING FIRST "                                                "
                                             BY "JSC-TEMPDROGA-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"

           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                             BY LLAVE-SESION-LLEGA-W.  


       ABRIR-ARCHIVOS.
           OPEN INPUT  ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT AT END MOVE 0 TO OPCION-1.
           CLOSE ARCHIVO-USUARIOS.


           OPEN INPUT TEMPORAL-DROGA.
           
       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-TEM.

           MOVE "{*DROGA*:[" TO  DATOS-JSON   
           INSPECT DATOS-JSON  REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON  END-WRITE
           MOVE 0 TO SW-FIN.

       LEER-ARCHIVO-TEMOPORAL.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-JSON  
              WRITE REG-JSON  END-WRITE
           END-IF

           READ TEMPORAL-DROGA NEXT WITH NO LOCK AT END 
                GO TO CERRAR-ARCHIVO-TEMPORAL
           END-READ.
           
           MOVE REG-TEM      TO REG-VEN (I)

           IF COD-ART-VEN (I) = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE 
              IF COD-ART-VEN (I)= COD-ART-J
                 MOVE 0 TO SW-FIN
              ELSE
                 ADD 1 TO CONT 
                 INITIALIZE LIN-1

                 MOVE COD-ART-VEN (I)    TO COD-ART-J
                 MOVE COD-LOTE-VEN (I)   TO COD-LOTE-J
                 MOVE DESCRIP-VEN (I)    TO DESCRIP-J
                 MOVE REF-VEN     (I)    TO REF-J
                 MOVE CANT-VEN    (I)    TO CANT-J
                 MOVE VLR-VEN     (I)    TO VLR-J

             INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
             MOVE "," TO CIERRE-LIN-1 
             END-IF
           END-IF
           
           GO TO LEER-ARCHIVO-TEMOPORAL.
                                    
        CERRAR-ARCHIVO-TEMPORAL.

           INITIALIZE LIN-1
           
           IF CONT = ZEROS 
              INITIALIZE COD-ART-J
                         COD-LOTE-J
                         DESCRIP-J
                         REF-J
                         CANT-J
                         VLR-J

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
		END-IF.
           MOVE LIN-1 TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
          
           MOVE "]}" TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
     
           
           CLOSE TEMPORAL-DROGA
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