      *============================================================
      * VENTANA DE SALDOS DE UN ALMACEN POR LOTEC - INV808
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

           SELECT MAESTRO-ARTICULOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-ART-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY COD-ART
                  ALTERNATE RECORD KEY CLASE-ART       WITH DUPLICATES
                  ALTERNATE RECORD KEY COD-BARRAS-ART  WITH DUPLICATES
                  ALTERNATE RECORD KEY DESCRIP-ART     WITH DUPLICATES
                  ALTERNATE RECORD KEY USO-ART         WITH DUPLICATES
                  ALTERNATE RECORD KEY CL-ROTAC-ART    WITH DUPLICATES
                  ALTERNATE RECORD KEY UBICAC-ART      WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-VENCE-ART WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.
     
           SELECT ARCHIVO-SALDOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-SALDOS-W
      * "SC-SALDO.DAT"
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-SAL
                  ALTERNATE RECORD KEY COD-ART-SAL  WITH DUPLICATES
                  ALTERNATE RECORD KEY COD-LOTE-SAL WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-JSON LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.
                                                          
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-ARTIC.CBL".
       COPY "..\..\FUENTES\FD-SALDO.CBL".
   .

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON      PIC X(500).
     
      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LLEGADA-W                    PIC X(100).

       77 NOM-USUAR-W                  PIC X(70).
       77 NOM-ART-W                    PIC X(70).
       77 NOM-SALDOS-W                 PIC X(70).
       77 NOM-PLANO-W                  PIC X(90).
       77 CONT                         PIC 9(4).
       77 VLR-UNIT                     PIC 9(9)V99.
       77  SW9                         PIC 9.
       01 LLAVE-ART-SDO-LNK.
          02 ALMAC-SDO-LNK                PIC X(5).
          02 COD-ART-SDO-LNK.
             03 TIPO-SDO-LNK              PIC 9.
             03 GRUPO-SDO-LNK             PIC XX.
             03 NUMERO-SDO-LNK            PIC X(13).
             03 CLASE-SDO-LNK             PIC XX.
       01 COD-LOTE-SDO-LNK                PIC 9(9).

       01 SDO-ACT-LNK                     PIC 9(12)V99.
       01 VLR-ACT-LNK                     PIC 9(12)V99.
       01 DESC-ART-LNK                    PIC X(30).
       01 SL-W                            PIC 99.

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(21).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 LLAVE-LNK.
             03 LOCAL-LNK            PIC X(5).
             03 CTA-LNK.
                05 TIPO-LNK          PIC 9.
                05 GRUPO-LNK         PIC XX.
                05 NUMERO-LNK        PIC X(13).
                05 CLASE-LNK         PIC XX.
             03 COD-LOTE-LNK         PIC X(4).
          02 SDO-ACT-W               PIC 9(11)V99.


        01 LIN-1.  
          02 FILLER                  PIC X(7)   VALUE "{*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SDO-ANT-L3-J            PIC 9(12)V99.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*NOMBRE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ACUM-ENT-L3-J           PIC 9(12)V99.   
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ACUM-SAL-L3-J           PIC 9(12)V99. 
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SDO-ACT-L3-J            PIC 9(12)V99.         
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VLR-UNIT-L3-J           PIC 9(9)V99.       
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-LOTE-L3-J           PIC 9(9).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-ALM-L3-J            PIC X(5).
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
              MOVE "INV808"                TO MSJ3-HTML
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
              MOVE "INV808"                TO MSJ3-HTML
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
              MOVE "INV808"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR    
           END-IF. 

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MAESTRO-ARTICULOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-ART-W                TO MSJ2-HTML
              MOVE "INV808"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR    
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SALDOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-SALDOS-W             TO MSJ2-HTML
              MOVE "INV808"                TO MSJ3-HTML
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
              MOVE "INV808"                TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, LLAVE-LNK, SDO-ACT-W
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

           INITIALIZE NOM-SALDOS-W NOM-ART-W NOM-USUAR-W
           MOVE "\\" TO NOM-SALDOS-W NOM-ART-W NOM-USUAR-W
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-SALDOS-W REPLACING FIRST "                    "
                                           BY IP-DATOS-USUNET

           INSPECT NOM-SALDOS-W REPLACING FIRST " "
                                           BY "\"

           INSPECT NOM-SALDOS-W REPLACING FIRST "                     "
                                           BY DIR-CONTAB-LLEGADA-W.

           MOVE NOM-SALDOS-W TO NOM-ART-W
                                NOM-USUAR-W
                   
           INSPECT NOM-SALDOS-W REPLACING FIRST "                     "           
                                           BY "\CONTROL\SC-SALDO.DAT".              

           INSPECT NOM-ART-W REPLACING FIRST "                    "           
                                           BY "\CONTROL\MAESART.DAT".   

           INSPECT NOM-USUAR-W REPLACING FIRST "                       "           
                                               BY "\CONTROL\SC-ARCHUSU.DAT".
                                                                                                                
           INITIALIZE NOM-PLANO-W
           MOVE "\\" TO NOM-PLANO-W

           INSPECT NOM-PLANO-W REPLACING FIRST "                    "
                                         BY IP-DATOS-USUNET

           INSPECT NOM-PLANO-W REPLACING FIRST  "                 "
                                            BY  "\PROG\DATOS\JSON\" 
      
           INSPECT NOM-PLANO-W  REPLACING FIRST "                                                "
                                             BY "JSC-ARCHSALDO-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"

           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                             BY LLAVE-SESION-LLEGA-W.  


       INICIAR.
           IF COD-ART-SDO-LNK = SPACES
              GO TO CERRAR-SESION 
           END-IF.

           INITIALIZE SW9.
     
           OPEN INPUT  ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END
           MOVE 0 TO OPCION-1
           END-READ
           CLOSE ARCHIVO-USUARIOS.

           IF LOTE-FARM-USU < 3
              MOVE ZEROS TO COD-LOTE-SDO-LNK
           END-IF.

       ABRIR-ARCHIVOS.
           OPEN INPUT  ARCHIVO-SALDOS
                       MAESTRO-ARTICULOS.

           MOVE COD-ART-SDO-LNK TO COD-ART.
           READ MAESTRO-ARTICULOS WITH NO LOCK
                INVALID KEY INITIALIZE DATOS-ART
           END-READ

           INITIALIZE REG-SALDOS SDO-ACT-LNK.

           MOVE COD-ART-SDO-LNK TO COD-ART-SAL.

           START ARCHIVO-SALDOS KEY = COD-ART-SAL
                INVALID KEY INITIALIZE DATOS-SAL
                GO TO CERRAR-SESION
           END-START.
 
       LEER-SALDOS.
           READ ARCHIVO-SALDOS NEXT WITH NO LOCK AT END
                GO TO CERRAR-SESION
           END-READ

           IF COD-ART-SAL IS NOT = COD-ART-SDO-LNK
              GO TO CERRAR-SESION    
           END-IF.

       MOSTRAR-SALDO. 
           IF LN < 1
               GO TO CERRAR-SESION
           END-IF.

           INITIALIZE VARIABLES-SALDOS-W SL-W
           PERFORM CALCULAR-SALDO VARYING SL-W FROM 1 BY 1 UNTIL SL-W > LN
     
           IF SDO-ACT-CANT-W IS NOT = 0
              COMPUTE VLR-UNIT ROUNDED = SDO-ACT-VLR-W / SDO-ACT-CANT-W END-COMPUTE
           END-IF.
     
           MOVE ACUM-ENT-CANT (32) TO SDO-ANT-L3-J
           MOVE SDO-AC-CANT-ENT-W  TO ACUM-ENT-L3-J
           MOVE SDO-AC-CANT-SAL-W  TO ACUM-SAL-L3-J
           MOVE SDO-ACT-CANT-W     TO SDO-ACT-L3-J
             
           IF VLR-UNIT = 0
              MOVE ZEROS TO VLR-UNIT-L3-J
           ELSE
              MOVE VLR-UNIT       TO VLR-UNIT-L3-J
           END-IF
     
           MOVE COD-LOTE-SAL       TO COD-LOTE-L3-J
           MOVE COD-ALM-SAL        TO COD-ALM-L3-J

           IF COD-ALM-SAL = ALMAC-SDO-LNK
      *       AND (COD-LOTE-SDO-LNK = ZEROS  OR COD-LOTE-SDO-LNK = COD-LOTE-SAL)
              ADD SDO-ACT-CANT-W  TO SDO-ACT-LNK
           END-IF.

           GO TO LEER-SALDOS.

       CERRAR-VENTANA.

           CLOSE ARCHIVO-SALDOS.
           

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
      
       CALCULAR-SALDO.
           MOVE SL-W TO DIA-SDO-W
     
           IF DIA-SDO-W = 1
              ADD  ACUM-ENT-CANT     (32)    TO SDO-ACT-CANT-W SDO-INI-CANT-W
              ADD  ACUM-ENT-VLR      (32)    TO SDO-ACT-VLR-W  SDO-INI-VLR-W
              ADD  DEP-ENT-VLR       (32)    TO SDO-ACT-DEP-W  SDO-INI-DEP-W
              ADD  INF-ENT-VLR       (32)    TO SDO-ACT-INF-W  SDO-INI-INF-W
              ADD  INFD-ENT-VLR      (32)    TO SDO-ACT-INFD-W SDO-INI-INFD-W
           END-IF

              ADD ACUM-ENT-CANT (DIA-SDO-W)  TO SDO-AC-CANT-ENT-W SDO-ACT-CANT-W
              ADD ACUM-ENT-VLR  (DIA-SDO-W)  TO SDO-AC-VLR-ENT-W  SDO-ACT-VLR-W
              ADD DEP-ENT-VLR   (DIA-SDO-W)  TO SDO-AC-DEP-ENT-W  SDO-ACT-DEP-W
              ADD INF-ENT-VLR   (DIA-SDO-W)  TO SDO-AC-INF-ENT-W  SDO-ACT-INF-W 
              ADD INFD-ENT-VLR  (DIA-SDO-W)  TO SDO-AC-INFD-ENT-W SDO-ACT-INFD-W
     
              ADD ACUM-SAL-CANT (DIA-SDO-W)  TO SDO-AC-CANT-SAL-W 
              ADD ACUM-SAL-VLR  (DIA-SDO-W)  TO SDO-AC-VLR-SAL-W  
              ADD DEP-SAL-VLR   (DIA-SDO-W)  TO SDO-AC-DEP-SAL-W  
              ADD INF-SAL-VLR   (DIA-SDO-W)  TO SDO-AC-INF-SAL-W  
              ADD INFD-SAL-VLR  (DIA-SDO-W)  TO SDO-AC-INFD-SAL-W.

              SUBTRACT ACUM-SAL-CANT (DIA-SDO-W) FROM SDO-ACT-CANT-W
              SUBTRACT ACUM-SAL-VLR  (DIA-SDO-W) FROM SDO-ACT-VLR-W
              SUBTRACT DEP-SAL-VLR   (DIA-SDO-W) FROM SDO-ACT-DEP-W
              SUBTRACT INF-SAL-VLR   (DIA-SDO-W) FROM SDO-ACT-INF-W 
              SUBTRACT INFD-SAL-VLR  (DIA-SDO-W) FROM SDO-ACT-INFD-W.



       COPY "..\..\FUENTES\SC-WEB19.CBL".

       
      
