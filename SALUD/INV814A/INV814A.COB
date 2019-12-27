      *============================================================
      * CREACION DE ARCHIVO JSON CONSULTA DE LOTES DE FARMACIA 
      * DE UN ARTICULO - INV814A 
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

           SELECT ARCHIVO-LOTE-FARM LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-LOTEFARM-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY COD-LTF
                  ALTERNATE RECORD KEY ART-LTF WITH DUPLICATES
                  ALTERNATE RECORD KEY DESCRIP-LTF WITH DUPLICATES
                  ALTERNATE RECORD KEY LOTE-LTF WITH DUPLICATES
                  ALTERNATE RECORD KEY VENCE-LTF WITH DUPLICATES
                  ALTERNATE RECORD KEY CUM-LTF WITH DUPLICATES
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

           SELECT REGCONT LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-REGCONT-W
      *"SC-REGCONT.DAT"
                  FILE STATUS OTR-STAT.           
       

           SELECT ARCHIVO-JSON LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.
                                                          
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-LTFAR.CBL".
       COPY "..\..\FUENTES\FD-ARTIC.CBL".
       COPY "..\..\FUENTES\FD-SALDO.CBL".
       COPY "..\..\FUENTES\FD-FECHA.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON      PIC X(500).
     
      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LLEGADA-W                    PIC X(100).

       77 NOM-LOTEFARM-W               PIC X(70).
       77 NOM-ART-W                    PIC X(70).
       77 NOM-SALDOS-W                 PIC X(70).
       77 NOM-REGCONT-W                PIC X(70).
       77 NOM-PLANO-W                  PIC X(90).
       77 CONT                         PIC 9(4).
       77 SW0                          PIC 9.

       01  FECHA-LIMI-W.
           02 ANO-LIMI-W               PIC 9(4).
           02 MES-LIMI-W               PIC 99.

       01  FECHA-OPER1-W.
           02 ANO-OPER1-W              PIC 9(4).
           02 MES-OPER1-W              PIC 9(2).


       01  FECHA-OPER2-W.
           02 ANO-OPER2-W              PIC 9(4).
           02 MES-OPER2-W              PIC 9(2).

       01  FECHA-TOTAL-W.
           02 ANO-TOTAL-W              PIC 9(4).
           02 MES-TOTAL-W              PIC 9(2).


      
       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(21).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 LLAVE-W.
             03 ALM-W                  PIC X(5).
             03 ART-W                  PIC X(18).
             03 CTA-W                  PIC X(4).
          02 SW-MARCA                  PIC 9.
          02 ADMIN-LLEGA-W             PIC X(4).

       01 LIN-1.  
          02 FILLER                  PIC X(7)   VALUE "{*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-L3-J                PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*NOMBRE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LAB-L3-J                PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LOTE-L3-J               PIC X(12).   
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 VENCE-L3-J.
             07 MES-L3-J             PIC XX.
             07 GUION-L3-J           PIC X.
             07 ANO-L3-J             PIC X(4).          
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CUM-L3-J                PIC X(15).         
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ALM-L3-J                PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*DESCRIP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SALDO-L3-J              PIC 9(9)V99.
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
              MOVE "INV814A"                TO MSJ3-HTML
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
              MOVE "INV814A"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-LOTE-FARM.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-LOTEFARM-W           TO MSJ2-HTML
              MOVE "INV814A"                TO MSJ3-HTML
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
              MOVE "INV814A"                TO MSJ3-HTML
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
              MOVE "INV814A"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR    
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON REGCONT.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-REGCONT-W            TO MSJ2-HTML
              MOVE "INV814A"                TO MSJ3-HTML
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
              MOVE "INV814A"                TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, LLAVE-W, SW-MARCA, ADMIN-LLEGA-W 
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

           INITIALIZE NOM-SALDOS-W NOM-ART-W NOM-LOTEFARM-W NOM-REGCONT-W
           MOVE "\\" TO NOM-SALDOS-W NOM-ART-W NOM-LOTEFARM-W NOM-REGCONT-W
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-SALDOS-W REPLACING FIRST "                    "
                                           BY IP-DATOS-USUNET

           INSPECT NOM-SALDOS-W REPLACING FIRST " "
                                           BY "\"

           INSPECT NOM-SALDOS-W REPLACING FIRST "                     "
                                           BY DIR-CONTAB-LLEGADA-W.

           MOVE NOM-SALDOS-W TO NOM-ART-W
                                NOM-LOTEFARM-W
                                NOM-REGCONT-W
                   
           INSPECT NOM-SALDOS-W REPLACING FIRST "                     "           
                                           BY "\CONTROL\SC-SALDO.DAT".              

           INSPECT NOM-ART-W  REPLACING FIRST "                    "           
                                           BY "\CONTROL\MAESART.DAT".   

           INSPECT NOM-LOTEFARM-W REPLACING FIRST "                     "           
                                               BY "\CONTROL\SC-LOTEF.DAT".
     
           INSPECT NOM-REGCONT-W REPLACING FIRST "                       "           
                                               BY "\CONTROL\SC-REGCONT.DAT".
                         
                                                                                                           
           INITIALIZE NOM-PLANO-W
           MOVE "\\" TO NOM-PLANO-W

           INSPECT NOM-PLANO-W REPLACING FIRST "                    "
                                         BY IP-DATOS-USUNET

           INSPECT NOM-PLANO-W REPLACING FIRST  "                 "
                                            BY  "\PROG\DATOS\JSON\" 
      
           INSPECT NOM-PLANO-W  REPLACING FIRST "                                                "
                                             BY "JSC-ARCHLOTEF-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"

           INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                             BY LLAVE-SESION-LLEGA-W.  


       ABRIR-ARCHIVOS.
           OPEN INPUT REGCONT.
           READ REGCONT NEXT WITH NO LOCK AT END 
                GO TO CERRAR-NOMBRE
           END-READ
           CLOSE REGCONT.

           MOVE ANO-ALFA TO ANO-LIMI-W.
           MOVE MES-NUM  TO MES-LIMI-W.


           OPEN INPUT ARCHIVO-LOTE-FARM
                      MAESTRO-ARTICULOS
                      ARCHIVO-SALDOS.

           MOVE ART-W TO COD-ART.
           READ MAESTRO-ARTICULOS INVALID KEY
                MOVE ALL "*" TO DESCRIP-ART.
          
         
       LEER-MAESTRO.

           MOVE ART-W TO ART-LTF.
           START ARCHIVO-LOTE-FARM  KEY = ART-LTF
                 INVALID KEY MOVE 8 TO SW-INVALID
                  MOVE SPACES TO CTA-W
                                GO TO CERRAR-SESION
           END-START.

     
       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-LTF.

           MOVE "{*LOTESFARM*:[" TO  DATOS-JSON   
           INSPECT DATOS-JSON  REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON  END-WRITE
           MOVE 0 TO SW-FIN.

       LEER-NOMBRE.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-JSON  
              WRITE REG-JSON  END-WRITE
           END-IF

           MOVE LLAVE-W TO LLAVE-SAL.
           MOVE COD-LTF TO COD-LOTE-SAL.
           READ ARCHIVO-SALDOS INVALID KEY
                GO TO CERRAR-NOMBRE
           END-READ.

      *     READ ARCHIVO-SALDOS NEXT WITH NO LOCK AT END 
      *          GO TO CERRAR-NOMBRE
      *     END-READ.
           
           IF COD-LTF = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE 
              IF COD-LTF = COD-L3-J   
              MOVE 0 TO SW-FIN
           ELSE
             ADD 1 TO CONT 
             INITIALIZE LIN-1

             MOVE COD-LTF        TO COD-L3-J
             MOVE LAB-LTF        TO LAB-L3-J
             MOVE LOTE-LTF       TO LOTE-L3-J
             MOVE VENCE-LTF      TO VENCE-L3-J
             MOVE CUM-LTF        TO CUM-L3-J
             MOVE ALM-W          TO ALM-L3-J
              

             INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
             MOVE "," TO CIERRE-LIN-1 
             END-IF
           END-IF
           
           GO TO LEER-NOMBRE.
                                    
        CERRAR-NOMBRE.

           INITIALIZE LIN-1
           
           IF CONT = ZEROS 
              INITIALIZE COD-L3-J
                         LAB-L3-J
                         LOTE-L3-J
                         VENCE-L3-J
                         CUM-L3-J
                         ALM-L3-J


           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
		END-IF.
           MOVE LIN-1 TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
          
           MOVE "]}" TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE          

           CLOSE ARCHIVO-LOTE-FARM
                 MAESTRO-ARTICULOS
                 ARCHIVO-SALDOS
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