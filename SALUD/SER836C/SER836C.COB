      *=================================================================
      * BUSQUEDA DE FACTURAS ABIERTAS DE P DE UN PACIENTE - SER836C
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

           SELECT ARCHIVO-NUMERACION LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-NUMER-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-NUM
		  ALTERNATE RECORD KEY LLAVE-ELE-NUM   WITH DUPLICATES
		  ALTERNATE RECORD KEY SUCUR-NUM       WITH DUPLICATES
		  ALTERNATE RECORD KEY NIT-NUM         WITH DUPLICATES
                  ALTERNATE RECORD KEY DESCRIP-NUM     WITH DUPLICATES
                  ALTERNATE RECORD KEY NOMBRE-PAC-NUM  WITH DUPLICATES
                  ALTERNATE RECORD KEY HAB-NUM         WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-ING-NUM   WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-RET-NUM   WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-NUMER19.CBL".
      
       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LLEGADA-W                    PIC X(100). 
                                      
       77 NOM-NUMER-W                  PIC X(70).

       01 LLAVE-NUM-W                  PIC X(7).  
       01 FECHA-INI-W.
          02 ANO-INI-W               PIC 9(4).
          02 MES-INI-W               PIC 99.
          02 DIA-INI-W               PIC 99.        
  
       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(30).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 PACIENTE-LLEGADA-W        PIC X(15).
          02 FECHA-LLEGADA-W.
             05 ANO-LLEGADA-W          PIC 99.
             05 MES-LLEGADA-W          PIC 99.
             05 DIA-LLEGADA-W          PIC 99.
          02 LLAVE-ACT-LLEGADA-W       PIC X(7).
          02 OPERADOR-LLEGADA-W        PIC X(4).  
      
       01 DATOS-ENVIO.
          02 SW-INVALID-ENV            PIC XX.           
          02 FILLER                    PIC X VALUE "|".
          02 LLAVE-NUM-ENV             PIC X(7).

                  
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
              MOVE "SER836C"                TO MSJ3-HTML
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
              MOVE "SER836C"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-NUMERACION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-NUMER-W              TO MSJ2-HTML
              MOVE "SER836C"                TO MSJ3-HTML
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

           UNSTRING LLEGADA-W DELIMITED BY "|"
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, PACIENTE-LLEGADA-W,
                   FECHA-LLEGADA-W, LLAVE-ACT-LLEGADA-W, OPERADOR-LLEGADA-W
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.


       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
          
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.

       ABRIR-USUARIO.
006910     INITIALIZE OTR-STAT.
006920     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
006930
006940     OPEN INPUT ARCHIVO-USUNET
006950     EVALUATE OTR-STAT
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
007070
007080     INITIALIZE LLAVE-USUNET.

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
         
           INITIALIZE NOM-NUMER-W 

           MOVE "\\" TO NOM-NUMER-W 

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-NUMER-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-NUMER-W REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-NUMER-W REPLACING FIRST "                              "
                                            BY  DIR-CONTAB-LLEGADA-W
           
           INSPECT NOM-NUMER-W REPLACING FIRST "                     "
                                            BY "\CONTROL\SC-NUMER.DAT".

       VENTANA-PACIENTES.      	
           INITIALIZE LLAVE-NUM-W.

            COMPUTE ANO-INI-W = ANO-LLEGADA-W + 2000
            MOVE MES-LLEGADA-W TO MES-INI-W.
            MOVE DIA-LLEGADA-W TO DIA-INI-W.

            IF DIA-INI-W > 4
               SUBTRACT 4 FROM DIA-INI-W

            ELSE
               IF MES-INI-W = 01
                  MOVE 12 TO MES-INI-W
                  SUBTRACT 1 FROM ANO-INI-W

               ELSE
                  SUBTRACT 1 FROM MES-INI-W

               END-IF

               MOVE 28 TO DIA-INI-W
            END-IF.

           
       ABRIR-NUMERACION.
           OPEN INPUT  ARCHIVO-NUMERACION.
           MOVE 00         TO SW-INVALID-ENV.

           MOVE FECHA-INI-W TO FECHA-ING-NUM.

           START ARCHIVO-NUMERACION KEY >= FECHA-ING-NUM
                 INVALID KEY
                 MOVE 01    TO SW-INVALID-ENV
           END-START.

       LEER-FACTURAS.
           READ ARCHIVO-NUMERACION NEXT WITH NO LOCK AT END 
                GO TO CERRAR-NUMERACION
           END-READ. 
                                                            
           IF ESTADO-NUM = 0 OR 3
              CONTINUE
           ELSE
              GO TO LEER-FACTURAS
           END-IF

           IF PACIENTE-LLEGADA-W IS NOT EQUAL TO ID-PAC-NUM
              GO TO LEER-FACTURAS
           END-IF
           MOVE 99                  TO MSJ1-HTML
           MOVE ESTADO-NUM  TO MSJ2-HTML
           MOVE "Sc"                TO MSJ3-HTML
           GO TO ENVIAR2-ERROR.
        otr.
           IF LLAVE-ACT-LLEGADA-W  =  LLAVE-NUM
                GO TO LEER-FACTURAS
           END-IF                

           MOVE LLAVE-NUM   TO LLAVE-NUM-W

           GO TO LEER-FACTURAS.

       CERRAR-NUMERACION.
           MOVE LLAVE-NUM-W TO LLAVE-NUM-ENV.
           CLOSE ARCHIVO-NUMERACION.
    
       PAGINA-CONFIG.	 
           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO      TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3
         
           MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
           PERFORM ABRIR-HTML.

       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.

              
       COPY "..\..\FUENTES\SC-WEB.CBL".
