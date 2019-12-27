      *=================================================================
      * BUSQUEDA EN LA FACTURAS LA AOTRIZACION DICE SI ESTA REPETIDA - SER836AU
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
       01 NRO-AUTORI-EDIT              PIC X(15).
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
          02 OPERADOR-LLEGADA-W        PIC X(4).
          02 PACIENTE-LLEGADA-W        PIC X(15).
          02 FECHA-LLEGADA-W.
             05 ANO-LLEGADA-W          PIC 99.
             05 MES-LLEGADA-W          PIC 99.
             05 DIA-LLEGADA-W          PIC 99.
          02 LLAVE-NUM-LLEGADA-W       PIC X(7). 
          02 LLAVE-ACT-LLEGADA-W       PIC X(7). 
          02 NRO-AUTORI-LLEGADA-W      PIC X(15).  
       

       01 DATOS-ENVIO.
          02 ESTADO-ENVIO              PIC XX.           
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W, PACIENTE-LLEGADA-W,
                   FECHA-LLEGADA-W, LLAVE-NUM-LLEGADA-W, LLAVE-ACT-LLEGADA-W, NRO-AUTORI-LLEGADA-W      
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
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
      
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
         
           INITIALIZE NOM-NUMER-W 

           MOVE "\\" TO NOM-NUMER-W 

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-NUMER-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-NUMER-W REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-NUMER-W REPLACING FIRST "                              "
                                            BY  DIR-CONTAB-LLEGADA-W.
           
           INSPECT NOM-NUMER-W REPLACING FIRST "                     "
                                            BY "\CONTROL\SC-NUMER.DAT".
       VENTANA-PACIENTES.      	
           INITIALIZE LLAVE-NUM-LLEGADA-W.

            COMPUTE ANO-INI-W = ANO-LLEGADA-W + 2000.
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

            MOVE NRO-AUTORI-LLEGADA-W TO NRO-AUTORI-EDIT.  

       ABRIR-NUMERACION.
           OPEN INPUT  ARCHIVO-NUMERACION.

           MOVE FECHA-INI-W TO FECHA-ING-NUM.
           START ARCHIVO-NUMERACION KEY >= FECHA-ING-NUM
                 INVALID KEY
                 GO TO CERRAR-ARCHIVO
           END-START.

       LEER-FACTURAS.
           
           READ ARCHIVO-NUMERACION NEXT AT END
                GO TO CERRAR-ARCHIVO 
           END-READ
      
           IF PACIENTE-LLEGADA-W IS NOT EQUAL TO ID-PAC-NUM
              GO TO LEER-FACTURAS
           END-IF.

           IF LLAVE-ACT-LLEGADA-W  =  LLAVE-NUM
                GO TO LEER-FACTURAS
           END-IF. 

           IF NRO-AUTORI-NUM IS NOT = SPACES
           AND NRO-AUTORI-EDIT = NRO-AUTORI-NUM
                CONTINUE
           ELSE
              GO TO LEER-FACTURAS
           END-IF.
              
           MOVE LLAVE-NUM   TO LLAVE-NUM-LLEGADA-W.

       ENVIO-DATOS.

           MOVE "00"                TO ESTADO-ENVIO.
           MOVE LLAVE-NUM-LLEGADA-W TO LLAVE-NUM-ENV. 

       CERRAR-ARCHIVO.
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

              
       COPY "..\..\FUENTES\SC-WEB19.CBL".
