      *=================================================================
      * CONSULTA DE ARCHIVO TERCEROS- CON110C_01 -MOSTRAR DATOS 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

            SELECT ARCHIVO-TERCEROS  LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-TER-W
                   ORGANIZATION IS INDEXED;
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS COD-TERCERO
                   ALTERNATE RECORD KEY DESCRIP-TER WITH DUPLICATES
                   ALTERNATE RECORD KEY ACT-TER     WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.


            SELECT ARCHIVO-JSON LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-PLANO-W
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS OTR-STAT.            

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-TERCE.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON      PIC X(500).

       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LLEGADA-W                    PIC X(100).
       77 NOM-TER-W                    PIC X(70).
       77 NOM-PLANO-W                  PIC X(90).

       01 DATOS-DIRECCIONES-TERW.
          02 TABLA-DIRECCIONES-TERW  OCCURS 20   PIC X(15).


       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(30).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
	  02 OPERADOR-LLEGADA-W        PIC X(4).
          02 COD-TERCERO-LLEGADA-W.
             03 COD-TER                PIC 9(10).
          
       01 LIN-1.  
          02 FILLER                   PIC X(13)  VALUE "{*DIRECCION*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DIRECCION-EXT-TER-J      PIC X(60).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*CODCIU*:".
          02 FILLER                   PIC X      VALUE "*".
          02 COD-CIU-EXT-TER-J.
             03 DPTO-CIU-EXT-TER-J    PIC 99.
             03 CIUD-CIU-EXT-TER-J    PIC 9(3).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*TELEXT*:".
          02 FILLER                   PIC X      VALUE "*".
          02 TELEFONO-EXT-TER-J       PIC 9(12).  
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*BARRIOTER*:".
          02 FILLER                   PIC X      VALUE "*".
          02 BARRIO-TER-J             PIC 9(8).     
          02 FILLER                   PIC XX     VALUE "*}".    
          02 CIERRE-LIN-1             PIC X.


       01 DATOS-ENVIO.
          02 SW-INVALID-ENV            PIC X(2).
          02 FILLER                    PIC X VALUE "|".
          02 NRO-VEH-ENV               PIC X(4).
          02 FILLER                    PIC X VALUE "|".
          02 PLACA-VEH-ENV.
             03 PLACA1-VEH             PIC XX.
             03 PLACA2-VEH             PIC X(4).
          02 FILLER                    PIC X VALUE "|".
          02 REPR-TER-ENV.
             03 ID-REPR-LEGAL-TER      PIC 9(10).
             03 REPR-LEGAL-TER         PIC X(30). 
          02 FILLER                    PIC X VALUE "|".
          02 E-MAIL-REP-TER2-ENV       PIC X(50).
          02 FILLER                    PIC X VALUE "|".
          02 TESOR-TER-ENV.
             03 ID-TESOR-TER           PIC 9(10).
             03 TESORERO-TER           PIC X(30).
          02 FILLER                    PIC X VALUE "|".
          02 E-MAIL-TES-TER2-ENV       PIC X(50).
          02 FILLER                    PIC X VALUE "|".
          02 REF1-TER2-ENV.
             03 NOM-REF1-TER2            PIC X(30).
             03 DIR-REF1-TER2            PIC X(30).
             03 TEL-REF1-TER2            PIC X(15).
             03 REL-REF1-TER2            PIC X(10).
          02 FILLER                    PIC X VALUE "|".
          02 REF2-TER2-ENV.
             03 NOM-REF2-TER2            PIC X(30).
             03 DIR-REF2-TER2            PIC X(30).
             03 TEL-REF2-TER2            PIC X(15).
             03 REL-REF2-TER2            PIC X(10).
          02 FILLER                    PIC X VALUE "|".
          02 REF3-TER2-ENV.
             03 NOM-REF3-TER2            PIC X(30).
             03 DIR-REF3-TER2            PIC X(30).
             03 TEL-REF3-TER2            PIC X(15).
             03 REL-REF3-TER2            PIC X(10).
          02 FILLER                    PIC X VALUE "|".
          02 TRAB-TER2-ENV.
             03 NOM-TRAB-TER2            PIC X(30).
             03 DIR-TRAB-TER2            PIC X(30).
             03 TEL-TRAB-TER2            PIC X(15).
             03 CAR-TRAB-TER2            PIC X(15).
             03 SUE-TRAB-TER2            PIC 9(10).
             03 ANT-TRAB-TER2            PIC X(10).
          02 FILLER                    PIC X VALUE "|".
          02 FECHA-CUMP-VEH-ENV.
             03 ANO-NAC-TER2           PIC 9(4).
             03 MES-CUMP-VEH           PIC 99.
             03 DIA-CUMP-VEH           PIC 99.
          02 FILLER                    PIC X VALUE "|".
          02 EMBARGOS-TER2-ENV.
             03 EMBARG1-TER2             PIC X(3).
             03 EMBARG2-TER2             PIC X(27).
          02 FILLER                    PIC X VALUE "|".
          02 CIU-EXP-TER2-ENV.
             03 DPTO-EXP-TER2            PIC 99.
             03 CIUD-EXP-TER2            PIC 9(3).
          02 FILLER                    PIC X VALUE "|".
          02 ENTID-AFI-TER2-ENV              PIC 9(10).
          02 FILLER                    PIC X VALUE "|".
          02 FECHA-AFI-TER2-ENV.
             03 ANO-AFI-TER2             PIC 9(4).
             03 MES-AFI-TER2             PIC 9(2).
             03 DIA-AFI-TER2             PIC 9(2).
          02 FILLER                    PIC X VALUE "|".
          02 CLASIFICACION-TER-ENV           PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 NOMBRE-JSON-ENV            PIC X(28).

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
              MOVE "CON110C_01"             TO MSJ3-HTML
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
              MOVE "CON110C_01"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TERCEROS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-TER-W                TO MSJ2-HTML
              MOVE "CON110C_01"             TO MSJ3-HTML
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
              MOVE "INV103-15"              TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W,
                   COD-TERCERO-LLEGADA-W
                    
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGA-W   TO LLAVE-SESION-W.
      
       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W

      *     MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W

          
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.

       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W

      *     MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W

      
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
         
           INITIALIZE NOM-TER-W
                    
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
 
           MOVE "\\" TO NOM-TER-W
                       
           INSPECT NOM-TER-W REPLACING FIRST "                    "
                                           BY IP-DATOS-USUNET 
           INSPECT NOM-TER-W REPLACING FIRST " "
                                           BY "\"
           INSPECT NOM-TER-W REPLACING FIRST "                              "
                                            BY DIR-CONTAB-LLEGADA-W. 
           INSPECT NOM-TER-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-ARCHTER.DAT".

           INITIALIZE NOM-PLANO-W

           MOVE OPERADOR-LLEGADA-W TO OPER-JSON-W
           MOVE FUNCTION CURRENT-DATE TO FECHA-JSON-W
           ACCEPT HORA-JSON-W FROM TIME


           MOVE NOMBRE-JSON-W  TO NOM-PLANO-W.
           MOVE NOM-JSON-W     TO NOMBRE-JSON-ENV.

       ABRIR-ARCHIVO.

           OPEN INPUT ARCHIVO-TERCEROS.
                     
           MOVE "00"  TO SW-INVALID.

       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-JSON.

           MOVE "{*DIRECCIONES*:[" TO  DATOS-JSON
           INSPECT DATOS-JSON  REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON  END-WRITE
           MOVE 0 TO SW-FIN.


       LEER-ARCHIVO.

           READ ARCHIVO-TERCEROS NEXT WITH NO LOCK AT END
                CONTINUE
           END-READ.

           INITIALIZE DATOS-DIRECCIONES-TERW
           
           MOVE COD-TERCERO-LLEGADA-W  TO COD-TERCERO

           READ ARCHIVO-TERCEROS WITH NO LOCK 
                INVALID KEY
                  MOVE "01"  TO SW-INVALID
           END-READ.

           PERFORM MOVER-DIRECC VARYING I FROM 1 BY 1
                                     UNTIL I > 20.
          
           MOVE SW-INVALID        TO SW-INVALID-ENV
           MOVE NRO-VEH           TO NRO-VEH-ENV 
           MOVE PLACA-VEH         TO PLACA-VEH-ENV
           MOVE REPR-TER          TO REPR-TER-ENV           
           MOVE E-MAIL-REP-TER2   TO E-MAIL-REP-TER2-ENV                    
           MOVE TESOR-TER         TO TESOR-TER-ENV            
           MOVE E-MAIL-TES-TER2   TO E-MAIL-TES-TER2-ENV
           MOVE REF1-TER2         TO REF1-TER2-ENV
           MOVE REF2-TER2         TO REF2-TER2-ENV
           MOVE REF3-TER2         TO REF3-TER2-ENV
           MOVE TRAB-TER2         TO TRAB-TER2-ENV
           MOVE FECHA-CUMP-VEH    TO FECHA-CUMP-VEH-ENV
           MOVE EMBARGOS-TER2     TO EMBARGOS-TER2-ENV
           MOVE CIU-EXP-TER2      TO CIU-EXP-TER2-ENV
           MOVE ENTID-AFI-TER2    TO ENTID-AFI-TER2-ENV
           MOVE FECHA-AFI-TER2    TO FECHA-AFI-TER2-ENV
           MOVE CLASIFICACION-TER TO CLASIFICACION-TER-ENV. 
                
       CERRAR-JSON.
           INITIALIZE LIN-1

           INITIALIZE DIRECCION-EXT-TER-J
                      COD-CIU-EXT-TER-J
                      TELEFONO-EXT-TER-J 
                      BARRIO-TER-J
  
                                                                 
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-1 TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
          
           MOVE "]}" TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE.

       CERRAR-ARCHIVO.
        
           CLOSE ARCHIVO-TERCEROS
                 ARCHIVO-JSON.

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

       MOVER-DIRECC.
           INITIALIZE LIN-1

      *     DIRECCION-EXT-TER (I) = SPACES OR LOW-VALUES

         
           MOVE DIRECCION-EXT-TER (I)      TO DIRECCION-EXT-TER-J
           MOVE COD-CIU-EXT-TER (I)        TO COD-CIU-EXT-TER-J
           MOVE TELEFONO-EXT-TER (I)       TO TELEFONO-EXT-TER-J 
           MOVE BARRIO-TER (I)             TO BARRIO-TER-J

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
           INSPECT LIN-1 REPLACING ALL LOW-VALUE BY " "
           MOVE "," TO CIERRE-LIN-1

           MOVE LIN-1 TO DATOS-JSON
           WRITE REG-JSON END-WRITE. 

              
       COPY "..\..\FUENTES\SC-WEB19.CBL".
