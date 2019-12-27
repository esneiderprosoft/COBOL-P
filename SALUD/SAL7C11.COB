      *=================================================================
      * SALUD - MAESTRO DE CITAS
      * PO -> 19/11/2019 PABLO OLGUIN - CREACION 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
       COPY "..\..\FUENTES\FS-PACIE.CBL".
       COPY "..\..\FUENTES\FS-TERCE.CBL".

           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-USUAR-LNK
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-PROFESIONALES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PROF-LNK
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PROF
                  ALTERNATE RECORD KEY DESCRIP-PROF  WITH DUPLICATES
                  ALTERNATE RECORD KEY ATIENDE-PROF  WITH DUPLICATES
                  ALTERNATE RECORD KEY SUCURSAL-PROF WITH DUPLICATES
					   FILE STATUS IS OTR-STAT.

           SELECT  EXTENSION-PROFESIONALES LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-EXTEN-PROF-LNK
                   ORGANIZATION IS INDEXED;
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS LLAVE-PROF2
                   ALTERNATE RECORD KEY FECHA-PROF2 WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-HORARIO-PROF LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-HORAR-PROF-LNK
                   ORGANIZATION IS INDEXED;
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS LLAVE-HOR-P
                   ALTERNATE RECORD KEY FECHA-HOR-P WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-RECOMENDACION LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-RECOME-LNK
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-RECOME
                  ALTERNATE RECORD KEY IS PACI-RECOME      WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LLAVE-ALT-RECOME WITH DUPLICATES                  
                  FILE STATUS IS OTR-STAT.                   
      
            SELECT ARCHIVO-TARIFAS LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-TAR-LNK
                   ORGANIZATION IS INDEXED;
                   ACCESS MODE DYNAMIC;
                   RECORD KEY  IS COD-TAR
                   FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-CONT-CAP LOCK MODE IS AUTOMATIC   
                   ASSIGN NOM-CONC-LNK
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS CONT-CNCAP
                   ALTERNATE RECORD KEY IS NIT-CNCAP WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.                  

            SELECT ARCHIVO-TABLAS LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-TAB-LNK
                   ORGANIZATION IS INDEXED;
                   ACCESS MODE DYNAMIC;
                   RECORD KEY  IS LLAVE-TAB
                   ALTERNATE RECORD KEY IS DESCRIP-TAB   WITH DUPLICATES
                   ALTERNATE RECORD KEY IS COD-RIPS-TAB  WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.

	        SELECT ARCHIVO-MACRO-INV  LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-MACRINV-LNK
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-MACRO
                  ALTERNATE RECORD KEY IS NOMBRE-MACRO WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.            
                         
           SELECT ARCHIVO-CITAS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-CITAS-LNK
                  ORGANIZATION INDEXED;
                  ACCESS MODE  DYNAMIC;
                  RECORD KEY LLAVE-CIT
				      ALTERNATE RECORD KEY IS FECHA-CIT        WITH DUPLICATES
				      ALTERNATE RECORD KEY IS MEDICO-CIT       WITH DUPLICATES
				      ALTERNATE RECORD KEY IS PACI-CIT         WITH DUPLICATES
                  ALTERNATE RECORD KEY IS LLAVE-ALT-CIT    WITH DUPLICATES
				      ALTERNATE RECORD KEY IS ATIENDE-PROF-CIT WITH DUPLICATES				  
                  ALTERNATE RECORD KEY IS FECHA-FACTU-CIT  WITH DUPLICATES
				      ALTERNATE RECORD KEY IS FECHA-SOLIC-CIT  WITH DUPLICATES				  
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-CITAUT LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-CITAUT-LNK
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC;
                   RECORD KEY IS COD-CITAUT
                   FILE STATUS IS OTR-STAT.                  

           SELECT ARCHIVO-ESPEC-CUP LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-ESPECUP-LNK
                   ORGANIZATION IS INDEXED;
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS COD-ESP-CUP
                   FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-CUPS LOCK MODE IS AUTOMATIC
                   ASSIGN NOM-CUPS-LNK
                   ORGANIZATION IS INDEXED;
                   ACCESS MODE DYNAMIC;
                   RECORD KEY  IS LLAVE-CUP
                   ALTERNATE RECORD KEY IS DESCRIP-CUP WITH DUPLICATES
                   ALTERNATE RECORD KEY IS LLAVE-ALT-CUP WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-COLEGIOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-COLEG-LNK
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY LLAVE-COLEG
                  ALTERNATE RECORD KEY IS DESCRIP-COLEG WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.                   
                  
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-PACIE.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-HORAR.CBL".
       COPY "..\..\FUENTES\FD-PROFE.CBL".
       COPY "..\..\FUENTES\FD-COLEG.CBL".
       COPY "..\..\FUENTES\FD-RECOME.CBL".
       COPY "..\..\FUENTES\FD-PROFE2.CBL".
       COPY "..\..\FUENTES\FD-TARIF.CBL".
       COPY "..\..\FUENTES\FD-TABLA.CBL".
       COPY "..\..\FUENTES\FD-CNCAP.CBL".
       COPY "..\..\FUENTES\FD-CUPS19.CBL".
       COPY "..\..\FUENTES\FD-TERCE.CBL".
       COPY "..\..\FUENTES\FD-MACRI.CBL".
       COPY "..\..\FUENTES\FD-CITAS.CBL".
       COPY "..\..\FUENTES\FD-CITAUT.CBL".
       COPY "..\..\FUENTES\FD-ESCUP.CBL".
      * COPY "..\..\FUENTES\FD-SMS.CBL".
      
       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".

      *  ESPEJO PARA RECIBIR DATOS-CITA MEDICA SELECCIONADA O CREADA 
       COPY "..\..\FUENTES\WS-SAL7C11.CBL".
       
       01 ANO-X.
          03 ANO-X1     PIC XX.    
          03 ANO-X2     PIC XX.  

       01 ID-PACI-W     PIC X(15).
       01 UNSERV-W      PIC 99.

       77  SAL-MIN-W    PIC 9(6)V99.
       77  ESTADO-ANT   PIC X.

       01 DATOS-ENVIO.
          02 DATO1-ENV  PIC X.
       
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
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-USU-W        TO MSJ2-HTML
              MOVE "SAL7C11"         TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" 
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-SESION-W     TO MSJ2-HTML
              MOVE "SAL7C11"         TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" 
              CONTINUE 
           ELSE
              MOVE OTR-STAT          TO MSJ1-HTML
              MOVE NOM-USUAR-LNK     TO MSJ2-HTML
              MOVE "SAL7C11"         TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" 
              CONTINUE 
           ELSE
              MOVE OTR-STAT          TO MSJ1-HTML
              MOVE NOM-PACIE-LNK     TO MSJ2-HTML
              MOVE "SAL7C11"         TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

        I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PROFESIONALES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PROF-LNK             TO MSJ2-HTML
              MOVE "SAL7C11"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       
        I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON EXTENSION-PROFESIONALES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-EXTEN-PROF-LNK       TO MSJ2-HTML
              MOVE "SAL7C11"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

        I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-HORARIO-PROF.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-HORAR-PROF-LNK       TO MSJ2-HTML
              MOVE "SAL7C11"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

        I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-RECOMENDACION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-RECOME-LNK           TO MSJ2-HTML
              MOVE "SAL7C11"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

        I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TARIFAS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-TAR-LNK              TO MSJ2-HTML
              MOVE "SAL7C11"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

        I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TABLAS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-TAB-LNK              TO MSJ2-HTML
              MOVE "SAL7C11"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

        I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-MACRO-INV.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-MACRINV-LNK          TO MSJ2-HTML
              MOVE "SAL7C11"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

        I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CITAS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-CITAS-LNK            TO MSJ2-HTML
              MOVE "SAL7C11"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

        I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CITAUT.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-CITAUT-LNK           TO MSJ2-HTML
              MOVE "SAL7C11"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

        I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ESPEC-CUP.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-ESPECUP-LNK           TO MSJ2-HTML
              MOVE "SAL7C11"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

        I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CONT-CAP.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-CONC-LNK             TO MSJ2-HTML
              MOVE "SAL7C11"                TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA    TO LINEA-LLEGAD-W

           UNSTRING LINEA-LLEGAD-W DELIMITED BY "|"
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W, ADMIN-W,
                   UNSERV-W, NOVEDAD-W, DATOS-CITA-W, DATOS-PACI-CIT-W
           END-UNSTRING.  
          
           MOVE SESION-LLEGAD-W   TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.

       INICIAR-SESION.
000806     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.
006840
006900 ABRIR-USUARIO.
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
007010                  MOVE "SC-2"                       TO MSJ1-HTML
007020                  MOVE "Falta configurar usuarios"  TO MSJ2-HTML
007030                  MOVE "SAL7C11"                     TO MSJ3-HTML
007040                  GO TO ENVIAR2-ERROR
007050      WHEN OTHER GO TO CERRAR-SESION
007060     END-EVALUATE.

007110 LEER-USUARIO.
007120     READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
007140     CLOSE ARCHIVO-USUNET.
007200     
           IF NOMBRE-USUNET = SPACES
007220     OR NIT-USUNET = ZEROS
007230        MOVE "SC-2"      TO MSJ1-HTML
007240        MOVE "Falta configurar usuarios" TO MSJ2-HTML
007250        MOVE "SAL7C11"    TO MSJ3-HTML
007260        GO TO ENVIAR2-ERROR
007270     END-IF.

       ASIGNAR-NOMBRES.

           INITIALIZE NOM-USUAR-LNK
           MOVE "\\" TO NOM-USUAR-LNK
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

           INSPECT NOM-USUAR-LNK REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

           MOVE NOM-USUAR-LNK TO NOM-CITAS-LNK NOM-CITAUT-LNK  NOM-ESPECUP-LNK
                                 NOM-CONC-LNK NOM-HORAR-PROF-LNK

           INSPECT NOM-USUAR-LNK REPLACING FIRST " " BY "\"              

           INSPECT NOM-USUAR-LNK 
           REPLACING FIRST "                              "
                        BY DIRECTORIO-LLEGAD-W

           MOVE NOM-USUAR-LNK TO NOM-TAR-LNK NOM-MACRINV-LNK NOM-TAB-LNK
                                 NOM-PROF-LNK NOM-EXTEN-PROF-LNK NOM-PACIE-LNK
                                 NOM-RECOME-LNK             
  
           INSPECT NOM-USUAR-LNK REPLACING FIRST "                       "
                                             BY  "\CONTROL\SC-ARCHUSU.DAT"
  
           INSPECT NOM-TAR-LNK REPLACING FIRST "                       "
                                           BY  "\CONTROL\SC-ARCHTAR.DAT"

           INSPECT NOM-TAB-LNK REPLACING FIRST "                     "
                                           BY  "\control\SC-TABLA.DAT"

           INSPECT NOM-MACRINV-LNK REPLACING FIRST "                        "
                                               BY  "\control\SC-MACROINV.DAT"

           INSPECT NOM-PROF-LNK REPLACING FIRST "                        "
                                            BY  "\CONTROL\SC-ARCHPROF.DAT"

           INSPECT NOM-PACIE-LNK REPLACING FIRST "                     "
                                            BY   "\CONTROL\SC-PACIE.DAT"

           INSPECT NOM-EXTEN-PROF-LNK REPLACING FIRST "                         "
                                                  BY  "\CONTROL\SC-ARCHPROF2.DAT"
            
           IF ANO-W > 2014 
              INSPECT NOM-CITAS-LNK REPLACING FIRST "                          "
                                                 BY "\PROG\DATOS\SC-AGENDX1.S19"
              MOVE ANO-W TO ANO-X
              INSPECT NOM-CITAS-LNK REPLACING FIRST "X1" BY ANO-X2 
           END-IF

           IF ANO-TOTAL <= 2014 
              INSPECT NOM-CITAS-LNK REPLACING FIRST "                          "
                                                 BY "\PROG\DATOS\SC-AGEND00.S19"
           END-IF

           INSPECT NOM-CITAUT-LNK REPLACING FIRST  "                         "
                                               BY  "\PROG\DATOS\SC-CITAUT.DAT"

           INSPECT NOM-RECOME-LNK REPLACING FIRST  "                        "
                                               BY  "\PROG\DATOS\SC-RECOM.DAT"

           INSPECT NOM-ESPECUP-LNK REPLACING FIRST "                          "
                                               BY  "\PROG\DATOS\SC-ESP-CUP.DAT"

           INSPECT NOM-HORAR-PROF-LNK REPLACING FIRST "                        "
                                                  BY  "\PROG\DATOS\SC-HORAR.DAT".
                                                  
           INSPECT NOM-CUPS-LNK   REPLACING FIRST "                           "
                                              BY  "\PROG\DATOS\SC-ARCHCUPS.DAT".

           INSPECT NOM-COLEG-LNK   REPLACING FIRST "                        "
                                               BY  "\PROG\DATOS\SC-COLEG.DAT".

           
       INICIO-CITAS.
           MOVE 0 TO SW-BUSCAR.

      * INCIO VALIDACIONES
       VALIDAR-ARCHIVO-CITAS.
           OPEN INPUT ARCHIVO-CITAS.
           EVALUATE OTR-STAT
             WHEN "00" CLOSE ARCHIVO-CITAS 
                       CONTINUE
             WHEN OTHER GO TO CERRAR-SESION
           END-EVALUATE.

       VALIDAR-ARCHIVO-HORARIO-PROF.
           OPEN INPUT ARCHIVO-HORARIO-PROF.
           EVALUATE OTR-STAT
             WHEN "00"  CLOSE ARCHIVO-HORARIO-PROF
                        OPEN OUTPUT ARCHIVO-HORARIO-PROF
                        CLOSE       ARCHIVO-HORARIO-PROF
           END-EVALUATE.           

       VALIDAR-ARCHIVO-EXTENSION-PROF.
           OPEN INPUT EXTENSION-PROFESIONALES.
           EVALUATE OTR-STAT
             WHEN "00"  CLOSE EXTENSION-PROFESIONALES
                        OPEN OUTPUT EXTENSION-PROFESIONALES
                        CLOSE       EXTENSION-PROFESIONALES
           END-EVALUATE.

       VALIDAR-ARCHIVO-CITAUT.
           OPEN INPUT ARCHIVO-CITAUT.
           EVALUATE OTR-STAT
             WHEN "00" CLOSE ARCHIVO-CITAUT
                       CONTINUE          
             WHEN "35" OPEN OUTPUT ARCHIVO-CITAUT
                       CLOSE ARCHIVO-CITAUT
           END-EVALUATE.

       VALIDAR-ARCHIVO-RECOMENDACION.
           OPEN INPUT ARCHIVO-RECOMENDACION.
           EVALUATE OTR-STAT
             WHEN "00"  CLOSE ARCHIVO-RECOMENDACION
             WHEN "35"  OPEN OUTPUT ARCHIVO-RECOMENDACION
                        CLOSE       ARCHIVO-RECOMENDACION
                        GO TO ABRIR-RECOMENDA
             WHEN OTHER CLOSE ARCHIVO-PROFESIONALES
                            ARCHIVO-HORARIO-PROF
                            ARCHIVO-CITAUT
                            ARCHIVO-RECOMENDACION
           END-EVALUATE.
      * FIN VALIDACIONES

       LEER-CITAS.
           INITIALIZE REG-CIT.
           MOVE 00 TO SW-INVALID
           MOVE LLAVE-CIT-W TO LLAVE-CIT

           READ ARCHIVO-CITAS WITH NO LOCK
                INVALID KEY  MOVE 01 TO SW-INVALID
           END-READ

           EVALUATE NOVEDAD-W
             WHEN "7"   IF SW-INVALID = 01 
                           GO TO CITA-NUEVA
                        ELSE IF SW-INVALID = 00
                           MOVE "SC-2"                   TO MSJ1-HTML
                           MOVE "NOVEDAD NO CORRESPONDE" TO MSJ2-HTML
                           MOVE "SAL7C11"                TO MSJ3-HTML
                        END-IF
             WHEN "8"   IF SW-INVALID = 00
                           GO TO ACTUALIZAR-CITA.
                        ELSE IF SW-INVALID = 01
                           MOVE SW-INVALID TO DATO1-ENV
                           GO TO PAGINA-CONFIG
                        END-IF    

             WHEN "9"   IF SW-INVALID = 00 
                           GO TO RETIRO-CITA
                        ELSE IF  SW-INVALID = 01   
                           MOVE  SW-INVALID TO DATO1-ENV
                           GO TO PAGINA-CONFIG
                        END-IF
           END-EVALUATE.

       CITA-NUEVA.
           INITIALIZE OBSER-CIT-W        FINALID-CIT-W  
                      CLAVE-CANC-CIT-W   SECU-RBO-CIT-W
                      TABLA-ESTAD-CIT-W  EMBARAZO-CIT-W
                      FECHA-FACTU-CIT-W  OPER-CORR-CIT-W
                      HORA-FACTU-CIT-W   FECHA-COR-CIT-W

           MOVE MEDICO-CIT-W    TO MEDICO-CIT
           MOVE FECHA-CIT-W     TO FECHA-CIT 

           

       ACEPTAR-DATOS-CITA.
           



                      
010700 PAGINA-CONFIG.
010720     MOVE "statuscode" TO COBW3-CNV-NAME
010730     MOVE "00"         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

000570     MOVE "datosrecibidos" TO COBW3-CNV-NAME
000580     MOVE  DATOS-ENVIO     TO COBW3-CNV-VALUE
000590	   CALL "COBW3_SET_CNV"  USING COBW3

010720     MOVE "programa-id"   TO COBW3-CNV-NAME
010730     MOVE "SAL7C11"       TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010760     MOVE "..\PAGINAS\RECIBIDOSPLANO.ASPX" TO SALIDA-HTML
010770     PERFORM ABRIR-HTML.

007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.

008040 COPY "..\..\FUENTES\SC-WEB19.CBL".
