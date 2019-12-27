      *=================================================================
      * FACTURACION - CONSULTA DE FACTURA PARA IMPRIMIR 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\..\FUENTES\FS-USUNET.CBL".
           COPY "..\..\FUENTES\FS-SESION.CBL".
  
           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-USUAR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

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

           SELECT ARCHIVO-ENTIDADES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-ENT-W
      *"\PROG\DATOS\SC-ARCHENT.DAT";
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-ENT
                  ALTERNATE RECORD KEY IS NOMBRE-ENT WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-PACIENTES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PACIE-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PACI
                  ALTERNATE RECORD KEY  DESCRIP-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  EPS-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  ID-COTIZ-PACI WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-TERCEROS  LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TER-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-TERCERO
                  ALTERNATE RECORD KEY DESCRIP-TER WITH DUPLICATES
                  ALTERNATE RECORD KEY ACT-TER     WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.


       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-NUMER19.CBL".
       COPY "..\..\FUENTES\FD-ENTID.CBL".
       COPY "..\..\FUENTES\FD-PACIE.CBL".
       COPY "..\..\FUENTES\FD-TERCE.CBL".


005190 WORKING-STORAGE SECTION.
005200 COPY "..\..\FUENTES\COBW3.CBL".
005210 COPY "..\..\FUENTES\WEB-CARAC19.CBL".

       77 NOM-USUAR-W                     PIC X(70).
       77 NOM-NUMER-W                     PIC X(70).
       77 NOM-ENT-W                       PIC X(70).
       77 NOM-PACIE-W                     PIC X(70).
       77 NOM-TER-W                       PIC X(70).  
       77 DATOS-PLANO-W                   PIC X(2000).
       77 ADMIN-W                         PIC X(4).
       01 LINEA-LLEGADA-W                 PIC X(100).
       
       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W               PIC X(15).
             03 FECHA-LLEGAD-W            PIC X(8).
             03 HORA-LLEGAD-W             PIC X(6).
          02 DIR-CONTAB-LLEGADA-W         PIC X(30).
          02 MES-CONTAB-LLEGADA-W         PIC XX.
          02 LLAVE-NUM-LLEGADA-W.
             03 PREFIJO-W                 PIC X.
             03 NRO-W                     PIC 9(6).
          02 NIT-NUM-LLEGADA-W            PIC 9(10).
          02 DESCRIP-TER-LLEGADA-W        PIC X(40).
          02 OPERADOR-LLEGADA-W           PIC X(4).

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV               PIC XX.
          02 FILLER                       PIC X      VALUE "|".

       01 LIN-1.
          02 FILLER                         PIC X(11) VALUE "{*FACTURA*:".
          02 FILLER                         PIC X     VALUE "*".
          02 LLAVE-NUM-J.
             03 PREFIJO-NUM-J               PIC X.
             03 NRO-NUM-J                   PIC 9(6).          
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(6) VALUE "*NIT*:".
          02 FILLER                         PIC X     VALUE "*".
          02 NIT-NUM-J                      PIC 9(10).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(14) VALUE "*DESCRIPCION*:".
          02 FILLER                         PIC X     VALUE "*".
          02 DESCRIP-NUM-J                  PIC X(40).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(9) VALUE "*CEDULA*:".
          02 FILLER                         PIC X     VALUE "*".
          02 ID-PAC-NUM-J                   PIC X(15).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(11) VALUE "*PACIENTE*:".
          02 FILLER                         PIC X     VALUE "*".
          02 DESCRIP-PACI-J                 PIC X(54).          
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(6) VALUE "*EPS*:".
          02 FILLER                         PIC X     VALUE "*".
          02 EPS-PACI-J                     PIC X(6).         
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(12) VALUE "*NOMBREEPS*:".
          02 FILLER                         PIC X     VALUE "*".
          02 NOMBRE-ENT-J                   PIC X(70).            
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(13) VALUE "*HABITACION*:".
          02 FILLER                         PIC X     VALUE "*".
          02 HAB-NUM-J                      PIC X(4).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(10) VALUE "*INGRESO*:".
          02 FILLER                         PIC X     VALUE "*".
          02 FECHA-ING-NUM-J                PIC 9(8).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(9) VALUE "*RETIRO*:".
          02 FILLER                         PIC X     VALUE "*".
          02 FECHA-RET-NUM-J                PIC 9(8).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(14) VALUE "*FUNCIONARIO*:".
          02 FILLER                         PIC X     VALUE "*".
          02 FUNC-AUTOR-ING-NUM-J           PIC 9(10).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(13) VALUE "*NOMBREFUNC*:".
          02 FILLER                         PIC X     VALUE "*".
          02 DESCRIP-TER-J                  PIC X(50). 
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(14) VALUE "*OBSERVACION*:".
          02 FILLER                         PIC X     VALUE "*".  
          02 OBSERV-CRE-NUM-J               PIC X(50).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(13) VALUE "*OPERCREADO*:".
          02 FILLER                         PIC X     VALUE "*".  
          02 OPER-NUM-J                     PIC X(4).         
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(14) VALUE "*FECHACREADO*:".
          02 FILLER                         PIC X     VALUE "*".  
          02 FECHA-CRE-NUM-J                PIC 9(8).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(12) VALUE "*OPERMODIF*:".
          02 FILLER                         PIC X     VALUE "*". 
          02 OPER-MOD-NUM-J                 PIC X(4).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(13) VALUE "*FECHAMODIF*:".
          02 FILLER                         PIC X     VALUE "*". 
          02 FECHA-MOD-NUM-J                PIC 9(8).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(11) VALUE "*OPERBLOQ*:".
          02 FILLER                         PIC X     VALUE "*".  
          02 OPER-BLOQ-NUM-J                PIC X(4).
          02 FILLER                         PIC XX    VALUE "*,".
          02 FILLER                         PIC X(8) VALUE "*ADMIN*:".
          02 FILLER                         PIC X     VALUE "*".  
          02 ADMIN-J                        PIC X(4).
          02 FILLER                         PIC XX    VALUE "*}".
          02 CIERRE-LIN-1                   PIC X.         

006310 LINKAGE SECTION.
006320 COPY "..\..\FUENTES\ISAPICTX.CBL".
006330 PROCEDURE DIVISION WITH stdcall LINKAGE USING ISAPI-CTX-CNT.
006340
006350 DECLARATIVES.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUNET.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USU-W                TO MSJ2-HTML
              MOVE "SER108-15"              TO MSJ3-HTML
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
              MOVE "SER108-15"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-USUAR-W          TO MSJ2-HTML
              MOVE "SER108-15"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-NUMERACION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-NUMER-W          TO MSJ2-HTML
              MOVE "SER108-03"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ENTIDADES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-ENT-W            TO MSJ2-HTML
              MOVE "SER108-15"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-PACIE-W          TO MSJ2-HTML
              MOVE "SER108-15"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TERCEROS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-TER-W            TO MSJ2-HTML
              MOVE "SER108-15"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
006720  END DECLARATIVES.
006730
006740 INICIAR-IIS.
006750     MOVE LOW-VALUE TO COBW3.
006760     MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
006770     CALL "COBW3_INIT" USING COBW3.
006780
006790 LEER-DATO-HTML.
006800     MOVE "datosh" TO COBW3-SEARCH-DATA.
006810     CALL "COBW3_GET_VALUE" USING COBW3.
006820     MOVE COBW3-GET-DATA    TO LINEA-LLEGADA-W.

           UNSTRING LINEA-LLEGADA-W DELIMITED BY "|"
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, 
                    LLAVE-NUM-LLEGADA-W, NIT-NUM-LLEGADA-W, DESCRIP-TER-LLEGADA-W, 
                    OPERADOR-LLEGADA-W 
           END-UNSTRING. 
001855     MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.

000797 INICIAR-SESION.
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
007010                  MOVE "Validacion de Usuario"       TO MSJ1-HTML
007020                  MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007030                  MOVE "Sc"                          TO MSJ3-HTML
007040                  GO TO ENVIAR2-ERROR
007050       WHEN OTHER GO TO CERRAR-SESION
007060     END-EVALUATE.
007070
007080     INITIALIZE LLAVE-USUNET.

007100
007110 LEER-USUARIO.
007120     READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
007140     CLOSE ARCHIVO-USUNET.
007200     
007210     IF NOMBRE-USUNET = SPACES
007230        MOVE "Validacion de Sesion"                  TO MSJ1-HTML
007240        MOVE "Falta configurar usuarios"             TO MSJ2-HTML
007250        MOVE "Sc"                                    TO MSJ3-HTML
007260        GO TO ENVIAR2-ERROR
007270     END-IF.
007280 
007350

        ASIGNAR-NOMBRES. 

           INITIALIZE NOM-USUAR-W
                      NOM-NUMER-W
                      NOM-ENT-W
                      NOM-PACIE-W 
                      NOM-TER-W 

           MOVE "\\" TO NOM-USUAR-W 
                        NOM-NUMER-W
                        NOM-ENT-W
                        NOM-PACIE-W
                        NOM-TER-W

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-USUAR-W REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-USUAR-W REPLACING FIRST "                              " 
                                            BY DIR-CONTAB-LLEGADA-W.
           
           MOVE NOM-USUAR-W    TO NOM-NUMER-W
                                  NOM-PACIE-W
                                  NOM-TER-W 

           INSPECT NOM-USUAR-W REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHUSU.DAT"

           INSPECT NOM-NUMER-W REPLACING FIRST "                     "
                                            BY "\CONTROL\SC-NUMER.DAT".

           INSPECT NOM-PACIE-W REPLACING FIRST "                     "
                                            BY "\CONTROL\SC-PACIE.DAT".

           INSPECT NOM-TER-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-ARCHTER.DAT".


           INSPECT NOM-ENT-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET
           INSPECT NOM-ENT-W REPLACING FIRST " "
                                          BY "\"
           INSPECT NOM-ENT-W REPLACING FIRST "                         "
                                           BY "PROG\DATOS\SC-ARCHENT.DAT".

       CONSULTA-FACTURA.

           OPEN INPUT ARCHIVO-USUARIOS
                      ARCHIVO-NUMERACION
                      ARCHIVO-ENTIDADES 
                      ARCHIVO-PACIENTES
                      ARCHIVO-TERCEROS. 

                                    
           MOVE LLAVE-NUM-LLEGADA-W  TO LLAVE-NUM
           MOVE "{*FACTURAS*:["       TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO.

       ABRIR-FACTURAS.  

           READ ARCHIVO-NUMERACION WITH NO LOCK
                INVALID KEY                
                CLOSE ARCHIVO-NUMERACION
                MOVE "SC-1"              TO MSJ1-HTML
                MOVE "01"                TO MSJ2-HTML
                MOVE "SER108-15"         TO MSJ3-HTML
                GO TO ENVIAR2-ERROR
           END-READ.
 
           MOVE LLAVE-NUM                TO LLAVE-NUM-J 
                                       
           MOVE NIT-NUM-LLEGADA-W        TO NIT-NUM-J
           MOVE DESCRIP-TER-LLEGADA-W    TO DESCRIP-NUM-J

           MOVE ID-PAC-NUM          TO ID-PAC-NUM-J
           MOVE ID-PAC-NUM-J        TO COD-PACI
           READ ARCHIVO-PACIENTES WITH NO LOCK
                INVALID KEY
                  MOVE COD-PACI    TO DESCRIP-PACI 
           END-READ 
           MOVE DESCRIP-PACI       TO DESCRIP-PACI-J
           MOVE EPS-PACI           TO EPS-PACI-J
           MOVE EPS-PACI-J         TO COD-ENT
           READ ARCHIVO-ENTIDADES WITH NO LOCK
                INVALID KEY
                MOVE COD-ENT       TO NOMBRE-ENT
           END-READ
           MOVE NOMBRE-ENT         TO NOMBRE-ENT-J
  
           MOVE HAB-NUM            TO HAB-NUM-J
           MOVE FECHA-ING-NUM      TO FECHA-ING-NUM-J
           MOVE FECHA-RET-NUM      TO FECHA-RET-NUM-J
           MOVE FUNC-AUTOR-ING-NUM TO FUNC-AUTOR-ING-NUM-J

           IF FUNC-AUTOR-ING-NUM-J IS ZERO
              MOVE SPACES          TO DESCRIP-TER 
           ELSE
              MOVE FUNC-AUTOR-ING-NUM-J TO COD-TER
              READ ARCHIVO-TERCEROS
                   INVALID KEY 
                   MOVE COD-TER    TO DESCRIP-TER
              END-READ
           MOVE DESCRIP-TER        TO DESCRIP-TER-J 

           IF OBSERV-CRE-NUM IS EQUAL SPACES
              MOVE SPACES TO OBSERV-CRE-NUM-J
           ELSE
              MOVE OBSERV-CRE-NUM  TO OBSERV-CRE-NUM-J
           END-IF.

           MOVE OPER-NUM           TO OPER-NUM-J
           MOVE FECHA-CRE-NUM      TO FECHA-CRE-NUM-J
           MOVE OPER-MOD-NUM       TO OPER-MOD-NUM-J 
           MOVE FECHA-MOD-NUM      TO FECHA-MOD-NUM-J
 
           IF OPER-BLOQ-NUM IS EQUAL SPACES
              MOVE SPACES   TO OPER-BLOQ-NUM-J
           ELSE
              MOVE OPER-BLOQ-NUM   TO OPER-BLOQ-NUM-J
           END-IF.

           MOVE OPERADOR-LLEGADA-W TO ADMIN-J

           MOVE "," TO CIERRE-LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO.


        CERRAR-FACTURA.
           INITIALIZE LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           MOVE "]}" TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO

           CLOSE ARCHIVO-USUARIOS
                 ARCHIVO-NUMERACION
                 ARCHIVO-ENTIDADES 
                 ARCHIVO-PACIENTES
                 ARCHIVO-TERCEROS.
        
010700  PAGINA-CONFIG.
010720     MOVE "statuscode" TO COBW3-CNV-NAME
           MOVE "00"         TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3

           MOVE "programa-id" TO COBW3-CNV-NAME
           MOVE "SER108-15"   TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV" USING COBW3

           MOVE "..\PAGINAS\RECIBIDOSLISTADO.ASPX" TO SALIDA-HTML
           PERFORM ABRIR-HTML.
007980   
007990  CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.

        DATOS-ENVIO.
           MOVE "datosrecibidos" TO COBW3-CNV-NAME
           MOVE DATOS-PLANO-W    TO COBW3-CNV-VALUE
           CALL "COBW3_SET_REPEAT" USING COBW3.
008030
008040  COPY "..\..\FUENTES\SC-WEB19.CBL".
