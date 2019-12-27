004820*=================================================================
      * CREACION DE ARCHUIVO JSON SER108 SALUD
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "..\..\FUENTES\FS-USUNET.CBL".
004920     COPY "..\..\FUENTES\FS-SESION.CBL".
004930
005020     SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN  NOM-USUA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.
           SELECT ARCHIVO-TERCEROS  LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TER-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-TERCERO
                  ALTERNATE RECORD KEY DESCRIP-TER WITH DUPLICATES
                  ALTERNATE RECORD KEY ACT-TER     WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.
           SELECT ARCHIVO-NUMERACION LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-NUMER-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-NUM
	          ALTERNATE RECORD KEY NIT-NUM         WITH DUPLICATES
                  ALTERNATE RECORD KEY DESCRIP-NUM     WITH DUPLICATES
                  ALTERNATE RECORD KEY NOMBRE-PAC-NUM  WITH DUPLICATES
                  ALTERNATE RECORD KEY HAB-NUM         WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-ING-NUM   WITH DUPLICATES
                  ALTERNATE RECORD KEY FECHA-RET-NUM   WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.
           SELECT ARCHIVO-PACIENTES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PACI-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PACI
                  ALTERNATE RECORD KEY  DESCRIP-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  EPS-PACI WITH DUPLICATES
                  ALTERNATE RECORD KEY  ID-COTIZ-PACI WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-TERCE.CBL".
       COPY "..\..\FUENTES\FD-NUMER.CBL".
       COPY "..\..\FUENTES\FD-PACIE.CBL".

       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-USUA-W                 PIC X(70).
       77 NOM-TER-W                  PIC X(70).
       77 NOM-NUMER-W                PIC X(70).
       77 NOM-PACI-W                 PIC X(70).
    

       01  VAR-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 IP-SERVER-W               PIC X(18).
           02 DIR-CONTAB-LLEGADA-W      PIC X(8).
           02 MES-CONTAB-LLEGADA-W      PIC XX.

       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.
       
       01 LIN-1.  
          02 FILLER                   PIC X(7)   VALUE "{*NUM_FACT*:".
          02 FILLER                   PIC X      VALUE "*".
          02 NRO-NUM-W                PIC X(2).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)  VALUE "*HABITAC*:".
          02 FILLER                   PIC X      VALUE "*".
          02 HAB-NUM-W                PIC X(5).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*FECHA_ING*:".
          02 FILLER                   PIC X      VALUE "*".
          02 FECHA-ING-NUM-W          PIC X(3).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*DESCRIP_TER*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DESCRIP1-NUM-W           PIC X(35).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(13)  VALUE "*NOM_PAC*:".
          02 FILLER                   PIC X      VALUE "*".
          02 REST-NOM-NUM-W           PIC X(11).
          02 FILLER                   PIC X(7)   VALUE "*CONVENIO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CONVENIO-NUM-W           PIC XX.             
          02 FILLER                   PIC XX     VALUE "*}".    
          02 CIERRE-LIN-1             PIC X.

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
              MOVE "SER108"                 TO MSJ3-HTML
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
              MOVE "SER108"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"  
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MENSAJE1-HTML
              MOVE NOM-USUA-W       TO MENSAJE2-HTML
              MOVE "ARC-USUOK"      TO MENSAJE3-HTML
              GO TO ENVIAR-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TERCEROS.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MENSAJE1-HTML
              MOVE NOM-TER-W        TO MENSAJE2-HTML
              MOVE "ARC-TEROK"      TO MENSAJE3-HTML
              GO TO ENVIAR-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-NUMERACION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MENSAJE1-HTML
              MOVE NOM-NUMER-W      TO MENSAJE2-HTML
              MOVE "ARC-NUMOK"      TO MENSAJE3-HTML
              GO TO ENVIAR-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MENSAJE1-HTML
              MOVE NOM-PACI-W       TO MENSAJE2-HTML
              MOVE "ARC-PACIOK"          TO MENSAJE3-HTML
              GO TO ENVIAR-ERROR
           END-IF.

           END DECLARATIVES.
006730
006740 INICIAR-IIS.
006750     MOVE LOW-VALUE TO COBW3.
006760     MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
006770     CALL "COBW3_INIT" USING COBW3.
006780
006790 LEER-DATO-HTML.
006800     MOVE "datosh" TO COBW3-SEARCH-DATA.
006810     CALL "COBW3_GET_VALUE" USING COBW3.
006820     MOVE COBW3-GET-DATA    TO DATO-LLEGADA-W.

001855     MOVE LLAVESESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
000830     ACCEPT HORA-TOTAL FROM TIME.

000797 INICIAR-SESION.
000806     MOVE "D:\PROGELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
000820    
000830     GO TO VALIDAR-SESION. 
000850
000860 FIN-VALIDAR-SESION.
006840
006900 ABRIR-USUARIO.
006910     INITIALIZE OTR-STAT.
006920     MOVE "D:\PROGELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
006930
006940     OPEN INPUT ARCHIVO-USUNET
006950     EVALUATE OTR-STAT
006960       WHEN "00"  CONTINUE
006970       WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
006980                  INITIALIZE REG-USUNET
006990                  WRITE REG-USUNET
007000                  CLOSE ARCHIVO-USUNET
007010                  MOVE "Validacion de Usuario"       TO MENSAJE1-HTML
007020                  MOVE "Falta configurar usuarios"   TO MENSAJE2-HTML
007030                  MOVE "Sc"                          TO MENSAJE3-HTML
007040                  GO TO ENVIAR-ERROR
007050       WHEN OTHER GO TO CERRAR-SESION
007060     END-EVALUATE.
007070
007080     INITIALIZE LLAVE-USUNET.

007100
007110 LEER-USUARIO.
007120     READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
007130
007140     CLOSE ARCHIVO-USUNET.
007200     
007210     IF NOMBRE-USUNET = SPACES
007220     OR NIT-USUNET = ZEROS
007230        MOVE "Validacion de Sesion"        TO MENSAJE1-HTML
007240        MOVE "Falta configurar usuarios"   TO MENSAJE2-HTML
007250        MOVE "Sc"                          TO MENSAJE3-HTML
007260        GO TO ENVIAR-ERROR
007270     END-IF.
007280 
007350

       ASIGNAR-NOMBRES.

           INITIALIZE NOM-USUA-W NOM-TER-W NOM-NUMER-W NOM-PACI-W
                     
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\".
           MOVE "\\"  TO NOM-NUMER-W
    
           INSPECT NOM-REST-W  REPLACING FIRST "                    "
007160                                      BY IP-DATOS-USUNET
007170     INSPECT NOM-REST-W  REPLACING FIRST " "
007180                                      BY "\"
007190     INSPECT NOM-REST-W  REPLACING FIRST "                          "
007200                                      BY "NEWCOBOL\DATOS\ARCHREST.SC".


           INSPECT NOM-USUAR-W REPLACING FIRST  "                    "
                                            BY  IP-DATOS-USUNET
           INSPECT NOM-USUAR-W REPLACING FIRST  " "
                                            BY  "\"
           INSPECT NOM-USUAR-W REPLACING FIRST  "                              "
007183                                      BY  DIRECTORIO-USUNET
           INSPECT NOM-USUAR-W REPLACING FIRST  "                "
                                            BY  "\CONTROL\ARCHUSU".

          
           MOVE  NOM-USUAR-W   TO NOM-PROF-W

           INSPECT NOM-PROF-W  REPLACING FIRST  "ARCHUSU     "
                                            BY  "ARCHPROF.DAT".

           MOVE NOM-USUAR-W  TO NOM-PACIE-W       
           
           INSPECT NOM-PACIE-W  REPLACING FIRST "ARCHUSU     "
                                             BY "SC-PACIE.DAT".

          

           INSPECT NOM-HCAPE-W REPLACING FIRST  "                    "
                                            BY  IP-DATOS-USUNET

           INSPECT NOM-HCAPE-W REPLACING FIRST  "                   "
                                            BY  "\NEWCOBOL\HC\DATOS\"


           MOVE NOM-HCAPE-W    TO NOM-HCEVO-W

           INSPECT NOM-HCAPE-W REPLACING FIRST  "             "
                                            BY  "SC-HCAPER.S17".

       ABRIR-USUARIOS.











10700  PAGINA-CONFIG.  
010720     MOVE "datosrecibidos" TO COBW3-CNV-NAME
010730     MOVE 00               TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3
010750
010760     MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
010770     PERFORM ABRIR-HTML.
007980   
007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.

007990
008030
008040 COPY "..\..\FUENTES\SC-WEB.CBL".
005210* COPY "..\..\FUENTES\WS-HCEVO.CBL".

0
