004820*=================================================================
004830* GENERA UN ARCHIVO PLANO ADT PARA HL7 ADT VERSION 2.4
      * ORM GENERA INFORMACION SOBRE CUPS FACTURADO
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "..\..\FUENTES\FS-USUNET.CBL".
004920     COPY "..\..\FUENTES\FS-SESION.CBL".

                 SELECT ARCHIVO-SALIDA
                        ASSIGN RANDOM NOM-SALIDA
                        ORGANIZATION IS LINE SEQUENTIAL
                        ACCESS MODE IS SEQUENTIAL.

005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".

       FD  ARCHIVO-SALIDA
           LABEL RECORD STANDARD
           RECORD VARYING IN SIZE FROM 1 TO 600 CHARACTERS
           DEPENDING ON NRO-CAR-W.
       01  REG-SALIDA.
           02 LN-EDIT          PIC X(600).

005190 WORKING-STORAGE SECTION.
005200 COPY "..\..\FUENTES\COBW3.CBL".
005210 COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LINEA-LLEGADA-W               PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W             PIC X(15).
             03 FECHA-LLEGAD-W          PIC X(8).
             03 HORA-LLEGAD-W           PIC X(6).
          02 DIR-CONTAB-LLEGADA-W       PIC X(30).
          02 MES-CONTAB-LLEGADA-W       PIC XX.
          02 OPERADOR-LLEGADA-W         PIC X(4).

       77 HORA-W                        PIC 9(8).
       77 NRO-CAR-W                     PIC 9(3).
       77 SW-CONT                       PIC 9.
       77 SW-INI                        PIC 9.
       77 K                             PIC 9(3).
       77 PASO-W                        PIC 9.

       01 HORA-CIT-FACT.
          02 HR-CIT-FACT                PIC 99.
          02 MN-CIT-FACT                PIC 99.  
       01 REMITE-FACT                   PIC 9(10).
       01 CL-FACT                       PIC 9.
       01 VLR-LATER-FACT                PIC 9.
       01 CAUSA-ESTAD                   PIC 99.
       01 PUERTA-ESTAD-FACT             PIC 9.
       01 DESCRIP2-TER                  PIC X(20).
       01 DESCRIP-PROF                  PIC X(30).
       01 REG-MED-PROF                  PIC X(10).
       01 DESCRIP-CUP.
          05 DESCRIP1-CUP.
             07 OCULTAR-CUP             PIC X.
             07 FILLER                  PIC X(29).
          05 DESCRIP2-CUP               PIC X(50).
       01 NOM-SALIDA                    PIC X(50).
       
       01  COD-PREST-W.
           02 CIUDAD-W                  PIC X(5).
           02 NUIR-W                    PIC X(5).
           02 SUC-W                     PIC XX.

       01 COD-PACI                      PIC X(15).

       01  COD-PACI-X.
           02 CAR-PACI-A                PIC X OCCURS 15.

       01  COD-PACI-Y.
           02 CAR-PACI-B                PIC X OCCURS 15.

       01  LINEA-X.
           02 CAR-LIN-A                 PIC X OCCURS 600.

       01  LINEA-Y.
           02 CAR-LIN-B                 PIC X OCCURS 600.

       01 FECHA-SIG-W.
          02 SIG-W                      PIC 99.
          02 FECHA-W.
             05 ANO-W                   PIC 99.
             05 MES-W                   PIC 99.
             05 DIA-W                   PIC 99.

       01 LINEA-1.
          02 ENCAB-L1       PIC X(23)  VALUE "MSH|^~\&|PROSOFT|TDX|||".
          02 FECHA-L1                   PIC 9(8).
          02 HORA-L1.
             03 HR-L1                   PIC 99.
             03 MN-L1                   PIC 99.
             03 SG-L1                   PIC 99.
          02 TIPO-MSG-L1                PIC X(10) VALUE "||ORM^O01|".
          02 CONSEC-MSG-L1.
             03 EMPR-MSG-L1             PIC 9.
             03 SUC-MSG-L1              PIC XX.
             03 CL-MSG-L1               PIC 9.
             03 NRO-MSG-L1              PIC 9(6).
             03 ITEM-MSG-L1             PIC 9(2).
          02 PROCC-L1                   PIC X(3)  VALUE "|P|".
          02 VERSION-L1                 PIC X(3)  VALUE "2.4".
          02 FIN-L1                     PIC X     VALUE "*".

       01 LINEA-2.
          02 ENCAB-L2                   PIC X(5) VALUE "PID||".
          02 ID-PAC-L2                  PIC X(15).
          02 FILLER                     PIC X VALUE "|".
          02 ID-HC-L2                   PIC X(15).
          02 FILLER                     PIC X(2) VALUE "||".
          02 APELLIDO1-L2               PIC X(15).
          02 FILLER                     PIC X    VALUE " ".
          02 APELLIDO2-L2               PIC X(15).
          02 FILLER                     PIC X    VALUE "^".
          02 NOMBRE1-L2                 PIC X(15).
          02 FILLER                     PIC X VALUE " ".
          02 NOMBRE2-L2                 PIC X(15).
          02 FILLER                     PIC X(2) VALUE "||".
          02 F-NACI-L2                  PIC 9(8).
          02 FILLER                     PIC X VALUE "|".
          02 SEXO-L2                    PIC X.
          02 FILLER                     PIC X VALUE "|".
          02 FILLER                     PIC X(2)  VALUE "||".
          02 DIRECC-L2                  PIC X(15).
          02 FILLER                     PIC X(2)  VALUE "^^".
          02 NOMBRE-CIU-L2              PIC X(15).
          02 FILLER                     PIC X(2)  VALUE "^^".
          02 COD-CIU-L2                 PIC X(5).
          02 FILLER                     PIC X     VALUE "^".
          02 PAIS-L2                    PIC X(2).
          02 FILLER                     PIC X     VALUE "^".
          02 DIR-COMP-L2                PIC X(30).
          02 FILLER                     PIC X(2)  VALUE "||".
          02 TELEFONO-L2                PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 TELEFONO2-L2               PIC X(10).
          02 FILLER                     PIC X(2)  VALUE "||".
          02 EST-CIVIL-L2               PIC X.
          02 FILLER                     PIC X(13)  VALUE "|||||||||||||".
          02 FIN-L2                     PIC X     VALUE "*".

       01 LINEA-IN1.
          02 ENCAB-IN1                  PIC X(7) VALUE "IN1|1||".
          02 NIT-IN1                    PIC X(10).
          02 FILLER                     PIC X VALUE "|".
          02 DESCR-NIT-IN1              PIC X(60).
          02 FIN-IN1                    PIC X     VALUE "*".

       01 LINEA-3.
          02 ENCAB-L3                   PIC X(4) VALUE "ORC|".
          02 ORDEN-CTL-L3               PIC X(2).
          02 FILLER                     PIC X VALUE "|".
          02 ID-MSG-L3                  PIC X(12).
          02 FILLER                     PIC X VALUE "^".
          02 SUC-IPS-L3                 PIC XX.
          02 FILLER                     PIC X(2) VALUE "||".
          02 ORC4-1-L3                  PIC X(12).
          02 FILLER                     PIC X VALUE "|".
          02 ESTADO-L3                  PIC XX.
          02 FILLER                     PIC X(2) VALUE "||".
          02 FILLER                     PIC X(3) VALUE "^^^".
      *   ORC7-4
          02 FECHA-INI-L3               PIC 9(8).
          02 HORA-INI-L3.
             03 HR-INI-L3               PIC 99.
             03 MN-INI-L3               PIC 99.
          02 FILLER                     PIC X VALUE "^".
      *   ORC7-5
          02 FECHA-FIN-L3               PIC 9(8).
          02 HORA-FIN-L3.
             03 HR-FIN-L3               PIC 99.
             03 MN-FIN-L3               PIC 99.
             03 SG-FIN-L3               PIC 99.
          02 FILLER                     PIC X VALUE "^".
          02 PRIORIDAD-L3               PIC 9.
          02 FILLER                     PIC X(3) VALUE "|||".
          02 MED-SOLIC-L3               PIC 9(10).
          02 FILLER                     PIC X(2) VALUE "||".
          02 REG-MED-SOL-L3             PIC X(10).
          02 FILLER                     PIC X VALUE "^".
          02 APELL-MED-L3               PIC X(30).
          02 FILLER                     PIC X VALUE "^".
          02 NOMBRES-MED-L3             PIC X(30).
          02 FILLER                     PIC X VALUE "|".
          02 FROM-ATENC-L3              PIC X(11).
          02 FILLER                     PIC X(4) VALUE "||||".
          02 EST-ORDEN-L3               PIC X(3).
          02 FILLER                     PIC X VALUE "^".
          02 FIN-L3                     PIC X     VALUE "*".

       01 LINEA-4.
          02 ENCAB-L4                   PIC X(5) VALUE "OBR||".
          02 ID-MSG-L4                  PIC X(12).
          02 FILLER                     PIC X VALUE "^".
          02 SUC-IPS-L4                 PIC XX.
          02 FILLER                     PIC X(2) VALUE "||".
          02 CUPS-L4                    PIC X(17).
          02 FILLER                     PIC X     VALUE "^".
          02 NOM-CUPS-L4                PIC X(50).
          02 FILLER                     PIC X     VALUE "^".
          02 SUC2-IPS-L4                PIC XX.
          02 FILLER                     PIC X(3)  VALUE "^^^".
          02 LATERALIDAD-L4             PIC X(11).
          02 FILLER                     PIC X(2)  VALUE "||".
          02 FECHA-ORD-L4               PIC 9(8).
          02 HORA-ORD-L4.
             03 HR-ORD-L4               PIC 99.
             03 MN-ORD-L4               PIC 99.
             03 SG-ORD-L4               PIC 99.
          02 FILLER                     PIC X(7)  VALUE "|||||||".
          02 INFO-CLINIC-L4             PIC X(30).
          02 FILLER                     PIC X(18) VALUE "||||||||||||||||||".
          02 CAUSA-L4                   PIC X(15).
          02 FIN-L4                     PIC X     VALUE "*".

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV             PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 NOM-SALIDA-ENV             PIC X(50).
          

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
              MOVE "HL7002"                 TO MSJ3-HTML
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
              MOVE "HL7002"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SALIDA.
       ESCR-EXCEPTIONES.                                                                                                                                
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-SALIDA               TO MSJ2-HTML
              MOVE "HL7002"                 TO MSJ3-HTML
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
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W,
                    EMPR-MSG-L1, SUC-MSG-L1, CL-MSG-L1, NRO-MSG-L1, ITEM-MSG-L1, COD-PACI,
                    APELLIDO1-L2, APELLIDO2-L2, NOMBRE1-L2, NOMBRE2-L2, F-NACI-L2, SEXO-L2, DIRECC-L2, COD-CIU-L2,
                    DIR-COMP-L2, TELEFONO-L2, TELEFONO2-L2, EST-CIVIL-L2, HORA-CIT-FACT, REMITE-FACT, CL-FACT, 
                    ART-LNK, VLR-LATER-FACT, CAUSA-ESTAD, DESCRIP2-TER, PUERTA-ESTAD, DESCRIP-PROF, REG-MED-PROF,
                    DESCRIP-CUP
           END-UNSTRING.

001855     MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.

000797 INICIAR-SESION.
000806     MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.
006840
006900 ABRIR-USUARIO.
006910     INITIALIZE OTR-STAT.
006920     MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
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
007230        MOVE "Validacion de Sesion"        TO MSJ1-HTML
007240        MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007250        MOVE "Sc"                          TO MsJ3-HTML
007260        GO TO ENVIAR2-ERROR
007270     END-IF.
007280 
007350

        ASIGNAR-NOMBRES.    
           MOVE "00"   TO SW-INVALID-ENV
001614     INITIALIZE NOM-SALIDA

           MOVE "S:\EXPORTAR\HL7\ORM-X1X1X1X1X1X1.HL7" TO NOM-SALIDA.
           INSPECT NOM-SALIDA REPLACING FIRST "X1X1X1X1X1X1"
                                        BY CONSEC-MSG-L1.

           OPEN OUTPUT ARCHIVO-SALIDA.
           ACCEPT FECHA-ACT   FROM DATE.
           MOVE FECHA-ACT     TO FECHA-W.

           IF ANO-W > 90
              MOVE 19         TO SIG-W
           ELSE
              MOVE 20         TO SIG-W.

           MOVE FECHA-SIG-W   TO FECHA-L1
           ACCEPT HORA-L1     FROM TIME.

      *     MOVE CONSEC-MSG-L1 TO NOM-MSG-L1.

           INSPECT LINEA-1 REPLACING ALL "*" BY " ".
           MOVE "*" TO FIN-L1
           MOVE LINEA-1 TO LINEA-X.
           PERFORM ELIMINAR-BLANCOS.
           MOVE LINEA-Y TO LINEA-1
           INSPECT LINEA-1 REPLACING ALL "*" BY " ".
           MOVE LINEA-1 TO REG-SALIDA.
           SUBTRACT 1 FROM NRO-CAR-W
           WRITE REG-SALIDA.

      *LINEA-2.
           MOVE COD-PACI TO COD-PACI-X.
           PERFORM ELIMINAR-CEROS
           MOVE COD-PACI-Y      TO ID-PAC-L2
           MOVE COD-PACI-Y      TO ID-HC-L2
           MOVE "BOGOTA"        TO NOMBRE-CIU-L2
           MOVE "CO"            TO PAIS-L2

           INSPECT LINEA-2 REPLACING ALL "*" BY " ".
           MOVE "*" TO FIN-L2
           MOVE LINEA-2 TO LINEA-X.
           PERFORM ELIMINAR-BLANCOS.
           MOVE LINEA-Y TO LINEA-2
           INSPECT LINEA-2 REPLACING ALL "*" BY " ".
           MOVE LINEA-2 TO REG-SALIDA.
           SUBTRACT 1 FROM NRO-CAR-W
           WRITE REG-SALIDA.

      *LINEA-IN1.
           IF DESCRIP2-TER IS SPACES OR ZEROS
              MOVE NIT-FACT  TO DESCRIP2-TER.

           MOVE NIT-FACT     TO NIT-IN1
           MOVE DESCRIP-TER2 TO DESCR-NIT-IN1

           INSPECT LINEA-IN1 REPLACING ALL "*" BY " ".
           MOVE "*" TO FIN-IN1
           MOVE LINEA-IN1 TO LINEA-X.
           PERFORM ELIMINAR-BLANCOS.
           MOVE LINEA-Y TO LINEA-IN1
           INSPECT LINEA-IN1 REPLACING ALL "*" BY " ".
           MOVE LINEA-IN1 TO REG-SALIDA.
           SUBTRACT 1 FROM NRO-CAR-W
           WRITE REG-SALIDA.

      *LINEA-3.
           IF PROCESO-W = 1
              MOVE "CA"          TO ORDEN-CTL-L3
              MOVE "CA"          TO ESTADO-L3
           ELSE
              MOVE "NW"          TO ORDEN-CTL-L3
              MOVE "SC"          TO ESTADO-L3
           END-IF.

           MOVE CONSEC-MSG-L1 TO ID-MSG-L3.
           MOVE CONSEC-MSG-L1 TO ORC4-1-L3.
           MOVE SUC-MSG-L1    TO SUC-IPS-L3.
           MOVE FECHA-L1      TO FECHA-INI-L3 FECHA-FIN-L3
           MOVE HORA-L1       TO HORA-FIN-L3.
           MOVE HORA-CIT-FACT TO HORA-INI-L3

           IF PUERTA-ESTAD = 1
              MOVE 1 TO PRIORIDAD-L3
           ELSE
              MOVE 2 TO PRIORIDAD-L3.

            IF DESCRIP-PROF IS SPACES
                MOVE REMITE-FACT TO DESCRIP-PROF
                INITIALIZE REG-MED-PROF.

           MOVE COD-PROF        TO MED-SOLIC-L3.
           MOVE REG-MED-PROF    TO REG-MED-SOL-L3.

           EVALUATE CL-FACT
             WHEN 0 MOVE "FARMACIA"    TO FROM-ATENC-L3
             WHEN 1 MOVE "CIRUGIA "    TO FROM-ATENC-L3
             WHEN 2 MOVE "LABORATORIO" TO FROM-ATENC-L3
             WHEN 3 MOVE "IMAGENES   " TO FROM-ATENC-L3
             WHEN 4 MOVE "OTROS SERV." TO FROM-ATENC-L3
             WHEN 5 MOVE "CONSULTAS  " TO FROM-ATENC-L3
             WHEN 7 MOVE "PROMOC&PREV" TO FROM-ATENC-L3.

           MOVE 0 TO EST-ORDEN-L3.

           INSPECT LINEA-3 REPLACING ALL "*" BY " ".
           MOVE "*" TO FIN-L3
           MOVE LINEA-3 TO LINEA-X.
           PERFORM ELIMINAR-BLANCOS.
           MOVE LINEA-Y TO LINEA-3
           INSPECT LINEA-3 REPLACING ALL "*" BY " ".
           MOVE LINEA-3 TO REG-SALIDA.
           SUBTRACT 1 FROM NRO-CAR-W
           WRITE REG-SALIDA.

      *LINEA-4.
           MOVE CONSEC-MSG-L1 TO ID-MSG-L4.
           MOVE SUC-MSG-L1    TO SUC-IPS-L4.
           MOVE SUC-MSG-L1    TO SUC2-IPS-L4.

           MOVE ART-LNK       TO CUPS-L4

           MOVE ART-LNK TO LLAVE-CUP

           IF DESCRIP-CUP IS SPACES
              MOVE LLAVE-CUP   TO DESCRIP-CUP.              
           
           MOVE DESCRIP-CUP        TO NOM-CUPS-L4.

           EVALUATE VLR-LATER-FACT (L)
              WHEN 1     MOVE "Derecha    " TO LATERALIDAD-L4
              WHEN 2     MOVE "Izquierda  " TO LATERALIDAD-L4
              WHEN 3     MOVE "Comparativa" TO LATERALIDAD-L4
              WHEN OTHER MOVE "No aplica  " TO LATERALIDAD-L4
           END-EVALUATE.

           MOVE FECHA-SIG-W        TO FECHA-ORD-L4
           ACCEPT HORA-ORD-L4      FROM TIME.

           INITIALIZE INFO-CLINIC-L4

           EVALUATE CAUSA-ESTAD
           WHEN 0  MOVE    "SIN ASIGNAR"        TO CAUSA-L4
           WHEN 1  MOVE    "ACCIDENTE TRABAJO " TO CAUSA-L4
           WHEN 2  MOVE    "ACCIDENTE TRANSITO" TO CAUSA-L4
           WHEN 3  MOVE    "ACCIDENTE RABICO  " TO CAUSA-L4
           WHEN 4  MOVE    "ACCIDENTE OFIDICO " TO CAUSA-L4
           WHEN 5  MOVE    "OTRO ACCIDENTE    " TO CAUSA-L4
           WHEN 6  MOVE    "EVENTO CATASTROFIC" TO CAUSA-L4
           WHEN 7  MOVE    "LESION AGRESION   " TO CAUSA-L4
           WHEN 8  MOVE    "LESION AUTO INFLIG" TO CAUSA-L4
           WHEN 9  MOVE    "SOSP. MALTRATO FIS" TO CAUSA-L4
           WHEN 10 MOVE    "SOSP. ABUSO SEXUAL" TO CAUSA-L4
           WHEN 11 MOVE    "SOSP. VIOLECIA SEX" TO CAUSA-L4
           WHEN 12 MOVE    "SOSP. MALTRATO EMO" TO CAUSA-L4
           WHEN 13 MOVE    "ENFERMEDAD GENERAL" TO CAUSA-L4
           WHEN 14 MOVE    "ENFERMEDAD PROFES." TO CAUSA-L4
           WHEN 15 MOVE    "OTRA CAUSA        " TO CAUSA-L4.

           MOVE LATERALIDAD-L4 TO CAUSA-L4

           INSPECT LINEA-4 REPLACING ALL "*" BY " ".
           MOVE "*" TO FIN-L4
           MOVE LINEA-4 TO LINEA-X.
           PERFORM ELIMINAR-BLANCOS.
           MOVE LINEA-Y TO LINEA-4
           INSPECT LINEA-4 REPLACING ALL "*" BY " ".
           MOVE LINEA-4 TO REG-SALIDA.
           SUBTRACT 1 FROM NRO-CAR-W
           WRITE REG-SALIDA.

           MOVE NOM-SALIDA  TO NOM-SALIDA-ENV

           CLOSE ARCHIVO-SALIDA.

010700  PAGINA-CONFIG.
010720      MOVE "datosrecibidos"    TO COBW3-CNV-NAME
010730      MOVE DATOS-ENVIO         TO COBW3-CNV-VALUE
010740      CALL "COBW3_SET_CNV" USING COBW3
010750
010760      MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
010770      PERFORM ABRIR-HTML.
007980   
007990 CERRAR-SESION.
008000      CALL "COBW3_FREE" USING COBW3.
008010      MOVE 1 TO PROGRAM-STATUS.
008020      EXIT PROGRAM.

008030
008040 COPY "..\..\FUENTES\SC-WEB.CBL".

       ELIMINAR-BLANCOS.
           MOVE 0 TO SW-FIN K NRO-CAR-W.
           PERFORM BUSCAR-BLANCO VARYING Y FROM 1 BY 1
                                 UNTIL Y > 599 OR SW-FIN = 1.

       BUSCAR-BLANCO.
           IF  CAR-LIN-A (Y) = SPACES
           AND CAR-LIN-A (Y + 1) = SPACES
           AND CAR-LIN-A (Y + 2) = SPACES
              CONTINUE
           ELSE
              IF  CAR-LIN-A (Y) = SPACES
              AND CAR-LIN-A (Y + 1) = SPACES
                  CONTINUE
              ELSE
                 IF CAR-LIN-A (Y) = SPACES
                 AND (CAR-LIN-A (Y + 1) = "|" OR
                     CAR-LIN-A (Y + 1) = "^")
                     CONTINUE
                 ELSE
                     ADD 1 TO K NRO-CAR-W
                     MOVE CAR-LIN-A (Y) TO CAR-LIN-B (K)
                 END-IF
              END-IF
           END-IF.

           IF CAR-LIN-B (K) = "*"
              MOVE 1 TO SW-FIN
           END-IF.


       ELIMINAR-CEROS.
           MOVE 0 TO SW-FIN K NRO-CAR-W SW-INI.
           PERFORM BUSCAR-CEROS VARYING Y FROM 1 BY 1
                                UNTIL Y > 15 OR SW-FIN = 1.

       BUSCAR-CEROS.
           IF CAR-PACI-A (Y) = 0
              IF SW-INI > 0
                 ADD 1 TO K NRO-CAR-W
                 MOVE CAR-PACI-A (Y) TO CAR-PACI-B (K)
              END-IF
           ELSE
              MOVE CAR-PACI-A (Y) TO SW-INI
              ADD 1 TO K NRO-CAR-W
              MOVE CAR-PACI-A (Y) TO CAR-PACI-B (K)
           END-IF.


005210* COPY "..\..\FUENTES\WS-HCEVO.CBL".
