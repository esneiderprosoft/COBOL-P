004820*=================================================================
004830* CREA JSON DE CONSULTA DE HISTORIAS CLINICAS POR PACIENTE.    
004840*=================================================================
      * PABLO OLG. 16/10/19 SE AGREGAN PSO,UND-PESO,TALLA,PER-CEF,PER-ABDO,PER-TORA
      * A EL ARCHIVO .JSON
      
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
004920     COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".

           SELECT HISTORIA-CLINICA LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-HCAPE-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-HC
                  ALTERNATE RECORD KEY IS FOLIO-HC WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FECHA-HC WITH DUPLICATES
                  ALTERNATE RECORD KEY IS MED-HC WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ESTADO-HC WITH DUPLICATES
                  FILE STATUS OTR-STAT.

           SELECT HISTORIA-DETALLADA LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-HCDET-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY IS LLAVE-DETHC
                  ALTERNATE RECORD KEY IS LLAVE-FOLIO-DETHC
                  WITH DUPLICATES
                  FILE STATUS OTR-STAT.

           SELECT ARCHIVO-PACIENTES
                  ASSIGN NOM-PACIE-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PACI
                  ALTERNATE RECORD KEY  DESCRIP-PACI  WITH DUPLICATES
                  ALTERNATE RECORD KEY  EPS-PACI      WITH DUPLICATES
                  ALTERNATE RECORD KEY  ID-COTIZ-PACI WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-PROFESIONALES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PROF-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-PROF
                  ALTERNATE RECORD KEY DESCRIP-PROF WITH DUPLICATES
                  ALTERNATE RECORD KEY ATIENDE-PROF  WITH DUPLICATES
                  ALTERNATE RECORD KEY SUCURSAL-PROF WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-UNID-SERV   LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-UNSERV-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CODIGO-UNID-SERV
                   FILE STATUS IS OTR-STAT.


           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-USUAR-W
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

           SELECT ARCHIVO-ESPECIALIDAD LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-ESPEC-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-ESP
                  ALTERNATE RECORD KEY IS NOMBRE-ESP WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-RESTRICCION LOCK MODE IS AUTOMATIC
                  ASSIGN TO NOM-RESTR-W
                  ORGANIZATION INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY LLAVE-REST
                  ALTERNATE RECORD KEY NOMBRE-REST WITH DUPLICATES
                  FILE STATUS OTR-STAT.

005070
005080 DATA DIVISION.
005090 FILE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-HCAPER17.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-HCDETAL17.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-PACIE.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-PROFE.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-UNSER.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUAR.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-TERCE.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-ESPEC.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-RESTR.CBL".


005190 WORKING-STORAGE SECTION.
005200 COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
005210 COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC19.CBL".
005210 COPY "P:\PROG\PROYECT\FUENTES\WS-8001.CBL".
005210 COPY "P:\PROG\PROYECT\FUENTES\WS-8031.CBL".
005220 
       77 NOM-USUAR-W                PIC X(70).
       77 NOM-RESTR-W                PIC X(70).
       77 NOM-HCAPE-W                PIC X(70).
       77 NOM-HCDET-W                PIC X(70).
       77 NOM-PACIE-W                PIC X(70).
       77 NOM-UNSERV-W               PIC X(70).
       77 NOM-PROF-W                 PIC X(70).
       77 NOM-TER-W                  PIC X(70).
       77 NOM-ESPEC-W                PIC X(70).

       77 DATOS-PLANO-W              PIC X(3000).
       77 CONT                       PIC 99.
      
       77 SUCURSAL-W                 PIC X.
       77 SW-PYP                     PIC 999.
       77 SW-BLANCA                  PIC 999.
       77 SW-OPEN                    PIC 9.
       77 ULT-DIA-W                  PIC 99.

       01 FECHA-LIMI.
          02 ANO-LIMI                PIC 9(4). 
          02 MES-LIMI                PIC 99. 
          02 DIA-LIMI                PIC 99. 

       01 DATOS-ADMIN-W.
          02 ATIENDE-ADMIN-W         PIC X.
          02 ENTIDAD-ADMIN-W         PIC X(5).

       01 DATOS-OPER-W.
          02 NOMBRE-OPER-W.          
             03 NOMBRE1-OPER-W       PIC X(28).
             03 SUC-OPER-W           PIC X(2).
          02 IDENT-OPER-W            PIC 9(10).

       01 ID-EDIT.
          02 ID1-EDIT                PIC X(14).
          02 ID2-EDIT                PIC X.

       01 FOLIO-W.
          02 SUC-FOLIO-W             PIC XX.
          02 FILLER                  PIC X VALUE "-".
          02 NRO-FOLIO-W             PIC 9(6).

          
       01 MOTIVO-EDIT.
          02 MOTIVO1-EDIT            PIC X(10).
          02 MOTIVO2-EDIT            PIC X(35).

       01 FECHA-EDIT.
          02 MES-EDIT                PIC X(4).
          02 DIA-EDIT                PIC XX.
          02 FILLER                  PIC X VALUE "/".
          02 ANO-EDIT                PIC X(4).
         
       01 HORA-EDIT.
          02 HR-EDIT                 PIC XX.
          02 FILLER                  PIC X VALUE ":".
          02 MN-EDIT                 PIC XX.

       01 LLAVE-PACI-W               PIC X(15).


       01 FECHA-SIG-W.
          02 SIG-W              PIC 99.
          02 FECHA-W.
             03 ANO-W           PIC 99.
             03 MES-W           PIC 99.
             03 DIA-W           PIC 99.

       01 LIN-01.
          02 FILLER                  PIC X(09)  VALUE "{*ID-HC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ID-J                    PIC X(15).

          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*FOLIO-HC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FOLIO-SUC-J             PIC XX.
          02 FILLER                  PIC X      VALUE "-".
          02 FOLIO-NRO-J             PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".

          02 FILLER                  PIC X(10)  VALUE "*SERV-HC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SERV-J                  PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".

          02 FILLER                  PIC X(12)  VALUE "*UNSERV-HC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UNSERV-J                PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".

          02 FILLER                  PIC X(11)  VALUE "*NOM-SERV*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOM-SERV-J              PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".

          02 FILLER                  PIC X(11)  VALUE "*FECHA-HC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIA-J                   PIC XX.
          02 FILLER                  PIC X      VALUE "/".
          02 MES-J                   PIC XX.
          02 FILLER                  PIC X      VALUE "/".
          02 ANO-J                   PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".

          02 FILLER                  PIC X(10)  VALUE "*HORA-HC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HR-J                    PIC XX.
          02 FILLER                  PIC X      VALUE ":".
          02 MN-J                    PIC XX.

          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*PESO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PESO-HC-J               PIC ZZZZZ.Z.

          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*UND-PESO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 UND-PESO-HC-J           PIC X.

          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*TALLA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TALLA-HC-J              PIC X(3).

          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*PER_CEF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PER-CEF-HC-J            PIC ZZZ.Z.

          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*PER_TORA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PER-TORA-HC-J           PIC ZZZ.Z.

          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*PER_ABDO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PER-ABDO-HC-J           PIC ZZZ.Z.
          02 FILLER                  PIC XX     VALUE "*,".

          02 FILLER                  PIC X(11)  VALUE "*MOTIV-HC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 MOTIV-J                 PIC X(50).
          02 FILLER                  PIC XX     VALUE "*,".

          02 FILLER                  PIC X(12)  VALUE "*MED-HC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 MED-J                   PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          
          02 FILLER                  PIC X(15)  VALUE "*ESPECIALIDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESPEC-J                 PIC X(70).
          02 FILLER                  PIC XX     VALUE "*,".

          02 FILLER                  PIC X(9)   VALUE "*ESTADO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESTADO-J                PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          
          02 FILLER                  PIC X(12)  VALUE "*ESTADO-HC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESTADO-HC-J             PIC X(12).
          02 FILLER                  PIC XX     VALUE "*,".

          02 FILLER                  PIC X(10)  VALUE "*DETALLE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DETALLE-J               PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".

          02 FILLER                  PIC X(14)  VALUE "*NIT-FACT-HC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NIT-FACT-J              PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".

          02 FILLER                  PIC X(10)  VALUE "*FACT-HC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FACT-J                  PIC X(07).
          02 FILLER                  PIC XX     VALUE "*,".

          02 FILLER                  PIC X(09)  VALUE "*HAB-HC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HAB-J                   PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".

          02 FILLER                  PIC X(12)  VALUE "*EGRESO-HC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIA-EGRES-J             PIC XX.
          02 FILLER                  PIC X      VALUE "/".
          02 MES-EGRES-J             PIC XX.
          02 FILLER                  PIC X      VALUE "/".
          02 ANO-EGRES-J             PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".

          02 FILLER                  PIC X(15)  VALUE "*DIAG-MUER-HC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIAG-MUER-J             PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".

          02 FILLER                  PIC X(14)  VALUE "*OPER-CIE-HC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OPER-CIE-J              PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".

          02 FILLER                  PIC X(21)  VALUE "*OPER-ORD-SALIDA-HC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OPER-ORD-SALIDA-J       PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".

          02 FILLER                  PIC X(22)  VALUE "*FECHA-ORD-SALIDA-HC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIA-ORD-SALIDA-J        PIC XX.
          02 FILLER                  PIC X      VALUE "/".
          02 MES-ORD-SALIDA-J        PIC XX.
          02 FILLER                  PIC X      VALUE "/".
          02 ANO-ORD-SALIDA-J        PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".

          02 FILLER                  PIC X(21)  VALUE "*HORA-ORD-SALIDA-HC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 HR-ORD-SALIDA-J         PIC XX.
          02 FILLER                  PIC X      VALUE ":".
          02 MN-ORD-SALIDA-J         PIC XX.
          02 FILLER                  PIC XX     VALUE "*}".

          02 CIERRE-LIN-01           PIC X.


006310 LINKAGE SECTION.
006320 COPY "P:\PROG\PROYECT\FUENTES\ISAPICTX.CBL".
006330 PROCEDURE DIVISION WITH stdcall LINKAGE USING ISAPI-CTX-CNT.
006340
006350 DECLARATIVES.
006360 I-O-TEST SECTION.
006370     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUNET.         
006380 ESCR-EXCEPTIONES.
006390     IF OTR-STAT = "00"
006400        CONTINUE 
006410     ELSE
006420        MOVE OTR-STAT         TO MSJ1-HTML
006430        MOVE NOM-USU-W        TO MSJ2-HTML
006440        MOVE "HC811"          TO MSJ3-HTML
006450        GO TO ENVIAR2-ERROR
006460     END-IF.
006470
006480 I-O-TEST SECTION.
006490     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.
006500 ESCR-EXCEPTIONES.
006510     IF OTR-STAT = "00" 
006520        CONTINUE 
006530     ELSE
006540        MOVE OTR-STAT         TO MSJ1-HTML
006550        MOVE NOM-SESION-W     TO MSJ2-HTML
006560        MOVE "HC811"          TO MSJ3-HTML
006570        GO TO ENVIAR2-ERROR
006580     END-IF.
006590  

006480 I-O-TEST SECTION.
006490     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
006500 ESCR-EXCEPTIONES.
006510     IF OTR-STAT = "00" 
006520        CONTINUE 
006530     ELSE
006540        MOVE OTR-STAT         TO MSJ1-HTML
006550        MOVE NOM-USUAR-W      TO MSJ2-HTML
006560        MOVE "HC811"          TO MSJ3-HTML
006570        GO TO ENVIAR2-ERROR
006580     END-IF.
006590  

006480 I-O-TEST SECTION.
006490     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-RESTRICCION.
006500 ESCR-EXCEPTIONES.
006510     IF OTR-STAT = "00" 
006520        CONTINUE 
006530     ELSE
006540        MOVE OTR-STAT         TO MSJ1-HTML
006550        MOVE NOM-RESTR-W      TO MSJ2-HTML
006560        MOVE "HC811"          TO MSJ3-HTML
006570        GO TO ENVIAR2-ERROR
006580     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON HISTORIA-CLINICA.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-HCAPE-W      TO MSJ2-HTML
006680        MOVE "HC811"          TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR  
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON HISTORIA-DETALLADA.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-HCDET-W      TO MSJ2-HTML
006680        MOVE "HC811"          TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR  
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" 
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-PACIE-W      TO MSJ2-HTML
006680        MOVE "HC811"          TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

       I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-UNID-SERV.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00"
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-UNSERV-W     TO MSJ2-HTML
006680        MOVE "HC811"          TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PROFESIONALES.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" 
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-PROF-W       TO MSJ2-HTML
006680        MOVE "HC811"          TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TERCEROS.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" 
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-TER-W        TO MSJ2-HTML
006680        MOVE "HC811"          TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.

006600 I-O-TEST SECTION.
006610     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ESPECIALIDAD.
006620 ESCR-EXCEPTIONES.
006630     IF OTR-STAT = "00" 
006640        CONTINUE 
006650     ELSE
006660        MOVE OTR-STAT         TO MSJ1-HTML
006670        MOVE NOM-ESPEC-W      TO MSJ2-HTML
006680        MOVE "HC811"          TO MSJ3-HTML
006690        GO TO ENVIAR2-ERROR
006700     END-IF.
       END DECLARATIVES.    

006740 INICIAR-IIS.
006750     MOVE LOW-VALUE TO COBW3.
006760     MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
006770     CALL "COBW3_INIT" USING COBW3.
006780
006790 LEER-DATO-HTML.
006800     MOVE "datosh" TO COBW3-SEARCH-DATA.
006810     CALL "COBW3_GET_VALUE" USING COBW3.
006820     MOVE COBW3-GET-DATA    TO LINEA-LLEGAD-W.
           INITIALIZE DATO-LLEGADA-W

           UNSTRING LINEA-LLEGAD-W DELIMITED BY "|"
           INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W, LLAVE-PACI-W
           END-UNSTRING
001855     MOVE SESION-LLEGAD-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL
           ACCEPT HORA-TOTAL FROM TIME.

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
007010                  MOVE "SC-2"                        TO MSJ1-HTML
007020                  MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007030                  MOVE "HC811"                      TO MSJ3-HTML
007040                  GO TO ENVIAR2-ERROR
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
007230        MOVE "SC-2"      TO MSJ1-HTML
007240        MOVE "Falta configurar usuarios" TO MSJ2-HTML
007250        MOVE "HC811"    TO MSJ3-HTML
007260        GO TO ENVIAR2-ERROR
007270     END-IF.

       ASIGNAR-NOMBRES.
           INITIALIZE NOM-USUAR-W
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
           MOVE "\\" TO NOM-USUAR-W 

           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

           INSPECT NOM-USUAR-W REPLACING FIRST " "
                                            BY "\"

           INSPECT NOM-USUAR-W REPLACING FIRST "                              "
                                            BY DIRECTORIO-LLEGAD-W
                                               
           MOVE NOM-USUAR-W TO NOM-PACIE-W NOM-UNSERV-W NOM-PROF-W 
                               NOM-TER-W



           INSPECT NOM-USUAR-W REPLACING FIRST  "                       "
                                            BY  "\CONTROL\SC-ARCHUSU.DAT".

           INSPECT NOM-PACIE-W REPLACING FIRST  "                     "
                                           BY   "\CONTROL\SC-PACIE.DAT".

           INSPECT NOM-UNSERV-W REPLACING FIRST "                     "
                                             BY "\CONTROL\SC-UNSER.DAT".

           INSPECT NOM-PROF-W   REPLACING FIRST "                        "
                                             BY "\CONTROL\SC-ARCHPROF.DAT".

           INSPECT NOM-TER-W    REPLACING FIRST "                       "
                                             BY "\CONTROL\SC-ARCHTER.DAT".




      
           INITIALIZE NOM-RESTR-W
           MOVE "\\" TO NOM-RESTR-W 
           INSPECT NOM-RESTR-W  REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

           INSPECT NOM-RESTR-W REPLACING FIRST " "                                               
                                            BY "\".





           MOVE NOM-RESTR-W TO NOM-HCAPE-W NOM-HCDET-W NOM-ESPEC-W

           INSPECT NOM-RESTR-W  REPLACING FIRST "                         "
                                            BY  "PROG\DATOS\SC-ARCHREST.SC".



           INSPECT NOM-ESPEC-W REPLACING FIRST "                         "
                                            BY "PROG\DATOS\SC-ARCHESP.DAT".


           INSPECT NOM-HCAPE-W REPLACING FIRST "                        "
                                            BY "PROG\DATOS\SC-HCAPER.S17".

           INSPECT NOM-HCDET-W REPLACING FIRST "                          "
                                            BY "PROG\DATOS\SC-HCDETA-X.S17".

       LEER-USUARIO.
           OPEN INPUT ARCHIVO-USUARIOS
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.

           IF PREFIJO-USU = SPACES OR SPACE OR LOW-VALUES
              MOVE 01 TO PREFIJO-USU
           END-IF.


           PERFORM BUSCAR-OPER


           IF NIT-USU = 900565371 OR 900541158
               MOVE NOMBRE-REST   TO NOMBRE-OPER-W
               MOVE SUC-OPER-W    TO SUCURSAL-W
           ELSE
               MOVE PREFIJO-USU   TO SUCURSAL-W
           END-IF

           IF SUCURSAL-W = SPACES
              MOVE 01 TO SUCURSAL-W
           END-IF.

       LEER-PACIENTE.
           OPEN INPUT ARCHIVO-PACIENTES.
           MOVE LLAVE-PACI-W    TO   COD-PACI.
           READ ARCHIVO-PACIENTES WITH NO LOCK   
                INVALID KEY
                         MOVE "SC-2"          TO MSJ1-HTML
                         MOVE "Paciente no existe.!" TO MSJ2-HTML
                         MOVE "HC811"       TO MSJ3-HTML
                         GO TO ENVIAR2-ERROR
           END-READ.

       ABRIR-ARCHIVOS.

           OPEN INPUT ARCHIVO-TERCEROS
                      ARCHIVO-UNID-SERV
                      ARCHIVO-PROFESIONALES
                      ARCHIVO-ESPECIALIDAD
                      HISTORIA-CLINICA.

       ABRIR-DETALLE.
           MOVE COD-PACI TO ID-EDIT
           IF ID2-EDIT = "1" OR "2" OR "3" OR "4" OR "5" OR
                          "6" OR "7" OR "8" OR "9" OR "0"
              CONTINUE
           ELSE
              MOVE "0" TO ID2-EDIT
           END-IF

           INSPECT NOM-HCDET-W REPLACING FIRST "X" BY ID2-EDIT
           OPEN INPUT HISTORIA-DETALLADA.

       LEER-ADMIN.
           MOVE ID-REST  TO COD-PROF
           READ ARCHIVO-PROFESIONALES WITH NO LOCK
                INVALID KEY
                          MOVE 0 TO ATIENDE-PROF
                          MOVE COD-PROF TO DESCRIP-PROF
           END-READ.


           IF ATIENDE-PROF = "B"
              MOVE ID-REST TO COD-TER
              READ ARCHIVO-TERCEROS WITH NO LOCK
                   INVALID KEY
                           INITIALIZE DATOS-ADMIN-W REG-TERCERO                                       
              END-READ
              MOVE ENTIDAD-TER TO ENTIDAD-ADMIN-W
           END-IF.



       BUSCAR-MOVIMIENTO.
           INITIALIZE FOLIO-HC
           MOVE COD-PACI          TO ID-HC
           START HISTORIA-CLINICA KEY = ID-HC
                 INVALID KEY  GO TO HISTORIA-NUEVA
           END-START.  
     
       CREAR-PLANO.
           MOVE "{*HC*:["   TO DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO
           MOVE 0 TO SW-FIN.

       LEER-HISTORIA-CLINICA.
           IF SW-FIN = 0
             MOVE 1 TO SW-FIN
           ELSE
             MOVE LIN-01 TO DATOS-PLANO-W
             PERFORM DATOS-ENVIO
           END-IF      

           READ HISTORIA-CLINICA NEXT WITH NO LOCK AT END GO TO CERRAR-PLANO.

           IF ID-HC = COD-PACI
              CONTINUE
           ELSE
              GO TO HISTORIA-NUEVA
           END-IF

           IF ATIENDE-PROF = "B"
              MOVE ID-HC TO COD-TER
              READ ARCHIVO-TERCEROS WITH NO LOCK
                   INVALID KEY INITIALIZE REG-TERCERO
              END-READ

              IF ENTIDAD-TER = ENTIDAD-ADMIN-W
                 CONTINUE
              ELSE
                 GO TO LEER-HISTORIA-CLINICA
              END-IF
           END-IF

           IF TEMPORAL-HC = 1
              MOVE 0 TO ESTADO-HC
           END-IF

           IF ESTADO-HC = 1 AND CLASE-HC = "B"
              IF SERV-HC = 08
                 ADD 1 TO SW-PYP
              ELSE
                 ADD 1 TO SW-BLANCA
              END-IF
           ELSE
              IF SW-OPEN = 0 AND ESTADO-HC < 2
                 MOVE 1 TO SW-OPEN
              END-IF
           END-IF

           MOVE ID-HC           TO ID-J
           MOVE ESTADO-HC       TO ESTADO-J
           
           PERFORM EVALUAR-ESTADO-HC
           
           MOVE FOLIO-SUC-HC    TO FOLIO-SUC-J
           MOVE FOLIO-NRO-HC    TO FOLIO-NRO-J

           IF UNSERV-HC = 00
              MOVE SERV-HC TO UNSERV-HC
           END-IF

           MOVE SERV-HC TO SERV-J
           MOVE UNSERV-HC TO UNSERV-J
           MOVE UNSERV-HC       TO CODIGO-UNID-SERV
           READ ARCHIVO-UNID-SERV WITH NO LOCK
                INVALID KEY 
                        MOVE "No existe unidad de servicio" TO DESCRIP-UNID-SERV
           END-READ
           
           IF CLASE-HC = 3
              MOVE "PERINATAL" TO MOTIVO1-EDIT
              MOVE MOTIV-HC-1  TO MOTIVO2-EDIT
           ELSE
              EVALUATE ESQUEMA-HC
                  WHEN "AI01" MOVE "AIEPI"    TO MOTIVO1-EDIT
                              MOVE MOTIV-HC-1 TO MOTIVO2-EDIT
                              IF (NIT-USU = 900005594 OR 892000458)
                                 ADD 3 TO DIA-LIMI
                              ELSE
                                 IF (CLASE-HC = 1 OR 2 OR "O")   
                                    MOVE FECHA-HC TO FECHA-LIMI
                                    ADD 1 TO DIA-LIMI
                                 ELSE
                                    PERFORM CALCULAR-FECHA-LIMITE
                                 END-IF
                              END-IF         
                  WHEN "AI02" MOVE "AIEPI"    TO MOTIVO1-EDIT
                              MOVE MOTIV-HC-1 TO MOTIVO2-EDIT
                  WHEN "8001" 
                              INITIALIZE REG-DETHC
                              MOVE 8001     TO COD-DETHC
                              PERFORM LEER-HISTORIA-DETALLADA
                              MOVE DETALLE-DETHC TO DATO-8001

                              IF TIPO-8001 = 1
                                 MOVE "CITOLOGIA PRIMERA VEZ"  TO MOTIVO-EDIT
                              ELSE
                                 MOVE "CONTROL DE CITOLOGIA "  TO MOTIVO-EDIT
                              END-IF
                  WHEN "8002" MOVE "CITOLOGIA BETHESDA"         TO MOTIVO-EDIT
                  WHEN "8031" 
                              INITIALIZE REG-DETHC
                              MOVE 8031     TO COD-DETHC
                              PERFORM LEER-HISTORIA-DETALLADA
                              MOVE DETALLE-DETHC TO DATO-8031

                              IF TIPO-8031 = 1
                                 MOVE "PLANIFICACION FAMILIAR" TO MOTIVO-EDIT
                              ELSE
                                 MOVE "CONTROL DE PLANIFICACION FAMILIAR" TO MOTIVO-EDIT
                              END-IF
                 WHEN "8051"  MOVE "EVALUACION ADULTO JOVEN"    TO MOTIVO-EDIT
                 WHEN OTHER   MOVE MOTIV-HC-1                   TO MOTIVO-EDIT
              END-EVALUATE
           END-IF

           MOVE MOTIVO-EDIT   TO MOTIV-J

           MOVE MED-HC        TO COD-PROF
           READ ARCHIVO-PROFESIONALES WITH NO LOCK
                INVALID KEY MOVE COD-PROF TO DESCRIP-PROF
           END-READ
           MOVE DESCRIP-PROF  TO MED-J

           MOVE ESP-PROF (1)  TO COD-ESP
           READ ARCHIVO-ESPECIALIDAD WITH NO LOCK
                INVALID KEY MOVE SPACES  TO NOMBRE-ESP
           END-READ
           MOVE NOMBRE-ESP    TO ESPEC-J

           IF NIT-USU = 891855847 AND ESPEC-J IS NOT = SPACES
               MOVE NOMBRE-ESP   TO DETALLE-J
           ELSE
               MOVE DESCRIP-PROF TO DETALLE-J
           END-IF

           MOVE DESCRIP-UNID-SERV    TO NOM-SERV-J

           MOVE ANO-HC          TO ANO-J
           MOVE MES-HC          TO MES-J
           MOVE DIA-HC          TO DIA-J

           MOVE HR-HC           TO HR-J
           MOVE MN-HC           TO MN-J

           MOVE UND-PESO-HC        TO UND-PESO-HC-J
           MOVE PESO-HC            TO PESO-HC-J
           MOVE TALLA-HC           TO TALLA-HC-J
           MOVE PER-CEF-HC         TO PER-CEF-HC-J
           MOVE PER-TORA-HC        TO PER-TORA-HC-J
           MOVE PER-ABDO-HC        TO PER-ABDO-HC-J
           MOVE NIT-FACT-HC          TO NIT-FACT-J
           MOVE FACT-HC              TO FACT-J
           MOVE HAB-HC               TO HAB-J



           MOVE DIA-EGRES-HC         TO DIA-EGRES-J
           MOVE MES-EGRES-HC         TO MES-EGRES-J
           MOVE ANO-EGRES-HC         TO ANO-EGRES-J

           MOVE DIAG-MUER-HC         TO DIAG-MUER-J
           MOVE OPER-CIE-HC          TO OPER-CIE-J
           MOVE OPER-ORD-SALIDA-HC   TO OPER-ORD-SALIDA-J

           MOVE DIA-ORD-SALIDA-HC    TO DIA-ORD-SALIDA-J
           MOVE MES-ORD-SALIDA-HC    TO MES-ORD-SALIDA-J
           MOVE ANO-ORD-SALIDA-HC    TO ANO-ORD-SALIDA-J


           MOVE HR-ORD-SALIDA-HC     TO HR-ORD-SALIDA-J
           MOVE MN-ORD-SALIDA-HC     TO MN-ORD-SALIDA-J

           MOVE "," TO CIERRE-LIN-01
           INSPECT LIN-01  REPLACING ALL "*" BY CARAC-COMILLA
           GO TO LEER-HISTORIA-CLINICA.

         
       HISTORIA-NUEVA.
           IF  SW-OPEN = 0
               IF (SW-PYP > 0 AND SW-BLANCA > 0)
                   MOVE 1 TO SW-OPEN
               ELSE
                   IF  (SW-PYP > 1 OR SW-BLANCA > 1)
                        MOVE 1 TO SW-OPEN
                   END-IF
               END-IF
           END-IF.

           IF NIT-USU = 900565371 OR 900566047 OR 900658867
                     OR 900541158 OR 900435177 OR 830511298
              MOVE 0 TO SW-OPEN
           ELSE
             IF SW-OPEN = 1
                 GO TO CERRAR-ARCHIVOS
              END-IF
           END-IF

           MOVE ID-W       TO ID-HC
           MOVE SUCURSAL-W TO FOLIO-SUC-HC
           MOVE 999999     TO FOLIO-NRO-HC



           START HISTORIA-CLINICA KEY <= LLAVE-HC
                 INVALID KEY
                    MOVE ID-W       TO ID-HC
                    MOVE SUCURSAL-W TO FOLIO-SUC-HC
                    MOVE 1          TO FOLIO-NRO-HC
                 NOT INVALID KEY
                       READ HISTORIA-CLINICA PREVIOUS END-READ

                    INITIALIZE DATOS-HC
                    IF ID-HC = ID-W
                       IF FOLIO-SUC-HC = SUCURSAL-W
                          ADD 1 TO FOLIO-NRO-HC
                       ELSE
                          MOVE SUCURSAL-W TO FOLIO-SUC-HC
                          MOVE 1          TO FOLIO-NRO-HC
                       END-IF
                    ELSE
                       MOVE ID-W       TO ID-HC
                       MOVE SUCURSAL-W TO FOLIO-SUC-HC
                       MOVE 1          TO FOLIO-NRO-HC
                    END-IF
           END-START.

       CERRAR-PLANO.
           INITIALIZE LIN-01
008890     MOVE LIN-01 TO DATOS-PLANO-W
008900     PERFORM DATOS-ENVIO.
008910
008920     MOVE "]}" TO DATOS-PLANO-W
008930     PERFORM DATOS-ENVIO.    

       CERRAR-ARCHIVOS.
           CLOSE ARCHIVO-PACIENTES
                 ARCHIVO-UNID-SERV
                 HISTORIA-CLINICA.

010700 PAGINA-CONFIG.
010720     MOVE "statuscode" TO COBW3-CNV-NAME
010730     MOVE "00"         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010720     MOVE "programa-id" TO COBW3-CNV-NAME
010730     MOVE "HC811"      TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010760     MOVE "..\PAGINAS\RECIBIDOSLISTADO.ASPX" TO SALIDA-HTML
010770     PERFORM ABRIR-HTML.
007980   
007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.

       DATOS-ENVIO.
010720     MOVE "datosrecibidos" TO COBW3-CNV-NAME
010730     MOVE DATOS-PLANO-W    TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_REPEAT" USING COBW3.

       LEER-HISTORIA-DETALLADA.

           MOVE LLAVE-HC TO LLAVE-HC-DETHC
           READ HISTORIA-DETALLADA WITH NO LOCK
                INVALID KEY INITIALIZE DETALLE-DETHC
           END-READ.

       BUSCAR-OPER.

           OPEN INPUT ARCHIVO-RESTRICCION
           IF ADMIN-W = "GEBC"
              MOVE "Prosoft E.A.T"  TO NOMBRE-REST
              MOVE 0017329215       TO ID-REST
           ELSE
              IF ADMIN-W = SPACES
                 MOVE SPACES TO NOMBRE-REST
                 MOVE 0      TO ID-REST
              ELSE
                 MOVE ADMIN-W TO COD-REST
                 READ ARCHIVO-RESTRICCION WITH NO LOCK
                      INVALID KEY
                              MOVE COD-REST TO NOMBRE-REST
                 END-READ
              END-IF
           END-IF
           CLOSE ARCHIVO-RESTRICCION.
           
       EVALUAR-ESTADO-HC.
           IF TEMPORAL-HC = "1"
              MOVE "TEMPORAL" TO ESTADO-HC-J
           ELSE   
               EVALUATE ESTADO-J
                   WHEN '0' MOVE "NO HAY H.C. " TO ESTADO-HC-J
                   WHEN '1' MOVE "H.C. ABIERTA" TO ESTADO-HC-J
                   WHEN '2' MOVE "H.C. CERRADA" TO ESTADO-HC-J
               END-EVALUATE
            END-IF.

       EDITAR-FECHA.
           EVALUATE MES-HC
                    WHEN 01 MOVE "ENE "     TO MES-EDIT
                    WHEN 02 MOVE "FEB "     TO MES-EDIT
                    WHEN 03 MOVE "MAR "     TO MES-EDIT
                    WHEN 04 MOVE "ABR "     TO MES-EDIT
                    WHEN 05 MOVE "MAY "     TO MES-EDIT
                    WHEN 06 MOVE "JUN "     TO MES-EDIT
                    WHEN 07 MOVE "JUL "     TO MES-EDIT
                    WHEN 08 MOVE "AGT "     TO MES-EDIT
                    WHEN 09 MOVE "SEP "     TO MES-EDIT
                    WHEN 10 MOVE "OCT "     TO MES-EDIT
                    WHEN 11 MOVE "NOV "     TO MES-EDIT
                    WHEN 12 MOVE "DIC "     TO MES-EDIT
           END-EVALUATE.

        CALCULAR-FECHA-LIMITE.
           IF MES-EGRES-HC IS ZERO
              IF ESTADO-HC < 2
                 MOVE FECHA-SIG-W TO FECHA-LIMI
              ELSE
                 MOVE FECHA-HC TO FECHA-LIMI
              END-IF
           ELSE
              MOVE EGRESO-HC TO FECHA-LIMI
              EVALUATE MES-LIMI
              WHEN 01 MOVE 31 TO ULT-DIA-W
              WHEN 02 MOVE 28 TO ULT-DIA-W
              WHEN 03 MOVE 31 TO ULT-DIA-W
              WHEN 04 MOVE 30 TO ULT-DIA-W
              WHEN 05 MOVE 31 TO ULT-DIA-W
              WHEN 06 MOVE 30 TO ULT-DIA-W
              WHEN 07 MOVE 31 TO ULT-DIA-W
              WHEN 08 MOVE 31 TO ULT-DIA-W
              WHEN 09 MOVE 30 TO ULT-DIA-W
              WHEN 10 MOVE 31 TO ULT-DIA-W
              WHEN 11 MOVE 30 TO ULT-DIA-W
              WHEN 12 MOVE 31 TO ULT-DIA-W
              END-EVALUATE    .

008040 COPY "P:\PROG\PROYECT\FUENTES\SC-WEB19.CBL".

