      *============================================================
      * CREACION DE ARCHIVO JSON SAL719-01  SALUD
      *============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL". 
       COPY "..\..\FUENTES\FS-PROFE.CBL".
 



           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN TO USUAR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-TERCEROS  LOCK MODE IS AUTOMATIC
                  ASSIGN TERCEROS-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-TERCERO
                  ALTERNATE RECORD KEY DESCRIP-TER WITH DUPLICATES
                  ALTERNATE RECORD KEY ACT-TER     WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.
           
           SELECT ARCHIVO-MAESTROS LOCK MODE IS AUTOMATIC
                  ASSIGN MAESTROS-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-MAE
                  ALTERNATE RECORD KEY IS NOMBRE-MAE    WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CTA-INTER-MAE WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

      *      SELECT ARCHIVO-PROFESIONALES LOCK MODE IS AUTOMATIC
      *             ASSIGN PROFESIONALES-W
      *             ORGANIZATION IS INDEXED
      *             ACCESS MODE  IS DYNAMIC
      *             RECORD KEY   IS COD-PROF
      *             ALTERNATE RECORD KEY DESCRIP-PROF WITH DUPLICATES
      *             FILE STATUS IS OTR-STAT.
                                                                        
      ** "\PROG\DATOS\SC-ARCHESP.DAT"
            SELECT ARCHIVO-ESPECIALIDAD LOCK MODE IS AUTOMATIC
	               ASSIGN ESPECIALIDAD-W;
                   ORGANIZATION IS INDEXED
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS COD-ESP
                   ALTERNATE RECORD KEY IS NOMBRE-ESP WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.

      * "../control/SC-DIVIS.DAT"
            SELECT ARCHIVO-DIVISION LOCK MODE IS AUTOMATIC
                   ASSIGN DIVISION-W
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC;
                   RECORD KEY IS LLAVE-DIV
                   ALTERNATE RECORD KEY IS DESCRIP-DIV WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.

      *"\PROG\HC\DATOS\SC-HORAR.DAT"
            SELECT ARCHIVO-HORARIO-PROF LOCK MODE IS AUTOMATIC
                   ASSIGN HORARIO-W
                   ORGANIZATION IS INDEXED
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS LLAVE-HOR-P
                   ALTERNATE RECORD KEY FECHA-HOR-P WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.

            SELECT ARCHIVO-CITAS LOCK MODE IS AUTOMATIC
                   ASSIGN CITAS-W
                   ORGANIZATION INDEXED
                   ACCESS MODE  DYNAMIC;
                   RECORD KEY LLAVE-CIT
				   ALTERNATE RECORD KEY IS FECHA-CIT     WITH DUPLICATES
				   ALTERNATE RECORD KEY IS PACI-CIT      WITH DUPLICATES
                   ALTERNATE RECORD KEY IS LLAVE-ALT-CIT WITH DUPLICATES
                   ALTERNATE RECORD KEY IS FECHA-FACTU-CIT WITH DUPLICATES
				   ALTERNATE RECORD KEY IS FECHA-SOLIC-CIT WITH DUPLICATES
                   FILE STATUS IS OTR-STAT.

      *"P:\PROG\DATOS\SC-SUCUR.DAT"
            SELECT ARCHIVO-SUCUR LOCK MODE IS AUTOMATIC
                   ASSIGN SUCURSAL-W
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC;
                   RECORD KEY IS COD-SUCUR
                   FILE STATUS IS OTR-STAT.

                                                          
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-TERCE.CBL".
       COPY "..\..\FUENTES\FD-MAEST.CBL".
       COPY "..\..\FUENTES\FD-PROFE.CBL".
       COPY "..\..\FUENTES\FD-ESPEC.CBL".
       COPY "..\..\FUENTES\FD-DIVIS.CBL".
       COPY "..\..\FUENTES\FD-HORAR.CBL".
       COPY "..\..\FUENTES\FD-CITAS.CBL".
       COPY "..\..\FUENTES\FD-SUCUR.CBL".

000470
000540 WORKING-STORAGE  SECTION.
000550 COPY "..\..\FUENTES\COBW3.CBL".
000560 COPY "..\..\FUENTES\WEB-CARAC19.CBL".

       77 NOM-PLANO-W               PIC x(90).
       77 USUAR-W                   PIC X(60).
       77 TERCEROS-W                PIC X(60).
       77 MAESTROS-W                PIC X(60).
       77 PROFESIONALES-W           PIC X(60).
       77 ESPECIALIDAD-W            PIC X(60).
       77 DIVISION-W                PIC X(60).
       77 HORARIO-W                 PIC X(60).
       77 CITAS-W                   PIC X(60).
       77 SUCURSAL-W                PIC X(60).

       77 DATOS-PLANO-W             PIC X(200). 

       77 ESP-W                     PIC X(3).
       77 NOMBRE-ESP-W              PIC X(70).
 
       77 MES-SAL-W                 PIC X(4).

       01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).           
           02 DIR-CONTAB-LLEGADA-W      PIC X(30).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 OPERADOR-LLEGADA-W        PIC X(4).
           02 CEDULA-LLEGADA-W          PIC X(10).
        
       01 DATO-IMPORTAR-WK              PIC X(100). 
        
       01 LIN-1.  
          02 FILLER                   PIC X(11)  VALUE "{*DESCRIP*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DESCRIP-PERATEN-J        PIC X(30).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)  VALUE "*DETALLE*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DETALLE-PERATEN-J        PIC X(30).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*REGISTRO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 REGIS-PERATEN-J          PIC X(10).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*MAYORRET*:".
          02 FILLER                   PIC X      VALUE "*".
          02 MAYOR-RET-PERATEN-J      PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)  VALUE "*ATIENDE*:".
          02 FILLER                   PIC X      VALUE "*".
          02 ATIEN-PERATEN-J          PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*CONTRATO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CONTRATO-PERATEN-J       PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)   VALUE "*PORCENT*:".
          02 FILLER                   PIC X      VALUE "*".
          02 PORCENT-PERATEN-J        PIC X(3).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*ESTADO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 ESTADO-PERATEN-J         PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*CTARET*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CTARET-PERATEN-J         PIC X(11).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*NOMCTA*:".
          02 FILLER                   PIC X      VALUE "*".
          02 NOMCTA-PERATEN-J         PIC X(30).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*DIVISION*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DIV-PERATEN-J            PIC XX.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)  VALUE "*DESCDIV*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DESCDIV-PERATEN-J        PIC X(20).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*SUCURSAL*:".
          02 FILLER                   PIC X      VALUE "*".
          02 SUCUR-PERATEN-J          PIC X(2).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)  VALUE "*DESCSUC*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DESCSUC-PERATEN-J        PIC X(20).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(6)   VALUE "*CL1*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CL1-PERATEN-J            PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(6)   VALUE "*CL2*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CL2-PERATEN-J            PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(6)   VALUE "*CL3*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CL3-PERATEN-J            PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(6)   VALUE "*CL4*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CL4-PERATEN-J            PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(6)   VALUE "*CL5*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CL5-PERATEN-J            PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(6)   VALUE "*CL6*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CL6-PERATEN-J            PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(6)   VALUE "*CL7*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CL7-PERATEN-J            PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(7)   VALUE "*ESP1*:".
          02 FILLER                   PIC X      VALUE "*".
          02 ESPECIALIDAD1-PERATEN-J  PIC X(3).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*DESCESP1*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DESCESPEC1-PERATEN-J     PIC X(70).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(7)   VALUE "*ESP2*:".
          02 FILLER                   PIC X      VALUE "*".
          02 ESPECIALIDAD2-PERATEN-J  PIC X(3).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*DESCESP2*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DESCESPEC2-PERATEN-J     PIC X(70).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(7)   VALUE "*ESP3*:".
          02 FILLER                   PIC X      VALUE "*".
          02 ESPECIALIDAD3-PERATEN-J  PIC X(3).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*DESCESP3*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DESCESPEC3-PERATEN-J     PIC X(70).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*ESP4*:".
          02 FILLER                   PIC X      VALUE "*".
          02 ESPECIALIDAD4-PERATEN-J  PIC X(3).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*DESCESP4*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DESCESPEC4-PERATEN-J     PIC X(70).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*ESP5*:".
          02 FILLER                   PIC X      VALUE "*".
          02 ESPECIALIDAD5-PERATEN-J  PIC X(3).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*DESCESP5*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DESCESPEC5-PERATEN-J     PIC X(70).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)  VALUE "*OPERAUT*:".
          02 FILLER                   PIC X      VALUE "*".
          02 OPERAUT-PERATEN-J        PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*OPERCIRU*:".
          02 FILLER                   PIC X      VALUE "*".
          02 OPERCIRU-PERATEN-J       PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)  VALUE "*OPEROTR*:".
          02 FILLER                   PIC X      VALUE "*".
          02 OPEROTR-PERATEN-J        PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*OPERAUT4*:".
          02 FILLER                   PIC X      VALUE "*".   
          02 OPERAUT4-PERATEN-J       PIC X(4). 
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*OPERAUT5*:".
          02 FILLER                   PIC X      VALUE "*".
          02 OPERAUT5-PERATEN-J       PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*INTMIN*:".
          02 FILLER                   PIC X      VALUE "*".
          02 INTMIN-PERATEN-J         PIC XX.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(8)   VALUE "*CITAS*:".
          02 FILLER                   PIC X      VALUE "*".   
          02 CITAS-PERATEN-J          PIC X(3).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*FORMAGEN*:".
          02 FILLER                   PIC X      VALUE "*".  
          02 FORMAGEN-PERATEN-J       PIC X.  
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*SOBREAGEN*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 SOBREAGEN-PERATEN-J      PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(8)   VALUE "*RANGO*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 RANGO-PERATEN-J          PIC XX.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(7)   VALUE "*OPER*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 OPER-PERATEN-J           PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(8)   VALUE "*FECHA*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 FECHA-PERATEN-J          PIC X(6).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*ANOINI*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 ANOINI-PERATEN-J         PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*MESINI*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 MESINI-PERATEN-J         PIC XX.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*DIAINI*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 DIAINI-PERATEN-J         PIC XX.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(8)   VALUE "*HRINI*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 HRINI-PERATEN-J          PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*ANOFIN*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 ANOFIN-PERATEN-J         PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*MESFIN*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 MESFIN-PERATEN-J         PIC XX.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*DIAFIN*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 DIAFIN-PERATEN-J         PIC XX.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(8)   VALUE "*HRFIN*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 HRFIN-PERATEN-J          PIC X(4). 
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(11)  VALUE "*DATO_DVD*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 IMP-DVD-PERATEN-J        PIC X. 
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(13)  VALUE "*DATO_BIRAD*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 IMP-BIRAD-PERATEN-J      PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*DATO_NORM*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 IMP-NORM-PERATEN-J       PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(13)  VALUE "*DATO_ASOCI*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 ASOC-PROF-PERATEN-J      PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(9)   VALUE "*TABLA*:[". 
         


       01 LIN-2.
          02 FILLER                   PIC X(12)  VALUE "{*HORAING1*:".
          02 FILLER                   PIC X      VALUE "*".
          02 HORAING1-PERATEN-J       PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*HORARET1*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 HORARET1-PERATEN-J       PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(13)  VALUE "*INTMINTAB1*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 INTMIN1-PERATEN-J        PIC XX.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*HORAING2*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 HORAING2-PERATEN         PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*HORARET2*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 HORARET2-PERATEN-J       PIC X(4).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(13)  VALUE "*INTMINTAB2*:".
          02 FILLER                   PIC X      VALUE "*".    
          02 INTMIN2-PERATEN-J        PIC XX.    
          02 FILLER                   PIC XX   VALUE "*}".    
          02 CIERRE-LIN-2             PIC X.

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
              MOVE "SAL719-01"                 TO MSJ3-HTML
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
              MOVE "SAL719-01"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USUARIOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE USUAR-W              TO MSJ2-HTML
              MOVE "SAL719-01"           TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  


       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-MAESTROS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE MAESTROS-W       TO MSJ2-HTML
              MOVE "SAL719-01"      TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PROFESIONALES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-PROF-LNK          TO MSJ2-HTML
              MOVE "SAL719-01"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-ESPECIALIDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE ESPECIALIDAD-W          TO MSJ2-HTML
              MOVE "SAL719-01"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-DIVISION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE DIVISION-W          TO MSJ2-HTML
              MOVE "SAL719-01"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CITAS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE CITAS-W              TO MSJ2-HTML
              MOVE "SAL719-01"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.  

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SUCUR.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT            TO MSJ1-HTML
              MOVE SUCURSAL-W          TO MSJ2-HTML
              MOVE "SAL719-01"         TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO DATO-IMPORTAR-WK.

           UNSTRING DATO-IMPORTAR-WK DELIMITED BY "|" 
             INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W, CEDULA-LLEGADA-W
           END-UNSTRING.

           MOVE LLAVESESION-LLEGA-W    TO LLAVE-SESION-W.


       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W


           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.
       
       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W           


           OPEN INPUT ARCHIVO-USUNET
           EVALUATE OTR-STAT
             WHEN "00"  CONTINUE
             WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
                        CLOSE       ARCHIVO-USUNET
                        OPEN I-O    ARCHIVO-USUNET
                        INITIALIZE REG-USUNET
                        MOVE "." TO NOMBRE-USUNET
                        WRITE REG-USUNET
                        CLOSE       ARCHIVO-USUNET
                        MOVE "Validacion de usuarios"      TO MSJ1-HTML
                        MOVE "Falta configurar usuario"    TO MSJ2-HTML
                        MOVE "Sc"                          TO MSJ3-HTML
                        GO TO ENVIAR2-ERROR
             WHEN OTHER GO TO CERRAR-SESION
           END-EVALUATE.                          
      
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
           INITIALIZE USUAR-W
                      TERCEROS-W
                      MAESTROS-W
                      NOM-PROF-LNK
                      ESPECIALIDAD-W
                      DIVISION-W
                      HORARIO-W
                      CITAS-W
                      SUCURSAL-W
                      


           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

           MOVE "\\" TO USUAR-W 

           INSPECT USUAR-W REPLACING FIRST "                    "
                                        BY IP-DATOS-USUNET


           INSPECT USUAR-W REPLACING FIRST " "
                                        BY "\"



           MOVE USUAR-W TO SUCURSAL-W HORARIO-W ESPECIALIDAD-W

           INSPECT SUCURSAL-W REPLACING FIRST "                       "
                                           BY "PROG\DATOS\SC-SUCUR.DAT".

           INSPECT HORARIO-W REPLACING FIRST "                           " 
                                          BY "\PROG\HC\DATOS\SC-HORAR.DAT".

           INSPECT ESPECIALIDAD-W REPLACING FIRST "                          " 
                                               BY "\PROG\DATOS\SC-ARCHESP.DAT".

      *     MOVE USUAR-W TO CITAS-W

      *     INSPECT CITAS-W REPLACING FIRST "                           " 
      *                                  BY "\PROG\HC\DATOS\SC-AGEND.DAT".

           INSPECT USUAR-W REPLACING FIRST "                              "
                                        BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT USUAR-W REPLACING FIRST "         "
                                        BY "\CONTROL\".

           MOVE USUAR-W TO TERCEROS-W MAESTROS-W NOM-PROF-LNK DIVISION-W
           

           INSPECT USUAR-W REPLACING FIRST "              "
                                        BY "SC-ARCHUSU.DAT".

           INSPECT TERCEROS-W REPLACING FIRST "              "
                                           BY "SC-ARCHTER.DAT".

           INSPECT MAESTROS-W REPLACING FIRST "              "
                                           BY "SC-ARCHMAE.DAT".

           INSPECT NOM-PROF-LNK REPLACING FIRST "               "
                                                BY "SC-ARCHPROF.DAT".

           INSPECT DIVISION-W REPLACING FIRST "            "
                                           BY "SC-DIVIS.DAT".




       ABRIR-ARCHIVOS.

           OPEN INPUT ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT AT END MOVE 0 TO OPCION-1.
           CLOSE ARCHIVO-USUARIOS.

           IF (PUC-USU = 4 OR 6)
              MOVE 2436  TO MAYOR-RET-PERATEN-J
           ELSE
              MOVE 2365  TO MAYOR-RET-PERATEN-J
           END-IF.


           OPEN INPUT  ARCHIVO-TERCEROS
                       ARCHIVO-MAESTROS
                       ARCHIVO-PROFESIONALES
                       ARCHIVO-ESPECIALIDAD
                       ARCHIVO-DIVISION
                       ARCHIVO-HORARIO-PROF
                       ARCHIVO-SUCUR.



       ABRIR-JSON-PERSATI.
      *     OPEN INPUT ARCHIVO-PROFESIONALES.
           INITIALIZE REG-PROF
           MOVE "{*PERSATI*:[" TO  DATOS-PLANO-W   
           INSPECT DATOS-PLANO-W  REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM ENVIO-DATOS
           MOVE 0 TO SW-FIN.

       LEER-PERSONAL-ATIENDE.
            MOVE CEDULA-LLEGADA-W TO COD-PROF
            READ ARCHIVO-PROFESIONALES WITH NO LOCK
                 INVALID KEY MOVE SPACES TO DESCRIP-PROF
            END-READ.


           MOVE DESCRIP-PROF    TO DESCRIP-PERATEN-J
           MOVE DETALLE-PROF    TO DETALLE-PERATEN-J
           MOVE REG-MED-PROF    TO REGIS-PERATEN-J
           MOVE ATIENDE-PROF    TO ATIEN-PERATEN-J
           MOVE CONTRATO-P      TO CONTRATO-PERATEN-J
           MOVE PORCENT-P       TO PORCENT-PERATEN-J
           MOVE ESTADO-PROF     TO ESTADO-PERATEN-J

           MOVE CTA-RET-PROF    TO CTARET-PERATEN-J
           IF CTARET-PERATEN-J = SPACES   
              MOVE SPACES TO NOMCTA-PERATEN-J
           ELSE 
              MOVE CTARET-PERATEN-J TO CTA-MAE
              MOVE 4         TO TIPO-MAE
              READ ARCHIVO-MAESTROS WITH NO LOCK
                   INVALID KEY MOVE SPACES TO NOMBRE-MAE
              END-READ
              MOVE NOMBRE-MAE      TO NOMCTA-PERATEN-J
           END-IF.

           MOVE DIV-PROF        TO DIV-PERATEN-J
           IF DIV-PERATEN-J = SPACES  
              MOVE SPACES TO DESCDIV-PERATEN-J
           ELSE  
              MOVE DIV-PERATEN-J TO COD-DIV
              READ ARCHIVO-DIVISION WITH NO LOCK
                  INVALID KEY MOVE SPACES TO DESCDIV-PERATEN-J
              END-READ
              MOVE DESCRIP-DIV TO DESCDIV-PERATEN-J
           END-IF.

           MOVE SUCURSAL-PROF TO SUCUR-PERATEN-J
           IF SUCUR-PERATEN-J = SPACES  
             MOVE SPACES TO DESCSUC-PERATEN-J
           ELSE  
             MOVE SUCUR-PERATEN-J TO COD-SUCUR
             READ ARCHIVO-SUCUR WITH NO LOCK
                 INVALID KEY MOVE SPACES TO DESCSUC-PERATEN-J
             END-READ
             MOVE DESCRIP-SUCUR TO DESCSUC-PERATEN-J
           END-IF.

           MOVE CL-PROF(1) TO CL1-PERATEN-J
           MOVE CL-PROF(2) TO CL2-PERATEN-J
           MOVE CL-PROF(3) TO CL3-PERATEN-J
           MOVE CL-PROF(4) TO CL4-PERATEN-J
           MOVE CL-PROF(5) TO CL5-PERATEN-J
           MOVE CL-PROF(6) TO CL6-PERATEN-J
           MOVE CL-PROF(7) TO CL7-PERATEN-J

           MOVE ESP-PROF(1) TO ESP-W
           PERFORM MOSTRAR-ESP
           MOVE ESP-W TO ESPECIALIDAD1-PERATEN-J
           MOVE NOMBRE-ESP-W TO DESCESPEC1-PERATEN-J
          
           MOVE ESP-PROF(2) TO ESP-W
           PERFORM MOSTRAR-ESP
           MOVE ESP-W TO ESPECIALIDAD2-PERATEN-J
           MOVE NOMBRE-ESP-W TO DESCESPEC2-PERATEN-J

           MOVE ESP-PROF(3) TO ESP-W
           PERFORM MOSTRAR-ESP
           MOVE ESP-W TO ESPECIALIDAD3-PERATEN-J
           MOVE NOMBRE-ESP-W TO DESCESPEC3-PERATEN-J

           MOVE ESP-PROF(4) TO ESP-W
           PERFORM MOSTRAR-ESP
           MOVE ESP-W TO ESPECIALIDAD4-PERATEN-J
           MOVE NOMBRE-ESP-W TO DESCESPEC4-PERATEN-J

           MOVE ESP-PROF(5) TO ESP-W
           PERFORM MOSTRAR-ESP
           MOVE ESP-W TO ESPECIALIDAD5-PERATEN-J
           MOVE NOMBRE-ESP-W TO DESCESPEC5-PERATEN-J


           IF OPER-AUT-PROF = SPACES 
              MOVE "XXXX" TO OPERAUT-PERATEN-J
           ELSE
              MOVE OPER-AUT-PROF TO OPERAUT-PERATEN-J
           END-IF.

           IF  OPER-AUT-CIRU-PROF = SPACES  
              MOVE "XXXX" TO OPERCIRU-PERATEN-J
           ELSE   
              MOVE OPER-AUT-CIRU-PROF TO OPERCIRU-PERATEN-J
           END-IF.

           IF OPER-AUT-OTRO-PROF = SPACES  
              MOVE "XXXX" TO OPEROTR-PERATEN-J
           ELSE
              MOVE OPER-AUT-OTRO-PROF TO OPEROTR-PERATEN-J
           END-IF.

           IF OPER-AUT-4-PROF = SPACES  
              MOVE "XXXX" TO OPERAUT4-PERATEN-J
           ELSE
              MOVE OPER-AUT-4-PROF TO OPERAUT4-PERATEN-J
           END-IF.

           IF OPER-AUT-5-PROF = SPACES  
              MOVE "XXXX" TO OPERAUT5-PERATEN-J
           ELSE
              MOVE OPER-AUT-5-PROF TO OPERAUT5-PERATEN-J
           END-IF.

           MOVE INT-MIN-P TO INTMIN-PERATEN-J
           MOVE CANTD-CITAS-PROF TO CITAS-PERATEN-J

           MOVE FORMA-AGE-PROF TO FORMAGEN-PERATEN-J
           IF FORMAGEN-PERATEN-J = "S"
              CONTINUE
           ELSE
              MOVE "N" TO FORMAGEN-PERATEN-J
           END-IF.

           MOVE SOBREAGE-PROF TO SOBREAGEN-PERATEN-J
           MOVE RANGO-BLOQ-PROF TO RANGO-PERATEN-J
           MOVE OPER-PROF TO OPER-PERATEN-J
           MOVE FECHA-PROF TO FECHA-PERATEN-J
           MOVE ANO-INI-DES-PROF TO ANOINI-PERATEN-J
           MOVE MES-INI-DES-PROF TO MESINI-PERATEN-J
           MOVE DIA-INI-DES-PROF TO DIAINI-PERATEN-J
           MOVE HORA-INI-DES-PROF TO HRINI-PERATEN-J
           MOVE ANO-FIN-DES-PROF TO ANOFIN-PERATEN-J
           MOVE MES-FIN-DES-PROF TO MESFIN-PERATEN-J
           MOVE DIA-FIN-DES-PROF TO DIAFIN-PERATEN-J
           MOVE HORA-FIN-DES-PROF TO HRFIN-PERATEN-J
           MOVE IMP-DVD-PROF   TO IMP-DVD-PERATEN-J
           MOVE IMP-BIRAD-PROF TO IMP-BIRAD-PERATEN-J 
           MOVE IMP-NORM-PROF  TO IMP-NORM-PERATEN-J
           MOVE ASOC-PROF      TO ASOC-PROF-PERATEN-J 


           IF NIT-USU = 830092718 OR 830092719
              CONTINUE
           ELSE
              MOVE SPACES TO IMP-DVD-PERATEN-J
              MOVE SPACES TO IMP-BIRAD-PERATEN-J
              MOVE SPACES TO IMP-NORM-PERATEN-J
              MOVE SPACES TO ASOC-PROF-PERATEN-J 
           END-IF			  
		     

           INSPECT OPERAUT-PERATEN-J   REPLACING ALL "*" BY "X".
           INSPECT OPERCIRU-PERATEN-J  REPLACING ALL "*" BY "X".
           INSPECT OPEROTR-PERATEN-J   REPLACING ALL "*" BY "X".
           INSPECT OPERAUT4-PERATEN-J  REPLACING ALL "*" BY "X".
           INSPECT OPERAUT5-PERATEN-J  REPLACING ALL "*" BY "X".
		   

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA  
           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES           
              
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM ENVIO-DATOS
           INITIALIZE I

           PERFORM LLENAR-TABLA VARYING I FROM 1 BY 1
                                          UNTIL I > 7.

           GO TO CERRAR-PERSONAL-ATIENDE.


       LLENAR-TABLA.
           IF HORA-INI1-P (I) = ZERO
              IF (NIT-USU = 830092718 OR 830092719 OR 800156469)
                 MOVE HORA-INI1-P (I) TO HORAING1-PERATEN-J
                 MOVE HORA-FIN1-P (I) TO HORARET1-PERATEN-J
                 MOVE INT-MIN-TABLA (I) TO INTMIN1-PERATEN-J
                 MOVE HORA-INI2-P (I) TO HORAING2-PERATEN
                 MOVE HORA-FIN2-P (I) TO HORARET2-PERATEN-J
                 MOVE INT-MIN2-TAB-PROF (I) TO INTMIN2-PERATEN-J
               ELSE
                 MOVE SPACES TO HORAING1-PERATEN-J
                 MOVE SPACES TO HORARET1-PERATEN-J
                 MOVE SPACES TO INTMIN1-PERATEN-J
                 MOVE SPACES TO HORAING2-PERATEN
                 MOVE SPACES TO HORARET2-PERATEN-J
                 MOVE SPACES TO INTMIN2-PERATEN-J
               END-IF
           ELSE
               MOVE HORA-INI1-P (I) TO HORAING1-PERATEN-J
               MOVE HORA-FIN1-P (I) TO HORARET1-PERATEN-J
               MOVE INT-MIN-TABLA (I) TO INTMIN1-PERATEN-J
               MOVE HORA-INI2-P (I) TO HORAING2-PERATEN
               MOVE HORA-FIN2-P (I) TO HORARET2-PERATEN-J
               MOVE INT-MIN2-TAB-PROF (I) TO INTMIN2-PERATEN-J
           END-IF.
           
           IF I < 7
              MOVE "," TO CIERRE-LIN-2
           ELSE
              MOVE SPACE TO CIERRE-LIN-2
           END-IF.

           INSPECT LIN-2 REPLACING ALL "*" BY CARAC-COMILLA          
           MOVE LIN-2 TO DATOS-PLANO-W.
      *     WRITE REG-JSON END-WRITE.   

       MOSTRAR-ESP.
              MOVE ESP-W TO COD-ESP
              READ ARCHIVO-ESPECIALIDAD WITH NO LOCK
                   INVALID KEY MOVE SPACES TO NOMBRE-ESP
              END-READ
              MOVE NOMBRE-ESP TO NOMBRE-ESP-W.

       CERRAR-PERSONAL-ATIENDE.
           MOVE "]}]}" TO DATOS-PLANO-W 
           PERFORM ENVIO-DATOS
                
           CLOSE ARCHIVO-TERCEROS
                 ARCHIVO-MAESTROS
                 ARCHIVO-PROFESIONALES
                 ARCHIVO-ESPECIALIDAD
                 ARCHIVO-DIVISION
                 ARCHIVO-HORARIO-PROF
                 ARCHIVO-SUCUR.
      *           ARCHIVO-JSON.      

       PAGINA-CONFIG.  
           MOVE "statuscode"     TO COBW3-CNV-NAME
           MOVE "00"             TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"  USING COBW3

           MOVE "programa-id"    TO COBW3-CNV-NAME
           MOVE "SAL719"         TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"  USING COBW3

           MOVE "..\PAGINAS\RECIBIDOSLISTADO.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML.


       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.


       ENVIO-DATOS.
010720     MOVE "datosrecibidos"   TO COBW3-CNV-NAME
010730     MOVE DATOS-PLANO-W      TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_REPEAT" USING COBW3. 
      
      
       COPY "..\..\FUENTES\SC-WEB19.CBL".      


