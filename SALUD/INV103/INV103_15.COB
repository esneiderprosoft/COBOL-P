      *=================================================================
      * BUSQUEDA Y CONSULTA DE MAESTRO ARTICULOS 
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

           SELECT ARCHIVO-PREFIJOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PREF-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS LLAVE-PREF
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-GRUPOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-GRUP-W
      *"../control/SC-GRUPOINV.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS LLAVE-GRUPO
                  ALTERNATE RECORD KEY IS DESCRIP-GRUPO WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-USO LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-USO-W
      *"../CONTROL/SC-ARCHUSO.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS LLAVE-USO
                  ALTERNATE RECORD KEY IS DESCRIP-USO WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-TERCEROS  LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-TER-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-TERCERO
                  ALTERNATE RECORD KEY DESCRIP-TER WITH DUPLICATES
                  ALTERNATE RECORD KEY ACT-TER     WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-POLITICAS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-POL-W
      * "../CONTROL/SC-ARCHPOL.DAT"
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE DYNAMIC;
                  RECORD KEY  IS COD-POL
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-COSTO LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-COSTO-W
      * "../control/SC-ARCHCOS.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS COD-COSTO
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-MAESTROS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-MAE-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS LLAVE-MAE
                  ALTERNATE RECORD KEY IS NOMBRE-MAE    WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CTA-INTER-MAE WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-JSON LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".
       COPY "..\..\FUENTES\FD-ARTIC.CBL".
       COPY "..\..\FUENTES\FD-PREFI.CBL".
       COPY "..\..\FUENTES\FD-GRUPO.CBL".
       COPY "..\..\FUENTES\FD-USOAR.CBL".
       COPY "..\..\FUENTES\FD-TERCE.CBL".
       COPY "..\..\FUENTES\FD-POLIT.CBL".
       COPY "..\..\FUENTES\FD-COSTO.CBL".
       COPY "..\..\FUENTES\FD-MAEST.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON      PIC X(500).

       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-USUAR-W                            PIC X(70).
       77 NOM-ART-W                              PIC X(70).
       77 NOM-PREF-W                             PIC X(70).
       77 NOM-GRUP-W                             PIC X(70).
       77 NOM-USO-W                              PIC X(70).
       77 NOM-TER-W                              PIC X(70).
       77 NOM-POL-W                              PIC X(70).
       77 NOM-COSTO-W                            PIC X(70).
       77 NOM-PLANO-W                            PIC X(90).
       77 NOM-MAE-W                              PIC X(70).

       01 TABLA-DESCRIP-W.
          02 TAB-DESCRIP-W  OCCURS 20            PIC X(15).

       01 UNID-CONV-ART-EDIT                     PIC ZZZZ.ZZ.
       01 DCTO-GRAL-VTA-ART-EDIT                 PIC ZZZ.ZZ.
       01 PESO-ART-ENV-EDIT                      PIC ZZZZZZ.ZZZ. 
       01 VLR-ULT-COMPRA-EDIT                    PIC ZZZZZZZZZZZZ.ZZ.
       01 VLR-LISTA-COMP-ART-EDIT                PIC ZZZZZZZZZZZZ.ZZ.
       01 VLR-REF-ART-EDIT                       PIC ZZZZZZZZZZZZ.ZZ.
       01 VLR-RAZONABLE-ART-EDIT                 PIC ZZZZZZZZZZZZ.ZZ.
       01 VR-VENTA1-ART-EDIT                     PIC ZZZZZZZZZZZZ.ZZ.
       01 PORC-INC-COMP-ART-EDIT                 PIC ZZZ.ZZ.
       01 VLR-COMP-ALTA-ART-EDIT                 PIC ZZZZZZZZZZZZ.ZZ.                 
       01 VR-INCREM-ART-EDIT                     PIC ZZZZZZZZZZZZ.ZZ.
       01 PORC-INC-VTA-ART-EDIT                  PIC ZZZ.ZZ.
      
       01 LLEGADA-W                              PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W                      PIC X(15).
             03 FECHA-LLEGAD-W                   PIC X(8).
             03 HORA-LLEGAD-W                    PIC X(6).
          02 DIR-CONTAB-LLEGADA-W                PIC X(30).
          02 MES-CONTAB-LLEGADA-W                PIC XX.
          02 OPERADOR-LLEGADA-W                  PIC X(4).
          02 COD-ART-LLEGADA-W.
             03 LLAVE-NRO-ART-LLEGADA-W.
                05 LLAVE-GRUPO-ART-LLEGADA-W.
                   07 TIPO-ART-LLEGADA-W         PIC X.
                   07 GRUPO-ART-LLEGADA-W.
                      09 GRUPO1-ART-LLEGADA-W    PIC X.
                      09 GRUPO2-ART-LLEGADA-W    PIC X.
                05 NUMERO-ART-LLEGADA-W.
                   07 NRO1-ART-LLEGADA-W         PIC XX.
                   07 NRO2-ART-LLEGADA-W         PIC X(11).
             03 CLASE-ART-LLEGADA-W              PIC XX.
          02 COD-BARRAS-LLEGADA-W                PIC X(15).
          

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV                      PIC XX.
          02 FILLER                              PIC X     VALUE "|".
          02 COD-ART-ENV.
             03 LLAVE-NRO-ENV.
                05 LLAVE-GRUPO-ENV.
                   07 TIPO-ENV                   PIC 9.
                   07 GRUPO-ENV.
                      09 GRUPO1-ENV              PIC X.
                      09 GRUPO2-ENV              PIC X.
                05 NUMERO-ENV.
                   07 NRO1-ENV                   PIC XX.
                   07 NRO2-ENV                   PIC X(11).
             03 CLASE-ENV                        PIC XX. 
          02 FILLER                              PIC X     VALUE "|".
          02 DESCRIP-GRUPO-ENV                   PIC X(30).
          02 FILLER                              PIC X     VALUE "|".
          02 DESCRIP-ART-ENV                     PIC X(50).
          02 FILLER                              PIC X      VALUE "|".  
          02 COD-BARRAS-ART-ENV                  PIC X(15).
          02 FILLER                              PIC X      VALUE "|".  
          02 USO-ART-ENV                         PIC XX.
          02 FILLER                              PIC X      VALUE "|".     
          02 COD-MARCA-ENV                       PIC XX.
          02 FILLER                              PIC X     VALUE "|".  
          02 DESCRIP-MARCA-ENV                   PIC X(15).    
          02 FILLER                              PIC X     VALUE "|".  
          02 REF-ART-ENV                         PIC X(15).
          02 FILLER                              PIC X      VALUE "|".     
          02 VISUAL-WEB-ART-ENV                  PIC X.
          02 FILLER                              PIC X      VALUE "|".
          02 ING-ACT-ART-ENV                     PIC X(2).
          02 FILLER                              PIC X      VALUE "|".
          02 ACOMPA-ART-ENV                      PIC X.    
          02 FILLER                              PIC X      VALUE "|".
          02 NIT-ART-ENV                         PIC 9(10).
          02 FILLER                              PIC X      VALUE "|".
          02 DESCRIP-TER-ENV.
             03 DESCRIP1-TER.
                05 INI1-TER                      PIC X.
                05 INI2-TER                      PIC X(29).
             03 DESCRIP2-TER                     PIC X(20).
          02 FILLER                              PIC X      VALUE "|".
          02 OTROS-ART-ENV                       PIC X(188).
          02 FILLER                              PIC X      VALUE "|".
          02 CTA-ART-ENV.                       
             05 MAYOR-ART.
                07 MAY1-ART                      PIC XX.
                07 MAY2-ART                      PIC XX.
             05 SCTA-ART                         PIC XX.
             05 AUXIL-ART.
                07 AUXIL1-ART                    PIC X.
                07 AUXIL2-ART                    PIC XX.
                07 AUXIL3-ART                    PIC XX.
          02 FILLER                              PIC X      VALUE "|".
          02 NOMBRE-MAE-ENV.
             05 NOMBRE1-MAE.
                10 NOM-A-MAE                     PIC X.
                10 NOM-B-MAE                     PIC X(26).
             05 NOMBRE2-MAE                      PIC X(3).
          02 FILLER                              PIC X      VALUE "|".
          02 ESTADO-ART-ENV                      PIC 9.
          02 FILLER                              PIC X      VALUE "|".     
          02 DCTO-GRAL-VTA-ART-ENV               PIC X(6). 
          02 FILLER                              PIC X      VALUE "|".
          02 AUTORET-ART-ENV                     PIC 9(5)V9.                 
          02 FILLER                              PIC X      VALUE "|".            
          02 PAQUETES-ART-ENV                    PIC 99.
          02 FILLER                              PIC X      VALUE "|". 
          02 UNIDAD-ART-ENV                      PIC X(5).  
          02 FILLER                              PIC X      VALUE "|".     
          02 RESPOS-ART-ENV                      PIC 9(5).
          02 FILLER                              PIC X      VALUE "|".
          02 UNID-CONV-ART-ENV                   PIC X(7).
          02 FILLER                              PIC X      VALUE "|".
          02 STOCK-ART-ENV.       
             05 STOCK-MIN-ART-ENV                PIC 9(6).
             05 STOCK-MAX-ART-ENV                PIC 9(6).
             05 STOCK-ALMAC-ART-ENV              PIC 9(5).
          02 FILLER                              PIC X      VALUE "|".
          02 POL-ART-ENV                         PIC XX.
          02 FILLER                              PIC X      VALUE "|".
          02 DESCRIP-POL-ENV                     PIC X(20).
          02 FILLER                              PIC X      VALUE "|".
          02 MERMA-ART-ENV                       PIC 99.
          02 FILLER                              PIC X      VALUE "|".
          02 FORMA-LIQ-ART-ENV                   PIC X.
          02 FILLER                              PIC X      VALUE "|".
          02 PESO-ART-ENV                        PIC X(10).
          02 FILLER                              PIC X      VALUE "|".
          02 IVA-ART-ENV                         PIC X.
          02 FILLER                              PIC X      VALUE "|".
          02 VLR-ULT-COMPRA-ENV                  PIC X(15).
          02 FILLER                              PIC X      VALUE "|".
          02 ULT-CANT-COMPRA-ART-ENV             PIC 9(8).
          02 FILLER                              PIC X      VALUE "|".    
          02 FECHA-ULT-COMPRA-ENV.
             03 ANO-ULT-COMPRA-ENV               PIC 9(4).
             03 MES-ULT-COMPRA-ENV               PIC 99.
             03 DIA-ULT-COMPRA-ENV               PIC 99.
          02 FILLER                              PIC X      VALUE "|".
          02 VLR-COMP-ALTA-ART-ENV               PIC 9(12)V99.
          02 FILLER                              PIC X      VALUE "|".
          02 FECHA-COMP-ALTA-ART-ENV.
             05 ANO-COMP-ALTA-ART-ENV            PIC 9(4).
             05 MES-COMP-ALTA-ART-ENV            PIC 9(2).
             05 DIA-COMP-ALTA-ART-ENV            PIC 9(2).
          02 FILLER                              PIC X      VALUE "|".
          02 COLOR-ART-ENV                       PIC X(20).
          02 FILLER                              PIC X      VALUE "|".      
          02 FECHA-LISTA-COMP-ENV.
             03 ANO-LISTA-COMP-ENV               PIC 9(4).
             03 MES-LISTA-COMP-ENV               PIC 99.
             03 DIA-LISTA-COMP-ENV               PIC 99.   
          02 FILLER                              PIC X      VALUE "|".
          02 TALLA-ART-ENV                       PIC X(3).
          02 FILLER                              PIC X      VALUE "|".    
          02 VLR-LISTA-COMP-ART-ENV              PIC X(15). 
          02 FILLER                              PIC X      VALUE "|".
          02 CCOSTO-ART-ENV                      PIC X(4).       
          02 FILLER                              PIC X      VALUE "|".
          02 NOMBRE-COSTO-ENV.
             05 NOMBRE1-COSTO                    PIC X.
             05 NOMBRE2-COSTO                    PIC X(14).
             05 NOMBRE3-COSTO                    PIC X(5).
          02 FILLER                              PIC X      VALUE "|".
          02 VLR-REF-ART-ENV                     PIC X(15).
          02 FILLER                              PIC X      VALUE "|".
          02 FECHA-ELAB-ENV                      PIC X(6).
          02 FILLER                              PIC X     VALUE "|".   
          02 VLR-RAZONABLE-ART-ENV               PIC X(15).
          02 FILLER                              PIC X      VALUE "|".
          02 VLR-RESIDUAL-ART-ENV                PIC 9(12).  
          02 FILLER                              PIC X      VALUE "|". 
          02 VIDA-UTIL-ART-ENV                   PIC 9(4).
          02 FILLER                              PIC X      VALUE "|".
          02 OBSERV-FACT-ART-ENV                 PIC X.
          02 FILLER                              PIC X      VALUE "|".
          02 UBICAC-ART-ENV                      PIC X(5).
          02 FILLER                              PIC X      VALUE "|".
          02 PORC_IMPOCONS-ART-ENV               PIC 9(2).
          02 FILLER                              PIC X      VALUE "|".
          02 VLR_IMPOCONS-ART-ENV                PIC 9(12).
          02 FILLER                              PIC X     VALUE "|".             
          02 OPER-ELAB-ART-ENV                   PIC X(4).
          02 FILLER                              PIC X     VALUE "|".  
          02 FECHA-ELAB-ART-ENV                  PIC X(6).
          02 FILLER                              PIC X     VALUE "|".  
          02 HORA-ELAB-ART-ENV                   PIC X(4).
          02 FILLER                              PIC X     VALUE "|".  
          02 OPER-MOD-ART-ENV                    PIC X(4).
          02 FILLER                              PIC X     VALUE "|".  
          02 FECHA-MOD-ART-ENV                   PIC X(6).
          02 FILLER                              PIC X     VALUE "|".  
          02 HORA-MOD-ART-ENV                    PIC X(4).
          02 FILLER                              PIC X     VALUE "|".  
          02 ID-ART-ENV                          PIC X(50).
          02 FILLER                              PIC X     VALUE "|".  
          02 DESCRIP-USO-ENV                     PIC X(20).
          02 FILLER                              PIC X     VALUE "|".
          02 DESCRIP-USO2-ENV                    PIC X(20).
          02 FILLER                              PIC X     VALUE "|".  
          02 DATO-J                              PIC X(28).
          
          
       01 LIN-1.
          02 FILLER                              PIC X(14)  VALUE "{*TB_DESCRIP*:".
          02 FILLER                              PIC X      VALUE "*".
          02 TAB-DESCRIP-W-J                     PIC X(15).
          02 FILLER                              PIC XX     VALUE "*,".
          02 FILLER                              PIC X(12)  VALUE "*VR_VENTA*:".
          02 FILLER                              PIC X      VALUE "*".
          02 VR-VENTA1-ART-J                     PIC 9(12)V99.
          02 FILLER                              PIC XX     VALUE "*,".
          02 FILLER                              PIC X(16)  VALUE "*PORCINC_COMPR*:".
          02 FILLER                              PIC X      VALUE "*".
          02 PORC-INC-COMP-ART-J                 PIC 9(3)V99.
          02 FILLER                              PIC XX     VALUE "*,".
          02 FILLER                              PIC X(12)  VALUE "*VR_INCREM*:".
          02 FILLER                              PIC X      VALUE "*".
          02 VR-INCREM-ART-J                     PIC 9(12)V99.
          02 FILLER                              PIC XX     VALUE "*,".
          02 FILLER                              PIC X(14)  VALUE "*PORCINV_VTA*:".
          02 FILLER                              PIC X      VALUE "*".
          02 PORC-INC-VTA-ART-J                  PIC 9(3)V99. 
          02 FILLER                              PIC XX     VALUE "*}".    
          02 CIERRE-LIN-1                        PIC X.

                       
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
              MOVE "INV103-15"              TO MSJ3-HTML
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
              MOVE "INV103-15"              TO MSJ3-HTML
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
              MOVE "INV103-15"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.


       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MAESTRO-ARTICULOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-ART-W                TO MSJ2-HTML
              MOVE "INV103-15"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PREFIJOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PREF-W               TO MSJ2-HTML
              MOVE "INV103-15"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-GRUPOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-GRUP-W               TO MSJ2-HTML
              MOVE "INV103-15"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.    

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-USO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-USO-W                TO MSJ2-HTML
              MOVE "INV103-15"              TO MSJ3-HTML
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
              MOVE "INV103-15"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON  ARCHIVO-POLITICAS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-POL-W                TO MSJ2-HTML
              MOVE "INV103-15"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON  ARCHIVO-COSTO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-COSTO-W              TO MSJ2-HTML
              MOVE "INV103-15"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON  ARCHIVO-MAESTROS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-MAE-W                TO MSJ2-HTML
              MOVE "INV103-15"              TO MSJ3-HTML
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
                   COD-ART-LLEGADA-W, COD-BARRAS-LLEGADA-W                   
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
      
       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W

      *     MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W          
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.

       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
         
      *      MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
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
         
           INITIALIZE NOM-USUAR-W          
                      NOM-ART-W                          
                      NOM-PREF-W                           
                      NOM-GRUP-W                         
                      NOM-USO-W                           
                      NOM-TER-W                        
                      NOM-POL-W
                      NOM-COSTO-W
                      NOM-MAE-W
             
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
 
           MOVE "\\" TO NOM-USUAR-W          
                        NOM-ART-W                          
                        NOM-PREF-W                           
                        NOM-GRUP-W                         
                        NOM-USO-W                           
                        NOM-TER-W                        
                        NOM-POL-W
                        NOM-COSTO-W
                        NOM-MAE-W  

           INSPECT NOM-ART-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET
           INSPECT NOM-ART-W REPLACING FIRST " "
                                          BY "\"          
           INSPECT NOM-ART-W REPLACING FIRST "                              "
                                          BY DIR-CONTAB-LLEGADA-W

           MOVE NOM-ART-W TO NOM-USUAR-W
                             NOM-PREF-W
                             NOM-GRUP-W                         
                             NOM-USO-W                           
                             NOM-TER-W                        
                             NOM-POL-W
                             NOM-COSTO-W   
                             NOM-MAE-W

           INSPECT NOM-ART-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-MAESART.DAT".

           INSPECT NOM-USUAR-W REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHUSU.DAT".

           INSPECT NOM-PREF-W REPLACING FIRST "                        "
                                           BY "\CONTROL\SC-ARCHPREF.DAT".

           INSPECT NOM-GRUP-W REPLACING FIRST "                        "
                                           BY "\CONTROL\SC-GRUPOINV.DAT".

           INSPECT NOM-USO-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-ARCHUSO.DAT".

           INSPECT NOM-TER-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-ARCHTER.DAT".

           INSPECT NOM-POL-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-ARCHPOL.DAT".

           INSPECT NOM-COSTO-W REPLACING FIRST "                       "
                                            BY "\CONTROL\SC-ARCHCOS.DAT".

           INSPECT NOM-MAE-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-ARCHMAE.DAT".

           INITIALIZE NOM-PLANO-W

           MOVE OPERADOR-LLEGADA-W TO OPER-JSON-W
           MOVE FUNCTION CURRENT-DATE TO FECHA-JSON-W
           ACCEPT HORA-JSON-W FROM TIME


           MOVE NOMBRE-JSON-W  TO NOM-PLANO-W.
           MOVE NOM-JSON-W     TO DATO-J.

       ABRIR-ARCHIVOS.
           OPEN INPUT ARCHIVO-PREFIJOS
                      ARCHIVO-GRUPOS
                      ARCHIVO-USO
                      ARCHIVO-TERCEROS
                      ARCHIVO-POLITICAS
                      ARCHIVO-COSTO
                      ARCHIVO-MAESTROS.

       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-JSON.

           MOVE "{*TABLA*:[" TO  DATOS-JSON
           INSPECT DATOS-JSON  REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON  END-WRITE
           MOVE 0 TO SW-FIN.

       ENVIO-DATOS.
           READ ARCHIVO-PREFIJOS NEXT WITH NO LOCK AT END
                CONTINUE
           END-READ.

           INITIALIZE TABLA-DESCRIP-W

           PERFORM MOVER-DESCRIPCION VARYING I FROM 1 BY 1
                                     UNTIL I > 20.

           OPEN INPUT ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.

           OPEN INPUT MAESTRO-ARTICULOS.

           IF COD-BARRAS-LLEGADA-W = SPACES OR LOW-VALUES
              MOVE COD-ART-LLEGADA-W  TO COD-ART
              MOVE "00"                      TO SW-INVALID-ENV
              READ MAESTRO-ARTICULOS WITH NO LOCK 
                   INVALID KEY
                     MOVE "01" TO SW-INVALID-ENV                  
              END-READ
           ELSE 
              MOVE COD-BARRAS-LLEGADA-W    TO COD-BARRAS-ART
              MOVE "00"                    TO SW-INVALID-ENV
              READ MAESTRO-ARTICULOS WITH NO LOCK KEY IS COD-BARRAS-ART 
                   INVALID KEY
                      MOVE "01" TO SW-INVALID-ENV                  
              END-READ
           END-IF.

           PERFORM MOVER-VRART VARYING I FROM 1 BY 1
                               UNTIL I > 20.

           MOVE COD-ART             TO COD-ART-ENV
           MOVE LLAVE-GRUPO-ENV     TO LLAVE-GRUPO
           READ ARCHIVO-GRUPOS WITH NO LOCK
                INVALID KEY 
                   MOVE LLAVE-GRUPO TO DESCRIP-GRUPO  
           END-READ
           MOVE DESCRIP-GRUPO       TO DESCRIP-GRUPO-ENV

           MOVE 1                   TO TIPO-USO
           MOVE CLASE-ENV           TO COD-USO 
           READ ARCHIVO-USO WITH NO LOCK
                INVALID KEY 
                   MOVE LLAVE-USO   TO DESCRIP-USO   
           END-READ
           MOVE DESCRIP-USO         TO DESCRIP-USO-ENV


           MOVE DESCRIP-ART         TO DESCRIP-ART-ENV 
           MOVE COD-BARRAS-ART      TO COD-BARRAS-ART-ENV

           MOVE USO-ART             TO USO-ART-ENV

           MOVE 2                   TO TIPO-USO
           MOVE USO-ART-ENV         TO COD-USO 
           READ ARCHIVO-USO WITH NO LOCK
                INVALID KEY 
                   MOVE LLAVE-USO   TO DESCRIP-USO   
           END-READ
           MOVE DESCRIP-USO         TO DESCRIP-USO2-ENV

 
           MOVE COD-MARCA-ART       TO COD-MARCA-ENV  
           MOVE DESCRIP-MARCA-ART   TO DESCRIP-MARCA-ENV                  
           MOVE REF-ART             TO REF-ART-ENV      
           MOVE VISUAL-WEB-ART      TO VISUAL-WEB-ART-ENV       
           MOVE ING-ACT-ART         TO ING-ACT-ART-ENV           
           MOVE ACOMPA-ART          TO ACOMPA-ART-ENV
           MOVE NIT-ART             TO NIT-ART-ENV

           MOVE NIT-ART-ENV         TO COD-TERCERO
           READ ARCHIVO-TERCEROS WITH NO LOCK
                INVALID KEY 
                   MOVE COD-TERCERO TO DESCRIP-TER  
           END-READ
           MOVE DESCRIP-TER         TO DESCRIP-TER-ENV
                 
           MOVE OTROS-ART           TO OTROS-ART-ENV


             
           MOVE CTA-ART             TO CTA-ART-ENV

           MOVE CTA-ART-ENV         TO CTA-MAE
           MOVE 4                   TO TIPO-MAE

           READ ARCHIVO-MAESTROS WITH NO LOCK
                INVALID KEY 
                   MOVE LLAVE-MAE   TO NOMBRE-MAE  
           END-READ
           MOVE NOMBRE-MAE          TO NOMBRE-MAE-ENV



              
           MOVE ESTADO-ART          TO ESTADO-ART-ENV 
               
           MOVE DCTO-GRAL-VTA-ART   TO DCTO-GRAL-VTA-ART-EDIT
           MOVE DCTO-GRAL-VTA-ART-EDIT TO DCTO-GRAL-VTA-ART-ENV
                 
           MOVE AUTORET-ART         TO AUTORET-ART-ENV             
           MOVE PAQUETES-ART        TO PAQUETES-ART-ENV 
           MOVE UNIDAD-ART          TO UNIDAD-ART-ENV                  
           MOVE RESPOS-ART          TO RESPOS-ART-ENV

           MOVE UNID-CONV-ART       TO UNID-CONV-ART-EDIT
           MOVE UNID-CONV-ART-EDIT  TO UNID-CONV-ART-ENV

           MOVE STOCK-ART  (I)      TO STOCK-ART-ENV     
           MOVE POL-ART             TO POL-ART-ENV

           MOVE POL-ART-ENV         TO COD-POL
           READ ARCHIVO-POLITICAS WITH NO LOCK
                INVALID KEY 
                   MOVE COD-POL     TO DESCRIP-POL  
           END-READ
           MOVE DESCRIP-POL         TO DESCRIP-POL-ENV
              
           MOVE MERMA-ART           TO MERMA-ART-ENV           
           MOVE FORMA-LIQ-ART       TO FORMA-LIQ-ART-ENV
       
           MOVE PESO-ART            TO PESO-ART-ENV-EDIT
           MOVE PESO-ART-ENV-EDIT   TO PESO-ART-ENV
             
           MOVE IVA-ART             TO IVA-ART-ENV 
        
           MOVE VLR-ULT-COMPRA      TO VLR-ULT-COMPRA-EDIT
           MOVE VLR-ULT-COMPRA-EDIT TO VLR-ULT-COMPRA-ENV 
      
           MOVE ULT-CANT-COMPRA-ART TO ULT-CANT-COMPRA-ART-ENV   
           MOVE FECHA-ULT-COMPRA    TO FECHA-ULT-COMPRA-ENV

           MOVE VLR-COMP-ALTA-ART   TO VLR-COMP-ALTA-ART-EDIT
           MOVE VLR-COMP-ALTA-ART-EDIT TO VLR-COMP-ALTA-ART-ENV 

           MOVE FECHA-COMP-ALTA-ART TO FECHA-COMP-ALTA-ART-ENV 
       
           MOVE COLOR-ART           TO COLOR-ART-ENV            
           MOVE FECHA-LISTA-COMP    TO FECHA-LISTA-COMP-ENV         
           MOVE TALLA-ART           TO TALLA-ART-ENV 
          
           MOVE VLR-LISTA-COMP-ART  TO VLR-LISTA-COMP-ART-EDIT
           MOVE VLR-LISTA-COMP-ART-EDIT TO VLR-LISTA-COMP-ART-ENV
     
           MOVE CCOSTO-ART          TO CCOSTO-ART-ENV

           MOVE CCOSTO-ART-ENV      TO COD-COSTO
           READ ARCHIVO-COSTO WITH NO LOCK
                INVALID KEY 
                   MOVE COD-COSTO   TO NOMBRE-COSTO  
           END-READ
           MOVE NOMBRE-COSTO        TO NOMBRE-COSTO-ENV
             
           MOVE VLR-REF-ART         TO VLR-REF-ART-EDIT
           MOVE VLR-REF-ART-EDIT    TO VLR-REF-ART-ENV

           MOVE FECHA-ELAB-ART      TO FECHA-ELAB-ENV 
       
           MOVE VLR-RAZONABLE-ART      TO VLR-RAZONABLE-ART-EDIT
           MOVE VLR-RAZONABLE-ART-EDIT TO VLR-RAZONABLE-ART-ENV 
   
           MOVE VLR-RESIDUAL-ART    TO VLR-RESIDUAL-ART-ENV      
           MOVE VIDA-UTIL-ART       TO VIDA-UTIL-ART-ENV         
           MOVE OBSERV-FACT-ART     TO OBSERV-FACT-ART-ENV        
           MOVE UBICAC-ART          TO UBICAC-ART-ENV            
           MOVE PORC-IMPOCONS-ART   TO PORC_IMPOCONS-ART-ENV     
           MOVE VLR-IMPOCONS-ART    TO VLR_IMPOCONS-ART-ENV
           MOVE OPER-ELAB-ART       TO OPER-ELAB-ART-ENV
           MOVE HORA-ELAB-ART       TO HORA-ELAB-ART-ENV
           MOVE OPER-MOD-ART        TO OPER-MOD-ART-ENV
           MOVE FECHA-MOD-ART       TO FECHA-MOD-ART-ENV
           MOVE HORA-MOD-ART        TO HORA-MOD-ART-ENV

           IF ID-ART > 0
              MOVE ID-ART TO COD-TER
              READ ARCHIVO-TERCEROS WITH NO LOCK
                   INVALID KEY 
                     MOVE ID-ART TO ID-ART-ENV
              END-READ
              MOVE DESCRIP-TER TO ID-ART-ENV
           END-IF.

       CERRAR-JSON.
           INITIALIZE LIN-1

           INITIALIZE VR-VENTA1-ART-J       
                      PORC-INC-COMP-ART-J     
                      VR-INCREM-ART-J        
                      PORC-INC-VTA-ART-J   
                                                                 
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE LIN-1 TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
          
           MOVE "]}" TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE.

       CERRAR-MAESTRO-ARTICULOS.           
           CLOSE MAESTRO-ARTICULOS
                 ARCHIVO-PREFIJOS
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

       MOVER-DESCRIPCION.
           IF DESCRIP-PREF (I) = SPACES
               CONTINUE
           ELSE
               MOVE DESCRIP-PREF (I) TO TAB-DESCRIP-W (I)
           END-IF.

       MOVER-VRART.
           INITIALIZE LIN-1
         
           MOVE TAB-DESCRIP-W (I)     TO TAB-DESCRIP-W-J

           MOVE VR-VENTA1-ART (I)      TO VR-VENTA1-ART-J
           MOVE PORC-INC-COMP-ART (I)  TO PORC-INC-COMP-ART-J
           MOVE VR-INCREM-ART (I)      TO VR-INCREM-ART-J 
           MOVE PORC-INC-VTA-ART (I)   TO PORC-INC-VTA-ART-J

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-1

           MOVE LIN-1 TO DATOS-JSON
           WRITE REG-JSON END-WRITE.       
              
       COPY "..\..\FUENTES\SC-WEB19.CBL".
