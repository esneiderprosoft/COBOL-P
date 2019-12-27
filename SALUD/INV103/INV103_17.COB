      *=================================================================
      *GRABAR ACTUALIZACION MAESTRO DE ARTICULOS 
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
       

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
        
           SELECT ARCHIVO-CLASCLIENTE LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-CLAS-W
      *"\PROG\DATOS\SC-CLASC.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS LLAVE-CLASC
                  ALTERNATE RECORD KEY IS DESCRIP-CLASC WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-TEXTO LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.

      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-ARTIC.CBL".
       COPY "..\..\FUENTES\FD-CLASC.CBL".

       FD  ARCHIVO-TEXTO
           LABEL RECORD STANDARD.
       01  REG-PLANO.
           02 DATOS-PLANO            PIC X(500).

      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".
  
       77 NOM-ART-W                            PIC X(70).
       77 NOM-CLAS-W                           PIC X(70).
       77 NOM-PLANO-W   	               PIC X(90).
       77 CONT-W                               PIC 99.
       01 LINEA-LLEGADA-W                      PIC X(500).

       01 VR-VENT1-ART-EDIT.
          02 ENT-VR-VENT1-EDIT                  PIC 9(12).
          02 DEC-VR-VENT1-EDIT                  PIC 99.
           
       01 PORC-INC-COMP-EDIT.
          02 ENT-PORC-INC-EDIT                  PIC 9(3).
          02 DEC-PORC-INC-EDIT                  PIC 99.

       01 VR-INCREM-ART-EDIT.
          02 ENT-VR-INCREM-EDIT                 PIC 9(12).
          02 DEC-VR-INCREM-EDIT                 PIC 99.

       01 PORC-INC-VTA-EDIT.
          02 ENT-PORC-VTA-EDIT                  PIC 9(3).
          02 DEC-PORC-VTA-EDIT                  PIC 99.


       01 CANT-EMP-ART-EDIT.
          02 ENT-CANT-EMP-EDIT                  PIC 9(3).
          02 DEC-CANT-EMP-EDIT                  PIC 9(4).


       01 UNID-CONV-EDIT.
          02 ENT-UNID-CONV-EDIT                 PIC 9(4).
          02 DEC-UNID-CONV-EDIT                 PIC 99.

       01 AUTORET-EDIT.
          02 ENT-AUTORET-EDIT                   PIC 9(5).
          02 DEC-AUTORET-EDIT                   PIC 9.

       01 VLR-REF-EDIT.
          02 ENT-VLR-REF-EDIT                   PIC 9(5).
          02 DEC-VLR-REF-EDIT                   PIC 9.

       01 VLR-ULT-COMPRA-EDIT.
          02 ENT-VLRULT-COMPR-EDIT              PIC 9(12).
          02 DEC-VLRULT-COMPR-EDIT              PIC 99.

       01 VLR-LISTA-COMP-EDIT.
          02 ENT-VLR-LISTA-EDIT                 PIC 9(12).
          02 DEC-VLR-LISTA-EDIT                 PIC 99.

       01 DCTO-GRAL-VTA-EDIT.
          02 ENT-DCTO-GRAL-EDIT                 PIC 9(3).
          02 DEC-DCTO-GRAL-EDIT                 PIC 99.
                                   
       01 CAMP-EMP-EDIT.
          02 ENT-CAMP-EMP-EDIT                  PIC 9(3).
          02 DEC-CAMP-EMP-EDIT                  PIC 9(4).

       01 VLR-COMP-ALTA-EDIT.       
          02 ENT-VLRCOMPRA-ALTA-EDIT            PIC 9(12).
          02 DEC-VLRCOMPRA-ALTA-EDIT            PIC 9(2). 

       01 VLR-RAZONABLE-EDIT.
          02 ENT-VLR-RAZONABLE-EDIT             PIC 9(12).
          02 DEC-VLR-RAZONABLE-EDIT             PIC 9(2).

       01 PESO-EDIT.
          02 ENT-PESO-EDIT                      PIC 9(6).
          02 DEC-PESO-EDIT                      PIC 9(3).

       01 NOMBRE-TXT                           PIC X(42).

       01 NOMBREDOS-TXT                        PIC X(42).
     
       01  DATO-LLEGADA-W.
           02 LLAVE-SESION-LLEGA-W.
              03 ID-LLEGAD-W                   PIC X(15).
              03 FECHA-LLEGAD-W                PIC X(8).
              03 HORA-LLEGAD-W                 PIC X(6).
           02 DIR-CONTAB-LLEGADA-W             PIC X(30).
           02 MES-CONTAB-LLEGADA-W             PIC XX.
           02 NOV-LLEGADA-W                    PIC X.

       01  REG-MAESTRO-W.
           02 COD-ART-W.
              03 LLAVE-NRO-ART-W.
                 05 LLAVE-GRUPO-ART-W.
                    07 TIPO-ART-W           PIC 9.
                    07 GRUPO-ART-W.
                       09 GRUPO1-ART-W      PIC X.
                       09 GRUPO2-ART-W      PIC X.
                 05 NUMERO-ART-W.
                    07 NRO1-ART-W           PIC XX.
                    07 NRO2-ART-W           PIC X(11).
              03 CLASE-ART-W                PIC XX.
           02 COD-BARRAS-ART-W              PIC X(15).
           02 DESCRIP-ART-W.
              03 DESCRIP-ART1-W.
                 05 INICIAL-ART-W.
                    07 DESCRIP1-ART-W       PIC X.
                    07 DESCRIP2-ART-W       PIC X(3).
                 05 RESTO-ART-W             PIC X(26).
              03 DESCRIP-ART2-W             PIC X(20).
           02 DATOS-ART-W.
              03 MARCA-ART-W.
                 05 COD-MARCA-ART-W         PIC XX.
                 05 DESCRIP-MARCA-ART-W     PIC X(15).
              03 REF-ART-W.
                 07 REF1-ART-W              PIC X(7).
                 07 REF2-ART-W              PIC X(8).
              03 UNIDAD-ART-W               PIC X(5).
              03 FILLER                   PIC X(8).
              03 UNID-CONV-ART-W            PIC 9(4)V99.
              03 AUTORET-ART-W              PIC 9(5)V9.
              03 POL-ART-W                  PIC XX.
              03 VLR-REF-ART-W              PIC 9(12)V99.
              03 PAQUETES-ART-W             PIC 99.
              03 USO-ART-W.
                 05 USO1-ART-W              PIC X.
                 05 USO2-ART-W              PIC X.
              03 COD-EQUIV-ART-W            PIC X(13).
              03 CL-ROTAC-ART-W             PIC X.       
              03 UBICAC-ART-W               PIC X(5).
              03 IVA-ART-W                  PIC X.
              03 COSTO-USD-ART-W            PIC 9(12)V99.
              03 VLR-ULT-COMPRA-W           PIC 9(12)V99.
              03 NIT-ART-W                  PIC 9(10).
              03 OBSERV-FACT-ART-W          PIC X.
              03 FORMA-LIQ-ART-W            PIC X.
              03 ING-ACT-ART-W              PIC X(2).
              03 CTA-ART-W.
                 05 MAYOR-ART-W.
                    07 MAY1-ART-W           PIC XX.
                    07 MAY2-ART-W           PIC XX.
                 05 SCTA-ART-W              PIC XX.
                 05 AUXIL-ART-W.
                    07 AUXIL1-ART-W         PIC X.
                    07 AUXIL2-ART-W         PIC XX.
                    07 AUXIL3-ART-W         PIC XX.
              03 FECHA-ULT-COMPRA-W.
                 05 ANO-ULT-COMPRA-W        PIC 9(4).
                 05 MES-ULT-COMPRA-W        PIC 99.
                 05 DIA-ULT-COMPRA-W        PIC 99.
              03 FECHA-VENCE-ART-W.
                 05 ANO-VENCE-ART-W         PIC 9(4).
                 05 MES-VENCE-ART-W         PIC 99.
                 05 DIA-VENCE-ART-W         PIC 99.
              03 DATOS-LISTA-COMP-ART-W.
                  05 VLR-LISTA-COMP-ART-W   PIC 9(12)V99.
                  05 FECHA-LISTA-COMP-W.
                     07 ANO-LISTA-COMP-W    PIC 9(4).
                     07 MES-LISTA-COMP-W    PIC 99.
                     07 DIA-LISTA-COMP-W    PIC 99.
              03 RESPOS-ART-W               PIC 9(5).
              03 OTROS-ART-W.
                 05 OTROS1-ART-W.
                    07 OTROS1A-ART-W        PIC X(3).
                    07 OTROS1B-ART-W        PIC X(47).
                 05 OTROS2-ART-W            PIC X(50).
                 05 OTROS3-ART-W.
                    07 OTROS3A-ART-W        PIC X(15).
                    07 OTROS3B-ART-W        PIC X(35).
                 05 OTROS4-ART-W.
                    07 OTROS4A-ART-W        PIC X(12).
                    07 OTROS4B-ART-W        PIC X(26).
              03 VISUAL-WEB-ART-W           PIC X.
              03 DCTO-GRAL-VTA-ART-W        PIC 9(3)V99.
              03 FILLER                   PIC X(30).
              03 EMPAQUE-ART-W.
                 05 TABLA-EMP-ART-W OCCURS 10.
                    07 ALM-EMP-ART-W        PIC X(5).
                    07 COD-EMP-ART-W        PIC X(18).
                    07 CANT-EMP-ART-W       PIC 9(3)V9(4).
              03 TABLA-ART-W.
                 05 RENG-ART-W  OCCURS 60.
                    07 RENG1-ART-W          PIC X(30).
                    07 RENG2-ART-W          PIC X(30).
                    07 RENG3-ART-W          PIC X(30).
              03 CCOSTO-ART-W.
                 05 CCOSTO1-ART-W           PIC 99.
                 05 CCOSTO2-ART-W           PIC XX.
              03 EXC-SISMED-ART-W           PIC X.
              03 TAB-VTA-ART-W.
                 05 TABLA-VTA-ART-W OCCURS 20.
                    07 VR-VENTA1-ART-W      PIC 9(12)V99.
                    07 PORC-INC-COMP-ART-W  PIC 9(3)V99.
                    07 VR-INCREM-ART-W      PIC 9(12)V99.
                    07 PORC-INC-VTA-ART-W   PIC 9(3)V99.
                    07 FILLER             PIC X(9).
           02 CUM-LTF-ART-W                 PIC X(15).
           02 CALIF-ART-W                   PIC X.
           02 FECH-CALIF-ART-W.
              03 ANO-CALIF-ART-W            PIC 99.
              03 MES-CALIF-ART-W            PIC 99.
              03 DIA-CALIF-ART-W            PIC 99.
           02 VLR-RESIDUAL-ART-W            PIC S9(12).
           02 CLASE-RIESGO-ART-W            PIC 9.
           02 HOMOLOGO-ART-W                PIC X.
           02 HOMOLOGO-COD-ART-W.
              03 HOMOLOGO-GRUPO-ART-W       PIC X(3).
              03 HOMOLOGO-NUMERO-ART-W      PIC X(13).
              03 HOMOLOGO-CLASE-ART-W       PIC XX.
           02 VIDA-UTIL-ART-W               PIC 9(4).
           02 ULT-CANT-COMPRA-ART-W         PIC 9(8).
           02 ESTADO-ART-W                  PIC 9.
           02 STOCK-ART-W OCCURS 5.
              05 STOCK-ALMAC-ART-W          PIC 9(5).
              05 STOCK-MIN-ART-W            PIC 9(6).
              05 STOCK-MAX-ART-W            PIC 9(6).
           02 MERMA-ART-W                   PIC 99.
           02 ID-ART-W                      PIC 9(10).
           02 ACOMPA-ART-W                  PIC X.
           02 ITEM-ACOMP-ART-W.
              03 ITEM-AC1-ART-W             PIC 99.
              03 ITEM-AC2-ART-W             PIC 99.
           02 VLR-RAZONABLE-ART-W           PIC 9(12)V99.
           02 PESO-ART-W                    PIC 9(6)V999.
           02 TALLA-ART-W                   PIC X(3).
           02 COLOR-ART-W                   PIC X(20).
           02 UBICAC2-ART-W                 PIC X(20).
           02 PORC-IMPOCONS-ART-W           PIC 9(2).
           02 VLR-IMPOCONS-ART-W            PIC 9(12).
           02 DATOS-COMP-ALTA-ART-W.
              03 FECHA-COMP-ALTA-ART-W.
                 05 ANO-COMP-ALTA-ART-W     PIC 9(4).
                 05 MES-COMP-ALTA-ART-W     PIC 9(2).
                 05 DIA-COMP-ALTA-ART-W     PIC 9(2).
              03 VLR-COMP-ALTA-ART-W        PIC 9(12)V99.
           02 FILLER                        PIC X(789).
           02 REST-ART-W.
              05 OPER-ELAB-ART-W            PIC X(4).
              05 FECHA-ELAB-ART-W           PIC X(6).
              05 HORA-ELAB-ART-W            PIC X(4).
              05 OPER-MOD-ART-W             PIC X(4).
              05 FECHA-MOD-ART-W            PIC X(6).
              05 HORA-MOD-ART-W             PIC X(4).
                
       01 DATOS-ENVIO.
          02 DAT0-ENV                  PIC XX.
          02 FILLER                    PIC X VALUE "|".
          02 DAT1-ENV                  PIC X(30).

      
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
              MOVE "INV103"                 TO MSJ3-HTML
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
              MOVE "INV103"                 TO MSJ3-HTML
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
              MOVE "INV103"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CLASCLIENTE.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-CLAS-W               TO MSJ2-HTML
              MOVE "INV103"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-TEXTO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PLANO-W              TO MSJ2-HTML
              MOVE "INV103"                 TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO LINEA-LLEGADA-W.

           INITIALIZE REG-MAESTRO-W.

           UNSTRING LINEA-LLEGADA-W DELIMITED BY "|"
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, NOV-LLEGADA-W, TIPO-ART-W, GRUPO-ART-W, NUMERO-ART-W,  
                   CLASE-ART-W, DESCRIP-ART-W, USO-ART-W, VISUAL-WEB-ART-W, ING-ACT-ART-W, NIT-ART-W, ACOMPA-ART-W, COD-BARRAS-ART-W, OTROS-ART-W,
                   ESTADO-ART-W, COD-MARCA-ART-W, DESCRIP-MARCA-ART-W, REF-ART-W, DCTO-GRAL-VTA-EDIT, AUTORET-EDIT, PAQUETES-ART-W, UNIDAD-ART-W,
                   RESPOS-ART-W, UNID-CONV-EDIT, STOCK-ALMAC-ART-W (1), STOCK-MIN-ART-W (1), STOCK-MAX-ART-W (1), POL-ART-W, MERMA-ART-W,
                   FORMA-LIQ-ART-W, PESO-EDIT, IVA-ART-W, VLR-ULT-COMPRA-EDIT, ULT-CANT-COMPRA-ART-W, FECHA-ULT-COMPRA-W, VLR-COMP-ALTA-EDIT, FECHA-COMP-ALTA-ART-W,
                   COLOR-ART-W, FECHA-LISTA-COMP-W, TALLA-ART-W, VLR-LISTA-COMP-EDIT, CCOSTO-ART-W, VLR-REF-EDIT, VLR-RAZONABLE-EDIT, 
                   VLR-RESIDUAL-ART-W, VIDA-UTIL-ART-W, OBSERV-FACT-ART-W, UBICAC-ART-W, PORC-IMPOCONS-ART-W, VLR-IMPOCONS-ART-W, CTA-ART-W,
                   OPER-ELAB-ART-W, FECHA-ELAB-ART-W, HORA-ELAB-ART-W, OPER-MOD-ART-W, FECHA-MOD-ART-W, HORA-MOD-ART-W, NOMBRE-TXT, NOMBREDOS-TXT 
 
           END-UNSTRING.

           MOVE UNID-CONV-EDIT        TO UNID-CONV-ART-W       
           MOVE AUTORET-EDIT          TO AUTORET-ART-W
           MOVE VLR-REF-EDIT          TO VLR-REF-ART-W    
           MOVE VLR-ULT-COMPRA-EDIT   TO VLR-ULT-COMPRA-W    
           MOVE VLR-LISTA-COMP-EDIT   TO VLR-LISTA-COMP-ART-W
           MOVE VLR-COMP-ALTA-EDIT    TO VLR-COMP-ALTA-ART-W  
           MOVE DCTO-GRAL-VTA-EDIT    TO DCTO-GRAL-VTA-ART-W                                               
           MOVE VLR-RAZONABLE-EDIT    TO VLR-RAZONABLE-ART-W     
           MOVE PESO-EDIT             TO PESO-ART-W.

           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.

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
              MOVE "Validacion de usuarios"                TO MSJ1-HTML
              MOVE "Falta configurar usuario"              TO MSJ2-HTML
              MOVE "Sc"                                    TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.

           INITIALIZE NOM-ART-W 
           
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           MOVE "\\" TO NOM-ART-W 


           INSPECT NOM-ART-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET

           INSPECT NOM-ART-W REPLACING FIRST " "
                                          BY "\"

           INSPECT NOM-ART-W REPLACING FIRST "                              "
                                          BY DIR-CONTAB-LLEGADA-W.
           
           INSPECT NOM-ART-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-MAESART.DAT".

       ABRIR-TABLA.
                                                   
           MOVE NOMBRE-TXT  TO NOM-PLANO-W
		
           OPEN INPUT ARCHIVO-TEXTO.

           INITIALIZE CONT-W TAB-VTA-ART-W.


       LEER-TABLA.
           READ ARCHIVO-TEXTO NEXT WITH NO LOCK AT END GO TO ABRIR-TABLA2.

           IF DATOS-PLANO = SPACES OR LOW-VALUES
              GO TO LEER-TABLA
           ELSE
           
	      ADD 1 TO CONT-W

              IF CONT-W > 20
                 GO TO ABRIR-TABLA2
              END-IF

              UNSTRING DATOS-PLANO DELIMITED BY "|"
                  INTO VR-VENT1-ART-EDIT, PORC-INC-COMP-EDIT, VR-INCREM-ART-EDIT, PORC-INC-VTA-EDIT
              END-UNSTRING

 
              MOVE VR-VENT1-ART-EDIT   TO VR-VENTA1-ART-W (CONT-W) 
              MOVE PORC-INC-COMP-EDIT  TO PORC-INC-COMP-ART-W (CONT-W)
              MOVE VR-INCREM-ART-EDIT  TO VR-INCREM-ART-W (CONT-W)
              MOVE PORC-INC-VTA-EDIT   TO PORC-INC-VTA-ART-W (CONT-W)

           GO TO LEER-TABLA. 

       ABRIR-TABLA2.

           CLOSE ARCHIVO-TEXTO.

           MOVE NOMBREDOS-TXT TO NOM-PLANO-W

           OPEN INPUT ARCHIVO-TEXTO.

           INITIALIZE CONT-W.

       LEER-TABLA2.

           READ ARCHIVO-TEXTO NEXT WITH NO LOCK AT END GO TO CERRAR-TABLA.

           IF DATOS-PLANO = SPACES OR LOW-VALUES
              GO TO LEER-TABLA2
           ELSE
              ADD 1 TO CONT-W

              IF CONT-W > 10
                 GO TO CERRAR-TABLA
              END-IF

              UNSTRING DATOS-PLANO DELIMITED BY "|"
                  INTO ALM-EMP-ART-W (CONT-W), COD-EMP-ART-W (CONT-W), CANT-EMP-ART-EDIT
              END-UNSTRING
                   
              MOVE CANT-EMP-ART-EDIT  TO CANT-EMP-ART-W (CONT-W)
           END-IF.

           GO TO LEER-TABLA2.            
           
       CERRAR-TABLA.
           CLOSE ARCHIVO-TEXTO.

       ABRIR-MAESTRO.

           OPEN INPUT MAESTRO-ARTICULOS. 
       
       LEER-MAESTRO.

           MOVE COD-ART-W TO COD-ART
           READ MAESTRO-ARTICULOS WITH NO LOCK
                INVALID KEY 
                  MOVE 01 TO SW-INVALID
           END-READ.

           IF NOV-LLEGADA-W = 7 AND SW-INVALID = 01 GO TO NUEVO.
           IF NOV-LLEGADA-W = 7 AND SW-INVALID = 00 GO TO ERROR-1.
           IF NOV-LLEGADA-W = 8 AND SW-INVALID = 00 GO TO CAMBIO.
           IF NOV-LLEGADA-W = 8 AND SW-INVALID = 01 GO TO ERROR-1.
           IF NOV-LLEGADA-W = 9 AND SW-INVALID = 00 GO TO RETIRO.
           IF NOV-LLEGADA-W = 9 AND SW-INVALID = 01 GO TO ERROR-1.

       NUEVO.
        
           MOVE REG-MAESTRO-W TO REG-MAESTRO 


           CLOSE MAESTRO-ARTICULOS.
           OPEN I-O MAESTRO-ARTICULOS.

           WRITE REG-MAESTRO END-WRITE
           CLOSE MAESTRO-ARTICULOS.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO GRABADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       CAMBIO.
            
           CLOSE MAESTRO-ARTICULOS.

           OPEN I-O MAESTRO-ARTICULOS.

           MOVE REG-MAESTRO-W TO REG-MAESTRO 

           REWRITE REG-MAESTRO END-REWRITE
           CLOSE MAESTRO-ARTICULOS.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO MODIFICADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       RETIRO.
           CLOSE MAESTRO-ARTICULOS.

           OPEN I-O MAESTRO-ARTICULOS.

           DELETE MAESTRO-ARTICULOS END-DELETE

           CLOSE MAESTRO-ARTICULOS.

           MOVE 00 TO DAT0-ENV
           MOVE "REGISTRO BORRADO" TO DAT1-ENV.

           GO TO ENVIO-DATOS.

       ERROR-1.
           MOVE 99 TO DAT0-ENV
           MOVE "ERROR" TO DAT1-ENV.

           GO TO ENVIO-DATOS.   


       ENVIO-DATOS.

           MOVE 00         TO DAT0-ENV
           MOVE "CORRECTO" TO DAT1-ENV.

       PAGINA-CONFIG.

           MOVE "datosrecibidos"  TO COBW3-CNV-NAME
           MOVE DATOS-ENVIO       TO COBW3-CNV-VALUE
           CALL "COBW3_SET_CNV"   USING COBW3

           MOVE "..\..\FRAMEWORKS\PAGINAS\RECIBIDOS.ASPX"  TO SALIDA-HTML
           PERFORM ABRIR-HTML.
      
       CERRAR-SESION.
           CALL "COBW3_FREE" USING COBW3.
           MOVE 1 TO PROGRAM-STATUS.
           EXIT PROGRAM.
        
       COPY "..\..\FUENTES\SC-WEB19.CBL".
      
