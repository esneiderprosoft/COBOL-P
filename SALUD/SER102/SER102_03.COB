      *=================================================================
      * BUSQUEDA Y CONSULTA DE MAESTRO ARTICULOS- CLASE ARTICULO
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

                  SELECT ARCHIVO-PREFIJOS LOCK MODE IS AUTOMATIC
                         ASSIGN NOM-PREF-W
                         ORGANIZATION IS INDEXED;
                         ACCESS MODE DYNAMIC;
                         RECORD KEY  IS LLAVE-PREF
                         FILE STATUS IS OTR-STAT.


                  SELECT ARCHIVO-JSON LOCK MODE IS AUTOMATIC
                         ASSIGN NOM-PLANO-W
                         ORGANIZATION IS LINE SEQUENTIAL
                         FILE STATUS IS OTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-ARTIC.CBL".
       COPY "..\..\FUENTES\FD-PREFI.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON      PIC X(500).


       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-ART-W                              PIC X(70).
       77 NOM-PREF-W                             PIC X(70).
       77 NOM-PLANO-W                            PIC X(90).
       77 CONT                                   PIC 9(4).

       01 TABLA-DESCRIP-W.
          02 TAB-DESCRIP-W  OCCURS 20            PIC X(15).    

       01 LLEGADA-W                              PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W                      PIC X(15).
             03 FECHA-LLEGAD-W                   PIC X(8).
             03 HORA-LLEGAD-W                    PIC X(6).
          02 DIR-CONTAB-LLEGADA-W                PIC X(8).
          02 MES-CONTAB-LLEGADA-W                PIC XX.
          02 COD-LLEGADA-W.
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
        
       01 DATOS-ENVIO.
          02 SW-INVALID-ENV                      PIC XX.
          02 FILLER                              PIC X      VALUE "|".
          02 LLAVE-GRUPO-ART-ENV                 PIC X(3).
          02 FILLER                              PIC X      VALUE "|".
          02 NUMERO-ART-ENV                      PIC X(13).
          02 FILLER                              PIC X      VALUE "|".
          02 CLASE-ART-ENV                       PIC XX.
          02 FILLER                              PIC X      VALUE "|".
          02 COD-ART-ENV                         PIC X(18).
          02 FILLER                              PIC X      VALUE "|".
          02 DESCRIP-ART-ENV                     PIC X(50).
          02 FILLER                              PIC X      VALUE "|".  
          02 COD-BARRAS-ART-ENV                  PIC X(15).
          02 FILLER                              PIC X      VALUE "|".  
          02 USO-ART-ENV                         PIC XX.
          02 FILLER                              PIC X      VALUE "|".     
          02 MARCA-ART-ENV                       PIC X(17).
          02 FILLER                              PIC X      VALUE "|".
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
          02 OTROS-ART-ENV                       PIC X(188).
          02 FILLER                              PIC X      VALUE "|".
          02 CTA-ART-ENV                         PIC X(11).
          02 FILLER                              PIC X      VALUE "|".
          02 ESTADO-ART-ENV                      PIC 9.
          02 FILLER                              PIC X      VALUE "|".     
          02 DCTO-GRAL-VTA-ART-ENV               PIC 9(3)V99.
          02 FILLER                              PIC X      VALUE "|".
          02 AUTORET-ART-ENV                     PIC 9(5)V9.
          02 FILLER                              PIC X      VALUE "|".            
          02 PAQUETES-ART-ENV                    PIC 99.
          02 FILLER                              PIC X      VALUE "|".      
          02 UNID-CONV-ART-ENV                   PIC 9(4)V99.  
          02 FILLER                              PIC X      VALUE "|".
          02 STOCK-ART-ENV.       
             05 STOCK-MIN-ART-ENV                PIC 9(6).
             05 STOCK-MAX-ART-ENV                PIC 9(6).
             05 STOCK-ALMAC-ART-ENV              PIC 9(5).
          02 FILLER                              PIC X      VALUE "|".
          02 POL-ART-ENV                         PIC XX.
          02 FILLER                              PIC X      VALUE "|".
          02 MERMA-ART-ENV                       PIC 99.
          02 FILLER                              PIC X      VALUE "|".
          02 FORMA-LIQ-ART-ENV                   PIC X.
          02 FILLER                              PIC X      VALUE "|".
          02 PESO-ART-ENV                        PIC 9(6)V999.
          02 FILLER                              PIC X      VALUE "|".
          02 IVA-ART-ENV                         PIC X.
          02 FILLER                              PIC X      VALUE "|".
          02 VLR-ULT-COMPRA-ENV                  PIC 9(12)V99.
          02 FILLER                              PIC X      VALUE "|".
          02 ULT-CANT-COMPRA-ART-ENV             PIC 9(8).
          02 FILLER                              PIC X      VALUE "|".    
          02 ANO-ULT-COMPRA-ENV                  PIC 9(4).
          02 FILLER                              PIC X      VALUE "|".
          02 MES-ULT-COMPRA-ENV                  PIC 99.
          02 FILLER                              PIC X      VALUE "|".
          02 DIA-ULT-COMPRA-ENV                  PIC 99.
          02 FILLER                              PIC X      VALUE "|".
          02 COLOR-ART-ENV                       PIC X(20).
          02 FILLER                              PIC X      VALUE "|".     
          02 ANO-LISTA-COMP-ENV                  PIC 9(4).
          02 FILLER                              PIC X      VALUE "|".
          02 MES-LISTA-COMP-ENV                  PIC 99.
          02 FILLER                              PIC X      VALUE "|". 
          02 DIA-LISTA-COMP-ENV                  PIC 99.
          02 FILLER                              PIC X      VALUE "|".
          02 TALLA-ART-ENV                       PIC X(3).
          02 FILLER                              PIC X      VALUE "|".    
          02 VLR-LISTA-COMP-ART-ENV              PIC 9(12)V99.
          02 FILLER                              PIC X      VALUE "|".
          02 CCOSTO-ART-ENV                      PIC X(4).       
          02 FILLER                              PIC X      VALUE "|".
          02 VLR-REF-ART-ENV                     PIC 9(12)V99.
          02 FILLER                              PIC X      VALUE "|".
          02 VLR-RAZONABLE-ART-ENV               PIC 9(12)V99.
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
          02 FILLER                              PIC X      VALUE "|". 
          02 TABLA-VTA-ART-ENV.
             05 VR-VENTA1-ART-ENV                PIC 9(12)V99.
             05 PORC-INC-COMP-ART-ENV            PIC 9(3)V99.
             05 VR-INCREM-ART-ENV                PIC 9(12)V99.
             05 PORC-INC-VTA-AR-ENV              PIC 9(3)V99.
          02 FILLER                              PIC X      VALUE "|".


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
              MOVE "INV403_03"              TO MSJ3-HTML
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
              MOVE "INV403_03"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MAESTRO-ARTICULOS.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MENSAJE1-HTML
              MOVE NOM-ART-W                TO MENSAJE2-HTML
              MOVE "INV403_03"              TO MENSAJE3-HTML
              GO TO ENVIAR-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PLANO-W              TO MSJ2-HTML
              MOVE "INV103-03"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
        I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PREFIJOS.
        ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PREF-W               TO MSJ2-HTML
              MOVE "INV103-03"              TO MSJ3-HTML
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
              INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, LLAVE-NRO-ART-LLEGADA-W
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGAD-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.


       INICIAR-SESION.
           MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W
          
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.

       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
      
           OPEN INPUT ARCHIVO-USUNET
      
           INITIALIZE LLAVE-USUNET.

       LEER-USUARIO.           
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
           CLOSE ARCHIVO-USUNET.
      
           IF NOMBRE-USUNET = SPACES
              MOVE "Validacion de usuarios"      TO MENSAJE1-HTML
              MOVE "Falta configurar usuario"    TO MENSAJE2-HTML
              MOVE "Sc"                          TO MENSAJE3-HTML
              GO TO ENVIAR-ERROR
           END-IF.
 
       ASIGNAR-NOMBRES.                  
         
           INITIALIZE NOM-ART-W 
                      NOM-PREF-W
 
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"
 
           MOVE "\\" TO NOM-ART-W

           INSPECT NOM-ART-W REPLACING FIRST "                    "
                                          BY IP-DATOS-USUNET
           INSPECT NOM-ART-W REPLACING FIRST " "
                                          BY "\"          
           INSPECT NOM-ART-W REPLACING FIRST "        "
                                          BY DIR-CONTAB-LLEGADA-W          
           INSPECT NOM-ART-W REPLACING FIRST " "
                                          BY "\"

           MOVE NOM-ART-W TO NOM-PREF-W

           INSPECT NOM-ART-W REPLACING FIRST "                      "
                                          BY "CONTROL\SC-MAESART.DAT".
           INSPECT NOM-PREF-W REPLACING FIRST "                       "
                                           BY "CONTROL\SC-ARCHPREF.DAT".

           INITIALIZE NOM-PLANO-W

           MOVE "\\" TO NOM-PLANO-W

           INSPECT NOM-PLANO-W REPLACING FIRST "                    "
                                         BY IP-DATOS-USUNET

           INSPECT NOM-PLANO-W REPLACING FIRST  "                 "
                                            BY  "\PROG\DATOS\JSON\"  

           INSPECT NOM-PLANO-W REPLACING FIRST "                                              "
                                            BY "JSC-ARCHMAE-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"

           INSPECT NOM-PLANO-W REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                                            BY LLAVE-SESION-LLEGAD-W.

       ABRIR-MAESTRO-ARTICULOS.
           OPEN INPUT ARCHIVO-PREFIJOS.
           OPEN INPUT MAESTRO-ARTICULOS.

           READ ARCHIVO-PREFIJOS NEXT WITH NO LOCK.

           INITIALIZE TABLA-DESCRIP-W

           PERFORM MOVER-DESCRIPCION VARYING I FROM 1 BY 1
                                     UNTIL I > 20.

           MOVE "00"               TO SW-INVALID-ENV.

       BUSQUEDA-MAESTRO-ARTICULOS.
           MOVE COD-LLEGADA-W TO COD-ART
           START MAESTRO-ARTICULOS KEY >= COD-ART
                 INVALID KEY
                   MOVE "01"         TO SW-INVALID-ENV
           END-START.

           READ MAESTRO-ARTICULOS WITH NO LOCK
                INVALID KEY
                  MOVE "01"          TO SW-INVALID-ENV
                  MOVE COD-LLEGADA-W TO COD-ART 
           END-READ.

       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-JSON.

           MOVE "{*TABLA*:[" TO  DATOS-JSON
           INSPECT DATOS-JSON  REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON  END-WRITE
           MOVE 0 TO SW-FIN.

       ENVIO-DATOS-JSON.
           PERFORM MOVER-VRART VARYING I FROM 1 BY 1
                               UNTIL I > 20.

       CERRAR-JSON.
           INITIALIZE LIN-1

           IF CONT = ZEROS 
              INITIALIZE VR-VENTA1-ART-J       
                         PORC-INC-COMP-ART-J     
                         VR-INCREM-ART-J        
                         PORC-INC-VTA-ART-J   
                                                                 
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
           END-IF.
           MOVE LIN-1 TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
          
           MOVE "]}" TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE

            
           MOVE LLAVE-GRUPO-ART    TO LLAVE-GRUPO-ART-ENV            
           MOVE NUMERO-ART         TO NUMERO-ART-ENV  
           MOVE CLASE-ART          TO CLASE-ART-ENV        
           MOVE COD-ART            TO COD-ART-ENV
           MOVE DESCRIP-ART        TO DESCRIP-ART-ENV 
           MOVE COD-BARRAS-ART     TO COD-BARRAS-ART-ENV
           MOVE USO-ART            TO USO-ART-ENV 
           MOVE MARCA-ART          TO MARCA-ART-ENV            
           MOVE REF-ART            TO REF-ART-ENV      
           MOVE VISUAL-WEB-ART     TO VISUAL-WEB-ART-ENV       
           MOVE ING-ACT-ART        TO ING-ACT-ART-ENV           
           MOVE ACOMPA-ART         TO ACOMPA-ART-ENV
           MOVE NIT-ART            TO NIT-ART-ENV                 
           MOVE OTROS-ART          TO OTROS-ART-ENV             
           MOVE CTA-ART            TO CTA-ART-ENV              
           MOVE ESTADO-ART         TO ESTADO-ART-ENV                
           MOVE DCTO-GRAL-VTA-ART  TO DCTO-GRAL-VTA-ART-ENV                 
           MOVE AUTORET-ART        TO AUTORET-ART-ENV             
           MOVE PAQUETES-ART       TO PAQUETES-ART-ENV                   
           MOVE UNID-CONV-ART      TO UNID-CONV-ART-ENV 
           MOVE STOCK-ART  (I)     TO STOCK-ART-ENV     
           MOVE POL-ART            TO POL-ART-ENV              
           MOVE MERMA-ART          TO MERMA-ART-ENV           
           MOVE FORMA-LIQ-ART      TO FORMA-LIQ-ART-ENV       
           MOVE PESO-ART           TO PESO-ART-ENV             
           MOVE IVA-ART            TO IVA-ART-ENV         
           MOVE VLR-ULT-COMPRA     TO VLR-ULT-COMPRA-ENV      
           MOVE ULT-CANT-COMPRA-ART TO ULT-CANT-COMPRA-ART-ENV   
           MOVE ANO-ULT-COMPRA     TO ANO-ULT-COMPRA-ENV    
           MOVE MES-ULT-COMPRA     TO MES-ULT-COMPRA-ENV        
           MOVE DIA-ULT-COMPRA     TO DIA-ULT-COMPRA-ENV      
           MOVE COLOR-ART          TO COLOR-ART-ENV            
           MOVE ANO-LISTA-COMP     TO ANO-LISTA-COMP-ENV        
           MOVE MES-LISTA-COMP     TO MES-LISTA-COMP-ENV       
           MOVE DIA-LISTA-COMP     TO DIA-LISTA-COMP-ENV        
           MOVE TALLA-ART          TO TALLA-ART-ENV           
           MOVE VLR-LISTA-COMP-ART TO VLR-LISTA-COMP-ART-ENV     
           MOVE CCOSTO-ART         TO CCOSTO-ART-ENV             
           MOVE VLR-REF-ART        TO VLR-REF-ART-ENV        
           MOVE VLR-RAZONABLE-ART  TO VLR-RAZONABLE-ART-ENV    
           MOVE VLR-RESIDUAL-ART   TO VLR-RESIDUAL-ART-ENV      
           MOVE VIDA-UTIL-ART      TO VIDA-UTIL-ART-ENV         
           MOVE OBSERV-FACT-ART    TO OBSERV-FACT-ART-ENV        
           MOVE UBICAC-ART         TO UBICAC-ART-ENV            
           MOVE PORC_IMPOCONS-ART  TO PORC_IMPOCONS-ART-ENV     
           MOVE VLR_IMPOCONS-ART   TO VLR_IMPOCONS-ART-ENV     
           MOVE TABLA-VTA-ART (I)  TO TABLA-VTA-ART-ENV.


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
         
           MOVE VR-VENTA1-ART(I)      TO VR-VENTA1-ART-J       
           MOVE PORC-INC-COMP-ART (I) TO PORC-INC-COMP-ART-J     
           MOVE VR-INCREM-ART (I)     TO VR-INCREM-ART-J        
           MOVE PORC-INC-VTA-ART (I) TO PORC-INC-VTA-ART-J

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
           MOVE "," TO CIERRE-LIN-1

           MOVE LIN-1 TO DATOS-JSON
           WRITE REG-JSON END-WRITE.       

       COPY "..\..\FUENTES\SC-WEB19.CBL".
