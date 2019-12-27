      *=================================================================
      * CREACION DE ARCHIVO JSON INV103 MAESTRO ARTICULOS 
      * CONSULTA DE DATOS 
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

           SELECT ARCHIVO-JSON LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.

       
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-ARTIC.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON      PIC X(500).



005190 WORKING-STORAGE SECTION.
005200 COPY "..\..\FUENTES\COBW3.CBL".
005210 COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 LINEA-LLEGADA-W                 PIC X(100).
       77 NOM-ART-W                       PIC X(70).
       77 NOM-PLANO-W                     PIC X(90).
       77 CONT                            PIC 9(4).


       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W               PIC X(15).
             03 FECHA-LLEGAD-W            PIC X(8).
             03 HORA-LLEGAD-W             PIC X(6).
          02 DIR-CONTAB-LLEGADA-W         PIC X(30).
          02 MES-CONTAB-LLEGADA-W         PIC XX.
          02 COD-ART-LLEGADA-W.
             03 LLAVE-NRO-ART-LLEGADA-W.
                05 LLAVE-GRUPO-ART-LLEGADA-W.
                   07 TIPO-ART-LLEGADA-W         PIC X.
                   07 GRUPO-ART-LLEGADA-W.
                      09 GRUPO1-ART-LLEGADA-W    PIC X.
                      09 GRUPO2-ART-LLEGADA-W    PIC X.
                05 NUMERO-ART-LLEGADA-W.
                   07 NRO1-ART-LLEGADA-W        PIC XX.
                   07 NRO2-ART-LLEGADA-W        PIC X(11).
             03 CLASE-ART-LLEGADA-W             PIC XX.
          
       

       01 DATOS-ENVIO.
          02 SW-INVALID-ENV               PIC XX.
          02 FILLER                       PIC X      VALUE "|".
          02 LLAVE-GRUPO-ART-ENV          PIC X(3).
          02 FILLER                       PIC X      VALUE "|".
          02 NUMERO-ART-ENV               PIC X(13).
          02 FILLER                       PIC X      VALUE "|".
          02 CLASE-ART-ENV                PIC XX.
          02 FILLER                       PIC X      VALUE "|".
          02 COD-ART-ENV                  PIC X(18).
          02 FILLER                       PIC X      VALUE "|".
          02 DESCRIP-ART-ENV              PIC X(50).
          02 FILLER                       PIC X      VALUE "|".  
          02 COD-BARRAS-ART-ENV           PIC X(15).
          02 FILLER                       PIC X      VALUE "|".  
          02 USO-ART-ENV                  PIC XX.
          02 FILLER                       PIC X      VALUE "|".     
          02 MARCA-ART-ENV                PIC X(17).
          02 FILLER                       PIC X      VALUE "|".
          02 REF-ART-ENV                  PIC X(15).
          02 FILLER                       PIC X      VALUE "|".     
          02 VISUAL-WEB-ART-ENV           PIC X.
          02 FILLER                       PIC X      VALUE "|".
          02 ING-ACT-ART-ENV              PIC X(2).
          02 FILLER                       PIC X      VALUE "|".
          02 ACOMPA-ART-ENV               PIC X.    
          02 FILLER                       PIC X      VALUE "|".
          02 NIT-ART-ENV                  PIC 9(10).
          02 FILLER                       PIC X      VALUE "|".
          02 OTROS-ART-ENV                PIC X(188).
          02 FILLER                       PIC X      VALUE "|".
          02 CTA-ART-ENV                  PIC X(11).
          02 FILLER                       PIC X      VALUE "|".
          02 ESTADO-ART-ENV               PIC 9.
          02 FILLER                       PIC X      VALUE "|".     
          02 DCTO-GRAL-VTA-ART-ENV        PIC 9(3)V99.
          02 FILLER                       PIC X      VALUE "|".
          02 AUTORET-ART-ENV              PIC 9(5)V9.
          02 FILLER                       PIC X      VALUE "|".            
          02 PAQUETES-ART-ENV             PIC 99.
          02 FILLER                       PIC X      VALUE "|".      
          02 UNID-CONV-ART-ENV            PIC 9(4)V99.  
          02 FILLER                       PIC X      VALUE "|".
          02 STOCK-ART-ENV.       
             05 STOCK-MIN-ART-ENV         PIC 9(6).
             05 STOCK-MAX-ART-ENV         PIC 9(6).
             05 STOCK-ALMAC-ART-ENV       PIC 9(5).
          02 FILLER                       PIC X      VALUE "|".
          02 POL-ART-ENV                  PIC XX.
          02 FILLER                       PIC X      VALUE "|".
          02 MERMA-ART-ENV                PIC 99.
          02 FILLER                       PIC X      VALUE "|".
          02 FORMA-LIQ-ART-ENV            PIC X.
          02 FILLER                       PIC X      VALUE "|".
          02 PESO-ART-ENV                 PIC 9(6)V999.
          02 FILLER                       PIC X      VALUE "|".
          02 IVA-ART-ENV                  PIC X.
          02 FILLER                       PIC X      VALUE "|".
          02 VLR-ULT-COMPRA-ENV           PIC 9(12)V99.
          02 FILLER                       PIC X      VALUE "|".
          02 ULT-CANT-COMPRA-ART-ENV      PIC 9(8).
          02 FILLER                       PIC X      VALUE "|".    
          02 ANO-ULT-COMPRA-ENV           PIC 9(4).
          02 FILLER                       PIC X      VALUE "|".
          02 MES-ULT-COMPRA-ENV           PIC 99.
          02 FILLER                       PIC X      VALUE "|".
          02 DIA-ULT-COMPRA-ENV           PIC 99.
          02 FILLER                       PIC X      VALUE "|".
          02 COLOR-ART-ENV                PIC X(20).
          02 FILLER                       PIC X      VALUE "|".     
          02 ANO-LISTA-COMP-ENV           PIC 9(4).
          02 FILLER                       PIC X      VALUE "|".
          02 MES-LISTA-COMP-ENV           PIC 99.
          02 FILLER                       PIC X      VALUE "|". 
          02 DIA-LISTA-COMP-ENV           PIC 99.
          02 FILLER                       PIC X      VALUE "|".
          02 TALLA-ART-ENV                PIC X(3).
          02 FILLER                       PIC X      VALUE "|".    
          02 VLR-LISTA-COMP-ART-ENV       PIC 9(12)V99.
          02 FILLER                       PIC X      VALUE "|".
          02 CCOSTO-ART-ENV               PIC X(4).       
          02 FILLER                       PIC X      VALUE "|".
          02 VLR-REF-ART-ENV              PIC 9(12)V99.
          02 FILLER                       PIC X      VALUE "|".
          02 VLR-RAZONABLE-ART-ENV        PIC 9(12)V99.
          02 FILLER                       PIC X      VALUE "|".
          02 VLR-RESIDUAL-ART-ENV         PIC 9(12).  
          02 FILLER                       PIC X      VALUE "|". 
          02 VIDA-UTIL-ART-ENV            PIC 9(4).
          02 FILLER                       PIC X      VALUE "|".
          02 OBSERV-FACT-ART-ENV          PIC X.
          02 FILLER                       PIC X      VALUE "|".
          02 UBICAC-ART-ENV               PIC X(5).
          02 FILLER                       PIC X      VALUE "|".
      *    02 PORC_IMPOCONS-ART-ENV        PIC 9(2).
      *    02 FILLER                       PIC X      VALUE "|".
      *    02 VLR_IMPOCONS-ART-ENV         PIC 9(12).
      *    02 FILLER                       PIC X      VALUE "|". 
          02 TABLA-VTA-ART-ENV.
             05 VR-VENTA1-ART-ENV         PIC 9(12)V99.
             05 PORC-INC-COMP-ART-ENV     PIC 9(3)V99.
             05 VR-INCREM-ART-ENV         PIC 9(12)V99.
             05 PORC-INC-VTA-AR-ENV       PIC 9(3)V99.
          02 FILLER                       PIC X      VALUE "|".


       01 LIN-1.  
          02 FILLER                   PIC X(12)  VALUE "{*VR_VENTA*:".
          02 FILLER                   PIC X      VALUE "*".
          02 VR-VENTA1-ART-J          PIC 9(12)V99.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(16)  VALUE "*PORCINC_COMPR*:".
          02 FILLER                   PIC X      VALUE "*".
          02 PORC-INC-COMP-ART-J      PIC 9(3)V99.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(12)  VALUE "*VR_INCREM*:".
          02 FILLER                   PIC X      VALUE "*".
          02 VR-INCREM-ART-J          PIC 9(12)V99.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(14)  VALUE "*PORCINV_VTA*:".
          02 FILLER                   PIC X      VALUE "*".
          02 PORC-INC-VTA-ART-J       PIC 9(3)V99.
          02 FILLER                   PIC XX     VALUE "*}".    
          02 CIERRE-LIN-1             PIC X.


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
              MOVE "ARCHIVO-USUNET"         TO MSJ2-HTML
              MOVE "INV103_13"                 TO MSJ3-HTML
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
              MOVE "INV103-13"              TO MSJ3-HTML
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
              MOVE "INV103-13"              TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE 
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PLANO-W              TO MSJ2-HTML
              MOVE "INV103-13"              TO MSJ3-HTML
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
                    COD-ART-LLEGADA-W
           END-UNSTRING.

001855     MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.
       
000797 INICIAR-SESION.
000806     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W

      *     MOVE "D:\progelect\DATOS\SC-SESION.DAT" TO NOM-SESION-W
000810     GO TO VALIDAR-SESION.
000850
000860 FIN-VALIDAR-SESION.
006840
006900 ABRIR-USUARIO.
006910     INITIALIZE OTR-STAT.
006920     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
 
      *     MOVE "D:\progelect\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
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

        ASIGNAR-NOMBRES. 
           INITIALIZE NOM-ART-W 

           MOVE "\\" TO NOM-ART-W 

           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           INSPECT NOM-ART-W  REPLACING FIRST "                    "
                                           BY IP-DATOS-USUNET

           INSPECT NOM-ART-W  REPLACING FIRST " "
                                           BY "\"

           INSPECT NOM-ART-W  REPLACING FIRST "                              "
                                            BY DIR-CONTAB-LLEGADA-W.
          
           INSPECT NOM-ART-W REPLACING FIRST "                       "
                                          BY "\CONTROL\SC-MAESART.DAT".

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


       ABRIR-MAESTROARTICULOS.
           OPEN INPUT MAESTRO-ARTICULOS.
           MOVE "00"  TO SW-INVALID-ENV.

       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-MAESTRO.

           MOVE "{*TABLA*:[" TO  DATOS-JSON
           INSPECT DATOS-JSON  REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON  END-WRITE
           MOVE 0 TO SW-FIN.


       LEER-MAESTROARTICULOS. 

           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-JSON  
              WRITE REG-JSON  END-WRITE
           END-IF
    
           READ MAESTRO-ARTICULOS NEXT WITH NO LOCK AT END
                GO TO CERRAR-MAESTROARTICULOS
           END-READ.

           IF VR-VENTA1-ART (I) = VR-VENTA1-ART-J 
              MOVE 0 TO SW-FIN 
           ELSE 
               ADD 1 TO CONT
               INITIALIZE LIN-1
              
               MOVE VR-VENTA1-ART(I)      TO VR-VENTA1-ART-J
       
               MOVE PORC-INC-COMP-ART (I) TO PORC-INC-COMP-ART-J     
               MOVE VR-INCREM-ART (I)     TO VR-INCREM-ART-J        
               MOVE PORC-INC-VTA-ART (I) TO PORC-INC-VTA-ART-J     
                              
                           
               INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
               MOVE "," TO CIERRE-LIN-1
             
           END-IF.

           GO TO LEER-MAESTROARTICULOS.
   
       CERRAR-MAESTROARTICULOS.
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
      *     MOVE PORC_IMPOCONS-ART  TO PORC_IMPOCONS-ART-ENV     
      *     MOVE VLR_IMPOCONS-ART   TO VLR_IMPOCONS-ART-ENV     
           MOVE TABLA-VTA-ART (I)  TO TABLA-VTA-ART-ENV.

        CERRAR-MAESTROS.
           CLOSE MAESTRO-ARTICULOS 
                 ARCHIVO-JSON. 

010700  PAGINA-CONFIG.           
010720     MOVE "datosrecibidos" TO COBW3-CNV-NAME
010730     MOVE DATOS-ENVIO       TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV"  USING COBW3
010750
010760     MOVE "..\PAGINAS\RECIBIDOS.ASPX" TO SALIDA-HTML  
010770     PERFORM ABRIR-HTML.
007980   
007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.

008030
008040 COPY "..\..\FUENTES\SC-WEB.CBL".


