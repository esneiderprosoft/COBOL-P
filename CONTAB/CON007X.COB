      *=================================================================
      * CONTABILIDAD - GRABA EL ULTIMO NUMERO DE UNA SECUENCIA.
      *=================================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PROSOFT.
       OBJECT-COMPUTER. PROSOFT.
       INPUT-OUTPUT SECTION.            
       FILE-CONTROL.
            COPY "P:\PROG\PROYECT\FUENTES\FS-USUNET.CBL".
            COPY "P:\PROG\PROYECT\FUENTES\FS-SESION.CBL".
            COPY "P:\PROG\PROYECT\FUENTES\FS-MOVIM.CBL".

           SELECT ARCHIVO-LOTES LOCK MODE IS AUTOMATIC
      	          ASSIGN NOM-LOTE-LNK
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-LOTE
                  ALTERNATE RECORD KEY IS NOMBRE-LOTE WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\FD-USUNET.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-SESION.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-MOVIM.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\FD-LOTES.CBL".
       
       WORKING-STORAGE SECTION.
       COPY "P:\PROG\PROYECT\FUENTES\COBW3.CBL".
       COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC19.CBL".

       77 ULT-NRO-W                    PIC 9(9).

       01 SECU-EDIT.
          02 SEC1-EDIT                 PIC 9.
          02 SEC2-EDIT                 PIC XX.

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(30).
          02 MES-CONTAB-LLEGADA-W      PIC X(5).

       01 DATO-ULT-MOV-W.
          02 LOTE-ULT-MOV-W.
             03 LOTE1-ULT-MOV-W        PIC X.
             03 LOTE2-ULT-MOV-W        PIC X.
          02 FILLER                    PIC X VALUE "|".
          02 NRO-ULT-COMP-W.
             03 NRO1-ULT-COMP-W        PIC 9(3).
             03 NRO2-ULT-COMP-W        PIC 9(6).
          02 FILLER                    PIC X VALUE "|".
          02 FECHA-ULT-MOV-W.
             03 ANO-ULT-MOV-W          PIC 99.
             03 MES-ULT-MOV-W          PIC 99.
             03 DIA-ULT-MOV-W          PIC 99.

       01 DATOS-ENVIO                  PIC X(100).

       LINKAGE SECTION.

       COPY "P:\PROG\PROYECT\FUENTES\ISAPICTX.CBL".

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
              MOVE "CON007X"                TO MSJ3-HTML
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
              MOVE "CON007X"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-LOTES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-LOTE-LNK             TO MSJ2-HTML
              MOVE "CON007X"                TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON MOVIMIENTO-DIARIO.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-MOV                  TO MSJ2-HTML
              MOVE "CON007X"                TO MSJ3-HTML
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

           INITIALIZE DATO-ULT-MOV-W.

           UNSTRING LINEA-LLEGAD-W DELIMITED BY "|"
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, 
                    LOTE-ULT-MOV-W, FECHA-ULT-MOV-W, NRO2-ULT-COMP-W
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGAD-W  TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE  TO FECHA-TOTAL.

       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
           GO TO VALIDAR-SESION.
      
       FIN-VALIDAR-SESION.
      
       ABRIR-USUARIO.
           INITIALIZE OTR-STAT.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
      
           OPEN INPUT ARCHIVO-USUNET
           EVALUATE OTR-STAT
             WHEN "00"  CONTINUE
             WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
                        INITIALIZE REG-USUNET
                        WRITE REG-USUNET
                        CLOSE ARCHIVO-USUNET
007010                  MOVE "SC-2"                        TO MSJ1-HTML
007020                  MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007030                  MOVE "CON007X"                      TO MSJ3-HTML
007040                  GO TO ENVIAR2-ERROR
             WHEN OTHER GO TO CERRAR-SESION
           END-EVALUATE.
      
           INITIALIZE LLAVE-USUNET.

      
       LEER-USUARIO.
           READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.     
           CLOSE ARCHIVO-USUNET.
           
           IF NOMBRE-USUNET = SPACES
007010        MOVE "SC-2"                        TO MSJ1-HTML
007020        MOVE "Falta configurar usuarios"   TO MSJ2-HTML
007030        MOVE "CON007X"                      TO MSJ3-HTML
007040        GO TO ENVIAR2-ERROR
           END-IF.

       ASIGNAR-NOMBRES.

           INITIALIZE NOM-LOTE-LNK NOM-MOV.

           MOVE "\\" TO NOM-LOTE-LNK

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

           INSPECT NOM-LOTE-LNK REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET
           INSPECT NOM-LOTE-LNK REPLACING FIRST " "
                                            BY "\"

           MOVE NOM-LOTE-LNK   TO NOM-MOV

           INSPECT NOM-MOV REPLACING FIRST "                              "
                                        BY DIR-CONTAB-LLEGADA-W.

           INSPECT NOM-LOTE-LNK REPLACING FIRST "                          "
                                            BY "PROG\DATOS\SC-ARCHLOTE.DAT".


       ABRIR-ARCHIVO-LOTES.
           OPEN INPUT ARCHIVO-LOTES
           MOVE LOTE-ULT-MOV-W  TO COD-LOTE
           READ ARCHIVO-LOTES WITH NO LOCK
                INVALID KEY
                   MOVE 1 TO CONSEC-LOTE
           END-READ
           CLOSE ARCHIVO-LOTES.

           IF CONSEC-LOTE = 1
              INSPECT NOM-MOV REPLACING FIRST "                       "
                                             BY "\CONTROL\SC-ARCHMOV.DAT"
           ELSE
              INSPECT NOM-MOV REPLACING FIRST "     "
                                           BY MES-CONTAB-LLEGADA-W
              INSPECT NOM-MOV REPLACING FIRST "              "
                                           BY "SC-ARCHMOV.DAT"
           END-IF.

       ABRIR-MOVIMIENTO.
           OPEN I-O MOVIMIENTO-DIARIO.


       LEER-NUMERACION.
           INITIALIZE MOV-DIARIO.
           MOVE "99999999"             TO LLAVE-COMP-MOV.
           MOVE 0                      TO SEC1-EDIT
           MOVE LOTE-ULT-MOV-W         TO SEC2-EDIT
           MOVE SECU-EDIT              TO SECU-MOV

           READ MOVIMIENTO-DIARIO RECORD KEY IS SECUENCIA-MOV
                INVALID KEY 
                    PERFORM ACTUALIZAR-NUMERO
                    PERFORM EDITAR-DETALLE
                    WRITE MOV-DIARIO END-WRITE
                NOT INVALID KEY
                    PERFORM ACTUALIZAR-NUMERO
                    IF DETALLE-MOV = "ULTIMO COMPROBANTE"
                    OR DETALLE-MOV = SPACES
                       PERFORM EDITAR-DETALLE
                    END-IF
                    REWRITE MOV-DIARIO END-REWRITE
           END-READ.
                                  
 
       CERRAR-ARCHIVOS.
            CLOSE MOVIMIENTO-DIARIO. 
            MOVE DATO-ULT-MOV-W  TO DATOS-ENVIO.

010700 PAGINA-CONFIG.  
010720     MOVE "statuscode" TO COBW3-CNV-NAME
010730     MOVE "00"         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

000570	   MOVE "datosrecibidos" TO COBW3-CNV-NAME
000580     MOVE DATOS-ENVIO      TO COBW3-CNV-VALUE
000590	   CALL "COBW3_SET_CNV"  USING COBW3

010720     MOVE "programa-id" TO COBW3-CNV-NAME
010730     MOVE "CON007X"     TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010760     MOVE "..\PAGINAS\RECIBIDOSPLANO.ASPX" TO SALIDA-HTML
010770     PERFORM ABRIR-HTML.

007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.


       EDITAR-DETALLE.
           EVALUATE LOTE-ULT-MOV-W
               WHEN "1R" MOVE "RECIBOS DE CAJA PPAL"     TO DETALLE-MOV
               WHEN "2R" MOVE "RECIBOS DE CAJA SUC "     TO DETALLE-MOV
               WHEN "3R" MOVE "RECIBOS DE CAJA SUC "     TO DETALLE-MOV
               WHEN "4R" MOVE "RECIBOS DE CAJA SUC "     TO DETALLE-MOV
               WHEN "5R" MOVE "RECIBOS DE CAJA SUC "     TO DETALLE-MOV
               WHEN "6R" MOVE "RECIBOS DE CAJA SUC "     TO DETALLE-MOV
               WHEN "7R" MOVE "RECIBOS DE CAJA SUC "     TO DETALLE-MOV
               WHEN "8R" MOVE "RECIBOS DE CAJA SUC "     TO DETALLE-MOV
               WHEN "9R" MOVE "RECIBOS DE CAJA SUC "     TO DETALLE-MOV
               WHEN "1C" MOVE "COMPROBANTE DE EGRESO"    TO DETALLE-MOV
               WHEN "2C" MOVE "COMP. EGRESO SUCURSAL"    TO DETALLE-MOV
               WHEN "3C" MOVE "COMP. EGRESO SUCURSAL"    TO DETALLE-MOV
               WHEN "4C" MOVE "COMP. EGRESO SUCURSAL"    TO DETALLE-MOV
               WHEN "5C" MOVE "COMP. EGRESO SUCURSAL"    TO DETALLE-MOV
               WHEN "6C" MOVE "COMP. EGRESO SUCURSAL"    TO DETALLE-MOV
               WHEN "7C" MOVE "COMP. EGRESO SUCURSAL"    TO DETALLE-MOV
               WHEN "8C" MOVE "COMP. EGRESO SUCURSAL"    TO DETALLE-MOV
               WHEN "9C" MOVE "COMP. EGRESO SUCURSAL"    TO DETALLE-MOV
               WHEN "1G" MOVE "NOTA CONTAB PRINCIPAL"    TO DETALLE-MOV
               WHEN "2G" MOVE "NOTA CONTAB. SUCURSAL"    TO DETALLE-MOV
               WHEN "3G" MOVE "NOTA CONTAB. SUCURSAL"    TO DETALLE-MOV
               WHEN "4G" MOVE "NOTA CONTAB. SUCURSAL"    TO DETALLE-MOV
               WHEN "5G" MOVE "NOTA CONTAB. SUCURSAL"    TO DETALLE-MOV
               WHEN "6G" MOVE "NOTA CONTAB. SUCURSAL"    TO DETALLE-MOV
               WHEN "7G" MOVE "NOTA CONTAB. SUCURSAL"    TO DETALLE-MOV
               WHEN "8G" MOVE "NOTA CONTAB. SUCURSAL"    TO DETALLE-MOV
               WHEN "1Z" MOVE "NOTAS BANCARIAS PPAL "    TO DETALLE-MOV
               WHEN "2Z" MOVE "NOTAS BANCARIAS SUCUR"    TO DETALLE-MOV
               WHEN "3Z" MOVE "NOTAS BANCARIAS SUCUR"    TO DETALLE-MOV
               WHEN "4Z" MOVE "NOTAS BANCARIAS SUCUR"    TO DETALLE-MOV
               WHEN "5Z" MOVE "NOTAS BANCARIAS SUCUR"    TO DETALLE-MOV
               WHEN "6Z" MOVE "NOTAS BANCARIAS SUCUR"    TO DETALLE-MOV
               WHEN "7Z" MOVE "NOTAS BANCARIAS SUCUR"    TO DETALLE-MOV
               WHEN "8Z" MOVE "NOTAS BANCARIAS SUCUR"    TO DETALLE-MOV
               WHEN "9Z" MOVE "NOTAS BANCARIAS SUCUR   " TO DETALLE-MOV
               WHEN "1F" MOVE "FACTURACION PRINCIPAL   " TO DETALLE-MOV
               WHEN "2F" MOVE "FACTURACION SUCURSAL 2  " TO DETALLE-MOV
               WHEN "3F" MOVE "FACTURACION SUCURSAL 3  " TO DETALLE-MOV
               WHEN "4F" MOVE "FACTURACION SUCURSAL 4  " TO DETALLE-MOV
               WHEN "5F" MOVE "FACTURACION SUCURSAL 5  " TO DETALLE-MOV
               WHEN "6F" MOVE "FACTURACION SUCURSAL 6  " TO DETALLE-MOV
               WHEN "7F" MOVE "FACTURACION SUCURSAL 7  " TO DETALLE-MOV
               WHEN "8F" MOVE "FACTURACION SUCURSAL 8  " TO DETALLE-MOV
               WHEN "9F" MOVE "FACTURACION SUCURSAL    " TO DETALLE-MOV
               WHEN "9P" MOVE "PENDIENTES DE ENTREGA   " TO DETALLE-MOV
               WHEN "9Q" MOVE "ENTREGA DE PENDIENTES   " TO DETALLE-MOV
               WHEN "9C" MOVE "COTIZACIONES                   " TO DETALLE-MOV
               WHEN "C1" MOVE "ORDENES PREST. SERVIC   " TO DETALLE-MOV
               WHEN "C2" MOVE "CONTRATO PREST. SERV.   " TO DETALLE-MOV
               WHEN "C3" MOVE "CONTRATO SUMINISTRO     " TO DETALLE-MOV
               WHEN "C4" MOVE "CONTRATO DE OBRA               " TO DETALLE-MOV
               WHEN "RP" MOVE "PROG. PRODUCC S/N PED   " TO DETALLE-MOV
               WHEN "1A" MOVE "CUENTA DE COBRO CARTERA " TO DETALLE-MOV
               WHEN "RF" MOVE "REMISIONES                     " TO DETALLE-MOV
               WHEN "R1" MOVE "ENTRADA MERCANC. REMIS. " TO DETALLE-MOV
               WHEN "R2" MOVE "REGISTRO MERCANC. REMIS " TO DETALLE-MOV
               WHEN "RE" MOVE "CARTERA RED EXTERNA     " TO DETALLE-MOV
               WHEN "70" MOVE "AUT. SERV. DROG. 9A1    " TO DETALLE-MOV
               WHEN "71" MOVE "AUT. SERV. CIRU. 9A1    " TO DETALLE-MOV
               WHEN "72" MOVE "AUT. SERV. LABO. 9A1    " TO DETALLE-MOV
               WHEN "73" MOVE "AUT. SERV. IMAG. 9A1    " TO DETALLE-MOV
               WHEN "74" MOVE "AUT. SERV. OTROS 9A1    " TO DETALLE-MOV
               WHEN "75" MOVE "AUT. SERV. CONS. 9A1    " TO DETALLE-MOV
               WHEN "77" MOVE "AUT. SERV. P Y P 9A1    " TO DETALLE-MOV
               WHEN "80" MOVE "COMPROB SERV. DROG.     " TO DETALLE-MOV
               WHEN "81" MOVE "COMPROB SERV. CIRUGIA   " TO DETALLE-MOV
               WHEN "82" MOVE "COMPROB SERV. LABORAT   " TO DETALLE-MOV
               WHEN "83" MOVE "COMPROB SERV. IMAGEN.   " TO DETALLE-MOV
               WHEN "84" MOVE "COMPROB SERV. OTROS     " TO DETALLE-MOV
               WHEN "85" MOVE "COMPROB SERV. CONSULT   " TO DETALLE-MOV
               WHEN "87" MOVE "COMPROB SERV. P.Y.P.    " TO DETALLE-MOV
               WHEN "89" MOVE "FACTURAS CONTADO               " TO DETALLE-MOV
               WHEN "9A" MOVE "FACTURAS AMBULATORIO    " TO DETALLE-MOV
               WHEN "9P" MOVE "FACTURAS HOSPITALIZAC   " TO DETALLE-MOV
               WHEN "9T" MOVE "FACTURAS TRANSITO       " TO DETALLE-MOV
               WHEN "9G" MOVE "RECEPCION DE GLOSAS     " TO DETALLE-MOV
               WHEN "9P" MOVE "PENDIENTES DE ENTREGA   " TO DETALLE-MOV
               WHEN "LT" MOVE "LOTES FARMACEUTICOS     " TO DETALLE-MOV
               WHEN "HP" MOVE "PEDIDOS DE FARMACIA     " TO DETALLE-MOV
               WHEN "AS" MOVE "AUTORIZ. SERV. URGENC   " TO DETALLE-MOV
               WHEN "AI" MOVE "AUTORIZ. INCAPACIDAD    " TO DETALLE-MOV
           END-EVALUATE.

       ACTUALIZAR-NUMERO.
           IF LOTE2-ULT-MOV-W = "Q"
              MOVE NRO-ULT-COMP-W   TO ULT-NROPQ-MOV
           ELSE
              MOVE NRO2-ULT-COMP-W  TO ULT-NRO-MOV
           END-IF

           IF LOTE-ULT-MOV-W = "LT"
              MOVE NRO-ULT-COMP-W   TO CONSEC-LTF-MOV
           END-IF

           IF FECHA-ULT-MOV-W > FECHA-VENCE-MOV
           OR ANO-VENCE-MOV > 89
           OR MES-VENCE-MOV > 12
           OR DIA-VENCE-MOV > 31
              MOVE FECHA-ULT-MOV-W TO FECHA-VENCE-MOV
           END-IF.
008040 COPY "P:\PROG\PROYECT\FUENTES\SC-WEB19.CBL".
