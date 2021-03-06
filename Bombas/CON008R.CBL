000002 IDENTIFICATION DIVISION.
000003 PROGRAM-ID. CON008R.
000004*   CONTABILIDAD - LEE EL PLAN DE CUENTAS Y DEVUELVE EL %
000005*                  DE RETENCION.
000006
000008 ENVIRONMENT     DIVISION.
000009 INPUT-OUTPUT SECTION.
000010 FILE-CONTROL.
000024 COPY "P:\PROG\PROYECT\FUENTES\FS-MAEST.CBL".
000025
000039 DATA DIVISION.
000040 FILE SECTION.
000041 COPY "P:\PROG\PROYECT\FUENTES\FD-MAEST.CBL".
000045
000046 WORKING-STORAGE SECTION.
000079 COPY "P:\PROG\PROYECT\FUENTES\WEB-CARAC.CBL".
000086 LINKAGE SECTION.
000087 01 CTA-008R-LNK          PIC X(11).
000088 01 PORC-008R-LNK         PIC 99V99.
000103 01 NOM-MAE-008R-LNK      PIC X(50).
000104 01 INVALID-INV           PIC 99.
000105
000106 PROCEDURE DIVISION USING CTA-008R-LNK PORC-008R-LNK NOM-MAE-008R-LNK INVALID-INV.
000107
000108 DECLARATIVES.
000109 I-O-TEST SECTION.
000110     USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-MAESTROS.
000111
000112 ESCR-EXCEPTIONES.
000113     IF OTR-STAT = "00" OR "35"             
000114        CONTINUE      
000115     ELSE
000116        MOVE OTR-STAT TO INVALID-INV
000117        GO TO SALIR     
000118     END-IF.
000122
000123 END DECLARATIVES.
000124 PROCEDURAL SECTION.
000125 
000130 LEER-MAESTRO.
000133     OPEN INPUT ARCHIVO-MAESTROS.
000134     MOVE CTA-008R-LNK TO CTA-MAE.
000135     MOVE 4            TO TIPO-MAE.
000136     
000137     READ ARCHIVO-MAESTROS WITH NO LOCK
000138          INVALID KEY 
000139            MOVE 0 TO PORCENT-RET-MAE
000140     END-READ.
000141 
000142     CLOSE ARCHIVO-MAESTROS.
000143     MOVE PORCENT-RET-MAE TO PORC-008R-LNK.
000197
000201 SALIR.
000202     EXIT PROGRAM.
000203 FIN.
000204     STOP RUN.
