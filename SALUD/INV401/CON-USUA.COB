004820*=================================================================
004830* FACTURACION - CONSULTA DATOS DE USUARIO
004840*=================================================================
004850 IDENTIFICATION DIVISION.
004860 PROGRAM-ID. "HttpExtensionProc".
004870 ENVIRONMENT DIVISION.
004880 CONFIGURATION SECTION.
004890 INPUT-OUTPUT SECTION.
004900 FILE-CONTROL.
004910     COPY "..\..\FUENTES\FS-USUNET.CBL".
004920     COPY "..\..\FUENTES\FS-SESION.CBL".
  
           SELECT REGCONT LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-REGCONT-W
      * "SC-REGCONT.DAT"
                  FILE STATUS OTR-STAT.

           SELECT ARCHIVO-USUARIOS LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-USUAR-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC;
                  RECORD KEY IS NOMBRE-USU
                  ALTERNATE RECORD KEY IS CTL-USU     WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-USU   WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CLAVE-2-USU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.

           SELECT ARCHIVO-JSON
                  ASSIGN NOM-PLANO-W
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS OTR-STAT.

005070 DATA DIVISION.
005090 FILE SECTION.
005100 COPY "..\..\FUENTES\FD-USUNET.CBL".
005110 COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-USUAR.CBL".	   
       COPY "..\..\FUENTES\FD-FECHA.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-PLANO.
           02 DATOS-PLANO     PIC X(3500).

005190 WORKING-STORAGE SECTION.
005200 COPY "..\..\FUENTES\COBW3.CBL".
005210 COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-USUAR-W                PIC X(70).
       77 NOM-REGCONT-W              PIC X(70).
       77 NOM-PLANO-W                PIC X(90).
       77 MES-SAL-W                  PIC X(4).
       77 PORC-CREE-EDIT             PIC Z.ZZ.

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGAD-W.
             03 ID-LLEGAD-W          PIC X(15).
             03 FECHA-LLEGAD-W       PIC X(8).
             03 HORA-LLEGAD-W        PIC X(6).
          02 DIR-CONTAB-LLEGADA-W    PIC X(8).
          02 MES-CONTAB-LLEGADA-W    PIC XX.
         
       01 LIN-1.
          02 FILLER                  PIC X(10)  VALUE "{*NOMBRE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOMBRE-USU-J            PIC X(50).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*NIT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NIT-USU-J               PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(5)   VALUE "*DV*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DV-USU-J                PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*DIRECC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIRECC-USU-J            PIC X(50).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*COD_CIUD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-CIU-USU-J           PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*CONTROL_USU*:".
          02 FILLER                  PIC X      VALUE "*".

          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*TEL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TEL-USU-J               PIC X(15).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*NUIR*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NUIR-USU-J              PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*PREFIJ*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PREFIJO-USU-J           PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*LIST_POL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LISTA-POL-USU-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*IVA_S*:".
          02 FILLER                  PIC X      VALUE "*".
          02 IVA-S-USU-J             PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*NOMBRE_ALT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOMBRE-ALTERNO-J        PIC X(50).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*NIT_ALT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NIT-ALT-J               PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*DV_ALT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DV-ALT-J                PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*DIRECC_ALT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIRECC-ALT-J            PIC X(50).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*COD_CIUD_ALT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-CIU-ALT-J           PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*TEL_ALT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TEL-ALT-J               PIC X(15).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*PREFIJ_ALT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PREFIJO-ALT-J           PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*LIST_POL_ALT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LISTA-POL-ALT-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*IVA_S_ALT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 IVA-S-ALT-J             PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*LIMI_COSTO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LIMI-COSTO-USU-J        PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*EXTEN_FACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EXTEN-FAC-USU-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*CTA_EFECT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CTA-EFECT-USU-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*RETAIL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 RETAIL-USU-J            PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*APROX*:".
          02 FILLER                  PIC X      VALUE "*".
          02 APROX-USU-J             PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*REPETIDO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 REPETIDO-USU-J          PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*DESCTO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCTO-USU-J            PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*RETENEDOR*:".
          02 FILLER                  PIC X      VALUE "*".
          02 RETENEDOR-USU-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*CONTADO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CONTADO-USU-J           PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*PRESUP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PRESUP-USU-J            PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*CTL_CHEQ*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CTL-CHEQ-USU-J          PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*DEP_INVEN*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DEP-INVEN-USU-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*FORMAT_RBO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FORMATO-RBO-USU-J       PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*SEPARA_CAJA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SEPARA-CAJA-ALT-J       PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*LIMIT_VENTA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LIMIT-VENTA-USU-J       PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*CUADRE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CUADRE-USU-J            PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)  VALUE "*COSTO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COSTO-USU-J             PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*CTL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CTL-USU-J               PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*INVENT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 INVENT-USU-J            PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*CARTERA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CARTERA-USU-J           PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*TIPO_COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-COD-USU-J          PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*COPIA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COPIA-USU-J             PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*PUC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PUC-USU-J               PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*DIRECC_USU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIRECTORIO-USU-J        PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*ARCHMAE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ARCHMAE-USU-J           PIC X(50).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*ARCHTER*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ARCHTER-USU-J           PIC X(50).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*BARRAS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 BARRAS-USU-J            PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(6)   VALUE "*POS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 POS-USU-J               PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*ASUME_IVA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ASUME-IVA-USU-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*ASUME_VLR*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ASUME-VLR-USU-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*IVA1*:".
          02 FILLER                  PIC X      VALUE "*".
          02 IVA-USU-J               PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*IVA2*:".
          02 FILLER                  PIC X      VALUE "*".
          02 IVA-2-USU-J             PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*IVA3*:".
          02 FILLER                  PIC X      VALUE "*".
          02 IVA-3-USU-J             PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*AUTORET*:".
          02 FILLER                  PIC X      VALUE "*".
          02 AUTORET-USU-J           PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*RESOL_DIAN*:".
          02 FILLER                  PIC X      VALUE "*".
          02 RESOL-DIAN-USU-J        PIC X(70).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*CLAVE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLAVE-USU-J             PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*CLAVE_NIT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLAVE-NIT-USU-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*CLAVE_INV*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLAVE-INV-USU-J         PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*CLAVE_MEN*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLAVE-MEN-USU-J         PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*CLAVE_CON*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLAVE-CON-USU-J         PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*CLAVE_CAR*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLAVE-CAR-USU-J         PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*CLAVE_OTR*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLAVE-OTR-USU-J         PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*CLAVE_MEN_INV*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLAVE-MEN-INV-J         PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*CLAVE_2*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLAVE-2-USU-J           PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*BARRAS_FACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 BARRAS-FACT-USU-J       PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*ACTIU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ACTIV-USU-J             PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*CLAVE_CORREC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLAVE-CORREC-USU-J      PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*RET_FACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 RET-FACT-USU-J          PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*PORC_CREE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PORC-CREE-USU-J         PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*NOM_EGRESO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOM-EGRESO-USU-J        PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*FORMAT_EGR*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FORMATO-EGR-USU-J       PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(18)  VALUE "*F_LIQ_BASE_FACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 F-LIQ-BASE-FACT-USU-J   PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*DESPACHO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESPACHO-USU-J          PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*NIIF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NIIF-USU-J              PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*COSTO_ART*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COSTO-ART-USU-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*MULT_RUBRO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 MULT-RUBRO-USU-J        PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*DIR_DIAN*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIR-DIAN-USU-J          PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*FACT_INF_COSTO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FACT-INF-COSTO-USU-J    PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*ALTERNA_TRANSP*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ALERTA-TRANSP-USU-J     PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*PER_INI_NIIF*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PER-INI-NIIF-USU-J      PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*SAL_MIN*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SAL-MIN-USU-J           PIC X(9).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*MENS_MENU*:".
          02 FILLER                  PIC X      VALUE "*".
          02 MENS-MENU-USU-J         PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*LEY1429*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LEY1429-USU-J           PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*MENU_SUC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 MENU-SUC-USU-J          PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*SUP_DEVENTA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SUP-DEVENTA-USU-J       PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*LOTE_FARM*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LOTE-FARM-USU-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*TIP_CAJ*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIP-CAJ-USU-J           PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*NOTA_ENTREGA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOTA-ENTREGA-USU-J      PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(8)   VALUE "*PLACA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PLACA-USU-J             PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*CUOTAS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CUOTAS-USU-J            PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*RESTRIC_EX*:".
          02 FILLER                  PIC X      VALUE "*".
          02 RESTRIC-EX-USU-J        PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*MAX_MORA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 MAX-MORA-USU-J          PIC X(3).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*CONTAB_POS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CONTAB-POS-USU-J        PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*RETEIVA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 RETEIVA-USU-J           PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*PEDIDO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PEDIDO-USU-J            PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*AUTOCREE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 AUTOCREE-USU-J          PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*DIR_INV*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIR-INV-USU-J           PIC X(50).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*TIPO_EMPRE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-EMPR-USU-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*E_MAIL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 E-MAIL-USU-J            PIC X(60).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*SERV_EMAIL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SERV-EMAIL-USU-J        PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(14)  VALUE "*CLAVE_EMAIL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLAVE-EMAIL-USU-J       PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*SSL_EMAIL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SSL-EMAIL-USU-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*ITEM_MAX*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ITEM-MAX-USU-J          PIC X(4).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*IMP_FACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 IMP-FACT-USU-J          PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*IMP_COT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 IMP-COT-USU-J           PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(10)  VALUE "*IMP_ORD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 IMP-ORD-USU-J           PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*LISTA_PRECIO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 LISTA-PRECIO-USU-J      PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*EMAIL_FACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EMAIL-FACT-USU-J        PIC X(60).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(18)  VALUE "*SERV_EMAIL_FACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SERV-EMAIL-FACT-USU-J   PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(19)  VALUE "*CLAVE_EMAIL_FACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLAVE-EMAIL-FACT-USU-J  PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*SSL_EMAIL_FACT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SSL-EMAIL-FACT-USU-J    PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*EMAIL_NOM*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EMAIL-NOM-USU-J         PIC X(60).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*SERV_EMAIL_NOM*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SERV-EMAIL-NOM-USU-J    PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(18)  VALUE "*CLAVE_EMAIL_NOM*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLAVE-EMAIL-NOM-USU-J   PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(16)  VALUE "*SSL_EMAIL_NOM*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SSL-EMAIL-NOM-USU-J     PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*COSTO_DEUD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COSTO-DEUD-USU-J        PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(18)  VALUE "*RUTA_ESTAMPILLA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 RUTA-ESTAMPILLA-USU-J   PIC X(50).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*AUTORTEICA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 AUTORTEICA-USU-J        PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*RET_CREE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 RET-CREE-USU-J          PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*CONV_IYC_ANUAL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CONV-IYC-ANUAL-USU-J    PIC X(13).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*CONV_IYC_BIMES*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CONV-IYC-BIMES-USU-J    PIC X(13).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(17)  VALUE "*DIRECT_ANT*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIRECTORIO-ANT-USU-J    PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*ACTA_REMI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ACTA-REMI-USU-J         PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(12)  VALUE "*CLAVE_IYC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLAVE-IYC-USU-J         PIC X(6).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*PRECIO_SUC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PRECIO-SUC-USU-J        PIC X.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(13)  VALUE "*IP_PUBLICA*:".
          02 FILLER                  PIC X      VALUE "*".
          02 IP-PUBLICA-USU-J        PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*FECHALNK*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-LNK-J.
             05 ANO-LNK-J            PIC 99.
             05 MES-LNK-J            PIC 99.
             05 DIA-LNK-J            PIC 99.
          02 FILLER                  PIC XX   VALUE "*}".

       01 LINEA-LLEGADA-W            PIC X(100).

       01 DATOS-ENVIO.
          02 ESTADO-ENVIO               PIC XX.

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
              MOVE "INV401"                 TO MSJ3-HTML
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
              MOVE "INV401"                 TO MSJ3-HTML
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
              MOVE "INV401"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON REGCONT.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT             TO MSJ1-HTML
              MOVE NOM-REGCONT-W        TO MSJ2-HTML
              MOVE "INV401"             TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-JSON.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-PLANO-W      TO MSJ2-HTML
              MOVE "INV401"         TO MSJ3-HTML
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
               INTO LLAVE-SESION-LLEGAD-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W
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
007010                  MOVE "Validacion de Usuario"       TO MENSAJE1-HTML
007020                  MOVE "Falta configurar usuarios"   TO MENSAJE2-HTML
007030                  MOVE "Sc"                          TO MENSAJE3-HTML
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
007230        MOVE "Validacion de Sesion"        TO MENSAJE1-HTML
007240        MOVE "Falta configurar usuarios"   TO MENSAJE2-HTML
007250        MOVE "Sc"                          TO MENSAJE3-HTML
007260        GO TO ENVIAR2-ERROR
007270     END-IF.
007280 
007350

        ASIGNAR-NOMBRES.
           EVALUATE MES-CONTAB-LLEGADA-W
             WHEN 01 MOVE "\ENE" TO MES-SAL-W 
             WHEN 02 MOVE "\FEB" TO MES-SAL-W 
             WHEN 03 MOVE "\MAR" TO MES-SAL-W 
             WHEN 04 MOVE "\ABR" TO MES-SAL-W 
             WHEN 05 MOVE "\MAY" TO MES-SAL-W 
             WHEN 06 MOVE "\JUN" TO MES-SAL-W 
             WHEN 07 MOVE "\JUL" TO MES-SAL-W 
             WHEN 08 MOVE "\AGT" TO MES-SAL-W 
             WHEN 09 MOVE "\SEP" TO MES-SAL-W 
             WHEN 10 MOVE "\OCT" TO MES-SAL-W 
             WHEN 11 MOVE "\NOV" TO MES-SAL-W 
             WHEN 12 MOVE "\DIC" TO MES-SAL-W 
           END-EVALUATE

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

001614     INITIALIZE NOM-USUAR-W

           MOVE "\\" TO NOM-USUAR-W

           INSPECT NOM-USUAR-W REPLACING FIRST "                    "
                                            BY IP-DATOS-USUNET

001621     INSPECT NOM-USUAR-W REPLACING FIRST " "
001624                                       BY "\"

007190     INSPECT NOM-USUAR-W REPLACING FIRST "        "
                                            BY DIR-CONTAB-LLEGADA-W

           MOVE NOM-USUAR-W TO NOM-REGCONT-W

           INSPECT NOM-USUAR-W REPLACING FIRST "                       "
007200                                      BY "\CONTROL\SC-ARCHUSU.DAT"

           INSPECT NOM-REGCONT-W REPLACING FIRST "    "
                                              BY MES-SAL-W
           INSPECT NOM-REGCONT-W REPLACING FIRST "               "
                                              BY "\SC-REGCONT.DAT".

000721     MOVE "\\" TO NOM-PLANO-W 

007220     INSPECT NOM-PLANO-W  REPLACING FIRST "                    "
007230                                       BY IP-DATOS-USUNET

007240     INSPECT NOM-PLANO-W  REPLACING FIRST " "
007250                                       BY "\"

007260     INSPECT NOM-PLANO-W  REPLACING FIRST "                                                             "
007280                                       BY "PROG\DATOS\JSON\SC-DAT-USU-XXXXXXXXXXXXXXXXXXXXXXXXXXXXX.JSON"

007281     INSPECT NOM-PLANO-W  REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
007282                                       BY LLAVE-SESION-LLEGAD-W.
           
       ABRIR-ARCHIVO-USUARIOS.

           OPEN INPUT ARCHIVO-USUARIOS.
           READ ARCHIVO-USUARIOS NEXT WITH NO LOCK AT END 
                CLOSE ARCHIVO-USUARIOS
           END-READ.

           OPEN INPUT REGCONT.
           READ REGCONT NEXT WITH NO LOCK AT END 
                CLOSE REGCONT
           END-READ.
     
       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.
           MOVE "{*DATOSUSUA*:[" TO  DATOS-PLANO
           INSPECT DATOS-PLANO REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-PLANO END-WRITE.

       ESCRIBIR-JSON.

          
           MOVE NOMBRE-USU           TO NOMBRE-USU-J
           MOVE NIT-USU              TO NIT-USU-J
           MOVE DV-USU               TO DV-USU-J
           MOVE DIRECC-USU           TO DIRECC-USU-J
           MOVE COD-CIU-USU          TO COD-CIU-USU-J
           MOVE TEL-USU              TO TEL-USU-J
           MOVE NUIR-USU             TO NUIR-USU-J
           MOVE PREFIJO-USU          TO PREFIJO-USU-J
           MOVE LISTA-POL-USU        TO LISTA-POL-USU-J
           MOVE IVA-S-USU            TO IVA-S-USU-J
           MOVE NOMBRE-ALTERNO       TO NOMBRE-ALTERNO-J
           MOVE NIT-ALT              TO NIT-ALT-J
           MOVE DV-ALT               TO DV-ALT-J
           MOVE DIRECC-ALT           TO DIRECC-ALT-J
           MOVE COD-CIU-ALT          TO COD-CIU-ALT-J
           MOVE TEL-ALT              TO TEL-ALT-J
           MOVE PREFIJO-ALT          TO PREFIJO-ALT-J
           MOVE LISTA-POL-ALT        TO LISTA-POL-ALT-J
           MOVE IVA-S-ALT            TO IVA-S-ALT-J
           MOVE LIMI-COSTO-USU       TO LIMI-COSTO-USU-J
           MOVE EXTEN-FAC-USU        TO EXTEN-FAC-USU-J
           MOVE CTA-EFECT-USU        TO CTA-EFECT-USU-J
           MOVE RETAIL-USU           TO RETAIL-USU-J
           MOVE APROX-USU            TO APROX-USU-J
           MOVE REPETIDO-USU         TO REPETIDO-USU-J
           MOVE DESCTO-USU           TO DESCTO-USU-J
           MOVE RETENEDOR-USU        TO RETEIVA-USU-J
           MOVE CONTADO-USU          TO CONTADO-USU-J
           MOVE PRESUP-USU           TO PRESUP-USU-J
           MOVE CTL-CHEQ-USU         TO CTL-CHEQ-USU-J
           MOVE DEP-INVEN-USU        TO DEP-INVEN-USU-J
           MOVE FORMATO-RBO-USU      TO FORMATO-RBO-USU-J
           MOVE SEPARA-CAJA-ALT      TO SEPARA-CAJA-ALT-J
           MOVE LIMIT-VENTA-USU      TO LIMIT-VENTA-USU-J
           MOVE CUADRE-USU           TO CUADRE-USU-J
           MOVE COSTO-USU            TO COSTO-USU-J
           MOVE CTL-USU              TO CTL-USU-J
           MOVE INVENT-USU           TO INVENT-USU-J
           MOVE CARTERA-USU          TO CARTERA-USU-J
           MOVE TIPO-COD-USU         TO TIPO-COD-USU-J
           MOVE COPIA-USU            TO COPIA-USU-J
           MOVE PUC-USU              TO PUC-USU-J
           MOVE DIRECTORIO-USU       TO DIRECTORIO-USU-J
           MOVE ARCHMAE-USU          TO ARCHMAE-USU-J
           MOVE ARCHTER-USU          TO ARCHTER-USU-J
           MOVE BARRAS-USU           TO BARRAS-USU-J
           MOVE POS-USU              TO POS-USU-J
           MOVE ASUME-IVA-USU        TO ASUME-IVA-USU-J
           MOVE ASUME-VLR-USU        TO ASUME-VLR-USU-J
           MOVE IVA-USU              TO IVA-USU-J
           MOVE IVA-2-USU            TO IVA-2-USU-J
           MOVE IVA-3-USU            TO IVA-3-USU-J
           MOVE AUTORET-USU          TO AUTORET-USU-J

           INSPECT RESOL-DIAN-USU REPLACING ALL '"' BY " "
           INSPECT RESOL-DIAN-USU REPLACING ALL "*" BY " "
           INSPECT RESOL-DIAN-USU REPLACING ALL "Ñ" BY CARAC-ENE-MAY 
           INSPECT RESOL-DIAN-USU REPLACING ALL "ñ" BY CARAC-ENE-MIN



           MOVE RESOL-DIAN-USU       TO RESOL-DIAN-USU-J
           MOVE CLAVE-USU            TO CLAVE-USU-J
           MOVE CLAVE-NIT-USU        TO CLAVE-NIT-USU-J
           MOVE CLAVE-INV-USU        TO CLAVE-INV-USU-J
           MOVE CLAVE-MEN-USU        TO CLAVE-MEN-INV-J
           MOVE CLAVE-CON-USU        TO CLAVE-CON-USU-J
           MOVE CLAVE-CAR-USU        TO CLAVE-CAR-USU-J
           MOVE CLAVE-OTR-USU        TO CLAVE-OTR-USU-J
           MOVE CLAVE-MEN-INV        TO CLAVE-MEN-INV-J
           MOVE CLAVE-2-USU          TO CLAVE-2-USU-J
           MOVE BARRAS-FACT-USU      TO BARRAS-FACT-USU-J
           MOVE ACTIV-USU            TO ACTIV-USU-J
           MOVE CLAVE-CORREC-USU     TO CLAVE-CORREC-USU-J
           MOVE RET-FACT-USU         TO RET-FACT-USU-J
           MOVE PORC-CREE-USU        TO PORC-CREE-EDIT
           MOVE PORC-CREE-EDIT       TO PORC-CREE-USU-J
           MOVE NOM-EGRESO-USU       TO NOM-EGRESO-USU-J
           MOVE FORMATO-EGR-USU      TO FORMATO-EGR-USU-J
           MOVE F-LIQ-BASE-FACT-USU  TO F-LIQ-BASE-FACT-USU-J
           MOVE DESPACHO-USU         TO DESPACHO-USU-J
           MOVE NIIF-USU             TO NIIF-USU-J
           MOVE COSTO-ART-USU        TO COSTO-ART-USU-J
           MOVE FACT-INF-COSTO-USU   TO FACT-INF-COSTO-USU-J
           MOVE ALERTA-TRANSP-USU    TO ALERTA-TRANSP-USU-J
           MOVE PER-INI-NIIF-USU     TO PER-INI-NIIF-USU-J
           MOVE SAL-MIN-USU          TO SAL-MIN-USU-J
           MOVE MENS-MENU-USU        TO MENS-MENU-USU-J
           MOVE LEY1429-USU          TO LEY1429-USU-J
           MOVE MENU-SUC-USU         TO MENU-SUC-USU-J
           MOVE SUP-DEVENTA-USU      TO SUP-DEVENTA-USU-J
           MOVE LOTE-FARM-USU        TO LOTE-FARM-USU-J
           MOVE TIP-CAJ-USU          TO TIP-CAJ-USU-J
           MOVE NOTA-ENTREGA-USU     TO NOTA-ENTREGA-USU-J
           MOVE PLACA-USU            TO PLACA-USU-J
           MOVE CUOTAS-USU           TO CUOTAS-USU-J
           MOVE RESTRIC-EX-USU       TO RESTRIC-EX-USU-J
           MOVE MAX-MORA-USU         TO MAX-MORA-USU-J
           MOVE CONTAB-POS-USU       TO CONTAB-POS-USU-J
           MOVE RETEIVA-USU          TO RETEIVA-USU-J
           MOVE PEDIDO-USU           TO PEDIDO-USU-J
           MOVE AUTOCREE-USU         TO AUTOCREE-USU-J
           MOVE DIR-INV-USU          TO DIR-INV-USU-J
           MOVE TIPO-EMPR-USU        TO TIPO-EMPR-USU-J
           MOVE E-MAIL-USU           TO E-MAIL-USU-J
           MOVE SERV-EMAIL-USU       TO SERV-EMAIL-USU-J
           MOVE CLAVE-EMAIL-USU      TO CLAVE-EMAIL-USU-J
           MOVE SSL-EMAIL-USU        TO SSL-EMAIL-USU-J
           MOVE ITEM-MAX-USU         TO ITEM-MAX-USU-J
           MOVE IMP-FACT-USU         TO IMP-FACT-USU-J
           MOVE IMP-COT-USU          TO IMP-COT-USU-J
           MOVE IMP-ORD-USU          TO IMP-ORD-USU-J
           MOVE LISTA-PRECIO-USU     TO LISTA-PRECIO-USU-J
           MOVE EMAIL-FACT-USU       TO EMAIL-FACT-USU-J
           MOVE SERV-EMAIL-FACT-USU  TO SERV-EMAIL-FACT-USU-J
           MOVE CLAVE-EMAIL-FACT-USU TO CLAVE-EMAIL-FACT-USU-J
           MOVE SSL-EMAIL-FACT-USU   TO SSL-EMAIL-FACT-USU-J
           MOVE EMAIL-NOM-USU        TO EMAIL-NOM-USU-J
           MOVE SERV-EMAIL-NOM-USU   TO SERV-EMAIL-NOM-USU-J
           MOVE CLAVE-EMAIL-NOM-USU  TO CLAVE-EMAIL-NOM-USU-J
           MOVE SSL-EMAIL-NOM-USU    TO SSL-EMAIL-NOM-USU-J
           MOVE COSTO-DEUD-USU       TO COSTO-DEUD-USU-J
           MOVE RUTA-ESTAMPILLA-USU  TO RUTA-ESTAMPILLA-USU-J
           MOVE AUTORTEICA-USU       TO AUTORTEICA-USU-J
           MOVE RET-CREE-USU         TO RET-CREE-USU-J
           MOVE CONV-IYC-ANUAL-USU   TO CONV-IYC-ANUAL-USU-J
           MOVE CONV-IYC-BIMES-USU   TO CONV-IYC-BIMES-USU-J
           MOVE DIRECTORIO-ANT-USU   TO DIRECTORIO-ANT-USU-J
           MOVE ACTA-REMI-USU        TO ACTA-REMI-USU-J
           MOVE CLAVE-IYC-USU        TO CLAVE-IYC-USU-J
           MOVE PRECIO-SUC-USU       TO PRECIO-SUC-USU-J
           MOVE IP-PUBLICA-USU       TO IP-PUBLICA-USU-J
           MOVE FECHA-NUM            TO FECHA-LNK-J

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 
           INSPECT LIN-1 REPLACING ALL "\" BY " "
           INSPECT LIN-1 REPLACING ALL "/" BY " "
           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES

           MOVE LIN-1 TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE

           MOVE "]}"  TO DATOS-PLANO
           WRITE REG-PLANO END-WRITE.

        CERRAR-ARCHIVOS.
           CLOSE ARCHIVO-USUARIOS
                 REGCONT
                 ARCHIVO-JSON.

           MOVE "00"                 TO ESTADO-ENVIO.

010700  PAGINA-CONFIG.
010720     MOVE "datosrecibidos"  TO COBW3-CNV-NAME
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
005210* COPY "..\..\FUENTES\WS-HCEVO.CBL".
