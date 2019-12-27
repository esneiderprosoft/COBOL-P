      *============================================================
      * CREACION DE ARCHIVO JSON CONSULTA DE CIUDADES F8- CON809 
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

           SELECT ARCHIVO-CIUDAD LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-CIUDAD-W
      * "P:\PROG\DATOS\SC-ARCHCIUD.DAT";
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-CIU
                  ALTERNATE RECORD KEY IS NOMBRE-CIU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.
 
           SELECT ARCHIVO-PAISES LOCK MODE IS AUTOMATIC
	            ASSIGN NOM-PAISES-W
      * "\PROG\DATOS\SC-PAIS.DAT";
                    ORGANIZATION IS INDEXED;
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS COD-PAIS
                    ALTERNATE RECORD KEY IS NOMBRE-PAIS WITH DUPLICATES
                    FILE STATUS IS OTR-STAT.
                                                                         
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-CIUDA.CBL".
       COPY "..\..\FUENTES\FD-PAIS.CBL".
       
  
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".
       77 NOM-PAISES-W                 PIC X(70).
       77 NOM-CIUDAD-W                 PIC X(70).
       77 DATOS-PLANO-W                PIC X(1000).  
       
       77 NOMBRE-EDIT                  PIC X(60).
       77 LN-W                         PIC 99.

       01 TABLA-1.
          02 TABLA-DPTO OCCURS 100.
             03 NOM-DPTO               PIC X(20).
 
       01 DATOS-VENTANA.
          02 DATOS-1 OCCURS 20.
             03 CTA-VEN.
                05 DPTO-VEN  PIC 99.
                05 CIUD-VEN  PIC 9(3).
             03 NOM-VEN      PIC X(20).
             03 PAIS-VEN     PIC X(30).

       01 LLEGADA-W                    PIC X(100).

       01 DATO-LLEGADA-W.
          02 LLAVE-SESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 DIR-CONTAB-LLEGADA-W      PIC X(30).
          02 MES-CONTAB-LLEGADA-W      PIC XX.
          02 COD-CIU-LLEGADA-W         PIC X(4). 
          02 OPERADOR-LLEGADA-W        PIC X(4).


       01 LIN-1.  
          02 FILLER                  PIC X(7)   VALUE "{*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-CIU-J               PIC X(5).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(9)   VALUE "*NOMBRE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOMBRE-CIU-J            PIC X(20).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(7)   VALUE "*PAIS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CODIGO-PAIS-J           PIC X(3). 
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(11)  VALUE "*NOM_PAIS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NOMBRE-PAIS-J           PIC X(60).
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(09)  VALUE "*DEPART*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DEPART-PAIS-J          PIC X(20).
          02 FILLER                  PIC XX     VALUE "*}".    
          02 CIERRE-LIN-1            PIC X.

      
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
              MOVE "ARCHIVO-USUNET"         TO MSJ2-HTML
              MOVE "CON809"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
          
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.         
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE "ARCHIVO-SESION"         TO MSJ2-HTML
              MOVE "CON809"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CIUDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-CIUDAD-W             TO MSJ2-HTML
              MOVE "CON809"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR    
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PAISES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" OR "35"
              CONTINUE            
           ELSE
              MOVE OTR-STAT                 TO MSJ1-HTML
              MOVE NOM-PAISES-W             TO MSJ2-HTML
              MOVE "CON809"                 TO MSJ3-HTML
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
           MOVE COBW3-GET-DATA TO LLEGADA-W

           UNSTRING LLEGADA-W DELIMITED BY "|"
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, 
                   COD-CIU-LLEGADA-W, OPERADOR-LLEGADA-W   
           END-UNSTRING.

           MOVE LLAVE-SESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.

       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W 
         
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.
              
       ABRIR-USUARIO.
           INITIALIZE OTR-STAT
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
     
           OPEN INPUT ARCHIVO-USUNET                         
           EVALUATE OTR-STAT
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

           INITIALIZE NOM-CIUDAD-W NOM-PAISES-W
           
           INSPECT IP-DATOS-USUNET REPLACING ALL "/" BY "\".

           MOVE "\\" TO NOM-CIUDAD-W  NOM-PAISES-W


           INSPECT NOM-CIUDAD-W REPLACING FIRST "                    "
                                             BY IP-DATOS-USUNET

           INSPECT NOM-CIUDAD-W REPLACING FIRST " "
                                             BY "\"

           MOVE NOM-CIUDAD-W TO NOM-PAISES-W
                   
           INSPECT NOM-CIUDAD-W REPLACING FIRST "                          "           
                                             BY "PROG\DATOS\SC-ARCHCIUD.DAT". 

           INSPECT NOM-PAISES-W REPLACING FIRST "                      "           
                                             BY "PROG\DATOS\SC-PAIS.DAT".                 
                                                           
       ABRIR-ARCHIVO-CIUDAD.
           OPEN INPUT ARCHIVO-CIUDAD
                      ARCHIVO-PAISES.

           IF NOM-DPTO (1)  = SPACES OR LOW-VALUES
              PERFORM LLENAR-DPTO
           END-IF.               
           
           INITIALIZE REG-CIU.          
           MOVE "{*CIUDAD*:[" TO  DATOS-PLANO-W  
           INSPECT DATOS-PLANO-W  REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO.
           MOVE 0 TO SW-FIN.

       LEER-ARCHIVO-CIUDAD.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-PLANO-W  
              PERFORM DATOS-ENVIO           
           END-IF

           READ ARCHIVO-CIUDAD NEXT WITH NO LOCK AT END 
                GO TO CERRAR-ARCHIVO-CIUDAD
           END-READ.
           
           MOVE COD-CIU    TO COD-CIU-J                 
                 
           INSPECT NOMBRE-CIU REPLACING ALL '"' BY " "
           INSPECT NOMBRE-CIU REPLACING ALL "*" BY " "
           INSPECT NOMBRE-CIU REPLACING ALL "Ñ" BY CARAC-ENE-MAY 
           INSPECT NOMBRE-CIU REPLACING ALL "ñ" BY CARAC-ENE-MIN

           MOVE NOMBRE-CIU      TO NOMBRE-CIU-J
           MOVE PAIS-CIU        TO CODIGO-PAIS-J
           MOVE CODIGO-PAIS-J   TO COD-PAIS
           READ ARCHIVO-PAISES WITH NO LOCK
                INVALID KEY 
                   MOVE COD-PAIS TO NOMBRE-PAIS
           END-READ

           MOVE NOMBRE-PAIS     TO NOMBRE-PAIS-J 
           MOVE DPTO-CIU        TO LN-W

           IF LN-W IS ZERO
              MOVE SPACES          TO DEPART-PAIS-J
           ELSE
              MOVE NOM-DPTO (LN-W) TO DEPART-PAIS-J
           END-IF
                                  
           MOVE ","               TO CIERRE-LIN-1

           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES
           
           GO TO LEER-ARCHIVO-CIUDAD.
                                    
        CERRAR-ARCHIVO-CIUDAD.

           INITIALIZE LIN-1
         
           MOVE LIN-1 TO DATOS-PLANO-W  
           PERFORM DATOS-ENVIO.
          
           MOVE "]}" TO DATOS-PLANO-W 
           PERFORM DATOS-ENVIO.
           
           CLOSE ARCHIVO-CIUDAD.               

       PAGINA-CONFIG.  
010720     MOVE "statuscode" TO COBW3-CNV-NAME
010730     MOVE "00"         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010720     MOVE "programa-id" TO COBW3-CNV-NAME
010730     MOVE "CON809"      TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010760     MOVE "..\PAGINAS\RECIBIDOSLISTADO.ASPX" TO SALIDA-HTML
010770     PERFORM ABRIR-HTML.

007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.

       DATOS-ENVIO.
010720     MOVE "datosrecibidos" TO COBW3-CNV-NAME
010730     MOVE DATOS-PLANO-W    TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_REPEAT" USING COBW3.

008040            

       LLENAR-DPTO.
           MOVE "AMAZONAS"           TO NOM-DPTO (91).
           MOVE "ANTIOQUIA"          TO NOM-DPTO (05).
           MOVE "ARAUCA"             TO NOM-DPTO (81).
           MOVE "ATLANTICO"          TO NOM-DPTO (08).
           MOVE "DISTRITO CAPITAL"   TO NOM-DPTO (11).
           MOVE "BOLIVAR"            TO NOM-DPTO (13).
           MOVE "BOYACA"             TO NOM-DPTO (15).
           MOVE "CALDAS"             TO NOM-DPTO (17).
           MOVE "CAQUETA"            TO NOM-DPTO (18).
           MOVE "CASANARE"           TO NOM-DPTO (85).
           MOVE "CAUCA"              TO NOM-DPTO (19).
           MOVE "CESAR"              TO NOM-DPTO (20).
           MOVE "CHOCO"              TO NOM-DPTO (27).
           MOVE "CORDOBA"            TO NOM-DPTO (23).
           MOVE "CUNDINAMARCA"       TO NOM-DPTO (25).
           MOVE "GUAINIA"            TO NOM-DPTO (94).
           MOVE "GUAJIRA"            TO NOM-DPTO (44).
           MOVE "GUAVIARE"           TO NOM-DPTO (95).
           MOVE "HUILA"              TO NOM-DPTO (41).
           MOVE "MAGDALENA"          TO NOM-DPTO (47).
           MOVE "META"               TO NOM-DPTO (50).
           MOVE "NARIÑO"             TO NOM-DPTO (52).
           MOVE "NORTE SANTANDER"    TO NOM-DPTO (54).
           MOVE "PUTUMAYO"           TO NOM-DPTO (86).
           MOVE "QUINDIO"            TO NOM-DPTO (63).
           MOVE "RISARALDA"          TO NOM-DPTO (66).
           MOVE "SAN ANDRES"         TO NOM-DPTO (88).
           MOVE "SANTANDER"          TO NOM-DPTO (68).
           MOVE "SUCRE"              TO NOM-DPTO (70).
           MOVE "TOLIMA"             TO NOM-DPTO (73).
           MOVE "VALLE"              TO NOM-DPTO (76).
           MOVE "VAUPES"             TO NOM-DPTO (97).
           MOVE "VICHADA"            TO NOM-DPTO (99).
           MOVE "FUERA DEL PAIS"     TO NOM-DPTO (90).

        COPY "..\..\FUENTES\SC-WEB19.CBL".     
