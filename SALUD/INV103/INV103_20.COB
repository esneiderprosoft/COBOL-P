      *=================================================================
      *JSON DE EMPAQUE DEL ARCHUIVO ARTICULOS 
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
      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".
  
       77 NOM-ART-W                            PIC X(70).
       77 NOM-PLANO-W                          PIC X(90).
       01 LINEA-LLEGADA-W                      PIC X(500).
       
                                        
       01  DATO-LLEGADA-W.
           02 LLAVE-SESION-LLEGA-W.
              03 ID-LLEGAD-W                   PIC X(15).
              03 FECHA-LLEGAD-W                PIC X(8).
              03 HORA-LLEGAD-W                 PIC X(6).
           02 DIR-CONTAB-LLEGADA-W             PIC X(30).
           02 MES-CONTAB-LLEGADA-W             PIC XX.    
           02 OPERADOR-LLEGADA-W               PIC X(4).
                                       
       01 DATOS-ENVIO.
          02 SW-INVALID-ENV                    PIC XX.
          02 FILLER                            PIC X      VALUE "|".
          02 DATO-J                            PIC X(28).

       01 LIN-1.
          02 FILLER                            PIC X(11)  VALUE "{°ALMACEN°:".
          02 FILLER                            PIC X      VALUE "°".
          02 ALM-EMP-ART-J                     PIC X(5).
          02 FILLER                            PIC XX     VALUE "°,".
          02 FILLER                            PIC X(9)  VALUE "°CODIGO°:".
          02 FILLER                            PIC X      VALUE "°".
          02 COD-EMP-ART-J                     PIC X(18).
          02 FILLER                            PIC XX     VALUE "°,".
          02 FILLER                            PIC X(11)  VALUE "°CANTIDAD°:".
          02 FILLER                            PIC X      VALUE "°".
          02 CANT-EMP-ART-J                    PIC 9(3)V9(4).
          02 FILLER                            PIC XX     VALUE "°}".    
          02 CIERRE-LIN-1                      PIC X.

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
              MOVE "INV103-20"                 TO MSJ3-HTML
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
              MOVE "INV103-20"                 TO MSJ3-HTML
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
              MOVE "INV103-20"                 TO MSJ3-HTML
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
              MOVE "INV103-20"              TO MSJ3-HTML
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

           UNSTRING LINEA-LLEGADA-W DELIMITED BY "|"
              INTO LLAVE-SESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W,
                   OPERADOR-LLEGADA-W

           END-UNSTRING.

           MOVE LINEA-LLEGADA-W  TO LLAVE-SESION-W.

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

           INITIALIZE NOM-PLANO-W

           MOVE OPERADOR-LLEGADA-W TO OPER-JSON-W
           MOVE FUNCTION CURRENT-DATE TO FECHA-JSON-W
           ACCEPT HORA-JSON-W FROM TIME


           MOVE NOMBRE-JSON-W  TO NOM-PLANO-W.
           MOVE NOM-JSON-W     TO DATO-J.

       ABRIR-MAESTRO-ARTICULOS.
           OPEN INPUT MAESTRO-ARTICULOS.
                   
           MOVE "00"  TO SW-INVALID-ENV.

       LEER-MAESTRO-ARTICULOS.
           READ MAESTRO-ARTICULOS NEXT WITH NO LOCK AT END
                                 GO TO ABRIR-JSON
           END-READ.

           GO TO LEER-MAESTRO-ARTICULOS.

         
       ABRIR-JSON.
           OPEN OUTPUT ARCHIVO-JSON.
           INITIALIZE REG-JSON.

           MOVE "{°EMPAQUE°:[" TO  DATOS-JSON
           INSPECT DATOS-JSON  REPLACING ALL "°" BY CARAC-COMILLA
           WRITE REG-JSON  END-WRITE
           MOVE 0 TO SW-FIN.      

       ENVIO-DATOS-JSON.
           PERFORM MOVER-EMP VARYING I FROM 1 BY 1
                                  UNTIL I > 10.
   
       CERRAR-JSON.
           INITIALIZE LIN-1

           INITIALIZE ALM-EMP-ART-J
                      COD-EMP-ART-J
                      CANT-EMP-ART-J
                                                                 
           INSPECT LIN-1 REPLACING ALL "°" BY CARAC-COMILLA
           MOVE LIN-1 TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE
          
           MOVE "]}" TO DATOS-JSON  
           WRITE REG-JSON  END-WRITE.


       CERRAR-MAESTRO-ARTICULOS.            
         
           CLOSE MAESTRO-ARTICULOS
                 ARCHIVO-JSON. 

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

       MOVER-EMP.
           INITIALIZE LIN-1
     
           MOVE ALM-EMP-ART (I)      TO ALM-EMP-ART-J
           MOVE COD-EMP-ART (I)      TO COD-EMP-ART-J
           MOVE CANT-EMP-ART (I)     TO CANT-EMP-ART-J 
         
           INSPECT LIN-1 REPLACING ALL "°" BY CARAC-COMILLA 
           INSPECT LIN-1 REPLACING ALL LOW-VALUE BY " "
           MOVE "," TO CIERRE-LIN-1

           MOVE LIN-1 TO DATOS-JSON
           WRITE REG-JSON END-WRITE.
        
       COPY "..\..\FUENTES\SC-WEB19.CBL".
      
