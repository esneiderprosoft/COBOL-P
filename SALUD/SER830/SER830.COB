      *============================================================
      * CREACION DE ARCHIVO JSON SER830  SALUD
      *============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".  

            SELECT ARCHIVO-JSON
                   ASSIGN NOM-PLANO-W
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS OTR-STAT.
                                                          
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".

       FD  ARCHIVO-JSON
           LABEL RECORD STANDARD.
       01  REG-JSON.
           02 DATOS-JSON      PIC X(1500).
      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       77 NOM-PLANO-W               PIC x(90).

       77 MES-SAL-W                 PIC X(4).

       01  DATO-LLEGADA-W.
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).
           02 DIR-CONTAB-LLEGADA-W      PIC X(30).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 OPERADOR-LLEGADA-W        PIC X(4).

        
       01 DATO-IMPORTAR-WK              PIC X(100). 
        

       01 LIN-1.  
          02 FILLER                   PIC X(10)  VALUE "{*CODIGO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 CODIGO-PROFESION-J       PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(14)  VALUE "*DESCRIPCION*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DESCRIPCION-PROFESION-J  PIC X(25).
          02 FILLER                   PIC XX     VALUE "*}".    
          02 CIERRE-LIN-1             PIC X.


       01 DATOS-ENVIO.
          02 DAT0-ENV                   PIC XX.
          02 FILLER                     PIC X VALUE "|".
          02 NOMBRE-JSON-ENV            PIC X(28).



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
              MOVE "SER830"                 TO MSJ3-HTML
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
              MOVE "SER830"                 TO MSJ3-HTML
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
              MOVE "SER830"                 TO MSJ3-HTML
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
             INTO LLAVESESION-LLEGA-W, DIR-CONTAB-LLEGADA-W, MES-CONTAB-LLEGADA-W, OPERADOR-LLEGADA-W
           END-UNSTRING.

           MOVE LLAVESESION-LLEGA-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-JSON-W.
           ACCEPT HORA-JSON-W FROM TIME.
           MOVE OPERADOR-LLEGADA-W TO OPER-JSON-W.

       INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W
          
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.
 
             
       ABRIR-USUARIO.
           INITIALIZE OTR-STAT

           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W 

      
           OPEN INPUT ARCHIVO-USUNET
           EVALUATE OTR-STAT
001308       WHEN "00"  CONTINUE
001309       WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
001310                  CLOSE       ARCHIVO-USUNET
001311                  OPEN I-O    ARCHIVO-USUNET
001312                  INITIALIZE REG-USUNET
001313                  MOVE "." TO NOMBRE-USUNET
001314                  WRITE REG-USUNET
001315                  CLOSE       ARCHIVO-USUNET
001316                  MOVE "Validacion de usuarios"      TO MSJ1-HTML
001317                  MOVE "Falta configurar usuario"    TO MSJ2-HTML
001318                  MOVE "Sc"                          TO MSJ3-HTML
001319                  GO TO ENVIAR2-ERROR
001320       WHEN OTHER GO TO CERRAR-SESION
001321     END-EVALUATE.                          
001322
001332     INITIALIZE LLAVE-USUNET.

001334 LEER-USUARIO.
001335     READ ARCHIVO-USUNET NEXT AT END MOVE 0 TO SW-FIN.
001345     CLOSE ARCHIVO-USUNET.
001336
001337     IF NOMBRE-USUNET = SPACES
001339        MOVE "Validacion de usuarios"      TO MSJ1-HTML
001340        MOVE "Falta configurar usuario"    TO MSJ2-HTML
001341        MOVE "Sc"                          TO MSJ3-HTML
001342        GO TO ENVIAR2-ERROR
001343     END-IF.

007210*     INITIALIZE NOM-PLANO-W
      *     MOVE "\\" TO NOM-PLANO-W 
007220*     INSPECT NOM-PLANO-W  REPLACING FIRST "                    "
007230*                                       BY IP-DATOS-USUNET
      
           INITIALIZE NOM-PLANO-W

           MOVE OPERADOR-LLEGADA-W TO OPER-JSON-W
           MOVE FUNCTION CURRENT-DATE TO FECHA-JSON-W
           ACCEPT HORA-JSON-W FROM TIME


           MOVE NOMBRE-JSON-W  TO NOM-PLANO-W.
           MOVE NOM-JSON-W     TO NOMBRE-JSON-ENV.

       ABRIR-JSON-PROFESION.
           OPEN OUTPUT ARCHIVO-JSON.
           MOVE "00" TO DAT0-ENV.

           MOVE "{*PROFESION*:[" TO  DATOS-JSON
           INSPECT DATOS-JSON REPLACING ALL "*" BY CARAC-COMILLA
           WRITE REG-JSON END-WRITE
           MOVE 0 TO SW-FIN.

       CREAR-PROFESION.
           MOVE "1"                      TO CODIGO-PROFESION-J
           MOVE "MEDICO ESPECIALISTA"    TO DESCRIPCION-PROFESION-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "2"                      TO CODIGO-PROFESION-J
           MOVE "MEDICO GENERAL"    TO DESCRIPCION-PROFESION-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "3"                      TO CODIGO-PROFESION-J
           MOVE "ENFERMERO(A) JEFE"    TO DESCRIPCION-PROFESION-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "4"                      TO CODIGO-PROFESION-J
           MOVE "AUXILIAR ENFERMERIA"    TO DESCRIPCION-PROFESION-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "5"                      TO CODIGO-PROFESION-J
           MOVE "TERAPEUTAS Y OTROS"    TO DESCRIPCION-PROFESION-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "6"                      TO CODIGO-PROFESION-J
           MOVE "ENFERMERA JEFE P Y P"    TO DESCRIPCION-PROFESION-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "7"                      TO CODIGO-PROFESION-J
           MOVE "PSICOLOGIA"             TO DESCRIPCION-PROFESION-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "8"                      TO CODIGO-PROFESION-J
           MOVE "NUTRICIONISTA"          TO DESCRIPCION-PROFESION-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "9"                      TO CODIGO-PROFESION-J
           MOVE "NO APLICA"              TO DESCRIPCION-PROFESION-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "A"                      TO CODIGO-PROFESION-J
           MOVE "ODONTOLOGO"    TO DESCRIPCION-PROFESION-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "B"                      TO CODIGO-PROFESION-J
           MOVE "AUDITOR MEDICO"    TO DESCRIPCION-PROFESION-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "H"                      TO CODIGO-PROFESION-J
           MOVE "HIGIENE ORAL"    TO DESCRIPCION-PROFESION-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "I"                      TO CODIGO-PROFESION-J
           MOVE "INSTRUMENTADOR(A)"    TO DESCRIPCION-PROFESION-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "O"                      TO CODIGO-PROFESION-J
           MOVE "OPTOMETRA"    TO DESCRIPCION-PROFESION-J
           PERFORM ESCRIBIR-LINEA.

           MOVE "T"                      TO CODIGO-PROFESION-J
           MOVE "TRABAJO SOCIAL"    TO DESCRIPCION-PROFESION-J
           PERFORM ESCRIBIR-ULTIMA-LINEA.

           GO TO CERRAR-PROFESION.
           
       ESCRIBIR-LINEA.
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE ","      TO CIERRE-LIN-1
           MOVE LIN-1 TO DATOS-JSON
007720     WRITE REG-JSON END-WRITE.

       ESCRIBIR-ULTIMA-LINEA.
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
           MOVE SPACES      TO CIERRE-LIN-1
           MOVE LIN-1 TO DATOS-JSON
007720     WRITE REG-JSON END-WRITE.

       CERRAR-PROFESION.
           MOVE "]}" TO DATOS-JSON
           WRITE REG-JSON END-WRITE
            
           CLOSE ARCHIVO-JSON.   

       ENVIO-DATOS.
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