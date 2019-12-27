      *============================================================
      * CREA, ENVIA OBJETO PERSONAL QUE ATIENDE (PROFESIONALES)
      *============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                     
       COPY "..\..\FUENTES\FS-USUNET.CBL".            
       COPY "..\..\FUENTES\FS-SESION.CBL".   
       COPY "..\..\FUENTES\FS-PROFE.CBL".                                                       
      
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-PROFE.CBL".
      
       WORKING-STORAGE  SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".
       
       77 DATOS-PLANO-W              PIC X(1024).

       77 K                         PIC 99.
       77 CEDULA-Z                  PIC ZZZZZZZZZZ.
       77 MES-SAL-W                 PIC X(4).
       77 CONT-W                    PIC 9 VALUE 0.

       01  DATO-LLEGADA-W.                                                                                 
           02 LLAVESESION-LLEGA-W.
              03 ID-LLEGAD-W            PIC X(15).
              03 FECHA-LLEGAD-W         PIC X(8).
              03 HORA-LLEGAD-W          PIC X(6).           
           02 DIR-CONTAB-LLEGADA-W      PIC X(30).
           02 MES-CONTAB-LLEGADA-W      PIC XX.
           02 OPERADOR-LLEGADA-W        PIC X(4).
           02 ESPEC-ELAB                PIC X(3).

       01 DATO-IMPORTAR-WK              PIC X(100). 
        
       01 DATOS-ENVIO.
          02 DAT0-ENV                   PIC XX.
          02 FILLER                     PIC X VALUE "|".

       01 LIN-1.  
          02 FILLER                   PIC X(10)  VALUE "{*NOMBRE*:".
          02 FILLER                   PIC X      VALUE "*".
          02 NOMBRE-PROFE-J           PIC X(30).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(17)  VALUE "*IDENTIFICACION*:".
          02 FILLER                   PIC X      VALUE "*".
          02 IDENTIFICACION-PROFE-J   PIC X(10).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(14)  VALUE "*DESCRIPCION*:".
          02 FILLER                   PIC X      VALUE "*".
          02 DESCRIPCION-PROFE-J      PIC X(30).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(13)  VALUE "*REG_MEDICO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 REG-MEDICO-PROFE-J       PIC X(10).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(15)  VALUE "*ATIENDE_PROF*:".
          02 FILLER                   PIC X      VALUE "*".
          02 ATIENDE-PROF-J           PIC X.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(10)   VALUE "*HORARIO*:".
          02 FILLER                   PIC X      VALUE "*".
          02 HORARIO-PROFE-J          PIC X(16).
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(5)   VALUE "*LU*:".
          02 FILLER                   PIC X      VALUE "*".
          02 LU-PROFE-J               PIC XX.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(5)   VALUE "*MA*:".
          02 FILLER                   PIC X      VALUE "*".
          02 MA-PROFE-J               PIC XX.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(5)   VALUE "*MI*:".
          02 FILLER                   PIC X      VALUE "*".
          02 MI-PROFE-J               PIC XX.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(5)   VALUE "*JU*:".
          02 FILLER                   PIC X      VALUE "*".
          02 JU-PROFE-J               PIC XX.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(5)   VALUE "*VI*:".
          02 FILLER                   PIC X      VALUE "*".
          02 VI-PROFE-J               PIC XX.
          02 FILLER                   PIC XX     VALUE "*,".
          02 FILLER                   PIC X(5)   VALUE "*SA*:".
          02 FILLER                   PIC X      VALUE "*".
          02 SA-PROFE-J               PIC XX.
          02 FILLER                   PIC X      VALUE "*".
          02 TITULO-TAB-PROF-J        PIC X(15).
          02 TABLA-PROF-J             OCCURS 10.
             05 ENCAB-TAB-ESP-J       PIC X(8)   VALUE "{*COD*:*".
             05 ESP-PROF-J            PIC X(3).
             05 FIN-TAB-ESP-J         PIC XX     VALUE "*}".
             05 CIERRE-ESP-J          PIC X.     
          02 CIERRE-TAB-ESP-J         PIC XXX.
          02 CIERRE-LIN-1             PIC X.  

          

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
              MOVE "SER819"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PROFESIONALES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00"
              CONTINUE            
           ELSE
              MOVE OTR-STAT              TO MSJ1-HTML
              MOVE NOM-PROF-LNK       TO MSJ2-HTML
              MOVE "SER819"              TO MSJ3-HTML
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
              MOVE "SER819"                 TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

001150 END DECLARATIVES.
001160
001161 INICIAR-IIS.
001162     MOVE LOW-VALUE TO COBW3.
001163     MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
001164     CALL "COBW3_INIT" USING COBW3.
001220

       LEER-DATO-HTML.
000788     MOVE "datosh" TO COBW3-SEARCH-DATA.
000790     CALL "COBW3_GET_VALUE" USING COBW3.
000791     MOVE COBW3-GET-DATA TO LINEA-LLEGAD-W.

           UNSTRING LINEA-LLEGAD-W DELIMITED BY "|" 
             INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W
           END-UNSTRING.

           MOVE SESION-LLEGAD-W    TO LLAVE-SESION-W.
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL.
           ACCEPT HORA-TOTAL FROM TIME.

001238 INICIAR-SESION.
           MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-SESION.DAT" TO NOM-SESION-W.
           GO TO VALIDAR-SESION.

       FIN-VALIDAR-SESION.
       
001303 ABRIR-USUARIO.
           INITIALIZE OTR-STAT
001304     MOVE "D:\WEB\MAIN-ELECT\DATOS\SC-ARCHUSU.DAT" TO NOM-USU-W
001305
001306     OPEN INPUT ARCHIVO-USUNET
001307     EVALUATE OTR-STAT
001308       WHEN "00"  CONTINUE
001309       WHEN "35"  OPEN OUTPUT ARCHIVO-USUNET
001310                  CLOSE       ARCHIVO-USUNET
001311                  OPEN I-O    ARCHIVO-USUNET
001312                  INITIALIZE REG-USUNET
001314                  WRITE REG-USUNET
001315                  CLOSE       ARCHIVO-USUNET
001316                  MOVE "SC-2"                      TO MSJ1-HTML
                        MOVE "Falta configurar usuario"  TO MSJ2-HTML
                        MOVE "SER819"                   TO MSJ3-HTML
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
001339        MOVE "SC-2"      TO MSJ1-HTML
001340        MOVE "Falta configurar usuario"    TO MSJ2-HTML
001341        MOVE "SER819"                      TO MSJ3-HTML
001342        GO TO ENVIAR2-ERROR
001343     END-IF.

       ASIGNAR-NOMBRES.
           INITIALIZE NOM-PROF-LNK.

           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

           MOVE "\\" TO NOM-PROF-LNK

           INSPECT NOM-PROF-LNK REPLACING FIRST "                    "
                                                BY IP-DATOS-USUNET

           INSPECT NOM-PROF-LNK REPLACING FIRST " "
                                                BY "\"

           INSPECT NOM-PROF-LNK REPLACING FIRST "                              "
                                            BY DIRECTORIO-LLEGAD-W
        
           INSPECT NOM-PROF-LNK REPLACING FIRST "                        "
                                                BY "\CONTROL\SC-ARCHPROF.DAT".

           
      *>    DATOS-SALAS.
      *>        IF NIT-USU = 830092718 OR 830092719
      *>           MOVE "S" TO SALAS-W
      *>        ELSE
      *>           MOVE "N" TO SALAS-W
      *>        END-IF.

       ABRIR-JSON-PROF.
           OPEN INPUT  ARCHIVO-PROFESIONALES.
           INITIALIZE REG-PROF.

           MOVE "{*ARCHPROF*:[" TO  DATOS-PLANO-W
           INSPECT DATOS-PLANO-W REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO.
           MOVE 0 TO SW-FIN.

       LEER-PROFESIONALES.
           IF SW-FIN = 0
              MOVE 1 TO SW-FIN
           ELSE
              MOVE LIN-1 TO DATOS-PLANO-W
              PERFORM DATOS-ENVIO
           END-IF

           READ ARCHIVO-PROFESIONALES NEXT WITH NO LOCK AT END 
                GO TO CERRAR-PROFESIONALES
           END-READ.

           IF INI1-PROF = "*"
           OR ESTADO-PROF = 2
              GO TO LEER-PROFESIONALES
           END-IF.

        *>    IF SALAS-W = "S"
        *>       IF COD-PROF > 9999
        *>       OR COD-PROF < 1300
        *>          GO TO LEER-PROFESIONALES
        *>       END-IF
        *>    END-IF
           INITIALIZE SW-BUSCAR.
           INITIALIZE HORARIO-PROFE-J

          
           
           IF DESCRIP-PROF = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
      *        GO TO LEER-PROFESIONALES
           END-IF           

           MOVE COD-PROF     TO CEDULA-Z.
           MOVE CEDULA-Z     TO IDENTIFICACION-PROFE-J.
           MOVE DESCRIP-PROF TO NOMBRE-PROFE-J
           MOVE DETALLE-PROF TO DESCRIPCION-PROFE-J.
           MOVE REG-MED-PROF TO REG-MEDICO-PROFE-J
           MOVE ATIENDE-PROF TO ATIENDE-PROF-J

           IF FORMA-AGE-PROF = "S"
              MOVE "HORARIO VARIABLE" TO HORARIO-PROFE-J
           ELSE
              MOVE SPACES TO HORARIO-PROFE-J
              INITIALIZE J
              PERFORM BUSCAR-SEMANA VARYING J
                      FROM 1 BY 1 UNTIL J > 6
            END-IF
             
           IF ESPEC-ELAB IS NOT = SPACES
              PERFORM BUSCAR-ESPECIALIDAD  VARYING K FROM 1 BY 1
                   UNTIL K > 10 OR SW-BUSCAR > 0
              IF CONT-W = 0
                  MOVE ",*TAB_ESPEC*:[" TO TITULO-TAB-PROF-J
                  ADD 1 TO CONT-W
              END-IF   
              IF SW-BUSCAR = 0
                 INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
                 INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES
                 GO TO LEER-PROFESIONALES
              END-IF
           END-IF
                 INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
                 INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES
                 MOVE ","               TO CIERRE-LIN-1

           GO TO LEER-PROFESIONALES.

       BUSCAR-SEMANA.
           EVALUATE J
           WHEN 1 IF HR-INI1-P (J) > 0
                     MOVE HR-INI1-P (J) TO LU-PROFE-J
                  ELSE
                     MOVE "X"           TO LU-PROFE-J
                  END-IF
           WHEN 2 IF HR-INI1-P (J) > 0
                     MOVE HR-INI1-P (J) TO MA-PROFE-J
                  ELSE
                     MOVE "X"           TO MA-PROFE-J
                  END-IF
           WHEN 3 IF HR-INI1-P (J) > 0
                     MOVE HR-INI1-P (J) TO MI-PROFE-J
                  ELSE
                     MOVE "X"           TO MI-PROFE-J
                  END-IF
           WHEN 4 IF HR-INI1-P (J) > 0
                     MOVE HR-INI1-P (J) TO JU-PROFE-J
                  ELSE
                     MOVE "X"           TO JU-PROFE-J
                  END-IF
           WHEN 5 IF HR-INI1-P (J) > 0
                     MOVE HR-INI1-P (J) TO VI-PROFE-J
                  ELSE
                     MOVE "X"           TO VI-PROFE-J
                  END-IF
           WHEN 6 IF HR-INI1-P (J) > 0
                     MOVE HR-INI1-P (J) TO SA-PROFE-J
                  ELSE
                     MOVE "X"           TO SA-PROFE-J
                  END-IF.

       CERRAR-PROFESIONALES.
           INITIALIZE LIN-1
           MOVE LIN-1 TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           
           MOVE "}]}" TO DATOS-PLANO-W
           PERFORM DATOS-ENVIO
           CLOSE ARCHIVO-PROFESIONALES.

       PAGINA-CONFIG.  
010720     MOVE "statuscode" TO COBW3-CNV-NAME
010730     MOVE "00"         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010720     MOVE "programa-id" TO COBW3-CNV-NAME
010730     MOVE "SER819"      TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010760     MOVE "..\PAGINAS\RECIBIDOSLISTADO.ASPX" TO SALIDA-HTML
010770     PERFORM ABRIR-HTML.

007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.

       BUSCAR-ESPECIALIDAD.
           
           MOVE ESP-PROF (K) TO ESP-PROF-J (J)
           INSPECT TABLA-PROF-J (1) REPLACING ALL "*" BY CARAC-COMILLA
           INSPECT TABLA-PROF-J (1) REPLACING ALL LOW-VALUES BY SPACES
           ADD 1 TO J
           IF J < 10
             MOVE ","  TO CIERRE-ESP-J (J)
           ELSE 
                MOVE "]} " TO CIERRE-TAB-ESP-J
           END-IF   
           IF K = 10
                MOVE "]}, " TO CIERRE-TAB-ESP-J
           END-IF.

       DATOS-ENVIO.
010720     MOVE "datosrecibidos" TO COBW3-CNV-NAME
010730     MOVE DATOS-PLANO-W    TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_REPEAT" USING COBW3.
008030
008040 COPY "..\..\FUENTES\SC-WEB19.CBL".
