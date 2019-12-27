      *=================================================================
      *CREA, ENVIA JSON PACIENTES
      *=================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
       COPY "..\..\FUENTES\FS-PACIE.CBL".
      
           SELECT ARCHIVO-ENTIDADES LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-ENTI-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-ENT
                  ALTERNATE RECORD KEY IS NOMBRE-ENT WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.                 

           SELECT ARCHIVO-OCUPACIONES
                  ASSIGN NOM-OCUP-W 
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-OCU
                  ALTERNATE RECORD KEY IS NOMBRE-OCU WITH DUPLICATES
                  FILE STATUS OTR-STAT.        
            
           SELECT ARCHIVO-CIUDAD LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-CIUDAD-W
                  ORGANIZATION IS INDEXED;
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS COD-CIU
                  ALTERNATE RECORD KEY IS NOMBRE-CIU WITH DUPLICATES
                  FILE STATUS IS OTR-STAT.                             
                  
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-PACIE.CBL".
       COPY "..\..\FUENTES\FD-ENTID.CBL".
       COPY "..\..\FUENTES\FD-OCUPA.CBL".
       COPY "..\..\FUENTES\FD-CIUDA.CBL".
      
       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC19.CBL".
       
       77 NOM-OCUP-W                   PIC X(100).
       77 NOM-ENTI-W                   PIC X(100).
       77 NOM-CIUDAD-W                 PIC X(100).
       77 DATOS-PLANO-W             PIC X(1024).
       77 TP-BUSCAR                    PIC 99.

       01 PATRON-W.
          02 SW-PATRON                 PIC X OCCURS 10.

       01 DATO-BUSCAR.
          02 DATO-BUSCAR-9.
             03 DATO-BUSCAR-8.
                04 DATO-BUSCAR-7.
                   05 DATO-BUSCAR-6.
                      06 DATO-BUSCAR-5.
                         07 DATO-BUSCAR-4.
                            09 DATO-BUSCAR-3  PIC X(3).
                            09 DATO-BUSCAR-3A PIC X.
                         07 DATO-BUSCAR-4A    PIC X.
                      06 DATO-BUSCAR-5A       PIC X.
                   05 DATO-BUSCAR-6A          PIC X.
                04 DATO-BUSCAR-7A             PIC X.
             03 DATO-BUSCAR-8A                PIC X.
          02 DATO-BUSCAR-9A                   PIC X.


       01 LIN-1.  
          02 FILLER                  PIC X(7)   VALUE "{*COD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 COD-PACI-J              PIC X(15).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(9)   VALUE "*NOMBRE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DESCRIP-PACI-J          PIC X(54).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(6)   VALUE "*EPS*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EPS-PACI-J              PIC X(74).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(10)  VALUE "*DERECHO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DERECHO-PACI-J          PIC X(22).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(13)  VALUE "*NACIMIENTO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NACIM-PACI-J            PIC X(8).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(8)   VALUE "*CIVIL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 EST-CIV-PACI-J          PIC X(14).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(7)   VALUE "*SEXO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 SEXO-PACI-J             PIC X(12).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(12)  VALUE "*OCUPACION*:".
          02 FILLER                  PIC X      VALUE "*".
          02 OCUP-PACI-J             PIC X(128).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(7)   VALUE "*TIPO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-PACI-J             PIC X(18).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(12)  VALUE "*TIPO_AFIL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TIPO-AFIL-PACI-J        PIC X(12).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(11)  VALUE "*NRO_AFIL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 NRO-AFIL-PACI-J         PIC X(15).          
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(13)  VALUE "*PARENTESCO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 PARENT-PACI-J           PIC X(15).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(10)  VALUE "*ESTRATO*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ESTRATO-PACI-J          PIC 9.
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(14)  VALUE "*FECHA_VENC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 FECHA-VENCE-PACI-J      PIC X(11).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(12)  VALUE "*DIRECCION*:".
          02 FILLER                  PIC X      VALUE "*".
          02 DIRECC-PACI-J           PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(6)   VALUE "*TEL*:".
          02 FILLER                  PIC X      VALUE "*".
          02 TELEFONO-PACI-J         PIC X(10).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(9)   VALUE "*CIUDAD*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CIUDAD-PACI-J           PIC X(26).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(14)  VALUE "*ACOMPAÑANTE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ACOMPA-PACI-J           PIC X(30).
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(11)  VALUE "*CAB_FAMI*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CABEZA-FAM-PACI-J       PIC 9.
          02 FILLER                  PIC XX     VALUE "*,".       
          02 FILLER                  PIC X(13)  VALUE "*CLASIFICAC*:".
          02 FILLER                  PIC X      VALUE "*".
          02 CLASIF-PACI-J           PIC XX.
          02 FILLER                  PIC XX     VALUE "*,".
          02 FILLER                  PIC X(15)  VALUE "*ID-COTIZANTE*:".
          02 FILLER                  PIC X      VALUE "*".
          02 ID-COTIZANTE-J          PIC X(15).
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
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-USU-W        TO MSJ2-HTML
              MOVE "SER810"         TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
      
       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-SESION.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" 
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-SESION-W     TO MSJ2-HTML
              MOVE "SER810"         TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-CIUDAD.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" 
              CONTINUE 
           ELSE
              MOVE OTR-STAT         TO MSJ1-HTML
              MOVE NOM-CIUDAD-W     TO MSJ2-HTML
              MOVE "SER810"         TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-PACIENTES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" 
              CONTINUE 
           ELSE
              MOVE OTR-STAT            TO MSJ1-HTML
              MOVE NOM-PACIE-LNK       TO MSJ2-HTML
              MOVE "SER810"            TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.

       I-O-TEST SECTION.
           USE AFTER EXCEPTION PROCEDURE ON ARCHIVO-OCUPACIONES.
       ESCR-EXCEPTIONES.
           IF OTR-STAT = "00" 
              CONTINUE 
           ELSE
              MOVE OTR-STAT          TO MSJ1-HTML
              MOVE NOM-OCUP-W        TO MSJ2-HTML
              MOVE "SER810"          TO MSJ3-HTML
              GO TO ENVIAR2-ERROR
           END-IF.
        END DECLARATIVES.
      
       INICIAR-IIS.
           MOVE LOW-VALUE TO COBW3.
           MOVE FUNCTION ADDR(ISAPI-CTX-CNT) TO COBW3-CONTEXT.
           CALL "COBW3_INIT" USING COBW3.
      
006790 LEER-DATO-HTML.
006800     MOVE "datosh" TO COBW3-SEARCH-DATA.
           CALL "COBW3_GET_VALUE" USING COBW3.
           MOVE COBW3-GET-DATA    TO LINEA-LLEGAD-W.

           INITIALIZE DATO-LLEGADA-W DATO-BUSCAR

           UNSTRING LINEA-LLEGAD-W DELIMITED BY "|"
              INTO SESION-LLEGAD-W, DIRECTORIO-LLEGAD-W, CARPTA-LLEGAD-W, PASO-W, DATO-BUSCAR
           END-UNSTRING.  
          
           MOVE SESION-LLEGAD-W       TO LLAVE-SESION-W
           MOVE FUNCTION CURRENT-DATE TO FECHA-TOTAL
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
                        MOVE "SER810"                    TO MSJ3-HTML
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
001341        MOVE "SER810"                      TO MSJ3-HTML
001342        GO TO ENVIAR2-ERROR
001343     END-IF.

       ASIGNAR-NOMBRES.

           INITIALIZE NOM-PACIE-LNK
           MOVE "\\" TO NOM-PACIE-LNK
           INSPECT IP-DATOS-USUNET REPLACING FIRST "/" BY "\"

           INSPECT NOM-PACIE-LNK REPLACING FIRST "                    "
                                              BY IP-DATOS-USUNET

           INSPECT NOM-PACIE-LNK REPLACING FIRST " "
                                              BY "\"

           MOVE NOM-PACIE-LNK TO NOM-OCUP-W NOM-ENTI-W NOM-CIUDAD-W

           INSPECT NOM-PACIE-LNK REPLACING FIRST "                              "
                                              BY DIRECTORIO-LLEGAD-W.

           INSPECT NOM-PACIE-LNK REPLACING FIRST "                     "
                                             BY "\CONTROL\SC-PACIE.DAT".
                                            
           INSPECT NOM-CIUDAD-W REPLACING FIRST "                          "           
                                             BY "PROG\DATOS\SC-ARCHCIUD.DAT". 

           INSPECT NOM-ENTI-W REPLACING FIRST "                         "           
                                           BY "PROG\DATOS\SC-ARCHENT.DAT". 

           INSPECT NOM-OCUP-W REPLACING FIRST "                         "           
                                           BY "PROG\DATOS\SC-ARCHOCU.DAT".
                                                       
           
       ABRIR-ARCHIVOS.
           OPEN INPUT ARCHIVO-PACIENTES
                      ARCHIVO-CIUDAD
                      ARCHIVO-ENTIDADES
                      ARCHIVO-OCUPACIONES.

       CREAR-OBJ-PACIENTES. 
007610     INITIALIZE REG-PACI.

007620     MOVE "{*PACIENTES*:[" TO  DATOS-PLANO-W 
007630     INSPECT DATOS-PLANO-W  REPLACING ALL "*" BY CARAC-COMILLA
           PERFORM DATOS-ENVIO
007650     MOVE 0 TO SW-FIN.
007660
007670 LEER-PACIENTES.
007680     IF SW-FIN = 0
007690        MOVE 1 TO SW-FIN
007700     ELSE
007710         MOVE LIN-1 TO DATOS-PLANO-W 
               PERFORM DATOS-ENVIO
007730     END-IF 
007740
007750     READ ARCHIVO-PACIENTES NEXT WITH NO LOCK AT END
                GO TO CERRAR-PACIENTES
           END-READ.
007760
007780     IF COD-PACI = SPACES OR LOW-VALUES
              MOVE 0 TO SW-FIN
           ELSE
              IF COD-PACI = COD-PACI-J
                 MOVE 0 TO SW-FIN 
              ELSE

                 IF PASO-W = "0" OR SPACE
                    CONTINUE
                 ELSE
                    PERFORM DEPURAR-REGISTRO
                    IF SW-BUSCAR = 0
                        MOVE 0 TO SW-FIN
                        GO TO LEER-PACIENTES
                    END-IF
                 END-IF

                 INITIALIZE LIN-1

                 MOVE EPS-PACI TO COD-ENT
                 READ ARCHIVO-ENTIDADES WITH NO LOCK
                      INVALID KEY MOVE SPACES TO EPS-PACI-J
                 END-READ

                 STRING  COD-ENT "  " NOMBRE-ENT DELIMITED BY SIZE 
                 INTO EPS-PACI-J

                 INITIALIZE REG-OCU                 
                 
                 MOVE OCUP-V8-PACI TO COD-OCU
                 READ ARCHIVO-OCUPACIONES WITH NO LOCK
                      INVALID KEY MOVE SPACES TO OCUP-PACI-J
                 END-READ

                 STRING OCUP-V8-PACI " - " NOMBRE-OCU DELIMITED BY SIZE 
                        INTO OCUP-PACI-J
                 END-STRING

                 MOVE CIUDAD-PACI TO COD-CIU
                 READ ARCHIVO-CIUDAD WITH NO LOCK
                      INVALID KEY MOVE SPACES TO CIUDAD-PACI-J
                 END-READ

                 STRING CIUDAD-PACI " - " NOMBRE-CIU DELIMITED BY SIZE
                        INTO CIUDAD-PACI-J
                 END-STRING

                 EVALUATE MES-VENCE-PACI
                   WHEN 01 MOVE "Ene dd/aaaa"    TO FECHA-VENCE-PACI-J
                   WHEN 02 MOVE "Feb dd/aaaa"    TO FECHA-VENCE-PACI-J
                   WHEN 03 MOVE "Mar dd/aaaa"    TO FECHA-VENCE-PACI-J
                   WHEN 04 MOVE "Abr dd/aaaa"    TO FECHA-VENCE-PACI-J
                   WHEN 05 MOVE "May dd/aaaa"    TO FECHA-VENCE-PACI-J
                   WHEN 06 MOVE "Jun dd/aaaa"    TO FECHA-VENCE-PACI-J
                   WHEN 07 MOVE "Jul dd/aaaa"    TO FECHA-VENCE-PACI-J
                   WHEN 08 MOVE "Agt dd/aaaa"    TO FECHA-VENCE-PACI-J
                   WHEN 09 MOVE "Sep dd/aaaa"    TO FECHA-VENCE-PACI-J
                   WHEN 10 MOVE "Oct dd/aaaa"    TO FECHA-VENCE-PACI-J
                   WHEN 11 MOVE "Nov dd/aaaa"    TO FECHA-VENCE-PACI-J
                   WHEN 12 MOVE "Dic dd/aaaa"    TO FECHA-VENCE-PACI-J
                 END-EVALUATE
                
                 INSPECT COD-PACI REPLACING ALL '-' BY "0"
           
                 MOVE COD-PACI TO COD-PACI-J
              
                 INSPECT DESCRIP-PACI REPLACING ALL '"' BY "X"
                 INSPECT DESCRIP-PACI REPLACING ALL "*" BY "X"
                 INSPECT DESCRIP-PACI REPLACING ALL "Ñ" BY CARAC-ENE-MAY 
                 INSPECT DESCRIP-PACI REPLACING ALL "ñ" BY CARAC-ENE-MIN

                 INSPECT DIRECC-PACI REPLACING ALL '"' BY "X"
                 INSPECT DIRECC-PACI REPLACING ALL "*" BY "X"
                 INSPECT DIRECC-PACI REPLACING ALL "Ñ" BY CARAC-ENE-MAY 
                 INSPECT DIRECC-PACI REPLACING ALL "ñ" BY CARAC-ENE-MIN

                 INSPECT ACOMPA-PACI REPLACING ALL '"' BY "X"
                 INSPECT ACOMPA-PACI REPLACING ALL "*" BY "X"
                 INSPECT ACOMPA-PACI REPLACING ALL "Ñ" BY CARAC-ENE-MAY 
                 INSPECT ACOMPA-PACI REPLACING ALL "ñ" BY CARAC-ENE-MIN

                 EVALUATE DERECHO-PACI
                    WHEN 1 MOVE "En base datos activo  " TO DERECHO-PACI-J
                    WHEN 2 MOVE "En base datos inactivo" TO DERECHO-PACI-J
                    WHEN 3 MOVE "Creado por un operador" TO DERECHO-PACI-J
                    WHEN OTHER MOVE SPACES               TO DERECHO-PACI-J
                 END-EVALUATE

                 MOVE NACIM-PACI       TO NACIM-PACI-J 
                 EVALUATE EST-CIV-PACI
                    WHEN "S" 
                             STRING EST-CIV-PACI  " - Soltero    " DELIMITED BY SIZE 
                                    INTO EST-CIV-PACI-J
                             END-STRING
                    WHEN "C" 
                             STRING EST-CIV-PACI  " - Casado     " DELIMITED BY SIZE 
                                    INTO EST-CIV-PACI-J
                             END-STRING
                    WHEN "U" 
                             STRING EST-CIV-PACI  " - Union libre" DELIMITED BY SIZE 
                                    INTO EST-CIV-PACI-J
                             END-STRING
                    WHEN "V" 
                             STRING EST-CIV-PACI  " - Viudo      " DELIMITED BY SIZE 
                                    INTO EST-CIV-PACI-J
                             END-STRING
                    WHEN "D" 
                             STRING EST-CIV-PACI  " - Separado   " DELIMITED BY SIZE 
                                    INTO EST-CIV-PACI-J
                             END-STRING
                    WHEN OTHER 
                             STRING EST-CIV-PACI  "              " DELIMITED BY SIZE 
                                    INTO EST-CIV-PACI-J
                             END-STRING
                 END-EVALUATE

                 EVALUATE SEXO-PACI
                    WHEN "F"   MOVE "F  FEMENINO " TO SEXO-PACI-J 
                    WHEN "M"   MOVE "M  MASCULINO" TO SEXO-PACI-J 
                    WHEN OTHER MOVE "         "    TO SEXO-PACI-J
                 END-EVALUATE

                 EVALUATE TIPO-PACI
                    WHEN "C"   MOVE "CONTRIBUTIVO" TO TIPO-PACI-J
                    WHEN "S"   MOVE "SUBSIDIADO  " TO TIPO-PACI-J
                    WHEN "V"   MOVE "VINCULADO   " TO TIPO-PACI-J
                    WHEN "P"   MOVE "PARTICULAR  " TO TIPO-PACI-J
                    WHEN "O"   MOVE "OTRO TIPO   " TO TIPO-PACI-J
                    WHEN "D"   MOVE "DESPLAZADO  " TO TIPO-PACI-J
                    WHEN OTHER MOVE "            " TO TIPO-PACI-J
                 END-EVALUATE                  
                 
                 EVALUATE TIPO-AFIL-PACI
                    WHEN 1     MOVE "COTIZANTE   " TO TIPO-AFIL-PACI-J
                    WHEN 2     MOVE "BENEFICIARIO" TO TIPO-AFIL-PACI-J
                    WHEN 3     MOVE "COT.PENSION." TO TIPO-AFIL-PACI-J
                    WHEN 4     MOVE "ADICIONAL   " TO TIPO-AFIL-PACI-J
                    WHEN 5     MOVE "CABEZA FAMIL" TO TIPO-AFIL-PACI-J
                    WHEN 6     MOVE "GRUPO FAMIL." TO TIPO-AFIL-PACI-J
                    WHEN 0     MOVE "SIN DETERMIN" TO TIPO-AFIL-PACI-J
                    WHEN OTHER MOVE "            " TO TIPO-AFIL-PACI-J
                 END-EVALUATE
                 MOVE NRO-AFIL-PACI    TO NRO-AFIL-PACI-J
                 
                 EVALUATE PARENT-PACI
                    WHEN "00"  STRING PARENT-PACI DELIMITED BY SIZE 
                                      INTO PARENT-PACI-J
                               END-STRING
                    WHEN "01"  STRING PARENT-PACI " CONYUGUE" DELIMITED BY SIZE 
                                      INTO PARENT-PACI-J
                               END-STRING
                    WHEN "02"  STRING PARENT-PACI " HIJO  " DELIMITED BY SIZE 
                                      INTO PARENT-PACI-J
                               END-STRING
                    WHEN "03"  STRING PARENT-PACI " PADRES" DELIMITED BY SIZE
                                      INTO PARENT-PACI-J
                               END-STRING
                    WHEN "04"  STRING PARENT-PACI " 2 GRADO" DELIMITED BY SIZE 
                                      INTO PARENT-PACI-J
                               END-STRING
                    WHEN "05"  STRING PARENT-PACI " 3 GRADO" DELIMITED BY SIZE 
                                      INTO PARENT-PACI-J
                               END-STRING
                    WHEN "06"  STRING PARENT-PACI " < 12  " DELIMITED BY SIZE 
                                      INTO PARENT-PACI-J
                               END-STRING
                    WHEN "07"  STRING PARENT-PACI " SUEGRO" DELIMITED BY SIZE 
                                      INTO PARENT-PACI-J
                               END-STRING
                    WHEN OTHER STRING PARENT-PACI "        " DELIMITED BY SIZE 
                                      INTO PARENT-PACI-J
                               END-STRING
                 END-EVALUATE
                 
                 MOVE ESTRATO-PACI     TO ESTRATO-PACI-J
                 MOVE ID-COTIZ-PACI    TO ID-COTIZANTE-J    
                 MOVE DIRECC-PACI      TO DIRECC-PACI-J        
                 MOVE TELEFONO-PACI    TO TELEFONO-PACI-J
                 MOVE DESCRIP-PACI     TO DESCRIP-PACI-J

                 MOVE ACOMPA-PACI      TO ACOMPA-PACI-J         
                 MOVE CABEZA-FAM-PACI  TO CABEZA-FAM-PACI-J      
                 MOVE CLASIF-PACI      TO CLASIF-PACI-J
                 

                 INSPECT FECHA-VENCE-PACI-J REPLACING FIRST "dd"  BY DIA-VENCE-PACI
                 INSPECT FECHA-VENCE-PACI-J REPLACING FIRST "aaaa" BY ANO-VENCE-PACI
                   
              END-IF
                 INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA
                 INSPECT LIN-1 REPLACING ALL LOW-VALUES BY SPACES      
                 MOVE ","               TO CIERRE-LIN-1
           END-IF.
           GO TO LEER-PACIENTES.

       CERRAR-PACIENTES.
           INITIALIZE LIN-1
           INSPECT LIN-1 REPLACING ALL "*" BY CARAC-COMILLA 

           STRING LIN-1, "]}" DELIMITED BY SIZE
                  INTO DATOS-PLANO-W
           END-STRING

           PERFORM DATOS-ENVIO.
  
       CERRAR-ARCHIVOS.  
           CLOSE ARCHIVO-PACIENTES
                 ARCHIVO-OCUPACIONES
                 ARCHIVO-ENTIDADES
                 ARCHIVO-CIUDAD.

010700 PAGINA-CONFIG.
010720     MOVE "statuscode" TO COBW3-CNV-NAME
010730     MOVE "00"         TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010720     MOVE "programa-id" TO COBW3-CNV-NAME
010730     MOVE "SER810"      TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_CNV" USING COBW3

010760     MOVE "..\PAGINAS\RECIBIDOSLISTADO.ASPX" TO SALIDA-HTML
010770     PERFORM ABRIR-HTML.

007990 CERRAR-SESION.
008000     CALL "COBW3_FREE" USING COBW3.
008010     MOVE 1 TO PROGRAM-STATUS.
008020     EXIT PROGRAM.

       DATOS-ENVIO.
010720     MOVE "datosrecibidos"   TO COBW3-CNV-NAME
010730     MOVE DATOS-PLANO-W      TO COBW3-CNV-VALUE
010740     CALL "COBW3_SET_REPEAT" USING COBW3.


       DEPURAR-REGISTRO.
           MOVE DATO-BUSCAR TO PATRON-W
           MOVE 0           TO SW-BUSCAR
           MOVE 3           TO TP-BUSCAR

           IF SW-PATRON (10) = SPACES
              PERFORM UBICAR-PATRON VARYING J FROM 10 BY -1
                                    UNTIL J < 4 OR SW-BUSCAR = 1
           ELSE
              MOVE 10 TO TP-BUSCAR
           END-IF

           MOVE 0 TO SW-BUSCAR
           PERFORM BUSCAR-PATRON.

       BUSCAR-PATRON.
           EVALUATE TP-BUSCAR
               WHEN  3 INSPECT DESCRIP-PACI  TALLYING SW-BUSCAR FOR ALL DATO-BUSCAR-3
               WHEN  4 INSPECT DESCRIP-PACI  TALLYING SW-BUSCAR FOR ALL DATO-BUSCAR-4
               WHEN  5 INSPECT DESCRIP-PACI  TALLYING SW-BUSCAR FOR ALL DATO-BUSCAR-5
               WHEN  6 INSPECT DESCRIP-PACI  TALLYING SW-BUSCAR FOR ALL DATO-BUSCAR-6
               WHEN  7 INSPECT DESCRIP-PACI  TALLYING SW-BUSCAR FOR ALL DATO-BUSCAR-7
               WHEN  8 INSPECT DESCRIP-PACI  TALLYING SW-BUSCAR FOR ALL DATO-BUSCAR-8
               WHEN  9 INSPECT DESCRIP-PACI  TALLYING SW-BUSCAR FOR ALL DATO-BUSCAR-9
               WHEN 10 INSPECT DESCRIP-PACI  TALLYING SW-BUSCAR FOR ALL DATO-BUSCAR
           END-EVALUATE.

       UBICAR-PATRON.
             IF SW-PATRON (J) = SPACES
                COMPUTE TP-BUSCAR = J - 1
             ELSE
                MOVE 1 TO SW-BUSCAR
             END-IF.

008040 COPY "..\..\FUENTES\SC-WEB19.CBL".
