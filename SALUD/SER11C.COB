      *============================================================
      * ACTUALIZACION GRUPO DE SERVICIOS FACTURACION - SER11C SALUD
      *============================================================
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. "HttpExtensionProc".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "..\..\FUENTES\FS-USUNET.CBL".
       COPY "..\..\FUENTES\FS-SESION.CBL".
      *"../control/SC-GRUPOSER.DAT"
           SELECT ARCHIVO-GR-SER LOCK MODE IS AUTOMATIC
                  ASSIGN NOM-GRUSER-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CODIGO-GRSER
                  FILE STATUS IS OTR-STAT.
       
       DATA DIVISION.
       FILE SECTION.
       COPY "..\..\FUENTES\FD-USUNET.CBL".
       COPY "..\..\FUENTES\FD-SESION.CBL".
       COPY "..\..\FUENTES\FD-GRSER.CBL".
       
       FD ARCHIVO-JSON
          LABEL RECORD STANDARD.
       01 REG-JSON.                  
          02 DATOS-JSON             PIC X(500).


       WORKING-STORAGE SECTION.
       COPY "..\..\FUENTES\COBW3.CBL".
       COPY "..\..\FUENTES\WEB-CARAC.CBL".

       01 TABLA-LLEGADA-W.
          02 DATO-LLEGADA-W PIC X OCCURS 200.

       01 LLEGADA-W         PIC X(100).
          02 CARA-TEMP.     PIC X OCCURS 50.

       77 NOM-PLANO-W       PIC X(90).
       77 NOM-GRUSER-W      PIC X(60).
       77 MES-SAL-W         PIC X(4).
       77 CONT              PIC 9999.

       01 VAR-LLEGADA-W.
          02 LLAVESESION-LLEGA-W.
             03 ID-LLEGAD-W            PIC X(15).
             03 FECHA-LLEGAD-W         PIC X(8).
             03 HORA-LLEGAD-W          PIC X(6).
          02 IP-SERVER-W               PIC X(18).
          02 DIR-CONTAB-LLEGADA-W      PIC X(8).
          02 MES-CONTAB-LLEGADA-W      PIC XX.

       01 DATOS-ENVIO.
          02 DATO-ENV                  PIC XX.
          



              
