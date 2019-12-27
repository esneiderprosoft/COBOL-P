000010*=================================================================
000020*  File name : ISATerm.cob
000030*  Outline   : This is the program for termination which is
000040*               indispensable for using ISAPI
000050*               It is directly called from IIS.
000060*
000070* All Rights Reserved, CopyRight(C) FUJITSU LIMITED 2000-2002
000080*=================================================================
000090 identification division.
000100 program-id. "TerminateExtension".
000110 environment division.
000120 data division.
000130  linkage section.
000131 COPY "P:\PROG\PROYECT\FUENTES\ISAPIFLG.CBL".
000150*
000160 procedure division with stdcall linkage using ISAPI-FLAG.
000170*
000180     move 1 to program-status.
000190     exit program.
