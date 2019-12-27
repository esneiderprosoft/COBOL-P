000010*---------------------------------------------------------------*
000020* Module Name :      GetExtensionVersion                        *
000030*                                                               *
000040*   All Rights Reserved, Copyright(C) FUJITSU LIMITED 2000-2002 *
000050*---------------------------------------------------------------*
000060 identification division.
000070 program-id. "GetExtensionVersion".
000080 environment division.
000090 data division.
000100 linkage section.
000101 COPY "..\..\FUENTES\ISAPIINF.CBL".
000120*
000130 procedure division with stdcall linkage using ISAPI-INFO.
000140     move 1 to program-status.
000150     exit program.
