TITLE
COLOR F9


taskkill /F /IM "W3WP.EXE">nul 2>&1

DEL *.OBJ
DEL *.SVD
DEL *.001
DEL *.EXP
DEL *.LIB
DEL *.MAK

XCOPY CON803-01.DLL  D:\WEB\MAIN-ELECT\APP\CONTAB\*.* /Y
XCOPY CON809.DLL     D:\WEB\MAIN-ELECT\APP\CONTAB\*.* /Y
PAUSE
