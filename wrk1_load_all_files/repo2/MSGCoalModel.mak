VPATH = d:\mdsgold\extcode

.SUFFIXES:

FOR7DIR  = d:\mdsgold\extcode
OBJ7DIR = d:\mdsgold\extcode
FOR90 = F90
FOR95 = F95
F95EXE = "C:\Program Files (x86)\Lahey-Fujitsu Fortran\v7.2\Win32\Bin\lf95"


OBJS: CODEOBJ

CODEOBJ: MSGCoalFrontEnd.OBJ Coal_modules.OBJ

MSGCoalFrontEnd.OBJ: $(FOR7DIR)\$*.$(FOR90)
              $(F95EXE) -fix  $(FOR7DIR)\$*.$(FOR90)

Coal_modules.OBJ: $(FOR7DIR)\$*.$(FOR95)
              $(F95EXE) -fix $(FOR7DIR)\$*.$(FOR95)

############################
##That wraps up the objects
############################
MSGCoalFrontEnd.EXE: OBJS
            "C:\Program Files (x86)\Lahey-Fujitsu Fortran\v7.2\Win32\Bin\lf95" -nc  -EXE MSGCoalFrontEnd  @MSGCoalModel.lnk


