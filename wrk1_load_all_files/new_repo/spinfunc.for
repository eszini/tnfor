!Last change: MSG 1/22/2004 12:56:02 PM
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! following are approximations of the documented Spindrift routines:
! they require linking with .obj-files extracted from LF9555\lib\cw32.lib:
! they is useful under DOS and WinConsole, but not under Windows GUI.


module SpinFuncLocals
  integer*2 &
    ScnBufSiz, &
    SupCAOfs , &
    LimitEnvSize, &
    SupEnvOffset
  integer*4 &
    pVGATextPage, &
    pMDATextPage
!!colors and attributes defined by Spindrift:
  integer*2    &
    BLKCHR   , &
    DBLUCHR  , &
    GREENCHR , &
    LBLUCHR  , &
    REDCHR   , &
    LAVCHR   , &
    YELLOCHR , &
    WHITECHR , &
    BLKBKG   , &
    DBLUBKG  , &
    GREENBKG , &
    LBLUBKG  , &
    REDBKG   , &
    LAVBKG   , &
    YELLOBKG , &
    WHITEBKG , &
    UNDERLINE, &
    NORMAL   , &
    BRITE    , &
    BRIGHT   , &
    INVERSE  , &
    FLASH
  parameter( &
    ScnBufSiz=4000, & ! 80x25x2
    SupCAOfs =3999, & ! always ScnBufSiz-1
    LimitEnvSize=4096, & ! sufficient to reach beyond end of env. table
    SupEnvOffset=4095, & ! always LimitEnvSize-1
!    pVGATextPage=Z'b8000', &
!    pMDATextPage=Z'b0000', &
!!colors and attributes defined by Spindrift:
    BLKCHR  =0, &
    DBLUCHR =1, &
    GREENCHR=2, &
    LBLUCHR =3, &
    REDCHR  =4, &
    LAVCHR  =5, &
    YELLOCHR=6, &
    WHITECHR=7, &
    BLKBKG  =  0, &
    DBLUBKG = 16, &
    GREENBKG= 32, &
    LBLUBKG = 48, &
    REDBKG  = 64, &
    LAVBKG  = 80, &
    YELLOBKG= 96, &
    WHITEBKG=112, &
    UNDERLINE=  1, &
    NORMAL   =  7, &
    BRITE    =  8, &
    BRIGHT   = 15, &
    INVERSE  =112, &
    FLASH    =128)

  character*1 NullChar,FindFileDataStruc(0:42) ! 43=21+1+2+2+4+13 bytes per various documents
  character*12 NameField
  character*16 FileName
  character*128 FindPrefix
  character*1024 sWork
  integer*1 FFSearchAttr,AttrByte ! Z'000e' for non-blinking yellow on black background
  integer*2 i,n,jBS,PosNullCh
  integer*4 FileSize32b,DateTime32b,cResult,DosError
  equivalence(FileSize32b,FindFileDataStruc( 4)) ! determined experimentally
  equivalence(DateTime32b,FindFileDataStruc(12))
  equivalence(NameField  ,FindFileDataStruc(16)) ! (30) per various documents
! declarations shared by Dat() and Clk()
  character*8 DateStr ! in CCYYMMDD format
  character*10 TimeStr ! in HHMMSS.SSS format
  character*5 ZoneStr
  integer*4 DaTiValues(8)
  data FFSearchAttr/0/ ! 0 for 'normal' files; 63 for volume-label only
  save FFSearchAttr
end module

  subroutine cls(ULRow,ULColumn,LRRow,LRColumn)
  integer, optional :: ULRow,ULColumn,LRRow,LRColumn

     if(PRESENT(LRColumn)) then
       call cls4(ULRow,ULColumn,LRRow,LRColumn)
     elseif(PRESENT(LRRow)) then
       call cls3(ULRow,ULColumn,LRRow)
     elseif(PRESENT(ULColumn)) then
       call cls2(ULRow,ULColumn)
     elseif(PRESENT(ULRow)) then
       call cls1(ULRow)
     else
       call cls0
     endif
  end subroutine cls

subroutine cls4(ULRow,ULColumn,LRRow,LRColumn)
      use SpinDriftLib
  integer ULRow,ULColumn,LRRow,LRColumn,iRow,jCol
! use SpinFuncLocals

! Regs(1)=Z'0600' ! video BIOS function to scroll/clear a window
! Regs(2)=AttrByte
! Regs(3)=(ULRow shl 8).or.ULColumn ! top row of window, leftmost column
! Regs(4)=(LRRow shl 8).or.LRColumn ! end row of window, rightmost column
! call intrup(Regs,Z'10')
  return ! (do nothing)
!  do iRow=ULRow,LRRow
!    call locate(iRow,ULColumn) ! ClrEol would be presumptuous
!    write(*,'(80a1)',advance='no') (' ',jCol=ULColumn,LRColumn)
!  end do
end


subroutine cls3(iRow,BegColumn,EndColumn)
  integer iRow,BegColumn,EndColumn

! Regs(1)=Z'0600' ! video BIOS function to scroll/clear a window
! Regs(2)=AttrByte
! Regs(3)=(iRow shl 8).or.BegColumn ! top row of window, leftmost column
! Regs(4)=(iRow shl 8).or.EndColumn ! end row of window, rightmost column
! call intrup(Regs,Z'10')
  call cls4(iRow,BegColumn,iRow,EndColumn)
end


subroutine cls2(i0,i1) ! 0-based to match SpinDrift cls arguments
      use SpinDriftLib
  integer i0,i1,i ! ,j,j1 ! 80th char written on row i==24 causes scrolling

! j1=80
!  do i=i0,i1
!    call locate(i,0)
!   if(i==24) j1=79 ! assume window is 80 characters wide, 25 rows high
!   write(*,'(80a1)',advance='no') (' ',j=1,j1)
!    call clrel()
!  end do
end


subroutine cls1(iRow)
      use SpinDriftLib
  integer iRow

! Regs(1)=Z'0600' ! video BIOS function to scroll/clear a window
! Regs(2)=AttrByte
! Regs(3)=iRow shl 8 ! top row of window, leftmost column
! Regs(4)=(iRow shl 8).or.79 ! end row of window, rightmost column
! call intrup(Regs,Z'10')
! call cls4(iRow,0,iRow,79)
!  call locate(iRow,0)
!  call clrel()
  return ! (do nothing)
end


subroutine cls0()
      use SpinDriftLib
! integer*2 iMode

! Regs(1)=Z'0f00' ! video BIOS function to get current mode
! Regs(2)=AttrByte
! call intrup(Regs,Z'10')
! iMode=iand(Regs(1),Z'ff')
! VideoMod(iMode) ! clears screen as it resets the current mode

! call cls4(0,0,24,79)
! (do nothing)  call clrsc()
  call locate(0,0)
end

integer*2 function IsError()
  use SpinFuncLocals

  IsError=DosError
end


character*1 function GetDrive()
  USE SERVICE_ROUTINES
  CHARACTER*256 CURRENT_DIRECTORY
  INTEGER IY
   IY = GETCWD(CURRENT_DIRECTORY)
   IF(IY <= 0) THEN
      GetDrive = CURRENT_DIRECTORY(1:1)
   ELSE
      GetDrive = 'C'
   ENDIF
end

subroutine CurDir(DriveCharIn,CurPath) ! returns without trailing '\'
  USE SERVICE_ROUTINES
  character*1 DriveCharIn
  character*(*) CurPath
  CHARACTER*1024 CURRENT_DIRECTORY
  integer IY
  IY = GETCWD(CURRENT_DIRECTORY)
  IF(IY == 0) THEN
     IY = LEN_TRIM(CURRENT_DIRECTORY)
     IF(CURRENT_DIRECTORY(IY:IY) == '\') THEN
        CURRENT_DIRECTORY(IY:IY) = ' '
     ENDIF
     CurPath = TRIM(CURRENT_DIRECTORY)
  ELSE
     CurPath = ' '
  ENDIF
end ! subroutine CurDir




subroutine GetArgs(ArgV,ArgC)
! use SpinFuncLocals
  integer*4 ArgC
  character*(*) ArgV(ArgC) ! was *16 ArgV(*), conflicted with caller
  character*1 ABlank
  parameter(ABlank=' ')
  character*1 ThisChar,PrevChar ! ,pCmdChar
  integer*2 i ! ,pEnvSeg,PSPSeg
! integer*1 pTailCount
  character*128 CmdString

  call getcl(CmdString)



  ArgC=0
  ThisChar=ABlank
  do i=1,len_trim(CmdString)
    PrevChar=ThisChar
    ThisChar=CmdString(i:i)
    if(ThisChar/=ABlank) then
      if(PrevChar==ABlank) then
        ArgC=ArgC+1 ! if only blanks delimit parameters
        ArgV(ArgC)=''
      end if
      ArgV(ArgC)=trim(ArgV(ArgC))//ThisChar
!     write(*,*) ArgC,i,ThisChar,ArgV(ArgC),CmdString
    end if
  end do
end ! subroutine GetArgs


logical*4 function iScroll()
! use SpinFuncLocals

! Regs(1)=Z'0200' ! BIOS function used to read the kbd flags
! call intrup(Regs,Z'16') ! AGT found no DOS-level function for this
! iScroll=iand(Regs(1),16)>0
  iScroll=.false.
end


logical*4 function KeyAvail()
! use SpinFuncLocals
!  integer*4 kbhi

! Regs(1)=Z'0b00' ! function used to check kbd status
! call intrup(Regs,Z'21')
! KeyAvail=iand(Regs(1),Z'ff')>0
  KeyAvail=.false.
end


subroutine ClrKb()
! use SpinFuncLocals

! Regs(1)=Z'0c08' ! flush kbd buffer
! call intrup(Regs,Z'21')
  call flush(5)
end


subroutine ReadKey(AsciiCode)
! use SpinFuncLocals
  integer*1 AsciiCode
  integer*4 getc
! character*1 AnyChar

! Regs(1)=Z'0800' ! get character without echo
! call intrup(Regs,Z'21')
! AsciiCode=iand(Regs(1),Z'ff')

! read(5,*) AnyChar ! note that this requires a terminating <Enter>
! AsciiCode=IAChar(AnyChar)
  AsciiCode=getc()
end


subroutine AnyKey() ! i.e., wait for any alphanumeric key to be struck
! use SpinFuncLocals
  integer*1 AsciiCode

  call flush(5)
  do
!   Regs(1)=Z'0c08' ! flush kbd buffer, get character without echo
!   call intrup(Regs,Z'21')
!   AsciiCode=iand(Regs(1),Z'ff')
! write(*,*)AsciiCode,char(AsciiCode)
    call ReadKey(AsciiCode)
    if(((48<=AsciiCode).and.(AsciiCode<= 57)) .or. & ! '0'..'9'
       ((65<=AsciiCode).and.(AsciiCode<= 90)) .or. & ! 'A'..'Z'
       ((97<=AsciiCode).and.(AsciiCode<=122))) exit  ! 'a'..'z'
  end do
end


character*1 function GetCh(KeyType)
  use SpinFuncLocals
  integer*1 KeyType,B,AsciiCode
  character*1 KeyedChar,UCKC,UpCase
  logical*1 AltPrefix,CtlPrefix,NulPrefix,EscKeyStruck

! do
!   Regs(1)=Z'0200' ! BIOS function used to read the kbd flags
!   call intrup(Regs,Z'16')
!   B=iand(Regs(1),Z'ff')
!   AltPrefix=iand(B,Z'08')>0
!   CtlPrefix=iand(B,Z'04')>0
!   if(KeyAvail()) exit
! end do

  AltPrefix=.false.
  CtlPrefix=.false.
  call ReadKey(AsciiCode)
  EscKeyStruck=AsciiCode==27
  NulPrefix=AsciiCode==0
!!write(*,*)AltPrefix,CtlPrefix,NulPrefix,EscKeyStruck,AsciiCode,char(AsciiCode)
  if(NulPrefix) then ! extended-ASCII keys return a 0-byte prefix
!   if(KeyAvail()) call ReadKey(AsciiCode) ! flush the keystroke buffer
                   call ReadKey(AsciiCode) ! flush the keystroke buffer
  end if
!!write(*,*)AltPrefix,CtlPrefix,NulPrefix,EscKeyStruck,AsciiCode,char(AsciiCode)
  KeyedChar=char(AsciiCode)
  UCKC=UpCase(KeyedChar)

!!if(AltPrefix .and.((15<=AsciiCode).and.(AsciiCode<=50))) then
!!  KeyedChar=AltChar(AsciiCode)
!!  AsciiCode=iAChar(KeyedChar)
!!end if ! decoding the keyboard-specific scan-codes

!!KeyType is coded to conform with Spindrift; ignore Function keys F11 & F12
  if(NulPrefix) then
    if    ((59<=AsciiCode).and.(AsciiCode<=68)) then
      KeyType=0 ! Function-key
    elseif(((71<=AsciiCode).and.(AsciiCode<=73)).or. &
           ((75<=AsciiCode).and.(AsciiCode<=77)).or. &
           ((79<=AsciiCode).and.(AsciiCode<=83))) then
      KeyType=8 ! Edit-key
    end if
  elseif(AltPrefix) then; KeyType=64 ! Alt-key
  elseif(AsciiCode==13) then; KeyType=4 ! Enter-key
  elseif(EscKeyStruck) then; KeyType=16 ! Esc-key
  elseif(((48<=AsciiCode).and.(AsciiCode<=57)).or. & ! '0'..'9'
    (KeyedChar=='+').or. &
    (KeyedChar=='-').or. &
    (KeyedChar=='.')) then; KeyType=3 ! Numeric
  elseif(CtlPrefix) then
  ! if    (KeyedChar=='m') then; KeyType=4 ! Enter-key
    if    (KeyedChar=='h') then; KeyType=8 ! Edit-key (backspace)
    elseif(KeyedChar=='i') then; KeyType=8 ! Edit-key (tab)
    elseif(KeyedChar=='j') then; KeyType=8 ! Edit-key (linefeed)
    elseif(KeyedChar=='(') then; KeyType=16 ! Esc-key
    else                       ; KeyType=32 ! Ctrl-key
    end if
  elseif((8<=AsciiCode).and.(AsciiCode<=10)) then; KeyType=8 ! Edit-key (BS,tab,LF)
!!elseif(('A'<=UCKC).and.(UCKC<='Z')) then; KeyType:= 2; ! Alphabetic
  else; KeyType=2 ! Alphabetic (seems the safest default)
  end if
!!write(*,*)KeyType,AsciiCode,KeyedChar
  GetCh=KeyedChar
end ! function GetCh


logical*4 function escape()
  use SpinFuncLocals
  integer*1 AsciiCode
  logical*4 KeyPending,KeyAvail

  KeyPending=KeyAvail()
  if(KeyPending) then
    call ReadKey(AsciiCode)
    escape=AsciiCode==27
  else
    escape=.false.
  end if
end


subroutine AppendNullTerm(s)
  use SpinFuncLocals
  character*(*) s

  PosNullCh=min(len(s),len_trim(s)+1)
  s(PosNullCh:PosNullCh)=NullChar ! c-language strings end in a null byte
end


subroutine TrapDosError()
  use SpinFuncLocals
  USE SERVICE_ROUTINES
  integer*4 errn,doserrno

   DosError= IERRNO()
end



 subroutine DisplayCString(s)
   character*(*) s
   integer*2 j
   do j=1,1024
!     write(*,*) j,iachar(s(j:j)),' ',s(j:j)
     if(iachar(s(j:j))==0) exit
   ! if(s(j:j)==' ') exit
   end do
   call AnyKey()
 end




subroutine erase(PathAndName)

  character*(*) PathAndName
  integer IOS
  open(10,file=PathAndName,IOSTAT=IOS)
  close(10,status='delete',IOSTAT=IOS)
!  call system("DEL "//PathAndName)
end ! subroutine erase


subroutine SetChr(CharVar,iValue)
  character CharVar
  integer*1 iValue

  CharVar=char(iValue)
end


!subroutine MovBytes(Sour,SourOfs,Dest,DestOfs,nBytes)
!  integer*1 Sour(*),Dest(*)
!  integer*4 nBytes,SourOfs,DestOfs
!! call cMove(Sour(SourOfs),Dest(DestOfs),nBytes)
!  Dest(DestOfs:DestOfs+nBytes-1)=Sour(SourOfs:SourOfs+nBytes-1)
!end


subroutine copy(SourFile,DestFile)
  character*(*) SourFile,DestFile

!!string sent to system is limited to 122 characters; AGT sees no faster way
  call system("copy "//trim(SourFile)//" "//trim(DestFile)) ! may need /C prefix
  call TrapDosError()
end


integer*4 function FilSiz(PathAndName)
  use SpinFuncLocals
  character*(*) PathAndName
  integer*4 LenInBytes
  logical*4 Extant

  inquire(file=PathAndName,exist=Extant,fLen=LenInBytes)
  if(Extant) then
    DosError=0
    FilSiz=LenInBytes
  else
    DosError=2 ! file not found
    FilSiz=0
  end if
end


character*11 function Clk() ! in HH:MM:SS.SS format
  use SpinFuncLocals

  call Date_And_Time(DateStr,TimeStr,ZoneStr,DaTiValues)
  Clk=TimeStr(1:2)//':'//TimeStr(3:4)//':'//TimeStr(5:9)
end


character*8 function Dat() ! in MM/DD/YY format
  use SpinFuncLocals

  call Date_And_Time(DateStr,TimeStr,ZoneStr,DaTiValues)
  Dat=DateStr(5:6)//'/'//DateStr(7:8)//'/'//DateStr(3:4)
end


subroutine Tone2(f1,f2,duration)
  integer*2 f1,f2,duration ! in Hz and hundredths of a second

!  write(*,*) char(7) ! sound the bell
end


subroutine QuickR(nItems,a,iPos) ! sorts nItems items into ascending order
!!based on values in (1-based) key array a;
!!Shell sort adapted from the source on page 110 of
!!SOFTWARE TOOLS IN PASCAL, by Kernighan & Plauger, Addison-Wesley 1981
  integer*4 nItems,i,j,k,Gap,iPsHeld,iPos(*)
  real*4 a(*)

  do i=1,nItems ! assume that the caller has not initialized the index array
    iPos(i)=i
  end do
  Gap=nItems/2
  do while(Gap>0)
    do i=Gap,nItems
      j=i-Gap
      do while(j>0)
        k=j+Gap
        if(a(iPos(j))<=a(iPos(k))) then
          j=0 ! break the while loop (assign j=-1 for 0-based arrays)
        else ! interchange the indexing array values
          iPsHeld=iPos(k)
          iPos(k)=iPos(j)
          iPos(j)=iPsHeld
        end if
        j=j-Gap
        CALL RW_PROCESS_MESSAGES()
      end do
      CALL RW_PROCESS_MESSAGES()
    end do
    Gap=Gap/2
    CALL RW_PROCESS_MESSAGES()
  end do
end ! subroutine QuickR

subroutine QuickI(nItems,a,iPos) ! sorts nItems items into ascending order
!!based on values in (1-based) key array a;
!!Shell sort adapted from the source on page 110 of
!!SOFTWARE TOOLS IN PASCAL, by Kernighan & Plauger, Addison-Wesley 1981
  integer*4 nItems,i,j,k,Gap,iPsHeld,iPos(*)
  integer*2 a(*)

  do i=1,nItems ! assume that the caller has not initialized the index array
    iPos(i)=i
  end do
  Gap=nItems/2
  do while(Gap>0)
    do i=Gap,nItems
      j=i-Gap
      do while(j>0)
        k=j+Gap
        if(a(iPos(j))<=a(iPos(k))) then
          j=0 ! break the while loop (assign j=-1 for 0-based arrays)
        else ! interchange the indexing array values
          iPsHeld=iPos(k)
          iPos(k)=iPos(j)
          iPos(j)=iPsHeld
        end if
        j=j-Gap
        CALL RW_PROCESS_MESSAGES()
      end do
      CALL RW_PROCESS_MESSAGES()
    end do
    Gap=Gap/2
    CALL RW_PROCESS_MESSAGES()
  end do
end ! subroutine QuickI

subroutine ConditionalQuickI(nItems,a,iPos,Condition) ! sorts nItems items into ascending order
!!based on values in (1-based) key array a;
!!Shell sort adapted from the source on page 110 of
!!SOFTWARE TOOLS IN PASCAL, by Kernighan & Plauger, Addison-Wesley 1981
  integer*4 nItems,i,j,k,Gap,iPsHeld,iPos(*)
  integer*2 a(nItems),Condition(nItems)

  do i=1,nItems ! assume that the caller has not initialized the index array
    iPos(i)=i
  end do
  Gap=nItems/2
  do while(Gap>0)
    do i=Gap,nItems
      j=i-Gap
      do while(j>0)
        k=j+Gap
        if(a(iPos(j))<=a(iPos(k)) .or. Condition(iPos(j))<=Condition(iPos(k))) then
          j=0 ! break the while loop (assign j=-1 for 0-based arrays)
        else ! interchange the indexing array values
          iPsHeld=iPos(k)
          iPos(k)=iPos(j)
          iPos(j)=iPsHeld
        end if
        j=j-Gap
        CALL RW_PROCESS_MESSAGES()
      end do
      CALL RW_PROCESS_MESSAGES()
    end do
    Gap=Gap/2
    CALL RW_PROCESS_MESSAGES()
  end do
end ! subroutine QuickI


integer*2 function VideoTyp()
! use SpinFuncLocals

! Regs(1)=Z'0f00'
! call intrup(Regs,Z'10')
!!nCol:= Regs(1) shr 8;
! VideoTyp=iand(Regs(1),Z'ff')
  VideoTyp=3 ! for 80x25 color
end


subroutine VideoMod(iMode) ! sets the video mode
! use SpinFuncLocals
  integer*2 iMode

! Regs(1)=iand(iMode,Z'ff')
! call intrup(Regs,Z'10')
end


subroutine CaptScn(DestBuffer)
  use SpinFuncLocals
  character*1 DestBuffer(0:*)
  integer*2 VideoTyp  ,j

  if(VideoTyp()<7) then
!    call memcp(DestBuffer,%val(pVGATextPage),cArg(ScnBufSiz))
  else
!    call memcp(DestBuffer,%val(pMDATextPage),cArg(ScnBufSiz))
  end if
!  write(*,'(1x,40a1)') (DestBuffer(j),j=0,158,2)
end


subroutine DispScn(SourBuffer)
  use SpinFuncLocals
  integer SourBuffer(*)
  integer*2 VideoTyp

  if(VideoTyp()<7) then
!    call memcp(%val(pVGATextPage),SourBuffer,cArg(ScnBufSiz))
  else
!    call memcp(%val(pMDATextPage),SourBuffer,cArg(ScnBufSiz))
  end if
end


subroutine CurOn()
! use SpinFuncLocals

! Regs(1)=Z'0300' ! video BIOS function to get cursor position & size
! Regs(2)=0 ! for page 0
! call intrup(Regs,Z'10')

! Regs(3)=iand(Regs(3),Z'1fff') ! clear bit 5 of CH to force cursor on
! Regs(1)=Z'0100' ! video BIOS function to set cursor size
! Regs(2)=0 ! for page 0
! call intrup(Regs,Z'10')
end





subroutine locate_22(Row,Col) ! 0-based (Row,Col) to match Spindrift arguments
  integer*2 Row,Col
  integer*4 cResult,movecursor

!  cResult=movecursor(cArg(Col),cArg(Row)) ! c function uses 0-based args
!  write(*,'(a1)',advance='no') ' '
! (do nothing) call movecursor(cArg(Col),cArg(Row)) ! c function uses 0-based args
end
subroutine locate_24(Row,Col) ! 0-based (Row,Col) to match Spindrift arguments
  integer*2 Row
  integer*4 Col
  integer*4 cResult,movecursor

!  cResult=movecursor(cArg(Col),cArg(Row)) ! c function uses 0-based args
!  write(*,'(a1)',advance='no') ' '
! (do nothing) call movecursor(cArg(Col),cArg(Row)) ! c function uses 0-based args
end
subroutine locate_42(Row,Col) ! 0-based (Row,Col) to match Spindrift arguments
  integer*4 Row
  integer*2 Col
  integer*4 cResult,movecursor

!  cResult=movecursor(cArg(Col),cArg(Row)) ! c function uses 0-based args
!  write(*,'(a1)',advance='no') ' '
! (do nothing) call movecursor(cArg(Col),cArg(Row)) ! c function uses 0-based args
end
subroutine locate_44(Row,Col) ! 0-based (Row,Col) to match Spindrift arguments
  integer*4 Row,Col
  integer*4 cResult,movecursor

!  cResult=movecursor(cArg(Col),cArg(Row)) ! c function uses 0-based args
!  write(*,'(a1)',advance='no') ' '
! (do nothing) call movecursor(cArg(Col),cArg(Row)) ! c function uses 0-based args
end


subroutine ClrScr()
      use SpinDriftLib
! (do nothing)  call clrsc()
  call locate(0,0)
end

subroutine SetAttr(iVal)
  use SpinFuncLocals
  integer*2 iVal

!!set global variable for use in cls and PrintS subroutines
  AttrByte=iVal
end


subroutine PrintS(s)
  use SpinDriftLib
  use SpinFuncLocals
  character*(*) s
! integer*2 jCh,iRow,jCol

! Regs(1)=Z'0300' ! video BIOS function to get cursor position & size
! Regs(2)=0 ! for page 0
! call intrup(Regs,Z'10')
! iRow=Regs(4) shr 8 ! DX has cursor row and column as needed below
! jCol=iand(Regs(4),Z'ff')
! do jCh=1, len(s)
!   Regs(3)=1 ! repeat-count
!   Regs(2)=AttrByte ! BH=0 for page 0
!   Regs(1)=Z'0900' + iAChar(s(jCh))
! ! video BIOS function to write a char to CRT, not advancing cursor
!   call intrup(Regs,Z'10')
!   jCol=jCol+1
!   if(jCol>79) then ! assume full screen of 25x80 chars
!     jCol=0
!     iRow=iRow+1
!     if(iRow>24) then
!       iRow=24
!       Regs(1)=Z'0601' ! video BIOS function to scroll/clear a window
!       Regs(2)=AttrByte
!       Regs(3)=0 ! top row of window, leftmost column
!       Regs(4)=(24 shl 8).or.79 ! end row of window, rightmost column
!       call intrup(Regs,Z'10')
!     end if
!   end if
!   call locate(iRow,jCol) ! advance the cursor
! end do
!  write(*,'(a)',advance='no') s
end


character*1 function UpCase(c)
  character*1 c

  if(('a'<=c).and.(c<='z')) then
    UpCase=char(iAChar(c)-32)
  else
    UpCase=c
  end if
end

character*1 function LowCase(c)
  character*1 c

  if(('A'<=c) .and. (c<='Z')) then
    LowCase = char(iAChar(c)+32)
  else
    LowCase = c
  end if
end


subroutine UpC(sInp,sOut)
  character*1024 sWork
  character*(*) sInp,sOut
  CHARACTER*1 UpCase
  INTEGER UPCHARS,I

        sWork = trim(sInp)
        UPCHARS = LEN_TRIM(sWork)
        DO I = 1, UPCHARS
           sWork(I:I) = UpCase(sWork(I:I))
        END DO
!        call strup(sWork)
        sOut= trim(sWork)
end


subroutine LwC(sInp,sOut)
  character*1024 sWork
  character*(*) sInp,sOut
  INTEGER UPCHARS,I
  CHARACTER*1 LowCase

        sWork = trim(sInp)
        UPCHARS = LEN_TRIM(sWork)
        DO I = 1, UPCHARS
           sWork(I:I) = LowCase(sWork(I:I))
        END DO
        sOut= trim(sWork)
end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! the following text-window routines are undocumented in the Spindrift
! Utility Library Reference manual, Revision E, April 1992 that AGT had;
! hence they are merely stubbed out to satisfy the linker;
! the call to undocumented UNDER0() in MIDAS.F90 was disabled.


subroutine WINDBUF(ScreenBuf,BorderTitle,BorderAttr)
  integer*4 ScreenBuf(*)
  integer*2 BorderAttr
  character*(*) BorderTitle
end


subroutine OPENWIND(WindowHandle,ULRow,ULCol,LRRow,LRCol,iDummy,CharAttr)
  integer WindowHandle,ULRow,ULCol,LRRow,LRCol,iDummy
  integer*2 CharAttr
end


subroutine SETATTRW(WindowHandle,CharAttr)
  integer*2 WindowHandle,CharAttr
end


subroutine FILLB(WindowHandle,FillString,CharAttr)
  integer WindowHandle
  integer*2 CharAttr
  character*(*) FillString
end


subroutine BLDB(WindowHandle,Row,Col,FillString,CharAttr)
  integer WindowHandle,Row,Col
  integer*2 CharAttr
  character*(*) FillString
end


subroutine CLRW(WindowHandle)
  integer*2 WindowHandle
end

subroutine LOCATEW_444(WindowHandle,Row,Col)
  integer*4 WindowHandle,Row,Col
end

subroutine LOCATEW_442(WindowHandle,Row,Col)
  integer*4 WindowHandle,Row
  integer*2 Col
end

subroutine LOCATEW_424(WindowHandle,Row,Col)
  integer*4 WindowHandle,Col
  integer*2 Row      
end

subroutine LOCATEW_422(WindowHandle,Row,Col)
  integer*4 WindowHandle
  integer*2 Row,Col 
end

subroutine LOCATEW_244(WindowHandle,Row,Col)
  integer*4 Row,Col
  integer*2 WindowHandle
end

subroutine LOCATEW_242(WindowHandle,Row,Col)
  integer*4 Row
  integer*2 WindowHandle,Col
end

subroutine LOCATEW_224(WindowHandle,Row,Col)
  integer*4 Col
  integer*2 WindowHandle,Row
end

subroutine LOCATEW_222(WindowHandle,Row,Col)
  integer*2 WindowHandle,Row,Col
end
    
subroutine PRINTW(WindowHandle,ThisString)
  integer WindowHandle
  character*(*) ThisString
end


subroutine OPENVS(nLines,LineLeng,iDummy)
  integer*2 nLines,LineLeng,iDummy
end


subroutine CLSVS(nLines)
  integer*2 nLines
end


subroutine LOCVS(nLines,Row,Column)
  integer*2 nLines,Row,Column
end


subroutine PRINTV(nLines,ThisString)
  integer*2 nLines
  character*(*) ThisString
end


subroutine VIEW(nLines,nRows,Row,Col)
  integer*2 nLines,nRows,Row,Col
end


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! the following are not related to the Spindrift routines, but deal with
! other discrepancies between F77L3 and the current (LF95) compiler:


subroutine InitDAFile(DafUnit,RecordLeng,FileName)
! write the direct-access file's header, compatible with existing F77L3 files
  integer*2 DafUnit,RecordLeng
  character*(*) FileName
  integer*1 F7
  F7 = 247 !z"f7"                                     ! status='REPLACE' ?
  open(DafUnit,file=FileName,access='TRANSPARENT',status='UNKNOWN') ! access='BINARY' failed to compile
! header is considered to have index rec=0 in F77L3, rec=1 in LF95
  write(DafUnit,rec=1) F7,RecordLeng,-1_1 ! item lengths in bytes:  1,2,1
! write(DafUnit,rec=1) -9_1,RecordLeng,-1_1 ! item lengths in bytes:  1,2,1
! HeaderInitByte is always z'f7'=-9_1 for RecordLeng>2
! EndFile(DafUnit) ! appropriate only to sequential files, failed to solve
                   ! the problem of only the LSB of RecordLeng being retained
                   ! in the file's 2nd byte, with the 3rd byte amazingly zero;
                   ! AGT hypothesizes that the C-language underpinnings of LF95
                   ! may truncate the file-size to an integer number of 16-bit
                   ! words when executing the close statement
! call flush(DafUnit) ! neither did this solve the above problem
! AGT 20000915 circumvented the problem by writing -1_1 to the 4th byte,
! thus filling out an integer number of 16-bit words (and an entire 32-bit
! double-word), but believes this is a bug in the LF95 v5.50f compiler
  close(DafUnit)
end


subroutine GetDAFileRecLen(DafUnit,RecordLeng,FileName)
  integer*2 DafUnit,RecordLeng
  character*(*) FileName
  character*1 HeaderInitByte

  RecordLeng=0
  open(DafUnit,file=FileName,access='transparent',status='old',err=9)
  read(DafUnit) HeaderInitByte,RecordLeng
9 close(DafUnit)
end


integer*2 function NextRecord(DafUnit)
  integer*2 DafUnit
  integer*4 ios,nr
  logical*4 opnd

  inquire(unit=DafUnit,IoStat=ios,opened=opnd,NextRec=nr)
  if((ios==0).and.opnd.and.(nr>0)) then
    NextRecord=nr
  else
    NextRecord=0
  end if
end


subroutine WriteStarNA(s)
  character*(*) s

!  write(*,'(a)',advance='no') s
end
