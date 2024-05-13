!     ******************************************************************
!     FILE_IOS.F90
!     Copyright(c)  2000
!
!     Created: 1/14/2011 12:57:53 PM
!     Author : MARK S GERBER
!     Last change: MSG 8/16/2011 12:02:59 PM
!     ******************************************************************

c-----
      subroutine GetArgs(ArgV,ArgC)
!     use SpinFuncLocals
      character*(*) ArgV(ArgC) ! was *16 ArgV(*), conflicted with caller
      integer*4 ArgC,nOfInt
      character*1 ABlank,AQuote
      parameter(ABlank=' ',AQuote='"')
      character*2048 CmdString
      character*1 ThisChar,PrevChar
      integer*2 j,jBat,jFirstB,jFinalB,j1stChar,jMax,LenArg,nQuoteMarks
      logical*1 InsideQuotes,BatchProcessing
!
      nOfInt=ArgC ! upper bound on number of arguments of interest to caller
c     write(*,*) nOfInt,' args expected or allocated for GetArgs'
      call GetCL(CmdString) ! converts each null-string ("") found into ABlank
c     write(*,'(i5,3a)') nOfInt,'[',trim(CmdString),']'
      jBat=index(CmdString,'/BAT')
      BatchProcessing=(jBat/=0)
c     BatchProcessing=(index(CmdString,'/BAT')/=0)
c    +  .or.          (index(CmdString,'/QUICKSILVER')/=0) ! N.A. per TS 20061201
      if(BatchProcessing) then
        call SetBatchMode()
        CmdString(jBat:jBat+3)='    ' ! needed in case nOfInt is not fully used
      end if
!
!     LF95v7p1 compresses runs of blanks to 1 between counted arguments
      ArgC=0
      nQuoteMarks=0
      ThisChar=ABlank
      jMax=len_trim(CmdString)
      do j=1,jMax
        PrevChar=ThisChar
        ThisChar=CmdString(j:j)
        if(ThisChar==AQuote) then
          nQuoteMarks=nQuoteMarks+1 ! possible with LF95v7p1
          cycle ! do not retain any AQuote characters
        end if
        InsideQuotes=(mod(nQuoteMarks,2)>0)
        if((ThisChar/=ABlank).or.InsideQuotes) then
          if(((PrevChar==ABlank).and.(.not.InsideQuotes)).or.
     +       ( PrevChar==AQuote)) then ! count another argument
c           if(ArgC>0) write(*,'(2i5," [",a,"]")') ! useful during debugging
c    +        ArgC,j,trim(ArgV(ArgC))
            if(ArgC>=nOfInt) exit ! ignore trailing parameters not of interest
            ArgC=ArgC+1 ! if only blanks delimit parameters
            ArgV(ArgC)=''
            j1stChar=j
            LenArg=0
          end if
          if(ArgC<=nOfInt) then ! test was strict inequality before 20061113
!           ArgV(ArgC)=trim(ArgV(ArgC))//ThisChar ! if embedded blanks are disallowed
            if(LenArg==0) then
              ArgV(ArgC)=ThisChar
            else
              ArgV(ArgC)=ArgV(ArgC)(1:LenArg)//ThisChar
            end if
            LenArg=LenArg+1 ! needed since embedded blanks are allowed after 20060705
        ! after 20061113, do not presume that CmdString has no extra keywords,
        ! so do not take the balance of the CmdString as a single (last) Arg
c         else ! ArgC==nOfInt
c           if(InsideQuotes.and.(CmdString(jMax:jMax))==AQuote)
c    +        jMax=jMax-1
c           ArgV(ArgC)=trim(CmdString(j1stChar:jMax))
c           exit ! with final arg containing the entire tail of CmdString
          end if
c         write(*,'(2i5,3(1x,a))') ArgC,j,ThisChar,
c    +      trim(ArgV(ArgC)),trim(CmdString)
        end if
      end do
c     write(*,'(2i5," [",a,"]")') ArgC,j,trim(ArgV(ArgC)) ! useful during debugging
      end ! subroutine GetArgs
c-----
      character*8 function Dat()
      character*10 TimeOfDay
      character*8 Hold,CYMDStr
      character*5 ReGMT
      integer*4 DateTimeIntValues(8)
!
      call date_and_time(CYMDStr,TimeOfDay,ReGMT,DateTimeIntValues)
    ! above returns CYMDStr in ccyymmdd format, where cc is century
      write(Hold,'(a2,"/",a2,"/",a2)') ! conform to mm/dd/yy of Spindrift's Dat()
     +  CYMDStr(5:6), ! mm
     +  CYMDStr(7:8), ! dd
     +  CYMDStr(3:4)  ! yy
      Dat=Hold
      end
c-----
c     subroutine aRead(s,LimLeng)
c     integer*4 LimLeng
c     integer*2 LengNB
c     character*(LimLeng) s
c     character*255 sAsRead
!
c     sAsRead=' '
c     do
c       read(*,'(a)') sAsRead
c       if(LengNB(sAsRead)<=LimLeng) exit
c       write(*,'(a,i3,a)')
c    +    ' response exceeds length-limit of ',LimLeng,'; retry: '
c     end do
c     s=sAsRead
c     end
c-----
      subroutine AnyKey ! weak attempt to emulate SpinDrift routine
      read(*,*) ! requires <Enter>; prompt should indicate this, not 'any key'
      end
c-----
      subroutine curon
        continue ! call movecursor(carg(int4(79_2)),carg(int4(24_2))) ! force next write(*,.) to scroll the screen
      end
c-----
      subroutine cls4(iRow,iCol,fRow,fCol) ! assume 80x25 screen
      integer*4 i,j,iRow,iCol,fRow,fCol ! SpinDrift specifies INTEGER, which seems wasteful
!
      do i=iRow,fRow
        continue ! call movecursor(carg(int4(int2(iCol))),carg(int4(int2(i))))
        write(*,'( 80a1)',advance='no') (' ',j=iCol,fCol)
      end do
      end
c-----
      subroutine cls1(iRow) ! assume 80x25 screen
      integer*4 iRow
!
      continue ! call movecursor(carg(int4(0_2)),carg(int4(int2(iRow))))
      write(*,'( a80)',advance='no') ' '
      end
c-----
      subroutine cls0 ! assume 80x25 screen
      integer*4 iRow
!
      do iRow=0,24
        call cls1(iRow)
      end do
      end
c-----
c-----
      subroutine InsertCmdLineDefaults(CmdLineArg,ControlParam,
     +  ControlParamDft,PromptStr,nCLArgs,MaxnCLArgs,MaxPromptsUsable)
      character*255 Response,IOBuffer
      character*(*) CmdLineArg(*),ControlParam(*),
     +  ControlParamDft(*),PromptStr(*)
      integer*4 nCLArgs
      integer*2 MaxnCLArgs,MaxPromptsUsable,i
!
c     write(*,'(//2x,a/)') 'To accept the default, strike <Enter>.'
c     call WriteStrAt(0,0,'To accept the default, strike <Enter>.')
c     write(*,'(1x)',advance='no') ! expect advance=no to follow
      do i=1,MaxnCLArgs
        if(i<=nCLArgs) then ! use CmdLineArg to over-ride the default
          ControlParam(i)=CmdLineArg(i)
          call BlankControlChars(ControlParam(i),255)
          call MakeUpper(ControlParam(i),255)
          cycle
        end if
        ControlParam(i)=ControlParamDft(i)
        if(i>MaxPromptsUsable) cycle ! accept the unused pro-forma defaults
        Response=' ' ! needed in F77, where character length cannot be zero
c       write(*,'(2i4)') LengNB(Response),iChar(Response(1:1))
        write(IOBuffer,'(4a)') trim(PromptStr(i)),
     +    ' (default is ',trim(ControlParamDft(i)),'):  '
c       write(*,'( 1x,4a)',advance='no') IOBuffer
        call WriteStrAt(int4(i),5,IOBuffer)
        read(*,'(a255)',err=100,end=100) Response ! eor=100 rejected by F95
        if(Response(1:1)/=' ') then
          ControlParam(i)=Response
          call BlankControlChars(ControlParam(i),255)
          call MakeUpper(ControlParam(i),255)
        end if
  100   continue
c       write(*,'( 1x,6a/)',advance='no') '[',trim(Response),
c    +    '] implies ',PromptStr(i),' result is ',ControlParam(i)
      end do
      end ! subroutine InsertCmdLineDefaults
c-----
      subroutine TerminatePath(PathStr)
      character*255 PathStr
      integer*2 j,LengNB
!
      j=LengNB(PathStr) ! cover the case of missing path terminator
      if((0<j).and.(j<255).and.
     +   (PathStr(j:j)/=':').and.
     +   (PathStr(j:j)/='\')) then
        if(PathStr(j:j)=='"') then
        ! cover the C-based error interpreting the closing '\"' of PathStr
          PathStr(j:j)='\'
        else
          PathStr=trim(PathStr)//'\'
        end if
      end if
      end
c-----
      integer*2 function CI_index(String,Target) ! case-insensitive index()
      character*(*) String,Target
      character*4095 UcStr,UcTgt
      integer*4 LenStr
      integer*2 LengNB
!
      LenStr=LengNB(String)
      if(LenStr>4095) call ps(1,'CI_index excessive length of arg 1')
      UcStr=String
c     UcTgt=Target
      call MakeUpper(UcStr,LenStr)
c     call MakeUpper(UcTgt,LenStr) ! beyond which length we don't care
c     CI_index=index(UcStr,UcTgt)
      CI_index=index(UcStr,Target) ! expect Target to already be in UpperCase
      end
c-----
      subroutine MakeUpper(s,n)
      integer*4 n,i
      character*(*) s ! len(s) is undependable
      character*1 c
!
      do i=1,n
        c=s(i:i) ! convert ith char to upper-case
        if(('a'<=c).and.(c<='z')) s(i:i)=char(ichar(c)-32)
      end do
      end
c-----
      subroutine BlankControlChars(s,n)
      integer*4 n,i
      character*(*) s ! len(s) is undependable
!
      do i=1,n
      ! convert ith char to blank if its ASCII is below 32
        if(iChar(s(i:i))<32) s(i:i)=' '
      end do
      end
c-----
      subroutine ExpleteWithCommas(s)
      character*(*) s
      integer*2 sLen,jInf,jCh,LengNB
!
      sLen=len(s)
!     this executes pitifully slowly, was abandoned 20020114
c     do
c       if(s(sLen:sLen)==',') exit
c       s=trim(s)//','
c     end do
!     this overwrote trailing digits with commas
c     do jCh=sLen,1,-1
c       if(s(jCh:jCh)==',') exit
c          s(jCh:jCh)=','
c     end do
      jInf=LengNB(s)+1
      do jCh=sLen,jInf,-1
        s(jCh:jCh)=','
      end do
      end
c-----
      integer*2 function nCommaDelims(s)
      character*(*) s
      character*1 c
      integer *2 j,sLeng,nCommas,nQuotes,LengNB
!
      nCommas=0
      nQuotes=0
      sLeng=LengNB(s)
      do j=sLeng,1,-1
        c=s(j:j)
        if(c=='"') then ! count only DoubleQuotes, not Apostrophes
          nQuotes=nQuotes+1
        else if(c==',') then
          if(j==sLeng) cycle ! discount the terminating comma as a delimiter
          if(mod(nQuotes,2)==0) nCommas=nCommas+1 ! discount those within strings
        end if
      end do
      nCommaDelims=nCommas
      end
c-----
      subroutine ExpleteWithCommasUpTo(s,nFields) ! limited to nFields
      character*(*) s
      integer*2 sLen,nFields,nCommas,jInf,jCh,LengNB
!
      sLen=len(s)
      nCommas=0
      do jCh=1,sLen ! count existing commas
        if(s(jCh:jCh)==',') nCommas=nCommas+1
      end do
      jInf=LengNB(s)+1
      do jCh=jInf,sLen
        if(nCommas>=nFields) exit
        nCommas=nCommas+1
        s(jCh:jCh)=','
      end do
      end
c-----
      subroutine EnumerateFields(s,jSupFoI,jOfs,OutUnit) ! useful on lines of DSF files
      character*(*) s
      character*255 Field(385) ! one more than necessary on 20060908
      character*1 DummyCh
      integer*4 i,j,jSupFoI,jOfs,OutUnit,jLimit,nTrunc
!
    ! jSupFoI is the nominal upper-limit on field-indices of interest
    ! jOfs is the number of leading fields to skip in the count
      jLimit=385 ! to force reading beyond the last field of possible interest
      do j=1,jLimit
        Field(j)='<>'
      end do
!
    ! since read(s,*) evidently counts fields before entering the implicit-do
    ! loop, we determine the feasible limit on readable fields by experimentation
      do nTrunc=0,385
        j=0
        read(s,*,err=101,end=101) (DummyCh,i=1,jOfs),
     +    (Field(j),j=1,385-nTrunc)
  101   write(OutUnit,'(3i5,a,i3.3)')jOfs,jSupFoI,j,
     +    ' Ofs,Sup,Max in Enum with #Truncated=',nTrunc
        if(j>0) exit ! with the lowest feasible nTrunc (trailing fields missing)
        if(jLimit<=0) exit
        jLimit=jLimit-1 ! j==0 above => s had too few fields, => error or EoF
      end do
!
      j=0
      read(s,*,err=102,end=102) (DummyCh,i=1,jOfs),(Field(j),j=1,jLimit)
  102 continue ! with (j) expected to be neither in error nor beyond end-of-string
c     jLimit=j-1 ! if the above loop exited with error or EoF
      write(OutUnit,'(3i5,a)')jOfs,jSupFoI,jLimit,' Ofs,Sup,Lim in Enum'
      if(jLimit>jSupFoI) jLimit=jSupFoI ! possibly < 385
      write(OutUnit,'(i4,1x,a)') (j,Field(j),j=1,jLimit)
      end
c-----
      subroutine ConcatCSV(sAccum,sAddend)
      character*(*) sAccum,sAddend
    ! appends sAddend [if not void] and a comma to sAccum
!
      if(iChar(sAddend(1:1))<=32) then ! sAddend is void
        if(iChar(sAccum(1:1))<=32) then ! sAccum void, preclude initial blank;
        ! F77 strings' length cannot be less than 1, even the result of trim
          sAccum=','
        else
          sAccum=trim(sAccum)//','
        end if
      else ! sAddend is not void
        if(iChar(sAccum(1:1))<=32) then ! sAccum void, preclude initial blank;
        ! F77 strings' length cannot be less than 1, even the result of trim
          sAccum=trim(sAddend)//','
        else
          sAccum=trim(sAccum)//trim(sAddend)//','
        end if
      end if
      end
c-----
      subroutine IndexedAlphaSort(nItems,iOrg,StrKey,Feedback)
!     sorts nItems strings in StrKey array in alphabetic order(iOrg array)
      integer*2 nItems,i,j,k,Gap,iHold,iOrg(*)
      character*(*) StrKey(*)
      logical*1 Feedback
!
      do i=1,nItems
        iOrg(i)=i
      end do
      Gap=nItems/2
      do while(Gap>0)
        do i=Gap+1,nItems
          j=i-Gap
          do while(j>0)
            k=j+Gap
            if(LLE(StrKey(iOrg(j)),StrKey(iOrg(k)))) then
              j=0 ! break the while loop (assign j=-1 for 0-based arrays)
            else ! interchange the pair of iOrgs
              iHold=iOrg(j)
              iOrg(j)=iOrg(k)
              iOrg(k)=iHold
            end if
            j=j-Gap
          end do
        end do
        Gap=Gap/2
      end do
      if(.not.Feedback) return
      do k=1,nItems
        write(4,'(2i6,2a)') k,iOrg(k),' aft IAS ',
     +    trim(StrKey(iOrg(k)))
      end do
      end ! subroutine IndexedAlphaSort
c-----
      subroutine IndexedAlphaSortZb(iSupIt,iOrg,StrKey) ! as above but for 0-based arrays
!     sorts 1+iSupIt strings in StrKey array in alphabetic order(iOrg array)
      integer*2 iSupIt,i,j,k,Gap,iHold,iOrg(0:*)
      character*(*) StrKey(0:*)
!
      do i=0,iSupIt
        iOrg(i)=i
      end do
      Gap=(1+iSupIt)/2
      do while(Gap>0)
        do i=Gap,iSupIt
          j=i-Gap
          do while(j>=0)
            k=j+Gap
            if(LLE(StrKey(iOrg(j)),StrKey(iOrg(k)))) then
              j=-1 ! break the while loop (assign j=-1 for 0-based arrays)
            else ! interchange the pair of iOrgs
              iHold=iOrg(j)
              iOrg(j)=iOrg(k)
              iOrg(k)=iHold
            end if
            j=j-Gap
          end do
        end do
        Gap=Gap/2
      end do
      do k=0,iSupIt
        write(4,'(2i6,2a)') k,iOrg(k),' aft IASzb ',
     +    trim(StrKey(iOrg(k)))
c       if(k>0) write(4,'(1x,L1)') LLE(StrKey(iHold),StrKey(iOrg(k)))
c       iHold=iOrg(k) ! used to verify the sorting logic above
      end do
      end ! subroutine IndexedAlphaSortZb
c-----
      integer*2 function LengNB(s)
    ! returns the length of the non-blank part of s,
    ! but note that leading blanks are not discounted
      integer*2 i
      character*(*) s
      character*1 c
!
      do i=len(s),1,-1 ! get the index of the last printable non-blank char
c       if(iChar(s(i:i))>32) exit ! len(s)=16384 caused 'program stack exhausted'
        c=s(i:i) ! breaking the above into two suffices to solve the stack exhaustion
        if(iChar(c)>32) exit
      end do
      LengNB=i ! possibly 0
      end
c-----
      subroutine EditField(CurrField,nPrevCommas,nMaxCommas,s)
      character*(*) s,CurrField
      integer*2 nPrevCommas,nMaxCommas,nCommas,jNextC,jCh,jMax,LengNB
!
    ! get the character-offset position of the target field
      nCommas=0 ! assume none is embedded inside a string
      jMax=len(s)
      do jCh=1,jMax
        if(s(jCh:jCh)==',') nCommas=nCommas+1
        if(nCommas==nPrevCommas) exit
      end do
      if(jCh>jMax) call ps(1,'Too few CSV fields in input record.')
      if(jCh<jMax) then
        jNextC=index(s(jCh+1:jMax),',')
        if(jNextC==0) then ! no commas follow in the source
          s=s(1:jCh)//trim(CurrField)//','
        else
          s=s(1:jCh)//trim(CurrField)//s(jCh+jNextC:jMax)
        ! overflow here of length of s seems not to be a problem
        end if
      end if
      do ! get character-offset at nMaxCommas
        if((jCh>=jMax).or.(nCommas>=nMaxCommas)) exit
        jCh=jCh+1
        if(s(jCh:jCh)==',') nCommas=nCommas+1
      end do
      do ! blank the string beyond nMaxCommas so trim can truncate
        if(jCh>=jMax) exit
        jCh=jCh+1
        s(jCh:jCh)=' '
      end do
      return
!
      entry TruncateAfterCommas(nMaxCommas,s)
    ! blank excess commas inserted by ExpleteWithCommas
      nCommas=0 ! assume none is embedded inside a string
      jMax=LengNB(s)
      do jCh=1,jMax
        if(s(jCh:jCh)==',') nCommas=nCommas+1
        if(nCommas>nMaxCommas) s(jCh:jCh)=' '
      end do
!
      end ! subroutine EditField
c-----
      subroutine RenameLong(fn1,fn2)
    ! circumvents problem that rename has with long filenames & embedded blanks
      character*255 fn1,fn2
      integer*4 iou1,iou2,ErrOrEnd
      integer*1 OneByte
!
      iou1=32768
      iou2=32769
      open(iou1,file=fn1,status='old',access='transparent')
      open(iou2,file=fn2,status='new',access='transparent')
      do
        read(iou1,ioStat=ErrOrEnd) OneByte
        if(ErrOrEnd/=0) exit
        write(iou2) OneByte
      end do
      close(iou2,status='keep')
      close(iou1,status='delete')
      end
c-----
      subroutine VerifyPath(iStat,DirPrefix) ! adapted 20031011 from OpenFmFile
      integer*2 LengPath,LengNB
      integer*4 iStat,OpenUnit
      logical*4 UnitInUse ! ,PathExtant
      logical*1 StopIfAbsent,MustUsePrefix/.true./
      character*255 DirPrefix,FilePath
      character*255 FileName
!
      StopIfAbsent=(iStat==0) ! else caller will inspect iStat
      iStat=0 ! default valid if path is extant
      LengPath=LengNB(DirPrefix)
      if((LengPath==0).or.(DirPrefix(1:1)==' ')) return ! current path is always available
      if((LengPath==3).and.(DirPrefix(2:3)==':\')) return ! root directory is always available
      FilePath=DirPrefix(1:LengPath-1) ! truncate the trailing backslash
    ! inquire evidently never succeeds in considering sub-dir as a 'file'
c     inquire(file=FilePath,exist=PathExtant,err=200,ioStat=iStat)
c     if((iStat==0).and.(.not.PathExtant)) iStat=-1
    ! begin replacement for inquire
      do OpenUnit=32700,32760 ! max allowed under F77 is 32767
        inquire(unit=OpenUnit,opened=UnitInUse,err=100,ioStat=iStat)
        if(.not.UnitInUse) exit
  100   continue
      end do
      if(OpenUnit>32760)
     +  call ps(1,'Error finding OpenUnit in VerifyPath')
      FileName='PathTest.txt' ! cannot pass literal constant below
      call OpenFmFile(OpenUnit,2,1,iStat, ! test-open a file in DirPrefix
     +  MustUsePrefix,DirPrefix,FileName)
      close(OpenUnit,status='delete')
    ! end replacement for inquire
      if(iStat==0) return ! file was opened okay => directory existed
  200 if(.not.StopIfAbsent) return ! with iStat/=0 implying 'path not found'
      call ps(1,'Path-not-found Error: '//FilePath)
      end ! subroutine VerifyPath
c-----
      subroutine OpenFmFile(iUnit,iFS,iCC,iStat,
     +  MustUsePrefix,DirPrefix,FileParm)
      integer*4 iUnit,iFS,iCC,iStat
      integer*2 iCh,j,LengNB
      logical*1 MustUsePrefix,StopIfAbsent
      character*255 DirPrefix,FilePath
      character*255 FileParm,FileName
      character*7
     +  FSType(0:2)/'Old','New','Replace'/, ! Replace is same as Unknown
     +  CCType(2)/'List','Fortran'/
!
      StopIfAbsent=(iStat==0) ! else caller will inspect iStat
      iStat=0 ! default valid if file is extant
      FileName=FileParm
      if(LengNB(FileName)<=0) then
        iStat=-1
        goto 200
      end if
      if(MustUsePrefix) goto 100
    ! look first in current dir
c     write(*,'(i3,2(1x,a))') LengNB(FileName),trim(FileName),FileName
      do j=255,1,-1 ! blank any control characters, since stack-var's tail ...
        iCh=iChar(FileName(j:j))
        if((32<iCh).and.(iCh<123)) exit ! accept ASCII chars on [33,122]
        FileName(j:j)=' ' ! ... from concatenation may contain garbage bytes
      end do
c     write(*,'(i5,3(i3,1x,a),a)') iUnit,LengNB(FileName),FileName,
c    +  iFS,FSType(iFS),iCC,CCType(iCC),' before open()'
      open(iUnit,FileName,form='formatted',status=FSType(iFS),
     +  CarriageControl=CCType(iCC),err=100)
      return
    ! look next in directory specified
  100 if(LengNB(DirPrefix)>0) then
        FilePath=trim(DirPrefix)//FileName
      else ! path prefix is blank
        FilePath=FileName
      end if
      open(iUnit,FilePath,form='formatted',status=FSType(iFS),
     +  CarriageControl=CCType(iCC),err=200,ioStat=iStat)
      return
  200 if(.not.StopIfAbsent) return ! with iStat/=0 implying 'file not found'
      call ps(1,'Error opening formatted file: '//FilePath)
      end ! subroutine OpenFmFile
c-----
      subroutine OpenSqFile(iUnit,iFS,iCC,iAM,iStat,
     +  MustUsePrefix,DirPrefix,FileParm) ! same as OpenFmFile with iAM added
      integer*4 iUnit,iFS,iCC,iAM,iStat
      integer*2 iCh,j,LengNB
      logical*1 MustUsePrefix,StopIfAbsent
      character*255 DirPrefix,FilePath
      character*255 FileParm,FileName
      character*7
     +  FSType(0:2)/'Old','New','Replace'/, ! Replace is same as Unknown
     +  CCType(2)/'List','Fortran'/
      character*10
     +  AMType(2)/'Append','Sequential'/
!
      StopIfAbsent=(iStat==0) ! else caller will inspect iStat
      iStat=0 ! default valid if file is extant
      FileName=FileParm
      if(LengNB(FileName)<=0) then
        iStat=-1
        goto 200
      end if
      if(MustUsePrefix) goto 100
    ! look first in current dir
c     write(*,'(i3,2(1x,a))') LengNB(FileName),trim(FileName),FileName
      do j=255,1,-1 ! blank any control characters, since stack-var's tail ...
        iCh=iChar(FileName(j:j))
        if((32<iCh).and.(iCh<123)) exit ! accept ASCII chars on [33,122]
        FileName(j:j)=' ' ! ... from concatenation may contain garbage bytes
      end do
c     write(*,'(i5,3(i3,1x,a),a)') iUnit,LengNB(FileName),FileName,
c    +  iFS,FSType(iFS),iCC,CCType(iCC),' before open()'
      open(iUnit,FileName,form='formatted',status=FSType(iFS),
     +  CarriageControl=CCType(iCC),access=AMType(iAM),err=100)
      return
    ! look next in directory specified
  100 if(LengNB(DirPrefix)>0) then
        FilePath=trim(DirPrefix)//FileName
      else ! path prefix is blank
        FilePath=FileName
      end if
      open(iUnit,FilePath,form='formatted',status=FSType(iFS),
     +  CarriageControl=CCType(iCC),access=AMType(iAM),err=200,
     +  ioStat=iStat)
      return
  200 if(.not.StopIfAbsent) return ! with iStat/=0 implying 'file not found'
      call ps(1,'Error opening sequential file: '//FilePath)
      end ! subroutine OpenSqFile
c-----
!     subroutine ReadVecIfNeg(rTest,vDest,UseVector,iRow,nCol,LimnCol,
!    +  iUnit)
!     logical*1 UseVector(*)
!     integer*2 iRow,nCol,jCol,LimnCol,iUnit,VecOfInt,iVec
!     real*4 rTest,vDest(LimnCol,*)
!     character*1 FirstChar,Descriptor
!     character*1024 IOBuffer
!
!     UseVector(iRow)=.false. ! default, implying 'use value in rTest'
!     if(rTest>=0.0) return
!     if(iUnit<0) call ps(1,'required RSV file is absent.')
!     UseVector(iRow)=.true.
!     rewind(iUnit)
!     VecOfInt=nint(-rTest)
!     do
!       read(iUnit,'(a)',err=80,end=80) IOBuffer
!       call ExpleteWithCommas(IOBuffer)
!       read(IOBuffer,*,err=80,end=80) FirstChar,Descriptor,iVec
!       if(iVec==VecOfInt) then
!         read(IOBuffer,*,err=80,end=80) FirstChar,Descriptor,iVec,
!    +      (vDest(jCol,iRow),jCol=1,nCol)
!         return
!       end if
!     end do
!     call ps(1,'Target vector not found in RSV file.')
!  80 call ps(1,'Error reading RSV file.')
!     end ! subroutine ReadVecIfNeg
c-----
      subroutine ReadVecIfNeg(rTest,vDest,UseVector,
     +  nCol,svUnit,iTargMo)
      logical*1 UseVector
      integer*2 nCol,jCol,VecOfInt,iVec,iTargMo,iMoBeg,iMoEnd
      integer*4 svUnit
      real*4 rTest,vDest(*)
      character*80 Descriptor
      character*1 FirstChar
      character*1024 IOBuffer
!
      UseVector=.false. ! default, implying 'use value in rTest'
      if(rTest>=0.0) return
      UseVector=.true.
      if(svUnit<=0) call ps(1,'Specified vector file not found.')
      rewind(svUnit)
      VecOfInt=nint(-rTest)
      iVec=0 ! preclude retention of prior-read value
      do
        read(svUnit,'(a)',err=80,end=80) IOBuffer
        call ExpleteWithCommas(IOBuffer)
        read(IOBuffer,*,err=80,end=80) FirstChar
        if(FirstChar>='7') cycle ! skip the header & tab-lines
        read(IOBuffer,*,err=80,end=80) FirstChar,Descriptor,iVec,
     +    iMoBeg,iMoEnd
        if((iVec==VecOfInt).and.((iTargMo==0).or. ! iTargMo==0 => no restriction
     +    ((iMoBeg<=iTargMo).and.(iTargMo<=iMoEnd)))) then ! iTargMo is included
        ! obsolete line below presumed that all nCol values were contiguous
c         read(IOBuffer,*,err=80,end=80) FirstChar,Descriptor,iVec,
c    +      iMoBeg,iMoEnd,(vDest(jCol),jCol=1,nCol)
        ! after 20051214, only first 30 columns are contiguous;
        ! skip over Description and Comment fields following first 30 values
          read(IOBuffer,*,err=80,end=80) FirstChar,Descriptor,iVec,
     +      iMoBeg,iMoEnd,(vDest(jCol),jCol=1,min0(nCol,30)),
     +      FirstChar,FirstChar,
     +                    (vDest(jCol),jCol=31,nCol)
          return
        end if
      end do
      write(4,'(i3,i4,1x,a)') iTargMo,VecOfInt,trim(IOBuffer)
      call ps(1,'Target vector not found in RSV file.')
   80 write(4,'(i3,i4,1x,a)') iTargMo,nCol,trim(IOBuffer)
      call ps(1,'Error reading RSV file.')
      end ! subroutine ReadVecIfNeg
c-----
      real*4 function VectorSum(n,x)
      integer*4 i,n
      real*4 sum,x(*)
!
      sum=0.0
      do i=1,n
        sum=sum+x(i)
      end do
      VectorSum=sum
      end
c-----
      subroutine WriteStrAt(Row,Col,Message)
      integer*4 Row,Col,iStat
      integer*2 jF,LengNB
      logical*4 UnitInUse
      character*(*) Message
!
      write(*,'(1x,a)',advance='yes') trim(Message)
      inquire(unit=4,opened=UnitInUse,err=100,ioStat=iStat)
  100 if((iStat==0).and.UnitInUse) ! file is open
     +  write(4,'(1x,a)') trim(Message)
      end
c-----
      subroutine ShowProgress(iRow,nRecReported,suffix)
      integer*4 iRow,nRecReported
      character*255 IOBuffer,suffix
!
      write(IOBuffer,'(i8,2a)') nRecReported,' numeric records ',
     +  trim(suffix)
      call WriteStrAt(iRow,5,IOBuffer)
      end
c-----
      subroutine IssueWarningAt(Row,Col,Message)
      logical*1 InBatchMode
      integer*4 Row,Col
      character*(*) Message
!
      write(4,'(/1x,a)') trim(Message)
      call WriteStrAt(Row,Col,Message)
      call GetBatchMode(InBatchMode)
      if(.not.InBatchMode) then
        ! TODO: Get rid of this pause.
      call end_program("file_ios:0001 - Found PAUSE " //
     + "entry in IssueWarningAt.")
 
		
	  endif
	  
      end subroutine IssueWarningAt
c-----
      subroutine AppendStr(u,s) ! enclose s in double-quotes, add a trailing comma
      integer*4 u
      character*(*) s
!
      write(u,'( 4a)',advance='no') '"',trim(s),'"',","
      end
c-----
      subroutine AppendFixed7(u,r) ! most useful for multipliers on [0,9.999994)
      integer*4 u
      real*4 r ! ,rOut
!
c     if((0.0>r).or.(r>9.999995)) then
c       call AppendFloat7(u,r)
c     else ! use fixed-point format
c       rOut=r
c       if(r<=0.000005) rOut=0.00001 ! round upward per TS request 20030602
c       write(u,'( f7.5,",")' ,advance='no') rOut
c     end if
      if((0.000005<=r).and.(r<9.999995)) then ! use fixed-point format
        write(u,'( f7.5,",")' ,advance='no') r
      elseif(( 0.0<=r).and.(r<0.000005)) then ! round upward per TS request 20030602
        write(u,'( "0.00001,")' ,advance='no')
      else ! use various formats
        call AppendFloat7(u,r)
      end if
      end ! subroutine AppendFixed7
c-----
      character*8 function Fixed8StrEqv(r) ! adapted from AppendFixed8 below
      integer*4 iMant,iExpo
      character*12 Str12
      character*9 InclZero
      character*8 StrEquiv
      real*4 r,t
!
      if((0.0000005<=r).and.(r<0.9999995)) then ! use fixed-point format ...
        write(InclZero,'(f9.7)') r ! ... here including the leading zero ...
c       StrEquiv=InclZero(2:9) ! ... but here excluding the leading zero ...
        write(StrEquiv,'(a8)') InclZero(2:9) ! ... to get 7 sig digits
      elseif(( 0.0<=r).and.(r<0.0000005)) then ! round upward per TS request 20030602
        write(StrEquiv,'("0.000001")')
      elseif(r<9.999995) then ! use fixed-point format
        write(StrEquiv,'(f8.6)') r
      elseif(r<99.99995) then ! use fixed-point format
        write(StrEquiv,'(f8.5)') r
      elseif(r<999.9995) then ! use fixed-point format
        write(StrEquiv,'(f8.4)') r
      elseif(r<9999.995) then ! use fixed-point format
        write(StrEquiv,'(f8.3)') r
      elseif(r<99999.95) then ! use fixed-point format
        write(StrEquiv,'(f8.2)') r
      elseif(r<999999.5) then ! use fixed-point format
        write(StrEquiv,'(f8.1)') r
      elseif(r<999999e2) then ! use integer     format
        write(StrEquiv,'(i8.8)') nint(r)
      else ! use floating-point formats
        t=abs(r)
        write(Str12,'(e12.6)') t ! e12.6 allows for 0.nnnnnnEsnn
        read(Str12,'(2x,i6,1x,i3)') iMant,iExpo
        iExpo=iExpo-6 ! account for expressing the mantissa as an integer
        if(r<0.0) then ! shrink magnitude of iMant to fit the sign in 6 chars
          iMant=-nint(0.1d0*dble(iMant))
          iExpo=iExpo+1 ! account for shortening the integer above
        end if
        if    (iExpo>=10) then
          iMant=nint(0.1d0*dble(iMant))
          write(StrEquiv,'(i5,"E",i2.2)') iMant,iExpo+1
        elseif(iExpo>= 0) then
          write(StrEquiv,'(i6,"E",i1  )') iMant,iExpo
        elseif(iExpo>=-9) then ! shrink magnitude of iMant to fit in 4 chars
          iMant=nint(0.1d0*dble(iMant))
          write(StrEquiv,'(i5,"E",i2.1)') iMant,iExpo+1
        else ! iExpo<=-10      ! shrink magnitude of iMant to fit in 4 chars
          iMant=nint(0.01d0*dble(iMant))
          write(StrEquiv,'(i4,"E",i3.2)') iMant,iExpo+2
        ! worst case of precision above allows only -nnnE-nn for very
        ! small negative r, which are not expected
        end if
      end if
      Fixed8StrEqv=StrEquiv
      end ! function Fixed8StrEqv
c-----
      subroutine AppendFixed8(u,r) ! most useful for multipliers on [0,9.999994)
      integer*4 u,iMant,iExpo
      character*12 Str12
      character*9 InclZero
c     character*8 StrEquiv
      real*4 r,t
!
      if((0.0000005<=r).and.(r<0.9999995)) then ! use fixed-point format ...
        write(InclZero,'(f9.7)') r ! ... here including the leading zero ...
c       StrEquiv=InclZero(2:9) ! ... but here excluding the leading zero ...
        write(u,'( a8,",")' ,advance='no') InclZero(2:9) ! ... to get 7 sig digits
      elseif(( 0.0<=r).and.(r<0.0000005)) then ! round upward per TS request 20030602
        write(u,'( "0.000001,")' ,advance='no')
      elseif(r<9.999995) then ! use fixed-point format
        write(u,'( f8.6,",")' ,advance='no') r
      elseif(r<99.99995) then ! use fixed-point format
        write(u,'( f8.5,",")' ,advance='no') r
      elseif(r<999.9995) then ! use fixed-point format
        write(u,'( f8.4,",")' ,advance='no') r
      elseif(r<9999.995) then ! use fixed-point format
        write(u,'( f8.3,",")' ,advance='no') r
      elseif(r<99999.95) then ! use fixed-point format
        write(u,'( f8.2,",")' ,advance='no') r
      elseif(r<999999.5) then ! use fixed-point format
        write(u,'( f8.1,",")' ,advance='no') r
      elseif(r<999999e2) then ! use integer     format
        write(u,'( i8.8,",")' ,advance='no') nint(r)
      else ! use floating-point formats
        t=abs(r)
        write(Str12,'(e12.6)') t ! e12.6 allows for 0.nnnnnnEsnn
        read(Str12,'(2x,i6,1x,i3)') iMant,iExpo
        iExpo=iExpo-6 ! account for expressing the mantissa as an integer
        if(r<0.0) then ! shrink magnitude of iMant to fit the sign in 6 chars
          iMant=-nint(0.1d0*dble(iMant))
          iExpo=iExpo+1 ! account for shortening the integer above
        end if
        if    (iExpo>=10) then
          iMant=nint(0.1d0*dble(iMant))
          write(u,'( i5,"E",i2.2,",")' ,advance='no') iMant,iExpo+1
        elseif(iExpo>= 0) then
          write(u,'( i6,"E",i1  ,",")' ,advance='no') iMant,iExpo
        elseif(iExpo>=-9) then ! shrink magnitude of iMant to fit in 4 chars
          iMant=nint(0.1d0*dble(iMant))
          write(u,'( i5,"E",i2.1,",")' ,advance='no') iMant,iExpo+1
        else ! iExpo<=-10      ! shrink magnitude of iMant to fit in 4 chars
          iMant=nint(0.01d0*dble(iMant))
          write(u,'( i4,"E",i3.2,",")' ,advance='no') iMant,iExpo+2
        ! worst case of precision above allows only -nnnE-nn for very
        ! small negative r, which are not expected
        end if
      end if
      end ! subroutine AppendFixed8
c-----
      subroutine AppendInt4(u,r)
      integer*4 u,t
      real*4 r ! ,t
!
c     t=r
c     if(r<0) t=-10.0*t ! allow one more place for the minus-sign
c     if(    t<9.5) then
c       write(u,1 ,advance='no') nint(r)
c     elseif(t<99.5) then
c       write(u,2 ,advance='no') nint(r)
c     elseif(t<999.5) then
c       write(u,3 ,advance='no') nint(r)
c     else ! t<9999.5 by assumption [else these format codes may be extended]
c       write(u,4 ,advance='no') nint(r)
c     end if
      t=nint(r)
      if(t<0) t=-10*t ! allow one more place for the minus-sign
      if(    t<10) then
        write(u,1 ,advance='no') nint(r)
      elseif(t<100) then
        write(u,2 ,advance='no') nint(r)
      elseif(t<1000) then
        write(u,3 ,advance='no') nint(r)
      else ! t<10000 by assumption [else these format codes may be extended]
        write(u,4 ,advance='no') nint(r)
      end if
      return
    ! ampersand usage under F77L3 requires CarriageControl=fortran
    1 format( i1,',')
    2 format( i2,',')
    3 format( i3,',')
    4 format( i4,',')
      end ! subroutine AppendInt4
c-----
      subroutine AppendInt5(u,r)
      integer*4 u,t
      real*4 r ! ,t
!
c     t=r
c     if(r<0) t=-10.0*t ! allow one more place for the minus-sign
c     if(    t<9.5) then
c       write(u,1 ,advance='no') nint(r)
c     elseif(t<99.5) then
c       write(u,2 ,advance='no') nint(r)
c     elseif(t<999.5) then
c       write(u,3 ,advance='no') nint(r)
c     elseif(t<9999.5) then
c       write(u,4 ,advance='no') nint(r)
c     else ! t<99999.5 by assumption [else these format codes may be extended]
c       write(u,5 ,advance='no') nint(r)
c     end if
      t=nint(r)
      if(t<0) t=-10*t ! allow one more place for the minus-sign
      if(    t<10) then
        write(u,1 ,advance='no') nint(r)
      elseif(t<100) then
        write(u,2 ,advance='no') nint(r)
      elseif(t<1000) then
        write(u,3 ,advance='no') nint(r)
      elseif(t<10000) then
        write(u,4 ,advance='no') nint(r)
      else ! t<100000 by assumption [else these format codes may be extended]
        write(u,5 ,advance='no') nint(r)
      end if
      return
    ! ampersand usage under F77L3 requires CarriageControl=fortran
    1 format( i1,',')
    2 format( i2,',')
    3 format( i3,',')
    4 format( i4,',')
    5 format( i5,',')
      end ! subroutine AppendInt5
c-----
      subroutine AppendFloat7(u,rOrg)
      integer*4 u
      integer*2 iExpo
      real*4 rOrg,r,t
!
      t=rOrg
      if(t<0.0) t=-10.0*t ! allow one more place for the minus-sign
c     if((0.0999995<=t).and.(t<9.99995)) then ! most likely branch
c       write(u, 5 ,advance='no') rOrg
      if(t<1.0e-36) then ! as if t were zero
c       if(rOrg<0.0) then
c         write(u, 4 ,advance='no') rOrg ! but no leading minus was evident in testing
c       else
          write(u, 5 ,advance='no') rOrg
c       end if
      elseif(t<0.0999995) then
        r=rOrg ! preclude modifying rOrg
        do iExpo=1,98 ! 4 is minimum needed to express nint(t) as iiiiE-n
          t=10.0*t
          r=10.0*r
          if((nint(t)>=1000).or.((iExpo>8).and.(nint(t)>=100))) exit
        end do
        if(iExpo<=9) then
          write(u,12 ,advance='no') nint(r),-iExpo
        else
          write(u,13 ,advance='no') nint(r),-iExpo
        end if
      elseif(t<9.99995) then
        write(u, 5 ,advance='no') rOrg
      elseif(t<99.9995) then
        write(u, 4 ,advance='no') rOrg
      elseif(t<999.995) then
        write(u, 3 ,advance='no') rOrg
      elseif(t<9999.95) then
        write(u, 2 ,advance='no') rOrg
      elseif(t<99999.5) then
        write(u, 1 ,advance='no') rOrg
      elseif(t<=9999999.4) then
        write(u,10 ,advance='no') nint(rOrg)
      else
!       sacrifice significant figures in output to gain fixed field-width
        r=rOrg ! preclude modifying rOrg
        do iExpo=1,98 ! 2 is minimum needed to express nint(t) as iiiiiEn
          t=0.1*t
          r=0.1*r
        ! preclude NDP error taking nint of large #s
          if(t<2.147484e9) then ! nint() can handle up to 2**31
            if(((nint(t)<=99999).and.(iExpo<=9)).or.(nint(t)<=9999))exit
          end if
        end do
        if(iExpo<=9) then
          write(u,11 ,advance='no') nint(r),iExpo
        else
          write(u,12 ,advance='no') nint(r),iExpo
        end if
      end if
      return
    ! ampersand usage under F77L3 requires CarriageControl=fortran
    1 format( f7.1,',')
    2 format( f7.2,',')
    3 format( f7.3,',')
    4 format( f7.4,',')
    5 format( f7.5,',')
   10 format( i7,  ',')
   11 format( i5,'E',i1,',')
   12 format( i4,'E',i2,',')
   13 format( i3,'E',i3,',')
      end ! subroutine AppendFloat7
c-----
      subroutine AppendFloat9(u,rOrg) ! for use in RPTtoCSV per TS 20060227
      real*4 MaxMagR4,MinMagR4
      real*8 MaxMagR8,MinMagR8
      parameter(MaxMagR4=1.0e36,MinMagR4=1.0e-36,
     +          MaxMagR8=1.0d36,MinMagR8=1.0d-36) ! 0.38e38 was seen in test loop
!     end of parameters
      integer*4 u
      integer*2 iExpo
      integer*4 iMant
      real*4 rOrg
      real*8 r,t
!
      t=dble(rOrg)
      if(t<0.0d0) t=-10.0d0*t ! allow one more place for the minus-sign
      if(t<MinMagR8) then ! as if t were zero
c       if(rOrg<0.0d0) then
c         write(u, 6 ,advance='no') rOrg ! but no leading minus was evident in testing
c       else
          write(u, 7 ,advance='no') rOrg
c       end if
      elseif(t<0.0999995d0) then
        r=dble(rOrg) ! preclude modifying rOrg
        do iExpo=1,98 ! 4 is minimum needed to express nint(t) as iiiiE-n
          t=10.0d0*t
          r=10.0d0*r
          iMant=idnint(t)
          if((iMant>=10000).or.((iExpo>8).and.(iMant>=1000))) exit
        end do
        iMant=idnint(r)
        if(iExpo<=9) then
          write(u,12 ,advance='no') iMant,-iExpo
        else
          write(u,13 ,advance='no') iMant,-iExpo
        end if
      elseif(t<9.9999995d0) then
        write(u, 7 ,advance='no') rOrg
      elseif(t<99.999995d0) then
        write(u, 6 ,advance='no') rOrg
      elseif(t<999.99995d0) then
        write(u, 5 ,advance='no') rOrg
      elseif(t<9999.9995d0) then
        write(u, 4 ,advance='no') rOrg
      elseif(t<99999.995d0) then
        write(u, 3 ,advance='no') rOrg
      elseif(t<999999.95d0) then
        write(u, 2 ,advance='no') rOrg
      elseif(t<9999999.5d0) then
        write(u, 1 ,advance='no') rOrg
      elseif(t<=9999999.4d0) then
        write(u,10 ,advance='no') nint(rOrg)
      else
!       sacrifice significant figures in output to gain fixed field-width
        r=dble(rOrg) ! preclude modifying rOrg
      ! preclude problems with rOrg near overflow in magnitude
        if(rOrg>MaxMagR4) then
          r=MaxMagR8
          t=r
        else if(rOrg<-MaxMagR4) then
          r=-MaxMagR8
          t=-10.0d0*r
        end if
        do iExpo=1,98 ! 2 is minimum needed to express nint(t) as iiiiiEn
          t=0.1d0*t
          r=0.1d0*r
        ! preclude NDP error taking idnint of large #s
          if(t<2.147484d9) then ! idnint() can handle up to 2**31
            iMant=idnint(t)
            if(((iMant<=999999).and.(iExpo<=9)).or.(iMant<=99999)) exit
          end if
        end do
        iMant=idnint(r)
        call flush(u)
        if(iExpo<=9) then
          write(u,11 ,advance='no') iMant,iExpo
        else
          write(u,12 ,advance='no') iMant,iExpo
        end if
      end if
      return
    ! ampersand usage under F77L3 requires CarriageControl=fortran
    1 format( f9.1,',')
    2 format( f9.2,',')
    3 format( f9.3,',')
    4 format( f9.4,',')
    5 format( f9.5,',')
    6 format( f9.6,',')
    7 format( f9.7,',')
   10 format( i9,  ',')
   11 format( i6,'.E',i1,',') ! note wasteful decimal point as requested
   12 format( i5,'.E',i2,',')
   13 format( i4,'.E',i3,',')
      end ! subroutine AppendFloat9
c-----
      subroutine ps(ErrorLevel,message) ! ps is a mnemonic for Print and Stop
!     message codes per GAT on 20060926:
!       14       => user terminated (N.A. to this subroutine)
!       15 or 16 => bomb/crash/error
!       17 or 18 => successful execution
      logical*1 CurrentBatModeState,BatchMode/.true./ ! default implying 'pause to read message'
      logical*4 UnitInUse
      integer*4 ErrorLevel,iStat
      integer*2 ExitCode
      character*(*) message
!
!     opening unit 4 inside a .DLL does not make it writable outside the .DLL;
!     .EXE-caller of .DLL finds UnitInUse below false on 20040628, so the .DLL
!     must write message and flush(4) just before returning to the caller
      inquire(unit=4,opened=UnitInUse,err=100,ioStat=iStat)
      if((iStat==0).and.UnitInUse) then ! file is open
        write(4,'(1x  )',advance='yes') ! terminate prior messages
        write(4,'(1x,a)',advance='yes') message
        close(4)
      end if
  100 continue

      write(*,'(1x  )',advance='yes') ! terminate prior messages
      write(*,'(1x,a)',advance='yes') message
      if(ErrorLevel/=0) then ! allow operator to read the diagnostic message

        if((ErrorLevel==1).and.(.not.BatchMode)) then
            call end_program("file_ios:0002 - Found PAUSE. " //
     + "Terminating.")

		endif
        ExitCode=15 ! failure due to error
      else
        ExitCode=17 ! success
      end if

      OPEN(10,FILE='SIMDONE') ! with default CarriageControl='list'

      WRITE(10,'(i2,a)') ExitCode,' Exit Code'
      CLOSE(10)

      CALL EXIT(ExitCode)
      return ! pro-forma; unnecessary here
c-----
      entry SetBatchMode
      BatchMode=.true. ! implying 'do not pause to read error message'
      return ! entry SetBatchMode
c-----
      entry GetBatchMode(CurrentBatModeState)
      CurrentBatModeState=BatchMode
      end ! subroutine ps
c-----
