!     ******************************************************************
!     MSGINDXR.F95
!     Copyright(c) DrG Solutions 2011
!
!     Created: 7/21/2011 9:02:13 AM
!     Author : Mark S Gerber
!     Last change: msg 4/17/2018 9:44:05 AM
!			MSG 7/10/2017 3:21:50 PM
!			MSG 7/10/2017 3:21:05 PM
!			MSG 7/10/2017 3:19:51 PM
!			MSG 7/10/2017 3:17:45 PM
!			MSG 7/10/2017 3:15:51 PM
!			MSG 7/10/2017 3:15:22 PM
!			MSG 7/10/2017 3:04:21 PM
!			MSG 7/10/2017 3:04:01 PM
!			MSG 7/10/2017 3:01:34 PM
!			MSG 7/10/2017 2:59:51 PM
!			MSG 7/10/2017 2:41:59 PM
!			MSG 7/10/2017 2:39:42 PM
!			MSG 7/10/2017 2:14:11 PM
!			MSG 7/10/2017 2:11:07 PM
!			MSG 7/10/2017 2:08:05 PM
!			MSG 7/10/2017 2:07:05 PM
!			MSG 7/7/2017 9:10:12 PM
!			MSG 7/7/2017 5:51:40 PM
!			MSG 7/7/2017 5:44:06 PM
!			MSG 7/7/2017 5:43:06 PM
!			MSG 7/7/2017 5:41:23 PM
!			MSG 7/7/2017 4:25:16 PM
!			MSG 7/7/2017 4:21:14 PM
!			MSG 7/7/2017 4:04:08 PM
!			MSG 7/7/2017 3:29:13 PM
!			MSG 7/7/2017 3:21:16 PM
!			MSG 7/7/2017 3:20:17 PM
!			MSG 7/7/2017 3:18:57 PM
!			MSG 7/7/2017 3:05:00 PM
!			MSG 7/7/2017 2:27:19 PM
!			MSG 7/7/2017 2:24:19 PM
!			MSG 7/7/2017 2:17:14 PM
!			MSG 7/7/2017 2:01:49 PM
!			MSG 7/7/2017 1:58:34 PM
!			MSG 7/7/2017 1:54:19 PM
!			MSG 7/7/2017 12:43:20 PM
!			MSG 7/7/2017 12:42:58 PM
!			MSG 7/7/2017 12:42:52 PM
!			MSG 7/7/2017 12:39:15 PM
!			MSG 7/7/2017 12:06:52 PM
!			MSG 7/7/2017 11:54:32 AM
!			MSG 7/7/2017 11:28:19 AM
!			MSG 7/7/2017 10:01:28 AM
!			MSG 7/7/2017 9:58:52 AM
!			MSG 7/7/2017 9:31:55 AM
!			MSG 7/6/2017 10:05:56 PM
!			MSG 7/6/2017 10:05:27 PM
!			MSG 7/6/2017 9:59:12 PM
!			MSG 7/6/2017 9:53:27 PM
!			MSG 7/6/2017 9:45:23 PM
!			MSG 7/6/2017 9:39:49 PM
!			MSG 7/6/2017 9:39:14 PM
!			MSG 7/6/2017 9:38:38 PM
!			MSG 7/6/2017 9:26:56 PM
!			MSG 7/6/2017 9:25:34 PM
!			MSG 7/6/2017 9:24:26 PM
!			MSG 7/6/2017 9:08:32 PM
!			MSG 7/6/2017 9:04:19 PM
!			MSG 7/6/2017 9:03:55 PM
!			MSG 7/6/2017 8:58:36 PM
!			MSG 7/6/2017 7:58:53 PM
!			MSG 7/6/2017 7:58:42 PM
!			MSG 7/6/2017 7:52:07 PM
!			MSG 7/6/2017 7:47:01 PM
!			MSG 7/6/2017 7:38:27 PM
!			MSG 7/6/2017 7:38:21 PM
!			MSG 7/6/2017 7:25:41 PM
!			MSG 7/6/2017 7:10:49 PM
!			MSG 7/6/2017 7:06:34 PM
!			MSG 7/6/2017 7:05:57 PM
!			MSG 7/6/2017 7:01:35 PM
!			MSG 7/6/2017 7:00:54 PM
!			MSG 7/6/2017 5:49:43 PM
!			MSG 7/6/2017 5:39:11 PM
!			MSG 7/6/2017 4:50:28 PM
!			MSG 7/6/2017 4:37:12 PM
!			MSG 7/6/2017 4:29:11 PM
!			MSG 7/6/2017 4:23:18 PM
!			MSG 7/6/2017 4:20:29 PM
!			MSG 7/6/2017 4:17:41 PM
!			MSG 7/6/2017 4:16:16 PM
!			MSG 7/6/2017 4:15:36 PM
!			MSG 7/6/2017 4:14:49 PM
!			MSG 7/6/2017 4:14:19 PM
!			MSG 7/6/2017 4:03:38 PM
!			MSG 7/6/2017 4:01:51 PM
!			MSG 7/6/2017 3:56:18 PM
!			MSG 7/6/2017 3:41:39 PM
!			MSG 7/6/2017 2:18:12 PM
!			MSG 7/6/2017 10:07:05 AM
!     ******************************************************************


      subroutine msgindex_main  
      USE realwin
      IMPLICIT NONE
      TYPE(status_t), DIMENSION(4) :: status_bar
      INTEGER  win_handle, i, font_type, status_bar_font,Arial_10_Bold
      character (len=256) MIDAS_EXE_NAME
      CHARACTER (LEN=256)::cmdln
         call get_exe_name(MIDAS_EXE_NAME)
         CALL GETCL(cmdln)
         CALL set_status_item (status_bar(1), portion = 0.,
     +                                          code = STATUS_DATE_TIME)
         CALL set_status_item (status_bar(2), portion = .35,
     +           code = STATUS_USER_TEXT, text = trim(MIDAS_EXE_NAME))
!         CALL set_status_item (status_bar(3), portion = .1,
!     +          code = STATUS_USER_TEXT, text = trim(cmdln))
!         CALL set_status_item (status_bar(4), portion = .2,
!     +                     code = STATUS_USER_TEXT, text = "My Message")
         Arial_10_Bold =
     +                select_font(typeface='ARIAL',point=10,bold=.true.)
         
         win_handle = create_main_window(
     +        window_name="MIDAS Gold Results Indexing Version WIN-95A",
     +        paint_code=SCROLL_VERT_TEXT,x=.2,y=.2,width=.5,height=.35,
     +        text_font=Arial_10_Bold, ! , status_bar=status_bar)
     +        icon=load_icon(resource="MIDASicon"))
         status_bar_font =
     +                 select_font(typeface='ARIAL',point=8,bold=.true.)
         call set_status_bar(status_bar=status_bar,update_flag=.true.,
     +                                             font=status_bar_font)
!         CALL write_scroll_line(window = win_handle, text = cmdln)
!         CALL set_status_item (status_bar(4), portion = .2,
!     +                              code = STATUS_USER_TEXT,
!     +                              text = "New Message for status bar")
         call set_status_bar(status_bar=status_bar,update_flag=.true.)
         call MSG_INDEX_DETAILED_REPORTS(win_handle,status_bar)


      end subroutine msgindex_main  
C
      SUBROUTINE MSG_INDEX_DETAILED_REPORTS(handle,status_bar)
C
      use WinFileFindData
      USE realwin
      IMPLICIT NONE
      TYPE(status_t), DIMENSION(4) :: status_bar
      INTEGER  handle, font_type, status_bar_font
      
      CHARACTER*256 COMMAND_LINE,STUDY_DIRECTORY
      CHARACTER*5 StudyName
      CHARACTER*2 ReportCode
      LOGICAL INDEX_MADE,INDEX_THIS_DETAILED_REPORT
      INTEGER I,LENGHT,StartTime,EndTime
      REAL REAL_TIME
      CHARACTER (LEN=30) :: ReportName
      INTEGER*2 MaxReportCodes
      PARAMETER(MaxReportCodes=77)
      CHARACTER (LEN=2), DIMENSION(MaxReportCodes) :: ReportCodeList=
     +              (/"DP","TB","CC","PD","UD","TE","TA","TQ","TZ","MZ",
     +                "EM","DO","MS","DW","CX","HX","TO","HM","TP","SD",
     +                "HC","TI","TK","TR","PI","TD","BH","TL","TN","LM",
     +                "PB","PA","AR","AO","AL","MX","RM","EP","TS","PS",
     +                "TM","TT","UM","UM","EC","CX","FD","FM","LD","DC",
     +                "MD","DT","RA","FA","ST","SA","FI","FX","MC","AI",
     +                "DI","CD","BD","XD","AE","AF","DD","DB","NF","OX",
     +                "HE","HA","VH","VD","VM","VP","GK"/)
      CHARACTER (LEN=30),DIMENSION(MaxReportCodes) :: ReportTitle
      type(WIN32_FIND_DATA) :: FileData
  
      integer :: FileHandle, FileNameLenght, CurrentFileHandle
      character (len=256) :: FileMask,MESSAGE_TEXT
      logical :: void_logical
C
         ReportTitle( 1) = "Energy Products"
         ReportTitle( 2) = "Monthly Summary"
         ReportTitle( 3) = "Transact Class Revenue"
         ReportTitle( 4) = "Market Duration"
         ReportTitle( 5) = "Transaction Group Duration"
         ReportTitle( 6) = "Monthly CL Trans Unit"
         ReportTitle( 7) = "Annual CL Unit"
         ReportTitle( 8) = "Monthly Transactions"
         ReportTitle( 9) = "Transact Fuel"
         ReportTitle(10) = "Monthly Group"
         ReportTitle(11) = "Monthly EL Unit"
         ReportTitle(12) = "CL Dispatch Order"
         ReportTitle(13) = "Transact Dispatch Order"
         ReportTitle(14) = "Calendar Day of Week"
         ReportTitle(15) = "Daily Utilization"
         ReportTitle(16) = "Hourly Utilization"
         ReportTitle(17) = "Load After Trans"
         ReportTitle(18) = "Regional Price"
         ReportTitle(19) = "MC After Trans"
         ReportTitle(20) = "Capacity Cost"
         ReportTitle(21) = "Capacity"
         ReportTitle(22) = "Hourly Transaction"
         ReportTitle(23) = "Last Traded Price"
         ReportTitle(24) = "Trans Revenues"
         ReportTitle(25) = "Increm Prod Cost"
         ReportTitle(26) = "Hourly Margins"
         ReportTitle(27) = "Hourly Load B4H"
         ReportTitle(28) = "Hourly Load B4 AFH"
         ReportTitle(29) = "Hourly MC B4"
         ReportTitle(30) = "Hourly Lamda"
         ReportTitle(31) = "Prod Cost B4"
         ReportTitle(32) = "Prod Cost AF"
         ReportTitle(33) = "Resource Additions"
         ReportTitle(34) = "Resources Tested"
         ReportTitle(35) = "Resource Options"
         ReportTitle(36) = "MRX Annual Resources"
         ReportTitle(37) = "Reserve Margin"
         ReportTitle(38) = "Expansion Pattern"
         ReportTitle(39) = "Annual CL Summary"
         ReportTitle(40) = "Annual Pool Summary"
         ReportTitle(41) = "Monthly CL Unit"
         ReportTitle(42) = "Monthly CL Summary"
         ReportTitle(43) = "Maintenance Report"
         ReportTitle(44) = "Availability Report"
         ReportTitle(45) = "Economy Report"
         ReportTitle(46) = "Monthly Contracts"
         ReportTitle(47) = "Forecasted Days"
         ReportTitle(48) = "Forecasted Months"
         ReportTitle(49) = "Load Duration Curves"
         ReportTitle(50) = "Load Management Cost"
         ReportTitle(51) = "Monthly LM Days"
         ReportTitle(52) = "DSM Rate Impact"
         ReportTitle(53) = "Class Revenue"
         ReportTitle(54) = "Cost Distribution"
         ReportTitle(55) = "Service Transactions"
         ReportTitle(56) = "Annual Service Transactions"
         ReportTitle(57) = "Fuel Inventory"
         ReportTitle(58) = "Resource Decisions"
         ReportTitle(59) = "Marginal Cost Report"
         ReportTitle(60) = "Asset Class Financial"
         ReportTitle(61) = "Income Drilling"
         ReportTitle(62) = "Cash Drilling"
         ReportTitle(63) = "Balance Drilling"
         ReportTitle(64) = "Tax Drilling"
         ReportTitle(65) = "Existing Assets"
         ReportTitle(66) = "Future Assets"
         ReportTitle(67) = "Debits"
         ReportTitle(68) = "Debt"
         ReportTitle(69) = "Nuclear Fuel"
         ReportTitle(70) = "Hourly Outage" 
         ReportTitle(71) = "HEC Hourly"
         ReportTitle(72) = "Hydro Aggregation"
         ReportTitle(73) = "Hourly Products"
         ReportTitle(74) = "Daily Products"
         ReportTitle(75) = "Monthly Products"
         ReportTitle(76) = "Portfolio Detail"
         ReportTitle(77) = "Monthly Greeks"
C
         STUDY_DIRECTORY = " "
         CALL GETCL(COMMAND_LINE)
         IF(LEN_TRIM(COMMAND_LINE) == 0) THEN
!            write(6,'(1X,A)',ADVANCE='No') 'Enter study name:'
!            READ(5,*) StudyName
!            write(6,'(A)',ADVANCE='No')
!     +         'Enter two letter report code or A for all reports:'
!            READ(5,*) ReportCode
!            WRITE(6,*) " "
             CALL write_scroll_line(window = handle,
     +                text = "Study Name and Project Directory must "//
     +                                "be entered on the command line.")
        ELSE
            IF(INDEX(COMMAND_LINE,'"') /= 0) THEN
               LENGHT = INDEX(COMMAND_LINE,' ')
               StudyName = COMMAND_LINE(2:LENGHT-2)
               COMMAND_LINE = COMMAND_LINE(LENGHT+1:)
               LENGHT = INDEX(COMMAND_LINE,'"')
               COMMAND_LINE = COMMAND_LINE(LENGHT+1:)
               LENGHT = INDEX(COMMAND_LINE,'"')
               STUDY_DIRECTORY = COMMAND_LINE(1:LENGHT-1)
            ELSE
               LENGHT = INDEX(COMMAND_LINE,' ')
               StudyName = COMMAND_LINE(1:LENGHT)
               STUDY_DIRECTORY = COMMAND_LINE(LENGHT+1:)
            ENDIF
!            WRITE(6,*) 'Study name: ',StudyName
!            CALL write_scroll_line(window = handle,
!     +                                 text = 'Study name: '//StudyName)
            CALL set_status_item (status_bar(3), portion = .1,
     +          code = STATUS_USER_TEXT, text = trim(StudyName))
            IF(trim(STUDY_DIRECTORY) /= ' ') THEN
!               WRITE(6,*) 'Project directory: ',STUDY_DIRECTORY
!               CALL write_scroll_line(window = handle,
!     +            text = 'Project directory: '//trim(STUDY_DIRECTORY))
               CALL set_status_item (status_bar(4), portion = .35,
     +          code = STATUS_USER_TEXT, text = trim(STUDY_DIRECTORY))
               LENGHT = INDEX(STUDY_DIRECTORY,' ')
               IF(STUDY_DIRECTORY(LENGHT-1:LENGHT-1) /= '\') THEN 
                  STUDY_DIRECTORY = trim(STUDY_DIRECTORY)//'\'
               ENDIF
            ENDIF
            ReportCode = 'A'
         ENDIF
C
         call set_status_bar(status_bar=status_bar,update_flag=.true.)
         CALL TIMER(StartTime)
         
         IF(trim(ReportCode) == 'A' .OR. 
     +                                   trim(ReportCode) == 'a') THEN
            IF(trim(STUDY_DIRECTORY) == ' ') THEN
               FileMask = 'MSG'//trim(StudyName)//'.*D'
            ELSE
               FileMask = trim(STUDY_DIRECTORY)//
     +                     'MSG'//trim(StudyName)//'.*D'
            ENDIF
            FileMask = trim(FileMask)//CHAR(0)
            FileHandle = FindFirstFileA(carg(FileMask),FileData)
            if(FileHandle > 0) then
               CurrentFileHandle = FileHandle
               DOWHILE (CurrentFileHandle > 0)
                  FileNameLenght = INDEX(FileData%FileName,CHAR(0))
                  ReportCode = FileData%FileName(FileNameLenght-3:
     +                                                 FileNameLenght-2)
                  if(ReportCode /= 'LC') then
                     ReportName = "No match!"
                     DO I = 1, MaxReportCodes
                        IF(ReportCodeList(I) == ReportCode) THEN
                           ReportName = ReportTitle(I)
                           EXIT
                        ENDIF
                     ENDDO
                     INDEX_MADE = INDEX_THIS_DETAILED_REPORT(StudyName,
     +                                                 ReportCode,
     +                                                 ReportName,
     +                                                 STUDY_DIRECTORY,
     +                                                 handle)
                  endif
                  CurrentFileHandle =
     +                          FindNextFileA(carg(FileHandle),FileData)
               ENDDO
               void_logical = FindClose(FileHandle)
            endif
         ELSE
            ReportName = "No match!"
            DO I = 1, MaxReportCodes
               IF(ReportCodeList(I) == ReportCode) THEN
                  ReportName = ReportTitle(I)
                  EXIT
               ENDIF
            ENDDO
            INDEX_MADE = INDEX_THIS_DETAILED_REPORT(StudyName,
     +                                              ReportCode,
     +                                              ReportName,
     +                                              STUDY_DIRECTORY,
     +                                              handle)
         ENDIF
         CALL TIMER(EndTime)
         REAL_TIME = FLOAT(EndTime-StartTime)/100.
         IF(REAL_TIME <= 60.) THEN
            WRITE(MESSAGE_TEXT,'(F4.1,A)') REAL_TIME,
     +                             ' seconds to index ALL results file.'
         ELSE
            REAL_TIME = REAL_TIME/60.
            WRITE(MESSAGE_TEXT,'(F4.1,A)') REAL_TIME,
     +                             ' minutes to index ALL results file.'
         ENDIF
         CALL write_scroll_line(window = handle,
     +                                      text = trim(MESSAGE_TEXT))
C
         RETURN
      END
C***********************************************************************
      LOGICAL FUNCTION INDEX_THIS_DETAILED_REPORT(StudyName,
     +                                            ReportCode,
     +                                            ReportName,
     +                                            STUDY_DIRECTORY,
     +                                            handle)
C***********************************************************************
      USE realwin
      IMPLICIT NONE
      CHARACTER*5 StudyName
      CHARACTER*2 ReportCode
      CHARACTER*256 FILE_NAME,MESSAGE_TEXT
      CHARACTER*4 NUM_INDEX_VALUE
      CHARACTER*3 INDEX_EXT
      CHARACTER*30 ReportName
      INTEGER FILE_SIZE,handle
      LOGICAL FILE_EXISTS,SECOND_DEM_IS_YEAR
      INTEGER*2 RECORD_LENGTH,
     +          DIMENSIONS,
     +          BASE_YEAR,
     +          LAST_YEAR,
     +          NUM_OF_END_POINTS
      CHARACTER (LEN=*) STUDY_DIRECTORY
      CHARACTER (LEN=20), ALLOCATABLE, DIMENSION(:) :: DIMENSION_NAME
      INTEGER*2, ALLOCATABLE, DIMENSION(:) :: DIMENSION_LENGHT
      CHARACTER (LEN=1), ALLOCATABLE, DIMENSION(:) :: DIMENSION_TYPE
      INTEGER, ALLOCATABLE, DIMENSION(:) :: DEM_1_PTR,
     +                                      DEM_2_PTR,
     +                                      DEM_3_PTR,
     +                                      DEM_4_PTR,
     +                                      DEM_5_PTR,
     +                                      DEM_6_PTR, 
     +                                      DEM_7_PTR 
      CHARACTER(LEN=90),ALLOCATABLE,DIMENSION(:) :: DEM_1_NAME_STORAGE,
     +                                              DEM_2_NAME_STORAGE,
     +                                              DEM_3_NAME_STORAGE,
     +                                              DEM_4_NAME_STORAGE,
     +                                              DEM_5_NAME_STORAGE,
     +                                              DEM_6_NAME_STORAGE, 
     +                                              DEM_7_NAME_STORAGE 
      INTEGER, ALLOCATABLE, DIMENSION(:) :: REC_POS
      CHARACTER (LEN=90), ALLOCATABLE :: TEMP_CHR(:)
      INTEGER (KIND=4), ALLOCATABLE :: TEMP_I4(:)
      CHARACTER (LEN=10) :: ValueText
      INTEGER DEM_1_UNIQUE_VALUES,
     +        DEM_2_UNIQUE_VALUES,
     +        DEM_3_UNIQUE_VALUES,
     +        DEM_4_UNIQUE_VALUES,
     +        DEM_5_UNIQUE_VALUES,
     +        DEM_6_UNIQUE_VALUES, 
     +        DEM_7_UNIQUE_VALUES,
     +        UniqueValues
      INTEGER DEM_1_MAX_VALUES,
     +        DEM_2_MAX_VALUES,
     +        DEM_3_MAX_VALUES,
     +        DEM_4_MAX_VALUES,
     +        DEM_5_MAX_VALUES,
     +        DEM_6_MAX_VALUES,
     +        DEM_7_MAX_VALUES,
     +        MEMORY_NEEDED
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: DEM_1_REC_POS,
     +                                        DEM_2_REC_POS,
     +                                        DEM_3_REC_POS,
     +                                        DEM_4_REC_POS,
     +                                        DEM_5_REC_POS,
     +                                        DEM_6_REC_POS,
     +                                        DEM_7_REC_POS
      INTEGER I,J,RECORDS_IN_FILE,MAX_YEAR_RECS,TOTAL_DIMENSION_LENGTH,
     +        POS,REC,MAX_UNIQUE_VALUES,ASize,NewSize
      CHARACTER (LEN=256) :: DIM_BALANCE
      real (kind=4) :: endpoint,year,day
      character (len=9) :: Month
      character (len=25) :: GenUnit
      character (len=65) :: resource
      REAL*4 REAL_TIME
      INTEGER IOS,LineToDelete,StartLine,LastColumn,FirstColumn
      INTEGER START_TIME,CURRENT_TIME,SUB_DIMENSION,
     +        PROCESS_START_TIME,PROCESS_END_TIME
      REAL REAL_VALUE
      CHARACTER*100 CHAR_NAME
      CHARACTER*4 CHAR_NUM
      INTEGER DIM_VALUE,VALUE_CHK
      EQUIVALENCE(REAL_VALUE,CHAR_NUM) 
C
         IF(trim(STUDY_DIRECTORY) == ' ') THEN
            FILE_NAME = 'MSG'//trim(StudyName)//'.'//ReportCode//'D'
         ELSE
            FILE_NAME = trim(STUDY_DIRECTORY)//
     +                  'MSG'//trim(StudyName)//'.'//ReportCode//'D'
         ENDIF
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS,FLEN=FILE_SIZE)
         IF(.NOT. FILE_EXISTS) THEN
           INDEX_THIS_DETAILED_REPORT = FILE_EXISTS
           RETURN
         ENDIF
         OPEN(11,FILE_NAME,ACCESS='TRANSPARENT',STATUS='OLD')
         READ(11,REC=2,IOSTAT=IOS) RECORD_LENGTH,  
     +                             DIMENSIONS,
     +                             BASE_YEAR,
     +                             LAST_YEAR,
     +                             NUM_OF_END_POINTS
         CLOSE(11)
         IF(IOS /= 0) RETURN
         IF(DIMENSIONS < 2) THEN
!            WRITE(6,*) ReportCode," ",ReportName    
!             WRITE(6,*)'***Because there are less than two dimensions,'
!            WRITE(6,*) 'This file cannot be indexed for use by this ',
!     +                'program for use in the MIDAS Gold Results module' 
!            WRITE(6,*) 'The results module will index it.***'
            WRITE(MESSAGE_TEXT,*) ReportCode," ",ReportName    
            CALL write_scroll_line(window = handle,
     +                                      text = trim(MESSAGE_TEXT))
            CALL write_scroll_line(window = handle,
     +          text = '***Because there are less than two dimensions,')
            CALL write_scroll_line(window = handle,
     +          text = 'This file cannot be indexed for use by this ')
            CALL write_scroll_line(window = handle,
     +          text='program for use in the MIDAS Gold Results module')
            CALL write_scroll_line(window = handle,
     +          text = 'The results module will index it.***')
            RETURN
         ENDIF
         OPEN(11,FILE_NAME,ACCESS='DIRECT',STATUS='OLD',
     +                                               RECL=RECORD_LENGTH)
         ALLOCATE(DIMENSION_NAME(DIMENSIONS),
     +            DIMENSION_TYPE(DIMENSIONS),
     +            DIMENSION_LENGHT(DIMENSIONS))
C
         TOTAL_DIMENSION_LENGTH = 0
         DO I = 1, DIMENSIONS
            READ(11,REC=3+I,IOSTAT=IOS) DIMENSION_NAME(I),
     +                       DIMENSION_TYPE(I),
     +                       DIMENSION_LENGHT(I)
            IF(IOS /= 0) THEN
               WRITE(MESSAGE_TEXT,*) "ERROR indexing ",
     +                                         ReportCode," ",ReportName    
               CALL write_scroll_line(window = handle,
     +                                      text = trim(MESSAGE_TEXT))
               CALL write_scroll_line(window = handle,
     +                                      text = trim(FILE_NAME))
               WRITE(MESSAGE_TEXT,*) "Dimensions ",DIMENSIONS
               CALL write_scroll_line(window = handle,
     +                                      text = trim(MESSAGE_TEXT))
               WRITE(MESSAGE_TEXT,*) "Record Lenght ",RECORD_LENGTH
               CALL write_scroll_line(window = handle,
     +                                      text = trim(MESSAGE_TEXT))
               DEALLOCATE(DIMENSION_NAME,
     +                    DIMENSION_TYPE,
     +                    DIMENSION_LENGHT)
               RETURN
            ENDIF
            TOTAL_DIMENSION_LENGTH = TOTAL_DIMENSION_LENGTH
     +                                          + DIMENSION_LENGHT(I)
         ENDDO
         RECORDS_IN_FILE = FILE_SIZE/RECORD_LENGTH
C
         DEM_1_UNIQUE_VALUES = 0
         DEM_2_UNIQUE_VALUES = 0
         DEM_3_UNIQUE_VALUES = 0
         DEM_4_UNIQUE_VALUES = 0
         DEM_5_UNIQUE_VALUES = 0
         DEM_6_UNIQUE_VALUES = 0
         DEM_7_UNIQUE_VALUES = 0
         MAX_UNIQUE_VALUES = MIN(10000,RECORDS_IN_FILE)
C
         IF(INDEX(DIMENSION_NAME(1),"Endpoint") /= 0) THEN
            ALLOCATE(DEM_1_PTR(NUM_OF_END_POINTS))
            ALLOCATE(DEM_1_NAME_STORAGE(NUM_OF_END_POINTS))
         ELSE
            ALLOCATE(DEM_1_PTR(MAX_UNIQUE_VALUES))
            ALLOCATE(DEM_1_NAME_STORAGE(MAX_UNIQUE_VALUES))
         ENDIF
         DEM_1_PTR = 0
C
         IF(INDEX(DIMENSION_NAME(2),"Year") /= 0) THEN
            ALLOCATE(DEM_2_PTR(MIN(101,RECORDS_IN_FILE)))
            ALLOCATE(DEM_2_NAME_STORAGE(MIN(101,RECORDS_IN_FILE)))
         ELSE
            ALLOCATE(DEM_2_PTR(MAX_UNIQUE_VALUES))
            ALLOCATE(DEM_2_NAME_STORAGE(MAX_UNIQUE_VALUES))
         ENDIF
         DEM_2_PTR = 0
C
         IF(DIMENSIONS >= 3) THEN
            IF(INDEX(DIMENSION_NAME(3),"Months") /= 0 .OR.
     +                   INDEX(DIMENSION_NAME(3),"Month") /= 0 .OR.
     +                      INDEX(DIMENSION_NAME(3),"Period") /= 0) THEN
               ALLOCATE(DEM_3_PTR(20))
               ALLOCATE(DEM_3_NAME_STORAGE(20))
            ELSE
               ALLOCATE(DEM_3_PTR(MAX_UNIQUE_VALUES))
               ALLOCATE(DEM_3_NAME_STORAGE(MAX_UNIQUE_VALUES))
            ENDIF
            DEM_3_PTR = 0
         ENDIF
         IF(DIMENSIONS >= 4) THEN
            IF(INDEX(DIMENSION_NAME(4),"Month") /= 0 .OR.
     +                      INDEX(DIMENSION_NAME(4),"Period") /= 0) THEN
               ALLOCATE(DEM_4_PTR(20))
               ALLOCATE(DEM_4_NAME_STORAGE(20))
            ELSEIF(INDEX(DIMENSION_NAME(4),"Day") /= 0) THEN
               ALLOCATE(DEM_4_PTR(32))
               ALLOCATE(DEM_4_NAME_STORAGE(32))
            ELSE
               ALLOCATE(DEM_4_PTR(RECORDS_IN_FILE))           !MAX_UNIQUE_VALUES))
               ALLOCATE(DEM_4_NAME_STORAGE(RECORDS_IN_FILE)) ! MAX_UNIQUE_VALUES))
            ENDIF
            DEM_4_PTR = 0
         ENDIF
         IF(DIMENSIONS >= 5) THEN
            ALLOCATE(DEM_5_PTR(RECORDS_IN_FILE))
            DEM_5_PTR = 0
            ALLOCATE(DEM_5_NAME_STORAGE(RECORDS_IN_FILE))
         ENDIF
         IF(DIMENSIONS >= 6) THEN
            ALLOCATE(DEM_6_PTR(MAX_UNIQUE_VALUES))
            DEM_6_PTR = 0
            ALLOCATE(DEM_6_NAME_STORAGE(MAX_UNIQUE_VALUES))
         ENDIF
         IF(DIMENSIONS >= 7) THEN
            ALLOCATE(DEM_7_PTR(MAX_UNIQUE_VALUES))
            DEM_7_PTR = 0
            ALLOCATE(DEM_7_NAME_STORAGE(MAX_UNIQUE_VALUES))
         ENDIF
C         YEAR_REC_POS = 0
         I = DIMENSIONS+4
         CALL TIMER(START_TIME)
         PROCESS_START_TIME = START_TIME
         SUB_DIMENSION = 0
c         WRITE(6,*) ' '
!         WRITE(6,'(A,A,A,A)',ADVANCE='NO')
!     +               ' Indexing ',ReportCode,"-",trim(ReportName)//" "
         WRITE(MESSAGE_TEXT,'(A,A,A,A)')
     +               ' Indexing ',ReportCode,"-",trim(ReportName)//" "
         CALL write_scroll_line(window = handle,
     +                          text = trim(MESSAGE_TEXT),
     +                          update=.TRUE.)
         CALL Get_scroll_position(window=handle,
     +                            first_line=StartLine,
     +                            last_line=LineToDelete,
     +                            last_column=LastColumn,
     +                            first_column=FirstColumn)
!         write(message_text,*) "Start Line=",StartLine,"  ",
!     +                         "Start Col=",FirstColumn,"  ",
!     +                         "Last Line=",LineToDelete,"  ",
!     +                         "End Col=",LastColumn
!         CALL write_scroll_line(window = handle,
!     +                                      text = trim(MESSAGE_TEXT))
         write(1709,*) I
         DO
            READ(11,REC=I,IOSTAT=IOS) 
     +                             DIM_BALANCE(1:TOTAL_DIMENSION_LENGTH)
C            READ(11,REC=I,IOSTAT=IOS) EndPoint,Year,Month,day,resource
C             IF(mod(I,500) == 1) Then
C                  write(1709,*) I,EndPoint,Year,Month,day,resource
C                  call flush(1709)
C              endif
           IF(IOS /= 0) EXIT
C First dimension
            CHAR_NAME =  DIM_BALANCE(1:DIMENSION_LENGHT(1))
            DO J = 1, DEM_1_UNIQUE_VALUES
               IF(DEM_1_NAME_STORAGE(J) ==
     +                            CHAR_NAME(1:DIMENSION_LENGHT(1))) THEN
                  DEM_1_PTR(J) = DEM_1_PTR(J) + 1
                  EXIT
               ENDIF
            ENDDO
            IF(J > DEM_1_UNIQUE_VALUES) THEN
               DEM_1_UNIQUE_VALUES = J
               DEM_1_NAME_STORAGE(J) = CHAR_NAME
               DEM_1_PTR(J) = DEM_1_PTR(J) + 1
            ENDIF
            SUB_DIMENSION = DIMENSION_LENGHT(1)
C Second dimension
            CHAR_NAME =  DIM_BALANCE(SUB_DIMENSION+1:
     +                                SUB_DIMENSION+DIMENSION_LENGHT(2))
            DO J = 1, DEM_2_UNIQUE_VALUES
               IF(DEM_2_NAME_STORAGE(J) ==
     +                         CHAR_NAME(1:DIMENSION_LENGHT(2))) THEN
                  DEM_2_PTR(J) = DEM_2_PTR(J) + 1
                  EXIT
               ENDIF
            ENDDO
            IF(J > DEM_2_UNIQUE_VALUES) THEN
               DEM_2_UNIQUE_VALUES = J
               DEM_2_NAME_STORAGE(J) = CHAR_NAME
               DEM_2_PTR(J) = DEM_2_PTR(J) + 1
            ENDIF
            SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(2)
C Third dimension
            IF(DIMENSIONS >= 3) THEN
               CHAR_NAME =  DIM_BALANCE(SUB_DIMENSION+1:
     +                             SUB_DIMENSION+DIMENSION_LENGHT(3))
               DO J = 1, DEM_3_UNIQUE_VALUES
                  IF(DEM_3_NAME_STORAGE(J) ==
     +                            CHAR_NAME(1:DIMENSION_LENGHT(3))) THEN
                     DEM_3_PTR(J) = DEM_3_PTR(J) + 1
                     EXIT
                  ENDIF
               ENDDO
               IF(J > DEM_3_UNIQUE_VALUES) THEN
                  IF(J >= SIZE(DEM_3_NAME_STORAGE)) THEN
                    ASize = SIZE(DEM_3_NAME_STORAGE)
c                    CALL ArraySizeIncrease(DEM_3_NAME_STORAGE,
c     +                                     DEM_3_PTR,ASize)
                    ALLOCATE(TEMP_I4(ASize))
                    TEMP_I4(1:ASize) = DEM_3_PTR(1:ASize)
                    DEALLOCATE(DEM_3_PTR)
                    NewSize = (FLOAT(J)/FLOAT(I))*FLOAT(RECORDS_IN_FILE)
                    NewSize = NewSize + 100
                    ALLOCATE(DEM_3_PTR(1.2*ASize))
                    DEM_3_PTR = 0
                    DEM_3_PTR(1:ASize) = TEMP_I4(1:ASize)
                    DEALLOCATE (TEMP_I4)
                  
                  
                    ALLOCATE(TEMP_CHR(ASize))
                    TEMP_CHR = ""
                    TEMP_CHR(1:ASize) = DEM_3_NAME_STORAGE(1:ASize)
                    DEALLOCATE(DEM_3_NAME_STORAGE)
                    ALLOCATE(DEM_3_NAME_STORAGE(1.2*ASize))
                    DEM_3_NAME_STORAGE = ""
                    DEM_3_NAME_STORAGE(1:ASize) = TEMP_CHR(1:ASize)
                    DEALLOCATE (TEMP_CHR)
                  ENDIF
                  DEM_3_UNIQUE_VALUES = J
                  DEM_3_NAME_STORAGE(J) = CHAR_NAME
                  DEM_3_PTR(J) = DEM_3_PTR(J) + 1
               ENDIF
               SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(3)
            ENDIF
C Forth dimension
            IF(DIMENSIONS >= 4) THEN
               CHAR_NAME =  DIM_BALANCE(SUB_DIMENSION+1:SUB_DIMENSION
     +                                         + DIMENSION_LENGHT(4))
               DO J = 1, DEM_4_UNIQUE_VALUES
                  IF(DEM_4_NAME_STORAGE(J) ==
     +                            CHAR_NAME(1:DIMENSION_LENGHT(4))) THEN
                     DEM_4_PTR(J) = DEM_4_PTR(J) + 1
                     EXIT
                  ENDIF
               ENDDO
               IF(J > DEM_4_UNIQUE_VALUES) THEN
                  IF(J >= SIZE(DEM_4_NAME_STORAGE)) THEN
                    ASize = SIZE(DEM_4_NAME_STORAGE)
C                    CALL ArraySizeIncrease(DEM_4_NAME_STORAGE,
C     +                                     DEM_4_PTR,ArraySize)
                    ALLOCATE(TEMP_I4(ASize))
                    TEMP_I4(1:ASize) = DEM_4_PTR(1:ASize)
                    DEALLOCATE(DEM_4_PTR)
                    ALLOCATE(DEM_4_PTR(1.2*ASize))
                    DEM_4_PTR = 0
                    DEM_4_PTR(1:ASize) = TEMP_I4(1:ASize)
                    DEALLOCATE (TEMP_I4)
                  
                  
                    ALLOCATE(TEMP_CHR(ASize))
                    TEMP_CHR = ""
                    TEMP_CHR(1:ASize) = DEM_4_NAME_STORAGE(1:ASize)
                    DEALLOCATE(DEM_4_NAME_STORAGE)
                    ALLOCATE(DEM_4_NAME_STORAGE(1.2*ASize))
                    DEM_4_NAME_STORAGE = ""
                    DEM_4_NAME_STORAGE(1:ASize) = TEMP_CHR(1:ASize)
                    DEALLOCATE (TEMP_CHR)
                  ENDIF
                  DEM_4_UNIQUE_VALUES = J
                  DEM_4_NAME_STORAGE(J) = CHAR_NAME
                  DEM_4_PTR(J) = DEM_4_PTR(J) + 1
               ENDIF
               SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(4)
            ENDIF
C Fifth dimension
            IF(DIMENSIONS >= 5) THEN
               CHAR_NAME = DIM_BALANCE(SUB_DIMENSION+1:SUB_DIMENSION
     +                                         + DIMENSION_LENGHT(5))
               IF(I == 30 .or. I == 260 .OR. I == 490) THEN
                  RESOURCE = CHAR_NAME
               ENDIF
               DO J = 1, DEM_5_UNIQUE_VALUES
                  IF(DEM_5_NAME_STORAGE(J) ==
     +                         CHAR_NAME(1:DIMENSION_LENGHT(5))) THEN
                     DEM_5_PTR(J) = DEM_5_PTR(J) + 1
                     EXIT
                  ENDIF
               ENDDO
               IF(J > DEM_5_UNIQUE_VALUES) THEN
                  IF(J >= SIZE(DEM_5_NAME_STORAGE)) THEN
                    ASize = SIZE(DEM_5_NAME_STORAGE)
C                    CALL ArraySizeIncrease(DEM_5_NAME_STORAGE,
C     +                                     DEM_5_PTR,ASize)
                    ALLOCATE(TEMP_I4(ASize))
                    TEMP_I4(1:ASize) = DEM_5_PTR(1:ASize)
                    DEALLOCATE(DEM_5_PTR)
                    NewSize = (FLOAT(J)/FLOAT(I))*FLOAT(RECORDS_IN_FILE)
                    NewSize = NewSize + 100
                    ALLOCATE(DEM_5_PTR(NewSize))
                    DEM_5_PTR = 0
                    DEM_5_PTR(1:ASize) = TEMP_I4(1:ASize)
                    DEALLOCATE (TEMP_I4)
                  
                  
                    ALLOCATE(TEMP_CHR(ASize))
                    TEMP_CHR = ""
                    TEMP_CHR(1:ASize) = DEM_5_NAME_STORAGE(1:ASize)
                    DEALLOCATE(DEM_5_NAME_STORAGE)
                    ALLOCATE(DEM_5_NAME_STORAGE(NewSize))
                    DEM_5_NAME_STORAGE = ""
                    DEM_5_NAME_STORAGE(1:ASize) = TEMP_CHR(1:ASize)
                    DEALLOCATE (TEMP_CHR)
                  ENDIF
                  DEM_5_UNIQUE_VALUES = J
                  DEM_5_NAME_STORAGE(J) = CHAR_NAME
                  DEM_5_PTR(J) = DEM_5_PTR(J) + 1
               ENDIF
               SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(5)
            ENDIF
C Sixth dimension
            IF(DIMENSIONS >= 6) THEN
               CHAR_NAME = DIM_BALANCE(SUB_DIMENSION+1:SUB_DIMENSION
     +                                         + DIMENSION_LENGHT(6))
               DO J = 1, DEM_6_UNIQUE_VALUES
                  IF(DEM_6_NAME_STORAGE(J) ==
     +                         CHAR_NAME(1:DIMENSION_LENGHT(6))) THEN
                     DEM_6_PTR(J) = DEM_6_PTR(J) + 1
                     EXIT
                  ENDIF
               ENDDO
               IF(J > DEM_6_UNIQUE_VALUES) THEN
                  DEM_6_UNIQUE_VALUES = J
                  DEM_6_NAME_STORAGE(J) = CHAR_NAME
                  DEM_6_PTR(J) = DEM_6_PTR(J) + 1
               ENDIF
               SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(6)
            ENDIF
C Seventh dimension
            IF(DIMENSIONS >= 7) THEN
               CHAR_NAME = DIM_BALANCE(SUB_DIMENSION+1:SUB_DIMENSION
     +                                         + DIMENSION_LENGHT(7))
               DO J = 1, DEM_7_UNIQUE_VALUES
                  IF(DEM_7_NAME_STORAGE(J) ==
     +                         CHAR_NAME(1:DIMENSION_LENGHT(7))) THEN
                     DEM_7_PTR(J) = DEM_7_PTR(J) + 1
                     EXIT
                  ENDIF
               ENDDO
               IF(J > DEM_7_UNIQUE_VALUES) THEN
                  DEM_7_UNIQUE_VALUES = J
                  DEM_7_NAME_STORAGE(J) = CHAR_NAME
                  DEM_7_PTR(J) = DEM_7_PTR(J) + 1
               ENDIF
            ENDIF
C
            IF(MOD(I,MAX(1,INT(RECORDS_IN_FILE/20))) == 0) THEN
               CALL TIMER(CURRENT_TIME)
               REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100
c               WRITE(6,'(A)',ADVANCE='NO') '*'
C               WRITE(6,*) I,' records processed in ',REAL_TIME,
C     +                                                       ' seconds.'
            ENDIF
            I = I + 1
         ENDDO
         CALL TIMER(CURRENT_TIME)
         REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100
c         WRITE(6,*) ' '
c         WRITE(6,*) I-1,' records processed in ',REAL_TIME,
c     +                                                       ' seconds.'
C
C SECOND PASS ON DIMENSION 3-Nth
C
c         WRITE(6,*) 'Second pass on results file-',ReportCode
C
C Dimension arrays for record positions
C
         MEMORY_NEEDED = 0
         DEM_1_MAX_VALUES = MAXVAL(DEM_1_PTR(1:DEM_1_UNIQUE_VALUES))
         MEMORY_NEEDED = MEMORY_NEEDED
     +                  + 4 * (DEM_1_MAX_VALUES+1) * DEM_1_UNIQUE_VALUES
         DEALLOCATE(DEM_1_PTR)
         ALLOCATE(DEM_1_PTR(DEM_1_UNIQUE_VALUES),
     +              DEM_1_REC_POS(DEM_1_UNIQUE_VALUES,DEM_1_MAX_VALUES))
         DEM_1_PTR = 0
C
         DEM_2_MAX_VALUES = MAXVAL(DEM_2_PTR(1:DEM_2_UNIQUE_VALUES))
         MEMORY_NEEDED = MEMORY_NEEDED
     +                  + 4 * (DEM_2_MAX_VALUES+1) * DEM_2_UNIQUE_VALUES
         DEALLOCATE(DEM_2_PTR)
         ALLOCATE(DEM_2_PTR(DEM_2_UNIQUE_VALUES),
     +              DEM_2_REC_POS(DEM_2_UNIQUE_VALUES,DEM_2_MAX_VALUES))
         DEM_2_PTR = 0
C
C
         IF(DIMENSIONS >= 3) THEN
            DEM_3_MAX_VALUES = MAXVAL(DEM_3_PTR(1:DEM_3_UNIQUE_VALUES))
            MEMORY_NEEDED = MEMORY_NEEDED
     +                  + 4 * (DEM_3_MAX_VALUES+1) * DEM_3_UNIQUE_VALUES
            DEALLOCATE(DEM_3_PTR)
            ALLOCATE(DEM_3_PTR(DEM_3_UNIQUE_VALUES),
     +              DEM_3_REC_POS(DEM_3_UNIQUE_VALUES,DEM_3_MAX_VALUES))
            DEM_3_PTR = 0
         ENDIF
         IF(DIMENSIONS >= 4) THEN
            DEM_4_MAX_VALUES = MAXVAL(DEM_4_PTR(1:DEM_4_UNIQUE_VALUES))
            MEMORY_NEEDED = MEMORY_NEEDED
     +                  + 4 * (DEM_4_MAX_VALUES+1) * DEM_4_UNIQUE_VALUES
            DEALLOCATE(DEM_4_PTR)
            ALLOCATE(DEM_4_PTR(DEM_4_UNIQUE_VALUES),
     +              DEM_4_REC_POS(DEM_4_UNIQUE_VALUES,DEM_4_MAX_VALUES))
            DEM_4_PTR = 0
         ENDIF
C
         IF(DIMENSIONS >= 5) THEN
            DEM_5_MAX_VALUES = MAXVAL(DEM_5_PTR(1:DEM_5_UNIQUE_VALUES))
            MEMORY_NEEDED = MEMORY_NEEDED
     +                  + 4 * (DEM_5_MAX_VALUES+1) * DEM_5_UNIQUE_VALUES
            DEALLOCATE(DEM_5_PTR)
            ALLOCATE(DEM_5_PTR(DEM_5_UNIQUE_VALUES),
     +              DEM_5_REC_POS(DEM_5_UNIQUE_VALUES,DEM_5_MAX_VALUES))
            DEM_5_PTR = 0
         ENDIF
         IF(DIMENSIONS >= 6) THEN
            DEM_6_MAX_VALUES = MAXVAL(DEM_6_PTR(1:DEM_6_UNIQUE_VALUES))
            MEMORY_NEEDED = MEMORY_NEEDED
     +                  + 4 * (DEM_6_MAX_VALUES+1) * DEM_6_UNIQUE_VALUES
            DEALLOCATE(DEM_6_PTR)
            ALLOCATE(DEM_6_PTR(DEM_6_UNIQUE_VALUES),
     +              DEM_6_REC_POS(DEM_6_UNIQUE_VALUES,DEM_6_MAX_VALUES))
            DEM_6_PTR = 0
         ENDIF
         IF(DIMENSIONS >= 7) THEN
            DEM_7_MAX_VALUES = MAXVAL(DEM_7_PTR(1:DEM_7_UNIQUE_VALUES))
            MEMORY_NEEDED = MEMORY_NEEDED
     +                  + 4 * (DEM_7_MAX_VALUES+1) * DEM_7_UNIQUE_VALUES
            DEALLOCATE(DEM_7_PTR)
            ALLOCATE(DEM_7_PTR(DEM_7_UNIQUE_VALUES),
     +              DEM_7_REC_POS(DEM_7_UNIQUE_VALUES,DEM_7_MAX_VALUES))
            DEM_7_PTR = 0
         ENDIF
C
         I = DIMENSIONS+4
         CALL TIMER(START_TIME)
         DO
            READ(11,REC=I,IOSTAT=IOS)
     +                             DIM_BALANCE(1:TOTAL_DIMENSION_LENGTH)
            IF(IOS /= 0) EXIT
C
C First dimension
            CHAR_NAME = DIM_BALANCE(1:DIMENSION_LENGHT(1))
            DO J = 1, DEM_1_UNIQUE_VALUES
               IF(DEM_1_NAME_STORAGE(J) ==
     +                            CHAR_NAME(1:DIMENSION_LENGHT(1))) THEN
                  DEM_1_PTR(J) = DEM_1_PTR(J) + 1
                  DEM_1_REC_POS(J,DEM_1_PTR(J)) = I - 1 !LF95 IS 1 BASED RECORD
                  EXIT
               ENDIF
            ENDDO
            SUB_DIMENSION = DIMENSION_LENGHT(1)
C Second dimension
            CHAR_NAME = DIM_BALANCE(SUB_DIMENSION+1:
     +                                SUB_DIMENSION+DIMENSION_LENGHT(2))
            DO J = 1, DEM_2_UNIQUE_VALUES
               IF(DEM_2_NAME_STORAGE(J) ==
     +                            CHAR_NAME(1:DIMENSION_LENGHT(2))) THEN
                  DEM_2_PTR(J) = DEM_2_PTR(J) + 1
                  DEM_2_REC_POS(J,DEM_2_PTR(J)) = I - 1 !LF95 IS 1 BASED RECORD
                  EXIT
               ENDIF
            ENDDO
            SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(2)
C
C Third dimension
            IF(DIMENSIONS >= 3) THEN
                  CHAR_NAME =  DIM_BALANCE(SUB_DIMENSION+1:
     +                                SUB_DIMENSION+DIMENSION_LENGHT(3))
                  DO J = 1, DEM_3_UNIQUE_VALUES
                     IF(DEM_3_NAME_STORAGE(J) ==
     +                            CHAR_NAME(1:DIMENSION_LENGHT(3))) THEN
                        DEM_3_PTR(J) = DEM_3_PTR(J) + 1
                        DEM_3_REC_POS(J,DEM_3_PTR(J)) = I - 1 !LF95 IS 1 BASED RECORD
                        EXIT
                     ENDIF
                  ENDDO
               SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(3)
            ENDIF
C
C Forth dimension
            IF(DIMENSIONS >= 4) THEN
                  CHAR_NAME =  DIM_BALANCE(SUB_DIMENSION+1:
     +                                SUB_DIMENSION+DIMENSION_LENGHT(4))
                  DO J = 1, DEM_4_UNIQUE_VALUES
                     IF(DEM_4_NAME_STORAGE(J) ==
     +                            CHAR_NAME(1:DIMENSION_LENGHT(4))) THEN
                        DEM_4_PTR(J) = DEM_4_PTR(J) + 1
                        DEM_4_REC_POS(J,DEM_4_PTR(J)) = I - 1 !LF95 IS 1 BASED RECORD
                        EXIT
                     ENDIF
                  ENDDO
               SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(4)
            ENDIF
C
C Fifth dimension
            IF(DIMENSIONS >= 5) THEN
               CHAR_NAME =  DIM_BALANCE(SUB_DIMENSION+1:
     +                             SUB_DIMENSION+DIMENSION_LENGHT(5))
               DO J = 1, DEM_5_UNIQUE_VALUES
                  IF(DEM_5_NAME_STORAGE(J) ==
     +                            CHAR_NAME(1:DIMENSION_LENGHT(5))) THEN
                     DEM_5_PTR(J) = DEM_5_PTR(J) + 1
                     DEM_5_REC_POS(J,DEM_5_PTR(J)) = I - 1 !LF95 IS 1 BASED RECORD
                     EXIT
                  ENDIF
               ENDDO
               SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(5)
            ENDIF
C
C Sixth dimension
            IF(DIMENSIONS >= 6) THEN
               CHAR_NAME =  DIM_BALANCE(SUB_DIMENSION+1:
     +                             SUB_DIMENSION+DIMENSION_LENGHT(6))
               DO J = 1, DEM_6_UNIQUE_VALUES
                  IF(DEM_6_NAME_STORAGE(J) ==
     +                            CHAR_NAME(1:DIMENSION_LENGHT(6))) THEN
                     DEM_6_PTR(J) = DEM_6_PTR(J) + 1
                     DEM_6_REC_POS(J,DEM_6_PTR(J)) = I - 1 !LF95 IS 1 BASED RECORD
                     EXIT
                  ENDIF
               ENDDO
               SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(6)
            ENDIF
C
C Seventh dimension
            IF(DIMENSIONS >= 7) THEN
               CHAR_NAME =  DIM_BALANCE(SUB_DIMENSION+1:
     +                             SUB_DIMENSION+DIMENSION_LENGHT(7))
               DO J = 1, DEM_7_UNIQUE_VALUES
                  IF(DEM_7_NAME_STORAGE(J) ==
     +                            CHAR_NAME(1:DIMENSION_LENGHT(7))) THEN
                     DEM_7_PTR(J) = DEM_7_PTR(J) + 1
                     DEM_7_REC_POS(J,DEM_7_PTR(J)) = I - 1 !LF95 IS 1 BASED RECORD
                     EXIT
                  ENDIF
               ENDDO
               SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(7)
            ENDIF
C
            IF(MOD(I,MAX(1,INT(RECORDS_IN_FILE/20))) == 0) THEN
               CALL TIMER(CURRENT_TIME)
               REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100
c               WRITE(6,'(A)',ADVANCE='NO') '*'
C               WRITE(6,*) I,' records processed in ',REAL_TIME,
C     +                                                       ' seconds.'
            ENDIF
            I = I + 1
         ENDDO
         CALL TIMER(CURRENT_TIME)
         REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100
c         WRITE(6,*) ' '
c         WRITE(6,*) I-1,' records processed in ',REAL_TIME,
c     +                                                       ' seconds.'
         CLOSE(11)
c
!
! 041718. REMOVED.
!
!         WRITE(ValueText,'(I10)') RECORDS_IN_FILE
!         MESSAGE_TEXT = '   Records:  '//ADJUSTL(ValueText)
!         CALL write_scroll_line(window = handle,
!     +                                        text = TRIM(MESSAGE_TEXT))
!         DO I = 1, DIMENSIONS
!            SELECT CASE (I)
!               CASE (1)
!                  UniqueValues = DEM_1_UNIQUE_VALUES
!               CASE (2)
!                  UniqueValues = DEM_2_UNIQUE_VALUES
!               CASE (3)
!                  UniqueValues = DEM_3_UNIQUE_VALUES
!               CASE (4)
!                  UniqueValues = DEM_4_UNIQUE_VALUES
!               CASE (5)
!                  UniqueValues = DEM_5_UNIQUE_VALUES
!               CASE (6)
!                  UniqueValues = DEM_6_UNIQUE_VALUES
!               CASE (7)
!                  UniqueValues = DEM_7_UNIQUE_VALUES
!               CASE DEFAULT
!            END SELECT
!            WRITE(ValueText,'(I7)') UniqueValues
!            MESSAGE_TEXT = '   '//TRIM(DIMENSION_NAME(I))//': '//
!     +                                                ADJUSTL(ValueText)
!            CALL write_scroll_line(window = handle,
!     +                                      text = TRIM(MESSAGE_TEXT))
!         ENDDO
C
C PROCESS DIMENTION 1-ENDPOINTS
C
         START_TIME = CURRENT_TIME
         INDEX_EXT = ReportCode//'1'
         CALL INDEXING_FILE_HEADER_INFO(StudyName,
     +                                  INDEX_EXT,
     +                                  DEM_1_UNIQUE_VALUES,
     +                                  1,
     +                                  DEM_1_UNIQUE_VALUES,   
     +                                  DEM_1_PTR,
     +                                  STUDY_DIRECTORY)
         DO I = 1, DEM_1_UNIQUE_VALUES
            IF(DEM_1_PTR(I) > 0) THEN
               CALL INT_INDEX_SORT(DEM_1_PTR(I),DEM_1_REC_POS,I,
     +                                            1,DEM_1_UNIQUE_VALUES)
c               ALLOCATE(REC_POS(DEM_1_PTR(I)))
c               REC_POS(:) = DEM_1_REC_POS(I,:)
c               REC_POS = RECORD_LENGTH*REC_POS
               IF(DIMENSION_TYPE(1) == 'N') THEN  ! AN NUMBER
                  CHAR_NUM = DEM_1_NAME_STORAGE(I)
                  DIM_VALUE = INT(REAL_VALUE)
                  WRITE(NUM_INDEX_VALUE,'(I4)') DIM_VALUE
                  WRITE(10) NUM_INDEX_VALUE,              !REC_POS
     +               (RECORD_LENGTH*DEM_1_REC_POS(I,J),J=1,DEM_1_PTR(I))
               ELSE
                  WRITE(10)DEM_2_NAME_STORAGE(I)(1:DIMENSION_LENGHT(1)),
c     +                     REC_POS
     +               (RECORD_LENGTH*DEM_1_REC_POS(I,J),J=1,DEM_1_PTR(I))
               ENDIF
c               DEALLOCATE(REC_POS)
            ENDIF   
         ENDDO
         CLOSE(10)
         DEALLOCATE(DEM_1_PTR,DEM_1_REC_POS,DEM_1_NAME_STORAGE)
         CALL TIMER(CURRENT_TIME)
         REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100
c         WRITE(6,*) 'END POINT index file finished in ',REAL_TIME,
c     +                                                       ' seconds.'
C
C PROCESS SECOND DIMENSION
C
         START_TIME = CURRENT_TIME
         INDEX_EXT = ReportCode//'2'
         CALL INDEXING_FILE_HEADER_INFO(StudyName,
     +                                  INDEX_EXT,
     +                                  DEM_2_UNIQUE_VALUES,
     +                                  1,
     +                                  DEM_2_UNIQUE_VALUES,   
     +                                  DEM_2_PTR,
     +                                  STUDY_DIRECTORY)
         DO I = 1, DEM_2_UNIQUE_VALUES
            IF(DEM_2_PTR(I) > 0) THEN
               CALL INT_INDEX_SORT(DEM_2_PTR(I),DEM_2_REC_POS,I,
     +                                            1,DEM_2_UNIQUE_VALUES)
               IF(DIMENSION_TYPE(2) == 'N') THEN  ! AN NUMBER
                  CHAR_NUM = DEM_2_NAME_STORAGE(I)
                  DIM_VALUE = INT(REAL_VALUE)
                  WRITE(NUM_INDEX_VALUE,'(I4)') DIM_VALUE
                  WRITE(10) NUM_INDEX_VALUE,
     +               (RECORD_LENGTH*DEM_2_REC_POS(I,J),J=1,DEM_2_PTR(I))
               ELSE
                  WRITE(10)DEM_2_NAME_STORAGE(I)(1:DIMENSION_LENGHT(2)),
     +               (RECORD_LENGTH*DEM_2_REC_POS(I,J),J=1,DEM_2_PTR(I))
               ENDIF
            ENDIF   
         ENDDO
         CLOSE(10)
         DEALLOCATE(DEM_2_PTR,DEM_2_REC_POS,DEM_2_NAME_STORAGE)
         CALL TIMER(CURRENT_TIME)
         REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100
c         WRITE(6,*) trim(DIMENSION_NAME(2)),
c     +                  ' index file finished in ',REAL_TIME,' seconds.'
C
C PROCESS THIRD DIMENSION
C
         IF(DIMENSIONS >= 3) THEN
            START_TIME = CURRENT_TIME
            INDEX_EXT = ReportCode//'3'
            CALL INDEXING_FILE_HEADER_INFO(StudyName,
     +                                  INDEX_EXT,
     +                                  DEM_3_UNIQUE_VALUES,
     +                                  1,
     +                                  DEM_3_UNIQUE_VALUES,   
     +                                  DEM_3_PTR,
     +                                  STUDY_DIRECTORY)
            DO I = 1, DEM_3_UNIQUE_VALUES
               IF(DEM_3_PTR(I) > 0) THEN
                  CALL INT_INDEX_SORT(DEM_3_PTR(I),DEM_3_REC_POS,I,
     +                                            1,DEM_3_UNIQUE_VALUES)
                  IF(DIMENSION_TYPE(3) == 'N') THEN  ! AN NUMBER
                     CHAR_NUM = DEM_3_NAME_STORAGE(I)
                     DIM_VALUE = INT(REAL_VALUE)
                     WRITE(NUM_INDEX_VALUE,'(I4)') DIM_VALUE
                     WRITE(10) NUM_INDEX_VALUE,
     +               (RECORD_LENGTH*DEM_3_REC_POS(I,J),J=1,DEM_3_PTR(I))
                  ELSE
                     WRITE(10)
     +                     DEM_3_NAME_STORAGE(I)(1:DIMENSION_LENGHT(3)),
     +                    (RECORD_LENGTH*DEM_3_REC_POS(I,J),
     +                                                 J=1,DEM_3_PTR(I))
                  ENDIF
               ENDIF   
            ENDDO
            CLOSE(10)
            DEALLOCATE(DEM_3_PTR,DEM_3_REC_POS,DEM_3_NAME_STORAGE)
            CALL TIMER(CURRENT_TIME)
            REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100
c         WRITE(6,*) trim(DIMENSION_NAME(3)),
c     +                  ' index file finished in ',REAL_TIME,' seconds.'
         ENDIF
C
C
C PROCESS REMAINING DIMENTIONS
C
         IF(DIMENSIONS >= 4) THEN
            START_TIME = CURRENT_TIME
            INDEX_EXT = ReportCode//'4'
            CALL INDEXING_FILE_HEADER_INFO(StudyName,
     +                                     INDEX_EXT,
     +                                     DEM_4_UNIQUE_VALUES,
     +                                     1,
     +                                     DEM_4_UNIQUE_VALUES,   
     +                                     DEM_4_PTR,
     +                                     STUDY_DIRECTORY)
            DO I = 1, DEM_4_UNIQUE_VALUES
               IF(DEM_4_PTR(I) > 0) THEN
                  CALL INT_INDEX_SORT(DEM_4_PTR(I),DEM_4_REC_POS,I,
     +                                            1,DEM_4_UNIQUE_VALUES)
                  IF(DIMENSION_TYPE(4) == 'N') THEN  ! AN NUMBER
                     CHAR_NUM = DEM_4_NAME_STORAGE(I)
                     DIM_VALUE = INT(REAL_VALUE)
                     WRITE(NUM_INDEX_VALUE,'(I4)') DIM_VALUE
                     WRITE(10) NUM_INDEX_VALUE,
     +               (RECORD_LENGTH*DEM_4_REC_POS(I,J),J=1,DEM_4_PTR(I))
                  ELSE
                     WRITE(10)
     +                     DEM_4_NAME_STORAGE(I)(1:DIMENSION_LENGHT(4)),
     +                    (RECORD_LENGTH*DEM_4_REC_POS(I,J),
     +                                                 J=1,DEM_4_PTR(I))
                  ENDIF
               ENDIF   
            ENDDO
            CLOSE(10)
            CALL TIMER(CURRENT_TIME)
            REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100
c            WRITE(6,*) trim(DIMENSION_NAME(4)),
c     +                  ' index file finished in',REAL_TIME,' seconds.'
            DEALLOCATE(DEM_4_PTR,DEM_4_REC_POS,DEM_4_NAME_STORAGE)
         ENDIF
C
         IF(DIMENSIONS >= 5) THEN
            START_TIME = CURRENT_TIME
            INDEX_EXT = ReportCode//'5'
            CALL INDEXING_FILE_HEADER_INFO(StudyName,
     +                                     INDEX_EXT,
     +                                     DEM_5_UNIQUE_VALUES,
     +                                     1,
     +                                     DEM_5_UNIQUE_VALUES,   
     +                                     DEM_5_PTR,
     +                                     STUDY_DIRECTORY)
            DO I = 1, DEM_5_UNIQUE_VALUES
               IF(DEM_5_PTR(I) > 0) THEN
                  CALL INT_INDEX_SORT(DEM_5_PTR(I),DEM_5_REC_POS,I,
     +                                            1,DEM_5_UNIQUE_VALUES)
                  IF(DIMENSION_TYPE(5) == 'N') THEN  ! AN NUMBER
                     CHAR_NUM = DEM_5_NAME_STORAGE(I)
                     DIM_VALUE = INT(REAL_VALUE)
                     WRITE(NUM_INDEX_VALUE,'(I4)') DIM_VALUE
                     WRITE(10) NUM_INDEX_VALUE,
     +               (RECORD_LENGTH*DEM_5_REC_POS(I,J),J=1,DEM_5_PTR(I))
                  ELSE
                     WRITE(10)
     +                     DEM_5_NAME_STORAGE(I)(1:DIMENSION_LENGHT(5)),
     +                    (RECORD_LENGTH*DEM_5_REC_POS(I,J),
     +                                                 J=1,DEM_5_PTR(I))
                  ENDIF
               ENDIF   
            ENDDO
            CLOSE(10)
            CALL TIMER(CURRENT_TIME)
            REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100
c            WRITE(6,*) trim(DIMENSION_NAME(5)),
c     +                  ' index file finished in ',REAL_TIME,' seconds.'
            DEALLOCATE(DEM_5_PTR,DEM_5_REC_POS,DEM_5_NAME_STORAGE)
         ENDIF
C
         IF(DIMENSIONS >= 6) THEN
            START_TIME = CURRENT_TIME
            INDEX_EXT = ReportCode//'6'
            CALL INDEXING_FILE_HEADER_INFO(StudyName,
     +                                     INDEX_EXT,
     +                                     DEM_6_UNIQUE_VALUES,
     +                                     1,
     +                                     DEM_6_UNIQUE_VALUES,   
     +                                     DEM_6_PTR,
     +                                     STUDY_DIRECTORY)
            DO I = 1, DEM_6_UNIQUE_VALUES
               IF(DEM_6_PTR(I) > 0) THEN
                  CALL INT_INDEX_SORT(DEM_6_PTR(I),DEM_6_REC_POS,I,
     +                                            1,DEM_6_UNIQUE_VALUES)
                  IF(DIMENSION_TYPE(6) == 'N') THEN  ! AN NUMBER
                     CHAR_NUM = DEM_6_NAME_STORAGE(I)
                     DIM_VALUE = INT(REAL_VALUE)
                     WRITE(NUM_INDEX_VALUE,'(I4)') DIM_VALUE
                     WRITE(10) NUM_INDEX_VALUE,
     +               (RECORD_LENGTH*DEM_6_REC_POS(I,J),J=1,DEM_6_PTR(I))
                  ELSE
                     WRITE(10)
     +                     DEM_6_NAME_STORAGE(I)(1:DIMENSION_LENGHT(6)),
     +                    (RECORD_LENGTH*DEM_6_REC_POS(I,J),
     +                                                 J=1,DEM_6_PTR(I))
                  ENDIF
               ENDIF   
            ENDDO
            CLOSE(10)
            CALL TIMER(CURRENT_TIME)
            REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100
c            WRITE(6,*) trim(DIMENSION_NAME(6)),
c     +                 ' index file finished in ',REAL_TIME,' seconds.'
            DEALLOCATE(DEM_6_PTR,DEM_6_REC_POS,DEM_6_NAME_STORAGE)
         ENDIF
C
         IF(DIMENSIONS >= 7) THEN
            START_TIME = CURRENT_TIME
            INDEX_EXT = ReportCode//'7'
            CALL INDEXING_FILE_HEADER_INFO(StudyName,
     +                                     INDEX_EXT,
     +                                     DEM_7_UNIQUE_VALUES,
     +                                     1,
     +                                     DEM_7_UNIQUE_VALUES,   
     +                                     DEM_7_PTR,
     +                                     STUDY_DIRECTORY)
            DO I = 1, DEM_7_UNIQUE_VALUES
               IF(DEM_7_PTR(I) > 0) THEN
                  CALL INT_INDEX_SORT(DEM_7_PTR(I),DEM_7_REC_POS,I,
     +                                            1,DEM_7_UNIQUE_VALUES)
                  IF(DIMENSION_TYPE(7) == 'N') THEN  ! AN NUMBER
                     CHAR_NUM = DEM_7_NAME_STORAGE(I)
                     DIM_VALUE = INT(REAL_VALUE)
                     WRITE(NUM_INDEX_VALUE,'(I4)') DIM_VALUE
                     WRITE(10) NUM_INDEX_VALUE,
     +               (RECORD_LENGTH*DEM_7_REC_POS(I,J),J=1,DEM_7_PTR(I))
                  ELSE
                     WRITE(10)
     +                     DEM_7_NAME_STORAGE(I)(1:DIMENSION_LENGHT(7)),
     +                    (RECORD_LENGTH*DEM_7_REC_POS(I,J),
     +                                                 J=1,DEM_7_PTR(I))
                  ENDIF
               ENDIF   
            ENDDO
            CLOSE(10)
            CALL TIMER(CURRENT_TIME)
            REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100
c            WRITE(6,*) trim(DIMENSION_NAME(7)),
c     +                 ' index file finished in ',REAL_TIME,' seconds.'
            DEALLOCATE(DEM_7_PTR,DEM_7_REC_POS,DEM_7_NAME_STORAGE)
         ENDIF
C
         CALL TIMER(PROCESS_END_TIME)
         REAL_TIME = FLOAT(PROCESS_END_TIME-PROCESS_START_TIME)/100.
         IF(REAL_TIME <= 60.) THEN
            WRITE(MESSAGE_TEXT,'(A,A,A,F6.3,A)')
     +          ' '//ReportCode,"-",trim(ReportName)//' took ',
     +          REAL_TIME,' seconds to index.'
         ELSE
            REAL_TIME = REAL_TIME/60.
            WRITE(MESSAGE_TEXT,'(A,A,A,F6.3,A)')
     +          ' '//ReportCode,"-",trim(ReportName)//' took ',
     +                                    REAL_TIME,' minutes to index.'
         ENDIF
!         CALL Delete_scroll_lines(window=handle,start_line=LineToDelete)
         CALL write_scroll_line(window = handle,
     +                                      text = trim(MESSAGE_TEXT))
         DEALLOCATE(DIMENSION_NAME,
     +              DIMENSION_TYPE,
     +              DIMENSION_LENGHT)
         INDEX_THIS_DETAILED_REPORT = .TRUE.
      RETURN
      END
C***********************************************************************
      SUBROUTINE INDEXING_FILE_HEADER_INFO(StudyName,
     +                                     FILE_EXTENSION,
     +                                     UNIQUE_VALUES,
     +                                     START_LOOP,
     +                                     END_LOOP,   
     +                                     NUM_OF_EACH_VALUE,
     +                                     STUDY_DIRECTORY)
C***********************************************************************
C
      INTEGER START_LOOP,END_LOOP,I
      INTEGER*2 UNIQUE_VALUES,HEADER_LENGHT
      INTEGER NUM_OF_EACH_VALUE(START_LOOP:END_LOOP)
      CHARACTER*5 StudyName
      CHARACTER*3 FILE_EXTENSION
      CHARACTER*256 FILE_IN
      CHARACTER (LEN=*) :: STUDY_DIRECTORY
C
         FILE_IN = 'MGX'//trim(StudyName)//'.'//FILE_EXTENSION
         IF(trim(STUDY_DIRECTORY) /= ' ') FILE_IN = 
     +                                  trim(STUDY_DIRECTORY)//FILE_IN
         OPEN(10,FILE=FILE_IN,ACCESS="TRANSPARENT",STATUS="REPLACE")
         HEADER_LENGHT = 4*(UNIQUE_VALUES + 1)
         WRITE(10) HEADER_LENGHT,UNIQUE_VALUES
C
         DO I = START_LOOP, END_LOOP
            IF(NUM_OF_EACH_VALUE(I) > 0) WRITE(10) NUM_OF_EACH_VALUE(I)
         ENDDO
      RETURN
      END
C***********************************************************************               
!
      SUBROUTINE INT_INDEX_SORT(ISUP,A,I_POS,SIZE1,SIZE2)
!      
C***********************************************************************               
! SORTS ISUP ITEMS INTO INCREASING ORDER BASED ON VALUES IN (1-BASED) ARRAY A;
! SHELL SORT ADAPTED FROM THE SOURCE ON PAGE 110 OF
! SOFTWARE TOOLS IN PASCAL, BY KERNIGHAN & PLAUGER, ADDISON-WESLEY 1981
      INTEGER SIZE1,SIZE2
      INTEGER ISUP,I,J,K,M,GAP,A(SIZE1:SIZE2,*)
      INTEGER I_POS

      GAP = ISUP/2
      DO WHILE(GAP > 0)
         DO I = GAP, ISUP
            J = I-GAP
            DO WHILE(J>0)
               K=J+GAP
               IF(A(I_POS,J) <= A(I_POS,K)) THEN 
                  J = 0 ! BREAK THE WHILE LOOP (ASSIGN J=-1 FOR 0-BASED ARRAYS)
               ELSE ! INTERCHANGE THE ARRAY VALUES
                  M = A(I_POS,J)    
                  A(I_POS,J) = A(I_POS,K)
                  A(I_POS,K) = M
               END IF
               J = J - GAP
            END DO
         END DO
         GAP = GAP/2
      END DO
      RETURN
      END ! SUBROUTINE INT_SORT
C***********************************************************************               
!
      SUBROUTINE INT_SINGLE_DIM_SORT(ISUP,A)
!      
C***********************************************************************               
! SORTS ISUP ITEMS INTO INCREASING ORDER BASED ON VALUES IN (1-BASED) ARRAY A;
! SHELL SORT ADAPTED FROM THE SOURCE ON PAGE 110 OF
! SOFTWARE TOOLS IN PASCAL, BY KERNIGHAN & PLAUGER, ADDISON-WESLEY 1981
      INTEGER ISUP,I,J,K,M,GAP,A(ISUP)

      GAP = ISUP/2
      DO WHILE(GAP > 0)
         DO I = GAP, ISUP
            J = I-GAP
            DO WHILE(J > 0)
               K=J+GAP
               IF(A(J) <= A(K)) THEN 
                  J = 0 ! BREAK THE WHILE LOOP (ASSIGN J=-1 FOR 0-BASED ARRAYS)
               ELSE ! INTERCHANGE THE ARRAY VALUES
                  M = A(J)    
                  A(J) = A(K)
                  A(K) = M
               END IF
               J = J - GAP
            END DO
         END DO
         GAP = GAP/2
      END DO
      RETURN
      END ! SUBROUTINE
C***********************************************************************
      SUBROUTINE ArraySizeIncrease(DEM_NAME,DEM_PTR,TEMP_SIZE)
C***********************************************************************
           ALLOCATABLE DEM_NAME(:)
           CHARACTER*(*) DEM_NAME
           INTEGER (KIND=4), ALLOCATABLE :: DEM_PTR(:)
           CHARACTER (LEN=40), ALLOCATABLE :: TEMP_CHR(:)
           INTEGER (KIND=4), ALLOCATABLE :: TEMP_I4(:)
           INTEGER (KIND=4) :: TEMP_SIZE
C           TEMP_SIZE = SIZE(DEM_NAME)
           ALLOCATE(TEMP_I4(TEMP_SIZE))
           TEMP_I4(1:TEMP_SIZE) = DEM_PTR(1:TEMP_SIZE)
           DEALLOCATE(DEM_PTR)
           ALLOCATE(DEM_PTR(1.2*TEMP_SIZE))
           DEM_PTR = 0
           DEM_PTR(1:TEMP_SIZE) = TEMP_I4(1:TEMP_SIZE)
           DEALLOCATE (TEMP_I4)


           ALLOCATE(TEMP_CHR(TEMP_SIZE))
           TEMP_CHR = ""
           TEMP_CHR(1:TEMP_SIZE) = DEM_NAME(1:TEMP_SIZE)
           DEALLOCATE(DEM_NAME)
           ALLOCATE(DEM_NAME(1.2*TEMP_SIZE))
           DEM_NAME = ""
           DEM_NAME(1:TEMP_SIZE) = TEMP_CHR(1:TEMP_SIZE)
           DEALLOCATE (TEMP_CHR)

           RETURN
        END ! SUBROUTINE
