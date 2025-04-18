

      subroutine rw_main(command_line)! Required for RealWin
      USE realwin
      use logging
      use params
      use filename_tracker
      use screen_initialization
	  use startup_tasks
	  IMPLICIT NONE
	  CHARACTER (LEN=256):: command_line, tempcl
	  integer :: cl_len

      call read_command_line(command_line)
	  command_line=clData%command_line

        call write_log_entry( &
          "rw_main", "running rw-main.")


      call params_init
	  cl_len=len_trim(clData%command_line)

      call init_screen()
      call MIDAS_XP()

      end SUBROUTINE RW_Main
! ***********************************************************************
      FUNCTION RW_control(command, window, wparam, lparam)
      use end_routine, only: end_program, er_message
! ***********************************************************************
!
      USE realwin
      IMPLICIT NONE
      INTEGER trash
      LOGICAL :: logical_trash
      LOGICAL :: RW_control
      INTEGER, INTENT(IN) :: command, window, wparam, lparam
      INTEGER, PARAMETER :: idm_msg_file_exit = 1, &
                            idm_mg_about = 20
      LOGICAL(kind=1) :: MESSAGE,DEBUG_ON,NO_WARNING_WINDOW,WARNING_MESSAGES
      INTEGER(kind=2) :: INVERSE_VIDEO
      LOGICAL(kind=1) :: USER_TERMINATED
       character(len=25) :: build_num
      COMMON /ERRORS/ INVERSE_VIDEO,MESSAGE,DEBUG_ON,USER_TERMINATED
!
      select case (command)
         case (idm_msg_file_exit,idm_rw_file_exit)
            USER_TERMINATED = .TRUE.
            CALL ERROR_MESSAGE()
            er_message='Stop requested from msgold95RW SIID245'
            call end_program(er_message)
         case (idm_mg_about)
            trash = message_box (title="About", &
              text="MIDAS " // trim(BUILD_NUM()) // &
              " (c) 2024 Hitachi Energy")
         case default
! In the default case, recognize commands that RealWin knows how to process
! and diagnose commands that have no case defined.
            IF (realwin_knows_command (command)) THEN
          ! RealWin knows how to process this command
               RW_control = .FALSE.  ! Let RealWin handle it
               return
            ELSE
          ! This command apparently hasn't been implemented
               trash = message_box (title="default", &
                                    text="Command not implemented")
            END IF
      end select

      RW_control = .true. ! Let RealWin know we handled it already

      END function RW_Control
! ***********************************************************************
      SUBROUTINE STATUS_BAR_FUNCTIONS(R_Win_handle)
! ***********************************************************************
      USE realwin
      IMPLICIT NONE
      TYPE(status_t), SAVE, DIMENSION(8) :: status_bar
      INTEGER :: R_Win_handle
      CHARACTER(len=*) :: STATUS_TEXT, &
                    R_TOTAL_NUM_OF_END_POINTS_STR, &
                    STUDY_PERIOD_STR, &
                    SIM_MONTH_DAY
      CHARACTER (LEN=256) :: MIDAS_EXE_NAME
      CHARACTER (LEN=3) :: ENDPOINT_STR
      CHARACTER (LEN=5) :: YEAR_STR
      CHARACTER (LEN=4), SAVE :: TOTAL_NUM_OF_END_POINTS_STR
      INTEGER (KIND=4), SAVE :: status_bar_font,Win_handle
      INTEGER (KIND=2) :: END_POINTS,CURRENT_YEAR
      CHARACTER(len=25) :: BUILD_NUM
      REAL, DIMENSION(7) :: &
	  Status_Portion=(/.07,.20,.47,.045,.05,.12,.07/)
      LOGICAL (KIND=4) :: VOID_LOGICAL
      LOGICAL (KIND=1) :: SP_CAPEX_ACTIVE,SP_CAPEX_DISPLAY_ACTIVE
      CHARACTER (LEN=3) :: EXTENSION_PERIOD_STR
!
         Win_handle = R_Win_handle
      RETURN
      ENTRY RW_INITIALIZE_STATUS_LINE(STATUS_TEXT, &
                                      R_TOTAL_NUM_OF_END_POINTS_STR, &
                                      STUDY_PERIOD_STR, &
                                      SIM_MONTH_DAY)
         TOTAL_NUM_OF_END_POINTS_STR = R_TOTAL_NUM_OF_END_POINTS_STR
         call get_exe_name(MIDAS_EXE_NAME)
         CALL set_status_item (status_bar(1), portion=Status_Portion(1), &
                                                code = STATUS_TIME2)

         CALL set_status_item (status_bar(2), portion=Status_Portion(2), &
                 code = STATUS_USER_TEXT, text = trim(MIDAS_EXE_NAME))
         CALL set_status_item (status_bar(3), portion=Status_Portion(3), &
                code = STATUS_USER_TEXT)
         CALL set_status_item (status_bar(4), portion=Status_Portion(4), &
                code = STATUS_USER_TEXT, &
                text = '     ')
         CALL set_status_item (status_bar(5), portion=Status_Portion(5), &
                 code = STATUS_USER_TEXT, text = '     ')
         CALL set_status_item (status_bar(6), portion=Status_Portion(6), &
                 code = STATUS_USER_TEXT, &
                 text = TRIM(STATUS_TEXT)//' '//TRIM(STUDY_PERIOD_STR))
         CALL set_status_item (status_bar(7), portion=Status_Portion(7), &
                 code = STATUS_USER_TEXT)

         status_bar_font = &
                       select_font(typeface='ARIAL',point=8,bold=.true.)
         Call set_status_bar(status_bar=status_bar,update_flag=.true., &
                                                   font=status_bar_font)
      RETURN
      ENTRY RW_UPDATE_RUNTIME_MESSAGES(STATUS_TEXT)
         IF(.NOT. SP_CAPEX_DISPLAY_ACTIVE()) RETURN
         CALL set_status_item (status_bar(3), portion=Status_Portion(3), &
                 code = STATUS_USER_TEXT, text = trim(STATUS_TEXT))
         Call set_status_bar(status_bar=status_bar,update_flag=.true., &
                                                   font=status_bar_font)
      RETURN
      ENTRY RW_SET_ENDPOINTS_ON_STATUS(END_POINTS)
         IF(.NOT. SP_CAPEX_DISPLAY_ACTIVE()) RETURN
         WRITE(ENDPOINT_STR,'(I3)') END_POINTS
         CALL set_status_item (status_bar(4), portion=Status_Portion(4), &
                     code = STATUS_USER_TEXT, &
                     text = ENDPOINT_STR//'/'// &
                                    trim(TOTAL_NUM_OF_END_POINTS_STR))
         Call set_status_bar(status_bar=status_bar,update_flag=.true., &
                                                   font=status_bar_font)
      RETURN
      ENTRY RW_SET_YEAR_ON_STATUS(CURRENT_YEAR)
         IF(.NOT. SP_CAPEX_DISPLAY_ACTIVE()) RETURN
         WRITE(YEAR_STR,'(I4)') CURRENT_YEAR
         CALL set_status_item (status_bar(5), portion=Status_Portion(5), &
                     code = STATUS_USER_TEXT, &
                     text = YEAR_STR)
         Call set_status_bar(status_bar=status_bar,update_flag=.true., &
                                                   font=status_bar_font)
      RETURN
      ENTRY RW_UPDATE_STATUS_TIME(STATUS_TEXT)
         IF(.NOT. SP_CAPEX_DISPLAY_ACTIVE()) RETURN
         CALL set_status_item (status_bar(7), portion=Status_Portion(7), &
                 code = STATUS_USER_TEXT, text = trim(STATUS_TEXT))
         Call set_status_bar(status_bar=status_bar,update_flag=.true., &
                                                   font=status_bar_font)
      RETURN
      ENTRY RW_PROGRAM_LOCATION(STATUS_TEXT)
         IF(.NOT. SP_CAPEX_DISPLAY_ACTIVE()) RETURN
         CALL set_status_item (status_bar(2), portion=Status_Portion(2), &
                 code = STATUS_USER_TEXT, text = trim(STATUS_TEXT))
         Call set_status_bar(status_bar=status_bar,update_flag=.true., &
                                                   font=status_bar_font)
      RETURN
      ENTRY RW_PROCESS_MESSAGES()
         IF(.NOT. SP_CAPEX_DISPLAY_ACTIVE()) RETURN
         VOID_LOGICAL = PROCESS_MESSAGES()
      RETURN
      END SUBROUTINE
