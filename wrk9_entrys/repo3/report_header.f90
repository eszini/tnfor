module report_header
implicit none

! calculated once, before the end point or year loop
      character(len=130) :: banner_line,util_study_line
      character(len=60) :: study
      character(len=40) :: utility_name
      character(len=8) :: run_time
      character(len=8) :: run_date
! calculated for every new end point
      character*20 :: run_end_point
! calculated for every new year      
      character(len=25) :: forecast_year
      character(len=256) :: rpt_base_family_name,rpt_project_name

end module report_header