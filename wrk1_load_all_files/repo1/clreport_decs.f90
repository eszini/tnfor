module clreport_decs
implicit none
	  INTERFACE lwr
	  subroutine lwr(reportname, reportnum)
		implicit none
		character*256, intent(in) :: reportname
		integer, intent(in) :: reportnum
	  end subroutine lwr
	  end INTERFACE lwr
end module clreport_decs