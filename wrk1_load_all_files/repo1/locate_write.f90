module locate_write
implicit none
contains
subroutine mlr(lt, tp, screen_message, version_message, someint)
integer*2, intent(in) :: lt, tp, someint
character*512 :: screen_message
character*256 :: version_message

    call mg_locate_write(lt, tp, trim(screen_message), trim(version_message), someint)
    
    
end subroutine mlr
end module locate_write