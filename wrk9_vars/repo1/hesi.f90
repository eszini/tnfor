module hesi
use hesi_gendecs
implicit none
integer (kind=8) :: hesi_unit_id_num(max_cl_units)
contains

      function get_hesi_unit_id_num(r_nunits,r_hesi_second_unit_id_num)
         integer (kind=2) :: get_hesi_unit_id_num
         integer :: r_nunits, r_hesi_second_unit_id_num
         integer (kind=8) :: temp_i8
         if(hesi_unit_id_num(r_nunits) <= 999999) then
            get_hesi_unit_id_num = real(hesi_unit_id_num(r_nunits))
            r_hesi_second_unit_id_num = 0.
         else
            temp_i8 = hesi_unit_id_num(r_nunits)/1000000
            r_hesi_second_unit_id_num = real(temp_i8)
            get_hesi_unit_id_num = &
               int(real(hesi_unit_id_num(r_nunits)-1000000*temp_i8),2)
         endif
      end function get_hesi_unit_id_num

end module hesi