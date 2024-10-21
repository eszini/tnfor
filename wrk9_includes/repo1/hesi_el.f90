module hesi_el
use hesi_gendecs
implicit none
integer (kind=2) :: hesi_el_unit_id_num(max_el_units)
contains
      function get_hesi_el_unit_id_num(el_unit_no)
         integer (kind=2) :: get_hesi_el_unit_id_num
         integer (kind=2) :: el_unit_no
         get_hesi_el_unit_id_num = hesi_el_unit_id_num(el_unit_no)
      return
      end function get_hesi_el_unit_id_num
end module hesi_el