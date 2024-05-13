module index_lookup

use end_routine
implicit none
integer :: max_dicts=-1
integer, allocatable :: dicts(:)
character*256, allocatable :: dictnames(:)
integer, allocatable :: dictionary(:,:)
character*256, allocatable :: dictkeys(:,:)
integer :: last_available_dict=1

contains
subroutine dict_init()

    if(max_dicts==-1) then
		dictionary=-1

		last_available_dict=1
		max_dicts=50
		allocate(dicts(max_dicts))
		dicts=-1
		allocate(dictnames(max_dicts))
		dictnames=""
		allocate(dictkeys(max_dicts,1024))
		dictkeys=""
		allocate(dictionary(max_dicts, 1024))
		dictionary=-1
	endif
    
end subroutine dict_init

function get_entry_index(dictname, key)
integer :: get_entry_index, index
integer :: dictindex=-1
character(*) dictname, key
	get_entry_index=-1
	dictindex=get_dictindex(dictname)
	
	do index=1, ubound(dictkeys,2)
		if(dictkeys(dictindex, index)==trim(key)) then
			get_entry_index=index
			exit
		else if (trim(dictkeys(dictindex,index))=="") then
			exit ! no more entries
		endif
	enddo
	
end function get_entry_index

function get_new_entry_index(dictname, key)
character(*), intent(in) :: dictname, key
integer :: index
integer :: dictindex
integer :: elemindex
integer :: get_new_entry_index

	get_new_entry_index=-1
	dictindex=get_dictindex(dictname)
	do elemindex=1,ubound(dictionary, dictindex)
		if(dictionary(dictindex,elemindex)==-1) then
            dictionary(dictindex,elemindex)=elemindex
			get_new_entry_index=elemindex
			exit
		endif
		
	enddo
end function get_new_entry_index

function get_dictindex(dictname)
	integer :: index
	character*256 :: this_key
    character(*) :: dictname
    integer :: get_dictindex
	get_dictindex=-1
	do index=1, last_available_dict-1
		this_key=dictnames(index)
		if(trim(this_key)==trim(dictname)) then
			get_dictindex=index
			exit
		endif
	enddo
	
end function get_dictindex

function create_dict(name) 
	integer :: dictindex
	character(*) :: name
	integer :: create_dict

	dictindex=last_available_dict
	create_dict=dictindex
	last_available_dict=last_available_dict+1
	dictnames(dictindex)=trim(name)

end function  create_dict

function add_entry(dictname, key, value)
	integer :: add_entry, value
	integer :: dictindex
	integer :: dict_elemindex
    character(*) :: dictname, key
    
	add_entry=-1
    
	dictindex=get_dictindex(dictname)
	if(dictindex<1) then
		er_message="Cannot get dictionary index for " // trim(key) //". index_lookup:1"
		call end_program(er_message)
	endif
    dict_elemindex=get_entry_index(dictname, key)
    if(dict_elemindex==-1) then
		er_message="Cannot find entry in " // trim(dictname) // " for " // trim(key) // " index_lookup:2"
		call end_program(er_message)
    endif
	dict_elemindex=get_new_entry_index(dictname, key)
    
    add_entry=dict_elemindex
	dictkeys(dictindex,dict_elemindex)=trim(key)
    dictionary(dictindex,dict_elemindex)=value
    

end function add_entry

end module index_lookup
