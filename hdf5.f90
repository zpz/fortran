!! HDF5 utilities.

! Find out the approapriate type for an integer value in a file,
! which is to be read in.
function h5tget_file_int(val)
use hdf5
integer, intent(in) :: val
integer(hid_t) :: h5tget_file_int

    ! assuming radix(val) is 2
    if (digits(val) <= 31) then
        h5tget_file_int = H5T_STD_I32BE
    else
        h5tget_file_int = H5T_STD_I64BE
    end if

end function h5tget_file_int


! Find out the approapriate type for an integer value in the member,
! which is to be written out.
function h5tget_mem_int(val)
use hdf5
integer, intent(in) :: val
integer(hid_t) :: h5tget_mem_int

    h5tget_mem_int = H5T_NATIVE_INTEGER

end function h5tget_mem_int


function h5tget_file_real(val)
use hdf5
real, intent(in) :: val
integer(hid_t) :: h5tget_file_real

    ! assuming radix(val) is 2
    if (digits(val) <= 24) then
        h5tget_file_real = H5T_IEEE_F32BE
    else
        h5tget_file_real = H5T_IEEE_F64BE
    end if

end function h5tget_file_real


function h5tget_mem_real(val)
use hdf5
real, intent(in) :: val
integer(hid_t) :: h5tget_mem_real

    ! assuming radix(val) is 2
    if (digits(val) <= 24) then
        h5tget_mem_real = H5T_NATIVE_REAL
    else
        h5tget_mem_real = H5T_NATIVE_DOUBLE
    end if

end function h5tget_mem_real


function h5tget_file_char(val)
use hdf5
character(len=*), intent(in) :: val
integer(hid_t) :: h5tget_file_char

    h5tget_file_char = H5T_NATIVE_CHARACTER

end function h5tget_file_char


function h5tget_mem_char(val)
use hdf5
character(len=*), intent(in) :: val
integer(hid_t) :: h5tget_mem_char

    h5tget_mem_char = H5T_NATIVE_CHARACTER

end function h5tget_mem_char

