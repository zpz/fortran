! This group of functions are used when a function-returned array needs
! to be subscripted in the expression.
!
! Need to make it more general such that different data types and array shapes
! are handled automatically.
!
! Zepu Zhang, 16 Feb 2006.

function array_elements_i(arr, idx)
    integer, dimension(:), intent(in) :: arr
    integer, intent(in) :: idx
    integer  :: array_elements_i

    array_elements_i = arr(idx)
end function array_elements_i

function array_elements_iarr(arr, idx)
    integer, dimension(:), intent(in) :: arr
    integer, dimension(:), intent(in) :: idx
    integer, dimension(size(idx)) :: array_elements_iarr

    array_elements_iarr = arr(idx)
end function array_elements_iarr


function array_elements_r(arr, idx)
    real, dimension(:), intent(in) :: arr
    integer, intent(in) :: idx
    real  :: array_elements_r

    array_elements_r = arr(idx)
end function array_elements_r

function array_elements_rarr(arr, idx)
    real, dimension(:), intent(in) :: arr
    integer, dimension(:), intent(in) :: idx
    real, dimension(size(idx)) :: array_elements_rarr

    array_elements_rarr = arr(idx)
end function array_elements_rarr


function array_elements_l(arr, idx)
    logical, dimension(:), intent(in) :: arr
    integer, intent(in) :: idx
    logical :: array_elements_l

    array_elements_l = arr(idx)
end function array_elements_l

function array_elements_larr(arr, idx)
    logical, dimension(:), intent(in) :: arr
    integer, dimension(:), intent(in) :: idx
    logical, dimension(size(idx)) :: array_elements_larr

    array_elements_larr = arr(idx)
end function array_elements_larr

