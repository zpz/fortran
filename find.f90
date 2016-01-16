function findfirst(mask)
    logical, dimension(:) :: mask
    integer :: findfirst

! Find the index of the first occurrence of .true.
! in the one-dimensional mask array.
! Return 0 if no .true. value exists.
!
! Zepu Zhang, 12 Feb 2006.

    integer :: i

    findfirst = 0
    do i = 1, size(mask)
        if (mask(i)) then
            findfirst = i
            exit
        end if
    end do
end function


function findlast(mask)
    logical, dimension(:) :: mask
    integer :: findlast

! Find the index of the last occurrence of .true.
! in the one-dimensional mask array.
! Return 0 if no .true. value exists.
!
! Zepu Zhang, 12 Feb 2006.

    integer :: i

    findlast = 0
    do i = size(mask), 1, -1
        if (mask(i)) then
            findlast = i
            exit
        end if
    end do
end function


integer function bfind(X, t) result(idx)
real, intent(in) :: X(:)
real, intent(in) :: t
! Find the location of a number in a series by bi-section.
!
! Values in X are in ascending order.
!
! This function returns index <idx> such that
!   X(idx) <= t < X(end)
!
! <idx> = 0 means t < X(1)
! <idx> = size(X) suggests t >= X(end)
!
! Zepu Zhang, 23 Feb 2006.

    integer n
    integer :: i, j, k

    n = size(X)

    if (t < X(1)) then
        idx = 0
        return
    end if

    if (t >= X(n)) then
        idx = n
        return
    end if

    i = 1
    j = n

    do
        if (t >= X(i) .and. t < X(i + 1)) then
            idx = i
            exit
        end if

        k = (i + j) / 2
        if (t >= X(k)) then
            i = k
        else
            j = k
        end if
    end do

end function bfind
