subroutine interp1ez_s(X, Y, XI, YI)
real, intent(in) :: X(:), Y(:)
real, intent(in) :: XI
real, intent(out) :: YI

    real :: XX(1), YY(1)

    XX(1) = XI
    call interp1ez_arr(X, Y, XX, YY, .true.)
    YI = YY(1)
end subroutine interp1ez_s


subroutine interp1ez_arr(X, Y, XI, YI, sorted)
real, intent(in) :: X(:), Y(:), XI(:)
real, intent(out) :: YI(:)
logical, intent(in) :: sorted
!
! 1-D piece-wise linear interpolation, easy version.
! Linearly interpolate curve defined by (X, Y) at
! locations specified by XI, returning the interpolated values in YI.
! Out-of-rangge XI values are interpolated linearly as well.
!
! Elements in X must be in strict ascending order
! and this is not checked.
!
! <sorted> refers to the elements in <XI>.
!
! It is okay to pass the same array as both <XI> and <YI>.
!
! Zepu Zhang, 23 Feb 2006.

    integer :: n
    real :: slope
    integer :: i, j, k

    n = size(X)
    j = 1

    do i = 1, size(XI) 
        if (.not.sorted) j = 1

        k = bfind(X(j : n), XI(i))
        k = j - 1 + k

        if (k == 0) then
            j = 1
        else if (k == n) then
            j = k - 1
        else
            j = k
        end if

        slope = (Y(j) - Y(j + 1)) / (X(j) - X(j + 1))
        YI(i) = Y(j) + (XI(i) - X(j)) * slope

        if (k == n) then
            if (sorted) then
                do k = i + 1, size(XI)
                    YI(k) = Y(j) + (XI(k) - X(j)) * slope
                end do
                exit
            else
                j = k
            end if
        end if
    end do

end subroutine interp1ez_arr

