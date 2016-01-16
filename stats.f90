subroutine ecdf(X, XYout, ordered)
real, intent(in) :: X(:)
real, intent(out), allocatable :: XYout(:, :)
logical, intent(in), optional :: ordered

! Piecewise linear empirical CDF.
! To do: 
!   1) return a stair-case like step function
!   2) specify X values where the CDF value are required.
!
! Zepu Zhang, 2 March 2006.

    real, dimension(size(X)) :: Xcopy
    integer :: n  ! number of unique elements
    integer :: i, j


    Xcopy = X
    n = size(X)
    if (.not.present(ordered) .or. .not.ordered) then
        call ssort(Xcopy, X, n, 1)
    end if

    n = n - count(Xcopy(1 : (n-1)) == Xcopy(2 : n))
    if (n < 2) then
        print *, '  Error in <ecdf>: Fewer than 2 distinct elements for CDF.'
        return
    end if

    allocate(XYout(n + 1, 2))
    ! The last row of <XYout> will be the largest element of <X>
    ! with CDF 1.0.
    ! The smallest element of <X>, on the second row of <XYout> will have a
    ! positive CDF.
    ! The first row of <XYout> will be extrapolated from the 2nd and 3rd rows
    ! to get a zero CDF point.


    n = size(Xcopy)
    XYout(2, 1) = Xcopy(1)
    XYout(2, 2) = 1.0
    j = 2
    do i = 2, n
        if (Xcopy(i) == Xcopy(i-1)) then
            XYout(j, 2) = XYout(j, 2) + 1.0
        else
            j = j + 1
            XYout(j, 1) = Xcopy(i)
            XYout(j, 2) = XYout(j-1, 2) + 1.0
        end if
    end do

    XYout(:, 2) = XYout(:, 2) / n
    
    ! Get the left bound by linear extrapolation.
    XYout(1, 2) = 0.
    XYout(1, 1) = (XYout(2, 1) * (XYout(3, 2) - XYout(1, 2)) &
        + XYout(3, 1) * (XYout(1, 2) - XYout(2, 2))) &
        / (XYout(3, 2) - XYout(2, 2))

end subroutine ecdf


subroutine moments(x, mn, var)
    real, dimension(:), intent(in) :: x
    real, intent(out) :: mn, var

! Calculate the mean and variance of a one-dimensional read vector.
!
! If only some elements of x are considered,
! filter the desired elements using 'pack' into
! array x before calling this function.
! In other words, this function does not provide
! a 'mask' parameter.
!
! Zepu Zhang, 09 Feb 2006.

    integer :: n

    n = size(x)
    mn = sum(x) / n
    var = sum((x - mn) ** 2) / (n - 1)
end subroutine


function quantile(x, q, sorted)
real, dimension(:), intent(in) :: x
real, intent(in) :: q
logical, intent(in), optional :: sorted
real :: quantile
!
! Zepu Zhang, 19 Feb 2006.

    real, dimension(size(x)) :: y
    real :: idx, h
    integer :: lo, hi

    if (q < 0 .or. q > 1) then
        print *, 'Error in <quantile>: Probability out of range [0, 1].'
        quantile = 0.
        return
    end if

    idx = 1. + (size(x) - 1.0) * q
    lo = floor(idx)
    hi = ceiling(idx)
    h = idx - lo

    if (present(sorted) .and. sorted) then
        quantile = (1.0 - h) * x(lo) + h * x(hi)
    else
        y = x
        call ssort(y, x, size(y), 1)
        quantile = (1.0 - h) * y(lo) + h * y(hi)
    end if

end function quantile


function quantile_arr(x, q, sorted)
real, dimension(:), intent(in) :: x
real, dimension(:), intent(in) :: q
logical, intent(in), optional :: sorted
real, dimension(size(q)) :: quantile_arr
!
! Zepu Zhang, 19 Feb 2006.

    real, dimension(size(x)) :: y
    real, dimension(size(q)) :: idx, h
    integer, dimension(size(q)) :: lo, hi
    integer :: i

    if (minval(q) < 0 .or. maxval(q) > 1) then
        print *, 'Error in <quantile_arr>: Probability out of range [0, 1].'
        quantile_arr = 0.
        return
    end if

    idx = 1. + (size(x) - 1.0) * q
    lo = floor(idx)
    hi = ceiling(idx)
    h = idx - lo

    if (present(sorted) .and. sorted) then
        do i = 1, size(q)
            quantile_arr(i) = (1.0 - h(i)) * x(lo(i)) + h(i) * x(hi(i))
        end do
    else
        y = x
        call ssort(y, x, size(y), 1)
        do i = 1, size(q)
            quantile_arr(i) = (1.0 - h(i)) * y(lo(i)) + h(i) * y(hi(i))
        end do
    end if

end function quantile_arr
