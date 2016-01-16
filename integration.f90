subroutine itgr_midpnt(g, lb, ub, s, reltol, nmaxiter, args)
interface
    real function g(x, args)
        real, intent(in) :: x
        real, dimension(:), intent(inout), optional :: args
    end function g
end interface
real, intent(in) :: lb, ub
real, intent(out) :: s
real, intent(in), optional :: reltol
integer, intent(in), optional :: nmaxiter
real, dimension(:), intent(inout), optional :: args

! The Extended Midpoint Rule for integration where
! the integral has sigularity at the limits of the integration interval:
!   int_{lb}^{ub} g(x) dx
!
! <g> is a real-valued function of x.
!
! <lb> and <ub> are integration limits.
! One or both boundaries can be singularity points.
!
! <s> is the result.
!
! <reltol> is relative tolerance. The procedure stops when
!   abs((s - sold)) / sold) < reltol
!
! <nmaxiter> is max number of iterations.
!
! Zepu Zhang
!   2004/11/26
!   2004/12/09
!   2006/02/22
!   2006/03/26

    real, parameter :: log3 = 1.0986  ! log(3)
    real :: h, ysum, sold, v, tol
    integer :: i, n, iloop, nloopmax
    real, dimension(:), allocatable :: x

    if (present(reltol)) then
        tol = reltol
    else
        tol = 0.001
    end if

    n = 100

    if (present(nmaxiter) .and. nmaxiter >= 20) then
        nloopmax = ceiling(log(real(nmaxiter) / n) / log3)
    else
        nloopmax = ceiling(log(100000. / n) / log3)
    end if
    allocate(x(3**nloopmax * n))

    h = (ub - lb) / n
    x(1 : n) = lb + h * (/ (i, i=1, n) /) - 0.5 * h

    ysum = 0.
    do i = 1, n 
        ysum = ysum + g(x(i), args)
    end do
    s = h * ysum

    do iloop = 1, nloopmax 
        h = h / 3.

        ysum = 0.
        do i = 1, n
            ysum = ysum + g(x(i) - h, args) + g(x(i) + h, args)
            x(n + i) = x(i) - h
            x(n + n + i) = x(i) + h
        end do
        n = 3 * n

        sold = s
        s = sold / 3. + h * ysum

        if (iloop > 4 .and. abs(s - sold) < abs(sold) * tol) exit
    end do

    if (iloop == nloopmax) then
        v = (s - sold) / sold * 100.
        if (v > 1) then
            print *, 'Warning <midpnt_args>: Maximum number of iterations reached ', &
                s, v
        end if
    end if

    deallocate(x)

end subroutine itgr_midpnt


subroutine itgr_trapzd(g, lb, ub, s, reltol, nmaxiter, args)
interface
    real function g(x, args)
        real, intent(in) :: x
        real, intent(inout), optional :: args(:)
    end function g
end interface
real, intent(in) :: lb, ub
real, intent(out) :: s
real, intent(in), optional :: reltol
integer, intent(in), optional :: nmaxiter
real, intent(inout), optional :: args(:)

! The Extended Trapezoidal Rule for integration where
! the integral is regular in the entire integration interval, i.e., no sigularities.
!
! <g> is a real-valued function to be integrated.
! Additional arguments for <g> is passed in through <args>
! in the "trapzd_args" version of the procedure.
!
! <lb> and <ub> are integration limits.
!
! <s> is the result.
!
! <reltol> is relative tolerance. The procedure stops when
!       abs((s - sold) / sold) < reltol
!
! <nmaxiter> is maximum number of iterations.
!
! Zepu Zhang
!  2004/09/02
!  2006/02/24
!  2006/03/26

    real, parameter :: log3 = 1.0986      ! log(3)
    real, allocatable :: x(:)
    real :: ysum, h, sold, tol
    integer :: n, nloopmax
    integer :: i, iloop


    if (present(reltol)) then
        tol = reltol
    else
        tol = 0.001
    end if

    n = 100;

    if (present(nmaxiter) .and. nmaxiter > 20) then
        nloopmax = ceiling(log(real(nmaxiter) / n) / log3)
    else
        nloopmax = ceiling(log(100000. / n) / log3)
    end if
    allocate(x(2**nloopmax * n))

    h = (ub - lb) / n
    x(1 : (n+1)) = lb + h * (/ (i, i = 0, n) /)

    ysum = 0.
    do i = 1, n + 1
        ysum = ysum + g(x(i), args)
    end do
    s = h * ysum

    do iloop = 1, nloopmax
        h = h / 2.

        ysum = 0.
        do i = 1, n
            ysum = ysum + g(x(i) + h, args)
        end do
        x((n + 1) : (n + n)) = x(1 : n) + h
        n = n + n

        sold = s
        s = sold / 2. + h * ysum
        s = 4. / 3. * s - sold / 3.

        if (iloop > 4 .and. abs(s - sold) < abs(sold) * tol) exit
    end do

    if (iloop == nloopmax) then
        print *, 'Warning in <trapzd_args>: Maximum number of iterations reached ', &
            s, (s - sold)/sold * 100
    end if

    deallocate(x)

end subroutine itgr_trapzd

