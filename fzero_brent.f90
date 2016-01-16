!! Root finding by the Brent's method.
!! Copied from Numerical Recipes, Vol. 1 (Fortran 77), 2nd ed., 1992,
!! with mechanical changes.
!!
!! I added a mechanism that allows the function to take
!! auxiliary arguments, besides X, in a real array.
!! As the target function is almost always a cumstom one,
!! it should internally assume the content of the parameter array
!! that is passed on by the root finding routine.
!!
!! Renamed the function from 'zbrent' to 'fzero_brent'.
!!
!! Zepu Zhang, March 2006.


real function fzero_brent(func, x1, x2, nmaxiter, args)
interface
    real function func(x, args)
        real, intent(in) :: x
        real, intent(inout), optional :: args(:)
    end function func
end interface
real, intent(in) :: x1, x2
integer, intent(in), optional :: nmaxiter
real, intent(inout), optional :: args(:)
! Using Brents method, find the root of a function func known to lie between x1 and x2.
! The root, returned as fzero_brent, will be refined until its accuracy is tol.
! Parameters: Maximum allowed number of iterations, and machine floating-point precision.

    real, parameter :: eps = 3.e-8
    real, parameter :: tol = epsilon(0.)
    integer :: itmax
    integer iter
    real a, b, c, d, e, fa, fb, fc, p, q, r,  s, tol1, xm

    if (present(nmaxiter)) then
        itmax = nmaxiter
    else
        itmax = 100
    end if

    fzero_brent = 0.

    a = x1
    b = x2
    fa = func(a, args)
    fb = func(b, args)

    if ((fa > 0. .and. fb > 0.) .or. (fa < 0. .and. fb < 0.)) then
        print *, 'Error in <fzero_brent>: ', &
            'Root must be bracketed by the initial input. ',&
            'Function values at boundaries: ', fa, ' ', fb
        if (abs(fa) < abs(fb)) then
            fzero_brent = x1
        else
            fzero_brent = x2
        end if
        return
    end if

    c = b
    fc = fb

    do iter = 1, itmax
        if ((fb > 0. .and. fc > 0.) .or. (fb < 0. .and. fc < 0.)) then
            c = a            ! Rename a, b, c and adjust bounding interval d.
            fc = fa
            d = b - a
            e = d
        end if

        if (abs(fc) < abs(fb)) then
            a = b
            b = c
            c = a
            fa = fb
            fb = fc
            fc = fa
        end if

        tol1 = (2. * eps + 0.5 * tol) * abs(b)     ! Convergence check.
        xm = .5 * (c - b)
        if (abs(xm) <= tol1 .or. fb == 0.) then
            fzero_brent = b
            return
        end if

        if(abs(e) >= tol1 .and. abs(fa) > abs(fb)) then
            s = fb / fa        ! Attempt inverse quadratic interpolation.
            if (a .eq. c) then
                p = 2. * xm * s
                q = 1. - s
            else
                q = fa / fc
                r = fb / fc
                p = s * (2. * xm * q * (q - r) - (b - a) * (r - 1.))
                q = (q - 1.) * (r - 1.) * (s - 1.)
            end if

            if(p > 0.) q = -q  ! Check whether in bounds.
            p = abs(p)
            if(2. * p  <  min(3. * xm * q - abs(tol1 * q), abs(e * q))) then
                e = d          ! Accept interpolation.
                d = p / q
            else
                d = xm         ! Interpolation failed, use bisection.
                e = d
            end if
        else                   ! Bounds decreasing too slowly, use bisection.
            d = xm
            e = d
        end if

        a = b                  ! Move last best guess to a.
        fa = fb
        if(abs(d)  >  tol1) then    ! Evaluate new trial root.
            b = b + d
        else
            b = b + sign(tol1, xm)
        end if

        fb = func(b, args)
    enddo

    print *, 'Warning in <fzero_brent>: Maximum number of iterations exceeded.'
    fzero_brent = b

end function fzero_brent


