! 1-D function minimization routine by the method of Richard Brent.
! Copied from the Numerical Recipes in Fortran 77, 2nd ed., 1992, Chapter 10,
! with mechanical changes.
!
! I added a mechanism for passing additional arguments in a real array 
! to the target function 'f'.
! <args> is declared 'intent(inout)' for flexibility,
! but within the actural target function it could be declared more restricted.
!
! Renamed the function from 'brent' to 'fmin_brent'.
! Zepu Zhang, March 2006.



real function fmin_brent(f, ax, bx, cx, xmin, args)
real, intent(in) :: ax, bx, cx
real, intent(out) :: xmin
real, intent(inout), optional :: args(:)
interface
    real function f(x, args)
        real, intent(in) :: x
        real, intent(inout), optional :: args(:)
    end function f
end interface
! Given a function f, and given a bracketing triplet of abscissas ax, bx, cx 
! (such that bx is between ax and cx, and f(bx) is less than 
! both f(ax) and f(cx)), 
! this routine isolates the minimum to a fractional precision of about tol 
! using Brent's method.
! The abscissa of the minimum is returned as xmin, 
! and the minimum function value is returned as fmin_brent,
! the returned function value.
! Parameters: Maximum allowed number of iterations; golden ratio; 
! and a small number that protects against trying to 
! achieve fractional accuracy for a minimum that happens to be exactly zero.

    integer, parameter :: ITMAX = 100
    real, parameter :: CGOLD = 0.3819660
    real, parameter :: ZEPS = 1.0e-10
    real, parameter :: TOL = sqrt(epsilon(ax))

    integer iter
    real a, b, d, e, etemp, fu, fv, fw, fx, p, q, r
    real tol1, tol2, u, v, w, x, xm

    ! Initializations...
    a = min(ax, cx)
    b = max(ax, cx)
        ! a and b must be in ascending order, though the input
        ! abscissas need not be.
    v = bx
    w = v
    x = v
    e = 0.  ! This will be the distance moved on the step before last.
    fx = f(x, args)
    fv = fx
    fw = fx

    do iter = 1, ITMAX    ! Main program loop.

        xm = 0.5 * (a + b)
        tol1 = TOL * abs(x) + ZEPS
        tol2 = 2. * tol1
        if(abs(x - xm) .le. (tol2 - .5 * (b - a))) goto 3 ! Test for done here.

        if(abs(e) .gt. tol1) then ! Construct a trial parabolic fit.
            r = (x - w) * (fx - fv)
            q = (x - v) * (fx - fw)
            p = (x - v) * q - (x - w) * r
            q = 2. * (q - r)
            if(q .gt. 0.) p = -p
            q = abs(q)
            etemp = e
            e = d

            if(abs(p) .ge. abs(.5 * q * etemp) .or. p .le. q * (a - x) .or. p .ge. q * (b - x)) goto 1
            ! The above conditions determine the acceptability of the parabolic fit.
            ! Here it is o.k.:

            d = p/q  ! Take the parabolic step.
            u = x + d
            if(u - a .lt. tol2 .or. b - u .lt. tol2) d = sign(tol1, xm - x)
            goto 2 ! Skip over the golden section step.
        endif

1       if(x .ge. xm) then
            ! We arrive here for a golden section step, which we take
            ! into the larger of the two segments.
            e = a - x
        else
            e = b - x
        endif
        d = CGOLD * e ! Take the golden section step.

2       if(abs(d) .ge. tol1) then
            ! Arrive here with d computed either from
            ! parabolic fit, or from golden section.
            u = x + d
        else
            u = x + sign(tol1, d)
        endif

        fu = f(u, args)
            ! This is the one function evaluation per iteration,
            ! and now we have to decide what to do with our function
            ! evaluation. Housekeeping follows:
        if(fu .le. fx) then

            if(u .ge. x) then 
                a = x
            else
                b = x
            endif

            v = w
            fv = fw
            w = x
            fw = fx
            x = u
            fx = fu
        else
            if(u .lt. x) then
                a = u
            else
                b = u
            endif

            if(fu .le. fw .or. w .eq. x) then
                v = w
                fv = fw
                w = u
                fw = fu
            else if(fu .le. fv .or. v .eq. x .or. v .eq. w) then
                v = u
                fv = fu
            endif
        endif ! Done with housekeeping. Back for another iteration.
    enddo
    print *, 'Warning in <fmin_brent>: maximum iterations exceeded.'

3   xmin = x ! Arrive here ready to exit with best values.
    fmin_brent = fx

end function fmin_brent

