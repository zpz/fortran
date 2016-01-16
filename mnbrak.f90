! 1-D function bracketing to find an interfal in which the function
! achieves a min value.
! This routine is usually called in preparation for a minimization routine
! that requires a search interval to start with.
!
! Copied from the Numerical Recipes in Fortran 77, 2nd ed., 1992, Chapter 10,
! with mechanical changes.
!
! I added a mechanism for passing additional arguments in a real array 
! to the target function.
! <args> is declared 'intent(inout)' for flexibility,
! but within the target function it may be declared more restricted.
!
! Zepu Zhang, March 2006.


subroutine mnbrak(func, ax, bx, cx, fa, fb, fc, args)
interface
    real function func(x, args)
        real, intent(in) :: x
        real, intent(inout) :: args(:)
    end function func
end interface
real, intent(inout) :: ax, bx
real, intent(out) :: cx, fa, fb, fc
real, intent(inout), optional :: args(:)
! Given a function func, and given distinct initial points ax and bx, 
! this routine searches in the downhill direction 
! (defined by the function as evaluated at the initial points) and
! returns new points ax, bx, cx that bracket a minimum of the function.
! Also returned are
! the function values at the three points, fa, fb, and fc.

    real GOLD,GLIMIT,TINY
    parameter (GOLD = 1.618034, GLIMIT = 100., TINY = 1.e-20)
        ! Parameters: GOLD is the default ratio by which successive intervals
        ! are magnified; 
        ! GLIMIT is the maximum magnification allowed for a parabolic-fit step.

    real dum,fu,q,r,u,ulim

    fa = func(ax, args)
    fb = func(bx, args)

    if(fb .gt. fa)then
        ! Switch roles of a and b so that we can go downhill 
        ! in the direction from a to b.
        dum = ax
        ax = bx
        bx = dum
        dum = fb
        fb = fa
        fa = dum
    endif

    cx = bx + GOLD * (bx - ax) ! First guess for c.
    fc = func(cx, args)

    do
        ! "do while": keep returning here until we bracket.

        if (fb .lt. fc) exit

        ! Compute u by parabolic extrapolation from a, b, c.
        ! TINY is used to prevent any possible division by zero.
        r = (bx - ax) * (fb - fc)
        q = (bx - cx) * (fb - fa) 
        u = bx - ((bx - cx) * q - (bx - ax) * r) &
            / (2. * sign(max(abs(q - r), TINY), q - r))
        ulim = bx + GLIMIT * (cx - bx)
            ! We won't go farther than this.

        ! Test various possibilities:
        if ((bx - u) * (u - cx) .gt. 0.) then
            ! Parabolic u is between b and c: try it.
            fu = func(u, args)
            if (fu .lt. fc) then ! Got a minimum between b and c.
                ax = bx
                fa = fb
                bx = u
                fb = fu
                return
            else if (fu .gt. fb) then ! Got a minimum between between a and u.
                cx = u
                fc = fu
                return
            end if
            u = cx + GOLD * (cx - bx)
                ! Parabolic fit was no use. Use default magnification.
            fu = func(u, args)
        else if ((cx - u) * (u - ulim) .gt. 0.) then
            ! Parabolic fit is between c and its allowed limit

            fu = func(u, args)
            if (fu .lt. fc) then
                bx = cx
                cx = u
                u = cx + GOLD * (cx - bx)
                fb = fc
                fc = fu
                fu = func(u, args)
            end if
        else if ((u - ulim) * (ulim - cx) .ge. 0.) then
            ! Limit parabolic u to maximum allowed value

            u = ulim
            fu = func(u, args)
        else ! Reject parabolic u, use default magnification.
            u = cx + GOLD * (cx - bx)
            fu = func(u, args)
        end if

        ax = bx ! Eliminate oldest point and continue.
        bx = cx
        cx = u
        fa = fb
        fb = fc
        fc = fu
    end do

end subroutine mnbrak

