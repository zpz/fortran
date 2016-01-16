! Use these routines until an IEEE module is implemented
! by the compiler.
!
! Create +Inf with 1./0.
!        -Inf with -1./0.
!         NaN with 0./0.
!
! To check 'Inf' or 'Nan' of a complex type, 
! check its real and imaginary components separately.

elemental logical function ieee_is_finite(val)
real, intent(in) :: val

    ieee_is_finite = (val >= -huge(val) .and. val <= huge(val))

end function ieee_is_finite

elemental logical function ieee_isnan(val)
real, intent(in) :: val

    ieee_isnan = (val /= val)

end function ieee_isnan

