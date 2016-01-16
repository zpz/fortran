function cbind(a, b)
real, intent(in) :: a(:)
real, intent(in) :: b(size(a))
real :: cbind(size(a), 2)
! Juxtapose to columns side by size.
! Analogous to R functions cbind() and rbind().
!
! Zepu Zhang, 24 Feb 2006.

	cbind(:, 1) = a
	cbind(:, 2) = b
end function

