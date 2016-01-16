! Zepu Zhang, 20 Feb 2006.

function random_int(n)
integer, intent(in) :: n
integer :: random_int
! Return a random integer between 1 and n, inclusively.

    real :: x

    if (n == 0) then
        random_int = 0
        return
    end if

    call random_number(x)
    random_int = sign(1, n) + int(x * n)

end function


function random_int_arr(n, m, replace)
    integer, intent(in) :: n, m
    logical, intent(in), optional :: replace
    integer, dimension(m) :: random_int_arr

! Randomly sample <m> integers between 1 and <n>, inclusively,
! and put them in the first <m> spots of the receiving array.
! If the receiving array has more than <m> elements,
! the elements beyond index <m> are untouched.
!
! If <replace> is .true., or missing, numbers can be sampled repeatedly.
!
! If <replace> if .false., and m > n,
! then the first <n> spots of the receiving array 
! are filled with a permutation of 1-n,
! and the rest of the receiving array is untouched.
!
! It would have been handy if we can use this function like this:
!
!   integer pp(20)
!   pp = ...
!   print *, pp(random_index(20, 10)) 
!
! but, alas, the (current) compiler does not allow this.
! Neither is the returned array allowed to be used directly as argument
! to another procedure.
! It has to be assigned to an array,
! but it can appear in an expression, which is in turn assigned to an array.
!
! This restricts the would-be most convenient use of this function.
! I decided to make it a function instead of a subroutine mainly because
! I have made "random_sample" a function.
!
! Zepu Zhang, 15 Feb 2006.

    integer :: i, j, k
    real :: wk
    integer, dimension(n) :: intarr
    real, dimension(m) :: realarr

    random_int_arr = 0

    if (present(replace) .and. .not. replace) then
        if (m > n) then
            print *, 'Warning in <random_int_arr>: Trying to sample more than the available number ', &
                'when replacement is prihibited. ', &
                'Only as many as available are sampled.'
        end if

        intarr = (/ (i, i = 1, n) /)
        do i = n, n - min(n, m) + 1, -1
            call random_number(wk)
            j = 1 + int(i * wk)
            random_int_arr(n - i + 1) = intarr(j)
            if (j < i) then
                k = intarr(i)
                intarr(i) = intarr(j)
                intarr(j) = k
            end if
        end do
    else
        call random_number(realarr)
        random_int_arr(1 : m) = 1 + int(realarr * n)
    end if

end function  random_int_arr


function random_sample(source, n, replace)
    real, dimension(:), intent(in) :: source 
    integer, intent(in) :: n
    logical, intent(in), optional :: replace
    real, dimension(n) :: random_sample

! Randomly sample <n> elements of <source> 
! and put them at the beginning of the receiving array.
!
! If <replace> is .true., or missing, items can be sampled repeatedly.
!
! If <replace> if .false., and n > size(source), 
! then the first size(source) spots of the receiving array
! are filled with a permutation of <source>,
! and the later part of the receiving array is untouched.
!
! The size of the receiving array must be >= n.
! If it is greater the <n>, its elements after index <n> are untouched.
!
! The returned array can not be used as argument to another procedure.
! It has to be assigned to a receiving array;
! but it can appear in an expression that is assigned to an array.
!
! One reason that convinced me to make this a function instead of
! a subroutine is that we can use this real-valued function
! to sample an integer array:
!
!     integer a(20), b(10)
!     a = ...
!     b = int(random_sample(real(a), 10))
!
! Zepu Zhang, 15 Feb 2006.

    integer, dimension(n) :: idx

    if (present(replace) .and. .not. replace .and. n > size(source)) then
        print *, 'Warning in <random_sample>: Trying to sample more than the available number ', &
            'when replacement is prohibited. ', &
            'Only as many as available are sampled.'

        random_sample(1 : size(source)) = source(idx(1 : size(source)))
    else
        idx = random_int_arr(size(source), n, replace)
        random_sample(1 : n) = source(idx)
    end if

end function random_sample


function random_unif(a, b)
    real, intent(in), optional :: a, b
    real :: random_unif

! Draw a random number from the uniform distribution on [0, 1)
! and transform it onto [a, b).
!
! It does not matter which of <a> and <b> is bigger.
! They correctly determine the interval.
!
! Although both <a> and <b> are options, usually
! both are present or both are missing.
!
! The basic motivation for this is to make it possible to generate
! one single number and use it in an array subscript or as
! a procedure parameter, for example:
!
!   integer x(10)
!   ... x(1 + int(10 * rand()) ...
!   call proc(rand(3, 5.8), ...)
!
! Zepu Zhang, 19 Feb 2005.

    call random_number(random_unif)

    if (present(a)) then
        if (present(b)) then
           random_unif = a + random_unif * (b - a) 
        else
           random_unif = a + random_unif * (1.0 - a) 
        end if
    else
        if (present(b)) then
           random_unif = random_unif * b 
        end if
    end if

end function random_unif

