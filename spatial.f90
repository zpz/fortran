! Simulate a Gaussian random process on the plane with given
! stationary covariance function.
! Unconditional, i.e. not constrained by measurements.
!
! See also: function 'sim.rf' of the R package 'fields' by Nychka et. al.
subroutine simul_gaussian(cov_func, cov_args, dx, dy, z)
use frand_single
    ! Link with -lfrand_single
    ! For generating random Gaussian numbers; may change to use
    ! other libraries for this task.
include 'fftw3.f'
    ! Link with -lfftw3
interface
    function cov_func(x, args)
        real, dimension(:), intent(in) :: x
        real, dimension(:), intent(in) :: args
        real, dimension(size(x)) :: cov_func
    end function cov_func
end interface
real, dimension(:), intent(in) :: cov_args
    ! Since this routine works for 'stationary' cov function only,
    ! the cov function takes argument 'x' which is the distance.
    ! Extra arguments to the function are passed in via 'cov_args'.
    !
    ! The user often needs to write a wrapper function for their actual cov
    ! function in order to use this routine.
    !
    ! One thing the wrapper function may need to do is
    ! re-arrange the multiple arguments to the actural cov function into a vector,
    ! which is required by this routine.
    !
    ! Because 'cov_func' is always going to be called many times,
    ! this routine makes it a vectorized function, which takes a vector of
    ! distances and produces the vector of cov values at those distances.
    ! If nontrivial processing of 'cov_args' that are common to different
    ! distances are involved, this vectorized form is also desirable.
    ! This 1-D vectorized input and output may be another reason for a wrapper
    ! to be created for the actural cov function.
real, intent(in) :: dx, dy
    ! Grid step size in X and Y directions.
real, dimension(:, :, :), intent(out) :: z
    ! size(z, 1) and size(z, 2) are the grid sizes, i.e., number of grid points
    ! in X and Y directions.
    ! size(z, 3) is number of realizations to simulate.
    ! If only one realization is needed, the user still needs to pass in a 3-D
    ! array, with the 3rd dimension of size 1.

    integer :: nx, ny
    double complex, dimension(:,:), allocatable :: &
        cmplx_in, cmplx_out, cov_weights
        ! Need double precision to reduce the damage of numerical errors.
    integer(kind=8) :: plan1, plan2
    integer :: i, j, k

    nx = 1
    ny = 1
    do
        if (nx < 2 * size(z, 1)) then
            nx = nx * 2
        else
            exit
        end if
    end do
    do
        if (ny < 2 * size(z, 2)) then
            ny = ny * 2
        else
            exit
        end if
    end do
        ! Simulate on a larger grid than what is requested,
        ! and extract the requested size in the end.

    allocate(cmplx_in(nx,ny))
    allocate(cmplx_out(nx,ny))
    allocate(cov_weights(nx,ny))
        ! I couldn't figure out how to expand the shrinked matrix
        ! returned by the real-to-complex transforms of fftw.
        ! For now I'll use complex-to-complex routines even if the input
        ! is actually real.

    call dfftw_plan_dft_2d(plan1, nx, ny, cmplx_in, cmplx_out, &
        FFTW_FORWARD, FFTW_ESTIMATE)
    call dfftw_plan_dft_2d(plan2, nx, ny, cmplx_in, cmplx_out, &
        FFTW_BACKWARD, FFTW_ESTIMATE)

    forall (i=1:nx, j=1:ny) cmplx_in(i,j) = cmplx(i*dx, j*dy)
    cmplx_in = reshape(&
        cov_func(reshape(real(abs(cmplx_in - cmplx(nx/2.*dx, ny/2.*dy))), &
            (/nx * ny/)), cov_args), &
        (/nx, ny/))

    call dfftw_execute(plan1)
    cov_weights = cmplx_out

    cmplx_in = cmplx(0.d0, 0.d0)
    cmplx_in(nx/2, ny/2) = cmplx(1.d0, 0.d0)
    call dfftw_execute(plan1)

    cov_weights = cov_weights / (cmplx_out * nx * ny)

    if (any(real(cov_weights) < 0.)) then
        ! This tends to happen when the correlation range of 'cov_func'
        ! is large relative to M * dx and N * dy.
        print *, 'FFT of covariances has negative values.'
        print *, 'Value range is ', minval(real(cov_weights)), maxval(real(cov_weights))
        stop
    end if

    cov_weights = sqrt(cov_weights)

    call random_seed

    do k = 1, size(z, 3)
        do i = 1, nx
        do j = 1, ny
            cmplx_in(i, j) = cmplx(random_normal(), 0.d0)
        end do
        end do

        call dfftw_execute(plan1)
        cmplx_in = cov_weights * cmplx_out
        call dfftw_execute(plan2)
        z(:, :, k) = real(cmplx_out(1 : size(z,1), 1 : size(z, 2)))
    end do
    z = z / sqrt(real(nx) * real(ny))

    call dfftw_destroy_plan(plan1)
    call dfftw_destroy_plan(plan2)

    deallocate(cmplx_in, cmplx_out, cov_weights)
end subroutine simul_gaussian

