! Call LAPACK routines to compute the inverse of a square matrix
! or the pseudo inverse (generalized inverse) of a symmetric matrix.
!
! Any program that calls these procedures needs to link liblapack.

subroutine inverse(A, exitflag)
real, dimension(:,:), intent(inout) :: A
    ! A is the matrix to be inverted.
integer, intent(out) :: exitflag

    integer :: N, LDA
    double precision :: B(size(A, 1), size(A, 2))
    integer :: i

    N = size(A, 2)
    LDA = size(A, 1)

    exitflag = 1

    if (N == LDA) then
        B = A
        call dpotrf('L', N, B, LDA, exitflag)
        if (exitflag == 0) then
            call dpotri('L', N, B, LDA, exitflag)
            if (exitflag == 0) then
                do i = 1, N - 1
                    B(i, (i+1) : N) = B((i+1) : N, i)
                end do
                print *, '--- Cov matrix inverted by Cholesky. ---'
                A = real(B)
                exitflag = 0
            else
                !print *, 'Warning in <invert_cov_data2data>: Cholesky failed.'
            end if
        else
            !print *, 'Warning in <invert_cov_data2data>: Cholesky failed.'
        end if
    end if

end subroutine inverse


subroutine ginverse(A, uplow, exitflag)
real, intent(inout), dimension(:,:) :: A
character, intent(in) :: uplow  ! 'L' or 'U'
integer, intent(out) :: exitflag

    integer :: negv                  ! Number of eigen values found.
    double precision :: AA(size(A, 1), size(A, 2))
    double precision :: egv(size(AA,2))           ! Eigen values found, in ascending order
    double precision :: Z(size(AA,1), size(AA,2))  ! Eigen vectors corresponding to 'egv'
    integer :: Zsupp(size(AA,2) * 2)  ! The support of Z.
    double precision :: U(size(AA, 1), size(AA, 1)), VT(size(AA, 2), size(AA, 2))
    double precision, allocatable :: work(:)     ! Work space, real.
    integer, allocatable :: iwork(:) ! Work space, integer.
    double precision :: twork(1)     ! For query.
    integer :: tiwork(1)             ! For query.
    integer :: i
    double precision :: tol
    integer :: method = 2

    ! Query on the needed sizes of the work spaces.

    select case (method)
    case (1)
        ! The 'divide and conquer' method for SVD.
        ! Not taking advantage of the fact that the matrix is square and symmetric.
        ! This method seems to take 3 times as long as the other two methods.
        call dgesvd('A', 'A', size(AA, 1), size(AA, 2), AA, size(AA, 1), &
            egv, &
            U, size(U, 1), VT, size(VT, 1), &
            twork, -1, exitflag)

        allocate(work(ceiling(twork(1))))

        call dgesvd('A', 'A', size(AA, 1), size(AA, 2), AA, size(AA, 1), &
            egv, &
            U, size(U, 1), VT, size(VT, 1), &
            work, size(work), exitflag)
        negv = size(AA, 2)

    case (2)
        ! The 'Divide and Conquer' method.
        ! More accurate, but uses more storage.
        call dsyevd('V', uplow, size(AA, 2), AA, size(AA, 1), &
            egv, &
            twork, -1, tiwork, -1, exitflag)

        allocate(work(ceiling(twork(1))))
        allocate(iwork(tiwork(1)))

        call dsyevd('V', uplow, size(AA, 2), AA, size(AA, 1), &
            egv, &
            work, size(work), iwork, size(iwork), exitflag)
        negv = size(AA, 2)
        U = AA
        VT = transpose(U)

    case (3)
        ! The 'Relatively Robust Representation' method.
        ! Uses less storage.
        call dsyevr('V', 'A', uplow, size(AA, 2), AA, size(AA, 1), &
            0., 0., 0, 0, -1., negv, egv, Z, size(Z, 1), Zsupp, &
            twork, -1, tiwork, -1, exitflag)

        allocate(work(ceiling(twork(1))))
        allocate(iwork(tiwork(1)))

        call dsyevr('V', 'A', uplow, size(AA, 2), AA, size(AA, 1), &
            0., 0., 0, 0, -1., negv, egv, Z, size(Z, 1), Zsupp, &
            work, size(work), iwork, size(iwork), exitflag)
        U = Z
        VT = transpose(U)

    case default
        stop 'Unknown "method" value.'
    end select

    if (exitflag /= 0) then
        if (exitflag < 0) then
            print *, 'Error in <pseudo_inverse>: The ', -exitflag, &
                '-th argument to SSYEVR had an illegal value.'
        else
            print *, 'Error in <pseudo_inverse>: Internal error in SSYEVR.'
        end if
    else
        tol = tiny(0.) * maxval(abs(egv))

        AA = 0.
        do i = 1, negv
            if (abs(egv(i)) > tol) then
                AA(:, i) = VT(i, :) / egv(i)
            else
                AA(:, i) = 0.
            end if
        end do

        AA = matmul(AA, transpose(U))
    end if

    A = real(AA)

    if (allocated(work)) deallocate(work)
    if (allocated(iwork)) deallocate(iwork)
end subroutine ginverse

! Solve a least square problem:
!   minimize_x ||b - A x||_2
! where input and output a, b, x are all in single precision.
! Warning: single precision routines sometimes cause significant rounding
! errors.
! subroutine slsq(a, b, exitflag)
! real, intent(in), dimension(:,:) :: a
! real, intent(inout), dimension(:) :: b
! integer, intent(out) :: exitflag
! ! Requirement:
! !   size(b) = size(a, 1)
! 
!     real :: A(count(obsrv_mask) * 2 + size(m_coast_loc), &
!             count(obsrv_mask) * 2 + size(m_coast_loc))
!     real :: B(count(obsrv_mask) * 2 + size(m_coast_loc), 1)
!     real :: S(size(m_kdd, 1))
!         ! Singular values of A.
!     real, allocatable :: work(:)
!     integer, allocatable :: iwork(:)
!     logical :: data_mask(size(m_kdd, 1), size(m_kdd, 2))
!     integer :: i, j, k, ndata, ndata_good, no
!     integer :: exitflag
! 
!     allocate(work(1), iwork(1))
!     call sgelsd(ndata_good, ndata_good, 1, A, size(A, 1),&
!         B, size(B, 1), S, -1., rank, work, -1, iwork, exitflag)
! 
!     deallocate(work, iwork)
!     allocate(work(int(work(1) + .5)))
!     allocate(iwork(int(iwork(1) + .5)))
! 
!     call sgelsd(ndata_good, ndata_good, 1, A, size(A, 1),&
!         B, size(B, 1), s, -1., rank, work, size(work), iwork, exitflag)
!     deallocate(work, iwork)
!     if (exitflag /= 0) then
!         print *, 'Least square solution failed.'
!         stop
!     end if
! 
!     lambda = unpack(B, data_mask, 0.)
! 
! end subroutine slsq
