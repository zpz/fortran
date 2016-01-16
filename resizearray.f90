! Re-allocate an array pointer so that it assumes a new size
! while keeping its old content.
!
! This procedure is needed from time to time,
! and it saves the caller program from declaring one more
! temporal pointer variable for carrying out the task in situ.
!
! The procedure handles both shrinking and expanding of an array.
! Currently, vector and matrix are accepted.
!
! Optional arguments <dim> and <mask> specifies the dimension to resize
! and the subset of the old content, on the dimension to be resized,
! to keep. <mask> must be the same size as the original array along
! the dimension to be resized.
!
! Upon return, the array has been re-allocated, content copied,
! and its old allocation released.
!
! Zepu Zhang, 28 Feb 2006.

subroutine resizearray_vec(arr, n, mask)
real, allocatable, intent(inout) :: arr(:)
integer, intent(in), optional :: n
logical, intent(in), optional :: mask(:)

    real :: arr_copy(size(arr))
    integer :: nx, nn, i, j

    nx = size(arr)

    if (present(mask)) then
        if (size(mask) .ne. nx) then
            print *, 'Error in <resizearray>: ', &
                'Mask array must be the same size as the original array.'
            return
        end if
    end if

    if (present(n)) then
        nn = n
    else
        if (present(mask)) then
            nn = count(mask) 
        else
            print *, 'Warning in <resizearray>: ', &
                'Neither desired size nor mask array is provided; procedure call has no effect.'
            return
        end if
    end if


    if (nn == 0) then
        if (allocated(arr)) deallocate(arr)
        return
    end if


    if (nn == nx) then
        if (present(mask)) then
            arr = pack(arr, mask)
        end if
    else
        arr_copy = arr
        if (allocated(arr)) deallocate(arr)
        allocate(arr(nn))

        if (present(mask)) then
            ! Keep the first min(nn, count(mask)) true elements,
            i = 0
            do j = 1, nx
                if (mask(j)) then
                    i = i + 1
                    arr(i) = arr_copy(j)
                    if (i == nn) exit
                end if
            end do
        else
            i = min(nx, nn)
            arr(1 : i) = arr_copy(1 : i)
        end if
    end if

end subroutine resizearray_vec


subroutine resizearray_mat(arr, n, dim, mask)
real, allocatable, intent(inout) :: arr(:,:)
integer, intent(in), optional :: n
integer, intent(in), optional :: dim
logical, intent(in), optional :: mask(:)

    real :: arr_copy(size(arr, 1), size(arr, 2))
    integer :: nx, nn, mm, i, j

    if (present(dim) .and. dim == 2) then
        nx = size(arr, 2)
    else
        nx = size(arr, 1)
    end if

    if (present(mask)) then
        if (size(mask) .ne. nx) then
            print *, 'Error in <resizearray>: ', &
                'Mask array must be the same size as the original array.'
            return  
        end if
    end if

    if (present(n)) then
        nn = n
    else
        if (present(mask)) then
            nn = count(mask) 
        else
            print *, 'Warning in <resizearray>: ', &
                'Neither desired size nor mask array is provided; procedure call has no effect.'
            return
        end if
    end if


    if (nn == 0) then
        if (allocated(arr)) deallocate(arr)
        return
    end if


    if (nn == nx) then
        if (present(mask)) then
            mm = count(mask)
            if (present(dim) .and. dim == 2) then
                do i = 1, size(arr, 1)
                    arr(i, 1:mm) = pack(arr(i, :), mask)
                end do
            else 
                do i = 1, size(arr, 2)
                    arr(1:mm, i) = pack(arr(:, i), mask)
                end do
            end if
        end if
    else
        arr_copy = arr
        if (allocated(arr)) deallocate(arr)

        if (present(dim) .and. dim == 2) then
            allocate(arr(size(arr_copy, 1), nn))

            if (present(mask)) then
                ! Keep the first min(nn, count(mask)) true elements,
                i = 0
                do j = 1, nx
                    if (mask(j)) then
                        i = i + 1
                        arr(:, i) = arr_copy(:, j)
                        if (i == nn) exit
                    end if
                end do
            else
                mm = min(nx, nn)
                arr(:, 1:mm) = arr_copy(:, 1:mm)
            end if
        else
            allocate(arr(nn, size(arr_copy, 2)))

            if (present(mask)) then
                ! Keep the first min(nn, count(mask)) true elements,
                i = 0
                do j = 1, nx
                    if (mask(j)) then
                        i = i + 1
                        arr(i, :) = arr_copy(j, :)
                        if (i == nn) exit
                    end if
                end do
            else
                mm = min(nx, nn)
                arr(1:mm, :) = arr_copy(1:mm, :)
            end if
        end if  
    end if

end subroutine resizearray_mat


subroutine resizearray_vecptr(arr, n, mask)
real, pointer :: arr(:)
integer, intent(in), optional :: n 
logical, intent(in), optional :: mask(:)

    real, pointer :: arr_new(:)
    integer :: nx, nn, i, j

    nx = size(arr)

    if (present(mask)) then
        if (size(mask) .ne. nx) then
            print *, 'Error in <resizearray>: ', &
                'Mask array must be the same size as the original array.'
            return  
        end if
    end if

    if (present(n)) then
        nn = n
    else
        if (present(mask)) then
            nn = count(mask) 
        else
            print *, 'Warning in <resizearray>: ', &
                'Neither desired size nor mask array is provided; procedure call has no effect.'
            return
        end if
    end if


    if (nn == 0) then
        if (associated(arr)) deallocate(arr)
        nullify(arr)
        return
    end if


    if (nn == nx) then
        if (present(mask)) then
            arr = pack(arr, mask)
        end if
    else
        allocate(arr_new(nn))
        arr_new = 0.

        if (present(mask)) then
            ! Keep the first min(nn, count(mask)) true elements,
            i = 0
            do j = 1, nx
                if (mask(j)) then
                    i = i + 1
                    arr_new(i) = arr(j)
                    if (i == nn) exit
                end if
            end do
        else
            i = min(nx, nn)
            arr_new(1 : i) = arr(1 : i)
        end if

        deallocate(arr)
        arr => arr_new
    end if

end subroutine resizearray_vecptr


subroutine resizearray_matptr(arr, n, dim, mask)
real, pointer :: arr(:,:)
integer, intent(in), optional :: n 
integer, intent(in), optional :: dim
logical, intent(in), optional :: mask(:)

    real, pointer :: arr_new(:, :)
    integer :: nx, nn, mm, i, j

    if (present(dim) .and. dim == 2) then
        nx = size(arr, 2)
    else
        nx = size(arr, 1)
    end if

    if (present(mask)) then
        if (size(mask) .ne. nx) then
            print *, 'Error in <resizearray>: ', &
                'Mask array must be the same size as the original array.'
            return  
        end if
    end if

    if (present(n)) then
        nn = n
    else
        if (present(mask)) then
            nn = count(mask) 
        else
            print *, 'Warning in <resizearray>: ', &
                'Neither desired size nor mask array is provided; procedure call has no effect.'
            return
        end if
    end if


    if (nn == 0) then
        if (associated(arr)) deallocate(arr)
        nullify(arr)
        return
    end if


    if (nn == nx) then
        if (present(mask)) then
            mm = count(mask)
            if (present(dim) .and. dim == 2) then
                do i = 1, size(arr, 1)
                    arr(i, 1:mm) = pack(arr(i, :), mask)
                end do
            else 
                do i = 1, size(arr, 2)
                    arr(1:mm, i) = pack(arr(:, i), mask)
                end do
            end if
        end if
    else
        if (present(dim) .and. dim == 2) then
            allocate(arr_new(size(arr, 1), nn))
            arr_new = 0.

            if (present(mask)) then
                ! Keep the first min(nn, count(mask)) true elements,
                i = 0
                do j = 1, nx
                    if (mask(j)) then
                        i = i + 1
                        arr_new(:, i) = arr(:, j)
                        if (i == nn) exit
                    end if
                end do
            else
                mm = min(nx, nn)
                arr_new(:, 1:mm) = arr(:, 1:mm)
            end if

            deallocate(arr)
            arr => arr_new
        else
            allocate(arr_new(nn, size(arr, 2)))
            arr_new = 0.

            if (present(mask)) then
                ! Keep the first min(nn, count(mask)) true elements,
                i = 0
                do j = 1, nx
                    if (mask(j)) then
                        i = i + 1
                        arr_new(i, :) = arr(j, :)
                        if (i == nn) exit
                    end if
                end do
            else
                mm = min(nx, nn)
                arr_new(1:mm, :) = arr(1:mm, :)
            end if

            deallocate(arr)
            arr => arr_new
        end if
    end if

end subroutine resizearray_matptr


