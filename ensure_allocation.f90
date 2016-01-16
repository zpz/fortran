subroutine ensure_allocation_int_1(val, dims)
integer, intent(inout), allocatable :: val(:)
integer, intent(in) :: dims(:)

    if (allocated(val)) then
        if (size(val) /= dims(1)) then
            deallocate(val)
            allocate(val(dims(1)))
        end if
    else
        allocate(val(dims(1)))
    end if

    val = 0
end subroutine ensure_allocation_int_1


subroutine ensure_allocation_int_2(val, dims)
integer, intent(inout), allocatable :: val(:, :)
integer, intent(in) :: dims(:)

    if (allocated(val)) then
        if (size(val, 1) /= dims(1) .or. size(val, 2) /= dims(2)) then
            deallocate(val)
            allocate(val(dims(1), dims(2)))
        end if
    else
        allocate(val(dims(1), dims(2)))
    end if

    val = 0
end subroutine ensure_allocation_int_2


subroutine ensure_allocation_int_3(val, dims)
integer, intent(inout), allocatable :: val(:, :, :)
integer, intent(in) :: dims(:)

    if (allocated(val)) then
        if (size(val, 1) /= dims(1) .or. size(val, 2) /= dims(2) &
                .or. size(val, 3) /= dims(3)) then
            deallocate(val)
            allocate(val(dims(1), dims(2), dims(3)))
        end if
    else
        allocate(val(dims(1), dims(2), dims(3)))
    end if

    val = 0
end subroutine ensure_allocation_int_3


subroutine ensure_allocation_int_4(val, dims)
integer, intent(inout), allocatable :: val(:, :, :, :)
integer, intent(in) :: dims(:)

    if (allocated(val)) then
        if (size(val, 1) /= dims(1) .or. size(val, 2) /= dims(2) &
                .or. size(val, 3) /= dims(3) .or. size(val, 4) /= dims(4)) then
            deallocate(val)
            allocate(val(dims(1), dims(2), dims(3), dims(4)))
        end if
    else
        allocate(val(dims(1), dims(2), dims(3), dims(4)))
    end if

    val = 0
end subroutine ensure_allocation_int_4


subroutine ensure_allocation_real_1(val, dims)
real, intent(inout), allocatable :: val(:)
integer, intent(in) :: dims(:)

    if (allocated(val)) then
        if (size(val) /= dims(1)) then
            deallocate(val)
            allocate(val(dims(1)))
        end if
    else
        allocate(val(dims(1)))
    end if

    val = 0.
end subroutine ensure_allocation_real_1


subroutine ensure_allocation_real_2(val, dims)
real, intent(inout), allocatable :: val(:, :)
integer, intent(in) :: dims(:)

    if (allocated(val)) then
        if (size(val, 1) /= dims(1) .or. size(val, 2) /= dims(2)) then
            deallocate(val)
            allocate(val(dims(1), dims(2)))
        end if
    else
        allocate(val(dims(1), dims(2)))
    end if

    val = 0.
end subroutine ensure_allocation_real_2


subroutine ensure_allocation_real_3(val, dims)
real, intent(inout), allocatable :: val(:, :, :)
integer, intent(in) :: dims(:)

    if (allocated(val)) then
        if (size(val, 1) /= dims(1) .or. size(val, 2) /= dims(2) &
                .or. size(val, 3) /= dims(3)) then
            deallocate(val)
            allocate(val(dims(1), dims(2), dims(3)))
        end if
    else
        allocate(val(dims(1), dims(2), dims(3)))
    end if

    val = 0.
end subroutine ensure_allocation_real_3


subroutine ensure_allocation_real_4(val, dims)
real, intent(inout), allocatable :: val(:, :, :, :)
integer, intent(in) :: dims(:)

    if (allocated(val)) then
        if (size(val, 1) /= dims(1) .or. size(val, 2) /= dims(2) &
                .or. size(val, 3) /= dims(3) .or. size(val, 4) /= dims(4)) then
            deallocate(val)
            allocate(val(dims(1), dims(2), dims(3), dims(4)))
        end if
    else
        allocate(val(dims(1), dims(2), dims(3), dims(4)))
    end if

    val = 0.
end subroutine ensure_allocation_real_4


subroutine ensure_allocation_cmplx_1(val, dims)
complex, intent(inout), allocatable :: val(:)
integer, intent(in) :: dims(:)

    if (allocated(val)) then
        if (size(val) /= dims(1)) then
            deallocate(val)
            allocate(val(dims(1)))
        end if
    else
        allocate(val(dims(1)))
    end if

    val = (0., 0.)
end subroutine ensure_allocation_cmplx_1


subroutine ensure_allocation_cmplx_2(val, dims)
complex, intent(inout), allocatable :: val(:, :)
integer, intent(in) :: dims(:)

    if (allocated(val)) then
        if (size(val, 1) /= dims(1) .or. size(val, 2) /= dims(2)) then
            deallocate(val)
            allocate(val(dims(1), dims(2)))
        end if
    else
        allocate(val(dims(1), dims(2)))
    end if

    val = (0., 0.)
end subroutine ensure_allocation_cmplx_2


subroutine ensure_allocation_cmplx_3(val, dims)
complex, intent(inout), allocatable :: val(:, :, :)
integer, intent(in) :: dims(:)

    if (allocated(val)) then
        if (size(val, 1) /= dims(1) .or. size(val, 2) /= dims(2) &
                .or. size(val, 3) /= dims(3)) then
            deallocate(val)
            allocate(val(dims(1), dims(2), dims(3)))
        end if
    else
        allocate(val(dims(1), dims(2), dims(3)))
    end if

    val = (0., 0.)
end subroutine ensure_allocation_cmplx_3


subroutine ensure_allocation_cmplx_4(val, dims)
complex, intent(inout), allocatable :: val(:, :, :, :)
integer, intent(in) :: dims(:)

    if (allocated(val)) then
        if (size(val, 1) /= dims(1) .or. size(val, 2) /= dims(2) &
                .or. size(val, 3) /= dims(3) .or. size(val, 4) /= dims(4)) then
            deallocate(val)
            allocate(val(dims(1), dims(2), dims(3), dims(4)))
        end if
    else
        allocate(val(dims(1), dims(2), dims(3), dims(4)))
    end if

    val = (0., 0.)
end subroutine ensure_allocation_cmplx_4


subroutine ensure_allocation_bool_1(val, dims)
logical, intent(inout), allocatable :: val(:)
integer, intent(in) :: dims(:)

    if (allocated(val)) then
        if (size(val) /= dims(1)) then
            deallocate(val)
            allocate(val(dims(1)))
        end if
    else
        allocate(val(dims(1)))
    end if

    val = .false.
end subroutine ensure_allocation_bool_1


subroutine ensure_allocation_bool_2(val, dims)
logical, intent(inout), allocatable :: val(:, :)
integer, intent(in) :: dims(:)

    if (allocated(val)) then
        if (size(val, 1) /= dims(1) .or. size(val, 2) /= dims(2)) then
            deallocate(val)
            allocate(val(dims(1), dims(2)))
        end if
    else
        allocate(val(dims(1), dims(2)))
    end if

    val = .false.
end subroutine ensure_allocation_bool_2


subroutine ensure_allocation_bool_3(val, dims)
logical, intent(inout), allocatable :: val(:, :, :)
integer, intent(in) :: dims(:)

    if (allocated(val)) then
        if (size(val, 1) /= dims(1) .or. size(val, 2) /= dims(2) &
                .or. size(val, 3) /= dims(3)) then
            deallocate(val)
            allocate(val(dims(1), dims(2), dims(3)))
        end if
    else
        allocate(val(dims(1), dims(2), dims(3)))
    end if

    val = .false.
end subroutine ensure_allocation_bool_3


subroutine ensure_allocation_bool_4(val, dims)
logical, intent(inout), allocatable :: val(:, :, :, :)
integer, intent(in) :: dims(:)

    if (allocated(val)) then
        if (size(val, 1) /= dims(1) .or. size(val, 2) /= dims(2) &
                .or. size(val, 3) /= dims(3) .or. size(val, 4) /= dims(4)) then
            deallocate(val)
            allocate(val(dims(1), dims(2), dims(3), dims(4)))
        end if
    else
        allocate(val(dims(1), dims(2), dims(3), dims(4)))
    end if

    val = .false.
end subroutine ensure_allocation_bool_4

