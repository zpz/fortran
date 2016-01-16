integer function spareunit(low, up)
integer, intent(in), optional :: low, up

    real :: tt
    logical :: stat
    integer :: L, U

    if (present(low)) then
        L = max(low, 7)
    else
        L = 20
    end if
    if (present(up)) then
        U = min(up, 99)
    else
        U = 90
    end if

    do
        call random_number(tt)
        spareunit = L + floor(tt * (U - L + 1))
        inquire (spareunit, opened=stat)
        if (.not.stat) exit
    end do

end function spareunit
