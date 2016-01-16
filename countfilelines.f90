function countfilelines(funit, stopblank)
    integer, intent(in) :: funit
    logical, intent(in), optional :: stopblank
    integer :: countfilelines

! Count the number of an opened text file
! from its current position to the end of the file,
! or to the next blank line, when <stopblank> is true.
!
! After the procedure, the current position in the file
! is changed, possibly at the end of the file.
!
! If counting to the next blank line,
! the blank line is not counted,
! but the file is positioned after reading the blank line.
!
! Zepu Zhang, 13 Feb 2006.

    character(len=256) :: line
    integer flag

    countfilelines = 0
    if (.not. present(stopblank) .or. .not. stopblank) then
        do
            read(funit, '(a256)', iostat=flag) line
            if (flag < 0) exit
            countfilelines = countfilelines + 1
        end do
    else
        do
            read(funit, '(a256)', iostat=flag) line
            if (len_trim(adjustl(line)) == 0 .or. flag < 0) exit
            countfilelines = countfilelines + 1
        end do
    end if

end function countfilelines
