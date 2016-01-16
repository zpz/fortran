! Zepu Zhang, Feb 2006.

function str2int(string)
character(len=*), intent(in) :: string
integer :: str2int
! <string> must be properly coded as an integer,
! but it can contain leading and trailing spaces.

    read(string, *) str2int
end function


function str2real(string)
character(len=*), intent(in) :: string
real :: str2real
! <string> must be properly coded as a real,
! but it can contain leading and trailing spaces.

    read(string, *) str2real
end function


function int2str(i) result(str)
integer, intent(in) :: i
character(len=(int(log10(real(max(abs(i),1)))) + 1 + merge(1,0,i<0))) :: str

    character(len=20) :: frmt

    write(frmt, "(A,I10,A)") &
        "(I", &
        (int(log10(real(max(abs(i),1)))) + 1 + merge(1,0,i<0)),&
        ")"
    write(str, frmt) i
end function int2str


function real2str(val)
real, intent(in) :: val
character(len=20) :: real2str
! The caller often needs to use trim(real2str(...))).

    write(real2str, *) val
    real2str = adjustl(real2str)
end function


subroutine uppercase(str)
    character(len=*), intent(inout) :: str

! Zepu Zhang, 18 Feb 2006.

    integer i, j
    integer, parameter :: ialow = ichar('a')
    integer, parameter :: izlow = ichar('z')

    do i = 1, len(str)
        j = ichar(str(i:i))
        if (j >= ialow .and. j <= izlow) then
            str(i:i) = char(j - 32)
        end if
    end do

end subroutine uppercase


subroutine lowercase(str)
    character(len=*), intent(inout) :: str

! Zepu Zhang, 18 Feb 2006.

    integer i, j
    integer, parameter :: ialow = ichar('A')
    integer, parameter :: izlow = ichar('Z')

    do i = 1, len(str)
        j = ichar(str(i:i))
        if (j >= ialow .and. j <= izlow) then
            str(i:i) = char(j + 32)
        end if
    end do

end subroutine lowercase


function wordcount(line)
    character(len=*), intent(in) :: line
    integer wordcount

! Count the number of words, i.e. space separated substrings, in a string.
!
! Zepu Zhang, 13 Feb 2006.

    character(len=len(line)) :: newline
    integer i

    newline = line
    wordcount = 0
    do 
        newline = adjustl(newline)
        i = index(newline, ' ')
        if (i == 1) exit
        wordcount = wordcount + 1
        newline(1 : i) = ' ' 
    end do
end function wordcount

