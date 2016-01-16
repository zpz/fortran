function date_and_time_str() result(str)
character(len=23) :: str
! Show the current date and time.
    integer :: values(8)

    call date_and_time(values = values)
    write (str, &
        '(i4, " ", i2, " ", i2, " ", i2.2, ":", i2.2, ":", i2.2, ".", i3.3)') &
        values(1:3), values(5:8)
end function date_and_time_str


elemental subroutine gregorian(jdate, year, month, day, hour, minute, second)
    real(kind(0.d0)), intent(in) :: jdate
    integer, intent(out) :: year, month, day
    integer, intent(out), optional :: hour, minute
    real, intent(out), optional :: second

! Given the Julian date number for a date and time,
! return the year, month, day, etc., on the Gregorian calendar.
!
! Always use -fdefault-real-8 option while compiling this procedure.
! This option makes double precision the default kind for 'real', 
! See julian.f90.
!
! Ref:
!   http://aa.usno.navy.mil/faq/docs/JD_Formula.html
!
! Related procedures:
!   julian.f90
!
! Zepu Zhang
!   17 Feb 2006
!   24 Mar 2006

    integer, parameter :: longint = selected_int_kind(12)
    real :: frac
    integer(kind=longint) :: I, J, K, L, N

    frac = jdate - floor(jdate, longint)
    if (frac < 0.5) then
        L = floor(jdate, longint) + 68569
        frac = frac + 0.5
    else
        L = ceiling(jdate, longint) + 68569
        frac = frac - 0.5
    end if
    N = 4 * L / 146097
    L = L - (146097 * N + 3) / 4
    I = 4000 * (L + 1) / 1461001
    L = L - 1461 * I / 4 + 31
    J = 80 * L / 2447
    K = L - 2447 * J / 80
    L = J / 11
    J = J + 2 - 12 * L
    I = 100 * (N - 49) + I + L

    year = int(I)
    month = int(J)
    day = int(K)

    if (present(hour)) hour = int(frac * 24)
    if (present(minute)) minute = int(mod(frac * 24., 1.) * 60)
    if (present(second)) second = mod(frac * 24. * 60., 1.) * 60.

end subroutine


elemental function julian(year, month, day, hour, minute, second)
    integer, intent(in) :: year, month, day
    integer, intent(in), optional :: hour, minute
    real, intent(in), optional :: second
    real(kind(0.d0)) :: julian

! Given date and time on the Gregorian caldendar,
! return its  Julian date number which is the number of days
! since noon on January 1, -4712, i.e., January 1, 4713 BC (Seidelmann 1992).
! The Julian date is a whole number at noon (not at midnight)
! because its reference time is noon.
!
! Because the Julian date is a huge number that may cause precision trouble,
! always use the -fdefault-real-8 option while compiling this procedure.
! This option makes double precision the default real kind, 
! Double precision is enforced in this function;
! but using that compiler option will make caller programs
! that declare 'real' variables consistent with this function.
!
! Some call the day numeric in the year the 'Julian date'.
! It is wrong and should be avoided.
!
! If hour, minute, and second are present, add the fraction of the day.
!
! Remember that day lengths vary; over time, the length of a day is increasing.  
! Counting days is not an absoulte measure of time in the strict sense.
! But it's okay for the time concept of most measurement data.
!
! A formula given by Danby (1988) and Sinnott (1991) for Gregorian calendar dates
! 1901-2099 is used.
!
! Ref:
!
! A clever computer algorithm for converting calendar dates to Julian
! days was developed using FORTRAN integer arithmetic (H. F. Fliegel'
! and T. C. Van Flandern, "A Machine Algorithm for Processing'
! Calendar Dates," Communications of the ACM 11 [1968]: 657). 
! In FORTRAN integer arithmetic, multiplication and division are
! performed left to right in the order of occurrence, and the
! absolute value of each result is truncated to the next lower
! integer value after each operation, so that both 2/12 and -2/12
! become 0. If I is the year, J the numeric order value of the month,
! and K the day of the month, then the algorithm is: [WRAPPED]
! [JD = K - 32075 + 1461 * (I + 4800 + (J -14)/12)/4 + 367 *
! (J-2-(J-14)/12*12)/12 - 3 * ((I + 4900 + (J-14)/12)/100)/4. ]
!
! Some reference date conversion results for checking:
!   http://www1.jsc.nasa.gov/er/seh/math16.html
!   http://ecsinfo.gsfc.nasa.gov/sec2/papers/noerdlinger2.html
!   http://aa.usno.navy.mil/data/docs/JulianDate.html
!
! Related procedures:
!   gregorian.f90
!
! Zepu Zhang
!   17 Feb 2006
!   24 Mar 2006

    julian = day - 32075 + 1461 * (year + 4800 + (month - 14) / 12) / 4 &
        + 367 * (month - 2 - (month - 14) / 12 * 12) / 12 &
        - 3 * ((year + 4900 + (month - 14) / 12) / 100) / 4 &
        - 0.5

    if (present(hour)) julian = julian + hour / 24.0
    if (present(minute)) julian = julian + minute / 60.0 / 24.0
    if (present(second)) julian = julian + second / 3600.0 / 24.0

end function

