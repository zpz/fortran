! Taken from the source code of R, which has the note below.
! Changed double pricision to real.  -- Zepu Zhang
!
!     from netlib/a/stl: no authorship nor copyright claim in the source;
!     presumably by the authors of
!
!     R.B. Cleveland, W.S.Cleveland, J.E. McRae, and I. Terpenning,
!     STL: A Seasonal-Trend Decomposition Procedure Based on Loess,
!     Statistics Research Report, AT&T Bell Laboratories.
!
!     Converted to double precision by B.D. Ripley 1999.
!     Indented, goto labels renamed, many goto's replaced by `if then {else}'
!     (using Emacs), many more comments;  by M.Maechler 2001-02.
!

! Example usage:
! Median of X(n), where n is even
!
!  integer mid(2)
!  mid(1) = n / 2
!  mid(2) = mid(1) + 1
!  call psort(x, n, mid, 2)
!  median = 0.5 * x(mid(1)) + 0.5 * x(mid(2))
!

      subroutine psort(a,n,ind,ni)
!
! Partial Sorting
!
      implicit none
! Arg
      integer n,ni
      real a(n)
      integer ind(ni)
! Var
      integer indu(16),indl(16),iu(16),il(16),p,jl,ju,i,j,m,k,ij,l
      real t,tt

      if(n .lt. 0 .or. ni .lt. 0) return

      if(n .lt. 2 .or. ni .eq. 0) return

      jl = 1
      ju = ni
      indl(1) = 1
      indu(1) = ni
      i = 1
      j = n
      m = 1

! Outer Loop
 161  continue
      if(i .lt. j) go to 10

!  _Loop_
 166  continue
      m = m-1
      if(m .eq. 0) return
      i = il(m)
      j = iu(m)
      jl = indl(m)
      ju = indu(m)
      if(.not.(jl .le. ju))  goto 166

!     while (j - i > 10)
 173  if(.not.(j-i .gt. 10)) goto 174

 10   k = i
      ij = (i+j)/2
      t = a(ij)
      if(a(i) .gt. t) then
         a(ij) = a(i)
         a(i) = t
         t = a(ij)
      endif
      l = j
      if(a(j) .lt. t) then
         a(ij) = a(j)
         a(j) = t
         t = a(ij)
         if(a(i) .gt. t) then
            a(ij) = a(i)
            a(i) = t
            t = a(ij)
         endif
      endif

 181  continue
      l = l-1
      if(a(l) .le. t)then
         tt = a(l)
 186     continue
         k = k+1
 187     if(.not.(a(k) .ge. t))goto   186

         if(k .gt. l) goto 183

         a(l) = a(k)
         a(k) = tt
      endif
 182  goto   181

 183  continue
      indl(m) = jl
      indu(m) = ju
      p = m
      m = m+1
      if(l-i .le. j-k) then
         il(p) = k
         iu(p) = j
         j = l

 193     continue
         if(jl .gt. ju)		goto 166
         if(ind(ju) .gt. j) then
            ju = ju-1
            goto 193
         endif
         indl(p) = ju+1
      else
         il(p) = i
         iu(p) = l
         i = k

 200     continue
         if(jl .gt. ju)		goto   166
         if(ind(jl) .lt. i) then
            jl = jl+1
            goto 200
         endif
         indu(p) = jl-1
      endif

      goto   173
!     end while
 174  continue

      if(i .ne. 1) then
         i = i-1
 209     continue
         i = i+1
         if(i .eq. j) goto 166
         t = a(i+1)
         if(a(i) .gt. t) then
            k = i
!           repeat
 216        continue
            a(k+1) = a(k)
            k = k-1
            if(.not.(t .ge. a(k))) goto 216
!           until  t >= a(k)
            a(k+1) = t
         endif
         goto 209

      endif

      goto 161
! End Outer Loop

      end subroutine psort
