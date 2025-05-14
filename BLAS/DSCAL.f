! 012 LAPACK_BLAS_AUX ##############################################################################################################

      SUBROUTINE  DSCAL(N,DA,DX,INCX)

      USE PENTIUM_II_KIND, ONLY         :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                  :  ERR, F04, F06, SC1, WRT_LOG
      USE SCONTR, ONLY                  :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                  :  HOUR, MINUTE, SEC,
     &                                     SFRAC, TSEC
      USE SUBR_BEGEND_LEVELS, ONLY      :  LAPACK_BEGEND

      USE OUTA_HERE_Interface

c
c     scales a vector by a constant.
c     uses unrolled loops for increment equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      REAL(DOUBLE) da,dx(*)
      integer i,incx,m,mp1,n,nincx
c
      if( n.le.0 .or. incx.le.0 )return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dx(i) = da*dx(i)
   10 continue
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dx(i) = da*dx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dx(i) = da*dx(i)
        dx(i + 1) = da*dx(i + 1)
        dx(i + 2) = da*dx(i + 2)
        dx(i + 3) = da*dx(i + 3)
        dx(i + 4) = da*dx(i + 4)
   50 continue
      return

      END SUBROUTINE DSCAL

