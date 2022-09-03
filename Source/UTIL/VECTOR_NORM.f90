! ##################################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2019 Dr William R Case, Jr (dbcase29@gmail.com)                                              
                                                                                                         
! Permission is hereby granted, free of charge, to any person obtaining a copy of this software and      
! associated documentation files (the "Software"), to deal in the Software without restriction, including
! without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to   
! the following conditions:                                                                              
                                                                                                         
! The above copyright notice and this permission notice shall be included in all copies or substantial   
! portions of the Software and documentation.                                                                              
                                                                                                         
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS                                
! OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,                            
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE                            
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER                                 
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,                          
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN                              
! THE SOFTWARE.                                                                                          
! _______________________________________________________________________________________________________
                                                                                                        
! End MIT license text.                                                                                      
 
      SUBROUTINE VECTOR_NORM ( VEC, NSIZE, WHICH, VEC_NORM, IERR )
 
! Calculates a particular norm (VEC_NORM) for an input vector (VEC) of extent NSIZE. The norm that is to be calculated is specified
! by WHICH:

!    (1) If WHICH = 'EUCLIDIAN', calculate the Euclidian norm (square root of the sum of the squares of all terms in the vector)
!    (2) If WHICH = 'INFINITY' , calculate the infinity  norm (maximum absolute value in the vector)

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO 
      USE SUBR_BEGEND_LEVELS, ONLY    :  VECTOR_NORM_BEGEND

      USE VECTOR_NORM_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'VECTOR_NORM'
      CHARACTER(LEN=*), INTENT(IN)    :: WHICH             ! Which norm to calculate (see below)

      INTEGER(LONG)   , INTENT(IN)    :: NSIZE             ! Extent of VEC
      INTEGER(LONG)   , INTENT(OUT)   :: IERR              ! Error indicator
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)   , PARAMETER     :: SUBR_BEGEND = VECTOR_NORM_BEGEND
 
      REAL(DOUBLE)    , INTENT(IN)    :: VEC(NSIZE)        ! The vector for which the norm will be calculated
      REAL(DOUBLE)    , INTENT(OUT)   :: VEC_NORM          ! The norm calculated for VEC
      REAL(DOUBLE)                    :: TEMP              ! Temporary variable

      INTRINSIC                       :: DSQRT

! *********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGIN',F10.3)
      ENDIF

! **********************************************************************************************************************************
      IERR     = 0
      TEMP     = ZERO
      VEC_NORM = ZERO

      IF (WHICH == 'EUCLIDIAN') THEN                       ! Euclidian norm

         DO I=1,NSIZE
            TEMP = TEMP + VEC(I)*VEC(I)
         ENDDO
         VEC_NORM = DSQRT(TEMP)

      ELSE IF (WHICH == 'INFINITY') THEN                   ! Infinity norm

         DO I=1,NSIZE
            IF (DABS(VEC(I)) > VEC_NORM) THEN
               VEC_NORM = DABS(VEC(I))
            ENDIF
         ENDDO 

      ELSE

         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,964) SUBR_NAME, WHICH
         WRITE(F06,964) SUBR_NAME, WHICH

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  964 FORMAT(' *ERROR   965: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INPUT VARIABLE WHICH = "',A,'" IS NOT A VALID VECTOR NORM METHOD FOR THIS SUBR')

! **********************************************************************************************************************************

      END SUBROUTINE VECTOR_NORM

