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
 
      SUBROUTINE GET_VEC_MIN_MAX_ABS ( NROWS, ID_LIST, VECTOR, VEC_MIN, VEC_MAX, VEC_ABS, ID_MIN, ID_MAX )
 
! Gets the MIN, MAX and ABS values from a column vector and the grids associated with the MIN and MAX
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_VEC_MIN_MAX_ABS_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MACHINE_PARAMS, ONLY        :  MACH_LARGE_NUM
  
      USE GET_VEC_MIN_MAX_ABS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GET_VEC_MIN_MAX_ABS'
 
      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in ID_LIST and VECTOR
      INTEGER(LONG), INTENT(IN)       :: ID_LIST(NROWS)    ! The ID (grid or elem) numbers corresponding to rows in VECTOR
      INTEGER(LONG), INTENT(OUT)      :: ID_MAX            ! ID where vector is max
      INTEGER(LONG), INTENT(OUT)      :: ID_MIN            ! ID where vector is min
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_VEC_MIN_MAX_ABS_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: VECTOR(NROWS)     ! Values to scan for MIN, MAX, ABS
      REAL(DOUBLE) , INTENT(OUT)      :: VEC_ABS           ! Abs value in vector
      REAL(DOUBLE) , INTENT(OUT)      :: VEC_MAX           ! Max value in vector
      REAL(DOUBLE) , INTENT(OUT)      :: VEC_MIN           ! Min value in vector

      INTRINSIC                       :: MAX, MIN, DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      ID_MAX  = 0
      ID_MIN  = 0
      VEC_MAX = -MACH_LARGE_NUM
      VEC_MIN = ZERO
      VEC_ABS = ZERO

! Get MAX, MIN, ABS values

      ID_MAX  = ID_LIST(1)

      DO I=1,NROWS
         IF (VECTOR(I) > VEC_MAX) THEN
            VEC_MAX  = VECTOR(I)
            ID_MAX = ID_LIST(I)
         ENDIF
      ENDDO

      VEC_MIN = VEC_MAX
      ID_MIN  = ID_LIST(1)

      DO I=1,NROWS
         IF (VECTOR(I) < VEC_MIN) THEN
            VEC_MIN  = VECTOR(I)
            ID_MIN = ID_LIST(I)
         ENDIF
      ENDDO

      VEC_ABS = MAX( DABS(VEC_MAX), DABS(VEC_MIN) )

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

! **********************************************************************************************************************************
 
      END SUBROUTINE GET_VEC_MIN_MAX_ABS
