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
 
      SUBROUTINE GET_ARRAY_ROW_NUM ( ARRAY_NAME, CALLING_SUBR, ASIZE, ARRAY, EXT_ID, ROW_NUM )
 
! Searches integer array ARRAY to find an external (actual) ID (EXT_ID) in order to find the row number where it exists.
! If EXT_ID is not found, ROW_NUM is set to -1 to indicate an error. ARRAY must be sorted into numerical order

! The algorithm uses HI and LO to bound the range in ARRAY where EXT_ID may be found. Initially, HI is set to the size
! of ARRAY and LO is set at 0 and the first estimate of the location of EXT_ID is near the middle of the range at:

!                              N = (HI + LO +1)/2                 (1)

! The algorithm then iterates until EXT_ID = ARRAY(N) by modifying HI and LO as follows:
!   (a) If EXT_ID < ARRAY(N) then HI is lowered   to N and a new N is calculated from (1) and the procedure repeated
!   (b) If EXT_ID > ARRAY(N) then LO is increased to N and a new N is calculated from (1) and the procedure repeated

! The FLOOR intrinsic function is used to ensure that N is the largest integer less than the real value of eqn (1), or:
 
!                          DBL_N = (DBL_HI + DBL_LO + 1.D0)/2.D0  (2) 

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, f06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ONE, TWO
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_ARRAY_ROW_NUM_BEGEND
 
      USE GET_ARRAY_ROW_NUM_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GET_ARRAY_ROW_NUM'
      CHARACTER(LEN=*), INTENT(IN)    :: ARRAY_NAME        ! Name of array to be searched
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Name of subr that called this one

      INTEGER(LONG), INTENT(IN)       :: ASIZE             ! Size of ARRAY
      INTEGER(LONG), INTENT(IN)       :: ARRAY(ASIZE)      ! Array to search
      INTEGER(LONG), INTENT(IN)       :: EXT_ID            ! External (actual) ID to find in ARRAY
      INTEGER(LONG), INTENT(OUT)      :: ROW_NUM           ! Internal ID (row in ARRAY) where EXT_ID exists
      INTEGER(LONG)                   :: HI, LO            ! Used to bound the range of N where EXT_ID is expected to be found
      INTEGER(LONG)                   :: LAST              ! Previous value of N in the search
      INTEGER(LONG)                   :: N                 ! When the search is completed, N is the ROW_NUM we ara looking for
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_ARRAY_ROW_NUM_BEGEND
 
      REAL(DOUBLE)                    :: DBL_N             ! Real value of (DBL_HI + DBL_LO + 1.D0)/2.D0
      REAL(DOUBLE)                    :: DBL_HI            ! Real value of HI
      REAL(DOUBLE)                    :: DBL_LO            ! Real value of LO

      INTRINSIC                       :: FLOOR             ! Largest integer less than real argument

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Make sure array is sorted into numerically increasing order

      DO N=2,ASIZE
         IF (ARRAY(N) < ARRAY(N-1)) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,920) CALLING_SUBR, ARRAY_NAME
            WRITE(F06,920) CALLING_SUBR, ARRAY_NAME
            CALL OUTA_HERE ( 'Y' )
         ENDIF
      ENDDO

! Initialize outputs

      ROW_NUM = 0

! Calc outputs

      HI     = ASIZE
      LO     = 0
      DBL_HI = DBLE(HI)
      DBL_LO = DBLE(LO)
      LAST   = 0

      DO
         DBL_N = (DBL_HI + DBL_LO + ONE)/TWO
         N     = FLOOR(DBL_N)
         IF (N == LAST) THEN
            ROW_NUM = -1
            RETURN
         ENDIF
         LAST = N  
         IF      (EXT_ID <  ARRAY(N)) THEN
           HI     = N
           DBL_HI = DBLE(HI)
           CYCLE
         ELSE IF (EXT_ID >  ARRAY(N)) THEN
           LO     = N
           DBL_LO = DBLE(LO)
           CYCLE
         ELSE IF (EXT_ID == ARRAY(N)) THEN 
           EXIT
         ENDIF
      ENDDO 
 
      ROW_NUM = N

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  920 FORMAT(' *ERROR   920: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INPUT ARRAY ',A,' MUST BE SORTED IN NUMERICALLY INCREASING ORDER FOR THIS SUBR TO WORK')




! **********************************************************************************************************************************

      END SUBROUTINE GET_ARRAY_ROW_NUM
