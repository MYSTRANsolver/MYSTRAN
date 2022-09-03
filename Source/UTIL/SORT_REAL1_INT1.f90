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
 
      SUBROUTINE SORT_REAL1_INT1 ( CALLING_SUBR, MESSAG, NSIZE, RARRAY, IARRAY )
 
! Performs shell sort on real array RARRAY to put it into numerically increasing order and sorts integer array IARRAY
! along with RARRAY. Both arrays are of size NSIZE
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE PARAMS, ONLY                :  SORT_MAX
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  SORT_REAL1_INT1_BEGEND
 
      USE SORT_REAL1_INT1_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SORT_REAL1_INT1'
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Subr that called this subr
      CHARACTER(LEN=*), INTENT(IN)    :: MESSAG            ! Message to be written out if this subr fails to sort
      CHARACTER( 3*BYTE)              :: SORTED            ! = 'YES' if array is sorted

      INTEGER(LONG), INTENT(IN)       :: NSIZE             ! No. rows in arrays IARRAY, RARRAY
      INTEGER(LONG), INTENT(INOUT)    :: IARRAY(NSIZE)     ! Integer array
      INTEGER(LONG)                   :: I,K,M             ! DO loop indices
      INTEGER(LONG)                   :: IDUM              ! Dummy values in IARRAY used when switching IARRAY rows during sort. 
      INTEGER(LONG)                   :: IFLIP             ! Indicates whether two values have been switched in sort order.
      INTEGER(LONG)                   :: JCT               ! Shell sort parameter returned from subroutine SORTLEN.
      INTEGER(LONG)                   :: MAXM              ! NSIZE - SORTPK
      INTEGER(LONG)                   :: N                 ! An array index
      INTEGER(LONG)                   :: SORTPK            ! Intermediate variable used in setting a DO loop range.
      INTEGER(LONG)                   :: SORT_NUM          ! How many times the sort has to be performed in order for the data
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SORT_REAL1_INT1_BEGEND

      REAL(DOUBLE),  INTENT(INOUT)    :: RARRAY(NSIZE)     ! Array of real values 
      REAL(DOUBLE)                    :: RDUM              ! Dummy values in RARRAY used when switching RARRAY rows during sort.

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Call SORTLEN to calculate the shell sort parameter JCT
 
      SORT_NUM = 1

      CALL SORTLEN ( NSIZE, JCT )
 
outer:DO                                                      ! Run sort until array is sorted or SORT_MAX is exceeded

         DO K = JCT,1,-1                                      ! Do the sort
            SORTPK = 2**K - 1
            MAXM  = NSIZE - SORTPK
            IFLIP = 0
            DO
               DO M = 1,MAXM
                  N = M + SORTPK
                  IF (RARRAY(M) > RARRAY(N)) THEN
                     RDUM      = RARRAY(M)
                     IDUM      = IARRAY(M)
                     RARRAY(M) = RARRAY(N)
                     IARRAY(M) = IARRAY(N)
                     RARRAY(N) = RDUM
                     IARRAY(N) = IDUM
                     IFLIP = 1
                  ENDIF
               ENDDO
               IF (IFLIP == 1) THEN
                  IFLIP = 0
                  CYCLE
               ELSE
                  EXIT
               ENDIF
            ENDDO 
         ENDDO
 
         SORTED = 'YES'                                    ! Make sure array is sorted.
chk_sort:DO I=1,NSIZE-1
            IF (RARRAY(I) > RARRAY(I+1)) THEN
            SORTED = 'NO '
            EXIT chk_sort
            ENDIF
         ENDDO chk_sort

         IF (SORTED == 'YES') THEN                         ! If array is sorted, exit outer loop
            EXIT outer
         ELSE                                              ! If array is not sorted, sort again if SORT_MAX num times not exceeded
            SORT_NUM = SORT_NUM + 1
            IF (SORT_NUM <= SORT_MAX) THEN
               CYCLE outer
            ELSE
               WRITE(ERR, 914) SUBR_NAME, CALLING_SUBR, SORT_MAX, MESSAG
               WRITE(F06, 914) SUBR_NAME, CALLING_SUBR, SORT_MAX, MESSAG
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )                              ! Can't get array sorted after trying SORT_MAX times, so quit
            ENDIF
         ENDIF

      ENDDO outer

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  914 FORMAT(' *ERROR   914: SUBROUTINE ',A,', CALLED BY SUBR ',A                                                                  &
                    ,/,14X,' HAS MADE ',I8,' UNSUCCESSFUL ATTEMPTS TO SORT ARRAY(S) ',A                                            &
                    ,/,14X,' THE MAX NUMBER OF SORT ATTEMPTS CAN BE INCREASED WITH BULK DATA PARAM SORT_MAX') 

! **********************************************************************************************************************************

      END SUBROUTINE SORT_REAL1_INT1
