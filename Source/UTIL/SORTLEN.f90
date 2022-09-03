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
 
      SUBROUTINE SORTLEN ( NLEN, JCT )
 
! Calculates shell sort length parameter, JCT
 
      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE CONSTANTS_1, ONLY           :  TWO
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  SORTLEN_BEGEND
 
      USE SORTLEN_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SORTLEN'

      INTEGER(LONG), INTENT(IN)       :: NLEN              ! Length of the array that will be sorted in the calling procedure
      INTEGER(LONG), INTENT(OUT)      :: JCT               ! Sort parameter to be used by calling procedure
      INTEGER(LONG)                   :: MAX_JCT           ! Max practical value of JCT to use in sort by the calling procedure.
!                                                            Values of JCT > MAX_JCT will not cause any error, but will introduce
!                                                            inefficiency into the sort (a DO loop will run excessively). 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SORTLEN_BEGEND
 
      INTRINSIC DLOG
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      JCT = 0

! MAX_JCT is the largest integer value of JCT that is practical to use in the shell sort routine in the calling
! routine. Larger values cause unnecessary DO looping in that routine.

      MAX_JCT  = FLOOR(  (DLOG(DBLE(NLEN)+1.D0)) / (DLOG(TWO))  )

! Calculate shell sort parameter JCT based on array size (NLEN)
 
      IF (NLEN <= 5) THEN
         JCT = 1
      ELSE IF ((NLEN >       5) .AND. (NLEN <=      13)) THEN ! Add      8
         JCT = 2
      ELSE IF ((NLEN >      13) .AND. (NLEN <=      29)) THEN ! Add     16
         JCT = 3
      ELSE IF ((NLEN >      29) .AND. (NLEN <=      61)) THEN ! Add     32
         JCT = 4
      ELSE IF ((NLEN >      61) .AND. (NLEN <=     125)) THEN ! Add     64
         JCT = 5
      ELSE IF ((NLEN >     125) .AND. (NLEN <=     253)) THEN ! Add    128
         JCT = 6
      ELSE IF ((NLEN >     253) .AND. (NLEN <=     509)) THEN ! Add    256
         JCT = 7
      ELSE IF ((NLEN >     509) .AND. (NLEN <=    1021)) THEN ! Add    512
         JCT = 8
      ELSE IF ((NLEN >    1022) .AND. (NLEN <=    2045)) THEN ! Add   1024
         JCT = 9
      ELSE IF ((NLEN >    2045) .AND. (NLEN <=    4093)) THEN ! Add   2048
         JCT = 10
      ELSE IF ((NLEN >    4093) .AND. (NLEN <=    8189)) THEN ! Add   4096
         JCT = 11
      ELSE IF ((NLEN >    8189) .AND. (NLEN <=   16381)) THEN ! Add   8192
         JCT = 12
      ELSE IF ((NLEN >   16381) .AND. (NLEN <=   32765)) THEN ! Add  16384
         JCT = 13
      ELSE IF ((NLEN >   32765) .AND. (NLEN <=   65533)) THEN ! Add  32768
         JCT = 14
      ELSE IF ((NLEN >   65533) .AND. (NLEN <=  131069)) THEN ! Add  65536
         JCT = 15      
      ELSE IF ((NLEN >  131069) .AND. (NLEN <=  262141)) THEN ! Add 131072
         JCT = 16      
      ELSE IF ((NLEN >  262141) .AND. (NLEN <=  524285)) THEN ! Add 262144
         JCT = 17      
      ELSE IF ((NLEN >  524285) .AND. (NLEN <= 1048573)) THEN ! Add 524288
         JCT = 18      
      ELSE
         JCT = MAX_JCT
      ENDIF
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 
      END SUBROUTINE SORTLEN
