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

      SUBROUTINE CALC_VEC_SORT_ORDER ( VEC, SORT_ORDER, SORT_INDICES )

! Determines the order of the 3 components of a vector if they were arranged from lowest value to largest value. The values are not
! actually sorted. 

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  CALC_VEC_SORT_ORDER_BEGEND

      USE CALC_VEC_SORT_ORDER_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'CALC_VEC_SORT_ORDER'
      CHARACTER( 5*BYTE), INTENT(OUT) :: SORT_ORDER        ! Order in which the VX(i) have been sorted. If none of the tests below
!                                                            are satisfied, SORT_ORDER is returned as null

      INTEGER(LONG), INTENT(OUT)      :: SORT_INDICES(3)   ! Indices of VEC in the order from lowest value component to highest
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CALC_VEC_SORT_ORDER_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: VEC(3)            ! A 3 component vector

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      SORT_ORDER = '     '

      IF ((DABS(VEC(1)) <= DABS(VEC(2))) .AND. (DABS(VEC(1)) <= DABS(VEC(3))) .AND. (DABS(VEC(2)) <= DABS(VEC(3)))) THEN
         SORT_ORDER      = '1 2 3'
         SORT_INDICES(1) = 1
         SORT_INDICES(2) = 2
         SORT_INDICES(3) = 3
      ENDIF

      IF ((DABS(VEC(1)) <= DABS(VEC(2))) .AND. (DABS(VEC(1)) <= DABS(VEC(3))) .AND. (DABS(VEC(3)) <= DABS(VEC(2)))) THEN
         SORT_ORDER      = '1 3 2'
         SORT_INDICES(1) = 1
         SORT_INDICES(2) = 3
         SORT_INDICES(3) = 2
      ENDIF

      IF ((DABS(VEC(2)) <= DABS(VEC(3))) .AND. (DABS(VEC(2)) <= DABS(VEC(1))) .AND. (DABS(VEC(1)) <= DABS(VEC(3)))) THEN
         SORT_ORDER      = '2 1 3'
         SORT_INDICES(1) = 2
         SORT_INDICES(2) = 1
         SORT_INDICES(3) = 3
      ENDIF

      IF ((DABS(VEC(2)) <= DABS(VEC(3))) .AND. (DABS(VEC(2)) <= DABS(VEC(1))) .AND. (DABS(VEC(3)) <= DABS(VEC(1)))) THEN
         SORT_ORDER      = '2 3 1'
         SORT_INDICES(1) = 2
         SORT_INDICES(2) = 3
         SORT_INDICES(3) = 1
      ENDIF

      IF ((DABS(VEC(3)) <= DABS(VEC(1))) .AND. (DABS(VEC(3)) <= DABS(VEC(2))) .AND. (DABS(VEC(1)) <= DABS(VEC(2)))) THEN
         SORT_ORDER      = '3 1 2'
         SORT_INDICES(1) = 3
         SORT_INDICES(2) = 1
         SORT_INDICES(3) = 2
      ENDIF

      IF ((DABS(VEC(3)) <= DABS(VEC(1))) .AND. (DABS(VEC(3)) <= DABS(VEC(2))) .AND. (DABS(VEC(2)) <= DABS(VEC(1)))) THEN
         SORT_ORDER      = '3 2 1'
         SORT_INDICES(1) = 3
         SORT_INDICES(2) = 2
         SORT_INDICES(3) = 1
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

      END SUBROUTINE CALC_VEC_SORT_ORDER

