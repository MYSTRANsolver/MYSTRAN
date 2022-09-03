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
 
      SUBROUTINE ELESORT
     
! Performs 3 functions:

!   (1) Generates ESORT1 array (actual elem ID's) and ESORT2 array (internal elem no.) for all elements.

!   (2) Sorts ESORT1, ESORT2, EPNT, ETYPE, EOFF so that elem ID's in ESORT1 are in numerically increasing order.

!   (3) Sorts RIGID_ELEM_IDS so that the actual ID's of rigid elements are in numerically increasing order

!   (4) Checks for redundant element ID's - elastic as well as rigid elements
 
! At the beginning of the subroutine, ESORT1(I) is set to element ID's in the order in which they were encountered
! in the Bulk Data Deck and ESORT2(I) = I. EPNT(I) gives the location in EDAT where connection data starts for element
! ESORT1(I) and ETYPE(I) is the element type for element ESORT1(I).
 
! This subr then sorts ESORT1, ESORT2, EPNT and ETYPE (together) so that the actual element numbers in ESORT1 are in
! numerical order. Then ESORT2(I) is the position, in the Bulk Data Deck (BDD), where actual elem ESORT1(I) was located.
  
! For example:
 
!    EID's in    |    At Beginning of Subr  |   At End of Subroutine:
!     Order      |        (after (1))       |
!    as input    |                          |                        
!    in BDD    I | ESORT1 ESORT2 EPNT ETYPE | ESORT1 ESORT2 EPNT ETYPE
!   -------------|--------------------------|-------------------------
!       31     1 |     31      1    1    B1 |     11      2    9    E1
!       11     2 |     11      2    9    E1 |     21      4   19    T2
!       41     3 |     41      3   15    R1 |     31      1    1    B1
!       21     4 |     21      4   19    T2 |     41      3   15    R1
!       61     5 |     61      5   24    Q2 |     51      6   30    B1
!       51     6 |     51      6   30    B1 |     61      5   24    Q2
     
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, ELESORT_RUN, NELE, NRIGEL
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELESORT_BEGEND
      USE MODEL_STUF, ONLY            :  EDAT, EOFF, EPNT, ESORT1, ESORT2, ETYPE, RIGID_ELEM_IDS
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      USE ELESORT_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ELESORT'

      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IERROR            ! Error count
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELESORT_BEGEND
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! (1) Generate ESORT1 and ESORT2. Initially, set ESORT1(I) = elem ID's in order read in Bulk Data and ESORT2(I) = I.

      DO I=1,NELE
         ESORT1(I) = EDAT(EPNT(I))
         ESORT2(I) = I
      ENDDO

      IF (DEBUG(7) == 1) THEN                              ! Debug output before sorting ESORT1, ESORT2, EPNT, ETYPE
         WRITE(F06,1001)
         WRITE(F06,1011)
         DO I=1,NELE
            WRITE(F06,1021) I,ESORT1(I),ESORT2(I),EPNT(I),ETYPE(I)
         ENDDO
         WRITE(F06,*)
      ENDIF

! (2) Sort ESORT1, ESORT2, EPNT, ETYPE so that ESORT1 has actual element ID's in numerically increasing order

      IF (NELE > 1) THEN
         CALL SORT_INT3_CHAR2 ( SUBR_NAME, 'ESORT1, ESORT2, EPNT, ETYPE, EOFF', NELE, ESORT1, ESORT2, EPNT, ETYPE, EOFF )
      ENDIF
 
      IF (DEBUG(7) == 1) THEN                              ! Debug output after sorting ESORT1, ESORT2, EPNT, ETYPE
         WRITE(F06,1002)
         WRITE(F06,1011)
         DO I=1,NELE
            WRITE(F06,1021) I,ESORT1(I),ESORT2(I),EPNT(I),ETYPE(I)
         ENDDO
         WRITE(F06,*)
      ENDIF

! (3) Sort RIGID_ELEM_IDS

      IF (NRIGEL > 1) THEN
         CALL SORT_INT1 ( SUBR_NAME, 'RIGID_ELEM_IDS', NRIGEL, RIGID_ELEM_IDS )
      ENDIF

! (4) Check for duplicate element numbers

      IERROR = 0

      DO I=1,NELE-1                                        ! Elastic elements
         IF (ESORT1(I) == ESORT1(I+1)) THEN
            IERROR = IERROR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1400) ESORT1(I+1)
            WRITE(F06,1400) ESORT1(I+1)
         ENDIF
      ENDDO

      DO I=1,NRIGEL-1                                      ! Rigid elements
         IF (RIGID_ELEM_IDS(I) == RIGID_ELEM_IDS(I+1)) THEN
            IERROR = IERROR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1400) RIGID_ELEM_IDS(I+1)
            WRITE(F06,1400) RIGID_ELEM_IDS(I+1)
         ENDIF
      ENDDO

      IF (IERROR > 0) THEN
         WRITE(ERR,1408) IERROR
         WRITE(F06,1408) IERROR
         CALL OUTA_HERE ( 'Y' )                                    ! Duplicate elem numbers, so quit
      ENDIF   

! Set ELESORT_RUN so subrs which need to know if ELESORT subr has run, will know it has run

      ELESORT_RUN = 'Y'

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1001 FORMAT('             Element arrays before sorting in subroutine ELESORT:')
                     
 1002 FORMAT('             Element arrays after sorting in subroutine ELESORT:')
                     
 1011 FORMAT('            I   ESORT1(I)   ESORT2(I)     EPNT(I)       ETYPE(I)')

 1021 FORMAT(1X,4I12,10X,A)

 1400 FORMAT(' *ERROR  1400: ELEMENT NUMBER ',I8,' IS A DUPLICATE ELEMENT NUMBER.')

 1408 FORMAT(' PROCESSING TERMINATED DUE TO ABOVE ',I8,' ERRORS')

! **********************************************************************************************************************************
 
      END SUBROUTINE ELESORT
