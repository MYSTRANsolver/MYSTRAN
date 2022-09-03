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
 
      SUBROUTINE PARTITION_VEC ( NDOF_X, CSET_X, CSET_1, CSET_2, PART_VEC )
                              
! Creates a partitioning vector, PART_VEC, containing 1's and 2's in a vector for displ set CSET_X:
!   The 1's are for the DOF's belonging to displ set CSET_1 (a subset of CSET_X)
!   The 2's are for the DOF's belonging to displ set CSET_2 (the compliment of CSET_1 in CSET_X)

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFG
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  PARTITION_VEC_BEGEND
      USE DOF_TABLES, ONLY            :  TDOFI
 
      USE PARTITION_VEC_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'PARTITION_VEC'
      CHARACTER(LEN=*), INTENT(IN )   :: CSET_X            ! Char description of displ set (e.g. 'A ' or 'SG')
      CHARACTER(LEN=*), INTENT(IN )   :: CSET_1            ! Char description of displ set (e.g. 'A ' or 'SG')
      CHARACTER(LEN=*), INTENT(IN )   :: CSET_2            ! Char description of displ set (e.g. 'A ' or 'SG')

      INTEGER(LONG), INTENT(IN )      :: NDOF_X            ! No. DOF's in CSET_X displ set
      INTEGER(LONG), INTENT(OUT)      :: PART_VEC(NDOF_X)  ! The partitioning vector described above
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IDOF              ! Internal DOF no. for a DOF in CSET_X
      INTEGER(LONG)                   :: SET_X             ! Col no. in array TDOFI where the DOF list is for CSET_X
      INTEGER(LONG)                   :: SET_1             ! Col no. in array TDOFI where the DOF list is for CSET_1
      INTEGER(LONG)                   :: SET_2             ! Col no. in array TDOFI where the DOF list is for CSET_2
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = PARTITION_VEC_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      DO I=1,NDOF_X
         PART_VEC(I) = 0
      ENDDO

! Get TDOF column numbers

      CALL TDOF_COL_NUM ( CSET_X, SET_X )
      CALL TDOF_COL_NUM ( CSET_1, SET_1 )
      CALL TDOF_COL_NUM ( CSET_2, SET_2 )

! Create partitioning vector

      DO I=1,NDOFG

         IDOF = TDOFI(I,SET_X)
         IF (IDOF > 0) THEN
            IF      (TDOFI(I,SET_1) > 0) THEN
               PART_VEC(IDOF) = 1
            ELSE IF (TDOFI(I,SET_2) > 0) THEN
               PART_VEC(IDOF) = 2
            ELSE
               WRITE(ERR,931) SUBR_NAME, CSET_X, CSET_1, CSET_2
               WRITE(F06,931) SUBR_NAME, CSET_X, CSET_1, CSET_2
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )                            ! Coding error (2 sets not complimentary), so quit
            ENDIF
         ENDIF

      ENDDO


! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  931 FORMAT(' *ERROR   931: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' ATTEMPT TO CREATE PARTITIONING VECTOR FROM 2 SETS THAT ARE NOT COMPLIMENTARY TO THE OUTPUT SET'       &
                    ,/,14X,' THE SET TO PARTITION IS "',A,'". THE SETS TO PARTITION IT INTO ARE "',A,'" AND "',A,'"')


! **********************************************************************************************************************************

      END SUBROUTINE PARTITION_VEC
            
