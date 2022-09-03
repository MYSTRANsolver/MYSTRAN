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
 
      SUBROUTINE MERGE_COL_VECS ( IN1_COL, IN1_NDOF, UIN1, IN2_COL, IN2_NDOF, UIN2  &
                        ,OUT_COL, OUT_NDOF, UOUT )

! Merges 2 input column vectors UIN1 (belonging to displ set IN1_COL) and UIN2 (belonging to displ set IN2_COL) into one  output
! column vector UOUT (belonging to displ set OUT_COL). All column vectors are stored in full format

! If the 2 input vector displ sets being merged are not complementary to the output vector displ set, a coding error is given
! and processing is stopped.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFG
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  MERGE_COL_VECS_BEGEND
      USE DOF_TABLES, ONLY            :  TDOFI
      
      USE MERGE_COL_VECS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MERGE_COL_VECS'

      INTEGER(LONG), INTENT(IN )      :: IN1_COL           ! Column number in TDOF, TDOFI for the displ set for input vector UIN1
      INTEGER(LONG), INTENT(IN )      :: IN2_COL           ! Column number in TDOF, TDOFI for the displ set for input vector UIN2
      INTEGER(LONG), INTENT(IN )      :: OUT_COL           ! Column number in TDOF, TDOFI for the displ set for input vector UOUT
      INTEGER(LONG), INTENT(IN )      :: IN1_NDOF          ! Size of array UIN1
      INTEGER(LONG), INTENT(IN )      :: IN2_NDOF          ! Size of array UIN2
      INTEGER(LONG), INTENT(IN )      :: OUT_NDOF          ! Size of array UOUT
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IN1_DOF           ! IN1_COL DOF number in array TDOFI of a term in UIN1
      INTEGER(LONG)                   :: IN2_DOF           ! IN2_COL DOF number in array TDOFI of a term in UIN2
      INTEGER(LONG)                   :: OUT_DOF           ! OUT_COL DOF number in array TDOFI of a term in UOUT
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MERGE_COL_VECS_BEGEND

      REAL(DOUBLE) , INTENT(IN )      :: UIN1(IN1_NDOF)    ! Input  vector for IN1_COL displ set
      REAL(DOUBLE) , INTENT(IN )      :: UIN2(IN2_NDOF)    ! Input  vector for IN2_COL displ set
      REAL(DOUBLE) , INTENT(OUT)      :: UOUT(OUT_NDOF)    ! Output vector for OUT_COL displ set
 
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      DO I=1,OUT_NDOF
         UOUT(I) = ZERO
      ENDDO

! Merge rows of UIN1 and UIN2 into UOUT

      DO I=1,NDOFG
    
         IN1_DOF = TDOFI(I,IN1_COL)
         IN2_DOF = TDOFI(I,IN2_COL)
         OUT_DOF = TDOFI(I,OUT_COL)

         IF ((IN1_DOF > IN1_NDOF) .OR. (IN2_DOF > IN2_NDOF) .OR. (OUT_DOF > OUT_NDOF)) THEN
            WRITE(ERR,938) SUBR_NAME,IN1_DOF,IN1_NDOF,IN2_DOF,IN2_NDOF,OUT_DOF,OUT_NDOF 
            WRITE(F06,938) SUBR_NAME,IN1_DOF,IN1_NDOF,IN2_DOF,IN2_NDOF,OUT_DOF,OUT_NDOF 
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )                                 ! Coding error (attempt to exceed allocated array size), so quit
         ENDIF            

         IF (OUT_DOF > 0) THEN
            IF      (IN1_DOF > 0) THEN
               UOUT(OUT_DOF) = UIN1(IN1_DOF)
            ELSE IF (IN2_DOF > 0) THEN
               UOUT(OUT_DOF) = UIN2(IN2_DOF)
            ELSE
               WRITE(ERR,917)
               WRITE(F06,917)
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )                              ! Coding error (vectors can't be merged), so quit
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
  938 FORMAT(' *ERROR   938: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' ATTEMPT TO EXCEED ALLOCATED ARRAY SIZE:'                                                              &
                    ,/,14X,' EITHER IN1_DOF = ',I8,' EXCEEDS IN1_NDOF = ',I8                                                       &
                    ,/,14X,' OR     IN2_DOF = ',I8,' EXCEEDS IN2_NDOF = ',I8                                                       &
                    ,/,14X,' OR     OUT_DOF = ',I8,' EXCEEDS OUT_NDOF = ',I8  )

  917 FORMAT(' *ERROR   917: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' ATTEMPT TO MERGE 2 VECTORS THAT ARE NOT COMPLIMENTARY TO THE OUTPUT VECTOR')

! **********************************************************************************************************************************

      END SUBROUTINE MERGE_COL_VECS
