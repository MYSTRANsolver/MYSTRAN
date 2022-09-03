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
 
      SUBROUTINE SOLVE_UO0

! Solves KOO*UO0 = PO for matrix UO0 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06, L2F, LINK2F, L2F_MSG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FACTORED_MATRIX, FATAL_ERR, KOO_SDIA, NDOFO, NSUB, NTERM_KOO, NTERM_PO
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  SOLVE_UO0_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE PARAMS, ONLY                :  PRTUO0, SOLLIB, SPARSE_FLAVOR
      USE SPARSE_MATRICES, ONLY       :  I_PO, J_PO, PO, I_KOO, J_KOO, KOO
      USE COL_VECS, ONLY              :  UO0_COL
      USE LAPACK_LIN_EQN_DPB
 
! Interface module not needed for subr DPBTRS. This is "CONTAIN'ed" in module LAPACK_LIN_EQN_DPB, which is "USE'd" above

      USE SOLVE_UO0_USE_IFs

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SOLVE_UO0'
      CHARACTER( 1*BYTE)              :: NULL_COL          ! 'Y' if a col of KAO(transpose) is null 
      CHARACTER(22*BYTE)              :: MODNAM1           ! Name to write to screen to describe module being run
 
      INTEGER(LONG)                   :: I,J               ! DO loop indices or counters
      INTEGER(LONG)                   :: INFO              ! Info on success of SuperLU solve
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SOLVE_UO0_BEGEND

      REAL(DOUBLE)                    :: NULL_SCALE_FACS(NDOFO)
                                                           ! LAPACK_S values not used so null this vector
      REAL(DOUBLE)                    :: INOUT_COL(NDOFO)  ! Temp variable for one col of load matrix PO

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Make units for writing errors the screen and output file
 
      OUNT(1) = ERR
      OUNT(2) = F06
 
! For SOLLIB = BANDED, Subr REDUCE_KFF_TO_KAA should have done the decomp of KOO, so here we run FBS over the columns of PO to get
! UO0. First make sure that ABAND (KOO triangular factor) was successfully generated when SOLVE_GOA was run.

      IF (SOLLIB == 'BANDED  ') THEN
         IF (FACTORED_MATRIX(1:3) /= 'KOO') THEN
            WRITE(ERR,2504) SUBR_NAME, FACTORED_MATRIX
            WRITE(F06,2504) SUBR_NAME, FACTORED_MATRIX
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )
         ENDIF
      ENDIF

! Open file for writing UO0
                                                           ! Write GOA matrix to file L2F
      CALL FILE_OPEN ( L2F, LINK2F, OUNT, 'REPLACE', L2F_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )

! **********************************************************************************************************************************
! Solve for UO0 by looping on columns of PO ("loads") to get columns of UO0 ("displs")
 
!xx   WRITE(SC1, * )                                       ! Advance 1 line for screen messages

      CALL ALLOCATE_COL_VEC ( 'UO0_COL', NDOFO, SUBR_NAME )
      DO J = 1,NSUB

         DO I=1,NDOFO
            INOUT_COL(I)       = ZERO                      ! Initialize INOUT_COL since GET_SPARSE_CRS_COL won't
            NULL_SCALE_FACS(I) = ZERO                      ! Initialize these (not used here snce KOO not equilibrated
         ENDDO  
         NULL_COL = 'Y'
         CALL GET_SPARSE_CRS_COL ( 'PO',J, NTERM_PO, NDOFO, NSUB, I_PO, J_PO, PO, ONE, INOUT_COL, NULL_COL )

         IF (NULL_COL == 'N') THEN                         ! Solve for UO0_COL

            CALL OURTIM
            MODNAM1 = '    Solve for UO0 col '
            WRITE(SC1,12345,ADVANCE='NO') MODNAM1, J, NSUB, CR13
                                                           ! FBS should not equilibrate since KOO was prevented from equilibrating
            IF      (SOLLIB == 'BANDED  ') THEN

               CALL FBS_LAPACK ( 'N', NDOFO, KOO_SDIA, NULL_SCALE_FACS, INOUT_COL )

            ELSE IF (SOLLIB == 'SPARSE  ') THEN

               IF (SPARSE_FLAVOR(1:7) == 'SUPERLU') THEN

                  INFO = 0
                  CALL FBS_SUPRLU ( SUBR_NAME, 'KOO', NDOFO, NTERM_KOO, I_KOO, J_KOO, KOO, J, INOUT_COL, INFO )

               ELSE

                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,9991) SUBR_NAME, 'SPARSE_FLAVOR'
                  WRITE(F06,9991) SUBR_NAME, 'SPARSE_FLAVOR'
                  CALL OUTA_HERE ( 'Y' )

               ENDIF


           ELSE

               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,9991) SUBR_NAME, 'SOLLIB'
               WRITE(F06,9991) SUBR_NAME, 'SOLLIB'
               CALL OUTA_HERE ( 'Y' )

            ENDIF

            DO I=1,NDOFO
               UO0_COL(I) = INOUT_COL(I)
            ENDDO

         ELSE

            DO I=1,NDOFO
               UO0_COL(I) = ZERO
            ENDDO 

         ENDIF

! Write UO0 for this S/C to L2F

         DO I=1,NDOFO
            WRITE(L2F) UO0_COL(I)
         ENDDO          

         IF (PRTUO0 > 0) THEN
            WRITE(F06,201) J
            CALL WRITE_VECTOR ( 'UO0 = KOO(-1)*PO', 'DISPL', NDOFO, UO0_COL)
         ENDIF

      ENDDO
 
      WRITE(SC1,*) CR13

      CALL DEALLOCATE_COL_VEC ( 'UO0_COL' )

      CALL FILE_CLOSE ( L2F, LINK2F, 'KEEP', 'Y' )

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  201 FORMAT(59X,'COLUMN',I6)

 2504 FORMAT(' *ERROR  2504: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' THE NAME OF MATRIX THAT HAS BEEN DECOMPOSED INTO TRIANGULAR FACTORS SHOULD BE "KOO".'                 &
                    ,/,14X,' HOWEVER, IT IS NAMED "',A,'". CANNOT CONTINUE')

 9991 FORMAT(' *ERROR  9991: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,A, ' = ',A,' NOT PROGRAMMED ',A)

12345 FORMAT(3X,A,I8,' of ',I8,A)

! **********************************************************************************************************************************
 
      END SUBROUTINE SOLVE_UO0        
