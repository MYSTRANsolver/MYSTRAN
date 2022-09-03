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
 
      SUBROUTINE SPARSE_PG

! Convert full array SYS_LOAD of all loads for all subcases to sparse array PG and write data to file LINK1E. The data that is
! written (for nonzero loads) is: G-set DOF number, internal subcase number, non-zero load value  

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, L1E, L1E_MSG, L1ESTAT, LINK1E, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  FATAL_ERR, NDOFG, NSUB, NTERM_PG, BLNK_SUB_NAM, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  SPARSE_PG_BEGEND
      USE PARAMS, ONLY                :  EPSIL, PRTFOR
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP, NL_NUM_LOAD_STEPS
      USE MODEL_STUF, ONLY            :  SYS_LOAD
      USE SPARSE_MATRICES, ONLY       :  I_PG, J_PG, PG
 
      USE SPARSE_PG_USE_IFs

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SPARSE_PG'

      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: KTERM_PG          ! Count of the number of terms written to file L1E for PG loads
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SPARSE_PG_BEGEND
 
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare real zero

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

! Open L1E to write G-set loads to.
  
      OUNT(1) = ERR
      OUNT(2) = F06
      CALL FILE_OPEN ( L1E, LINK1E, OUNT, 'REPLACE', L1E_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )

! Count the nonzero's in SYS_LOAD

      DO I=1,NDOFG
         DO J=1,NSUB
            IF (DABS(SYS_LOAD(I,J)) > EPS1) THEN
               NTERM_PG = NTERM_PG + 1
            ENDIF
         ENDDO 
      ENDDO 

! Allocate PG sparse arrays

      CALL ALLOCATE_SPARSE_MAT ( 'PG', NDOFG, NTERM_PG, SUBR_NAME )

! Formulate I_PG

      I_PG(1) = 1
      DO I=1,NDOFG
         I_PG(I+1) = I_PG(I)
         DO J=1,NSUB
            IF (DABS(SYS_LOAD(I,J)) > EPS1) THEN
               I_PG(I+1) = I_PG(I+1) + 1
!xx?????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
!xx         ELSE
!xx            I_PG(I+1) = I_PG(I)
!xx 11/06/08: THESE LINES SHOULD BE COMMENTED OUT SINCE, IF SYS_LOAD(I,J) IS ZERO, WE DON'T WANT THAT TERM IN PG. ALLOWING THE ABOVE
!xx           TO BE IN THE CODE MADE A SERIOUS ERROR IN RUN TRUSS-2-CRODS THAT I DIDN'T DISCOVER UNTIL THIS DATE
!xx?????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
            ENDIF
         ENDDO
      ENDDO 

! Write the systems loads matrix to L1E in sparse form

      WRITE(L1E) NTERM_PG
      KTERM_PG = 0
      WRITE(SC1, * )
      DO I=1,NDOFG                                         ! Inner loop must be over subcases so that READ_MATRIX_1 will be reading
         DO J=1,NSUB                                       ! one row of sparse PG after another
            WRITE(SC1,12345,ADVANCE='NO') I, NDOFG, J, NSUB, CR13
            IF (DABS(SYS_LOAD(I,J)) > EPS1) THEN
               KTERM_PG = KTERM_PG + 1
               J_PG(KTERM_PG) = J
               IF ((SOL_NAME(1:8) == 'DIFFEREN') .OR. (SOL_NAME(1:8) == 'NLSTATIC')) THEN
                  IF (NL_NUM_LOAD_STEPS > 0) THEN
                     SYS_LOAD(I,J) = SYS_LOAD(I,J)*LOAD_ISTEP/NL_NUM_LOAD_STEPS
                  ELSE
                     FATAL_ERR = FATAL_ERR + 1
                     WRITE(ERR,965) SUBR_NAME, NL_NUM_LOAD_STEPS
                     WRITE(F06,965) SUBR_NAME, NL_NUM_LOAD_STEPS
                     CALL OUTA_HERE ( 'Y' )
                  ENDIF
               ENDIF
               PG(KTERM_PG) = SYS_LOAD(I,J)
               WRITE(L1E) I, J, SYS_LOAD(I,J)
            ENDIF
         ENDDO 
      ENDDO

      WRITE(SC1,*) CR13

      IF (KTERM_PG /= NTERM_PG) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1627) KTERM_PG, NTERM_PG
         WRITE(F06,1627) KTERM_PG, NTERM_PG
      ENDIF

   
      IF (NTERM_PG > 0) THEN
         CALL FILE_CLOSE ( L1E, LINK1E, 'KEEP', 'Y' )
      ELSE
         CALL FILE_CLOSE ( L1E, LINK1E, L1ESTAT, 'Y' )
      ENDIF
  
      IF (PRTFOR(1) == 1) THEN                             ! Print PG if requested
         IF (NTERM_PG > 0) THEN
            CALL WRITE_SPARSE_CRS ( 'G-SET LOADS, MATRIX PG', 'G ', 'SUBCASE', NTERM_PG, NDOFG, I_PG, J_PG, PG )
         ENDIF
      ENDIF

! **********************************************************************************************************************************
  965 FORMAT(' *ERROR   965: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' NONLINEAR PARAMETER NL_NUM_LOAD_STEPS MUST BE > 0 BUT VALUE WAS ',I8)

 1627 FORMAT(' *ERROR  1614: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' THE NUMBER OF G-SET LOAD MATRIX RECORDS WRITTEN TO FILE:'                                             &
                    ,/,15X,A                                                                                                       &
                    ,/,14X,' WAS KTERM_PG = ',I12,'. IT SHOULD HAVE BEEN NTERM_PG = ',I12)

12345 FORMAT(5X,'G-set DOF ',I8,' of ',I8,', subcase ',I8,' of ',I8, A)

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 
      END SUBROUTINE SPARSE_PG
