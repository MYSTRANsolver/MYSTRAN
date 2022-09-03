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
 
      SUBROUTINE SOLVE_PHIZL1 ( NTERM_CRS3 )


! Solves KLL*PHIZL1 = CRS3 for PHIZL1   where CRS3 = (MLR + MLL*DLR).
 
! For a description of Craig-Bamptom analyses, see Appendix D to the MYSTRAN User's Referance Manual


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  FILE_NAM_MAXLEN, WRT_ERR, WRT_LOG, ERR, F04, F06, SCR
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FACTORED_MATRIX, FATAL_ERR, KLL_SDIA, NDOFR, NDOFL, NTERM_DLR,              &
                                         NTERM_PHIZL1, NTERM_KLL, NTERM_KLLs
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, TSEC
      USE PARAMS, ONLY                :  EPSIL, SOLLIB, SPARSE_FLAVOR, SPARSTOR
      USE SUBR_BEGEND_LEVELS, ONLY    :  SOLVE_PHIZL1_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE SCRATCH_MATRICES, ONLY      :  I_CRS3, J_CRS3, CRS3
      USE SPARSE_MATRICES, ONLY       :  I2_PHIZL1, I_PHIZL1, J_PHIZL1, PHIZL1, I2_PHIZL1t, I_PHIZL1t, J_PHIZL1t, PHIZL1t,         &
                                         I_KLL, I2_KLL, J_KLL, KLL, I_KLLs, I2_KLLs, J_KLLs, KLLs
      USE LAPACK_LIN_EQN_DPB

      USE SOLVE_PHIZL1_USE_IFs

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SOLVE_PHIZL1  '
      CHARACTER(  1*BYTE)             :: CLOSE_IT          ! Input to subr READ_MATRIX_i. 'Y'/'N' whether to close a file or not 
      CHARACTER(  8*BYTE)             :: CLOSE_STAT        ! What to do with file when it is closed
      CHARACTER(  1*BYTE)             :: EQUED             ! 'Y' if KLL stiff matrix was equilibrated in subr EQUILIBRATE    
      CHARACTER( 24*BYTE)             :: MESSAG            ! File description. Input to subr UNFORMATTED_OPEN 
      CHARACTER( 24*BYTE)             :: MODNAM1           ! Name to write to screen to describe module being run
      CHARACTER(  1*BYTE)             :: READ_NTERM        ! 'Y' or 'N' Input to subr READ_MATRIX_1 
      CHARACTER(  1*BYTE)             :: NULL_COL          ! 'Y' if a col of CRS3 is null 
      CHARACTER(  1*BYTE)             :: OPND              ! Input to subr READ_MATRIX_i. 'Y'/'N' whether to open  a file or not 
      CHARACTER(FILE_NAM_MAXLEN*BYTE) :: SCRFIL            ! File name
 
      INTEGER(LONG), INTENT(IN)       :: NTERM_CRS3        ! Number of terms in matrix CRS3  
      INTEGER(LONG)                   :: DEB_PRT(2)        ! Debug numbers to say whether to write ABAND and/or its decomp to output
!                                                            file in called subr SYM_MAT_DECOMP_LAPACK (ABAND = band form of KOO)

      INTEGER(LONG)                   :: I,J               ! DO loop indices or counters
      INTEGER(LONG)                   :: INFO        = 0   ! Input value for subr SYM_MAT_DECOMP_LAPACK (quit on sing KRRCB)
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening a file
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN   
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SOLVE_PHIZL1_BEGEND

      REAL(DOUBLE)                    :: EPS1              ! A small number to compare real zero

      REAL(DOUBLE)                    :: EQUIL_SCALE_FACS(NDOFL)
                                                           ! LAPACK_S values returned from subr SYM_MAT_DECOMP_LAPACK

      REAL(DOUBLE)                    :: INOUT_COL(NDOFL)  ! Temp variable for subr FBS
      REAL(DOUBLE)                    :: K_INORM           ! Inf norm of KLL matrix (det in  subr COND_NUM)
      REAL(DOUBLE)                    :: PHIZL1_COL(NDOFL)   ! A column of PHIZL1   solved for herein
      REAL(DOUBLE)                    :: RCOND             ! Recrip of cond no. of the KLL. Det in  subr COND_NUM
 
      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

! Solve for PHIZL1  

! Open a scratch file that will be used to write PHIZL1   nonzero terms to as we solve for columns of PHIZL1  . After all col's
! of PHIZL1   have been solved for, and we have a count on NTERM_PHIZL1  , we will allocate memory to the PHIZL1   arrays and read
! the scratch file values into those arrays. Then, in the calling subroutine, we will write NTERM_PHIZL1  , followed by
! PHIZL1    row/col/value to a permanent file

      OUNT(1) = ERR
      OUNT(2) = F06
      SCRFIL(1:)  = ' '
      SCRFIL(1:9) = 'SCRATCH-991'
      OPEN (SCR(1),STATUS='SCRATCH',FORM='UNFORMATTED',ACTION='READWRITE',IOSTAT=IOCHK)
      IF (IOCHK /= 0) THEN
         CALL OPNERR ( IOCHK, SCRFIL, OUNT, 'Y' )
         CALL FILE_CLOSE ( SCR(1), SCRFIL, 'DELETE', 'Y' )
         CALL OUTA_HERE ( 'Y' )                            ! Can't open scratch file, so quit
      ENDIF
 
! Loop on columns of CRS3
 
!xx   WRITE(SC1, * )                                       ! Advance 1 line for screen messages

      NTERM_PHIZL1   = 0
      DO J = 1,NDOFR

         CALL OURTIM
         MODNAM1 = '   Solve for PHIZL1 col '
         WRITE(SC1,12345,ADVANCE='NO') MODNAM1, J, NDOFR, CR13

! To solve for the j-th col of PHIZL1, use the j-th col of CRS3 (= MLR + MLL*DLR) as a rhs vector. Get the j-th col of CRS3 and put
! the negative of it into array INOUT_COL:

         NULL_COL = 'Y'
         DO I=1,NDOFL
            INOUT_COL(I) = ZERO
            PHIZL1_COL(I)  = ZERO
         ENDDO 
         CALL GET_SPARSE_CRS_COL ( 'MLR + MLL*DLR', J,  NTERM_CRS3, NDOFL, NDOFR, I_CRS3, J_CRS3, CRS3, -ONE, INOUT_COL, NULL_COL )

! Calculate PHIZL1_COL via forward/backward substitution.

         IF (NULL_COL == 'N') THEN                         ! FBS will solve for PHIZL1_COL & load it into PHIZL1   array
                                                           ! DPBTRS will return PHIZL1_COL = -KLL(-1)*RHS_col
!                                                            Note 1st arg = 'N' assures that EQUIL_SCAL_FACS will not be used
            IF      (SOLLIB == 'BANDED  ') THEN

               CALL FBS_LAPACK ( 'N', NDOFL, KLL_SDIA, EQUIL_SCALE_FACS, INOUT_COL )

            ELSE IF (SOLLIB == 'SPARSE  ') THEN

               IF (SPARSE_FLAVOR(1:7) == 'SUPERLU') THEN

                  INFO = 0
                  CALL FBS_SUPRLU ( SUBR_NAME, 'KLL', NDOFL, NTERM_KLL, I_KLL, J_KLL, KLL, J, INOUT_COL, INFO )
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

            DO I=1,NDOFL
               PHIZL1_COL(I) = INOUT_COL(I)
            ENDDO
            DO I=1,NDOFL                                   ! Count NTERM_PHIZL1   and write nonzero PHIZL1   to scratch file
               IF (DABS(PHIZL1_COL(I)) > EPS1) THEN
                  NTERM_PHIZL1   = NTERM_PHIZL1   + 1
                  WRITE(SCR(1)) I, J, PHIZL1_COL(I)
               ENDIF
            ENDDO 
         ENDIF
  
      ENDDO
  
      call deallocate_sparse_mat ( 'KLLs' )

! The PHIZL1 data in SCRATCH-991 is written one col at a time for PHIZL1. Therefore it is rows of PHIZL1t

      REWIND (SCR(1))
      MESSAG = 'SCRATCH: PHIZL1 ROW/COL/VAL'
      READ_NTERM = 'N'
      OPND       = 'Y'
      CLOSE_IT   = 'N'
      CLOSE_STAT = 'KEEP    '

      CALL ALLOCATE_SPARSE_MAT ( 'PHIZL1t', NDOFR, NTERM_PHIZL1, SUBR_NAME )
      CALL ALLOCATE_L6_2 ( 'PHIZL1t', SUBR_NAME )

      CALL ALLOCATE_SPARSE_MAT ( 'PHIZL1', NDOFL, NTERM_PHIZL1, SUBR_NAME )
      CALL ALLOCATE_L6_2 ( 'PHIZL1', SUBR_NAME )
                                                           ! J_PHIZL1t is same as I2_PHIZL1 and I2_PHIZL1t is same as J_PHIZL1
      CALL READ_MATRIX_2 ( SCRFIL, SCR(1), OPND, CLOSE_IT, CLOSE_STAT, MESSAG, 'PHIZL1t', NDOFL, NTERM_PHIZL1, READ_NTERM,         &
                           J_PHIZL1t, I2_PHIZL1t, PHIZL1t)

! Now get PHIZL1 from PHIZL1t

      CALL GET_I_MAT_FROM_I2_MAT ( 'PHIZL1t', NDOFR, NTERM_PHIZL1, I2_PHIZL1t, I_PHIZL1t )

      CALL MATTRNSP_SS ( NDOFR, NDOFL, NTERM_PHIZL1, 'PHIZL1t', I_PHIZL1t, J_PHIZL1t, PHIZL1t, 'PHIZL1', I_PHIZL1, J_PHIZL1, PHIZL1)

      CALL FILE_CLOSE ( SCR(1), SCRFIL, 'DELETE', 'Y' )

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  932 FORMAT(' *ERROR   932: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' PARAMETER SPARSTOR MUST BE EITHER "SYM" OR "NONSYM" BUT VALUE IS ',A)

 2092 FORMAT(4X,A44,20X,I2,':',I2,':',I2,'.',I3)

 6001 FORMAT(' *ERROR  6001: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' MATRIX KLL WAS EQUILIBRATED: EQUED = ',A,'. CODE NOT WRITTEN TO ALLOW THIS AS YET')

 9991 FORMAT(' *ERROR  9991: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,A, ' = ',A,' NOT PROGRAMMED ',A)

12345 FORMAT(3X,A,I8,' of ',I8,A) 








! **********************************************************************************************************************************
 
      END SUBROUTINE SOLVE_PHIZL1          
