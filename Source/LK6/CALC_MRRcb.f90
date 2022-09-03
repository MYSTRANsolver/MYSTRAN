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

      SUBROUTINE CALC_MRRcb
 
! Calculates the R-set row and col matrix MRRcb in the CB transformation matrix:

!            MRRcb = MRR + MRL*DLR + (MRL*DLR)' + DLR'*MLL*DLR
 
! For a description of Craig-Bamptom analyses, see Appendix D to the MYSTRAN User's Referance Manual


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFL, NDOFR, NTERM_DLR, NTERM_MLL, NTERM_MRL, NTERM_MRR,        &
                                         NTERM_MRRcb, NTERM_MRRcbn
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ONE
      USE PARAMS, ONLY                :  SPARSTOR, WTMASS
      USE RIGID_BODY_DISP_MATS, ONLY  :  TR6_MEFM
      USE MODEL_STUF, ONLY            :  MEFM_RB_MASS
      USE SPARSE_MATRICES , ONLY      :  SYM_DLR, SYM_MLL, SYM_MRL, SYM_MRR, SYM_MRRcb

      USE SPARSE_MATRICES , ONLY      :  I_MLL  , J_MLL  , MLL  , I_MRL  , J_MRL  , MRL  , I_MRR  , J_MRR  , MRR  ,                &
                                         I_DLR  , J_DLR  , DLR  , I_DLRt , J_DLRt , DLRt , I_MRRcb, J_MRRcb, MRRcb,                &
                                         SYM_MRRcb

      USE SCRATCH_MATRICES
      USE SUBR_BEGEND_LEVELS, ONLY    :  CALC_MRRcb_BEGEND

      USE CALC_MRRcb_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'CALC_MRRcb'
      CHARACTER(  1*BYTE)             :: SYM_CRS1            ! Storage format for matrix CRS1 (either 'Y' for sym storage or
!                                                              'N' for nonsymmetric storage)
      CHARACTER(  1*BYTE)             :: SYM_CRS3            ! Storage format for matrix CRS3 (either 'Y' for sym storage or
!                                                              'N' for nonsymmetric storage)

      INTEGER(LONG)                   :: AROW_MAX_TERMS      ! Output from MATMULT_SFS_NTERM and input to MATMULT_SFS
      INTEGER(LONG)                   :: I,J                 ! DO loop indices
      INTEGER(LONG)                   :: NTERM_CCS1          ! Number of terms in matrix CCS1  
      INTEGER(LONG)                   :: NTERM_CRS1          ! Number of terms in matrix CRS1  
      INTEGER(LONG)                   :: NTERM_CRS2          ! Number of terms in matrix CRS2  
      INTEGER(LONG)                   :: NTERM_CRS3          ! Number of terms in matrix CRS3  
      INTEGER(LONG)                   :: NUM_MRRcb_DIAG_0    ! Number of zero diagonal terms in MRRcb
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CALC_MRRcb_BEGEND

      REAL(DOUBLE)                    :: DUMR6(NDOFR,6)      ! Intermediate matrix
                                                             ! Full representation of MRRcb
      REAL(DOUBLE)                    :: MRRcb_FULL(NDOFR,NDOFR)

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Calc MRRcb = MRR + MRL*DLR + (MRL*DLR)' + DLR'*MLL*DLR

      NTERM_MRRcb = NTERM_MRR                              ! First, allocate MRRcb and equate to MRR until we get other terms later
      CALL ALLOCATE_SPARSE_MAT ( 'MRRcb', NDOFR, NTERM_MRRcb, SUBR_NAME )

      DO I=1,NDOFR+1
         I_MRRcb(I) = I_MRR(I)
      ENDDO
      DO J=1,NTERM_MRRcb
         J_MRRcb(J) = J_MRR(J)
           MRRcb(J) =   MRR(J)
      ENDDO 
                                                           ! CCS1 will be sparse CCS format version of sparse CRS matrix DLR
      CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NDOFR, NTERM_DLR, SUBR_NAME )
      CALL SPARSE_CRS_SPARSE_CCS ( NDOFL, NDOFR, NTERM_DLR, 'DLR', I_DLR, J_DLR, DLR, 'CCS1', J_CCS1, I_CCS1, CCS1, 'Y' )

      IF (NTERM_MRL > 0) THEN                              ! Part I of MRRcb: calc MRL*DLR & add it & transpose to MRR
                                                           ! I-1 , sparse multiply to get CRS1 = MRL*DLR. Use CCS1 for DLR CCS
         CALL MATMULT_SSS_NTERM ( 'MRL' , NDOFR, NTERM_MRL , SYM_MRL, I_MRL, J_MRL,                                                &
                                  'DLR' , NDOFR, NTERM_DLR , SYM_DLR, J_CCS1, I_CCS1, AROW_MAX_TERMS,                              &
                                  'CRS1',        NTERM_CRS1 )

         CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFR, NTERM_CRS1, SUBR_NAME )

         CALL MATMULT_SSS ( 'MRL' , NDOFR, NTERM_MRL , SYM_MRL, I_MRL , J_MRL , MRL ,                                              &
                            'DLR' , NDOFR, NTERM_DLR , SYM_DLR, J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                              &
                            'CRS1', ONE  , NTERM_CRS1,          I_CRS1, J_CRS1, CRS1 )

         NTERM_CRS2 = NTERM_CRS1                           ! I-2 , allocate memory to array CRS2 which will hold transpose of CRS1
         CALL ALLOCATE_SCR_CRS_MAT ( 'CRS2', NDOFR, NTERM_CRS2, SUBR_NAME )

                                                           ! I-3 , transpose CRS1 to get CRS2 = (MRL*DLR)t
         CALL MATTRNSP_SS ( NDOFR, NDOFR, NTERM_CRS1, 'CRS1', I_CRS1, J_CRS1, CRS1, 'CRS2', I_CRS2, J_CRS2, CRS2 )

                                                           ! I-4 , sparse add to get CRS3 = CRS1 + CRS2 = (MRL*DLR) + (MRL*DLR)t
         CALL MATADD_SSS_NTERM (NDOFR,'MRL*DLR', NTERM_CRS1, I_CRS1, J_CRS1, 'N', '(MRL*DLR)t', NTERM_CRS2, I_CRS2, J_CRS2, 'N',   &
                                      'CRS3', NTERM_CRS3 )
         CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFR, NTERM_CRS3, SUBR_NAME )
         CALL MATADD_SSS ( NDOFR, 'MRL*DLR', NTERM_CRS1, I_CRS1, J_CRS1, CRS1, ONE, '(MRL*DLR)t', NTERM_CRS2, I_CRS2, J_CRS2, CRS2,&
                                  ONE, 'CRS3', NTERM_CRS3, I_CRS3, J_CRS3, CRS3 )

         CALL DEALLOCATE_SCR_MAT ( 'CRS1' )                ! I-5 , deallocate CRS1, CRS2
         CALL DEALLOCATE_SCR_MAT ( 'CRS2' )

                                                           ! I-6 , CRS3 = (MRL*DLR) + (MRL*DLR)t has all nonzero terms in it.
         IF      (SPARSTOR == 'SYM   ') THEN               !       If SPARSTOR == 'SYM   ', rewrite CRS3 as sym in CRS1     

            CALL SPARSE_CRS_TERM_COUNT ( NDOFR, NTERM_CRS3, '(MRL*DLR) + (MRL*DLR)t', I_CRS3, J_CRS3, NTERM_CRS1 )
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFR, NTERM_CRS1, SUBR_NAME )
            CALL CRS_NONSYM_TO_CRS_SYM ( 'CRS3 = (MRL*DLR) + (MRL*DLR)t all nonzeros', NDOFR, NTERM_CRS3, I_CRS3, J_CRS3, CRS3,    &
                                         'CRS1 = (MRL*DLR) + (MRL*DLR)t stored sym'  ,        NTERM_CRS1, I_CRS1, J_CRS1, CRS1 )
            SYM_CRS1 = 'Y'

         ELSE IF (SPARSTOR == 'NONSYM') THEN               !      If SPARSTOR == 'NONSYM', rewrite CRS3 in CRS1 with NTERM_CRS3

            NTERM_CRS1 = NTERM_CRS3
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFR, NTERM_CRS1, SUBR_NAME )
            DO I=1,NDOFR+1
               I_CRS1(I) = I_CRS3(I)
            ENDDO
            DO I=1,NTERM_CRS1
               J_CRS1(I) = J_CRS3(I)
                 CRS1(I) =   CRS3(I)
            ENDDO
            SYM_CRS1 = 'N'

         ELSE

            WRITE(ERR,932) SUBR_NAME, SPARSTOR
            WRITE(F06,932) SUBR_NAME, SPARSTOR
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )

         ENDIF
                                          
         CALL DEALLOCATE_SCR_MAT ( 'CRS3' )                ! 1-7 , Now CRS1 = (MRL*DLR) + (MRL*DLR)t so deallocate CRS3
                                                           ! I-8 , sparse add: CRS3 = MRR + CRS1 = MRR + (MRL*DLR) + (MRL*DLR)t
         CALL MATADD_SSS_NTERM ( NDOFR, 'MRR', NTERM_MRR, I_MRR, J_MRR, SYM_MRR, 'MRL*DLR + (MRL*DLR)t', NTERM_CRS1,               &
                                 I_CRS1, J_CRS1, SYM_CRS1, 'CRS3', NTERM_CRS3 )
         CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFR, NTERM_CRS3, SUBR_NAME )
         CALL MATADD_SSS ( NDOFR, 'MRR', NTERM_MRR, I_MRR, J_MRR, MRR, ONE, 'MRL*DLR + (MRL*DLR)t', NTERM_CRS1,                    &
                           I_CRS1, J_CRS1, CRS1, ONE, 'CRS1', NTERM_CRS3, I_CRS3, J_CRS3, CRS3 )

         CALL DEALLOCATE_SCR_MAT ( 'CRS1' )                ! I-9 , Now CRS3 = MRR + (MRL*DLR) + (MRL*DLR)t so deallocate CRS1

         NTERM_MRRcb = NTERM_CRS3                          ! I-10, allocate MRRcb to be size of CRS1

         CALL DEALLOCATE_SPARSE_MAT ( 'MRRcb' )            ! Reset MRRcb to CRS3 = 
         CALL ALLOCATE_SPARSE_MAT ( 'MRRcb', NDOFR, NTERM_MRRcb, SUBR_NAME )
                                                           ! I-11, set MRRcb = CRS3 = MRR + (MRL*DLR) + (MRL*DLR)t
         DO I=1,NDOFR+1 
            I_MRRcb(I) = I_CRS3(I)
         ENDDO
         DO J=1,NTERM_MRRcb
            J_MRRcb(J) = J_CRS3(J)
              MRRcb(J) =   CRS3(J)
         ENDDO 

         CALL DEALLOCATE_SCR_MAT ( 'CRS3' )                ! I-12, deallocate CRS3

      ENDIF

      IF (NTERM_MLL > 0) THEN                              ! Part II of MRRcb: calc DLR(t)*MLL*DLR and add to MRRcb

                                                           ! II-1 , sparse multiply to get CRS1 = MLL*DLR using CCS1 for DLR CCS
         CALL MATMULT_SSS_NTERM ( 'MLL' , NDOFL, NTERM_MLL , SYM_MLL, I_MLL , J_MLL,                                               &
                                  'DLR' , NDOFR, NTERM_DLR , SYM_DLR, J_CCS1, I_CCS1, AROW_MAX_TERMS,                              &
                                  'CRS1',        NTERM_CRS1 )

         CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFL, NTERM_CRS1, SUBR_NAME )

         CALL MATMULT_SSS ( 'MLL ', NDOFL, NTERM_MLL , SYM_MLL, I_MLL, J_MLL, MLL,                                                 &
                            'DLR' , NDOFR, NTERM_DLR , SYM_DLR, J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                              &
                            'CRS1', ONE  , NTERM_CRS1,          I_CRS1, J_CRS1, CRS1 )

         CALL DEALLOCATE_SCR_MAT ( 'CCS1' )                ! II-2 , deallocate CCS1

         NTERM_CCS1 = NTERM_CRS1                           ! II-3 , allocate CCS1 to be same as CRS1 but in CCS format
         CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NDOFR, NTERM_CCS1, SUBR_NAME )
         CALL SPARSE_CRS_SPARSE_CCS ( NDOFL, NDOFR, NTERM_CRS1, 'CRS1', I_CRS1, J_CRS1, CRS1, 'CCS1', J_CCS1, I_CCS1, CCS1, 'Y' )

         CALL DEALLOCATE_SCR_MAT ( 'CRS1' )                ! II-4 , deallocate CRS1

                                                           ! II-5 , sparse multiply to get CRS1 = DLRt*CCS1 with CCS1 = MLL*DLR
!                                                                   (note: SYM_DLR used for sym indicator for CCS1)
         CALL MATMULT_SSS_NTERM ( 'DLRt', NDOFR, NTERM_DLR , SYM_DLR, I_DLRt, J_DLRt,                                              &
                                  'CCS1', NDOFR, NTERM_CCS1, SYM_DLR, J_CCS1, I_CCS1, AROW_MAX_TERMS,                              &
                                  'CRS1 ',       NTERM_CRS1 )

         CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFR, NTERM_CRS1, SUBR_NAME )

         CALL MATMULT_SSS ( 'DLRt', NDOFR, NTERM_DLR , SYM_DLR, I_DLRt, J_DLRt, DLRt,                                              &
                            'CCS1', NDOFR, NTERM_CCS1, SYM_DLR, J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                              &
                            'CRS1', ONE  , NTERM_CRS1,          I_CRS1, J_CRS1, CRS1 )

         CALL DEALLOCATE_SCR_MAT ( 'CCS1' )                ! II-6 , deallocate CCS1

                                                           ! II-7 , CRS1 = DLRt*MLL*DLR has all nonzero terms in it.
         IF      (SPARSTOR == 'SYM   ') THEN               !        If SPARSTOR == 'SYM   ', rewrite CRS1 as sym in CRS3     

            CALL SPARSE_CRS_TERM_COUNT ( NDOFR, NTERM_CRS1, 'DLRt*MLL*DLR all nonzeros', I_CRS1, J_CRS1, NTERM_CRS3 )
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFR, NTERM_CRS3, SUBR_NAME )
            CALL CRS_NONSYM_TO_CRS_SYM ( 'CRS1 = DLRt*MLL*DLR all nonzeros', NDOFR, NTERM_CRS1, I_CRS1, J_CRS1, CRS1,              &
                                         'CRS3 = DLRt*MLL*DLR stored sym'  ,        NTERM_CRS3, I_CRS3, J_CRS3, CRS3 )
            SYM_CRS3 = 'Y'

         ELSE IF (SPARSTOR == 'NONSYM') THEN               ! If SPARSTOR == 'NONSYM', rewrite CRS3 in CRS1 with NTERM_CRS3

            NTERM_CRS3 = NTERM_CRS1
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFR, NTERM_CRS3, SUBR_NAME )
            DO I=1,NDOFR+1
               I_CRS3(I) = I_CRS1(I)
            ENDDO
            DO I=1,NTERM_CRS3
               J_CRS3(I) = J_CRS1(I)
                 CRS3(I) =   CRS1(I)
            ENDDO
            SYM_CRS3 = 'N'

         ELSE

            WRITE(ERR,932) SUBR_NAME, SPARSTOR
            WRITE(F06,932) SUBR_NAME, SPARSTOR
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )

         ENDIF
                                                           ! II-8 , sparse add to get CRS2 = prior MRRcb + CRS3 or
!                                                                   CRS2= MRR + (MRL*DLR) + (MRL*DLR)t + DLRt*MLL*DLR which is MRRcb
         CALL MATADD_SSS_NTERM ( NDOFR, 'MRR', NTERM_MRRcb, I_MRRcb, J_MRRcb, SYM_MRRcb,                                           &
                                        '(MRL*DLR) + (MRL*DLR)t + DLRt*MLL*DLR' , NTERM_CRS3 , I_CRS3 , J_CRS3 , SYM_CRS3,         &
                                        'CRS2' , NTERM_CRS2 )
         CALL ALLOCATE_SCR_CRS_MAT ( 'CRS2', NDOFR, NTERM_CRS2, SUBR_NAME )
         CALL MATADD_SSS ( NDOFR, 'MRR', NTERM_MRRcb, I_MRRcb, J_MRRcb, MRRcb, ONE, 'CRS3', NTERM_CRS3, I_CRS3, J_CRS3, CRS3,      &
                               ONE,'(MRL*DLR) + (MRL*DLR)t + DLRt*MLL*DLR', NTERM_CRS2,  I_CRS2,  J_CRS2,  CRS2 )

         CALL DEALLOCATE_SCR_MAT ( 'CRS1' )                ! II-9 , deallocate CRS1 and CRS3
         CALL DEALLOCATE_SCR_MAT ( 'CRS3' )                ! II-10, deallocate CRS3

         NTERM_MRRcb = NTERM_CRS2                          ! II-11, reallocate MRRcb to be size of CRS2
         CALL DEALLOCATE_SPARSE_MAT ( 'MRRcb' )
         CALL ALLOCATE_SPARSE_MAT ( 'MRRcb', NDOFR, NTERM_MRRcb, SUBR_NAME )

         DO I=1,NDOFR+1                                    ! II-12, set MRRcb = CRS2 until we see if MRL is null, below
            I_MRRcb(I) = I_CRS2(I)
         ENDDO
         DO I=1,NTERM_MRRcb
            J_MRRcb(I) = J_CRS2(I)
              MRRcb(I) =   CRS2(I)
         ENDDO

         CALL DEALLOCATE_SCR_MAT ( 'CRS2' )                ! II-13, deallocate CRS2

      ELSE

         CALL DEALLOCATE_SCR_MAT ( 'CCS1')

      ENDIF

! Calc the 6x6 mass matrix relative to the MEFMGRID and convert it to same units as input mass

      CALL SPARSE_CRS_TO_FULL ( 'MRRcb', NTERM_MRRcb, NDOFR, NDOFR, SYM_MRRcb, I_MRRcb, J_MRRcb, MRRcb, MRRcb_FULL )

      CALL MATMULT_FFF   ( MRRcb_FULL, TR6_MEFM, NDOFR, NDOFR, 6, DUMR6   )
      CALL MATMULT_FFF_T ( TR6_MEFM  , DUMR6   , NDOFR, 6    , 6, MEFM_RB_MASS )
      DO I=1,6
         DO J=1,6
            MEFM_RB_MASS(I,J) = MEFM_RB_MASS(I,J)/WTMASS
         ENDDO
      ENDDO

! The following code sets NTERM_MRRcbn, needed in subr MERGE_MXX. NTERM_MRRcbn cannot wait to be calculated there since several
! arrays have to be dimensioned using it in that subr

      IF      (SPARSTOR == 'SYM   ') THEN                  ! Convert MRRcb (stored symmetric) to MRRcbn (stored nonsymmetric)

         CALL SPARSE_MAT_DIAG_ZEROS ( 'MRRcb', NDOFR, NTERM_MRRcb, I_MRRcb, J_MRRcb, NUM_MRRcb_DIAG_0 )
         NTERM_MRRcbn = 2*NTERM_MRRcb  - (NDOFR - NUM_MRRcb_DIAG_0)

      ELSE IF (SPARSTOR == 'NONSYM') THEN

         NTERM_MRRcbn = NTERM_MRRcb                        ! If SPARSTOR is nonsym, then MRRcb is also stored nonsym

      ELSE

         WRITE(ERR,932) SUBR_NAME, SPARSTOR
         WRITE(F06,932) SUBR_NAME, SPARSTOR
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )

      ENDIF

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

! **********************************************************************************************************************************
 
      END SUBROUTINE CALC_MRRcb