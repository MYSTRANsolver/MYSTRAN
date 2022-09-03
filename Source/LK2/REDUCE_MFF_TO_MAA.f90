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

      SUBROUTINE REDUCE_MFF_TO_MAA ( PART_VEC_F_AO )
 
! Call routines to reduce the MFF mass matrix from the F-set to the A, O-sets. See Appendix B to the MYSTRAN User's Reference Manual
! for the derivation of the reduction equations.
 
! NOTE: This subr has code for sparse matrices as well as full matrices, (i.e. Bulk Data PARAM MATSPARS = 'Y' for sparse and 'N'
! for full). The code for full matrices was put in originally in order that the sparse code could be thoroughly checked. That task
! is complete and the remaining full matrix code has not been maintained since around 2005. In addition, new capability added to
! MYSTRAN since that approx time does not have full matrix code.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFF, NDOFA, NDOFO, NTERM_MFF, NTERM_MAA, NTERM_MAO, NTERM_MOO, &
                                         NTERM_GOA
      USE PARAMS, ONLY                :  EPSIL, MATSPARS, SPARSTOR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_MFF_TO_MAA_BEGEND
      USE CONSTANTS_1, ONLY           :  ONE 
      USE SPARSE_MATRICES, ONLY       :  I_MFF, J_MFF, MFF, I_MAA, J_MAA, MAA, I_MAO, J_MAO, MAO, I_MOO, J_MOO, MOO,               &
                                         I_GOA, J_GOA, GOA,  I_GOAt, J_GOAt, GOAt
      USE SPARSE_MATRICES, ONLY       :  SYM_GOA, SYM_MFF, SYM_MAA, SYM_MAO, SYM_MOO
      USE FULL_MATRICES, ONLY         :  MAA_FULL, MAO_FULL, MOO_FULL, GOA_FULL, DUM1, DUM2, DUM3
      USE SCRATCH_MATRICES

      USE REDUCE_MFF_TO_MAA_USE_IFs

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'REDUCE_MFF_TO_MAA'
      CHARACTER(  1*BYTE)             :: SYM_CRS1            ! Storage format for matrix CRS1 (either 'Y' for sym storage or
!                                                              'N' for nonsymmetric storage)
      CHARACTER(  1*BYTE)             :: SYM_CRS3            ! Storage format for matrix CRS3 (either 'Y' for sym storage or
!                                                              'N' for nonsymmetric storage)
 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_F_AO(NDOFF)! Partitioning vector (F set into A and O sets) 
      INTEGER(LONG)                   :: AROW_MAX_TERMS      ! Output from MATMULT_SFS_NTERM and input to MATMULT_SFS
      INTEGER(LONG)                   :: I,J                 ! DO loop indices
!                                                              the ones on and above the diagonal (controlled by param SPARSTOR)
      INTEGER(LONG)                   :: ITRNSPB             ! Transpose indicator for matrix multiply routine
      INTEGER(LONG)                   :: MAA_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: MAO_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: MOO_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: NTERM_CCS1          ! Number of terms in matrix CCS1  
      INTEGER(LONG)                   :: NTERM_CRS1          ! Number of terms in matrix CRS1  
      INTEGER(LONG)                   :: NTERM_CRS2          ! Number of terms in matrix CRS2  
      INTEGER(LONG)                   :: NTERM_CRS3          ! Number of terms in matrix CRS3  
      INTEGER(LONG), PARAMETER        :: NUM1        = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_MFF_TO_MAA_BEGEND

      REAL(DOUBLE)                    :: ALPHA = ONE         ! Scalar multiplier for matrix
      REAL(DOUBLE)                    :: BETA  = ONE         ! Scalar multiplier for matrix
      REAL(DOUBLE)                    :: SMALL             ! A number used in filtering out small numbers from a full matrix
 
      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Partition MAA from MFF (This is MAA before reduction, or MAA(bar) )

      IF (NDOFA > 0) THEN

         CALL PARTITION_SS_NTERM ( 'MFF', NTERM_MFF, NDOFF, NDOFF, SYM_MFF, I_MFF, J_MFF,      PART_VEC_F_AO, PART_VEC_F_AO,       &
                                    NUM1, NUM1, MAA_ROW_MAX_TERMS, 'MAA', NTERM_MAA, SYM_MAA ) 

         CALL ALLOCATE_SPARSE_MAT ( 'MAA', NDOFA, NTERM_MAA, SUBR_NAME )

         IF (NTERM_MAA > 0) THEN      
            CALL PARTITION_SS ( 'MFF', NTERM_MFF, NDOFF, NDOFF, SYM_MFF, I_MFF, J_MFF, MFF, PART_VEC_F_AO, PART_VEC_F_AO,          &
                                 NUM1, NUM1, MAA_ROW_MAX_TERMS, 'MAA', NTERM_MAA, NDOFA, SYM_MAA, I_MAA, J_MAA, MAA )
         ENDIF

      ENDIF

! Partition MAO from MFF

      IF ((NDOFA > 0) .AND. (NDOFO > 0)) THEN

         CALL PARTITION_SS_NTERM ( 'MFF', NTERM_MFF, NDOFF, NDOFF, SYM_MFF, I_MFF, J_MFF,      PART_VEC_F_AO, PART_VEC_F_AO,       &
                                    NUM1, NUM2, MAO_ROW_MAX_TERMS, 'MAO', NTERM_MAO, SYM_MAO ) 

         CALL ALLOCATE_SPARSE_MAT ( 'MAO', NDOFA, NTERM_MAO, SUBR_NAME )

         IF (NTERM_MAO > 0) THEN
            CALL PARTITION_SS ( 'MFF', NTERM_MFF, NDOFF, NDOFF, SYM_MFF, I_MFF, J_MFF, MFF, PART_VEC_F_AO, PART_VEC_F_AO,          &
                                 NUM1, NUM2, MAO_ROW_MAX_TERMS, 'MAO', NTERM_MAO, NDOFA, SYM_MAO, I_MAO, J_MAO, MAO )
         ENDIF

      ENDIF

! Partition MOO from MFF

      IF (NDOFO > 0) THEN

         CALL PARTITION_SS_NTERM ( 'MFF', NTERM_MFF, NDOFF, NDOFF, SYM_MFF, I_MFF, J_MFF,      PART_VEC_F_AO, PART_VEC_F_AO,       &
                                    NUM2, NUM2, MOO_ROW_MAX_TERMS, 'MOO', NTERM_MOO, SYM_MOO ) 

         CALL ALLOCATE_SPARSE_MAT ( 'MOO', NDOFO, NTERM_MOO, SUBR_NAME )

         IF (NTERM_MOO > 0) THEN
            CALL PARTITION_SS ( 'MFF', NTERM_MFF, NDOFF, NDOFF, SYM_MFF, I_MFF, J_MFF, MFF, PART_VEC_F_AO, PART_VEC_F_AO,          &
                                 NUM2, NUM2, MOO_ROW_MAX_TERMS, 'MOO', NTERM_MOO, NDOFO, SYM_MOO, I_MOO, J_MOO, MOO )
         ENDIF

      ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
! Reduce MFF to MAA = MAA(bar) + MAO*GOA + (MAO*GOA)' + GOA'*MOO*GOA.
! If PARAM MATSPARS = 'Y', then we use sparse matrix operations (multiply/add/transpose). If not, use full matrix operations

      IF (MATSPARS == 'Y') THEN

         CALL ALLOCATE_SPARSE_MAT ( 'GOAt', NDOFA, NTERM_GOA, SUBR_NAME )
         CALL MATTRNSP_SS ( NDOFO, NDOFA, NTERM_GOA, 'GOA', I_GOA, J_GOA, GOA, 'GOAt', I_GOAt, J_GOAt, GOAt )

                                                           ! CCS1 will be sparse CCS format version of sparse CRS matrix GOA
         CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NDOFA, NTERM_GOA, SUBR_NAME )
         CALL SPARSE_CRS_SPARSE_CCS ( NDOFO, NDOFA, NTERM_GOA, 'GOA', I_GOA, J_GOA, GOA, 'CCS1', J_CCS1, I_CCS1, CCS1, 'Y' )

         IF (NTERM_MAO > 0) THEN                           ! Part I of reduced MAA: calc MAO*GOA & add it & transpose to orig MAA

                                                           ! I-1 , sparse multiply to get CRS1 = MAO*GOA. Use CCS1 for GOA CCS
            CALL MATMULT_SSS_NTERM ( 'MAO' , NDOFA, NTERM_MAO , SYM_MAO, I_MAO , J_MAO ,                                           &
                                     'GOA' , NDOFA, NTERM_GOA , SYM_GOA, J_CCS1, I_CCS1, AROW_MAX_TERMS,                           &
                                     'CRS1',        NTERM_CRS1 )

            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFA, NTERM_CRS1, SUBR_NAME )

            CALL MATMULT_SSS ( 'MAO' , NDOFA, NTERM_MAO , SYM_MAO, I_MAO , J_MAO , MAO ,                                           &
                               'GOA' , NDOFA, NTERM_GOA , SYM_GOA, J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                           &
                               'CRS1', ONE  , NTERM_CRS1,          I_CRS1, J_CRS1, CRS1 )

            NTERM_CRS2 = NTERM_CRS1                        ! I-2 , allocate memory to array CRS2 which will hold transpose of CRS1
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS2', NDOFA, NTERM_CRS2, SUBR_NAME )

                                                           ! I-3 , transpose CRS1 to get CRS2 = (MAO*GOA)t
            CALL MATTRNSP_SS ( NDOFA, NDOFA, NTERM_CRS1, 'CRS1', I_CRS1, J_CRS1, CRS1, 'CRS2', I_CRS2, J_CRS2, CRS2 )

                                                           ! I-4 , sparse add to get CRS3 = CRS1 + CRS2 = (MAO*GOA) + (MAO*GOA)t
            CALL MATADD_SSS_NTERM (NDOFA,'MAO*GOA', NTERM_CRS1, I_CRS1, J_CRS1, 'N', '(MAO*GOA)t', NTERM_CRS2, I_CRS2, J_CRS2, 'N',&
                                         'CRS3', NTERM_CRS3)
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFA, NTERM_CRS3, SUBR_NAME )
            CALL MATADD_SSS ( NDOFA, 'MAO*GOA', NTERM_CRS1, I_CRS1, J_CRS1, CRS1, ONE, '(MAO*GOA)t', NTERM_CRS2,                   &
                              I_CRS2, J_CRS2, CRS2, ONE, 'CRS3', NTERM_CRS3, I_CRS3, J_CRS3, CRS3 )

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! I-5 , deallocate CRS1, CRS2
            CALL DEALLOCATE_SCR_MAT ( 'CRS2' )

                                                           ! I-6 , CRS3 = (MAO*GOA) + (MAO*GOA)t has all nonzero terms in it.
            IF      (SPARSTOR == 'SYM   ') THEN            !      If SPARSTOR == 'SYM   ', rewrite CRS3 as sym in CRS1     

               CALL SPARSE_CRS_TERM_COUNT ( NDOFA, NTERM_CRS3, '(MAO*GOA) + (MAO*GOA)t all nonzeros', I_CRS3, J_CRS3, NTERM_CRS1 )
               CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFA, NTERM_CRS1, SUBR_NAME )
               CALL CRS_NONSYM_TO_CRS_SYM ( 'CRS3 = (MAO*GOA) + (MAO*GOA)t all nonzeros', NDOFA, NTERM_CRS3, I_CRS3, J_CRS3, CRS3, &
                                            'CRS1 = (MAO*GOA) + (MAO*GOA)t stored sym'  ,        NTERM_CRS1, I_CRS1, J_CRS1, CRS1 )
               SYM_CRS1 = 'Y'

            ELSE IF (SPARSTOR == 'NONSYM') THEN            !      If SPARSTOR == 'NONSYM', rewrite CRS3 in CRS1 with NTERM_CRS3

               NTERM_CRS1 = NTERM_CRS3
               CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFA, NTERM_CRS1, SUBR_NAME )
               DO I=1,NDOFA+1
                  I_CRS1(I) = I_CRS3(I)
               ENDDO
               DO I=1,NTERM_CRS1
                  J_CRS1(I) = J_CRS3(I)
                    CRS1(I) =   CRS3(I)
               ENDDO
               SYM_CRS1 = 'N'

            ELSE                                           !      Error - incorrect SPARSTOR

               WRITE(ERR,932) SUBR_NAME, SPARSTOR
               WRITE(F06,932) SUBR_NAME, SPARSTOR
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )

            ENDIF

            CALL DEALLOCATE_SCR_MAT ( 'CRS3' )             ! I-7 , deallocate CRS3

                                                           ! I-8 , sparse add to get CRS3 = MAA-bar + CRS1
            CALL MATADD_SSS_NTERM ( NDOFA, 'MAA-bar', NTERM_MAA , I_MAA , J_MAA , SYM_MAA , '(MAO*GOA) + (MAO*GOA)t',              &
                                                      NTERM_CRS1, I_CRS1, J_CRS1, SYM_CRS1, 'CRS3', NTERM_CRS3 )
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFA, NTERM_CRS3, SUBR_NAME )
            CALL MATADD_SSS ( NDOFA, 'MAA-bar', NTERM_MAA, I_MAA, J_MAA, MAA, ONE, '(MAO*GOA) + (MAO*GOA)t', NTERM_CRS1,           &
                              I_CRS1, J_CRS1, CRS1, ONE, 'CRS1', NTERM_CRS3, I_CRS3, J_CRS3, CRS3 )

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! I-9 , deallocate CRS1 and CRS3

            NTERM_MAA = NTERM_CRS3                         ! I-10, reallocate MAA to be size of CRS1
            WRITE(SC1, * ) '    Reallocate MAA'
      !xx   WRITE(SC1, * )                                 ! Advance 1 line for screen messages         
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MAA', CR13
            CALL DEALLOCATE_SPARSE_MAT ( 'MAA' )
            WRITE(SC1,12345,ADVANCE='NO') '       Allocate   MAA', CR13
            CALL ALLOCATE_SPARSE_MAT ( 'MAA', NDOFA, NTERM_MAA, SUBR_NAME )
                                                           ! I-11, set MAA = CRS1
            DO I=1,NDOFA+1
               I_MAA(I) = I_CRS3(I)
            ENDDO
            DO J=1,NTERM_MAA
               J_MAA(J) = J_CRS3(J)
                 MAA(J) =   CRS3(J)
            ENDDO 

            CALL DEALLOCATE_SCR_MAT ( 'CRS3' )             ! I-12, deallocate CRS3

         ENDIF

         IF (NTERM_MOO > 0) THEN                           ! Part II of reduced MAA: calc GOA(t)*MOO*GOA and add to MAA

                                                           ! II-1 , sparse multiply to get CRS1 = MOO*GOA using CCS1 for GOA CCS
            CALL MATMULT_SSS_NTERM ( 'MOO' , NDOFO, NTERM_MOO , SYM_MOO, I_MOO , J_MOO ,                                           &
                                     'GOA' , NDOFA, NTERM_GOA , SYM_GOA, J_CCS1, I_CCS1, AROW_MAX_TERMS,                           &
                                     'CRS1',        NTERM_CRS1 )

            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFO, NTERM_CRS1, SUBR_NAME )

            CALL MATMULT_SSS ( 'MOO' , NDOFO, NTERM_MOO , SYM_MOO, I_MOO , J_MOO , MOO ,                                           &
                               'GOA' , NDOFA, NTERM_GOA , SYM_GOA, J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                           &
                               'CRS1', ONE  , NTERM_CRS1,          I_CRS1, J_CRS1, CRS1 )

            CALL DEALLOCATE_SCR_MAT ( 'CCS1' )             ! II-2 , deallocate CCS1

            NTERM_CCS1 = NTERM_CRS1                        ! II-3 , allocate CCS1 to be same as CRS1 but in CCS format
            CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NDOFA, NTERM_CCS1, SUBR_NAME )
            CALL SPARSE_CRS_SPARSE_CCS ( NDOFO, NDOFA, NTERM_CRS1, 'CRS1', I_CRS1, J_CRS1, CRS1, 'CCS1', J_CCS1, I_CCS1, CCS1, 'Y' )

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! II-4 , deallocate CRS1
                                                           ! II-5 , sparse multiply to get CRS1 = GOAt*CCS1 with CCS1= MOO*GOA
!                                                            (note: use SYM_GOA for sym indicator of CCS1)
            CALL MATMULT_SSS_NTERM ( 'GOAt', NDOFA, NTERM_GOA , SYM_GOA, I_GOAt, J_GOAt,                                           &
                                     'CCS1', NDOFA, NTERM_CCS1, SYM_GOA, J_CCS1, I_CCS1, AROW_MAX_TERMS,                           &
                                     'CRS1 ',       NTERM_CRS1 )

            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFA, NTERM_CRS1, SUBR_NAME )

            CALL MATMULT_SSS ( 'GOAt', NDOFA, NTERM_GOA , SYM_GOA, I_GOAt, J_GOAt, GOAt,                                           &
                               'CCS1', NDOFA, NTERM_CCS1, SYM_GOA, J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                           &
                               'CRS1', ONE  , NTERM_CRS1,          I_CRS1, J_CRS1, CRS1 )

            CALL DEALLOCATE_SCR_MAT ( 'CCS1' )             ! II-6 , deallocate CCS1

                                                           ! II-7 , CRS1 = GOAt*MOO*GOA has all nonzero terms in it.
            IF      (SPARSTOR == 'SYM   ') THEN            !      If SPARSTOR == 'SYM   ', rewrite CRS1 as sym in CRS3     

               CALL SPARSE_CRS_TERM_COUNT ( NDOFA, NTERM_CRS1, 'GOAt*MOO*GOA all nonzeros', I_CRS1, J_CRS1, NTERM_CRS3 )
               CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFA, NTERM_CRS3, SUBR_NAME )
!xx            CALL SPARSE_NONSYM_TO_SYM ( NDOFA, NTERM_CRS1, 'CRS1', I_CRS1, J_CRS1, CRS1,                                        &
!xx                                               NTERM_CRS3, 'CRS3', I_CRS3, J_CRS3, CRS3 )
               CALL CRS_NONSYM_TO_CRS_SYM ( 'CRS1 = GOAt*MOO*GOA all nonzeros', NDOFA, NTERM_CRS1, I_CRS1, J_CRS1, CRS1,           &
                                            'CRS3 = GOAt*MOO*GOA stored sym'  ,        NTERM_CRS3, I_CRS3, J_CRS3, CRS3 )
               SYM_CRS3 = 'Y'

            ELSE IF (SPARSTOR == 'NONSYM') THEN            !      If SPARSTOR == 'NONSYM', rewrite CRS3 in CRS1 with NTERM_CRS3

               NTERM_CRS3 = NTERM_CRS1
               CALL ALLOCATE_SCR_CRS_MAT ( 'CRS3', NDOFA, NTERM_CRS3, SUBR_NAME )
               DO I=1,NDOFA+1
                  I_CRS3(I) = I_CRS1(I)
               ENDDO
               DO I=1,NTERM_CRS3
                  J_CRS3(I) = J_CRS1(I)
                    CRS3(I) =   CRS1(I)
               ENDDO
               SYM_CRS3 = 'N'

            ELSE                                           !      Error - incorrect SPARSTOR

               WRITE(ERR,932) SUBR_NAME, SPARSTOR
               WRITE(F06,932) SUBR_NAME, SPARSTOR
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )

            ENDIF
                                                           ! II-8 , sparse add to get CRS2 = MAA + CRS3 = MAA + GOAt*MOO*GOA
            CALL MATADD_SSS_NTERM ( NDOFA, 'MAA-bar + (MAO*GOA) + (MAO*GOA)t', NTERM_MAA, I_MAA, J_MAA, SYM_MAA, 'GOAt*MOO*GOA',   &
                                    NTERM_CRS3, I_CRS3, J_CRS3, SYM_CRS3, 'CRS2', NTERM_CRS2 )
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS2', NDOFA, NTERM_CRS2, SUBR_NAME )
            CALL MATADD_SSS ( NDOFA, 'MAA-bar + (MAO*GOA) + (MAO*GOA)t' , NTERM_MAA , I_MAA , J_MAA , MAA, ONE, 'GOAt*MOO*GOA',    &
                              NTERM_CRS3, I_CRS3, J_CRS3, CRS3, ONE, 'CRS2', NTERM_CRS2, I_CRS2, J_CRS2, CRS2 )

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! II-9 , deallocate CRS1 and CRS3
            CALL DEALLOCATE_SCR_MAT ( 'CRS3' )             ! II-10, deallocate CRS3

            NTERM_MAA = NTERM_CRS2                         ! II-11, reallocate MAA to be size of CRS2
            WRITE(SC1, * ) '    Reallocate MAA'
      !xx   WRITE(SC1, * )                                 ! Advance 1 line for screen messages         
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MAA', CR13
             CALL DEALLOCATE_SPARSE_MAT ( 'MAA' )
            WRITE(SC1,12345,ADVANCE='NO') '       Allocate   MAA', CR13
            CALL ALLOCATE_SPARSE_MAT ( 'MAA', NDOFA, NTERM_MAA, SUBR_NAME )

            DO I=1,NDOFA+1                                 ! II-12, set reduced MAA = CRS2 until we see if MAO is null, below
               I_MAA(I) = I_CRS2(I)
            ENDDO
            DO I=1,NTERM_MAA
               J_MAA(I) = J_CRS2(I)
                 MAA(I) =   CRS2(I)
            ENDDO

            CALL DEALLOCATE_SCR_MAT ( 'CRS2' )             ! II-13, deallocate CRS2

         ELSE

            CALL DEALLOCATE_SCR_MAT ( 'CCS1')

         ENDIF

         CALL DEALLOCATE_SPARSE_MAT ( 'GOAt' )

! ----------------------------------------------------------------------------------------------------------------------------------
      ELSE IF (MATSPARS == 'N') THEN

         CALL ALLOCATE_FULL_MAT ( 'MAA_FULL', NDOFA, NDOFA, SUBR_NAME )

         IF (NTERM_MAA > 0) THEN
            CALL SPARSE_CRS_TO_FULL ( 'MAA', NTERM_MAA, NDOFA, NDOFA, SYM_MAA, I_MAA, J_MAA,MAA, MAA_FULL )
         ENDIF

         IF (NTERM_MAO > 0) THEN                           ! Part 1: calc MAO*GOA and add it & it's transpose to MAA_FULL

            CALL ALLOCATE_FULL_MAT ( 'MAO_FULL', NDOFA, NDOFO, SUBR_NAME )
            CALL SPARSE_CRS_TO_FULL ( 'MAO', NTERM_MAO, NDOFA, NDOFO, SYM_MAO, I_MAO, J_MAO, MAO, MAO_FULL )

            CALL ALLOCATE_FULL_MAT ( 'GOA_FULL', NDOFO, NDOFA, SUBR_NAME )
            CALL SPARSE_CRS_TO_FULL ( 'GOA', NTERM_GOA, NDOFO, NDOFA, SYM_GOA, I_GOA, J_GOA, GOA, GOA_FULL )

            CALL ALLOCATE_FULL_MAT ( 'DUM1', NDOFA, NDOFA, SUBR_NAME )

            CALL MATMULT_FFF (  MAO_FULL, GOA_FULL, NDOFA, NDOFO, NDOFA, DUM1 )

            CALL DEALLOCATE_FULL_MAT ( 'MAO_FULL' )
            CALL DEALLOCATE_FULL_MAT ( 'GOA_FULL' )
            CALL ALLOCATE_FULL_MAT ( 'DUM2', NDOFA, NDOFA, SUBR_NAME )

            ITRNSPB = 0                                    ! Calc MAA-bar + MAO*GOA = MAA + DUM1 and put into DUM1
            CALL MATADD_FFF ( MAA_FULL, DUM1, NDOFA, NDOFA, ALPHA, BETA, ITRNSPB, DUM2 )
            ITRNSPB = 1                                    ! Calc MAA-bar + MAO*GOA + (OAM*GOA)' = DUM2+DUM1' and put into MAA_FULL
            CALL MATADD_FFF ( DUM2    , DUM1, NDOFA, NDOFA, ALPHA, BETA, ITRNSPB, MAA_FULL )


            CALL DEALLOCATE_FULL_MAT ( 'DUM1' )
            CALL DEALLOCATE_FULL_MAT ( 'DUM2' )

         ENDIF

         IF (NTERM_MOO > 0) THEN                           ! Part 2: calc GOA(t)*MOO*GOA and add to MAA_FULL

            CALL ALLOCATE_FULL_MAT ( 'MOO_FULL', NDOFO, NDOFO, SUBR_NAME )
            CALL SPARSE_CRS_TO_FULL ( 'MOO', NTERM_MOO, NDOFO, NDOFO, SYM_MOO, I_MOO, J_MOO, MOO, MOO_FULL )

            CALL ALLOCATE_FULL_MAT ( 'GOA_FULL', NDOFO, NDOFA, SUBR_NAME )
            CALL SPARSE_CRS_TO_FULL ( 'GOA', NTERM_GOA, NDOFO, NDOFA, SYM_GOA, I_GOA, J_GOA, GOA, GOA_FULL )

            CALL ALLOCATE_FULL_MAT ( 'DUM2', NDOFO, NDOFA, SUBR_NAME )

            CALL MATMULT_FFF ( MOO_FULL, GOA_FULL, NDOFO, NDOFO, NDOFA, DUM2 )

            CALL DEALLOCATE_FULL_MAT ( 'MOO_FULL' )

            CALL ALLOCATE_FULL_MAT ( 'DUM1', NDOFA, NDOFA, SUBR_NAME )

            CALL MATMULT_FFF_T (GOA_FULL, DUM2, NDOFO, NDOFA, NDOFA, DUM1 )

            CALL DEALLOCATE_FULL_MAT ( 'DUM2' )

            CALL DEALLOCATE_FULL_MAT ( 'GOA_FULL' )

            CALL ALLOCATE_FULL_MAT ( 'DUM2', NDOFA, NDOFA, SUBR_NAME )

            ITRNSPB = 0
            CALL MATADD_FFF ( MAA_FULL, DUM1, NDOFA, NDOFA, ALPHA, BETA, ITRNSPB, DUM2 )
            DO I=1,NDOFA
               DO J=1,NDOFA
                  MAA_FULL(I,J) = DUM2(I,J)
               ENDDO
            ENDDO

            CALL DEALLOCATE_FULL_MAT ( 'DUM1' )
            CALL DEALLOCATE_FULL_MAT ( 'DUM2' )

         ENDIF

         CALL CNT_NONZ_IN_FULL_MAT ( 'MAA_FULL  ', MAA_FULL, NDOFA, NDOFA, SYM_MAA, NTERM_MAA, SMALL )

         CALL DEALLOCATE_SPARSE_MAT ( 'MAA' )
         CALL ALLOCATE_SPARSE_MAT ( 'MAA', NDOFA, NTERM_MAA, SUBR_NAME )

         IF (NTERM_MAA > 0) THEN                            ! Create new sparse arrays from MAA_FULL
!xx         CALL FULL_TO_SPARSE_CRS ( 'MAA_FULL  ', NDOFA, NDOFA, MAA_FULL, NTERM_MAA, SYM_MAA, I_MAA, J_MAA, MAA )
            CALL DEALLOCATE_FULL_MAT ( 'MAA_FULL' )
         ENDIF

      ELSE

         WRITE(ERR,911) SUBR_NAME,MATSPARS
         WRITE(F06,911) SUBR_NAME,MATSPARS
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
  911 FORMAT(' *ERROR   911: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' PARAMETER MATSPARS MUST BE EITHER ','Y',' OR ','N',' BUT VALUE IS ',A)

  932 FORMAT(' *ERROR   932: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' PARAMETER SPARSTOR MUST BE EITHER "SYM" OR "NONSYM" BUT VALUE IS ',A)

12345 FORMAT(A,10X,A)

! **********************************************************************************************************************************
 
      END SUBROUTINE REDUCE_MFF_TO_MAA
