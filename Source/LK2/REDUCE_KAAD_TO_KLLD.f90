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

      SUBROUTINE REDUCE_KAAD_TO_KLLD ( PART_VEC_A_LR )
 
! Call routines to reduce the KAAD differential stiffness matrix from the A-set to the L, R-sets
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L2K, L2L, LINK2K, LINK2L, L2K_MSG, L2L_MSG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFA, NDOFL, NDOFR, NTERM_KAAD, NTERM_KLLD, NTERM_KRLD,         &
                                         NTERM_KRRD,  SOL_NAME
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_KAAD_TO_KLLD_BEGEND
      USE SPARSE_MATRICES, ONLY       :  I_KAAD, J_KAAD, KAAD, I_KLLD, J_KLLD, KLLD, I_KRLD, J_KRLD, KRLD, I_KRRD, J_KRRD, KRRD,   &
                                         SYM_KAAD, SYM_KLLD, SYM_KRLD, SYM_KRRD
      USE SCRATCH_MATRICES
 
      USE REDUCE_KAAD_TO_KLLD_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'REDUCE_KAAD_TO_KLLD'
 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_A_LR(NDOFA)! Partitioning vector (F set into A and O sets) 
      INTEGER(LONG)                   :: KLLD_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KRLD_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KRRD_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG), PARAMETER        :: NUM1        = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_KAAD_TO_KLLD_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Partition KLLD from KAAD (This is KLLD before reduction, or KLLD(bar) )

      IF (NDOFL > 0) THEN

         CALL PARTITION_SS_NTERM ( 'KAAD', NTERM_KAAD, NDOFA, NDOFA, SYM_KAAD, I_KAAD, J_KAAD,      PART_VEC_A_LR, PART_VEC_A_LR,  &
                                    NUM1, NUM1, KLLD_ROW_MAX_TERMS, 'KLLD', NTERM_KLLD, SYM_KLLD ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KLLD', NDOFL, NTERM_KLLD, SUBR_NAME )

         IF (NTERM_KLLD > 0) THEN      
            CALL PARTITION_SS ( 'KAAD', NTERM_KAAD, NDOFA, NDOFA, SYM_KAAD, I_KAAD, J_KAAD, KAAD, PART_VEC_A_LR, PART_VEC_A_LR,    &
                                 NUM1, NUM1, KLLD_ROW_MAX_TERMS, 'KLLD', NTERM_KLLD, NDOFL, SYM_KLLD, I_KLLD, J_KLLD, KLLD )
         ENDIF

      ENDIF

! Partition KRLD from KAAD

      IF ((NDOFL > 0) .AND. (NDOFR > 0)) THEN

         CALL PARTITION_SS_NTERM ( 'KAAD', NTERM_KAAD, NDOFA, NDOFA, SYM_KAAD, I_KAAD, J_KAAD,      PART_VEC_A_LR, PART_VEC_A_LR,  &
                                    NUM2, NUM1, KRLD_ROW_MAX_TERMS, 'KRLD', NTERM_KRLD, SYM_KRLD ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KRLD', NDOFR, NTERM_KRLD, SUBR_NAME )

         IF (NTERM_KRLD > 0) THEN
            CALL PARTITION_SS ( 'KAAD', NTERM_KAAD, NDOFA, NDOFA, SYM_KAAD, I_KAAD, J_KAAD, KAAD, PART_VEC_A_LR, PART_VEC_A_LR,    &
                                 NUM2, NUM1, KRLD_ROW_MAX_TERMS, 'KRLD', NTERM_KRLD, NDOFR, SYM_KRLD, I_KRLD, J_KRLD, KRLD )
         ENDIF

      ENDIF

! Partition KRRD from KAAD

      IF (NDOFR > 0) THEN

         CALL PARTITION_SS_NTERM ( 'KAAD', NTERM_KAAD, NDOFA, NDOFA, SYM_KAAD, I_KAAD, J_KAAD,      PART_VEC_A_LR, PART_VEC_A_LR,  &
                                    NUM2, NUM2, KRRD_ROW_MAX_TERMS, 'KRRD', NTERM_KRRD, SYM_KRRD ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KRRD', NDOFR, NTERM_KRRD, SUBR_NAME )

         IF (NTERM_KRRD > 0) THEN
            CALL PARTITION_SS ( 'KAAD', NTERM_KAAD, NDOFA, NDOFA, SYM_KAAD, I_KAAD, J_KAAD, KAAD, PART_VEC_A_LR, PART_VEC_A_LR,    &
                                 NUM2, NUM2, KRRD_ROW_MAX_TERMS, 'KRRD', NTERM_KRRD, NDOFR, SYM_KRRD, I_KRRD, J_KRRD, KRRD )
         ENDIF

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

      END SUBROUTINE REDUCE_KAAD_TO_KLLD
