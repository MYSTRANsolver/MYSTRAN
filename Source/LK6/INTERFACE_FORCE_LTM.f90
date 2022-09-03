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

      SUBROUTINE INTERFACE_FORCE_LTM
 
! Merges matrices to get the interface force Loads Transformation Matrix (LTM):

!            IF_LTM   = | MRRcb  MRN    KRRcb |

! For a description of Craig-Bamptom analyses, see Appendix D to the MYSTRAN User's Referance Manual

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFR, NTERM_KRRcb, NTERM_KRRcbn, NTERM_MRRcbn, NTERM_MRN  ,     &
                                         NTERM_IF_LTM  , NVEC
      USE PARAMS, ONLY                :  PRTIFLTM, SPARSTOR
      USE TIMDAT, ONLY                :  TSEC

      USE SPARSE_MATRICES, ONLY       :  SYM_KRRcb, SYM_KRRcbn, SYM_MRN  , SYM_MRRcbn, SYM_IF_LTM  

      USE SPARSE_MATRICES, ONLY       :  I_MRRcbn   , J_MRRcbn   , MRRcbn   , I_MRN      , J_MRN      , MRN      ,                 &
                                         I_KRRcb    , J_KRRcb    , KRRcb    , I_KRRcbn   , J_KRRcbn   , KRRcbn   ,                 &
                                         I_IF_LTM   , J_IF_LTM   , IF_LTM   

      USE SCRATCH_MATRICES, ONLY      :  I_CRS1, J_CRS1, CRS1

      USE SUBR_BEGEND_LEVELS, ONLY    :  INTERFACE_FORCE_LTM_BEGEND

      USE INTERFACE_FORCE_LTM_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'INTERFACE_FORCE_LTM'

      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: NCOL_CRS1         ! Number of cols in scratch matrix CRS1
      INTEGER(LONG)                   :: NTERM_CRS1        ! Number of nonzero terms in scratch matrix CRS1
      INTEGER(LONG)                   :: NUM_KRRcb_DIAG_0  ! Number of zeros on the diagonal of matrix KRRcb
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = INTERFACE_FORCE_LTM_BEGEND


! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Set KRRcbn based on SPARSTOR (we need KRRcb in nonsym format since IF_LTM   will be nonsym

      IF      (SPARSTOR == 'SYM   ') THEN                  ! Convert KRRcb (stored symmetric) to KRRcbn (stored nonsymmetric)

         CALL SPARSE_MAT_DIAG_ZEROS ( 'KRRcb', NDOFR, NTERM_KRRcb, I_KRRcb, J_KRRcb, NUM_KRRcb_DIAG_0 )
         NTERM_KRRcbn = 2*NTERM_KRRcb  - (NDOFR - NUM_KRRcb_DIAG_0)
         CALL ALLOCATE_SPARSE_MAT ( 'KRRcbn', NDOFR, NTERM_KRRcbn, SUBR_NAME )
         CALL CRS_SYM_TO_CRS_NONSYM ( 'KRRcb' , NDOFR, NTERM_KRRcb , I_KRRcb , J_KRRcb , KRRcb,                                    &
                                      'KRRcbn',        NTERM_KRRcbn, I_KRRcbn, J_KRRcbn, KRRcbn, 'Y' )
      ELSE IF (SPARSTOR == 'NONSYM') THEN

         NTERM_KRRcbn = NTERM_KRRcb                        ! If SPARSTOR is nonsym, then KRRcb is also stored nonsym
         CALL ALLOCATE_SPARSE_MAT ( 'KRRcbn', NDOFR, NTERM_KRRcbn, SUBR_NAME )
         DO I=1,NDOFR+1
            I_KRRcbn(I) = I_KRRcb(I)
         ENDDO
         DO J=1,NTERM_KRRcbn
            J_KRRcbn(J) = J_KRRcb(J)
              KRRcbn(J) =   KRRcb(J)
         ENDDO

      ELSE

         WRITE(ERR,932) SUBR_NAME, SPARSTOR
         WRITE(F06,932) SUBR_NAME, SPARSTOR
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )

      ENDIF


! Allocate enough memory for merge of cols of MRRcbn with MRN  

      NTERM_CRS1 = NTERM_MRRcbn+NTERM_MRN  
      CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFR, NTERM_CRS1, SUBR_NAME )

! Merge MRRcbn and MRN   (both in nonsym format) into nonsym format temporary scratch matrix CRS1.

      CALL MERGE_MAT_COLS_SSS ( 'MRRcbn' , NTERM_MRRcbn   , I_MRRcbn, J_MRRcbn, MRRcbn, SYM_MRRcbn, NDOFR,                         &
                                'MRN  '  , NTERM_MRN      , I_MRN   , J_MRN   , MRN   , SYM_MRN   , NDOFR,                         &
                                'MRRcbn merged with MRN  ', I_CRS1  , J_CRS1  , CRS1  , 'N'        )

! Merge CRS1 with KRRcb to get IF_LTM  

      NCOL_CRS1      = NDOFR + NVEC
      NTERM_IF_LTM   = NTERM_CRS1 + NTERM_KRRcbn
      CALL ALLOCATE_SPARSE_MAT ( 'IF_LTM  ', NDOFR, NTERM_IF_LTM  , SUBR_NAME )
      CALL MERGE_MAT_COLS_SSS ( 'CRS1'    , NTERM_CRS1    , I_CRS1    , J_CRS1    , CRS1    , 'N'         , NCOL_CRS1,             &
                                'KRRcbn'  , NTERM_KRRcbn  , I_KRRcbn  , J_KRRcbn  , KRRcbn  , SYM_KRRcbn  , NDOFR,                 &
                                'IF_LTM',                   I_IF_LTM  , J_IF_LTM  , IF_LTM  , SYM_IF_LTM   ) 
      CALL DEALLOCATE_SCR_MAT ( 'CRS1' )
      CALL DEALLOCATE_SPARSE_MAT ( 'KRRcbn' )

      IF (PRTIFLTM > 0) THEN
         CALL WRITE_SPARSE_CRS ( 'IF_LTM  ','  ','  ', NTERM_IF_LTM  , NDOFR, I_IF_LTM  , J_IF_LTM  , IF_LTM   )
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

      END SUBROUTINE INTERFACE_FORCE_LTM