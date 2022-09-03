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
 
      SUBROUTINE MERGE_PHIXA ( PART_VEC_A_LR )
 
! Merges matrices to get CB matrix PHIXA:

!                        |IRR  0RN |     IRR = R-set identity matrix, 0RN  = RxN null matrix (N = num modes)
!                PHIXA = |         |
!                        |DLR  PHIL|     DLR = C.B. boundary modes  , PHIL = EIGEN_VECS() LxN eigenvectors for the L-set x N modes


! For a description of Craig-Bamptom analyses, see Appendix D to the MYSTRAN User's Referance Manual


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFA, NDOFR, NVEC
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE EIGEN_MATRICES_1, ONLY      :  EIGEN_VEC
      USE SPARSE_MATRICES, ONLY       :  I_DLR , J_DLR , DLR , I_IRR , J_IRR , IRR , I_PHIXA, J_PHIXA, PHIXA
      USE SUBR_BEGEND_LEVELS, ONLY    :  MERGE_PHIXA_BEGEND
 
      USE MERGE_PHIXA_USE_IFs                                ! Added 2019/07/14

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MERGE_PHIXA'

      INTEGER(LONG), INTENT(IN)       :: PART_VEC_A_LR(NDOFA)! Partitioning vector (N set into F and S sets) 
      INTEGER(LONG)                   :: I,J                 ! DO loop indices or counters
      INTEGER(LONG)                   :: KTERM_IRR           ! Count of number terms in arrays J_IRR and A
      INTEGER(LONG)                   :: KTERM_DLR           ! Count of number terms in arrays J_DLR and B
      INTEGER(LONG)                   :: KTERM_PHIXA         ! Count of number terms in arrays J_C and C
!xx   INTEGER(LONG)                   :: NTERM_PHIXA         ! Number of nonzero terms in output matrix C
      INTEGER(LONG)                   :: NUM_IN_ROW_OF_IRR   ! Num terms in a row of IRR matrix
      INTEGER(LONG)                   :: NUM_IN_ROW_OF_DLR   ! Num terms in a row of DLR matrix
!xx   INTEGER(LONG)                   :: NUM_IN_ROW_OF_PHIXA ! Num terms in a row of IRR
      INTEGER(LONG)                   :: ROW_NUM_DLR         ! Row number in matrix DLR
      INTEGER(LONG)                   :: ROW_NUM_EV          ! Row number in matrix EIGEN_VEC (L-set eigenvectors)
      INTEGER(LONG)                   :: ROW_NUM_IRR         ! Row number in matrix IRR (R-set identity matrix)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MERGE_PHIXA_BEGEND
       
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      ROW_NUM_DLR        = 0
      ROW_NUM_EV         = 0
      ROW_NUM_IRR        = 0
      NUM_IN_ROW_OF_DLR  = 0
      NUM_IN_ROW_OF_IRR  = 0
!xx   NUM_IN_ROW_OF_PHIXA = 0
      KTERM_IRR   = 0
      KTERM_DLR   = 0
      KTERM_PHIXA = 0
      I_PHIXA(1)  = 1
      DO I=1,NDOFA
         I_PHIXA(I+1) = I_PHIXA(I)
         IF      (PART_VEC_A_LR(I) == 2) THEN              ! Get a row of matrix IRR and put it into PHIXA
            ROW_NUM_IRR         = ROW_NUM_IRR + 1
            NUM_IN_ROW_OF_IRR   = I_IRR(ROW_NUM_IRR+1) - I_IRR(ROW_NUM_IRR)
!xx         NUM_IN_ROW_OF_PHIXA = NUM_IN_ROW_OF_IRR
            DO J=1,NUM_IN_ROW_OF_IRR
               KTERM_IRR            = KTERM_IRR + 1
               I_PHIXA(I+1)         = I_PHIXA(I+1) + 1
               KTERM_PHIXA          = KTERM_PHIXA + 1
               J_PHIXA(KTERM_PHIXA) = J_IRR(KTERM_IRR)
                 PHIXA(KTERM_PHIXA) =   IRR(KTERM_IRR)
            ENDDO

         ELSE IF (PART_VEC_A_LR(I) == 1) THEN              ! Get a row of matrix DLR and EIGEN_VEC and put it into PHIXA
            ROW_NUM_DLR             = ROW_NUM_DLR + 1
            ROW_NUM_EV              = ROW_NUM_EV  + 1
            NUM_IN_ROW_OF_DLR       = I_DLR(ROW_NUM_DLR+1) - I_DLR(ROW_NUM_DLR)
!xx         NUM_IN_ROW_OF_PHIXA     = NUM_IN_ROW_OF_DLR
            DO J=1,NUM_IN_ROW_OF_DLR
               KTERM_DLR            = KTERM_DLR + 1
               I_PHIXA(I+1)         = I_PHIXA(I+1) + 1
               KTERM_PHIXA          = KTERM_PHIXA + 1
               J_PHIXA(KTERM_PHIXA) = J_DLR(KTERM_DLR)
                 PHIXA(KTERM_PHIXA) =   DLR(KTERM_DLR)
            ENDDO
            DO J=1,NVEC
               I_PHIXA(I+1)         = I_PHIXA(I+1) +1
               KTERM_PHIXA          = KTERM_PHIXA + 1
               J_PHIXA(KTERM_PHIXA) = NDOFR + J
                 PHIXA(KTERM_PHIXA) = EIGEN_VEC(ROW_NUM_EV,J)
            ENDDO
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

      END SUBROUTINE MERGE_PHIXA

