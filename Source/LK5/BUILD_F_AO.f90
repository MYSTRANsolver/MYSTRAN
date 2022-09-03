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

      SUBROUTINE BUILD_F_AO
 
! For one subcase:
    
!   1) Calcs UO displacements: UO = GOA*UA + UO0 where:
 
!          UA  = from calculation in subr BUILD_A_LR
!          UO0 = KOO(-1)*PO from LINK2

!   2) Merge UO and UA to get UF

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NDOFA, NDOFF, NDOFO, NTERM_GOA, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BUILD_F_AO_BEGEND
      USE CONSTANTS_1, ONLY           :  ONE
      USE PARAMS, ONLY                :  PRTDISP
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE SPARSE_MATRICES, ONLY       :  I_GOA, J_GOA, GOA, SYM_GOA
      USE COL_VECS, ONLY              :  UA_COL, UF_COL, UO_COL, UO0_COL
 
      USE BUILD_F_AO_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME   = 'BUILD_F_AO'
 
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: F_SET_COL         ! Col no. in TDOF for F  displ set definition
      INTEGER(LONG)                   :: A_SET_COL         ! Col no. in TDOF for A  displ set definition
      INTEGER(LONG)                   :: O_SET_COL         ! Col no. in TDOF for O  displ set definition
      INTEGER(LONG), PARAMETER        :: NUMCOLS     = 1   ! Variable for number of cols of an array
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BUILD_F_AO_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Multiply GOA x UA to recover part of UO

      IF (NDOFO > 0) THEN

         CALL MATMULT_SFF ( 'GOA', NDOFO, NDOFA, NTERM_GOA, SYM_GOA, I_GOA, J_GOA, GOA, 'UA', NDOFA, NUMCOLS, UA_COL, 'Y',         &
                            'UO', ONE, UO_COL )

! Add UO0 (= KOO(-1) x PO) to get final UO but only if statics solution 

         IF ((SOL_NAME(1:7) == 'STATICS') .OR. (SOL_NAME(1:8) == 'NLSTATIC') .OR.                                                  &
            ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1))) THEN
            DO I=1,NDOFO
               UO_COL(I) = UO_COL(I) + UO0_COL(I)
            ENDDO
         ENDIF

! Merge UA and UO to get UF

         CALL TDOF_COL_NUM ( 'F ', F_SET_COL )
         CALL TDOF_COL_NUM ( 'A ', A_SET_COL )
         CALL TDOF_COL_NUM ( 'O ', O_SET_COL )

         CALL MERGE_COL_VECS ( A_SET_COL, NDOFA, UA_COL, O_SET_COL, NDOFO, UO_COL, F_SET_COL, NDOFF, UF_COL )

      ELSE

         DO I=1,NDOFF
            UF_COL(I) = UA_COL(I)
         ENDDO 

      ENDIF

! Print out displ matrices if PRTDISP says to

      IF ((PRTDISP(4) == 1) .OR. (PRTDISP(4) == 3)) THEN
         IF (NDOFA  > 0) THEN
            CALL WRITE_VECTOR ( '   A-SET DISPL VECTOR   ', 'DISPL', NDOFA, UA_COL)
         ENDIF
      ENDIF

      IF ((PRTDISP(4) == 2) .OR. (PRTDISP(4) == 3)) THEN
         IF (NDOFO  > 0) THEN
            CALL WRITE_VECTOR ( '   O-SET DISPL VECTOR   ', 'DISPL', NDOFO, UO_COL)
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
 
      END SUBROUTINE BUILD_F_AO
