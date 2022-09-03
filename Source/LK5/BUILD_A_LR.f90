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

      SUBROUTINE BUILD_A_LR ( COL_NUM )
 
! For one subcase:

!   1) Merge UL and UR to get UA where UL was read into subr LINK5

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NDOFL, NDOFA, NDOFR, NVEC, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BUILD_A_LR_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE PARAMS, ONLY                :  PRTDISP
      USE COL_VECS, ONLY              :  UL_COL, UA_COL, UR_COL
 
      USE BUILD_A_LR_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME   = 'BUILD_A_LR'
  
      INTEGER(LONG), INTENT(IN)       :: COL_NUM
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: A_SET_COL         ! Col no. in TDOF for A  displ set definition
      INTEGER(LONG)                   :: L_SET_COL         ! Col no. in TDOF for L  displ set definition
      INTEGER(LONG)                   :: R_SET_COL         ! Col no. in TDOF for R  displ set definition
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BUILD_A_LR_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Get column numbers for various DOF sets

      IF (NDOFR > 0) THEN
                                                           ! Merge UL and UR to get UA 
         CALL TDOF_COL_NUM('A ', A_SET_COL)
         CALL TDOF_COL_NUM('L ', L_SET_COL)
         CALL TDOF_COL_NUM('R ', R_SET_COL)

         DO I=1,NDOFR
            UR_COL(I) = ZERO
         ENDDO

         IF ((SOL_NAME(1:12) == 'GEN CB MODEL') .AND. (COL_NUM > 0)) THEN
            UR_COL(COL_NUM-(NDOFR+NVEC)) = ONE
         ENDIF

         CALL MERGE_COL_VECS ( L_SET_COL, NDOFL, UL_COL, R_SET_COL, NDOFR, UR_COL, A_SET_COL, NDOFA, UA_COL )

      ELSE
                                                           ! Set UA = UL if no R set DOF's
         DO I=1,NDOFA
            UA_COL(I) = UL_COL(I)
         ENDDO
 
      ENDIF

! Print out displ matrices if PRTDISP says to

      IF ((PRTDISP(3) == 1) .OR. (PRTDISP(3) == 3)) THEN
         IF (NDOFL  > 0) THEN
            CALL WRITE_VECTOR ( '   F-SET DISPL VECTOR   ', 'DISPL', NDOFL, UL_COL)
         ENDIF
      ENDIF

      IF ((PRTDISP(3) == 2) .OR. (PRTDISP(3) == 3)) THEN
         IF (NDOFR  > 0) THEN
            CALL WRITE_VECTOR ( '   S-SET DISPL VECTOR   ', 'DISPL', NDOFR, UR_COL)
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
 
      END SUBROUTINE BUILD_A_LR
