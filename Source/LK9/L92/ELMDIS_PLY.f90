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
 
      SUBROUTINE ELMDIS_PLY
 
! Get displs for one ply, or lamina, of a multi-ply element given the UG displs for the overall element (i.e. at the mid plane of
! the laminate). Result goes back into UEL
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, F04, f06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE CONSTANTS_1, ONLY           :  CONV_DEG_RAD
      USE TIMDAT, ONLY                :  TSEC
      USE MODEL_STUF, ONLY            :  ELGP, ELDOF, UEL, ZPLY
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELMDIS_PLY_BEGEND
 
      USE ELMDIS_PLY_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ELMDIS_PLY'

      INTEGER(LONG)                   :: I,j               ! DO loop index
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELMDIS_PLY_BEGEND

      REAL(DOUBLE)                    :: DUM(6*ELGP)       ! Intermediate variable in the calculation of UEL for the ply 

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      DO I=1,ELGP
         DUM(6*(I-1)+1) = UEL(6*(I-1)+1) + ZPLY*UEL(6*(I-1)+5)
         DUM(6*(I-1)+2) = UEL(6*(I-1)+2) - ZPLY*UEL(6*(I-1)+4)
         DUM(6*(I-1)+3) = UEL(6*(I-1)+3)
         DUM(6*(I-1)+4) = UEL(6*(I-1)+4)
         DUM(6*(I-1)+5) = UEL(6*(I-1)+5)
         DUM(6*(I-1)+6) = UEL(6*(I-1)+6)
      ENDDO

      DO I=1,ELDOF
         UEL(I) = DUM(I)
      ENDDO


! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

! **********************************************************************************************************************************

      END SUBROUTINE ELMDIS_PLY
