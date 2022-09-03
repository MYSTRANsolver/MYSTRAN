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
 
      SUBROUTINE SHELL_ENGR_FORCE_OGEL ( NUM1 )
 
! Calculates element engineering forces for plate elements from array STRESS (generated in subr ELEM_STRE_STRN_ARRAYS) using FCONV
! (conversion factor from stress to engr force)
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NGRID
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  SHELL_ENGR_FORCE_OGEL_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  FCONV, STRESS
      USE LINK9_STUFF, ONLY           :  MAXREQ, MAXREQ, OGEL

      USE SHELL_ENGR_FORCE_OGEL_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SHELL_ENGR_FORCE_OGEL'

      INTEGER(LONG), INTENT(INOUT)    :: NUM1              ! Cum rows written to OGEL prior to running this subr
      INTEGER(LONG)                   :: I                 ! DO loop indices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SHELL_ENGR_FORCE_OGEL_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      NUM1 = NUM1 + 1
      IF (NUM1 > MAXREQ) THEN
         WRITE(ERR,9200) SUBR_NAME,MAXREQ
         WRITE(F06,9200) SUBR_NAME,MAXREQ
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )                            ! Coding error (dim of array OGEL too small), so quit
      ENDIF   
      DO I=1,3
         OGEL(NUM1,I) = FCONV(1)*STRESS(I)
      ENDDO
      DO I=4,6
         OGEL(NUM1,I) = FCONV(2)*STRESS(I)
      ENDDO
      DO I=7,9
         OGEL(NUM1,I) = FCONV(3)*STRESS(I)
      ENDDO
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 9200 FORMAT(' *ERROR  9200: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' ARRAY OGEL WAS ALLOCATED TO HAVE ',I12,' ROWS. ATTEMPT TO WRITE TO OGEL BEYOND THIS')
 
! **********************************************************************************************************************************

      END SUBROUTINE SHELL_ENGR_FORCE_OGEL