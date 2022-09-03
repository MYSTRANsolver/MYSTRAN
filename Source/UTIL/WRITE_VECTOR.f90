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
 
      SUBROUTINE WRITE_VECTOR ( VEC_NAME, WHAT, NUM, UX )
 
! Writes a vector in full format to the F06 file 

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_VECTOR_BEGEND
 
      USE WRITE_VECTOR_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_VECTOR'
      CHARACTER(LEN=*), INTENT(IN)    :: VEC_NAME          ! Name of vector being output
      CHARACTER(LEN=*), INTENT(IN)    :: WHAT              ! Title over output vector (e.g. DISPL, FORCE, etc.)
      CHARACTER(132*BYTE)             :: LINE_OUT          ! Line to print out (to describe matrix) that is centered

      INTEGER(LONG), INTENT(IN)       :: NUM               ! Size of vector UX to write out
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: INDEX             ! Index into character array LINE_OUT
      INTEGER(LONG)                   :: VEC_NAME_LEN      ! Length of char array VEC_NAME. On input, it is the length as defined
!                                                            in the calling subr. In this subr, VEC_NAME is striped of trailing
!                                                            blanks to get only the actual message. On exit VEC_NAME_LEN is the
!                                                            length of the finite message in VEC_NAME (i.e. without trailing blanks)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_VECTOR_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: UX(NUM)           ! Vector to write out
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Strip out trailing blanks from VEC_NAME and put remainder centered in array LINE_OUT

      VEC_NAME_LEN = LEN(VEC_NAME)                         ! This is the length of VEC_NAME as input (includes trailing blanks)

      DO  I=VEC_NAME_LEN,1,-1                              ! Calc length of description in VEC_NAME, excluding trailing blanks
         IF (VEC_NAME(I:I) == ' ') THEN
            CYCLE
         ELSE
            VEC_NAME_LEN = I
            EXIT
         ENDIF
      ENDDO

      LINE_OUT(1:) = ' '                                   ! Center VEC_NAME (w/0 trailing blanks) in LINE_OUT
      INDEX = (LEN(LINE_OUT) - VEC_NAME_LEN)/2 
      LINE_OUT(INDEX:) = VEC_NAME(1:VEC_NAME_LEN)

      WRITE(F06,2101) LINE_OUT, WHAT
 
      DO I=1,NUM
         WRITE(F06,2102) I, UX(I)
      ENDDO 
      WRITE(F06,*)

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 2101 FORMAT(A,//,54X,'I            ',A,/,51X,'(DOF)')
                                                     

 2102 FORMAT(43X,I12,8X,1ES13.6)

! **********************************************************************************************************************************
 
      END SUBROUTINE WRITE_VECTOR
