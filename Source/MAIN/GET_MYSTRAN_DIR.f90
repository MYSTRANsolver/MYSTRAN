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
 
      SUBROUTINE GET_MYSTRAN_DIR ( MYSTRAN_DIR, MYSTRAN_DIR_LEN )
 
! Gets the environment variable MYSTRAN_DIR that tells Windows where the MYSTRAN executable is located. The user must have set this
! environment variable on their computer

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  FILE_NAM_MAXLEN

      USE GET_MYSTRAN_DIR_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(FILE_NAM_MAXLEN*BYTE), INTENT(OUT) :: MYSTRAN_DIR       ! Directory where executable (and INI file) exist

      INTEGER(LONG), INTENT(OUT)                   :: MYSTRAN_DIR_LEN   ! Length of MYSTRAN_DIR (not including trailing blanks)
      INTEGER(LONG)                                :: I                 ! DO loop index
 
      INTRINSIC                                    :: GETENV

! **********************************************************************************************************************************
      CALL GETENV ( 'MYSTRAN_directory', MYSTRAN_DIR )
      MYSTRAN_DIR_LEN = FILE_NAM_MAXLEN
      DO I=FILE_NAM_MAXLEN,1,-1
         IF (MYSTRAN_DIR(I:I) /= ' ') THEN
            EXIT
         ELSE
            MYSTRAN_DIR_LEN = MYSTRAN_DIR_LEN - 1
            CYCLE
         ENDIF
      ENDDO

! **********************************************************************************************************************************

      END SUBROUTINE GET_MYSTRAN_DIR


