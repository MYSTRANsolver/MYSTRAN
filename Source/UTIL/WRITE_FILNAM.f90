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
  
      SUBROUTINE WRITE_FILNAM ( FILNAM, UNT, BLEN )

! Writes file name, FILNAM, to unit UNT with a format that depends on the actual length, in characters, of FILNAM (used
! to avoid using 4 lines of screen to write a short file name when the allowable length of FILNAM is 256 bytes)
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  FILE_NAM_MAXLEN, SC1
   
      USE WRITE_FILNAM_USE_IFs

      IMPLICIT NONE
  
      CHARACTER(LEN=*), INTENT(IN)    :: FILNAM            ! File name
      CHARACTER(15*BYTE)              :: LEADING_BLANKS    ! 
  
      INTEGER(LONG), INTENT(IN)       :: UNT               ! Unit number of file FILNAM to write to
      INTEGER(LONG), INTENT(IN)       :: BLEN              ! Length, in char's, of blanks to print before filnam
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: FLEN              ! Length, in char's, of FILNAM (non-blanks)
   
! **********************************************************************************************************************************
! Find actual length of file name
    
      LEADING_BLANKS(1:) = ' '

! Print filename

      IF (UNT == SC1) THEN
         WRITE(SC1,1001) LEADING_BLANKS(1:BLEN), TRIM(FILNAM)
      ELSE
         WRITE(UNT,1001) LEADING_BLANKS(1:BLEN), TRIM(FILNAM)
      ENDIF 
   
! **********************************************************************************************************************************
 1001 FORMAT(A,A)

! **********************************************************************************************************************************
  
      END SUBROUTINE WRITE_FILNAM
