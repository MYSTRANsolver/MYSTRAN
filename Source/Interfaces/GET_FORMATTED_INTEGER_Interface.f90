! ###############################################################################################################################
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

   MODULE GET_FORMATTED_INTEGER_Interface

   INTERFACE

      SUBROUTINE GET_FORMATTED_INTEGER ( INT, CHAR_INT, NUM_CHARS, NUM_DIGITS )

 
      USE PENTIUM_II_KIND, ONLY             :  BYTE, LONG
      USE IOUNT1, ONLY                      :  WRT_LOG, F04
      USE SCONTR, ONLY                      :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                      :  TSEC 
      USE SUBR_BEGEND_LEVELS, ONLY          :  GET_FORMATTED_INTEGER_BEGEND

      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER              :: WORD_LEN  = 13    ! Length of character string that INT will be entered into
      CHARACTER(WORD_LEN*BYTE), INTENT(OUT) :: CHAR_INT          ! Integer formatted to have comma's (36879 becomes 36,879)

      INTEGER(LONG), PARAMETER              :: SUBR_BEGEND = GET_FORMATTED_INTEGER_BEGEND
      INTEGER(LONG), INTENT(IN)             :: INT               ! Integer to be converted to formated value in CHAR_INT
      INTEGER(LONG), INTENT(OUT)            :: NUM_CHARS         ! Num of non blank chars in CHAR_INT after formatting w/ commas
      INTEGER(LONG), INTENT(OUT)            :: NUM_DIGITS        ! Number of digits in INT

      END SUBROUTINE GET_FORMATTED_INTEGER

   END INTERFACE

   END MODULE GET_FORMATTED_INTEGER_Interface

