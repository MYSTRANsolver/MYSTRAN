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

   MODULE PARSE_CHAR_STRING_Interface

   INTERFACE

      SUBROUTINE PARSE_CHAR_STRING ( CHAR_STRING, STRING_LEN, MAX_WORDS, MWLEN, NUM_WORDS, WORDS, IERR )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS 
      USE SUBR_BEGEND_LEVELS, ONLY    :  PARSE_CHAR_STRING_BEGEND

      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER         :: MAX_LEN_BAD_WRD=150!  

      INTEGER(LONG), INTENT(IN)        :: MAX_WORDS          ! Dim of WORDS (number of words in CHAR_STRING cannot exceed this)
      INTEGER(LONG), INTENT(IN)        :: MWLEN              ! Maximum length, in characters, of the entries in array WORDS

      CHARACTER(LEN=*)    , INTENT(IN) :: CHAR_STRING        ! Character string to be parsed
      CHARACTER(LEN=MWLEN), INTENT(OUT):: WORDS(MAX_WORDS)   ! Array of the words parsed from CHAR_STRING.

      INTEGER(LONG), INTENT(IN)        :: STRING_LEN         ! Length, in characters, of CHAR_STRING
      INTEGER(LONG), INTENT(OUT)       :: IERR               ! Error designator
      INTEGER(LONG), INTENT(OUT)       :: NUM_WORDS          ! Number of distinct words in CHAR_STRING
      INTEGER(LONG), PARAMETER         :: SUBR_BEGEND = PARSE_CHAR_STRING_BEGEND
 
      END SUBROUTINE PARSE_CHAR_STRING

   END INTERFACE

   END MODULE PARSE_CHAR_STRING_Interface

