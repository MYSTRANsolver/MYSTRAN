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

   MODULE BD_IMBEDDED_BLANK_Interface

   INTERFACE

      SUBROUTINE BD_IMBEDDED_BLANK ( JCARD, CF2, CF3, CF4, CF5, CF6, CF7, CF8, CF9 )

  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  FATAL_ERR, BLNK_SUB_NAM, JCARD_LEN
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_IMBEDDED_BLANK_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: JCARD(10)         ! The 10 fields of characters making up CARD
 
      INTEGER(LONG), INTENT(IN)       :: CF2               ! = 2 if field 2 is to be checked, or 0 otherwise
      INTEGER(LONG), INTENT(IN)       :: CF3               ! = 3 if field 3 is to be checked, or 0 otherwise
      INTEGER(LONG), INTENT(IN)       :: CF4               ! = 4 if field 4 is to be checked, or 0 otherwise
      INTEGER(LONG), INTENT(IN)       :: CF5               ! = 5 if field 5 is to be checked, or 0 otherwise
      INTEGER(LONG), INTENT(IN)       :: CF6               ! = 6 if field 6 is to be checked, or 0 otherwise
      INTEGER(LONG), INTENT(IN)       :: CF7               ! = 7 if field 7 is to be checked, or 0 otherwise
      INTEGER(LONG), INTENT(IN)       :: CF8               ! = 8 if field 8 is to be checked, or 0 otherwise
      INTEGER(LONG), INTENT(IN)       :: CF9               ! = 9 if field 9 is to be checked, or 0 otherwise
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_IMBEDDED_BLANK_BEGEND
 
      END SUBROUTINE BD_IMBEDDED_BLANK

   END INTERFACE

   END MODULE BD_IMBEDDED_BLANK_Interface

