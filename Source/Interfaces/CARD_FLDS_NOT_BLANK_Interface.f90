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

   MODULE CARD_FLDS_NOT_BLANK_Interface

   INTERFACE

      SUBROUTINE CARD_FLDS_NOT_BLANK ( JCARD, FLD2, FLD3, FLD4, FLD5, FLD6, FLD7, FLD8, FLD9 )

  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, JCARD_LEN, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  CARD_FLDS_NOT_BLANK_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER(LEN=JCARD_LEN), INTENT(IN):: JCARD(10)         ! The 10 fields of 8 characters making up CARD
 
      INTEGER(LONG), INTENT(IN)           :: FLD2              ! Refers to field 2 of a B.D. card. If /= 0, then check this field 
      INTEGER(LONG), INTENT(IN)           :: FLD3              ! Refers to field 3 of a B.D. card. If /= 0, then check this field
      INTEGER(LONG), INTENT(IN)           :: FLD4              ! Refers to field 4 of a B.D. card. If /= 0, then check this field
      INTEGER(LONG), INTENT(IN)           :: FLD5              ! Refers to field 5 of a B.D. card. If /= 0, then check this field
      INTEGER(LONG), INTENT(IN)           :: FLD6              ! Refers to field 6 of a B.D. card. If /= 0, then check this field
      INTEGER(LONG), INTENT(IN)           :: FLD7              ! Refers to field 7 of a B.D. card. If /= 0, then check this field
      INTEGER(LONG), INTENT(IN)           :: FLD8              ! Refers to field 8 of a B.D. card. If /= 0, then check this field
      INTEGER(LONG), INTENT(IN)           :: FLD9              ! Refers to field 9 of a B.D. card. If /= 0, then check this field
      INTEGER(LONG), PARAMETER            :: SUBR_BEGEND = CARD_FLDS_NOT_BLANK_BEGEND
 
      END SUBROUTINE CARD_FLDS_NOT_BLANK

   END INTERFACE

   END MODULE CARD_FLDS_NOT_BLANK_Interface

