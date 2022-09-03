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

   MODULE ELEPRO_Interface

   INTERFACE

      SUBROUTINE ELEPRO ( INCR_NELE, JCARD, NFIELD, NMORE,                                                                         &

                          CHK_FLD2, CHK_FLD3, CHK_FLD4, CHK_FLD5, CHK_FLD6, CHK_FLD7, CHK_FLD8, CHK_FLD9 )
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  IERRFL, FATAL_ERR, JF, LEDAT, LELE, NEDAT, NELE, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELEPRO_BEGEND
      USE MODEL_STUF, ONLY            :  EDAT, EPNT
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: CHK_FLD2          ! If 'N', then if field 2 is blank it will not be checked for > 0
      CHARACTER(LEN=*), INTENT(IN)    :: CHK_FLD3          ! If 'N', then if field 3 is blank it will not be checked for > 0
      CHARACTER(LEN=*), INTENT(IN)    :: CHK_FLD4          ! If 'N', then if field 4 is blank it will not be checked for > 0
      CHARACTER(LEN=*), INTENT(IN)    :: CHK_FLD5          ! If 'N', then if field 5 is blank it will not be checked for > 0
      CHARACTER(LEN=*), INTENT(IN)    :: CHK_FLD6          ! If 'N', then if field 6 is blank it will not be checked for > 0
      CHARACTER(LEN=*), INTENT(IN)    :: CHK_FLD7          ! If 'N', then if field 7 is blank it will not be checked for > 0
      CHARACTER(LEN=*), INTENT(IN)    :: CHK_FLD8          ! If 'N', then if field 8 is blank it will not be checked for > 0
      CHARACTER(LEN=*), INTENT(IN)    :: CHK_FLD9          ! If 'N', then if field 9 is blank it will not be checked for > 0
      CHARACTER(LEN=*), INTENT(IN)    :: INCR_NELE         ! If 'Y', increment NELE. Otherwise do not increment NELE
      CHARACTER(LEN=*), INTENT(IN)    :: JCARD(10)         ! The 10 fields of a Bulk Data card
 
      INTEGER(LONG), INTENT(IN)       :: NFIELD            ! Number of card fields to read from JCARD (start w/ field 2) 
      INTEGER(LONG), INTENT(IN)       :: NMORE             ! Number of terms that have to be written to EDAT for this element
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELEPRO_BEGEND
 
      END SUBROUTINE ELEPRO

   END INTERFACE

   END MODULE ELEPRO_Interface

