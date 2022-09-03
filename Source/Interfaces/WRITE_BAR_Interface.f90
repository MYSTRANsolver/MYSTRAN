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

   MODULE WRITE_BAR_Interface

   INTERFACE

      SUBROUTINE WRITE_BAR (NUM, FILL_F06, FILL_ANS, ISUBCASE, ITABLE,  &
                            TITLE, SUBTITLE, LABEL,           &
                            FIELD5_INT_MODE, FIELD6_EIGENVALUE )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ANS, ERR, F04, F06
      USE SCONTR, ONLY                :  BARTOR, BLNK_SUB_NAM, MOGEL
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_BAR_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE LINK9_STUFF, ONLY           :  EID_OUT_ARRAY, MAXREQ, MSPRNT, OGEL
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: FILL_F06          ! Padding for output format
      CHARACTER(LEN=*), INTENT(IN)    :: FILL_ANS          ! Padding for output format
      INTEGER(LONG), INTENT(IN)       :: NUM               ! The number of rows of OGEL to write out
      INTEGER(LONG), INTENT(IN)       :: ISUBCASE          ! The subcase ID

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_BAR_BEGEND

      INTEGER(LONG), INTENT(IN)       :: ITABLE            ! the current op2 subtable, should be -3, -5, ...
      CHARACTER(LEN=128), INTENT(IN)  :: TITLE             ! the model TITLE
      CHARACTER(LEN=128), INTENT(IN)  :: SUBTITLE          ! the subcase SUBTITLE
      CHARACTER(LEN=128), INTENT(IN)  :: LABEL             ! the subcase LABEL
      INTEGER(LONG), INTENT(IN)       :: FIELD5_INT_MODE
      REAL(DOUBLE),  INTENT(IN)       :: FIELD6_EIGENVALUE
 
      END SUBROUTINE WRITE_BAR

   END INTERFACE

   END MODULE WRITE_BAR_Interface

