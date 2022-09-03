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

   MODULE IP6CHK_Interface

   INTERFACE

      SUBROUTINE IP6CHK ( JCARDI, JCARDO, IP6TYP, TOTAL_NUM_DIGITS )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, JCARD_LEN, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  IP6CHK_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER(LEN=JCARD_LEN), INTENT(IN):: JCARDI            ! Input 8 character field
      CHARACTER(8*BYTE), INTENT(OUT)      :: IP6TYP            ! Descriptor of JCARDI, see above
      CHARACTER(LEN(JCARDI)), INTENT(OUT) :: JCARDO            ! Output 8 character field, described above
 
      INTEGER(LONG), INTENT(OUT)          :: TOTAL_NUM_DIGITS  ! Total of NUM_DIGITS(I)
      INTEGER(LONG), PARAMETER            :: SUBR_BEGEND = IP6CHK_BEGEND
 
      END SUBROUTINE IP6CHK

   END INTERFACE

   END MODULE IP6CHK_Interface

