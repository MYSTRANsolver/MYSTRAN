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

   MODULE CC_ELDA_Interface

   INTERFACE

      SUBROUTINE CC_ELDA ( CARD )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  FATAL_ERR, WARN_ERR, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  CC_ELDA_BEGEND
      USE MODEL_STUF, ONLY            :  CCELDT
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
 
      INTEGER(LONG), PARAMETER        :: IOUTMIN_BUG = 0   ! Min val of IOUT (=1,2,3,4,5,6,7,8,9 are the ELDATA print options)
      INTEGER(LONG), PARAMETER        :: IOUTMAX_BUG = 9   ! Max val of IOUT (=1,2,3,4,5,6,7,8,9 are the ELDATA print options)
      INTEGER(LONG), PARAMETER        :: IOUTMIN_FIJ = 2   ! Min val of IOUT (=1,2,3,4,5,6,7,8,9 are the ELDATA file  options)
      INTEGER(LONG), PARAMETER        :: IOUTMAX_FIJ = 6   ! Max val of IOUT (=1,2,3,4,5,6,7,8,9 are the ELDATA file  options)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CC_ELDA_BEGEND
 
      END SUBROUTINE CC_ELDA

   END INTERFACE

   END MODULE CC_ELDA_Interface

