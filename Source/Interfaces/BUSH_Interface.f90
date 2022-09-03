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

   MODULE BUSH_Interface

   INTERFACE

      SUBROUTINE BUSH ( INT_ELEM_ID, OPT, WRITE_WARN )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NCORD
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, HALF, QUARTER
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE MODEL_STUF, ONLY            :  BE1, BE2, BUSH_OCID, BUSHOFF, BUSH_DXA, BUSH_DXB, BUSH_DY, BUSH_DZ, CORD, EOFF, EPROP, KE,&
                                         ELEM_LEN_12, OFFDIS_B, RCORD, SE1, SE2, TE, XEB
      USE SUBR_BEGEND_LEVELS, ONLY    :  BUSH_BEGEND

      IMPLICIT NONE 
 
      CHARACTER(LEN=*) , INTENT(IN)   :: WRITE_WARN        ! If 'Y" write warning messages, otherwise do not
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BUSH_BEGEND
      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID       ! Internal element ID

      END SUBROUTINE BUSH

   END INTERFACE

   END MODULE BUSH_Interface

