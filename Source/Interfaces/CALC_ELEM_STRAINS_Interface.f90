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

   MODULE CALC_ELEM_STRAINS_Interface

   INTERFACE

      SUBROUTINE CALC_ELEM_STRAINS ( SIZE_ALLOCATED, NUM1, NUM_FEMAP_ROWS, WRITE_OGEL, WRITE_FEMAP )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR 
      USE TIMDAT, ONLY                :  TSEC
      USE MODEL_STUF, ONLY            :  TYPE
      USE SUBR_BEGEND_LEVELS, ONLY    :  CALC_ELEM_STRAINS_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
 
      IMPLICIT NONE

      CHARACTER( 1*BYTE), INTENT(IN)  :: WRITE_OGEL        ! If 'Y' then write data to array OGEL
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRITE_FEMAP       ! If 'Y' then write data to array FEMAP arrays
 
      INTEGER(LONG), INTENT(IN)       :: SIZE_ALLOCATED    ! No. of rows allocated to array that will be written to in a subr
      INTEGER(LONG), INTENT(IN)       :: NUM_FEMAP_ROWS    ! Number of rows that will be written to FEMAP arrays
      INTEGER(LONG), INTENT(INOUT)    :: NUM1              ! Cum rows written to OGEL prior to running this subr
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CALC_ELEM_STRAINS_BEGEND
 
      END SUBROUTINE CALC_ELEM_STRAINS

   END INTERFACE

   END MODULE CALC_ELEM_STRAINS_Interface

