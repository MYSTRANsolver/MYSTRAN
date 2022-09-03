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

   MODULE WRITE_FIJFIL_Interface

   INTERFACE

      SUBROUTINE WRITE_FIJFIL ( WHICH, JVEC )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, F04, F06, F21, F22, F23, F24, F25, F21_MSG, F22_MSG, F23_MSG, F24_MSG, F25_MSG
      USE DEBUG_PARAMETERS
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MAX_STRESS_POINTS, NSUB, NTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_FIJFIL_BEGEND
      USE MODEL_STUF, ONLY            :  EID, TYPE, ELGP, ELDOF, KE, ME, PEB, PEG, PEL, PPE, PTE,                                  &
                                         SE1, SE2, SE3, STE1, STE2, STE3, UEB, UEG, UEL 
      USE PARAMS, ONLY                :  ELFORCEN

      IMPLICIT NONE
 
      INTEGER(LONG), INTENT(IN)       :: JVEC              ! Internal subcase or vector number for data to be written
      INTEGER(LONG), INTENT(IN)       :: WHICH             ! Which F2j file to write to
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_FIJFIL_BEGEND
 
      END SUBROUTINE WRITE_FIJFIL

   END INTERFACE

   END MODULE WRITE_FIJFIL_Interface

