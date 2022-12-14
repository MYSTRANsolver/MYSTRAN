! ###############################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2022 Dr William R Case, Jr (mystransolver@gmail.com)                                              
                                                                                                         
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

   MODULE MGGC_MASS_MATRIX_Interface

   INTERFACE

      SUBROUTINE MGGC_MASS_MATRIX

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  NGRID, NTERM_MGGC, BLNK_SUB_NAM
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  EPSIL, SPARSTOR, WTMASS
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  MGGC_MASS_MATRIX_BEGEND
      USE MODEL_STUF, ONLY            :  AGRID, GRID_ID, INV_GRID_SEQ
      USE SPARSE_MATRICES, ONLY       :  I_MGGC, J_MGGC, MGGC
 
      IMPLICIT NONE
  
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT

      INTEGER(LONG)                   :: KSTART            ! Used in deciding whether to process all elem mass terms or only
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MGGC_MASS_MATRIX_BEGEND

      END SUBROUTINE MGGC_MASS_MATRIX

   END INTERFACE

   END MODULE MGGC_MASS_MATRIX_Interface

