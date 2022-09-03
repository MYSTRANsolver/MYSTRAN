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

   MODULE CONM2_PROC_1_Interface

   INTERFACE

      SUBROUTINE CONM2_PROC_1

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1Y
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, DATA_NAM_LEN, FATAL_ERR, MCMASS, MCONM2, MPMASS, MRCONM2, MRPMASS, NCMASS,  &
                                         NCONM2, NCORD, NGRID, NPMASS, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  CONM2_PROC_1_BEGEND
      USE MODEL_STUF, ONLY            :  CMASS, CONM2, PMASS, RCONM2, RPMASS, GRID, GRID_ID, CORD
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG

      IMPLICIT NONE
  
      CHARACTER(8*BYTE), PARAMETER    :: NAME      = 'CONM2   '
 
      INTEGER(LONG)                   :: NUM_RCONM2_RESET  ! No. RCONM2's reset to zero because they are connected to SPOINT's
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CONM2_PROC_1_BEGEND
 
      END SUBROUTINE CONM2_PROC_1

   END INTERFACE

   END MODULE CONM2_PROC_1_Interface

