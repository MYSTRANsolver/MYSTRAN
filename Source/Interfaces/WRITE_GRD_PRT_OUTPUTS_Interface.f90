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

   MODULE WRITE_GRD_PRT_OUTPUTS_Interface

   INTERFACE

      SUBROUTINE WRITE_GRD_PRT_OUTPUTS ( JVEC, NUM, WHAT, IHDR, ALL_SAME_CID, WRITE_OGEL )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ANS, ERR, F04, F06, PCH
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, INT_SC_NUM, MELGP, MOGEL, NDOFR, NVEC, NUM_CB_DOFS,              &
                                         SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_GRD_PRT_OUTPUTS_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE LINK9_STUFF, ONLY           :  GID_OUT_ARRAY, MAXREQ, OGEL
      USE MODEL_STUF, ONLY            :  LABEL, SCNUM, SUBLOD, STITLE, TITLE
      USE MACHINE_PARAMS, ONLY        :  MACH_LARGE_NUM  
  
      IMPLICIT NONE
 
      CHARACTER(LEN=*) , INTENT(IN)   :: IHDR              ! Indicator of whether to write an output header in this use of this subr
      CHARACTER(LEN=*) , INTENT(IN)   :: WHAT              ! Indicator whether to process displ or force output requests
      CHARACTER(1*BYTE), INTENT(IN)   :: ALL_SAME_CID      ! Indicator of whether all grids, for the output set, have the same
      INTEGER(LONG), INTENT(IN)       :: JVEC              ! Sol'n vector num. Can be internal subcase number or eigenvector number
      INTEGER(LONG), INTENT(IN)       :: NUM               ! The number of rows of OGEL to write out
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_GRD_PRT_OUTPUTS_BEGEND

      CHARACTER(1*BYTE), INTENT(IN)   :: WRITE_OGEL(NUM)   ! 'Y'/'N' as to whether to write OGEL for a grid (used to avoid writing
      END SUBROUTINE WRITE_GRD_PRT_OUTPUTS

   END INTERFACE

   END MODULE WRITE_GRD_PRT_OUTPUTS_Interface

