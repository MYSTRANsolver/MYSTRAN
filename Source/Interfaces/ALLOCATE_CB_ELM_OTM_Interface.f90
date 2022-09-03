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

   MODULE ALLOCATE_CB_ELM_OTM_Interface

   INTERFACE

      SUBROUTINE ALLOCATE_CB_ELM_OTM ( NAME_IN )


      USE PENTIUM_II_KIND
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG

      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR,                                                                  &
                                         ELOUT_ELFE_BIT, ELOUT_ELFN_BIT, ELOUT_STRE_BIT, ELOUT_STRN_BIT,                           &
                                         IBIT, NELE, NUM_CB_DOFS,                                                                  &
                                         NROWS_OTM_ELFE, NROWS_OTM_ELFN, NROWS_OTM_STRE, NROWS_OTM_STRN,                           &
                                         NROWS_TXT_ELFE, NROWS_TXT_ELFN, NROWS_TXT_STRE, NROWS_TXT_STRN, TOT_MB_MEM_ALLOC

      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONEPP6
      USE MODEL_STUF, ONLY            :  ELGP, ELOUT, ELMTYP, ETYPE, METYPE, NELGP, TYPE
      USE CC_OUTPUT_DESCRIBERS, ONLY  :  STRN_LOC, STRE_LOC
      USE OUTPUT4_MATRICES, ONLY      :  OTM_ELFE, OTM_ELFN, OTM_STRE, OTM_STRN, TXT_ELFE, TXT_ELFN, TXT_STRE, TXT_STRN
      USE PARAMS, ONLY                :  OTMSKIP
      USE SUBR_BEGEND_LEVELS, ONLY    :  ALLOCATE_CB_ELM_OTM_BEGEND

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: NAME_IN           ! Array name of the matrix to be allocated

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ALLOCATE_CB_ELM_OTM_BEGEND

      END SUBROUTINE ALLOCATE_CB_ELM_OTM

   END INTERFACE

   END MODULE ALLOCATE_CB_ELM_OTM_Interface

