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

   MODULE ELMTLB_Interface

   INTERFACE

      SUBROUTINE ELMTLB ( OPT )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, f06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MELDOF, NSUB, NTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELMTLB_BEGEND
      USE MODEL_STUF, ONLY            :  ELDOF, ELGP, KE, KED, ME, PTE, PPE, TE
  
      IMPLICIT NONE
 
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)

      INTEGER(LONG), PARAMETER        :: NCOLA     = 3     ! No. cols in a matrix for subr MATMULT_FFF/MATMULT_FFF_T, called herein
      INTEGER(LONG), PARAMETER        :: NROW      = 3     ! No. rows to get/put for subrs MATGET/MATPUT, called herein
      INTEGER(LONG), PARAMETER        :: NROWA     = 3     ! No. rows in a matrix for subr MATMULT_FFF/MATMULT_FFF_T, called herein
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELMTLB_BEGEND
  
      END SUBROUTINE ELMTLB

   END INTERFACE

   END MODULE ELMTLB_Interface

