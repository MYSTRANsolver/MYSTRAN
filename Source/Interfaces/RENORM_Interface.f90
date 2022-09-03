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

   MODULE RENORM_Interface

   INTERFACE

      SUBROUTINE RENORM ( VEC_NUM, NORM_GRD, NORM_COMP, NORM, NORM_GSET_DOF, GEN_MASS1, PHI_SCALE_FAC )

  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NDOFG, NDOFG, NGRID, WARN_ERR
      USE PARAMS, ONLY                :  EPSIL, SUPWARN
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  RENORM_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE COL_VECS, ONLY              :  UG_COL
  
      IMPLICIT NONE
  
      CHARACTER( 8*BYTE), INTENT(IN)  :: NORM              ! Eigenvector renormalization methof from EIGR card (e.g. 'MAX     ')
        
      INTEGER(LONG), INTENT(IN)       :: NORM_COMP         ! Comp. (1-6) for renormalizing eigenvectors (from EIGR card)
      INTEGER(LONG), INTENT(IN)       :: NORM_GRD          ! Grid Point  for renormalizing eigenvectors (from EIGR card)
      INTEGER(LONG), INTENT(IN)       :: NORM_GSET_DOF     ! G-set DOF no. for NORM_GRD/NORM_COMP 
      INTEGER(LONG), INTENT(IN)       :: VEC_NUM           ! Number used to control an output message (only want this information
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = RENORM_BEGEND
  
      REAL(DOUBLE) , INTENT(INOUT)    :: GEN_MASS1         ! Generalized mass for 1 eigenvector
      REAL(DOUBLE) , INTENT(OUT)      :: PHI_SCALE_FAC     ! Scale factor for the eigenvector to renormalize it
      REAL(DOUBLE)                    :: PHI_POINT         ! Variable used when normalizing gen. mass and eigenvectors
  
      END SUBROUTINE RENORM

   END INTERFACE

   END MODULE RENORM_Interface

