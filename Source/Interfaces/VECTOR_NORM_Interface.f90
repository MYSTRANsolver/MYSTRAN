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

   MODULE VECTOR_NORM_Interface

   INTERFACE

      SUBROUTINE VECTOR_NORM ( VEC, NSIZE, WHICH, VEC_NORM, IERR )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO 
      USE SUBR_BEGEND_LEVELS, ONLY    :  VECTOR_NORM_BEGEND

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: WHICH             ! Which norm to calculate (see below)

      INTEGER(LONG)   , INTENT(IN)    :: NSIZE             ! Extent of VEC
      INTEGER(LONG)   , INTENT(OUT)   :: IERR              ! Error indicator
      INTEGER(LONG)   , PARAMETER     :: SUBR_BEGEND = VECTOR_NORM_BEGEND
 
      REAL(DOUBLE)    , INTENT(IN)    :: VEC(NSIZE)        ! The vector for which the norm will be calculated
      REAL(DOUBLE)    , INTENT(OUT)   :: VEC_NORM          ! The norm calculated for VEC

      END SUBROUTINE VECTOR_NORM

   END INTERFACE

   END MODULE VECTOR_NORM_Interface

