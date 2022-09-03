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

   MODULE GET_OU4_MAT_STATS_Interface

   INTERFACE

      SUBROUTINE GET_OU4_MAT_STATS ( MAT, NROWS, NCOLS, FORM, SYM )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFA, NDOFG, NDOFL, NDOFR, NUM_CB_DOFS, NSUB, NVEC
      USE IOUNT1, ONLY                :  ERR, F06
      USE MODEL_STUF, ONLY            :  MCG
      USE PARAMS, ONLY                :  MPFOUT
      USE OUTPUT4_MATRICES

      IMPLICIT NONE

      CHARACTER(LEN=*) , INTENT(IN)   :: MAT               ! Name of matrix to get row, col size for
      CHARACTER(1*BYTE), INTENT(OUT)  :: SYM               ! Y if matrix stored symmetrically

      INTEGER(LONG)   , INTENT(OUT)   :: FORM              ! Matrix format
      INTEGER(LONG)   , INTENT(OUT)   :: NCOLS             ! Number of cols in MAT_NAME
      INTEGER(LONG)   , INTENT(OUT)   :: NROWS             ! Number of rows in MAT_NAME

      END SUBROUTINE GET_OU4_MAT_STATS

   END INTERFACE

   END MODULE GET_OU4_MAT_STATS_Interface

