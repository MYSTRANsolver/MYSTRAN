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

   MODULE BCHECK_2D_Interface

   INTERFACE

      SUBROUTINE BCHECK_2D ( B, BTYPE, ID, NROWB, NCOLB, NUM_GRIDS, XB, XL, BW )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  BUG, F04, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BCHECK_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, TWO
      USE MODEL_STUF, ONLY            :  ELDOF, NELGP, TE
      USE MODEL_STUF, ONLY            :  AGRID, ELGP
 
      IMPLICIT NONE
  
      CHARACTER(LEN=*), INTENT(IN)    :: BTYPE             ! Type of B matrix ('M' for membrane, 'B' for bending, 'S' for shear)
  
      INTEGER(LONG), INTENT(IN)       :: NROWB             ! Number of rows in the input B matrix
      INTEGER(LONG), INTENT(IN)       :: NCOLB             ! Number of cols in the input B matrix
      INTEGER(LONG), INTENT(IN)       :: NUM_GRIDS         ! Number of grids for the input B matrix
      INTEGER(LONG), INTENT(IN)       :: ID(NCOLB)         ! List of elem DOF's for each of the elem grids (e.g 3,4,5 for each of
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BCHECK_BEGEND
  
      REAL(DOUBLE) , INTENT(IN)       :: B(NROWB,NCOLB)    ! Strain-displ matrix
      REAL(DOUBLE) , INTENT(IN)       :: XB(NUM_GRIDS,3)   ! Basic coords of elem grids (diff than XEB for TPLT2's in a MIN4T QUAD4)
      REAL(DOUBLE) , INTENT(IN)       :: XL(NUM_GRIDS,3)   ! Local coords of elem grids (diff than XEL for TPLT2's in a MIN4T QUAD4)
      REAL(DOUBLE) , INTENT(OUT)      :: BW(NROWB,14)      ! Output from subr BCHECK_2D (matrix of NROWB elem strains for various
      END SUBROUTINE BCHECK_2D

   END INTERFACE

   END MODULE BCHECK_2D_Interface

