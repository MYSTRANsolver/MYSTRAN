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

   MODULE GET_GRID_6X6_MASS_Interface

   INTERFACE

      SUBROUTINE GET_GRID_6X6_MASS ( AGRID, IGRID, FOUND, GRID_MGG )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NGRID, NTERM_MGG
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_GRID_6X6_MASS_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DOF_TABLES, ONLY            :  TDOF
      USE SPARSE_MATRICES, ONLY       :  I2_MGG, J_MGG, MGG
 
      IMPLICIT NONE
  
      CHARACTER( 1*BYTE), INTENT(OUT) :: FOUND             ! 'Y' if there is a mass matrix for this grid and 'N' otherwise

      INTEGER(LONG), INTENT(IN)       :: AGRID             ! Actual grid number of grid for which we want the 6 x 6 mass matrix
      INTEGER(LONG), INTENT(IN)       :: IGRID             ! Internal grid number of grid for which we want the 6 x 6 mass matrix
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_GRID_6X6_MASS_BEGEND

      REAL(DOUBLE), INTENT(OUT)       :: GRID_MGG(6,6)     ! 6 x 6 mass matrix for internal grid IGRID

      END SUBROUTINE GET_GRID_6X6_MASS

   END INTERFACE

   END MODULE GET_GRID_6X6_MASS_Interface

