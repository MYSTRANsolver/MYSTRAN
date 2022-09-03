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

   MODULE RIGID_BODY_DISP_MAT_Interface

   INTERFACE

       SUBROUTINE RIGID_BODY_DISP_MAT ( GRD_COORDS, REF_COORDS, RB_DISP )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE SUBR_BEGEND_LEVELS, ONLY    :  RIGID_BODY_DISP_MAT_BEGEND

      IMPLICIT NONE

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = RIGID_BODY_DISP_MAT_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: GRD_COORDS(3)     ! Coords of grid point for which the RB matrix is to be formulated
      REAL(DOUBLE) , INTENT(IN)       :: REF_COORDS(3)     ! Coords of reference grid (grid about which the RB disps occur)
      REAL(DOUBLE) , INTENT(OUT)      :: RB_DISP(6,6)      ! The set of 6 RB displ vectors for the 6 disp components for GRID_NUM

      END SUBROUTINE RIGID_BODY_DISP_MAT

   END INTERFACE

   END MODULE RIGID_BODY_DISP_MAT_Interface

