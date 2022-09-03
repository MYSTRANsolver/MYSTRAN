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

   MODULE QPLT3_Interface

   INTERFACE

      SUBROUTINE QPLT3 ( OPT, AREA_QUAD, XSD, YSD, BIG_BB )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MEFE, MIN4T_QUAD4_TRIA_NO, NSUB, NTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  QPLT3_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, QUARTER, HALF, ONE, TWO, FOUR, CONV_RAD_DEG, PI
      USE PARAMS, ONLY                :  MIN4TRED
      USE MACHINE_PARAMS, ONLY        :  MACH_SFMIN
      USE MODEL_STUF, ONLY            :  BE2, BE3, DT, EID, ELDOF, EMG_IFE, ERR_SUB_NAM,NUM_EMG_FATAL_ERRS,                        &
                                         FCONV, KE, PHI_SQ, PPE, PTE, SE2, SE3, TE, XEB, XEL
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      IMPLICIT NONE 
  
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices.
      CHARACTER(1*BYTE)               :: OPT_MIN4T(6)      ! Values of OPT to use in this subr. We need OPT(4) = 'Y' if the
      CHARACTER(LEN=1), PARAMETER     :: MN4T_QD   = 'Y'   ! Arg used in call to TPLT2 to say the triangular elem is part of a QUAD4

      INTEGER(LONG), PARAMETER        :: IDV(9)  = (/ 1, & ! IDV( 1) =  1 means tria elem virgin DOF 1 is MYSTRAN elem DOF  1
                                                      4, & ! IDV( 2) =  4 means tria elem virgin DOF 2 is MYSTRAN elem DOF  4
                                                      7, & ! IDV( 3) =  7 means tria elem virgin DOF 3 is MYSTRAN elem DOF  7
                                                      2, & ! IDV( 4) =  2 means tria elem virgin DOF 4 is MYSTRAN elem DOF  2
                                                      5, & ! IDV( 5) =  5 means tria elem virgin DOF 5 is MYSTRAN elem DOF  5
                                                      8, & ! IDV( 6) =  8 means tria elem virgin DOF 6 is MYSTRAN elem DOF  8
                                                      3, & ! IDV( 7) =  3 means tria elem virgin DOF 7 is MYSTRAN elem DOF  3
                                                      6, & ! IDV( 8) =  6 means tria elem virgin DOF 8 is MYSTRAN elem DOF  6
                                                      9 /) ! IDV( 9) =  9 means tria elem virgin DOF 9 is MYSTRAN elem DOF  9

      INTEGER(LONG), PARAMETER        :: IDM(12) = (/ 3, & ! IDM( 1) =  3 means quad elem DOF  1 is MYSTRAN elem DOF  3
                                                      4, & ! IDM( 2) =  4 means quad elem DOF  2 is MYSTRAN elem DOF  4
                                                      5, & ! IDM( 3) =  5 means quad elem DOF  3 is MYSTRAN elem DOF  5
                                                      9, & ! IDM( 4) =  8 means quad elem DOF  4 is MYSTRAN elem DOF  9
                                                     10, & ! IDM( 5) = 10 means quad elem DOF  5 is MYSTRAN elem DOF 10
                                                     11, & ! IDM( 6) = 11 means quad elem DOF  6 is MYSTRAN elem DOF 11
                                                     15, & ! IDM( 7) = 15 means quad elem DOF  7 is MYSTRAN elem DOF 15
                                                     16, & ! IDM( 8) = 16 means quad elem DOF  8 is MYSTRAN elem DOF 16
                                                     17, & ! IDM( 9) = 17 means quad elem DOF  9 is MYSTRAN elem DOF 17
                                                     21, & ! IDM(10) = 21 means quad elem DOF 10 is MYSTRAN elem DOF 21
                                                     22, & ! IDM(11) = 22 means quad elem DOF 11 is MYSTRAN elem DOF 22
                                                     23 /) ! IDM(12) = 23 means quad elem DOF 12 is MYSTRAN elem DOF 23

      INTEGER(LONG), PARAMETER        :: NUM_TRIAS = 4     ! DO NOT CHANGE THIS. Num of triangles that subdivide the QUAD4
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = QPLT3_BEGEND
  
      REAL(DOUBLE) , INTENT(IN)       :: AREA_QUAD         ! Element area
      REAL(DOUBLE) , INTENT(IN)       :: XSD(4)            ! Diffs in x coords of quad sides in local coords
      REAL(DOUBLE) , INTENT(IN)       :: YSD(4)            ! Diffs in y coords of quad sides in local coords

      REAL(DOUBLE) , INTENT(OUT)      :: BIG_BB(3,ELDOF,4)

      REAL(DOUBLE)                    :: DUM3(3,18,1)      ! Matrix returned from subr TPLT2 not used here (BIG_BB for a tria but we
      END SUBROUTINE QPLT3

   END INTERFACE

   END MODULE QPLT3_Interface

