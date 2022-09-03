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

   MODULE SURFACE_FIT_Interface

   INTERFACE

      SUBROUTINE SURFACE_FIT ( NUM_FITS, NUM_COEFFS, XI, YI, WI, XO, YO, WO, DEB, MESSAGE, OUNT, POLY_PCT_ERR, PCT_ERR_MAX, IERR )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE SUBR_BEGEND_LEVELS, ONLY    :  SURFACE_FIT_BEGEND
      USE LSQ_MYSTRAN

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: MESSAGE               ! Message printed if debug output is printed

      INTEGER(LONG), INTENT(IN)       :: DEB                   ! If > 0 then DEB_SURFACE_FIT will be run
      INTEGER(LONG), INTENT(IN)       :: NUM_FITS              ! Number of data points to fit
      INTEGER(LONG), INTENT(IN)       :: NUM_COEFFS            ! Number of coefficients in the fitting polynomial
      INTEGER(LONG), INTENT(IN)       :: OUNT(2)               ! Output units for SURFACE_FIT
      INTEGER(LONG), INTENT(OUT)      :: IERR                  ! Error indicator
      INTEGER(LONG), PARAMETER        :: MAX_COEFFS = 6        ! Maximum number of coefficients coded for ther polynomial fit
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SURFACE_FIT_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: WI(NUM_FITS)          ! Values of the function to fit at the input data points
      REAL(DOUBLE), INTENT(IN)        :: XI(NUM_FITS)          ! X coords of the input  data points
      REAL(DOUBLE), INTENT(IN)        :: YI(NUM_FITS)          ! Y coords of the input  data points
      REAL(DOUBLE), INTENT(IN)        :: XO(NUM_FITS)          ! X coords of the output data points
      REAL(DOUBLE), INTENT(IN)        :: YO(NUM_FITS)          ! Y coords of the output data points
      REAL(DOUBLE), INTENT(OUT)       :: POLY_PCT_ERR(NUM_FITS)! % difference between fitted & actual data (norm'd to max act data)
      REAL(DOUBLE), INTENT(OUT)       :: PCT_ERR_MAX           ! Max value from array POLY_PCT_ERR (max error at any input data pt)
      REAL(DOUBLE), INTENT(OUT)       :: WO(NUM_FITS)          ! Values of the function to fit at the output data points

      REAL(DOUBLE), PARAMETER         :: WT = ONE            ! Parameter

      END SUBROUTINE SURFACE_FIT

   END INTERFACE

   END MODULE SURFACE_FIT_Interface

