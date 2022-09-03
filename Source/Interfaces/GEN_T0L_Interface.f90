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

   MODULE GEN_T0L_Interface

   INTERFACE

      SUBROUTINE GEN_T0L (RGRID_ROW, ICORD, THETAD, PHID, T0L )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, ONE80, PI
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04, f06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE PARAMS, ONLY                :  EPSIL
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  GEN_T0L_BEGEND
      USE MODEL_STUF, ONLY            :  RGRID, CORD, RCORD
 
      IMPLICIT NONE
 
      INTEGER(LONG), INTENT(IN)       :: RGRID_ROW         ! Row number in array RGRID where the RGRID data is stored for the grid
      INTEGER(LONG), INTENT(IN)       :: ICORD             ! Internal coord ID for coord sys L
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GEN_T0L_BEGEND
 
      REAL(DOUBLE),  INTENT(OUT)      :: THETAD,PHID       ! Azimuth and elevation angles (deg) for cylindrical/spherical coord sys 
      REAL(DOUBLE),  INTENT(OUT)      :: T0L(3,3)          ! 3 x 3 coord transformation matrix described above
      END SUBROUTINE GEN_T0L

   END INTERFACE

   END MODULE GEN_T0L_Interface

