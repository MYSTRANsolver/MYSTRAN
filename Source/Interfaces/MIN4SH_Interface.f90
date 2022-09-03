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

   MODULE MIN4SH_Interface

   INTERFACE

      SUBROUTINE MIN4SH ( SSI, SSJ, XSD, YSD, WRT_BUG_THIS_TIME, NXSH, NYSH, DNXSHG, DNYSHG )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  BUG, F04, F06, WRT_BUG, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  MIN4SH_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO, EIGHT
  
      IMPLICIT NONE
  
      CHARACTER(17*BYTE)              :: NAME(2)           ! Used for BUG output annotation
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRT_BUG_THIS_TIME ! If 'Y' then write to BUG file if WRT_BUG array says to

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MIN4SH_BEGEND
  
      REAL(DOUBLE) , INTENT(IN)       :: SSI               ! Gauss point coordinate
      REAL(DOUBLE) , INTENT(IN)       :: SSJ               ! Gauss point coordinate
      REAL(DOUBLE) , INTENT(IN)       :: XSD(4)            ! 1-D arrays of differences in x side dimensions (local)
      REAL(DOUBLE) , INTENT(IN)       :: YSD(4)            ! 1-D arrays of differences in y side dimensions (local)
      REAL(DOUBLE) , INTENT(OUT)      :: NXSH(4)           ! Constrained Nx shape functions for the MIN4 quad element
      REAL(DOUBLE) , INTENT(OUT)      :: NYSH(4)           ! Constrained Ny shape functions for the MIN4 quad element
      REAL(DOUBLE) , INTENT(OUT)      :: DNXSHG(2,4)       ! Derivatives of NXSH wrt xi, eta.
      REAL(DOUBLE) , INTENT(OUT)      :: DNYSHG(2,4)       ! Derivatives of NYSH wrt xi, eta.
      REAL(DOUBLE)                    :: N5,N6,N7,N8       ! N5 thru N8 are the virgin shape functions used in finding the
      REAL(DOUBLE)                    :: X12               ! Intermediate variable used in calculating outputs
      REAL(DOUBLE)                    :: X23               ! Intermediate variable used in calculating outputs
      REAL(DOUBLE)                    :: X34               ! Intermediate variable used in calculating outputs
      REAL(DOUBLE)                    :: X41               ! Intermediate variable used in calculating outputs
      REAL(DOUBLE)                    :: Y12               ! Intermediate variable used in calculating outputs
      REAL(DOUBLE)                    :: Y23               ! Intermediate variable used in calculating outputs
      REAL(DOUBLE)                    :: Y34               ! Intermediate variable used in calculating outputs
      REAL(DOUBLE)                    :: Y41               ! Intermediate variable used in calculating outputs
      REAL(DOUBLE)                    :: XM                ! Intermediate variable used in calculating outputs
      REAL(DOUBLE)                    :: XP                ! Intermediate variable used in calculating outputs
      REAL(DOUBLE)                    :: X2M               ! Intermediate variable used in calculating outputs
      REAL(DOUBLE)                    :: YM                ! Intermediate variable used in calculating outputs
      REAL(DOUBLE)                    :: YP                ! Intermediate variable used in calculating outputs
      REAL(DOUBLE)                    :: Y2M               ! Intermediate variable used in calculating outputs
  
      END SUBROUTINE MIN4SH

   END INTERFACE

   END MODULE MIN4SH_Interface

