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

   MODULE B3D_ISOPARAMETRIC_Interface

   INTERFACE

      SUBROUTINE B3D_ISOPARAMETRIC ( DPSHX, GAUSS_PT, IGAUS, JGAUS, KGAUS, MESSAG, WRT_BUG_THIS_TIME, BMAT )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_BUG, WRT_ERR, WRT_LOG, BUG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_BMAT_BIT, ELDT_BUG_BCHK_BIT
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  B3D_ISOPARAMETRIC_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  EID, ELGP, TYPE
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      IMPLICIT NONE
  
      CHARACTER(LEN=*)  , INTENT(IN)  :: MESSAG            ! Messag to print out if BCHECK is run
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRT_BUG_THIS_TIME ! If 'Y' then write to BUG file if WRT_BUG array says to

      INTEGER(LONG), INTENT(IN)       :: GAUSS_PT          ! Gauss point number (used for DEBUG output)
      INTEGER(LONG), INTENT(IN)       :: IGAUS             ! I index of Gaus point (needed for some optional output)
      INTEGER(LONG), INTENT(IN)       :: JGAUS             ! J index of Gaus point (needed for some optional output)
      INTEGER(LONG), INTENT(IN)       :: KGAUS             ! K index of Gaus point (needed for some optional output)
      INTEGER(LONG), PARAMETER        :: NR      = 6       ! An input to subr BCHECK, called herein
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = B3D_ISOPARAMETRIC_BEGEND
  
      REAL(DOUBLE) , INTENT(IN)       :: DPSHX(3,ELGP)     ! Derivatives of the 4 node bilinear isopar interps wrt elem x and y
      REAL(DOUBLE) , INTENT(OUT)      :: BMAT(6,3*ELGP)    ! Output strain-displ matrix for this elem
      END SUBROUTINE B3D_ISOPARAMETRIC

   END INTERFACE

   END MODULE B3D_ISOPARAMETRIC_Interface

