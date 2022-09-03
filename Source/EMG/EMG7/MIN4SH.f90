! ##################################################################################################################################
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
  
      SUBROUTINE MIN4SH ( SSI, SSJ, XSD, YSD, WRT_BUG_THIS_TIME, NXSH, NYSH, DNXSHG, DNYSHG )
 
! Generates constrained shapes for use with MIN4 plate element for transverse shear. This is based on the paper referenced in subr
! QPLT2.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  BUG, F04, F06, WRT_BUG, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  MIN4SH_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO, EIGHT
  
      USE MIN4SH_USE_IFs

      IMPLICIT NONE
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MIN4SH'
      CHARACTER(17*BYTE)              :: NAME(2)           ! Used for BUG output annotation
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRT_BUG_THIS_TIME ! If 'Y' then write to BUG file if WRT_BUG array says to

      INTEGER(LONG)                   :: I,J               ! DO loop indices
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
                                                             ! constrained shape functions (NXSH, NYSH)
      REAL(DOUBLE)                    :: N5X,N6X,N7X,N8X   ! N5X thru N8X are derivatives of N5 thru N8 wrt xi
      REAL(DOUBLE)                    :: N5Y,N6Y,N7Y,N8Y   ! N5Y thru N8Y are derivatives of N5 thru N8 wrt eta
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
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC, WRT_BUG_THIS_TIME, WRT_BUG(7), WRT_BUG(8), WRT_BUG(9)
 9001    FORMAT(1X,A,' BEGN ',F10.3, 3X, A1, 3(I3))
      ENDIF
! **********************************************************************************************************************************
! Initialize outputs

      DO I=1,4
         NXSH(I) = ZERO
         NYSH(I) = ZERO
      ENDDO

      DO I=1,2
         DO J=1,4
            DNXSHG(I,J) = ZERO
            DNYSHG(I,J) = ZERO
         ENDDO
      ENDDO

! Get constants needed from local G.P. geometry. Xij = Xi - Xj  and  Yij = Yi - Yj where Xi, Yi, etc., are element
! node coords in local element coord system
  
      X12 = XSD(1)
      X23 = XSD(2)
      X34 = XSD(3)
      X41 = XSD(4)
  
      Y12 = YSD(1)
      Y23 = YSD(2)
      Y34 = YSD(3)
      Y41 = YSD(4)
  
      XM  = ONE - SSI
      XP  = ONE + SSI
      YM  = ONE - SSJ
      YP  = ONE + SSJ
      X2M = ONE - SSI*SSI
      Y2M = ONE - SSJ*SSJ
  
! N5 thru N8 are the virgin shape functions used in finding the  constrained shape functions (NXSH, NYSH)
  
      N5 = X2M*YM/TWO
      N6 = Y2M*XP/TWO
      N7 = X2M*YP/TWO
      N8 = Y2M*XM/TWO
  
! N5X thru N8Y are derivatives of N5 thru N8 wrt xi, eta
  
      N5X = -SSI*YM
      N6X =  Y2M/TWO
      N7X = -SSI*YP
      N8X = -Y2M/TWO
  
      N5Y = -X2M/TWO
      N6Y = -SSJ*XP
      N7Y =  X2M/TWO
      N8Y = -SSJ*XM
  
! Constrained shapes:
  
      NXSH(1) = (-Y41*N8 + Y12*N5)/EIGHT
      NXSH(2) = (-Y12*N5 + Y23*N6)/EIGHT
      NXSH(3) = (-Y23*N6 + Y34*N7)/EIGHT
      NXSH(4) = (-Y34*N7 + Y41*N8)/EIGHT
  
      NYSH(1) = (-X41*N8 + X12*N5)/EIGHT
      NYSH(2) = (-X12*N5 + X23*N6)/EIGHT
      NYSH(3) = (-X23*N6 + X34*N7)/EIGHT
      NYSH(4) = (-X34*N7 + X41*N8)/EIGHT
  
! Derivatives of NXSH wrt xi, eta:
  
      DNXSHG(1,1) = (-Y41*N8X + Y12*N5X)/EIGHT
      DNXSHG(1,2) = (-Y12*N5X + Y23*N6X)/EIGHT
      DNXSHG(1,3) = (-Y23*N6X + Y34*N7X)/EIGHT
      DNXSHG(1,4) = (-Y34*N7X + Y41*N8X)/EIGHT
  
      DNXSHG(2,1) = (-Y41*N8Y + Y12*N5Y)/EIGHT
      DNXSHG(2,2) = (-Y12*N5Y + Y23*N6Y)/EIGHT
      DNXSHG(2,3) = (-Y23*N6Y + Y34*N7Y)/EIGHT
      DNXSHG(2,4) = (-Y34*N7Y + Y41*N8Y)/EIGHT
  
! Derivatives of NYSH wrt xi, eta:
  
      DNYSHG(1,1) = (-X41*N8X + X12*N5X)/EIGHT
      DNYSHG(1,2) = (-X12*N5X + X23*N6X)/EIGHT
      DNYSHG(1,3) = (-X23*N6X + X34*N7X)/EIGHT
      DNYSHG(1,4) = (-X34*N7X + X41*N8X)/EIGHT
  
      DNYSHG(2,1) = (-X41*N8Y + X12*N5Y)/EIGHT
      DNYSHG(2,2) = (-X12*N5Y + X23*N6Y)/EIGHT
      DNYSHG(2,3) = (-X23*N6Y + X34*N7Y)/EIGHT
      DNYSHG(2,4) = (-X34*N7Y + X41*N8Y)/EIGHT
  
! **********************************************************************************************************************************
! Debug output:
 
      IF ((WRT_BUG_THIS_TIME == 'Y') .AND. (WRT_BUG(7) > 0)) THEN

         NAME(1) = 'Nodes  1 thru  4:'
         NAME(2) = 'Nodes  5 thru  8:'

         WRITE(BUG,1112)

         WRITE(BUG,1104)
         WRITE(BUG,1105) NAME(2), N5, N6, N7, N8
         WRITE(BUG,*)
         WRITE(BUG,*)

         WRITE(BUG,1115)
         WRITE(BUG,1105) NAME(2), N5X, N6X, N7X, N8X
         WRITE(BUG,*)
         WRITE(BUG,*)

         WRITE(BUG,1125)
         WRITE(BUG,1105) NAME(2), N5Y, N6Y, N7Y, N8Y
         WRITE(BUG,*)
         WRITE(BUG,*)

         WRITE(BUG,1204)
         WRITE(BUG,1105) NAME(2), (NXSH(I), I=1,4)
         WRITE(BUG,*)
         WRITE(BUG,*)

         WRITE(BUG,1205)
         WRITE(BUG,1105) NAME(2), (NYSH(I),I=1,4)
         WRITE(BUG,*)
         WRITE(BUG,*)

         WRITE(BUG,1304)
         WRITE(BUG,1105) NAME(1), (DNXSHG(1,J), J=1,4)
         WRITE(BUG,1105) NAME(2), (DNXSHG(2,J), J=1,4)
         WRITE(BUG,*)
         WRITE(BUG,*)

         WRITE(BUG,1305)
         WRITE(BUG,1105) NAME(1), (DNYSHG(1,J), J=1,4)
         WRITE(BUG,1105) NAME(2), (DNYSHG(2,J), J=1,4)
         WRITE(BUG,*)
         WRITE(BUG,*)

      ENDIF
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1102 FORMAT(45X,A,' Element ',I8,', Gauss point ',I3,//)

 1112 format(51x,'Outputs from subroutine MIN4SH',/,51x,'------------------------------',/)

 1104 FORMAT(48X,'Unconstrained shape functions N5 - N8')

 1105 FORMAT(23X,A,1X,4(1ES16.6))

 1115 FORMAT(35X,'Derivatives of unconstrained shape functions with respect to XI')

 1125 FORMAT(35X,'Derivatives of unconstrained shape functions with respect to ET')

 1204 FORMAT(49X,'Constrained shape functions NXSH(i)')

 1205 FORMAT(51X,'Constrained shape functions NYSH(i)')

 1304 FORMAT(36X,'Derivatives of constrained shape functions with respect to XI')

 1305 FORMAT(36X,'Derivatives of constrained shape functions with respect to ET')

! **********************************************************************************************************************************
 
      END SUBROUTINE MIN4SH
