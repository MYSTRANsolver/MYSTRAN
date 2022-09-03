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
  
      SUBROUTINE BBDKQ ( DPSHX, XSD, YSD, SLN, IGAUS, JGAUS, MESSAG, WRT_BUG_THIS_TIME, BB )
 
! Calculate BB strain/displacement matrix for DKQ bending quadrilateral element. Called by subr QPLT1
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  BUG, F04, WRT_BUG, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_BMAT_BIT, ELDT_BUG_BCHK_BIT
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BBDKQ_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, TWO, THREE, FOUR
      USE MODEL_STUF, ONLY            :  EID, TYPE, XEB, XEL
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
  
      USE BBDKQ_USE_IFs

      IMPLICIT NONE
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BBDKQ'
      CHARACTER(LEN=*), INTENT(IN)    :: MESSAG            ! Messag to print out if BCHECK is run
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRT_BUG_THIS_TIME ! If 'Y' then write to BUG file if WRT_BUG array says to

      INTEGER(LONG), INTENT(IN)       :: IGAUS             ! I index of Gaus point (needed for some optional output)
      INTEGER(LONG), INTENT(IN)       :: JGAUS             ! J index of Gaus point (needed for some optional output)
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: ID(12)            ! An input to subr BCHECK, called herein
      INTEGER(LONG), PARAMETER        :: NR        = 3     ! An input to subr BCHECK, called herein
      INTEGER(LONG), PARAMETER        :: NC        = 12    ! An input to subr BCHECK, called herein
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BBDKQ_BEGEND
  
      REAL(DOUBLE) , INTENT(IN)       :: SLN(4)            ! Quad side lengths
      REAL(DOUBLE) , INTENT(IN)       :: XSD(4)            ! Array of 4 diffs of X dim. of sides
      REAL(DOUBLE) , INTENT(IN)       :: YSD(4)            ! Array of 4 diffs of Y dim. of sides
      REAL(DOUBLE) , INTENT(IN)       :: DPSHX(2,8)        ! Derivatives of the 8 node biquadratic isopar interps wrt elem x and y
      REAL(DOUBLE) , INTENT(OUT)      :: BB(3,12)          ! Output strain-displ matrix for the DKQ elem
      REAL(DOUBLE)                    :: BW(3,14)          ! Output from subr BCHECK (matrix of 3 elem strains for 14 various elem
                                                             ! rigid body motions/constant strain distortions)
      REAL(DOUBLE) , PARAMETER        :: C15 = THREE/TWO   ! Constant = 1.5
      REAL(DOUBLE)                    :: A(4)              ! Intermediate variables used in calculating outputs
      REAL(DOUBLE)                    :: B(4)              ! Intermediate variables used in calculating outputs
      REAL(DOUBLE)                    :: C(4)              ! Intermediate variables used in calculating outputs
      REAL(DOUBLE)                    :: D(4)              ! Intermediate variables used in calculating outputs
      REAL(DOUBLE)                    :: E(4)              ! Intermediate variables used in calculating outputs
      REAL(DOUBLE)                    :: DHXSHX(2,12)      ! Derivatives of Hx with respect to x and y (Hx is a fcn of DPSHX) 
      REAL(DOUBLE)                    :: DHYSHX(2,12)      ! Derivatives of Hy with respect to x and y (Hy is a fcn of DPSHX) 
      REAL(DOUBLE)                    :: SL2               ! The squares of elem side lengths
      REAL(DOUBLE)                    :: XB(4,3)           ! First 4 rows of XEB
      REAL(DOUBLE)                    :: XL(4,3)           ! First 4 rows of XEL
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC, WRT_BUG_THIS_TIME, WRT_BUG(7), WRT_BUG(8), WRT_BUG(9)
 9001    FORMAT(1X,A,' BEGN ',F10.3, 3X, A1, 3(I3))
      ENDIF
! **********************************************************************************************************************************
! Initialize outputs

      DO I=1,3
         DO J=1,12
            BB(I,J) = ZERO
         ENDDO
      ENDDO

! Calculate parameters needed
  
      DO I=1,4
         SL2 = SLN(I)*SLN(I)
         A(I) = -XSD(I)/SL2
         B(I) = THREE*XSD(I)*YSD(I)/(FOUR*SL2)
         C(I) = (XSD(I)*XSD(I)/FOUR - YSD(I)*YSD(I)/TWO)/SL2
         D(I) = -YSD(I)/SL2
         E(I) = (-XSD(I)*XSD(I)/TWO + YSD(I)*YSD(I)/FOUR)/SL2
      ENDDO   
  
! Derivatives of Hx with respect to x
 
      DHXSHX(1, 1) = C15*(A(1)*DPSHX(1,5) - A(4)*DPSHX(1,8))
      DHXSHX(1, 4) = C15*(A(2)*DPSHX(1,6) - A(1)*DPSHX(1,5))
      DHXSHX(1, 7) = C15*(A(3)*DPSHX(1,7) - A(2)*DPSHX(1,6))
      DHXSHX(1,10) = C15*(A(4)*DPSHX(1,8) - A(3)*DPSHX(1,7))
          
      DHXSHX(1, 2) = B(1)*DPSHX(1,5) + B(4)*DPSHX(1,8)
      DHXSHX(1, 5) = B(2)*DPSHX(1,6) + B(1)*DPSHX(1,5)
      DHXSHX(1, 8) = B(3)*DPSHX(1,7) + B(2)*DPSHX(1,6)
      DHXSHX(1,11) = B(4)*DPSHX(1,8) + B(3)*DPSHX(1,7)
  
      DHXSHX(1, 3) = DPSHX(1,1) - C(1)*DPSHX(1,5) - C(4)*DPSHX(1,8)
      DHXSHX(1, 6) = DPSHX(1,2) - C(2)*DPSHX(1,6) - C(1)*DPSHX(1,5)
      DHXSHX(1, 9) = DPSHX(1,3) - C(3)*DPSHX(1,7) - C(2)*DPSHX(1,6)
      DHXSHX(1,12) = DPSHX(1,4) - C(4)*DPSHX(1,8) - C(3)*DPSHX(1,7)
  
! Derivatives of Hx with respect to y
  
      DHXSHX(2, 1) = C15*(A(1)*DPSHX(2,5) - A(4)*DPSHX(2,8))
      DHXSHX(2, 4) = C15*(A(2)*DPSHX(2,6) - A(1)*DPSHX(2,5))
      DHXSHX(2, 7) = C15*(A(3)*DPSHX(2,7) - A(2)*DPSHX(2,6))
      DHXSHX(2,10) = C15*(A(4)*DPSHX(2,8) - A(3)*DPSHX(2,7))
          
      DHXSHX(2, 2) = B(1)*DPSHX(2,5) + B(4)*DPSHX(2,8)
      DHXSHX(2, 5) = B(2)*DPSHX(2,6) + B(1)*DPSHX(2,5)
      DHXSHX(2, 8) = B(3)*DPSHX(2,7) + B(2)*DPSHX(2,6)
      DHXSHX(2,11) = B(4)*DPSHX(2,8) + B(3)*DPSHX(2,7)
  
      DHXSHX(2, 3) = DPSHX(2,1) - C(1)*DPSHX(2,5) - C(4)*DPSHX(2,8)
      DHXSHX(2, 6) = DPSHX(2,2) - C(2)*DPSHX(2,6) - C(1)*DPSHX(2,5)
      DHXSHX(2, 9) = DPSHX(2,3) - C(3)*DPSHX(2,7) - C(2)*DPSHX(2,6)
      DHXSHX(2,12) = DPSHX(2,4) - C(4)*DPSHX(2,8) - C(3)*DPSHX(2,7)
  
! Derivatives of Hy with respect to x
  
      DHYSHX(1, 1) = C15*(D(1)*DPSHX(1,5) - D(4)*DPSHX(1,8))
      DHYSHX(1, 4) = C15*(D(2)*DPSHX(1,6) - D(1)*DPSHX(1,5))
      DHYSHX(1, 7) = C15*(D(3)*DPSHX(1,7) - D(2)*DPSHX(1,6))
      DHYSHX(1,10) = C15*(D(4)*DPSHX(1,8) - D(3)*DPSHX(1,7))
          
      DHYSHX(1, 2) = -DPSHX(1,1) + E(1)*DPSHX(1,5) + E(4)*DPSHX(1,8)
      DHYSHX(1, 5) = -DPSHX(1,2) + E(2)*DPSHX(1,6) + E(1)*DPSHX(1,5)
      DHYSHX(1, 8) = -DPSHX(1,3) + E(3)*DPSHX(1,7) + E(2)*DPSHX(1,6)
      DHYSHX(1,11) = -DPSHX(1,4) + E(4)*DPSHX(1,8) + E(3)*DPSHX(1,7)
  
      DHYSHX(1, 3) = -B(1)*DPSHX(1,5) - B(4)*DPSHX(1,8)
      DHYSHX(1, 6) = -B(2)*DPSHX(1,6) - B(1)*DPSHX(1,5)
      DHYSHX(1, 9) = -B(3)*DPSHX(1,7) - B(2)*DPSHX(1,6)
      DHYSHX(1,12) = -B(4)*DPSHX(1,8) - B(3)*DPSHX(1,7)
  
! Derivatives of Hy with respect to y
  
      DHYSHX(2, 1) = C15*(D(1)*DPSHX(2,5) - D(4)*DPSHX(2,8))
      DHYSHX(2, 4) = C15*(D(2)*DPSHX(2,6) - D(1)*DPSHX(2,5))
      DHYSHX(2, 7) = C15*(D(3)*DPSHX(2,7) - D(2)*DPSHX(2,6))
      DHYSHX(2,10) = C15*(D(4)*DPSHX(2,8) - D(3)*DPSHX(2,7))
          
      DHYSHX(2, 2) = -DPSHX(2,1) + E(1)*DPSHX(2,5) + E(4)*DPSHX(2,8)
      DHYSHX(2, 5) = -DPSHX(2,2) + E(2)*DPSHX(2,6) + E(1)*DPSHX(2,5)
      DHYSHX(2, 8) = -DPSHX(2,3) + E(3)*DPSHX(2,7) + E(2)*DPSHX(2,6)
      DHYSHX(2,11) = -DPSHX(2,4) + E(4)*DPSHX(2,8) + E(3)*DPSHX(2,7)
  
      DHYSHX(2, 3) = -B(1)*DPSHX(2,5) - B(4)*DPSHX(2,8)
      DHYSHX(2, 6) = -B(2)*DPSHX(2,6) - B(1)*DPSHX(2,5)
      DHYSHX(2, 9) = -B(3)*DPSHX(2,7) - B(2)*DPSHX(2,6)
      DHYSHX(2,12) = -B(4)*DPSHX(2,8) - B(3)*DPSHX(2,7)
  
! Now formulate the BB strain/displacement matrix
  
      DO J=1,12
         BB(1,J) = DHXSHX(1,J)
         BB(2,J) = DHYSHX(2,J)
         BB(3,J) = DHXSHX(2,J) + DHYSHX(1,J)
      ENDDO 
  
      IF ((WRT_BUG_THIS_TIME == 'Y') .AND. (WRT_BUG(8) > 0)) THEN

         WRITE(BUG,1101) ELDT_BUG_BMAT_BIT, TYPE, EID
         WRITE(BUG,8901) IGAUS, JGAUS, SUBR_NAME
         DO I=1,3
            WRITE(BUG,8902) I,(BB(I,J),J=1,12)
            WRITE(BUG,*)
         ENDDO 
         WRITE(BUG,*)

      ENDIF

      IF ((WRT_BUG_THIS_TIME == 'Y') .AND. (WRT_BUG(9) > 0)) THEN
        IF (DEBUG(202) > 0) THEN
         ID( 1) =  3
         ID( 2) =  4
         ID( 3) =  5
         ID( 4) =  9
         ID( 5) = 10
         ID( 6) = 11
         ID( 7) = 15
         ID( 8) = 16
         ID( 9) = 17
         ID(10) = 21
         ID(11) = 22
         ID(12) = 23
  
         DO I=1,4
            DO J=1,3
               XB(I,J) = XEB(I,J)
               XL(I,J) = XEL(I,J)
            ENDDO
         ENDDO

         WRITE(BUG,1101) ELDT_BUG_BCHK_BIT, TYPE, EID
         WRITE(BUG,9100)
         WRITE(BUG,9101) MESSAG, IGAUS, JGAUS
         WRITE(BUG,9102)
         WRITE(BUG,9103)
         WRITE(BUG,9104)
         CALL BCHECK_2D ( BB, 'B', ID, NR, NC, 4, XB, XL, BW )
        ENDIF
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1101 FORMAT(' ------------------------------------------------------------------------------------------------------------------',&
             '-----------------',/,                                                                                                &
             ' ELDATA(',I2,',PRINT) requests for ',A,' element number ',I8, ' (from subr ', A, ')'/,                               &
             ' ==============================================================',/)

 8901 FORMAT(' Strain-displacement matrix BB for Gauss point: I = ',I3,', J = ',I3,' for bending portion of element in subr '      &
             ,A,/)

 8902 FORMAT(' Row ',I2,/,6(1ES14.6))

 9100 FORMAT('                          Check on strain-displacement matrix BB for bending portion of the element in subr BCHECK'/)

 9101 FORMAT('                                                               S T R A I N S'/,                                      &
             '                                           (',A,' for Gauss point: I = ',I3,', J = ',I3,')')

 9102 FORMAT('                                                     Cxx            Cyy            Cxy')

 9103 FORMAT(1X,'      Element displacements consistent with:')

 9104 FORMAT(1X,'      ---------------------------------------')

! **********************************************************************************************************************************

      END SUBROUTINE BBDKQ
