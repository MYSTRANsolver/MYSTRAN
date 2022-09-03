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
  
      SUBROUTINE B3D_ISOPARAMETRIC ( DPSHX, GAUSS_PT, IGAUS, JGAUS, KGAUS, MESSAG, WRT_BUG_THIS_TIME, BMAT )
 
! Generates strain/displ matrix BMAT for solid 3D elements. Called by HEXA, PENTA and TETRA subroutines
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_BUG, WRT_ERR, WRT_LOG, BUG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_BMAT_BIT, ELDT_BUG_BCHK_BIT
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  B3D_ISOPARAMETRIC_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  EID, ELGP, TYPE
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      USE B3D_ISOPARAMETRIC_USE_IFs

      IMPLICIT NONE
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'B3D_ISOPARAMETRIC'
      CHARACTER(LEN=*)  , INTENT(IN)  :: MESSAG            ! Messag to print out if BCHECK is run
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRT_BUG_THIS_TIME ! If 'Y' then write to BUG file if WRT_BUG array says to

      INTEGER(LONG), INTENT(IN)       :: GAUSS_PT          ! Gauss point number (used for DEBUG output)
      INTEGER(LONG), INTENT(IN)       :: IGAUS             ! I index of Gaus point (needed for some optional output)
      INTEGER(LONG), INTENT(IN)       :: JGAUS             ! J index of Gaus point (needed for some optional output)
      INTEGER(LONG), INTENT(IN)       :: KGAUS             ! K index of Gaus point (needed for some optional output)
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: II,JJ             ! Counters
      INTEGER(LONG)                   :: ID(3*ELGP)        ! An input to subr BCHECK, called herein
      INTEGER(LONG), PARAMETER        :: NR      = 6       ! An input to subr BCHECK, called herein
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = B3D_ISOPARAMETRIC_BEGEND
  
      REAL(DOUBLE) , INTENT(IN)       :: DPSHX(3,ELGP)     ! Derivatives of the 4 node bilinear isopar interps wrt elem x and y
      REAL(DOUBLE) , INTENT(OUT)      :: BMAT(6,3*ELGP)    ! Output strain-displ matrix for this elem
      REAL(DOUBLE)                    :: BW(6,12)          ! Output from subr BCHECK (matrix of 3 elem strains for 14 various elem
                                                           ! rigid body motions/constant strain distortions)
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC, WRT_BUG_THIS_TIME, WRT_BUG(7), WRT_BUG(8), WRT_BUG(9)
 9001    FORMAT(1X,A,' BEGN ',F10.3, 3X, A1, 3(I3))
      ENDIF
! **********************************************************************************************************************************
! Initialize outputs

      DO I=1,6
         DO J=1,3*ELGP
            BMAT(I,J) = ZERO
         ENDDO
      ENDDO

! Calc outputs

      JJ = 0
      DO J=1,ELGP
  
         JJ = JJ + 1
         BMAT(1,JJ) = DPSHX(1,J)
         BMAT(2,JJ) = ZERO
         BMAT(3,JJ) = ZERO
         BMAT(4,JJ) = DPSHX(2,J)
         BMAT(5,JJ) = ZERO
         BMAT(6,JJ) = DPSHX(3,J)

         JJ = JJ + 1
         BMAT(1,JJ) = ZERO
         BMAT(2,JJ) = DPSHX(2,J)
         BMAT(3,JJ) = ZERO
         BMAT(4,JJ) = DPSHX(1,J)
         BMAT(5,JJ) = DPSHX(3,J)
         BMAT(6,JJ) = ZERO

         JJ = JJ + 1
         BMAT(1,JJ) = ZERO
         BMAT(2,JJ) = ZERO
         BMAT(3,JJ) = DPSHX(3,J)
         BMAT(4,JJ) = ZERO
         BMAT(5,JJ) = DPSHX(2,J)
         BMAT(6,JJ) = DPSHX(1,J)

      ENDDO 

      IF ((WRT_BUG_THIS_TIME == 'Y') .AND. (WRT_BUG(8) > 0)) THEN

         WRITE(BUG,1101) ELDT_BUG_BMAT_BIT, TYPE, EID
         WRITE(BUG,8901) IGAUS, JGAUS, KGAUS, SUBR_NAME
         DO I=1,6
            WRITE(BUG,8902) I,(BMAT(I,J),J=1,3*ELGP)
            WRITE(BUG,*)
         ENDDO 
         WRITE(BUG,*)

      ENDIF

      IF ((WRT_BUG_THIS_TIME == 'Y') .AND. (WRT_BUG(9) > 0)) THEN
        IF (DEBUG(202) > 0) THEN
         JJ = 0                                            ! Calculate ID array
         DO I=1,ELGP
            II = 6*(I - 1)
            DO J=1,3
               II = II + 1
               JJ = JJ + 1
               ID(JJ) = II                                 ! Want JJ to range 1 to 3*ELGP and II to range 1 to 45 in increments of 3
            ENDDO
         ENDDO

         WRITE(BUG,1101) ELDT_BUG_BCHK_BIT, TYPE, EID
         WRITE(BUG,9100)
         WRITE(BUG,9101) MESSAG, IGAUS, JGAUS, KGAUS
         WRITE(BUG,9102)
         WRITE(BUG,9103)
         WRITE(BUG,9104)
         CALL BCHECK_3D ( BMAT, ELGP, ID, NR, 3*ELGP, BW )
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

 8901 FORMAT(' Strain-displacement matrix B for Gauss point: I = ',I3,', J = ',I3,', K = ',I3,' for element in subr ',A,/)

 8902 FORMAT(' Row ',I2,/,6(1ES14.6))

 9100 FORMAT(47X,'   Check on strain-displacement matrix B for the element in subr BCHECK,',/)

 9101 FORMAT(47X,'                               S T R A I N S'/,                       &
             47X,'        (',A,' for Gauss point: I = ',I3,', J = ',I3,', K = ',I3,')')

 9102 FORMAT(47X,' Exx           Eyy           Ezz           Gxy           Gyz           Gzx      Note:')

 9103 FORMAT(1X,'      Element displacements consistent with:')

 9104 FORMAT(1X,'      ---------------------------------------')

! **********************************************************************************************************************************

      END SUBROUTINE B3D_ISOPARAMETRIC
