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
  
      SUBROUTINE SHP3DP ( IGAUS, JGAUS, KGAUS, NUM_NODES, CALLING_SUBR, IORD_MSG, INT_ORD_IJ, INT_ORD_K, SSI, SSJ, SSK,       &
                          WRT_BUG_THIS_TIME, PSH, DPSHG )

! Generates shape functions for 3D PENTA (6 node and 15 node) elements.
  
! The node numbering and axes convention are shown below with XI, ET ranging from 0 to 1 and ZI ranging from -1 to 1


!                                       ZI
!                                        |
!                                        |
!                                        |
!                                       4|
!                                      . | .           
!                                    .   |    .         
!                                  .     |       .       
!                                .       |          .     
!                              .         |             .   
!                           13           |              15. 
!                          .             |                   .
!                        .               |                      .
!                      .                 |                         .
!                    .                 10|                            . 
!                5. . . . . . . . . . . .| . 14 . . . . . . . . . . . . .6     
!                 |                 .    |    .                         |   
!                 |               .      |       .                      |    
!                 |             .        |          .                   |           
!                 |           .          |             .                |
!                 |         .            |                .             |
!                 |       .             1|                   .          |
!                 |     .              .   .                    .       |
!                 |   .              .        .                    .    |
!                 | .              .             .                    . |
!               11|              .                  .                   |12
!              .  |            7                       9                |   .
!            .    |          .                            .             |      .
!          .      |        .                                 .          |         .
!        .        |      .                                      .       |          ET
!      .          |    .                                           .    |
!     XI          |  .                                                . |
!                2| . . . . . . . . . . . . . 8 . . . . . . . . . . . . | 3          
                                                                            
                                                                               
                                                                                    
           
           


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_BUG, WRT_ERR, WRT_LOG, BUG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_SHPJ_BIT, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  SHP_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO, HALF
      USE MODEL_STUF, ONLY            :  EID, TYPE
 
      USE SHP3DP_USE_IFs

      IMPLICIT NONE
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SHP3DP'
      CHARACTER(LEN=*)  , INTENT(IN)  :: CALLING_SUBR      ! Subr that called this subr (used for debug output)
      CHARACTER(LEN=*)  , INTENT(IN)  :: IORD_MSG          ! Character name of the integration order (used for debug output)
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRT_BUG_THIS_TIME ! If 'Y' then write to BUG file if WRT_BUG array says to
      CHARACTER(17*BYTE)              :: NAME(5)           ! Used for DEBUG output annotation

      INTEGER(LONG), INTENT(IN)       :: IGAUS             ! I index of Gauss point (needed for some optional output)
      INTEGER(LONG), INTENT(IN)       :: JGAUS             ! J index of Gauss point (needed for some optional output)
      INTEGER(LONG), INTENT(IN)       :: KGAUS             ! K index of Gauss point (needed for some optional output)
      INTEGER(LONG), INTENT(IN)       :: INT_ORD_IJ        ! Integration order in triangle (used for debug output)
      INTEGER(LONG), INTENT(IN)       :: INT_ORD_K         ! Integration order along z (used for debug output)
      INTEGER(LONG), INTENT(IN)       :: NUM_NODES         ! Number of element nodes
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: NODES_6     = 6   ! Number of nodes for one type of element
      INTEGER(LONG)                   :: NODES_15    = 15  ! Number of nodes for one type of element
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SHP_BEGEND
  
      REAL(DOUBLE) , INTENT(IN)       :: SSI               ! Gauss point location component 1
      REAL(DOUBLE) , INTENT(IN)       :: SSJ               ! Gauss point location component 2
      REAL(DOUBLE) , INTENT(IN)       :: SSK               ! Gauss point location component 3
      REAL(DOUBLE) , INTENT(OUT)      :: PSH(NUM_NODES)    ! Shape functions for all grid points for this Gauss point
      REAL(DOUBLE) , INTENT(OUT)      :: DPSHG(3,NUM_NODES)! Derivatives of PSH with respect to xi, eta, zi.
      REAL(DOUBLE)                    :: PHI               ! Intermediate variable in calculating DPSHG
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC, WRT_BUG_THIS_TIME, WRT_BUG(7), WRT_BUG(8), WRT_BUG(9)
 9001    FORMAT(1X,A,' BEGN ',F10.3, 3X, A1, 3(I3))
      ENDIF
! **********************************************************************************************************************************
! Initialize outputs

      DO I=1,NUM_NODES
         PSH(I) = ZERO
      ENDDO

      DO I=1,3
         DO J=1,NUM_NODES
            DPSHG(I,J) = ZERO
         ENDDO
      ENDDO

! Generate shape functions and derivatives for 3D element with 4 nodes
  
      IF (NUM_NODES == NODES_6) THEN

         PHI = ONE - SSI - SSJ
 
         PSH(1) = HALF*PHI*(ONE - SSK)          ;          PSH(4) = HALF*PHI*(ONE + SSK)
         PSH(2) = HALF*SSI*(ONE - SSK)          ;          PSH(5) = HALF*SSI*(ONE + SSK)
         PSH(3) = HALF*SSJ*(ONE - SSK)          ;          PSH(6) = HALF*SSJ*(ONE + SSK)

         DPSHG(1,1) = -HALF*(ONE - SSK)         ;          DPSHG(1,4) = -HALF*(ONE + SSK)
         DPSHG(2,1) = -HALF*(ONE - SSK)         ;          DPSHG(2,4) = -HALF*(ONE + SSK)
         DPSHG(3,1) = -HALF*PHI                 ;          DPSHG(3,4) =  HALF*PHI

         DPSHG(1,2) =  HALF*(ONE - SSK)         ;          DPSHG(1,5) =  HALF*(ONE + SSK)
         DPSHG(2,2) =  ZERO                     ;          DPSHG(2,5) =  ZERO
         DPSHG(3,2) = -HALF*SSI                 ;          DPSHG(3,5) =  HALF*SSI

         DPSHG(1,3) =  ZERO                     ;          DPSHG(1,6) =  ZERO
         DPSHG(2,3) =  HALF*(ONE - SSK)         ;          DPSHG(2,6) =  HALF*(ONE + SSK)
         DPSHG(3,3) = -HALF*SSJ                 ;          DPSHG(3,6) =  HALF*SSJ

! **********************************************************************************************************************************
! Generate shape functions and derivatives for 3D element with 20 nodes
  
      ELSE IF (NUM_NODES == NODES_15) THEN
  
         PHI = ONE - SSI - SSJ

         PSH( 1) = PHI*(ONE - SSK)*(PHI - ONE - HALF*SSK)          ;     PSH( 4) = PHI*(ONE + SSK)*(PHI - ONE + HALF*SSK)
         PSH( 2) = SSI*(ONE - SSK)*(SSI - ONE - HALF*SSK)          ;     PSH( 5) = SSI*(ONE + SSK)*(SSI - ONE + HALF*SSK)
         PSH( 3) = SSJ*(ONE - SSK)*(SSJ - ONE - HALF*SSK)          ;     PSH( 6) = SSJ*(ONE + SSK)*(SSJ - ONE + HALF*SSK)
         PSH( 7) = TWO*SSI*PHI*(ONE - SSK)                         ;     PSH(13) = TWO*SSI*PHI*(ONE + SSK)
         PSH( 8) = TWO*SSI*SSJ*(ONE - SSK)                         ;     PSH(14) = TWO*SSI*SSJ*(ONE + SSK)
         PSH( 9) = TWO*SSJ*PHI*(ONE - SSK)                         ;     PSH(15) = TWO*SSJ*PHI*(ONE + SSK)
         PSH(10) = PHI*(ONE - SSK*SSK)
         PSH(11) = SSI*(ONE - SSK*SSK)
         PSH(12) = SSJ*(ONE - SSK*SSK)


         DPSHG(1, 1) =  (ONE - SSK)*(ONE + HALF*SSK - TWO*PHI)     ;     DPSHG(1, 4) =  (ONE + SSK)*(ONE - HALF*SSK - TWO*PHI)
         DPSHG(2, 1) =  (ONE - SSK)*(ONE + HALF*SSK - TWO*PHI)     ;     DPSHG(2, 4) =  (ONE + SSK)*(ONE - HALF*SSK - TWO*PHI)
         DPSHG(3, 1) =  PHI*(SSK - PHI + HALF)                     ;     DPSHG(3, 4) =  PHI*(SSK + PHI - HALF)

         DPSHG(1, 2) =  (ONE - SSK)*(TWO*SSI - ONE - HALF*SSK)     ;     DPSHG(1, 5) =  (ONE + SSK)*(TWO*SSI - ONE + HALF*SSK)
         DPSHG(2, 2) =  ZERO                                       ;     DPSHG(2, 5) =  ZERO
         DPSHG(3, 2) =  SSI*(SSK - SSI + HALF)                     ;     DPSHG(3, 5) =  SSI*(SSK + SSI - HALF)

         DPSHG(1, 3) =  ZERO                                       ;     DPSHG(1, 6) =  ZERO
         DPSHG(2, 3) =  (ONE - SSK)*(TWO*SSJ - ONE - HALF*SSK)     ;     DPSHG(2, 6) =  (ONE + SSK)*(TWO*SSJ - ONE + HALF*SSK)
         DPSHG(3, 3) =  SSJ*(SSK - SSJ + HALF)                     ;     DPSHG(3, 6) =  SSJ*(SSK + SSJ - HALF)

         DPSHG(1, 7) =  TWO*(ONE - SSK)*(PHI - SSI)                ;     DPSHG(1,13) =  TWO*(ONE + SSK)*(PHI - SSI)
         DPSHG(2, 7) = -TWO*SSI*(ONE - SSK)                        ;     DPSHG(2,13) = -TWO*SSI*(ONE + SSK)
         DPSHG(3, 7) = -TWO*SSI*PHI                                ;     DPSHG(3,13) =  TWO*SSI*PHI

         DPSHG(1, 8) =  TWO*SSJ*(ONE - SSK)                        ;     DPSHG(1,14) =  TWO*SSJ*(ONE + SSK)
         DPSHG(2, 8) =  TWO*SSI*(ONE - SSK)                        ;     DPSHG(2,14) =  TWO*SSI*(ONE + SSK)
         DPSHG(3, 8) = -TWO*SSI*SSJ                                ;     DPSHG(3,14) =  TWO*SSI*SSJ

         DPSHG(1, 9) = -TWO*SSJ*(ONE - SSK)                        ;     DPSHG(1,15) = -TWO*SSJ*(ONE + SSK)
         DPSHG(2, 9) =  TWO*(ONE - SSK)*(PHI - SSJ)                ;     DPSHG(2,15) =  TWO*(ONE + SSK)*(PHI - SSJ)
         DPSHG(3, 9) = -TWO*SSJ*PHI                                ;     DPSHG(3,15) =  TWO*SSJ*PHI

         DPSHG(1,10) =  SSK*SSK - ONE
         DPSHG(2,10) =  SSK*SSK - ONE
         DPSHG(3,10) = -TWO*SSK*PHI

         DPSHG(1,11) =  ONE - SSK*SSK
         DPSHG(2,11) =  ZERO
         DPSHG(3,11) = -TWO*SSI*SSK

         DPSHG(1,12) =  ZERO
         DPSHG(2,12) =  ONE - SSK*SSK
         DPSHG(3,12) = -TWO*SSJ*SSK

! **********************************************************************************************************************************
! Error: NUM_NODES is not 6 or 15
  
      ELSE
  
         WRITE(ERR,1932) SUBR_NAME, NUM_NODES, EID, TYPE, NODES_6, NODES_15
         WRITE(F06,1932) SUBR_NAME, NUM_NODES, EID, TYPE, NODES_6, NODES_15
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit
  
      ENDIF
  
! **********************************************************************************************************************************
! ELDATA output:
 
      IF ((WRT_BUG_THIS_TIME == 'Y') .AND. (WRT_BUG(7) > 0)) THEN

         NAME(1) = 'Nodes  1 thru  3:'
         NAME(2) = 'Nodes  4 thru  6:'
         NAME(3) = 'Nodes  7 thru  9:'
         NAME(4) = 'Nodes 10 thru 12:'
         NAME(5) = 'Nodes 13 thru 15:'

         WRITE(BUG,1101) ELDT_BUG_SHPJ_BIT, TYPE, EID, CALLING_SUBR

         WRITE(BUG,1102) IGAUS, JGAUS, KGAUS, IORD_MSG, INT_ORD_IJ, INT_ORD_K
         WRITE(BUG,1103) SSI,SSJ,SSK
         WRITE(BUG,1112)

         J = 0
         WRITE(BUG,1105)
         DO I=1,NUM_NODES,3
            J = J + 1
            WRITE(BUG,1201) NAME(J), PSH(I), PSH(I+1), PSH(I+2)
         ENDDO
         WRITE(BUG,*)
         WRITE(BUG,*)

         J = 0
         WRITE(BUG,1115)
         DO I=1,NUM_NODES,3
            J = J + 1
            WRITE(BUG,1201) NAME(J), DPSHG(1,I), DPSHG(1,I+1), DPSHG(1,I+2)
         ENDDO
         WRITE(BUG,*)
         WRITE(BUG,*)
 
         J = 0
         WRITE(BUG,1125)
         DO I=1,NUM_NODES,3
            J = J + 1
            WRITE(BUG,1201) NAME(J), DPSHG(2,I), DPSHG(2,I+1), DPSHG(2,I+2)
         ENDDO
         WRITE(BUG,*)
         WRITE(BUG,*)
 
         J = 0
         WRITE(BUG,1135)
         DO I=1,NUM_NODES,3
            J = J + 1
            WRITE(BUG,1201) NAME(J), DPSHG(3,I), DPSHG(3,I+1), DPSHG(3,I+2)
         ENDDO
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
 1932 FORMAT(' *ERROR  1932: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' SUBROUTINE CALLED WITH ILLEGAL NUMBER OF NODES =',I8,' FOR ELEMENT',I8,' TYPE ',A,'. MUST BE ',I3,    &
                           ' OR ',I3)

 1101 FORMAT(' ------------------------------------------------------------------------------------------------------------------',&
             '-----------------',/,                                                                                                &
             ' ELDATA(',I2,',PRINT) requests for ',A,' element number ',I8,', Calling subroutine ',A,/,                            &
             ' ========================================================================================',/)

 1102 FORMAT(47X,' Gauss point: I = ',I3,', J = ',I3,', K = ',I3,/,31X,'with integration order ',A,I2,'x',I2,/,                    &
             35X,'and using Gaussian integration to determine strain-displ matrix'//)

 1112 format(51x,'Outputs from subroutine SHP3DP',/,51x,'------------------------------',/)

 1103 FORMAT(53X,'Coordinates of Gauss point',/,49x,'XI              ET              ZI',/,40X,3(1ES16.6),//)

 1105 FORMAT(58X,'Shape functions')

 1115 FORMAT(42X,'Derivatives of shape functions with respect to XI')

 1125 FORMAT(42X,'Derivatives of shape functions with respect to ET')

 1135 FORMAT(42X,'Derivatives of shape functions with respect to ZI')

 1201 FORMAT(30X,A,1X,3(1ES16.6))

! **********************************************************************************************************************************
 
      END SUBROUTINE SHP3DP
