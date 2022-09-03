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
  
      SUBROUTINE SHP3DH ( IGAUS, JGAUS, KGAUS, NUM_NODES, CALLING_SUBR, IORD_MSG, IORZZZ, SSI, SSJ, SSK, WRT_BUG_THIS_TIME,   &
                          PSH, DPSHG )

! Generates shape functions for 3D HEXA (8 node and 20 node) elements.
  
! The node numbering and axes convention are shown below with XI, ETA, ZI ranging from -1 to +1
 
!                   In plane ZI = -1

!                         ETA
!                          |
!                          |
!                          |
!              4 . . . . .11 . . . . . 3
!              .           |           .
!              .           |           .
!              .           |           .
!             12           -----------10----> XI     NOTE: for HEXA8, only nodes 1-4 are used in plane ZI = -1
!              .                       .             -----
!              .                       .
!              .                       .
!              1 . . . . . 9 . . . . . 2
  


!                   In plane ZI = 0

!                         ETA
!                          |
!                          |
!                          |
!             16 . . . . . |.. . . . . 15
!              .           |           .
!              .           |           .
!              .           |           .
!              .           -----------------> XI     NOTE: for HEXA8 there are no nodes in plane ZI = 0
!              .                       .             -----
!              .                       .
!              .                       .
!             13 . . . . . . . . . . . 14
  


!                   In plane ZI = 1

!                         ETA
!                          |
!                          |
!                          |
!              8. . . . . 19. . . . .  7
!              .           |           .
!              .           |           .
!              .           |           .
!             20           -----------18----> XI     NOTE: for HEXA8, only nodes 5-8 are used in plane ZI = 1
!              .                       .             -----
!              .                       .
!              .                       .
!              5 . . . . .17 . . . . . 6 
  


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_BUG, WRT_ERR, WRT_LOG, BUG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_SHPJ_BIT, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  SHP_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO, FOUR, EIGHT
      USE MODEL_STUF, ONLY            :  EID, TYPE
 
      USE SHP3DH_USE_IFs

      IMPLICIT NONE
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SHP3DH'
      CHARACTER(LEN=*)  , INTENT(IN)  :: CALLING_SUBR      ! Subr that called this subr (used for debug output)
      CHARACTER(LEN=*)  , INTENT(IN)  :: IORD_MSG          ! Character name of the integration order (used for debug output)
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRT_BUG_THIS_TIME ! If 'Y' then write to BUG file if WRT_BUG array says to
      CHARACTER(17*BYTE)              :: NAME(5)           ! Used for DEBUG output annotation

      INTEGER(LONG), INTENT(IN)       :: IGAUS             ! I index of Gauss point (needed for some optional output)
      INTEGER(LONG), INTENT(IN)       :: JGAUS             ! J index of Gauss point (needed for some optional output)
      INTEGER(LONG), INTENT(IN)       :: KGAUS             ! K index of Gauss point (needed for some optional output)
      INTEGER(LONG), INTENT(IN)       :: IORZZZ            ! Integration order (used for debug output)
      INTEGER(LONG), INTENT(IN)       :: NUM_NODES         ! Number of element nodes
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: NODE(4)           ! Node numbers for sets of 4 corner nodes for 20 node HEX element
      INTEGER(LONG)                   :: NODES_8     = 8   ! Number of nodes for one type of element
      INTEGER(LONG)                   :: NODES_20    = 20  ! Number of nodes for one type of element
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SHP_BEGEND
  
      REAL(DOUBLE) , INTENT(IN)       :: SSI               ! Gauss point location component 1
      REAL(DOUBLE) , INTENT(IN)       :: SSJ               ! Gauss point location component 2
      REAL(DOUBLE) , INTENT(IN)       :: SSK               ! Gauss point location component 3
      REAL(DOUBLE) , INTENT(OUT)      :: PSH(NUM_NODES)    ! Shape functions for all grid points for this Gauss point
      REAL(DOUBLE) , INTENT(OUT)      :: DPSHG(3,NUM_NODES)! Derivatives of PSH with respect to xi, eta, zi.
      REAL(DOUBLE)                    :: XI(NUM_NODES)     ! Elem node location in isoparametric coord direction 1
      REAL(DOUBLE)                    :: ET(NUM_NODES)     ! Elem node location in isoparametric coord direction 2
      REAL(DOUBLE)                    :: ZI(NUM_NODES)     ! Elem node location in isoparametric coord direction 3
 
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

! Generate shape functions and derivatives for 3D element with 8 nodes
  
      IF (NUM_NODES == NODES_8) THEN
 
         XI(1) = -ONE                                      ! 1st coord direction
         XI(2) =  ONE
         XI(3) =  ONE
         XI(4) = -ONE

         XI(5) = -ONE
         XI(6) =  ONE
         XI(7) =  ONE
         XI(8) = -ONE


         ET(1) = -ONE                                      ! 2nd coord direction
         ET(2) = -ONE
         ET(3) =  ONE
         ET(4) =  ONE

         ET(5) = -ONE
         ET(6) = -ONE
         ET(7) =  ONE
         ET(8) =  ONE


         ZI(1) = -ONE                                      ! 3rd coord direction
         ZI(2) = -ONE
         ZI(3) = -ONE
         ZI(4) = -ONE

         ZI(5) =  ONE
         ZI(6) =  ONE
         ZI(7) =  ONE
         ZI(8) =  ONE

         DO I=1,8
            PSH(I) = (ONE + SSI*XI(I))*(ONE + SSJ*ET(I))*(ONE + SSK*ZI(I))/EIGHT
            DPSHG(1,I) = XI(I)*(ONE + SSJ*ET(I))*(ONE + SSK*ZI(I))/EIGHT
            DPSHG(2,I) = ET(I)*(ONE + SSI*XI(I))*(ONE + SSK*ZI(I))/EIGHT
            DPSHG(3,I) = ZI(I)*(ONE + SSI*XI(I))*(ONE + SSJ*ET(I))/EIGHT
         ENDDO 

! **********************************************************************************************************************************
! Generate shape functions and derivatives for 3D element with 20 nodes
  
      ELSE IF (NUM_NODES == NODES_20) THEN
  
         XI( 1) = -ONE                                      ! 1st coord direction
         XI( 2) =  ONE
         XI( 3) =  ONE
         XI( 4) = -ONE

         XI( 5) = -ONE
         XI( 6) =  ONE
         XI( 7) =  ONE
         XI( 8) = -ONE

         XI( 9) =  ZERO
         XI(10) =  ONE
         XI(11) =  ZERO
         XI(12) = -ONE

         XI(13) = -ONE
         XI(14) =  ONE
         XI(15) =  ONE
         XI(16) = -ONE

         XI(17) =  ZERO
         XI(18) =  ONE
         XI(19) =  ZERO
         XI(20) = -ONE


         ET( 1) = -ONE                                      ! 2nd coord direction
         ET( 2) = -ONE
         ET( 3) =  ONE
         ET( 4) =  ONE

         ET( 5) = -ONE
         ET( 6) = -ONE
         ET( 7) =  ONE
         ET( 8) =  ONE

         ET( 9) = -ONE
         ET(10) =  ZERO
         ET(11) =  ONE
         ET(12) =  ZERO

         ET(13) = -ONE
         ET(14) = -ONE
         ET(15) =  ONE
         ET(16) =  ONE

         ET(17) = -ONE
         ET(18) =  ZERO
         ET(19) =  ONE
         ET(20) =  ZERO


         ZI( 1) = -ONE                                      ! 3rd coord direction
         ZI( 2) = -ONE
         ZI( 3) = -ONE
         ZI( 4) = -ONE

         ZI( 5) =  ONE
         ZI( 6) =  ONE
         ZI( 7) =  ONE
         ZI( 8) =  ONE

         ZI( 9) = -ONE
         ZI(10) = -ONE
         ZI(11) = -ONE
         ZI(12) = -ONE

         ZI(13) =  ZERO
         ZI(14) =  ZERO
         ZI(15) =  ZERO
         ZI(16) =  ZERO

         ZI(17) =  ONE
         ZI(18) =  ONE
         ZI(19) =  ONE
         ZI(20) =  ONE

! Corner nodes

         DO I=1,8

            PSH(I) = ( ONE + SSI*XI(I) )*( ONE + SSJ*ET(I) )*( ONE + SSK*ZI(I) )*( SSI*XI(I) + SSJ*ET(I) + SSK*ZI(I) -2 )/EIGHT

            DPSHG(1,I) = XI(I)*(ONE + SSJ*ET(I))*(ONE + SSK*ZI(I))*(TWO*SSI*XI(I) +     SSJ*ET(I) +     SSK*ZI(I) - ONE)/EIGHT
            DPSHG(2,I) = ET(I)*(ONE + SSK*ZI(I))*(ONE + SSI*XI(I))*(    SSI*XI(I) + TWO*SSJ*ET(I) +     SSK*ZI(I) - ONE)/EIGHT
            DPSHG(3,I) = ZI(I)*(ONE + SSI*XI(I))*(ONE + SSJ*ET(I))*(    SSI*XI(I) +     SSJ*ET(I) + TWO*SSK*ZI(I) - ONE)/EIGHT

         ENDDO

! Mid side nodes for XI = 0

         NODE(1) =  9; NODE(2) = 11; NODE(3) = 17; NODE(4) = 19

         DO I=1,4

            PSH(NODE(I)) = (ONE - SSI*SSI)*(ONE + SSJ*ET(NODE(I)))*(ONE + SSK*ZI(NODE(I)))/FOUR

            DPSHG(1,NODE(I)) = -SSI*( ONE + SSJ*ET(NODE(I)) )*( ONE + SSK*ZI(NODE(I)) )/TWO
            DPSHG(2,NODE(I)) =  ET(NODE(I))*( ONE - SSI*SSI )*( ONE + SSK*ZI(NODE(I)) )/FOUR
            DPSHG(3,NODE(I)) =  ZI(NODE(I))*( ONE - SSI*SSI )*( ONE + SSJ*ET(NODE(I)) )/FOUR

         ENDDO

! Mid side nodes for ET = 0

         NODE(1) = 10; NODE(2) = 12; NODE(3) = 18; NODE(4) = 20

         DO I=1,4

            PSH(NODE(I)) = (ONE - SSJ*SSJ)*(ONE + SSK*ZI(NODE(I)))*(ONE + SSI*XI(NODE(I)))/FOUR

            DPSHG(1,NODE(I)) =  XI(NODE(I))*( ONE - SSJ*SSJ )*( ONE + SSK*ZI(NODE(I)) )/FOUR
            DPSHG(2,NODE(I)) = -SSJ*( ONE + SSI*XI(NODE(I)) )*( ONE + SSK*ZI(NODE(I)) )/TWO
            DPSHG(3,NODE(I)) =  ZI(NODE(I))*( ONE - SSJ*SSJ )*( ONE + SSI*XI(NODE(I)) )/FOUR

         ENDDO

! Mid side nodes for ZI = 0

         NODE(1) = 13; NODE(2) = 14; NODE(3) = 15; NODE(4) = 16

         DO I=1,4

            PSH(NODE(I)) = (ONE - SSK*SSK)*(ONE + SSI*XI(NODE(I)))*(ONE + SSJ*ET(NODE(I)))/FOUR

            DPSHG(1,NODE(I)) =  XI(NODE(I))*( ONE - SSK*SSK )*( ONE + SSJ*ET(NODE(I)) )/FOUR
            DPSHG(2,NODE(I)) =  ET(NODE(I))*( ONE - SSK*SSK )*( ONE + SSI*XI(NODE(I)) )/FOUR
            DPSHG(3,NODE(I)) = -SSK*( ONE + SSI*XI(NODE(I)) )*( ONE + SSJ*ET(NODE(I)) )/TWO

         ENDDO

! **********************************************************************************************************************************
! Error: NUM_NODES is not 8 or 20
  
      ELSE
  
         WRITE(F06,1932) SUBR_NAME, NUM_NODES, EID, TYPE, NODES_8, NODES_20
         WRITE(ERR,1932) SUBR_NAME, NUM_NODES, EID, TYPE, NODES_8, NODES_20
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit
  
      ENDIF
  
! **********************************************************************************************************************************
! ELDATA output:
 
      IF ((WRT_BUG_THIS_TIME == 'Y') .AND. (WRT_BUG(7) > 0)) THEN

         NAME(1) = 'Nodes  1 thru  4:'
         NAME(2) = 'Nodes  5 thru  8:'
         NAME(3) = 'Nodes  9 thru 12:'
         NAME(4) = 'Nodes 13 thru 16:'
         NAME(5) = 'Nodes 17 thru 20:'

         WRITE(BUG,1101) ELDT_BUG_SHPJ_BIT, TYPE, EID, CALLING_SUBR

         WRITE(BUG,1102) IGAUS, JGAUS, KGAUS, IORD_MSG, IORZZZ
         WRITE(BUG,1103) SSI,SSJ,SSK
         WRITE(BUG,1112)

         J = 0
         WRITE(BUG,1105)
         DO I=1,NUM_NODES,4
            J = J + 1
            WRITE(BUG,1201) NAME(J), PSH(I), PSH(I+1), PSH(I+2), PSH(I+3)
         ENDDO
         WRITE(BUG,*)
         WRITE(BUG,*)

         J = 0
         WRITE(BUG,1115)
         DO I=1,NUM_NODES,4
            J = J + 1
            WRITE(BUG,1201) NAME(J), DPSHG(1,I), DPSHG(1,I+1), DPSHG(1,I+2), DPSHG(1,I+3)
         ENDDO
         WRITE(BUG,*)
         WRITE(BUG,*)
 
         J = 0
         WRITE(BUG,1125)
         DO I=1,NUM_NODES,4
            J = J + 1
            WRITE(BUG,1201) NAME(J), DPSHG(2,I), DPSHG(2,I+1), DPSHG(2,I+2), DPSHG(2,I+3)
         ENDDO
         WRITE(BUG,*)
         WRITE(BUG,*)
 
         J = 0
         WRITE(BUG,1135)
         DO I=1,NUM_NODES,4
            J = J + 1
            WRITE(BUG,1201) NAME(J), DPSHG(3,I), DPSHG(3,I+1), DPSHG(3,I+2), DPSHG(3,I+3)
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

 1102 FORMAT(47X,' Gauss point: I = ',I3,', J = ',I3,', K = ',I3,/,31X,'with integration order ',A,I2,/,                           &
             35X,'and using Gaussian integration to determine strain-displ matrix'//)

 1112 format(51x,'Outputs from subroutine SHP3DH',/,51x,'------------------------------',/)

 1103 FORMAT(53X,'Coordinates of Gauss point',/,49x,'XI              ET              ZI',/,40X,3(1ES16.6),//)

 1105 FORMAT(58X,'Shape functions')

 1115 FORMAT(42X,'Derivatives of shape functions with respect to XI')

 1125 FORMAT(42X,'Derivatives of shape functions with respect to ET')

 1135 FORMAT(42X,'Derivatives of shape functions with respect to ZI')

 1201 FORMAT(22X,A,1X,4(1ES16.6))

! **********************************************************************************************************************************
 
      END SUBROUTINE SHP3DH
