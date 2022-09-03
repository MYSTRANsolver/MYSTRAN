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
  
      SUBROUTINE SHP3DT ( GAUSS_PT, NUM_NODES, CALLING_SUBR, IORD_MSG, IORZZZ, SSI, SSJ, SSK, WRT_BUG_THIS_TIME, PSH,  &
                          DPSHG )

! Generates shape functions for 3D TETRA (4 node and 10 node) elements.
  
! The node numbering and axes convention are shown below with XI, ET, ZI ranging from 0 to 1

 
!                                       ZI
!                                        .  
!                                        .
!                                        .
!                                        .4
!                                       . .
!                                      . . . 
!                                     .  .  .  
!                                    .   .   .   
!                                   .    .    .    
!                                  .     .     .   
!                                 .      .8     .
!                                .       .       .
!                               .        .        .
!                              .         .         .
!                             .          .          .10
!                           9.           .           .
!                           .          . 1 .          .
!                          .         .       .         .
!                         .        .           .        .
!                        .       .               .       .
!                       .      .                   .      .
!                      .     .5                     7.     .
!                     .    .                           .    .
!                    .   .                               .   . 
!                   .  .                                   .  .     
!                  . .                                       . .        
!                 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .ET
!               . 2                     6                       3           
!             .                                                                
!         .                                                                       
!         .
!        XI


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_BUG, WRT_ERR, WRT_LOG, BUG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_SHPJ_BIT, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  SHP_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO, FOUR
      USE MODEL_STUF, ONLY            :  EID, TYPE
 
      USE SHP3DT_USE_IFs

      IMPLICIT NONE
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SHP3DT'
      CHARACTER(LEN=*)  , INTENT(IN)  :: CALLING_SUBR      ! Subr that called this subr (used for debug output)
      CHARACTER(LEN=*)  , INTENT(IN)  :: IORD_MSG          ! Character name of the integration order (used for debug output)
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRT_BUG_THIS_TIME ! If 'Y' then write to BUG file if WRT_BUG array says to
      CHARACTER(17*BYTE)              :: NAME(5)           ! Used for DEBUG output annotation

      INTEGER(LONG), INTENT(IN)       :: GAUSS_PT          ! Gauss point (needed for some optional output)
      INTEGER(LONG), INTENT(IN)       :: IORZZZ            ! Integration order (used for debug output)
      INTEGER(LONG), INTENT(IN)       :: NUM_NODES         ! Number of element nodes
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: NODES_4     = 4   ! Number of nodes for one type of element
      INTEGER(LONG)                   :: NODES_10    = 10  ! Number of nodes for one type of element
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
  
      IF (NUM_NODES == NODES_4) THEN
 
         PSH(1) = 1 - SSI - SSJ - SSK
         PSH(2) = SSI
         PSH(3) = SSJ
         PSH(4) = SSK

         DPSHG(1,1) = -ONE     ;     DPSHG(2,1) = -ONE     ;     DPSHG(3,1) = -ONE
         DPSHG(1,2) =  ONE     ;     DPSHG(2,2) =  ZERO    ;     DPSHG(3,2) =  ZERO
         DPSHG(1,3) =  ZERO    ;     DPSHG(2,3) =  ONE     ;     DPSHG(3,3) =  ZERO
         DPSHG(1,4) =  ZERO    ;     DPSHG(2,4) =  ZERO    ;     DPSHG(3,4) =  ONE

! **********************************************************************************************************************************
! Generate shape functions and derivatives for 3D element with 10 nodes
  
      ELSE IF (NUM_NODES == NODES_10) THEN
  
         PHI = ONE - SSI - SSJ - SSK

         PSH( 1) = PHI*(TWO*PHI - ONE)

         PSH( 2) = SSI*(TWO*SSI - ONE)
         PSH( 3) = SSJ*(TWO*SSJ - ONE)
         PSH( 4) = SSK*(TWO*SSK - ONE)

         PSH( 5) = FOUR*SSI*PHI
         PSH( 6) = FOUR*SSI*SSJ
         PSH( 7) = FOUR*SSJ*PHI

         PSH( 8) = FOUR*SSK*PHI
         PSH( 9) = FOUR*SSI*SSK
         PSH(10) = FOUR*SSJ*SSK

         DPSHG(1, 1) = ONE - FOUR*PHI     ;     DPSHG(2, 1) =  DPSHG(1,1)        ;     DPSHG(3, 1) =  DPSHG(1,1)

         DPSHG(1, 2) = FOUR*SSI - ONE     ;     DPSHG(2, 2) =  ZERO              ;     DPSHG(3, 2) =  ZERO
         DPSHG(1, 3) = ZERO               ;     DPSHG(2, 3) =  FOUR*SSJ - ONE    ;     DPSHG(3, 3) =  ZERO
         DPSHG(1, 4) = ZERO               ;     DPSHG(2, 4) =  ZERO              ;     DPSHG(3, 4) =  FOUR*SSK - ONE

         DPSHG(1, 5) = FOUR*(PHI - SSI)   ;     DPSHG(2, 5) = -FOUR*SSI          ;     DPSHG(3, 5) = -FOUR*SSI
         DPSHG(1, 6) = FOUR*SSJ           ;     DPSHG(2, 6) =  FOUR*SSI          ;     DPSHG(3, 6) =  ZERO
         DPSHG(1, 7) =-FOUR*SSJ           ;     DPSHG(2, 7) =  FOUR*(PHI - SSJ)  ;     DPSHG(3, 7) = -FOUR*SSJ

         DPSHG(1, 8) =-FOUR*SSK           ;     DPSHG(2, 8) = -FOUR*SSK          ;     DPSHG(3, 8) =  FOUR*(PHI - SSK)
         DPSHG(1, 9) = FOUR*SSK           ;     DPSHG(2, 9) =  ZERO              ;     DPSHG(3, 9) =  FOUR*SSI
         DPSHG(1,10) = ZERO               ;     DPSHG(2,10) =  FOUR*SSK          ;     DPSHG(3,10) =  FOUR*SSJ

! **********************************************************************************************************************************
! Error: NUM_NODES is not 4 or 10
  
      ELSE
  
         WRITE(ERR,1932) SUBR_NAME, NUM_NODES, EID, TYPE, NODES_4, NODES_10
         WRITE(F06,1932) SUBR_NAME, NUM_NODES, EID, TYPE, NODES_4, NODES_10
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit
  
      ENDIF
  
! **********************************************************************************************************************************
! ELDATA output:
 
      IF ((WRT_BUG_THIS_TIME == 'Y') .AND. (WRT_BUG(7) > 0)) THEN

         WRITE(BUG,1101) ELDT_BUG_SHPJ_BIT, TYPE, EID, CALLING_SUBR

         WRITE(BUG,1102) GAUSS_PT, IORD_MSG, IORZZZ
         WRITE(BUG,1103) SSI,SSJ,SSK
         WRITE(BUG,1112)

         IF (NUM_NODES == NODES_4) THEN

            NAME(1) = 'Nodes  1 thru  4:'

            WRITE(BUG,1105)
            WRITE(BUG,1201) NAME(1), PSH(1), PSH(2), PSH(3), PSH(4)

            WRITE(BUG,1115)
            WRITE(BUG,1201) NAME(1), DPSHG(1,1), DPSHG(1,2), DPSHG(1,3), DPSHG(1,4)

            WRITE(BUG,1125)
            WRITE(BUG,1201) NAME(1), DPSHG(2,1), DPSHG(2,2), DPSHG(2,3), DPSHG(2,4)

            WRITE(BUG,1135)
            WRITE(BUG,1201) NAME(1), DPSHG(3,1), DPSHG(3,2), DPSHG(3,3), DPSHG(3,4)

         ELSE IF (NUM_NODES == NODES_10) THEN

            NAME(1) = 'Nodes  1 thru  5:'
            NAME(2) = 'Nodes  1 thru 10:'

            J = 0
            WRITE(BUG,1105)
            DO I=1,NUM_NODES,5
               J = J + 1
               WRITE(BUG,1202) NAME(J), PSH(I), PSH(I+1), PSH(I+2), PSH(I+3), PSH(I+4)
            ENDDO
            WRITE(BUG,*)
            WRITE(BUG,*)

            J = 0
            WRITE(BUG,1115)
            DO I=1,NUM_NODES,5
               J = J + 1
               WRITE(BUG,1202) NAME(J), DPSHG(1,I), DPSHG(1,I+1), DPSHG(1,I+2), DPSHG(1,I+3), DPSHG(1,I+3)
            ENDDO
            WRITE(BUG,*)
            WRITE(BUG,*)
 
            J = 0
            WRITE(BUG,1125)
            DO I=1,NUM_NODES,5
               J = J + 1
               WRITE(BUG,1202) NAME(J), DPSHG(2,I), DPSHG(2,I+1), DPSHG(2,I+2), DPSHG(2,I+3), DPSHG(2,I+3)
            ENDDO
            WRITE(BUG,*)
            WRITE(BUG,*)
 
            J = 0
            WRITE(BUG,1135)
            DO I=1,NUM_NODES,5
               J = J + 1
               WRITE(BUG,1202) NAME(J), DPSHG(3,I), DPSHG(3,I+1), DPSHG(3,I+2), DPSHG(3,I+3), DPSHG(3,I+3)
            ENDDO
            WRITE(BUG,*)
            WRITE(BUG,*)

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
 1932 FORMAT(' *ERROR  1932: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' SUBROUTINE CALLED WITH ILLEGAL NUMBER OF NODES =',I8,' FOR ELEMENT',I8,' TYPE ',A,'. MUST BE ',I3,    &
                           ' OR ',I3)

 1101 FORMAT(' ------------------------------------------------------------------------------------------------------------------',&
             '-----------------',/,                                                                                                &
             ' ELDATA(',I2,',PRINT) requests for ',A,' element number ',I8,', Calling subroutine ',A,/,                            &
             ' ========================================================================================',/)

 1102 FORMAT(57X,' Gauss point = ',I3,/,31X,'with integration order ',A,I2,/,                           &
             35X,'and using Gaussian integration to determine strain-displ matrix'//)

 1112 format(51x,'Outputs from subroutine SHP3DT',/,51x,'------------------------------',/)

 1103 FORMAT(53X,'Coordinates of Gauss point',/,49x,'XI              ET              ZI',/,40X,3(1ES16.6),//)

 1105 FORMAT(58X,'Shape functions')

 1115 FORMAT(42X,'Derivatives of shape functions with respect to XI')

 1125 FORMAT(42X,'Derivatives of shape functions with respect to ET')

 1135 FORMAT(42X,'Derivatives of shape functions with respect to ZI')

 1201 FORMAT(22X,A,1X,4(1ES16.6))

 1202 FORMAT(22X,A,1X,5(1ES16.6))

! **********************************************************************************************************************************
 
      END SUBROUTINE SHP3DT
