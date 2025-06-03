! ##################################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2022 Dr William R Case, Jr (mystransolver@gmail.com)                                              
                                                                                                         
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
 
      SUBROUTINE BD_CQUAD80 ( CARD, LARGE_FLD_INP )
 
! Processes CQUAD8 Bulk Data Cards to increment:
!   (1) LMATANGLE   if the elem has a material prop angle
!   (2) LPLATEOFF   if the elem has an offset
!   (3) LPLATETHICK if the elem has thicknesses defined
 
      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE SCONTR, ONLY                :  JCARD_LEN, LMATANGLE, LPLATEOFF, LPLATETHICK

      USE MKJCARD_Interface
      USE NEXTC0_Interface
      USE NEXTC20_Interface

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
 
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
 
! **********************************************************************************************************************************

! Skip the first card
      IF (LARGE_FLD_INP == 'N') THEN
         CALL NEXTC0  ( CARD, ICONT, IERR )
      ELSE
         CALL NEXTC20 ( CARD, ICONT, IERR, CHILD )
         CARD = CHILD
      ENDIF
      
      IF (ICONT == 1) THEN                                 ! There was a 1st continuation card (mandatory)

! See if there is a material orientation angle. If so, increment LMATANGLE

        IF (JCARD(8)(1:) /= ' ') THEN
           LMATANGLE = LMATANGLE + 1
        ENDIF

! See if there is a plate offset. If so, increment LPLATEOFF

        IF (JCARD(9)(1:) /= ' ') THEN
           LPLATEOFF = LPLATEOFF + 1
        ENDIF

        LPLATETHICK = LPLATETHICK + 4

      ENDIF
 

! **********************************************************************************************************************************

      RETURN

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_CQUAD80
