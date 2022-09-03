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
  
      SUBROUTINE BD_PCOMP0 ( CARD, LARGE_FLD_INP, IPLIES )
  
! Processes PCOMP Bulk Data Cards to determine the number of plies there are defined for this PCOMP entry
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, F04, f06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, JCARD_LEN
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_PCOMP0_BEGEND

      USE BD_PCOMP0_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_PCOMP0'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: LAM               ! Field 9 of parent entry. Symmetry option
 
      INTEGER(LONG), INTENT(OUT)      :: IPLIES            ! Count of number of plies defined by this PCOMP
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_PCOMP0_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! PCOMP Bulk Data Card:
 
!   FIELD   ITEM            EXPLANATION 
!   -----   ------------    -------------
!    2      PID             Prop ID
!    3      Z0              Dist from ref plane to bottom surface (default = -0.5 times layer thickness)
!    4      NSM             Non structural mass per unit area
!    5      SB              Allowable interlaminar shear stress. Required if FT is specified
!    6      FT              Failure theory ("HILL", "HOFF", "TSAI", "STRN")
!    7      TREF            Ref temperature
!    8      GE              Damping coeff
!    9      LAM             Symm lamination option (if "SYM" only plies on one side of elem centerline are specified)
!                           (plies are numbered starting with 1 at the bottom layer. If an odd number of plies is
!                           desired with SYM option the center ply thickness should be 1/2 the actual thickness
 
! continuation cards (2 plies specified per continuation entry:
! 
!   FIELD   ITEM            EXPLANATION
!   -----   ------------    -------------
!    2      MID1            Material ID of ply 1
!    3      T1              Thickness of ply 1
!    4      THETA1          Orientation angle of longitudinal direction of ply 1 wrt material axis for the composite element
!    5      SOUT1           Stress or strain output request ("YES" or "NO")
!    6      MID2            Same as above for 2nd ply
!    7      T2               
!    8      THETA2          
!    9      SOUT2           
 
! Subsequent continuation cards follow the same pattern as the 1st continuation card

 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      LAM = JCARD(9)
 
! Count number of plies on continuation cards. There can be 2 plies/cont card and a ply is assumed to exist if any of the 4
! fields for the ply have any data
  
      IPLIES = 0
 
      DO 
         IF (LARGE_FLD_INP == 'N') THEN
            CALL NEXTC0  ( CARD, ICONT, IERR )
         ELSE
            CALL NEXTC20 ( CARD, ICONT, IERR, CHILD )
            CARD = CHILD
         ENDIF
         CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
         IF (ICONT == 1) THEN
                                                           ! See if any data is in fields 2-5. If so, up ply count
            IF((JCARD(2)(1:) /= ' ') .OR. (JCARD(3)(1:) /= ' ') .OR. (JCARD(4)(1:) /= ' ') .OR. (JCARD(5)(1:) /= ' ')) THEN
               IPLIES = IPLIES + 1
            ENDIF
                                                           ! See if any data is in fields 6-9. If so, up ply count
            IF((JCARD(6)(1:) /= ' ') .OR. (JCARD(7)(1:) /= ' ') .OR. (JCARD(8)(1:) /= ' ') .OR. (JCARD(9)(1:) /= ' ')) THEN
               IPLIES = IPLIES + 1
            ENDIF

         ELSE
            EXIT
         ENDIF
      ENDDO 
   
      IF (LAM(1:4) == 'SYM ') THEN
         IPLIES = 2*IPLIES
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  
      END SUBROUTINE BD_PCOMP0
