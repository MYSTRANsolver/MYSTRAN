! ###############################################################################################################################
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

   MODULE BD_PLOAD1_Interface

   INTERFACE

      SUBROUTINE BD_PLOAD1 ( CARD, CC_LOAD_FND )

  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1Q
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LPLOAD, LSUB, NPCARD, NPLOAD, NSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_PLOAD1_BEGEND
      USE MODEL_STUF, ONLY            :  PRESS_SIDS, SUBLOD

      IMPLICIT NONE

      CHARACTER(LEN=*),INTENT(IN)     :: CARD               ! A Bulk Data card
      CHARACTER( 1*BYTE),INTENT(INOUT):: CC_LOAD_FND(LSUB,2)! 'Y' if B.D load/temp card w/ same set ID (SID) as C.C. LOAD = SID

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_PLOAD1_BEGEND

      CHARACTER(8*BYTE)               :: TYPE               ! The type flag
      CHARACTER(8*BYTE)               :: SCALE              ! The scale flag
      REAL(DOUBLE)                    :: X1, X2             ! x locations
      REAL(DOUBLE)                    :: P1, P2             ! pressures/loads

      END SUBROUTINE BD_PLOAD1

   END INTERFACE

   END MODULE BD_PLOAD1_Interface




!      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
!      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1Q
!      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LPLOAD1, LSUB, NPCARD, NPLOAD1, NPLOAD1, NSUB
!      USE TIMDAT, ONLY                :  TSEC
!      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_PLOAD1_BEGEND
!      USE MODEL_STUF, ONLY            :  PRESS_SIDS, SUBLOD

!      USE BD_PLOAD1_USE_IFs

!      IMPLICIT NONE

!      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_PLOAD1'
!      CHARACTER(LEN=*),INTENT(IN)     :: CARD               ! A Bulk Data card
!      CHARACTER( 1*BYTE),INTENT(INOUT):: CC_LOAD_FND(LSUB,2)! 'Y' if B.D load/temp card w/ same set ID (SID) as C.C. LOAD = SID
!      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)          ! The 10 fields of characters making up CARD
!      CHARACTER(8*BYTE)               :: TOKEN              ! The 1st 8 characters from a JCARD
!      CHARACTER(8*BYTE)               :: TOKTYP             ! The type of token in a field of parent card. Output from subr TOKCHK

!      INTEGER(LONG)                   :: J                  ! DO loop index
!      INTEGER(LONG)                   :: JERR               ! Error count
!      INTEGER(LONG)                   :: SETID              ! Load set ID on PLOADi card
!      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_PLOAD1_BEGEND
