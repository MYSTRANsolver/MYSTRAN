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
 
      SUBROUTINE BD_SNORM ( CARD )
 
! Processes SNORM Bulk Data Cards
!  1) Reads grid point ID and coord system ID and enters it into array SNORM
!  2) Reads normal vector into array RSNORM

      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, JCARD_LEN, JF, NSNORM
      USE MODEL_STUF, ONLY            :  SNORM, RSNORM
      USE CONSTANTS_1, ONLY           :  ZERO
      
      USE MKJCARD_Interface
      USE R8FLD_Interface
      USE I4FLD_Interface
      USE BD_IMBEDDED_BLANK_Interface
      USE CRDERR_Interface
      USE CARD_FLDS_NOT_BLANK_Interface

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_SNORM'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
 
      INTEGER(LONG)                   :: I4INP     = 0     ! A value read from input file that should be an integer value
 
      REAL(DOUBLE)                    :: R8INP     = ZERO  ! A value read from input file that should be a real value


! **********************************************************************************************************************************
! SNORM Bulk Data Card routine
 
!   FIELD   ITEM           ARRAY ELEMENT
!   -----   ------------   -------------
!    2      GID            SNORM(nsnorm,1)
!    3      CID            SNORM(nsnorm,2)
!    4      N1             RSNORM(nsnorm,1)
!    5      N2             RSNORM(nsnorm,2)
!    6      N3             RSNORM(nsnorm,3)
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )

      NSNORM  = NSNORM  + 1
 
      CALL I4FLD ( JCARD(2), JF(2), I4INP )
      SNORM(NSNORM,1) = I4INP
      
      CALL I4FLD ( JCARD(3), JF(3), I4INP )
      SNORM(NSNORM,2) = I4INP
      
      CALL R8FLD ( JCARD(4), JF(4), R8INP )
      RSNORM(NSNORM,1) = R8INP
      
      CALL R8FLD ( JCARD(5), JF(5), R8INP )
      RSNORM(NSNORM,2) = R8INP
      
      CALL R8FLD ( JCARD(6), JF(6), R8INP )
      RSNORM(NSNORM,3) = R8INP

      CALL BD_IMBEDDED_BLANK   ( JCARD,2,3,4,5,6,0,0,0 )
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,7,8,9 )
      CALL CRDERR ( CARD )


! **********************************************************************************************************************************

      RETURN

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_SNORM
