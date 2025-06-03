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

   MODULE BD_CQUAD8_Interface

   INTERFACE

      SUBROUTINE BD_CQUAD8 ( CARD, LARGE_FLD_INP, NUM_GRD )

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, IERRFL, FATAL_ERR, JCARD_LEN, JF, LMATANGLE, LPLATEOFF, LPLATETHICK,        &
                                         MEDAT_CQUAD8, NCQUAD8, NEDAT, NELE, NMATANGLE, NPLATEOFF, NPLATETHICK
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  EDAT, ETYPE, MATANGLE, PLATEOFF, PLATETHICK
 
      USE MKJCARD_Interface
      USE ELEPRO_Interface
      USE TOKCHK_Interface
      USE OUTA_HERE_Interface
      USE R8FLD_Interface
      USE I4FLD_Interface
      USE BD_IMBEDDED_BLANK_Interface
      USE CRDERR_Interface
      USE NEXTC_Interface
      USE NEXTC2_Interface
      USE CARD_FLDS_NOT_BLANK_Interface

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_CQUAD8'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: JCARD_EDAT(10)    ! JCARD values sent to subr ELEPRO
      CHARACTER( 8*BYTE)              :: TOKEN             ! The 1st 8 characters from a JCARD
      CHARACTER( 8*BYTE)              :: TOKTYP            ! Indicator of the type of val found in a B.D. field (e.g. int, real,...)
      CHARACTER(LEN=JCARD_LEN)        :: ID                ! Character value of element ID (field 2 of parent card)
      CHARACTER(LEN=JCARD_LEN)        :: NAME              ! Field 1 of CARD

      INTEGER(LONG), INTENT(OUT)      :: NUM_GRD           ! Number of GRID's + SPOINT's for the elem
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: I4INP             ! A value read from input file that should be an integer
      INTEGER(LONG)                   :: INT41,INT42        ! An integer used in getting MATANGLE
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
 
      REAL(DOUBLE)                    :: R8INP     = ZERO  ! A value read from input file that should be a real value
 
      END SUBROUTINE BD_CQUAD8

   END INTERFACE

   END MODULE BD_CQUAD8_Interface

