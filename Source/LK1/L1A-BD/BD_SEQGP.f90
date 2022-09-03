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
  
      SUBROUTINE BD_SEQGP ( CARD )
  
!  Processes SEQGP Bulk Data Cards. Reads and checks data:

!  1) Grid ID's entered into integer array SEQ1
!  2) Sequence ID's entered into real array SEQ2. If input sequence numbers are integer, they are converted
!     to real before entering them into array SEQ2.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, JCARD_LEN, JF, LSEQ, NSEQ
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_SEQGP_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  SEQ1, SEQ2

      USE BD_SEQGP_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_SEQGP'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
 
      INTEGER(LONG)                   :: DEC_COL           ! Number that indicates whether entry in a seq field is real or integer
      INTEGER(LONG)                   :: ISEQ              ! An integer sequence number
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: JFLD1             ! A field number on the SEQGP card where grid ID's are located
      INTEGER(LONG)                   :: JFLD2             ! A field number on the SEQGP card where sequence numbers are located
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_SEQGP_BEGEND
 
      REAL(DOUBLE)                    :: RSEQ              ! A real sequence number

      INTRINSIC INDEX,DBLE
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
!  SEQGP Bulk Data Card routine
 
!    FIELD   ITEM           ARRAY ELEMENT
!    -----   ------------   -------------
!    2,4,6,8 Grid ID's      SEQ1(nseq - nseq+4)
!    3,5,7,9 Seq ID's       SEQ2(nseq - nseq+4) Can be real or integer
 
!  Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Read and check data

      DO J = 1,4
         JFLD1 = 2*J
         JFLD2 = 2*J + 1
         IF (JCARD(JFLD1)(1:) /= ' ') THEN                 ! Check grid field. If blank CYCLE
            NSEQ = NSEQ+1                                  ! Increment NSEQ if field 2,4,6 or 8 is not blank
            CALL I4FLD(JCARD(JFLD1), JF(JFLD1), SEQ1(NSEQ))! Read grid ID in field 2,4,6 or 8
            IF (JCARD(JFLD2)(1:) /= ' ') THEN              ! If sequence field is not blank, OK. Otherwise, error
               DEC_COL = INDEX(JCARD(JFLD2),'.')
               IF (DEC_COL == 0) THEN                      ! Integer entry for sequence number
                  CALL I4FLD ( JCARD(JFLD2), JF(JFLD2), ISEQ )
                  SEQ2(NSEQ) = DBLE(ISEQ)
               ELSE                                        ! Real entry for sequence number
                  CALL R8FLD ( JCARD(JFLD2), JF(JFLD2), RSEQ )
                  SEQ2(NSEQ) = RSEQ
               ENDIF
               IF (SEQ2(NSEQ) <= ZERO) THEN
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1157) JFLD2,JCARD(JFLD2)
                  WRITE(F06,1157) JFLD2,JCARD(JFLD2)
               ENDIF 
            ELSE
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1159) JFLD2
               WRITE(F06,1159) JFLD2
            ENDIF
         ELSE
            CYCLE
         ENDIF
      ENDDO   
 
      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )     ! Make sure that there are no imbedded blanks in fields 2-9
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1157 FORMAT(' *ERROR  1157: VALUE ON SEQGP ENTRY, FIELD ',I3,', MUST BE >= 0 BUT IS = ',A)

 1159 FORMAT(' *ERROR  1159: FIELD ',I3,' OF SEQGP ENTRY CANNOT BE BLANK. MUST CONTAIN AN INTEGER OR REAL SEQUENCE NUMBER')
 
 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_SEQGP
