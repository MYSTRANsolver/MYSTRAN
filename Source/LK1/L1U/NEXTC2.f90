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
 
      SUBROUTINE NEXTC2 ( PARENT, ICONTINUE, IERR, CHILD )

      ! Looks for 2 physical Bulk Data large field format continuation
      ! entries belonging to a large field parent.
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06, IN1, INFILE
      USE SCONTR, ONLY                :  BD_ENTRY_LEN, BLNK_SUB_NAM, ECHO, FATAL_ERR, JCARD_LEN
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  NEXTC2_BEGEND
 
      USE NEXTC2_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'NEXTC2'
      CHARACTER(LEN=*), INTENT(IN)    :: PARENT            ! 

      CHARACTER(LEN=BD_ENTRY_LEN), INTENT(OUT) :: CHILD    ! 

      CHARACTER(LEN=BD_ENTRY_LEN)     :: CHILD1            ! 
      CHARACTER(LEN=BD_ENTRY_LEN)     :: CHILD2            ! 
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! 10 fields of 8 characters of PARENT
      CHARACTER(LEN(JCARD))           :: NEWTAG            ! Field 1  of cont   card
      CHARACTER(LEN(JCARD))           :: OLDTAG            ! Field 10 of parent card
 
      INTEGER(LONG), INTENT(OUT)      :: ICONTINUE         ! =1 if next card is current card's continuation or =0 if not
      INTEGER(LONG), INTENT(OUT)      :: IERR              ! Error indicator from subr FFIELD, called herein
      INTEGER(LONG)                   :: COMMENT_COL       ! Col on PARENT where a comment begins (if one exists)
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error value from READ
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr READERR  
      INTEGER(LONG)                   :: REC_NO            ! Record number when reading a file. Input to subr READERR
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = NEXTC2_BEGEND
 
! **********************************************************************************************************************************
      ! Initialize
      IERR = 0
      ICONTINUE = 0
      CHILD(1:) = 'z'
      OLDTAG(1:) = ' '

      IERR = 0

      ! Make units for writing errors the error file and output file
      OUNT(1) = ERR
      OUNT(2) = F06

      ! Make JCARD for PARENT and get the 1st 8 chars of field 10 (cont mnemonic)
      CALL MKJCARD ( SUBR_NAME, PARENT, JCARD )
      OLDTAG(1:8) = JCARD(10)(1:8)

      ! Read next card. If it is a continuation to the parent it will
      ! be the 1st half of the whole continuation
      CALL READ_BDF_LINE(IN1, IOCHK, CHILD1)

      NEWTAG = CHILD1(1:8)
      IF (NEWTAG == OLDTAG) THEN
         ICONTINUE = 1
      ELSE IF ((OLDTAG(1:1) == '*') .AND. (NEWTAG(1:1) == ' ') .AND. (OLDTAG(2:8) == NEWTAG(2:8))) THEN
         ! large field
         ICONTINUE = 1
      ELSE IF ((OLDTAG(1:1) == ' ') .AND. (NEWTAG(1:1) == '*') .AND. (OLDTAG(2:8) == NEWTAG(2:8))) THEN
         ! large field
         ICONTINUE = 1
      ELSE IF ((NEWTAG(1:1) /= '*') .AND. (NEWTAG(1:1) /= '$')) THEN
         ! different card type (e.g., LOAD -> FORCE
         BACKSPACE(IN1)
         CHILD = CHILD1
         RETURN
      ELSE
         ! can't find the continuation marker.  FATAL :)
         BACKSPACE(IN1)
         !WRITE(F06,102) OLDTAG
         !WRITE(ERR,102) OLDTAG
         !WRITE(F06,103)
         !WRITE(ERR,103)
         !WRITE(F06,104) 'CHILD1:', CHILD1
         !WRITE(ERR,104) 'CHILD1:', CHILD1
         FLUSH(F06)
         FLUSH(ERR)
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE('Y')  ! FATAL error
         RETURN
         RETURN
      ENDIF 

      ! Read 2nd half of continuation entry, if it exists
      CALL READ_BDF_LINE(IN1, IOCHK, CHILD2)

      OLDTAG = CHILD1(73:80)
      NEWTAG = CHILD2( 1: 8)
      ICONTINUE = 0


     IF (NEWTAG == OLDTAG) THEN
        ICONTINUE = 1
     ELSE IF ((OLDTAG(1:1) == '*') .AND. (NEWTAG(1:1) == ' ') .AND. (OLDTAG(2:8) == NEWTAG(2:8))) THEN
        ICONTINUE = 1
     ELSE IF ((OLDTAG(1:1) == ' ') .AND. (NEWTAG(1:1) == '*') .AND. (OLDTAG(2:8) == NEWTAG(2:8))) THEN
        ICONTINUE = 1
     ELSE
        BACKSPACE(IN1)
        CHILD2(1:) = ' '
        ICONTINUE = 1
        CALL FFIELD2 ( CHILD1, CHILD2, CHILD, IERR )
        RETURN
     ENDIF

      ! Call FFIELD2 to put the 2 CHILDi's together and left justify
      CALL FFIELD2(CHILD1, CHILD2, CHILD, IERR)
      ICONTINUE = 1
      IF (ECHO /= 'NONE  ') THEN
         WRITE(F06,101) CHILD1
         WRITE(F06,101) CHILD2
      ENDIF
! **********************************************************************************************************************************
  101 FORMAT('ECHO nextc2: ', A)

! **********************************************************************************************************************************

      END SUBROUTINE NEXTC2

