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

      SUBROUTINE FILE_INQUIRE ( MESSAGE )

! Inquires about whether files are opened. Writes results to file F06

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  FILE_NAM_MAXLEN,  MOT4,    MOU4,    OU4_EXT, OT4_EXT, WRT_LOG

      USE IOUNT1, ONLY                :  ANS,     BUG,     EIN,     ENF,     ERR,     F04,     F06,     IN0,     IN1,     NEU,     &
                                         PCH,     SEQ,     SC1,     SPC,                                                           &
                                         F21,     F22,     F23,     F24,     F25,                                                  &
                                         L1A,     L1B,     L1C,     L1D,     L1E,     L1F,     L1G,     L1H,     L1I,     L1J,     &
                                         L1K,     L1L,     L1M,     L1N,     L1O,     L1P,     L1Q,     L1R,     L1S,     L1T,     &
                                         L1U,     L1V,     L1W,     L1X,     L1Y,     L1Z,                                         &
                                         L2A,     L2B,     L2C,     L2D,     L2E,     L2F,     L2G,     L2H,     L2I,     L2J,     &
                                         L2K,     L2L,     L2M,     L2N,     L2O,     L2P,     L2Q,     L2R,     L2S,     L2T,     &
                                         L3A,     L4A,     L4B,     L4C,     L4D,     L5A,     L5B,     OP2,     OT4,     OU4,     &
                                         MAX_FIL

      USE IOUNT1, ONLY                :  ANSFIL,  BUGFIL,  EINFIL,  ENFFIL,  ERRFIL,  F04FIL,  F06FIL,  IN0FIL,  INFILE,  NEUFIL,  &
                                         PCHFIL,  SEQFIL,  SPCFIL,                                                                 &
                                         F21FIL,  F22FIL,  F23FIL,  F24FIL,  F25FIL,                                               &
                                         LINK1A,  LINK1B,  LINK1C,  LINK1D,  LINK1E,  LINK1F,  LINK1G,  LINK1H,  LINK1I,  LINK1J,  &
                                         LINK1K,  LINK1L,  LINK1M,  LINK1N,  LINK1O,  LINK1P,  LINK1Q,  LINK1R,  LINK1S,  LINK1T,  &
                                         LINK1U,  LINK1V,  LINK1W,  LINK1X,  LINK1Y,  LINK1Z,                                      &
                                         LINK2A,  LINK2B,  LINK2C,  LINK2D,  LINK2E,  LINK2F,  LINK2G,  LINK2H,  LINK2I,  LINK2J,  &
                                         LINK2K,  LINK2L,  LINK2M,  LINK2N,  LINK2O,  LINK2P,  LINK2Q,  LINK2R,  LINK2S,  LINK2T,  &
                                         LINK3A,  LINK4A,  LINK4B,  LINK4C,  LINK4D,  LINK5A,  LINK5B,  OP2FIL,  OT4FIL,  OU4FIL

      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  FILE_INQUIRE_BEGEND

      USE FILE_INQUIRE_USE_IFs

      IMPLICIT NONE
 
      LOGICAL                         :: LEXIST            ! True if file exists
      LOGICAL                         :: LOPND             ! True if file is opened

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'FILE_INQUIRE'
      CHARACTER(LEN=*), INTENT(IN)    :: MESSAGE           ! Message written when this subr is called
      CHARACTER( 3*BYTE)              :: FIL(100)          ! Descriptor of a MYSTRAN file
      CHARACTER(FILE_NAM_MAXLEN*BYTE) :: FILNAM(100)       ! Filename                     
      CHARACTER(14*BYTE)              :: ANSE              ! Message set based on value of LEXIST
      CHARACTER(10*BYTE)              :: ANSO              ! Message set based on value of LOPND

      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: UNT(100)          ! Unit number of a MYSTRAN file
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = FILE_INQUIRE_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      FIL(  1) = 'SC1'   ;   UNT(  1) =  SC1               ! SC1 - don't need to do INQUIRE on it
      FIL(  2) = 'ANS'   ;   UNT(  2) =  ANS   ;   FILNAM(  2) = ANSFIL
      FIL(  3) = 'BUG'   ;   UNT(  3) =  BUG   ;   FILNAM(  3) = BUGFIL
      FIL(  4) = 'EIN'   ;   UNT(  4) =  EIN   ;   FILNAM(  4) = EINFIL
      FIL(  5) = 'ENF'   ;   UNT(  5) =  ENF   ;   FILNAM(  5) = ENFFIL
      FIL(  6) = 'ERR'   ;   UNT(  6) =  ERR   ;   FILNAM(  6) = ERRFIL
      FIL(  7) = 'F04'   ;   UNT(  7) =  F04   ;   FILNAM(  7) = F04FIL
      FIL(  8) = 'F06'   ;   UNT(  8) =  F06   ;   FILNAM(  8) = F06FIL
      FIL(  9) = 'L1A'   ;   UNT(  9) =  L1A   ;   FILNAM(  9) = LINK1A
      FIL( 10) = 'IN0'   ;   UNT( 10) =  IN0   ;   FILNAM( 10) = IN0FIL
      FIL( 11) = 'NEU'   ;   UNT( 11) =  NEU   ;   FILNAM( 11) = NEUFIL
      FIL( 12) = 'PCH'   ;   UNT( 12) =  NEU   ;   FILNAM( 12) = PCHFIL
      FIL( 13) = 'SEQ'   ;   UNT( 13) =  SEQ   ;   FILNAM( 13) = SEQFIL
      FIL( 14) = 'SPC'   ;   UNT( 14) =  SPC   ;   FILNAM( 14) = SPCFIL
      FIL( 15) = 'F21'   ;   UNT( 15) =  F21   ;   FILNAM( 15) = F21FIL
      FIL( 16) = 'F22'   ;   UNT( 16) =  F22   ;   FILNAM( 16) = F22FIL
      FIL( 17) = 'F23'   ;   UNT( 17) =  F23   ;   FILNAM( 17) = F23FIL
      FIL( 18) = 'F24'   ;   UNT( 18) =  F24   ;   FILNAM( 18) = F24FIL
      FIL( 19) = 'F25'   ;   UNT( 19) =  F25   ;   FILNAM( 19) = F25FIL
      FIL( 20) = 'L1B'   ;   UNT( 20) =  L1B   ;   FILNAM( 20) = LINK1B
      FIL( 21) = 'L1C'   ;   UNT( 21) =  L1C   ;   FILNAM( 21) = LINK1C
      FIL( 22) = 'L1D'   ;   UNT( 22) =  L1D   ;   FILNAM( 22) = LINK1D
      FIL( 23) = 'L1E'   ;   UNT( 23) =  L1E   ;   FILNAM( 23) = LINK1E
      FIL( 24) = 'L1F'   ;   UNT( 24) =  L1F   ;   FILNAM( 24) = LINK1F
      FIL( 25) = 'L1G'   ;   UNT( 25) =  L1G   ;   FILNAM( 25) = LINK1G
      FIL( 26) = 'L1H'   ;   UNT( 26) =  L1H   ;   FILNAM( 26) = LINK1H
      FIL( 27) = 'L1I'   ;   UNT( 27) =  L1I   ;   FILNAM( 27) = LINK1I
      FIL( 28) = 'L1J'   ;   UNT( 28) =  L1J   ;   FILNAM( 28) = LINK1J
      FIL( 29) = 'L1K'   ;   UNT( 29) =  L1K   ;   FILNAM( 29) = LINK1K
      FIL( 30) = 'L1L'   ;   UNT( 30) =  L1L   ;   FILNAM( 30) = LINK1L
      FIL( 31) = 'L1M'   ;   UNT( 31) =  L1M   ;   FILNAM( 31) = LINK1M
      FIL( 32) = 'L1N'   ;   UNT( 32) =  L1N   ;   FILNAM( 32) = LINK1N
      FIL( 33) = 'L1O'   ;   UNT( 33) =  L1O   ;   FILNAM( 33) = LINK1O
      FIL( 34) = 'L1P'   ;   UNT( 34) =  L1P   ;   FILNAM( 34) = LINK1P
      FIL( 35) = 'L1Q'   ;   UNT( 35) =  L1Q   ;   FILNAM( 35) = LINK1Q
      FIL( 36) = 'L1R'   ;   UNT( 36) =  L1R   ;   FILNAM( 36) = LINK1R
      FIL( 37) = 'L1S'   ;   UNT( 37) =  L1S   ;   FILNAM( 37) = LINK1S
      FIL( 38) = 'L1T'   ;   UNT( 38) =  L1T   ;   FILNAM( 38) = LINK1T
      FIL( 39) = 'L1U'   ;   UNT( 39) =  L1U   ;   FILNAM( 39) = LINK1U
      FIL( 40) = 'L1V'   ;   UNT( 40) =  L1V   ;   FILNAM( 40) = LINK1V
      FIL( 41) = 'L1W'   ;   UNT( 41) =  L1W   ;   FILNAM( 41) = LINK1W
      FIL( 42) = 'L1X'   ;   UNT( 42) =  L1X   ;   FILNAM( 42) = LINK1X
      FIL( 43) = 'L1Y'   ;   UNT( 43) =  L1Y   ;   FILNAM( 43) = LINK1Y
      FIL( 44) = 'L1Z'   ;   UNT( 44) =  L1Z   ;   FILNAM( 44) = LINK1Z
      FIL( 45) = 'L2A'   ;   UNT( 45) =  L2A   ;   FILNAM( 45) = LINK2A
      FIL( 46) = 'L2B'   ;   UNT( 46) =  L2B   ;   FILNAM( 46) = LINK2B
      FIL( 47) = 'L2C'   ;   UNT( 47) =  L2C   ;   FILNAM( 47) = LINK2C
      FIL( 48) = 'L2D'   ;   UNT( 48) =  L2D   ;   FILNAM( 48) = LINK2D
      FIL( 49) = 'L2E'   ;   UNT( 49) =  L2E   ;   FILNAM( 49) = LINK2E
      FIL( 50) = 'L2F'   ;   UNT( 50) =  L2F   ;   FILNAM( 50) = LINK2F
      FIL( 51) = 'L2G'   ;   UNT( 51) =  L2G   ;   FILNAM( 51) = LINK2G
      FIL( 52) = 'L2H'   ;   UNT( 52) =  L2H   ;   FILNAM( 52) = LINK2H
      FIL( 53) = 'L2I'   ;   UNT( 53) =  L2I   ;   FILNAM( 53) = LINK2I
      FIL( 54) = 'L2J'   ;   UNT( 54) =  L2J   ;   FILNAM( 54) = LINK2J
      FIL( 55) = 'L2K'   ;   UNT( 55) =  L2K   ;   FILNAM( 55) = LINK2K
      FIL( 56) = 'L2L'   ;   UNT( 56) =  L2L   ;   FILNAM( 56) = LINK2L
      FIL( 57) = 'L2M'   ;   UNT( 57) =  L2M   ;   FILNAM( 57) = LINK2M
      FIL( 58) = 'L2N'   ;   UNT( 58) =  L2N   ;   FILNAM( 58) = LINK2N
      FIL( 59) = 'L2O'   ;   UNT( 59) =  L2O   ;   FILNAM( 59) = LINK2O
      FIL( 60) = 'L2P'   ;   UNT( 60) =  L2P   ;   FILNAM( 60) = LINK2P
      FIL( 61) = 'L2Q'   ;   UNT( 61) =  L2Q   ;   FILNAM( 61) = LINK2Q
      FIL( 62) = 'L2R'   ;   UNT( 62) =  L2R   ;   FILNAM( 62) = LINK2R
      FIL( 63) = 'L2S'   ;   UNT( 63) =  L2S   ;   FILNAM( 63) = LINK2S
      FIL( 64) = 'L2T'   ;   UNT( 64) =  L2T   ;   FILNAM( 64) = LINK2T
      FIL( 65) = 'L3A'   ;   UNT( 65) =  L3A   ;   FILNAM( 65) = LINK3A
      FIL( 66) = 'L4A'   ;   UNT( 66) =  L4A   ;   FILNAM( 66) = LINK4A
      FIL( 67) = 'L4B'   ;   UNT( 67) =  L4B   ;   FILNAM( 67) = LINK4B
      FIL( 68) = 'L4C'   ;   UNT( 68) =  L4C   ;   FILNAM( 68) = LINK4C
      FIL( 69) = 'L4D'   ;   UNT( 69) =  L4D   ;   FILNAM( 69) = LINK4D
      FIL( 70) = 'L5A'   ;   UNT( 70) =  L5A   ;   FILNAM( 70) = LINK5A
      FIL( 71) = 'L5B'   ;   UNT( 71) =  L5B   ;   FILNAM( 71) = LINK5B
      FIL( 72) = 'OP2'   ;   UNT( 72) =  OP2   ;   FILNAM( 71) = OP2FIL

      IF ( 72 > MAX_FIL) THEN
         WRITE(ERR,944) SUBR_NAME, MAX_FIL   
         WRITE(F06,944) SUBR_NAME, MAX_FIL   
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )
      ENDIF 

      DO I=1,MOU4
         FIL(MAX_FIL+I)      = OU4_EXT(I);   UNT(MAX_FIL+I)      =  OU4(I)   ;   FILNAM(MAX_FIL+I)      = OU4FIL(I)
      ENDDO
      DO I=1,MOT4
         FIL(MAX_FIL+MOU4+I) = OT4_EXT(I);   UNT(MAX_FIL+MOU4+I) =  OT4(I)   ;   FILNAM(MAX_FIL+MOU4+I) = OT4FIL(I)
      ENDDO

      WRITE(ERR,1)
      WRITE(ERR,2) MESSAGE 

      WRITE(F06,1)
      WRITE(F06,2) MESSAGE 

      DO I=2,MAX_FIL+MOU4+MOT4                                  ! Start at 2 since SC1 was 1 and we do not do INQUIRE on it
         IF (UNT(I) /= 0) THEN
           INQUIRE(FILE=FILNAM(I),EXIST=LEXIST,OPENED=LOPND)
           IF (LEXIST) THEN
               ANSE = 'exists'
            ELSE
               ANSE = 'does not exist'
            ENDIF
            IF (LOPND) THEN
               ANSO = '    opened'
            ELSE
               ANSO = 'not opened'
            ENDIF
            WRITE(ERR,2999) FIL(I), UNT(I), ANSE, ANSO, FILNAM(I)
            WRITE(F06,2999) FIL(I), UNT(I), ANSE, ANSO, FILNAM(I)
         ENDIF
      ENDDO 

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
    1 FORMAT(' *******************************************************************************************************************')

    2 FORMAT(' Messages from subr FILE_INQUIRE on whether files are opened ',A)

  944 FORMAT(' *ERROR   944: PROGRAMMING ERROR IN SUBROUTINE ',A,/,                                                                &
                        14X,'ATTEMPT TO EXCEED MAX_FIL = ',I4,' NUMBER OF FILES IN WRITING FILE STATUS')

 2999 FORMAT(1X,A3,' on unit ',I4,1X,A14,' and is ',A10,' with file name = ',A)

! **********************************************************************************************************************************

      END SUBROUTINE FILE_INQUIRE






































































