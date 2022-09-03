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
 
      SUBROUTINE CHK_CC_CMD_DESCRIBERS ( WHAT, NUM_WORDS )
 
! Checks Case Control output requests to make sure the descriptors in parens (e.g. SORT1, PRINT, etc) are valid. Write warning
! messages if a descriptor is not valid for MYSTRAN

      USE PENTIUM_II_KIND, ONLY        :  BYTE, LONG
      USE IOUNT1, ONLY                 :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                 :  BLNK_SUB_NAM, CC_CMD_DESCRIBERS, ECHO, FATAL_ERR, WARN_ERR
      USE TIMDAT, ONLY                 :  TSEC
      USE CC_OUTPUT_DESCRIBERS, ONLY   :  STRN_LOC, STRN_OPT, STRE_LOC, STRE_OPT
      USE PARAMS, ONLY                 :  SUPWARN 
      USE SUBR_BEGEND_LEVELS, ONLY     :  CHK_CC_CMD_DESCRIBERS_BEGEND

      USE CHK_CC_CMD_DESCRIBERS_USE_IFs

      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER         :: NUM_POSS_CCD = 31 ! Number of possible CC command describers (incl all MSC ones as well)
      INTEGER(LONG), PARAMETER         :: NUM_OUT_TYP  =  9 ! Number of OUTPUT_TYPE's

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)) :: SUBR_NAME = 'CHK_CC_CMD_DESCRIBERS'
      CHARACTER(LEN=*), INTENT(IN)     :: WHAT              ! What Case Control output is this call for (e.g. 'DISP')

                                                            ! OUTPUT_TYPE is 'ACCE', 'DISP', etc
      CHARACTER( 4*BYTE)               :: OUTPUT_TYPE(NUM_OUT_TYP)

                                                            ! List of all of the possible values for a MSC NASTRAN CC_CMD_DESCRIBERS
      CHARACTER(LEN(CC_CMD_DESCRIBERS)):: ALLOW_CC_CMD_DESCR(NUM_POSS_CCD,NUM_OUT_TYP)

      CHARACTER( 1*BYTE)               :: FOUND             ! ='Y' if we found something we are looking for in an IF test

      INTEGER(LONG), INTENT(IN)        :: NUM_WORDS         ! Number of words we need to check in CC_CMD_DESCRIBERS
      INTEGER(LONG)                    :: I,J               ! DO loop indices
      INTEGER(LONG)                    :: JCOL              ! Designator of a column in an array
      INTEGER(LONG), PARAMETER         :: SUBR_BEGEND = CHK_CC_CMD_DESCRIBERS_BEGEND
 
! *********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGIN',F10.3)
      ENDIF

! **********************************************************************************************************************************
      IF      (WHAT == 'ACCE') THEN   ;   OUTPUT_TYPE( 1) = 'ACCE'   ;   JCOL =  1  ;
      ELSE IF (WHAT == 'DISP') THEN   ;   OUTPUT_TYPE( 2) = 'DISP'   ;   JCOL =  2  ;
      ELSE IF (WHAT == 'ELFO') THEN   ;   OUTPUT_TYPE( 3) = 'ELFO'   ;   JCOL =  3  ;
      ELSE IF (WHAT == 'GPFO') THEN   ;   OUTPUT_TYPE( 4) = 'GPFO'   ;   JCOL =  4  ;
      ELSE IF (WHAT == 'MPCF') THEN   ;   OUTPUT_TYPE( 5) = 'MPCF'   ;   JCOL =  5  ;
      ELSE IF (WHAT == 'OLOA') THEN   ;   OUTPUT_TYPE( 6) = 'OLOA'   ;   JCOL =  6  ;
      ELSE IF (WHAT == 'SPCF') THEN   ;   OUTPUT_TYPE( 7) = 'SPCF'   ;   JCOL =  7  ;
      ELSE IF (WHAT == 'STRE') THEN   ;   OUTPUT_TYPE( 8) = 'STRE'   ;   JCOL =  8  ;
      ELSE IF (WHAT == 'STRN') THEN   ;   OUTPUT_TYPE( 9) = 'STRN'   ;   JCOL =  9  ;
      ELSE
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1204) SUBR_NAME, WHAT
         WRITE(F06,1204) SUBR_NAME, WHAT
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! Set all of the allowable values that can be in ALLOW_CC_CMD_DESCR. These are all of the values from MSC NASTRAN. Not all are
! implemented in MYSTRAN

!     =================ACCE=================   =================DISP=================   =================ELFO=================
      ALLOW_CC_CMD_DESCR( 1, 1) = 'SORT1   ' ; ALLOW_CC_CMD_DESCR( 1, 2) = 'SORT1   ' ; ALLOW_CC_CMD_DESCR( 1, 3) = 'SORT1   '
      ALLOW_CC_CMD_DESCR( 2, 1) = 'SORT2   ' ; ALLOW_CC_CMD_DESCR( 2, 2) = 'SORT2   ' ; ALLOW_CC_CMD_DESCR( 2, 3) = 'SORT2   '
      ALLOW_CC_CMD_DESCR( 3, 1) = 'PRINT   ' ; ALLOW_CC_CMD_DESCR( 3, 2) = 'PRINT   ' ; ALLOW_CC_CMD_DESCR( 3, 3) = 'PRINT   '
      ALLOW_CC_CMD_DESCR( 4, 1) = 'PUNCH   ' ; ALLOW_CC_CMD_DESCR( 4, 2) = 'PUNCH   ' ; ALLOW_CC_CMD_DESCR( 4, 3) = 'PUNCH   '
      ALLOW_CC_CMD_DESCR( 5, 1) = 'PLOT    ' ; ALLOW_CC_CMD_DESCR( 5, 2) = 'PLOT    ' ; ALLOW_CC_CMD_DESCR( 5, 3) = 'PLOT    '
      ALLOW_CC_CMD_DESCR( 6, 1) = 'REAL    ' ; ALLOW_CC_CMD_DESCR( 6, 2) = 'REAL    ' ; ALLOW_CC_CMD_DESCR( 6, 3) = 'REAL    '
      ALLOW_CC_CMD_DESCR( 7, 1) = 'IMAG    ' ; ALLOW_CC_CMD_DESCR( 7, 2) = 'IMAG    ' ; ALLOW_CC_CMD_DESCR( 7, 3) = 'IMAG    '
      ALLOW_CC_CMD_DESCR( 8, 1) = 'MAG     ' ; ALLOW_CC_CMD_DESCR( 8, 2) = 'MAG     ' ; ALLOW_CC_CMD_DESCR( 8, 3) = 'MAG     '
      ALLOW_CC_CMD_DESCR( 9, 1) = 'PHASE   ' ; ALLOW_CC_CMD_DESCR( 9, 2) = 'PHASE   ' ; ALLOW_CC_CMD_DESCR( 9, 3) = 'PHASE   '
      ALLOW_CC_CMD_DESCR(10, 1) = '        ' ; ALLOW_CC_CMD_DESCR(10, 2) = '        ' ; ALLOW_CC_CMD_DESCR(10, 3) = '        '
      ALLOW_CC_CMD_DESCR(11, 1) = '        ' ; ALLOW_CC_CMD_DESCR(11, 2) = '        ' ; ALLOW_CC_CMD_DESCR(11, 3) = '        '
      ALLOW_CC_CMD_DESCR(12, 1) = '        ' ; ALLOW_CC_CMD_DESCR(12, 2) = '        ' ; ALLOW_CC_CMD_DESCR(12, 3) = '        '
      ALLOW_CC_CMD_DESCR(13, 1) = '        ' ; ALLOW_CC_CMD_DESCR(13, 2) = '        ' ; ALLOW_CC_CMD_DESCR(13, 3) = '        '
      ALLOW_CC_CMD_DESCR(14, 1) = '        ' ; ALLOW_CC_CMD_DESCR(14, 2) = '        ' ; ALLOW_CC_CMD_DESCR(14, 3) = '        '
      ALLOW_CC_CMD_DESCR(15, 1) = '        ' ; ALLOW_CC_CMD_DESCR(15, 2) = '        ' ; ALLOW_CC_CMD_DESCR(15, 3) = '        '
      ALLOW_CC_CMD_DESCR(16, 1) = '        ' ; ALLOW_CC_CMD_DESCR(16, 2) = '        ' ; ALLOW_CC_CMD_DESCR(16, 3) = '        '
      ALLOW_CC_CMD_DESCR(17, 1) = '        ' ; ALLOW_CC_CMD_DESCR(17, 2) = '        ' ; ALLOW_CC_CMD_DESCR(17, 3) = '        '
      ALLOW_CC_CMD_DESCR(18, 1) = '        ' ; ALLOW_CC_CMD_DESCR(18, 2) = '        ' ; ALLOW_CC_CMD_DESCR(18, 3) = '        '
      ALLOW_CC_CMD_DESCR(19, 1) = '        ' ; ALLOW_CC_CMD_DESCR(19, 2) = '        ' ; ALLOW_CC_CMD_DESCR(19, 3) = '        '
      ALLOW_CC_CMD_DESCR(20, 1) = 'PSDF    ' ; ALLOW_CC_CMD_DESCR(20, 2) = 'PSDF    ' ; ALLOW_CC_CMD_DESCR(20, 3) = 'PSDF    '
      ALLOW_CC_CMD_DESCR(21, 1) = 'ATOC    ' ; ALLOW_CC_CMD_DESCR(21, 2) = 'ATOC    ' ; ALLOW_CC_CMD_DESCR(21, 3) = 'ATOC    '
      ALLOW_CC_CMD_DESCR(22, 1) = 'CRMS    ' ; ALLOW_CC_CMD_DESCR(22, 2) = 'CRMS    ' ; ALLOW_CC_CMD_DESCR(22, 3) = 'CRMS    '
      ALLOW_CC_CMD_DESCR(23, 1) = 'RALL    ' ; ALLOW_CC_CMD_DESCR(23, 2) = 'RALL    ' ; ALLOW_CC_CMD_DESCR(23, 3) = 'RALL    '
      ALLOW_CC_CMD_DESCR(24, 1) = 'RPRINT  ' ; ALLOW_CC_CMD_DESCR(24, 2) = 'RPRINT  ' ; ALLOW_CC_CMD_DESCR(24, 3) = 'RPRINT  '
      ALLOW_CC_CMD_DESCR(25, 1) = 'RPUNCH  ' ; ALLOW_CC_CMD_DESCR(25, 2) = 'RPUNCH  ' ; ALLOW_CC_CMD_DESCR(25, 3) = 'RPUNCH  '
      ALLOW_CC_CMD_DESCR(26, 1) = 'NOPRINT ' ; ALLOW_CC_CMD_DESCR(26, 2) = 'NOPRINT ' ; ALLOW_CC_CMD_DESCR(26, 3) = 'NOPRINT '
      ALLOW_CC_CMD_DESCR(27, 1) = 'RPUNCH  ' ; ALLOW_CC_CMD_DESCR(27, 2) = 'RPUNCH  ' ; ALLOW_CC_CMD_DESCR(27, 3) = 'RPUNCH  '
      ALLOW_CC_CMD_DESCR(28, 1) = 'CID     ' ; ALLOW_CC_CMD_DESCR(28, 2) = 'CID     ' ; ALLOW_CC_CMD_DESCR(28, 3) = 'CID     '
      ALLOW_CC_CMD_DESCR(29, 1) = '        ' ; ALLOW_CC_CMD_DESCR(29, 2) = '        ' ; ALLOW_CC_CMD_DESCR(29, 3) = 'BOTH    '
      ALLOW_CC_CMD_DESCR(30, 1) = '        ' ; ALLOW_CC_CMD_DESCR(30, 2) = '        ' ; ALLOW_CC_CMD_DESCR(30, 3) = 'ENGR    '
      ALLOW_CC_CMD_DESCR(31, 1) = '        ' ; ALLOW_CC_CMD_DESCR(31, 2) = '        ' ; ALLOW_CC_CMD_DESCR(31, 3) = 'NODE    '

!     =================GPFO=================   =================MPCF=================   =================OLOA=================
      ALLOW_CC_CMD_DESCR( 1, 4) = '        ' ; ALLOW_CC_CMD_DESCR( 1, 5) = 'SORT1   ' ; ALLOW_CC_CMD_DESCR( 1, 6) = 'SORT1   '
      ALLOW_CC_CMD_DESCR( 2, 4) = '        ' ; ALLOW_CC_CMD_DESCR( 2, 5) = 'SORT2   ' ; ALLOW_CC_CMD_DESCR( 2, 6) = 'SORT2   '
      ALLOW_CC_CMD_DESCR( 3, 4) = 'PRINT   ' ; ALLOW_CC_CMD_DESCR( 3, 5) = 'PRINT   ' ; ALLOW_CC_CMD_DESCR( 3, 6) = 'PRINT   '
      ALLOW_CC_CMD_DESCR( 4, 4) = 'PUNCH   ' ; ALLOW_CC_CMD_DESCR( 4, 5) = 'PUNCH   ' ; ALLOW_CC_CMD_DESCR( 4, 6) = 'PUNCH   '
      ALLOW_CC_CMD_DESCR( 5, 4) = 'PLOT    ' ; ALLOW_CC_CMD_DESCR( 5, 5) = 'PLOT    ' ; ALLOW_CC_CMD_DESCR( 5, 6) = 'PLOT    '
      ALLOW_CC_CMD_DESCR( 6, 4) = '        ' ; ALLOW_CC_CMD_DESCR( 6, 5) = 'REAL    ' ; ALLOW_CC_CMD_DESCR( 6, 6) = 'REAL    '
      ALLOW_CC_CMD_DESCR( 7, 4) = '        ' ; ALLOW_CC_CMD_DESCR( 7, 5) = 'IMAG    ' ; ALLOW_CC_CMD_DESCR( 7, 6) = 'IMAG    '
      ALLOW_CC_CMD_DESCR( 8, 4) = '        ' ; ALLOW_CC_CMD_DESCR( 8, 5) = 'MAG     ' ; ALLOW_CC_CMD_DESCR( 8, 6) = 'MAG     '
      ALLOW_CC_CMD_DESCR( 9, 4) = '        ' ; ALLOW_CC_CMD_DESCR( 9, 5) = 'PHASE   ' ; ALLOW_CC_CMD_DESCR( 9, 6) = 'PHASE   '
      ALLOW_CC_CMD_DESCR(10, 4) = '        ' ; ALLOW_CC_CMD_DESCR(10, 5) = '        ' ; ALLOW_CC_CMD_DESCR(10, 6) = '        '
      ALLOW_CC_CMD_DESCR(11, 4) = '        ' ; ALLOW_CC_CMD_DESCR(11, 5) = '        ' ; ALLOW_CC_CMD_DESCR(11, 6) = '        '
      ALLOW_CC_CMD_DESCR(12, 4) = '        ' ; ALLOW_CC_CMD_DESCR(12, 5) = '        ' ; ALLOW_CC_CMD_DESCR(12, 6) = '        '
      ALLOW_CC_CMD_DESCR(13, 4) = '        ' ; ALLOW_CC_CMD_DESCR(13, 5) = '        ' ; ALLOW_CC_CMD_DESCR(13, 6) = '        '
      ALLOW_CC_CMD_DESCR(14, 4) = '        ' ; ALLOW_CC_CMD_DESCR(14, 5) = '        ' ; ALLOW_CC_CMD_DESCR(14, 6) = '        '
      ALLOW_CC_CMD_DESCR(15, 4) = '        ' ; ALLOW_CC_CMD_DESCR(15, 5) = '        ' ; ALLOW_CC_CMD_DESCR(15, 6) = '        '
      ALLOW_CC_CMD_DESCR(16, 4) = '        ' ; ALLOW_CC_CMD_DESCR(16, 5) = '        ' ; ALLOW_CC_CMD_DESCR(16, 6) = '        '
      ALLOW_CC_CMD_DESCR(17, 4) = '        ' ; ALLOW_CC_CMD_DESCR(17, 5) = '        ' ; ALLOW_CC_CMD_DESCR(17, 6) = '        '
      ALLOW_CC_CMD_DESCR(18, 4) = '        ' ; ALLOW_CC_CMD_DESCR(18, 5) = '        ' ; ALLOW_CC_CMD_DESCR(18, 6) = '        '
      ALLOW_CC_CMD_DESCR(19, 4) = '        ' ; ALLOW_CC_CMD_DESCR(19, 5) = '        ' ; ALLOW_CC_CMD_DESCR(19, 6) = '        '
      ALLOW_CC_CMD_DESCR(20, 4) = '        ' ; ALLOW_CC_CMD_DESCR(20, 5) = 'PSDF    ' ; ALLOW_CC_CMD_DESCR(20, 6) = 'PSDF    '
      ALLOW_CC_CMD_DESCR(21, 4) = '        ' ; ALLOW_CC_CMD_DESCR(21, 5) = 'ATOC    ' ; ALLOW_CC_CMD_DESCR(21, 6) = 'ATOC    '
      ALLOW_CC_CMD_DESCR(22, 4) = '        ' ; ALLOW_CC_CMD_DESCR(22, 5) = 'CRMS    ' ; ALLOW_CC_CMD_DESCR(22, 6) = 'CRMS    '
      ALLOW_CC_CMD_DESCR(23, 4) = '        ' ; ALLOW_CC_CMD_DESCR(23, 5) = 'RALL    ' ; ALLOW_CC_CMD_DESCR(23, 6) = 'RALL    '
      ALLOW_CC_CMD_DESCR(24, 4) = '        ' ; ALLOW_CC_CMD_DESCR(24, 5) = 'RPRINT  ' ; ALLOW_CC_CMD_DESCR(24, 6) = 'RPRINT  '
      ALLOW_CC_CMD_DESCR(25, 4) = '        ' ; ALLOW_CC_CMD_DESCR(25, 5) = 'RPUNCH  ' ; ALLOW_CC_CMD_DESCR(25, 6) = 'RPUNCH  '
      ALLOW_CC_CMD_DESCR(26, 4) = '        ' ; ALLOW_CC_CMD_DESCR(26, 5) = 'NOPRINT ' ; ALLOW_CC_CMD_DESCR(26, 6) = 'NOPRINT '
      ALLOW_CC_CMD_DESCR(27, 4) = '        ' ; ALLOW_CC_CMD_DESCR(27, 5) = 'RPUNCH  ' ; ALLOW_CC_CMD_DESCR(27, 6) = 'RPUNCH  '
      ALLOW_CC_CMD_DESCR(28, 4) = '        ' ; ALLOW_CC_CMD_DESCR(28, 5) = 'CID     ' ; ALLOW_CC_CMD_DESCR(28, 6) = 'CID     '
      ALLOW_CC_CMD_DESCR(29, 4) = '        ' ; ALLOW_CC_CMD_DESCR(29, 5) = '        ' ; ALLOW_CC_CMD_DESCR(29, 6) = '        '
      ALLOW_CC_CMD_DESCR(30, 4) = '        ' ; ALLOW_CC_CMD_DESCR(30, 5) = '        ' ; ALLOW_CC_CMD_DESCR(30, 6) = '        '
      ALLOW_CC_CMD_DESCR(31, 4) = '        ' ; ALLOW_CC_CMD_DESCR(31, 5) = '        ' ; ALLOW_CC_CMD_DESCR(31, 6) = '        '

!     =================SPCF=================   =================STRE================= ; =================STRN=================
      ALLOW_CC_CMD_DESCR( 1, 7) = 'SORT1   ' ; ALLOW_CC_CMD_DESCR( 1, 8) = 'SORT1   ' ; ALLOW_CC_CMD_DESCR( 1, 9) = 'SORT1   '
      ALLOW_CC_CMD_DESCR( 2, 7) = 'SORT2   ' ; ALLOW_CC_CMD_DESCR( 2, 8) = 'SORT2   ' ; ALLOW_CC_CMD_DESCR( 2, 9) = 'SORT2   '
      ALLOW_CC_CMD_DESCR( 3, 7) = 'PRINT   ' ; ALLOW_CC_CMD_DESCR( 3, 8) = 'PRINT   ' ; ALLOW_CC_CMD_DESCR( 3, 9) = 'PRINT   '
      ALLOW_CC_CMD_DESCR( 4, 7) = 'PUNCH   ' ; ALLOW_CC_CMD_DESCR( 4, 8) = 'PUNCH   ' ; ALLOW_CC_CMD_DESCR( 4, 9) = 'PUNCH   '
      ALLOW_CC_CMD_DESCR( 5, 7) = 'PLOT    ' ; ALLOW_CC_CMD_DESCR( 5, 8) = 'PLOT    ' ; ALLOW_CC_CMD_DESCR( 5, 9) = 'PLOT    '
      ALLOW_CC_CMD_DESCR( 6, 7) = 'REAL    ' ; ALLOW_CC_CMD_DESCR( 6, 8) = 'REAL    ' ; ALLOW_CC_CMD_DESCR( 6, 9) = 'REAL    '
      ALLOW_CC_CMD_DESCR( 7, 7) = 'IMAG    ' ; ALLOW_CC_CMD_DESCR( 7, 8) = 'IMAG    ' ; ALLOW_CC_CMD_DESCR( 7, 9) = 'IMAG    '
      ALLOW_CC_CMD_DESCR( 8, 7) = 'MAG     ' ; ALLOW_CC_CMD_DESCR( 8, 8) = 'MAG     ' ; ALLOW_CC_CMD_DESCR( 8, 9) = 'MAG     '
      ALLOW_CC_CMD_DESCR( 9, 7) = 'PHASE   ' ; ALLOW_CC_CMD_DESCR( 9, 8) = 'PHASE   ' ; ALLOW_CC_CMD_DESCR( 9, 9) = 'PHASE   '
      ALLOW_CC_CMD_DESCR(10, 7) = '        ' ; ALLOW_CC_CMD_DESCR(10, 8) = 'VONMISES' ; ALLOW_CC_CMD_DESCR(10, 9) = 'VONMISES'
      ALLOW_CC_CMD_DESCR(11, 7) = '        ' ; ALLOW_CC_CMD_DESCR(11, 8) = 'MAXS    ' ; ALLOW_CC_CMD_DESCR(11, 9) = 'MAXS    '
      ALLOW_CC_CMD_DESCR(12, 7) = '        ' ; ALLOW_CC_CMD_DESCR(12, 8) = 'SHEAR   ' ; ALLOW_CC_CMD_DESCR(12, 9) = 'SHEAR   '
      ALLOW_CC_CMD_DESCR(13, 7) = '        ' ; ALLOW_CC_CMD_DESCR(13, 8) = 'STRCUR  ' ; ALLOW_CC_CMD_DESCR(13, 9) = 'STRCUR  '
      ALLOW_CC_CMD_DESCR(14, 7) = '        ' ; ALLOW_CC_CMD_DESCR(14, 8) = 'FIBER   ' ; ALLOW_CC_CMD_DESCR(14, 9) = 'FIBER   '
      ALLOW_CC_CMD_DESCR(15, 7) = '        ' ; ALLOW_CC_CMD_DESCR(15, 8) = 'CENTER  ' ; ALLOW_CC_CMD_DESCR(15, 9) = 'CENTER  '
      ALLOW_CC_CMD_DESCR(16, 7) = '        ' ; ALLOW_CC_CMD_DESCR(16, 8) = 'CORNER  ' ; ALLOW_CC_CMD_DESCR(16, 9) = 'CORNER  '
      ALLOW_CC_CMD_DESCR(17, 7) = '        ' ; ALLOW_CC_CMD_DESCR(17, 8) = 'BILIN   ' ; ALLOW_CC_CMD_DESCR(17, 9) = 'BILIN   '
      ALLOW_CC_CMD_DESCR(18, 7) = '        ' ; ALLOW_CC_CMD_DESCR(18, 8) = 'SGAGE   ' ; ALLOW_CC_CMD_DESCR(18, 9) = 'SGAGE   '
      ALLOW_CC_CMD_DESCR(19, 7) = '        ' ; ALLOW_CC_CMD_DESCR(19, 8) = 'CUBIC   ' ; ALLOW_CC_CMD_DESCR(19, 9) = 'CUBIC   '
      ALLOW_CC_CMD_DESCR(20, 7) = 'PSDF    ' ; ALLOW_CC_CMD_DESCR(20, 8) = 'PSDF    ' ; ALLOW_CC_CMD_DESCR(20, 9) = 'PSDF    '
      ALLOW_CC_CMD_DESCR(21, 7) = 'ATOC    ' ; ALLOW_CC_CMD_DESCR(21, 8) = 'ATOC    ' ; ALLOW_CC_CMD_DESCR(21, 9) = 'ATOC    '
      ALLOW_CC_CMD_DESCR(22, 7) = 'CRMS    ' ; ALLOW_CC_CMD_DESCR(22, 8) = 'CRMS    ' ; ALLOW_CC_CMD_DESCR(22, 9) = 'CRMS    '
      ALLOW_CC_CMD_DESCR(23, 7) = 'RALL    ' ; ALLOW_CC_CMD_DESCR(23, 8) = 'RALL    ' ; ALLOW_CC_CMD_DESCR(23, 9) = 'RALL    '
      ALLOW_CC_CMD_DESCR(24, 7) = 'RPRINT  ' ; ALLOW_CC_CMD_DESCR(24, 8) = 'RPRINT  ' ; ALLOW_CC_CMD_DESCR(24, 9) = 'RPRINT  '
      ALLOW_CC_CMD_DESCR(25, 7) = 'RPUNCH  ' ; ALLOW_CC_CMD_DESCR(25, 8) = 'RPUNCH  ' ; ALLOW_CC_CMD_DESCR(25, 9) = 'RPUNCH  '
      ALLOW_CC_CMD_DESCR(26, 7) = 'NOPRINT ' ; ALLOW_CC_CMD_DESCR(26, 8) = 'NOPRINT ' ; ALLOW_CC_CMD_DESCR(26, 9) = 'NOPRINT '
      ALLOW_CC_CMD_DESCR(27, 7) = 'RPUNCH  ' ; ALLOW_CC_CMD_DESCR(27, 8) = 'RPUNCH  ' ; ALLOW_CC_CMD_DESCR(27, 9) = 'RPUNCH  '
      ALLOW_CC_CMD_DESCR(28, 7) = 'CID     ' ; ALLOW_CC_CMD_DESCR(28, 8) = '        ' ; ALLOW_CC_CMD_DESCR(28, 9) = '        '
      ALLOW_CC_CMD_DESCR(29, 7) = '        ' ; ALLOW_CC_CMD_DESCR(29, 8) = '        ' ; ALLOW_CC_CMD_DESCR(29, 9) = '        '
      ALLOW_CC_CMD_DESCR(30, 7) = '        ' ; ALLOW_CC_CMD_DESCR(30, 8) = '        ' ; ALLOW_CC_CMD_DESCR(30, 9) = '        '
      ALLOW_CC_CMD_DESCR(31, 7) = '        ' ; ALLOW_CC_CMD_DESCR(31, 8) = '        ' ; ALLOW_CC_CMD_DESCR(31, 9) = '        '

! Check that every word that is in CC_CMD_DESCRIBERS is a member of ALLOW_CC_CMD_DESCR.

ido_1:DO I=1,NUM_WORDS
jdo_1:   DO J=1,NUM_POSS_CCD
            FOUND = 'N'
            IF (CC_CMD_DESCRIBERS(I) == ALLOW_CC_CMD_DESCR(J,JCOL)) THEN
               FOUND = 'Y'
               EXIT jdo_1
            ENDIF
         ENDDO jdo_1
         IF (FOUND == 'N') THEN
            WARN_ERR = WARN_ERR + 1
            WRITE(ERR,101) CC_CMD_DESCRIBERS(I), OUTPUT_TYPE(JCOL)
            IF (SUPWARN == 'N') THEN
               IF (ECHO == 'NONE  ') THEN
                  WRITE(F06,101) CC_CMD_DESCRIBERS(I), OUTPUT_TYPE(JCOL)
               ENDIF
            ENDIF
         ENDIF
      ENDDO ido_1
         
      DO I=1,NUM_WORDS

         IF (CC_CMD_DESCRIBERS(I)(1:5) == 'SORT2') THEN
            WARN_ERR = WARN_ERR + 1
            WRITE(ERR,201)
            IF (SUPWARN == 'N') THEN
               IF (ECHO == 'NONE  ') THEN
                  WRITE(F06,201)
               ENDIF
            ENDIF
         ENDIF

         IF (CC_CMD_DESCRIBERS(I)(1:5) == 'PUNCH') THEN
            WARN_ERR = WARN_ERR + 1
            WRITE(ERR,202)
            IF (SUPWARN == 'N') THEN
               IF (ECHO == 'NONE  ') THEN
                  WRITE(F06,202)
               ENDIF
            ENDIF
         ENDIF

         IF (CC_CMD_DESCRIBERS(I)(1:4) == 'PLOT') THEN
            WARN_ERR = WARN_ERR + 1
            WRITE(ERR,203)
            IF (SUPWARN == 'N') THEN
               IF (ECHO == 'NONE  ') THEN
                  WRITE(F06,203)
               ENDIF
            ENDIF
         ENDIF

      ENDDO

      IF ((WHAT == 'STRE') .OR. (WHAT == 'STRN')) THEN

         DO I=1,NUM_WORDS

            IF (CC_CMD_DESCRIBERS(I)(1:5) == 'STRCUR') THEN
               WARN_ERR = WARN_ERR + 1
               WRITE(ERR,301)
               IF (SUPWARN == 'N') THEN
                  IF (ECHO == 'NONE  ') THEN
                     WRITE(F06,301)
                  ENDIF
               ENDIF
            ENDIF

            IF (CC_CMD_DESCRIBERS(I)(1:5) == 'SGAGE') THEN
               WARN_ERR = WARN_ERR + 1
               WRITE(ERR,302)
               IF (SUPWARN == 'N') THEN
                  IF (ECHO == 'NONE  ') THEN
                     WRITE(F06,302)
                  ENDIF
               ENDIF
            ENDIF      

            IF (CC_CMD_DESCRIBERS(I)(1:5) == 'CUBIC') THEN
               WARN_ERR = WARN_ERR + 1
               WRITE(ERR,303)
               IF (SUPWARN == 'N') THEN
                  IF (ECHO == 'NONE  ') THEN
                     WRITE(F06,303)
                  ENDIF
               ENDIF
            ENDIF      

         ENDDO

      ENDIF

! Set values for variables in module CC_OUTPUT_DESCRIBERS

      IF (WHAT == 'STRE' ) THEN

         DO I=1,NUM_WORDS

            IF      (CC_CMD_DESCRIBERS(I)(1:6) == 'CENTER') THEN
               STRE_LOC = 'CENTER'
            ELSE IF (CC_CMD_DESCRIBERS(I)(1:6) == 'CORNER') THEN
               STRE_LOC = 'CORNER'
            ELSE IF (CC_CMD_DESCRIBERS(I)(1:5) == 'BILIN' ) THEN
               STRE_LOC = 'CORNER'
            ENDIF

            IF      (CC_CMD_DESCRIBERS(I)(1:8) == 'VONMISES') THEN
               STRE_OPT = 'VONMISES'
            ELSE IF (CC_CMD_DESCRIBERS(I)(1:4) == 'MAXS'    ) THEN
               STRE_OPT = 'MAXS'
            ELSE IF (CC_CMD_DESCRIBERS(I)(1:5) == 'SHEAR'   ) THEN
               STRE_OPT = 'SHEAR'
            ENDIF

         ENDDO

      ENDIF

      IF (WHAT == 'STRN' ) THEN

         DO I=1,NUM_WORDS

            IF      (CC_CMD_DESCRIBERS(I)(1:6) == 'CENTER') THEN
               STRN_LOC = 'CENTER'
            ELSE IF (CC_CMD_DESCRIBERS(I)(1:6) == 'CORNER') THEN
               STRN_LOC = 'CORNER'
            ELSE IF (CC_CMD_DESCRIBERS(I)(1:5) == 'BILIN' ) THEN
               STRN_LOC = 'CORNER'
            ENDIF

            IF      (CC_CMD_DESCRIBERS(I)(1:8) == 'VONMISES') THEN
               STRN_OPT = 'VONMISES'
            ELSE IF (CC_CMD_DESCRIBERS(I)(1:4) == 'MAXS'    ) THEN
               STRN_OPT = 'MAXS'
            ELSE IF (CC_CMD_DESCRIBERS(I)(1:5) == 'SHEAR'   ) THEN
               STRN_OPT = 'SHEAR'
            ENDIF

         ENDDO

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(' *WARNING    : "',A,'" IS NOT A RECOGNIZED CASE CONTROL COMMAND OPTION FOR OUTPUT TYPE "',A,'". OPTION IGNORED')

  201 FORMAT(' *WARNING    : "SORT2"    IS NOT AN OPTION IN MYSTRAN. "SORT1" WILL BE USED')

  202 FORMAT(' *WARNING    : "PUNCH"    IS NOT AN OPTION IN MYSTRAN. "PRINT" WILL BE USED')

  203 FORMAT(' *WARNING    : "PLOT"     IS NOT AN OPTION IN MYSTRAN. "PLOT"   IGNORED')

  301 FORMAT(' *WARNING    : "STRCUR"   IS NOT AN OPTION IN MYSTRAN. "STRCUR" IGNORED')

  302 FORMAT(' *WARNING    : "SGAGE"    IS NOT AN OPTION IN MYSTRAN. "SGAGE"  IGNORED')

  303 FORMAT(' *WARNING    : "CUBIC"    IS NOT AN OPTION IN MYSTRAN. "CUBIC"  IGNORED')

 1204 FORMAT(' *ERROR  1205: PROGRAMMING ERROR IN SUBROUTINE ',A,'. INVALID VALUE ',A,' FOR VARIABLE "WHAT"')

! **********************************************************************************************************************************

      END SUBROUTINE CHK_CC_CMD_DESCRIBERS

