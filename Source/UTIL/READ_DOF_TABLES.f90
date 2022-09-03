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

      SUBROUTINE READ_DOF_TABLES

! Reads DOF table data (TSET, TDOF, TDOFI) from file LINK1C

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  FILE_NAM_MAXLEN, WRT_ERR, WRT_LOG, ERR, F04, F06, L1C, LINK1C, L1C_MSG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, DATA_NAM_LEN, MTDOF, NDOFG, NGRID
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  READ_DOF_TABLES_BEGEND
      USE DOF_TABLES, ONLY            :  TDOFI, TDOF, TSET

      USE READ_DOF_TABLES_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'READ_DOF_TABLES'
      CHARACTER(LEN=DATA_NAM_LEN)     :: NAME_Is           ! Name of data actually read from file
      CHARACTER(LEN=DATA_NAM_LEN)     :: NAME_ShouldBe     ! Name of data that should be read from file

      INTEGER(LONG)                   :: I,J               ! DO loop indices or counters
      INTEGER(LONG)                   :: INT2              ! Integer value read from file
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening or reading a file
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG)                   :: REC_NO            ! Record number when reading a file
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = READ_DOF_TABLES_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Make units for writing errors the error file and output file

      OUNT(1) = ERR
      OUNT(2) = F06

! Open L1C and read data. Skip data sets we don't need by reading them but not saving them

      CALL FILE_OPEN ( L1C, LINK1C, OUNT, 'OLD', L1C_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )

! Read TSET array

      REC_NO = 0
      NAME_ShouldBe = 'TSET'

      READ(L1C,IOSTAT=IOCHK) NAME_Is                                           ; REC_NO = REC_NO + 1
      CALL READ_CHK ( IOCHK, LINK1C, NAME_ShouldBe, REC_NO, OUNT )
      IF (NAME_Is /= NAME_ShouldBe) CALL DATA_SET_NAME_ERROR ( NAME_ShouldBe, LINK1C, NAME_Is )

      READ(L1C,IOSTAT=IOCHK) INT2                                              ; REC_NO = REC_NO + 1
      CALL READ_CHK ( IOCHK, LINK1C, NAME_ShouldBe, REC_NO, OUNT )
      IF (INT2 /= NGRID) CALL DATA_SET_SIZE_ERROR ( LINK1C, NAME_Is, 'NGRID', NGRID, INT2 )

      DO I=1,NGRID
         DO J=1,6
            READ(L1C,IOSTAT=IOCHK) TSET(I,J)                                   ; REC_NO = REC_NO + 1
            CALL READ_CHK ( IOCHK, LINK1C, NAME_ShouldBe, REC_NO, OUNT )
         ENDDO 
      ENDDO 

! Read TDOFI array

      REC_NO = 0
      NAME_ShouldBe = 'TDOFI'

      READ(L1C,IOSTAT=IOCHK) NAME_Is                                           ; REC_NO = REC_NO + 1
      CALL READ_CHK ( IOCHK, LINK1C, NAME_ShouldBe, REC_NO, OUNT )
      IF (NAME_Is /= NAME_ShouldBe) CALL DATA_SET_NAME_ERROR ( NAME_ShouldBe, LINK1C, NAME_Is )

      READ(L1C,IOSTAT=IOCHK) INT2                                              ; REC_NO = REC_NO + 1
      CALL READ_CHK ( IOCHK, LINK1C, NAME_ShouldBe, REC_NO, OUNT )
      IF (INT2 /= NDOFG) CALL DATA_SET_SIZE_ERROR ( LINK1C, NAME_Is, 'NDOFG', NDOFG, INT2 )

      READ(L1C,IOSTAT=IOCHK) INT2                                              ; REC_NO = REC_NO + 1
      CALL READ_CHK ( IOCHK, LINK1C, NAME_ShouldBe, REC_NO, OUNT )
      IF (INT2 /= MTDOF  ) CALL DATA_SET_SIZE_ERROR ( LINK1C, NAME_Is, 'MTDOF', MTDOF, INT2 )

      DO I=1,NDOFG
         DO J=1,MTDOF
            READ(L1C,IOSTAT=IOCHK) TDOFI(I,J)                                  ; REC_NO = REC_NO + 1
            CALL READ_CHK ( IOCHK, LINK1C, NAME_ShouldBe, REC_NO, OUNT )
         ENDDO 
      ENDDO 

! Read TDOF array

      REC_NO = 0
      NAME_ShouldBe = 'TDOF'

      READ(L1C,IOSTAT=IOCHK) NAME_Is                                           ; REC_NO = REC_NO + 1
      CALL READ_CHK ( IOCHK, LINK1C, NAME_ShouldBe, REC_NO, OUNT )
      IF (NAME_Is /= NAME_ShouldBe) CALL DATA_SET_NAME_ERROR ( NAME_ShouldBe, LINK1C, NAME_Is )

      READ(L1C,IOSTAT=IOCHK) INT2                                              ; REC_NO = REC_NO + 1
      CALL READ_CHK ( IOCHK, LINK1C, NAME_ShouldBe, REC_NO, OUNT )
      IF (INT2 /= NDOFG) CALL DATA_SET_SIZE_ERROR ( LINK1C, NAME_Is, 'NDOFG', NDOFG, INT2 )

      READ(L1C,IOSTAT=IOCHK) INT2                                              ; REC_NO = REC_NO + 1
      CALL READ_CHK ( IOCHK, LINK1C, NAME_ShouldBe, REC_NO, OUNT )
      IF (INT2 /= MTDOF  ) CALL DATA_SET_SIZE_ERROR ( LINK1C, NAME_Is, 'MTDOF', MTDOF, INT2 )

      DO I=1,NDOFG
         DO J=1,MTDOF
            READ(L1C,IOSTAT=IOCHK) TDOF(I,J)                                   ; REC_NO = REC_NO + 1
            CALL READ_CHK ( IOCHK, LINK1C, NAME_ShouldBe, REC_NO, OUNT )
         ENDDO 
      ENDDO 

      CALL FILE_CLOSE ( L1C, LINK1C, 'KEEP', 'Y' )

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

      END SUBROUTINE READ_DOF_TABLES
