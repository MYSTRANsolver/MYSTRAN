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
 
      SUBROUTINE WRITE_ENF_TO_L1O
 
! Reads enforced displacement data from text file ENFFIL and writes it to unformatted file LINK1O. 
! Used when ENFORCED = filename in Case Control signifies that all DOF's will be in SE set and their values are in ENFFIL

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ENF, ENFFIL, ENFSTAT, ENF_MSG, ERR, F04, F06, L1O, LINK1O, L1OSTAT, L1O_MSG, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NDOFSG, NGRID, NSPC, NUM_SPC_RECORDS, NUM_SPC1_RECORDS, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      USE DOF_TABLES, ONLY            :  TSET_CHR_LEN, TSET
      USE MODEL_STUF, ONLY            :  SPCSET
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_ENF_TO_L1O_BEGEND
 
      USE WRITE_ENF_TO_L1O_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_ENF_TO_L1O'
      CHARACTER(LEN=LEN(TSET_CHR_LEN)):: DOFSET    = 'SE'  ! The name of a DOF set (e.g. 'SB', 'A ', etc)

      INTEGER(LONG)                   :: GRID_ID           ! ID of grid for which the enforced data belongs
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening or reading a file
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr FILE_OPEN  
      INTEGER(LONG)                   :: REC_NO            ! Number of the record read from ENF file
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_ENF_TO_L1O_BEGEND

      REAL(DOUBLE)                    :: RSPC(6)           ! Enforced displ components read from file ENF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      OUNT(1) = ERR
      OUNT(2) = F06

      IF (NUM_SPC_RECORDS /= 0) THEN
         WARN_ERR = WARN_ERR + 1
         WRITE(ERR,101) NUM_SPC_RECORDS
         IF (SUPWARN == 'N') THEN
            WRITE(F06,101) NUM_SPC_RECORDS
         ENDIF
      ENDIF

      IF (NUM_SPC1_RECORDS /= 0) THEN
         WARN_ERR = WARN_ERR + 1
         WRITE(ERR,102) NUM_SPC1_RECORDS
         IF (SUPWARN == 'N') THEN
            WRITE(F06,102) NUM_SPC1_RECORDS
         ENDIF
      ENDIF

      IF (NDOFSG /= 0) THEN
         WARN_ERR = WARN_ERR + 1
         WRITE(ERR,103) NDOFSG
         IF (SUPWARN == 'N') THEN
            WRITE(F06,103) NDOFSG
         ENDIF
      ENDIF

      CALL FILE_OPEN ( ENF, ENFFIL, OUNT, 'OLD'    , ENF_MSG, 'NEITHER'    , 'FORMATTED'  , 'READ' , 'REWIND', 'N', 'N', 'N' )

      REC_NO          = 0
      NUM_SPC_RECORDS = 0
      READ(ENF,*,IOSTAT=IOCHK)                             ! Title line not used. Let it have REC_NO = 0 (i.e. don't increment here)
      IF (IOCHK /= 0) THEN
         CALL READ_CHK ( IOCHK, ENFFIL, 'ENFORCED DISPL DATA', REC_NO, OUNT )
      ENDIF
      DO

         READ(ENF,*,IOSTAT=IOCHK) GRID_ID, (RSPC(J),J=1,6)
         REC_NO = REC_NO + 1
         IF (IOCHK /= 0) THEN
            CALL READ_CHK ( IOCHK, ENFFIL, 'ENFORCED DISPL DATA', REC_NO, OUNT )
         ENDIF

         DO J=1,6
            NUM_SPC_RECORDS = NUM_SPC_RECORDS + 1
            NSPC = NSPC + 1
            WRITE(L1O)  SPCSET, J, GRID_ID, GRID_ID, RSPC(J), DOFSET
         ENDDO

         IF (REC_NO == NGRID) THEN
            EXIT
         ELSE
            CYCLE
         ENDIF

      ENDDO

      CALL FILE_CLOSE ( ENF, ENFFIL, 'KEEP', 'Y' )
      CALL FILE_CLOSE ( L1O, LINK1O, 'KEEP', 'Y' )

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(' *WARNING    : WHEN CASE CONTROL ENFORCED COMMAND IS PRESENT ALL SPCs MUST BE DEFINED IN THE FILE REFERENCED IN',    &
                           ' THE ENFORCED COMMAND LINE.'                                                                           &
                    ,/,14X,' HOWEVER, THERE ARE ',I8,' SPCs DEFINED ON BULK DATA SPC ENTRIES. THESE WILL BE IGNORED')

  102 FORMAT(' *WARNING    : WHEN CASE CONTROL ENFORCED COMMAND IS PRESENT ALL SPCs MUST BE DEFINED IN THE FILE REFERENCED IN',    &
                           ' THE ENFORCED COMMAND LINE.'                                                                           &
                    ,/,14X,' HOWEVER, THERE ARE ',I8,' SPCs DEFINED ON BULK DATA SPC1 ENTRIES. THESE WILL BE IGNORED')

  103 FORMAT(' *WARNING    : WHEN CASE CONTROL ENFORCED COMMAND IS PRESENT ALL SPCs MUST BE DEFINED IN THE FILE REFERENCED IN',    &
                           ' THE ENFORCED COMMAND LINE.'                                                                           &
                    ,/,14X,' HOWEVER, THERE ARE ',I8,' SPCs DEFINED ON BULK DATA GRID ENTRIES. THESE WILL BE IGNORED')

! **********************************************************************************************************************************

      END SUBROUTINE WRITE_ENF_TO_L1O
