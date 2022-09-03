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
 
      SUBROUTINE RIGID_ELEM_PROC
 
! Processes RBAR, RBE1, RBE2 rigid elements to get terms for the RMG constraint matrix
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, L1F, LINK1F, L1F_MSG, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NRECARD
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  RIGID_ELEM_PROC_BEGEND

      USE RIGID_ELEM_PROC_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'RIGID_ELEM_PROC'
      CHARACTER( 8*BYTE)              :: RTYPE

      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening/reading a file
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG)                   :: IERR  = 0         ! Count of read errors when rigid elem data file is read
      INTEGER(LONG)                   :: REC_NO            ! Record number when reading a file
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = RIGID_ELEM_PROC_BEGEND
 
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
 
! Read a record from L1F and find out which rigid element it is

      IERR   = 0
      REC_NO = 0
      DO I=1,NRECARD

         READ(L1F,IOSTAT=IOCHK) RTYPE
         REC_NO = REC_NO + 1
         IF (IOCHK /= 0) THEN
            CALL READERR ( IOCHK, LINK1F, L1F_MSG, REC_NO, OUNT, 'Y' )
            CALL OUTA_HERE ( 'Y' )                                 ! Error reading RTYPE from rigid elem file. Can't continue
         ENDIF

         IF      (RTYPE == 'RBAR    ') THEN  

            WRITE(ERR,*) '*ERROR: Code for RBAR processing not written yet. RBAR element ignored'
            WRITE(F06,*) '*ERROR: Code for RBAR processing not written yet. RBAR element ignored'
            WRITE(SC1,*) '*ERROR: Code for RBAR processing not written yet. RBAR element ignored'
            IERR      = IERR + 1
            FATAL_ERR = FATAL_ERR + 1

         ELSE IF (RTYPE == 'RBE1    ') THEN

            WRITE(ERR,*) '*ERROR: Code for RBE1 processing not written yet. RBE1 element ignored'
            WRITE(F06,*) '*ERROR: Code for RBE1 processing not written yet. RBE1 element ignored'
            WRITE(SC1,*) '*ERROR: Code for RBE1 processing not written yet. RBE1 element ignored'
            IERR      = IERR + 1
            FATAL_ERR = FATAL_ERR + 1
         ELSE IF (RTYPE == 'RBE2    ') THEN

            CALL RBE2_PROC    ( RTYPE, REC_NO, IERR )

         ELSE IF (RTYPE == 'RBE3    ') THEN

            CALL RBE3_PROC    ( RTYPE, REC_NO, IERR )

         ELSE IF (RTYPE == 'RSPLINE ') THEN

            CALL RSPLINE_PROC ( RTYPE, REC_NO, IERR )

         ENDIF

      ENDDO

      IF (IERR > 0) THEN
         WRITE(ERR,9996) SUBR_NAME,IERR
         WRITE(F06,9996) SUBR_NAME,IERR
         CALL OUTA_HERE ( 'Y' )                                    ! Errors reading rigid element data file, so quit
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 9996 FORMAT(/,' PROCESSING ABORTED IN SUBROUTINE ',A,' DUE TO ABOVE ',I8,' ERRORS')

      END SUBROUTINE RIGID_ELEM_PROC
