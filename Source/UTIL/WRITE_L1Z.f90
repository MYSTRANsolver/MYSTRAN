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
 
      SUBROUTINE WRITE_L1Z
 
! Writes file LINK1Z of some of the data needed in a restart if user has CHKPNT in the Exec Control
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, F04, F06, L1Z, LINK1Z, L1Z_MSG, L1ZSTAT
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NSUB, SOL_NAME
      USE TIMDAT, ONLY                :  STIME, TSEC
      USE MODEL_STUF, ONLY            :  CC_EIGR_SID, MPCSET, SPCSET, SUBLOD
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_L1Z_BEGEND
 
      USE WRITE_L1Z_USE_IFs


      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_L1Z'

      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_L1Z_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      CALL FILE_OPEN ( L1Z, LINK1Z, OUNT, 'REPLACE', L1Z_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )
      WRITE(L1Z) SOL_NAME
      WRITE(L1Z) NSUB
      WRITE(L1Z) MPCSET
      WRITE(L1Z) SPCSET
      DO I=1,NSUB
         WRITE(L1Z) SUBLOD(I,1), SUBLOD(I,2)
      ENDDO
      WRITE(L1Z) CC_EIGR_SID
      CALL FILE_CLOSE ( L1Z, LINK1Z, L1ZSTAT, 'Y' )

 9002    FORMAT(1X,A,' END  ',F10.3)
! **********************************************************************************************************************************
 
      END SUBROUTINE WRITE_L1Z
