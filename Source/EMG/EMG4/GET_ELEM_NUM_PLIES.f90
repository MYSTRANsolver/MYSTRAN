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

      SUBROUTINE GET_ELEM_NUM_PLIES ( INT_ELEM_ID )

! Gets shell element number of plies (1 unless elem uses PCOMP props) given the element's internal ID

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  WRT_LOG, F04, f06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, DEDAT_Q4_SHELL_KEY, DEDAT_T3_SHELL_KEY, NPCOMP, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_ELEM_NUM_PLIES_BEGEND
      USE MODEL_STUF, ONLY            :  EDAT, EID, EPNT, ETYPE, INTL_PID, NUM_PLIES, PCOMP, TYPE

      USE GET_ELEM_NUM_PLIES_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GET_ELEM_NUM_PLIES'

      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID       ! Internal element ID for which
      INTEGER(LONG)                   :: EPNTK             ! Value from array EPNT at the row for this internal elem ID. It is the
!                                                            row number in array EDAT where data begins for this element. 
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: I1                ! Index into EDAT
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_ELEM_NUM_PLIES_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      NUM_PLIES = 1

      IF ((TYPE(1:5) == 'TRIA3') .OR. (TYPE(1:5) == 'QUAD4')) THEN

         EPNTK    = EPNT(INT_ELEM_ID)
         TYPE     = ETYPE(INT_ELEM_ID)                     ! NOTE: Must keep (this subr not always called when TYPE     is known)
         EID      = EDAT(EPNTK)                            ! NOTE: Must keep (this subr not always called when EID      is known)
         INTL_PID = EDAT(EPNTK+1)                          ! NOTE: Must keep (this subr not always called when INTL_PID is known)

         IF      ((TYPE == 'TRMEM   ') .OR. (TYPE == 'TRPLT1  ') .OR. (TYPE == 'TRPLT2  ') .OR.                                    &
                  (TYPE == 'TRIA3K  ') .OR. (TYPE == 'TRIA3   ')) THEN
             I1 = DEDAT_T3_SHELL_KEY
         ELSE IF ((TYPE == 'QDMEM   ') .OR. (TYPE == 'QDPLT1  ') .OR. (TYPE == 'QDPLT2  ') .OR.                                    &
                  (TYPE == 'QUAD4K  ') .OR. (TYPE == 'QUAD4   ')) THEN
             I1 = DEDAT_Q4_SHELL_KEY
         ENDIF

         IF (EDAT(EPNTK+I1) == 2) THEN
            DO I=1,NPCOMP
               NUM_PLIES = PCOMP(INTL_PID,5)
            ENDDO
         ENDIF 

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

      END SUBROUTINE GET_ELEM_NUM_PLIES
