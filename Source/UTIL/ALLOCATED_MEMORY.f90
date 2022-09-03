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

      SUBROUTINE ALLOCATED_MEMORY ( ARRAY_NAME, MB_ALLOCATED, WHAT, WRITE_TABLE, CURRENT_MB_ALLOCATED, CALLING_SUBR )

! Keeps track of memory allocated to every ALLOCATABLE array. Array ALLOCATED_ARRAY_MEM, after this subr has run, will have the
! amount of memory that is currently allocated to ARRAY_NAME. The output CURRENT_MB_ALLOCATED is what was allocated to
! ARRAY_NAME when this subr started

! (1) When this subr is called following a ALLOCATE   statement in some subr, the INTENT(IN) arg MB_ALLOCATED is expected to be
!     the amount of memory that the ALLOCATE action allocated.

! (2) When this subr is called following a DEALLOCATE statement in some subr, the INTENT(IN) arg MB_ALLOCATED is expected to be
!     ZERO (that is, the DEALLOCATE statement is expected to deallocate all memory allocated to the array

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, TOT_MB_MEM_ALLOC
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F06
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  SUPINFO
      USE ALLOCATED_ARRAY_DATA, ONLY  :  ALLOCATED_ARRAY_NAMES, ALLOCATED_ARRAY_MEM, NUM_ALLOC_ARRAYS

      USE ALLOCATED_MEMORY_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ALLOCATED_MEMORY'
      CHARACTER(LEN=*),INTENT(IN)     :: ARRAY_NAME           ! Name of allocatable array
      CHARACTER(LEN=*),INTENT(IN)     :: CALLING_SUBR         ! Name of subr that called this one
      CHARACTER(LEN=*),INTENT(IN)     :: WHAT                 ! 'ALLOC or 'DEALLOC'. Used for output message purpose
      CHARACTER(LEN=*),INTENT(IN)     :: WRITE_TABLE          ! If 'Y' and DEBUG says to, write out the memory table
      CHARACTER( 1*BYTE)              :: NAME_MATCH           ! 'Y' if ARRAY_NAME matches a name in array ALLOCATED_ARRAY_NAMES

      INTEGER(LONG)                   :: I,J                  ! DO loop indices
      INTEGER(LONG)                   :: INDEX                ! Index in array ALLOCATED_ARRAY_NAMES

      REAL(DOUBLE)    ,INTENT(OUT)    :: CURRENT_MB_ALLOCATED ! MB of memory that is allocated to ARRAY_NAME when this subr starts
      REAL(DOUBLE)    ,INTENT(IN)     :: MB_ALLOCATED       ! MB of memory to enter into array ALLOCATED_ARRAY_MEM for ARRAY_NAME
!                                                               when this subr returns

! **********************************************************************************************************************************
! Make sure WHAT is correct

      IF ((WHAT /= 'ALLOC') .AND. (WHAT /= 'DEALLOC')) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,971) SUBR_NAME, WHAT
         WRITE(ERR,971) SUBR_NAME, WHAT
         CALL OUTA_HERE ( 'Y' )
      ENDIF 

! Initialize outputs

      CURRENT_MB_ALLOCATED = 0

      IF (LEN(ARRAY_NAME) > LEN(ALLOCATED_ARRAY_NAMES)) THEN
         WRITE(ERR,922) SUBR_NAME, ARRAY_NAME, LEN(ARRAY_NAME),LEN(ALLOCATED_ARRAY_NAMES), CALLING_SUBR
         WRITE(F06,922) SUBR_NAME, ARRAY_NAME, LEN(ARRAY_NAME),LEN(ALLOCATED_ARRAY_NAMES), CALLING_SUBR
         CALL OUTA_HERE ( 'Y' )
      ENDIF

      INDEX = 0
i_do: DO I=1,NUM_ALLOC_ARRAYS

j_do:    DO J=1,LEN(ARRAY_NAME)
            IF (ARRAY_NAME(J:J) == ALLOCATED_ARRAY_NAMES(I)(J:J)) THEN
               NAME_MATCH = 'Y'
            ELSE
               NAME_MATCH = 'N'
               EXIT j_do
            ENDIF
         ENDDO j_do

         IF (NAME_MATCH == 'Y') THEN
            INDEX = I
            EXIT i_do
         ELSE
            CYCLE i_do
         ENDIF

      ENDDO i_do

      IF (INDEX > 0) THEN
         CURRENT_MB_ALLOCATED       = ALLOCATED_ARRAY_MEM(INDEX)
         ALLOCATED_ARRAY_MEM(INDEX) = MB_ALLOCATED
      ELSE
         CURRENT_MB_ALLOCATED = ZERO
         WRITE(ERR,101) ARRAY_NAME, SUBR_NAME
         IF (SUPINFO == 'N') THEN
            WRITE(F06,101) ARRAY_NAME, SUBR_NAME
         ENDIF
      ENDIF

      IF (WHAT == 'ALLOC') THEN
         TOT_MB_MEM_ALLOC = TOT_MB_MEM_ALLOC + MB_ALLOCATED
      ELSE
         TOT_MB_MEM_ALLOC = TOT_MB_MEM_ALLOC - CURRENT_MB_ALLOCATED
      ENDIF

      IF ((DEBUG(100) > 2) .AND. (WRITE_TABLE == 'Y')) THEN
         IF (WHAT == 'ALLOC') THEN
            WRITE(F06,*) ' MEMORY ALLOCATION TABLE AFTER ALLOCATING ARRAY ',ARRAY_NAME
         ELSE
            WRITE(F06,*) ' MEMORY ALLOCATION TABLE AFTER DEALLOCATING ARRAY ',ARRAY_NAME
         ENDIF
         CALL WRITE_ALLOC_MEM_TABLE ( '' )
      ENDIF

! **********************************************************************************************************************************
  101 FORMAT(' *INFORMATION: ARRAY ',A,' IS NOT IN THE LIST OF NAMES IN ARRAY "ALLOCATED_ARRAY_DATA" SO THAT SUBR,'                &
                    ,/,15X,A,' CANNOT CALC DATA FOR ARRAY "ALLOCATED_ARRAY_MEM".'                                                  &
                    ,/,14X,' THIS IS NOT A PROBLEM WITH THIS RUN, ONLY SOMETHING NEEDING ATTENTION BY THE AUTHOR')

  922 FORMAT(' *ERROR   922: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' ARRAY "',A,'" HAS LEN(ARRAY_NAME) = ',I4,' CANNOT BE GREATER THAN LEN(ALLOCATED_ARRAY_NAMES) = ',I4   &
                    ,/,14X,' THIS SUBR WAS CALLED BY ',A)

  971 FORMAT(' *ERROR   971: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INPUT ARG "WHAT" MUST BE EITHER "ALLOC" OR "DEALLOC" BUT IS = "',A,'"')

99887 FORMAT(' In ALLOCATED_MEMORY: I, ARRAY_NAME, LEN(ARRAY_NAME) = ',I4,'"',A31,'"',I4) 

! **********************************************************************************************************************************

      END SUBROUTINE ALLOCATED_MEMORY

