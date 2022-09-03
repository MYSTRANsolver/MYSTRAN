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

      SUBROUTINE WRITE_ALLOC_MEM_TABLE ( MESSAGE )

! Writes memory allocation table based on user supplied Bulk Data DEBUG entry (see module DEBUG_PARAMS). The memory allocation table
! is written to the F06 file and shows the amount of memory allocated to all arrays that are currently allocated.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F06
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE ALLOCATED_ARRAY_DATA, ONLY  :  ALLOCATED_ARRAY_NAMES, ALLOCATED_ARRAY_MEM, NUM_ALLOC_ARRAYS

      USE WRITE_ALLOC_MEM_TABLE_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_ALLOC_MEM_TABLE'
      CHARACTER(LEN=*), INTENT(IN)    :: MESSAGE           ! Name of subr that called this subr
      CHARACTER(2*BYTE)               :: ASTERISK

      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: NUM_ARRAYS_ALLOCTD! Number of arrays that have mem allocated to them

      REAL(DOUBLE)                    :: TOTAL             ! Sum of all rows of ALLOCATED_ARRAY_MEM (this should be the same as
!                                                            TOT_MB_MEM_ALLOC in module SCONTR but I wan't an independent calc.

! **********************************************************************************************************************************
! Write memory allocation table, if requested

      WRITE(F06,*)
      WRITE(F06,200) MESSAGE

      NUM_ARRAYS_ALLOCTD = 0
      TOTAL = ZERO
      ASTERISK = '  '
      DO I=1,NUM_ALLOC_ARRAYS

         TOTAL = TOTAL + ALLOCATED_ARRAY_MEM(I)

         IF (DEBUG(107) > 0) THEN                          ! Write info for all arrays (even ones that have no memory allocated

            IF (ALLOCATED_ARRAY_MEM(I) > ZERO) THEN
               NUM_ARRAYS_ALLOCTD = NUM_ARRAYS_ALLOCTD + 1
               ASTERISK = ' *'
            ENDIF
            WRITE(F06,201) I, ALLOCATED_ARRAY_NAMES(I), ALLOCATED_ARRAY_MEM(I), ASTERISK
            ASTERISK = '  '

         ELSE                                              ! Write info on only those arrays that have memory alocated

            IF (ALLOCATED_ARRAY_MEM(I) > ZERO) THEN
               ASTERISK = '  '
               NUM_ARRAYS_ALLOCTD = NUM_ARRAYS_ALLOCTD + 1
               WRITE(F06,201) NUM_ARRAYS_ALLOCTD, ALLOCATED_ARRAY_NAMES(I), ALLOCATED_ARRAY_MEM(I), MESSAGE, ASTERISK
            ENDIF

         ENDIF

      ENDDO

      WRITE(F06,203) TOTAL, NUM_ARRAYS_ALLOCTD
      WRITE(F06,*)

! **********************************************************************************************************************************
  200 FORMAT(40X,'Memory allocation table - ',A,/)

  201 FORMAT(10X,'(',I3,') allocatable array ',A,' has ',F13.6,' MB of memory allocated to ',A,A)

  203 FORMAT(70X,'----------',/,42X,'Total MB memory allocated = ',F13.6,' to ',I3,' different arrays')

! **********************************************************************************************************************************

      END SUBROUTINE WRITE_ALLOC_MEM_TABLE

