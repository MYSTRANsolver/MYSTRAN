! ###############################################################################################################################
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

   MODULE ALLOCATED_MEMORY_Interface

   INTERFACE

      SUBROUTINE ALLOCATED_MEMORY ( ARRAY_NAME, MB_ALLOCATED, WHAT, WRITE_TABLE, CURRENT_MB_ALLOCATED, CALLING_SUBR )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, TOT_MB_MEM_ALLOC
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F06
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  SUPINFO
      USE ALLOCATED_ARRAY_DATA, ONLY  :  ALLOCATED_ARRAY_NAMES, ALLOCATED_ARRAY_MEM, NUM_ALLOC_ARRAYS

      IMPLICIT NONE

      CHARACTER(LEN=*),INTENT(IN)     :: ARRAY_NAME           ! Name of allocatable array
      CHARACTER(LEN=*),INTENT(IN)     :: CALLING_SUBR         ! Name of subr that called this one
      CHARACTER(LEN=*),INTENT(IN)     :: WHAT                 ! 'ALLOC or 'DEALLOC'. Used for output message purpose
      CHARACTER(LEN=*),INTENT(IN)     :: WRITE_TABLE          ! If 'Y' and DEBUG says to, write out the memory table

      REAL(DOUBLE)    ,INTENT(OUT)    :: CURRENT_MB_ALLOCATED ! MB of memory that is allocated to ARRAY_NAME when this subr starts
      REAL(DOUBLE)    ,INTENT(IN)     :: MB_ALLOCATED       ! MB of memory to enter into array ALLOCATED_ARRAY_MEM for ARRAY_NAME
      END SUBROUTINE ALLOCATED_MEMORY

   END INTERFACE

   END MODULE ALLOCATED_MEMORY_Interface

