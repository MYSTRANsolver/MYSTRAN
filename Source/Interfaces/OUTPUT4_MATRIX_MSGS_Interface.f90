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

   MODULE OUTPUT4_MATRIX_MSGS_Interface

   INTERFACE

      SUBROUTINE OUTPUT4_MATRIX_MSGS ( OUNT )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  ERR, F06, LEN_INPUT_FNAME, MOU4, OU4, OU4FIL, OU4_MSG, OU4STAT
      USE OUTPUT4_MATRICES, ONLY      :  ACT_OU4_MYSTRAN_NAMES, ACT_OU4_MYSTRAN_NAMES, ACT_OU4_OUTPUT_NAMES,                       &
                                         ALLOW_OU4_MYSTRAN_NAMES, ALLOW_OU4_OUTPUT_NAMES, NUM_OU4_REQUESTS, NUM_OU4_VALID_NAMES,   &
                                         OU4_FILE_UNITS
      USE PARAMS, ONLY                :  SUPINFO

      IMPLICIT NONE

      INTEGER(LONG), INTENT(IN)       :: OUNT(2)             ! File units to write messages to. Input to subr UNFORMATTED_OPEN.

      END SUBROUTINE OUTPUT4_MATRIX_MSGS

   END INTERFACE

   END MODULE OUTPUT4_MATRIX_MSGS_Interface

