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

   MODULE EC_OUTPUT4_Interface

   INTERFACE

      SUBROUTINE EC_OUTPUT4 ( CARD1, IERR, ANY_OU4_NAME_BAD )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, EC_ENTRY_LEN
      USE SUBR_BEGEND_LEVELS, ONLY    :  EC_OUTPUT4_BEGEND
      USE IOUNT1, ONLY                :  ERR, F04, F06, MOU4, OU4, OU4_ELM_OTM, OU4_GRD_OTM, SC1, WRT_LOG
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE OUTPUT4_MATRICES, ONLY      :  NUM_OU4_VALID_NAMES, TAPE_ACTION_MAX_VAL, TAPE_ACTION_MIN_VAL, NUM_OU4_REQUESTS,          &
                                         OU4_FILE_UNITS, OU4_TAPE_ACTION, ACT_OU4_MYSTRAN_NAMES, ACT_OU4_OUTPUT_NAMES,             &
                                         ALLOW_OU4_MYSTRAN_NAMES, ALLOW_OU4_OUTPUT_NAMES
 
      USE TIMDAT, ONLY                :  TSEC

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: CARD1             ! Card read in LOADE and shifted to begin in col 1
      CHARACTER(LEN=*), INTENT(OUT)   :: ANY_OU4_NAME_BAD  ! 'Y'/'N' if requested OUTPUT4 matrix name is valid
 
      INTEGER(LONG), INTENT(OUT)      :: IERR              ! Error indicator. If CHAR not found, IERR set to 1
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = EC_OUTPUT4_BEGEND
 
      END SUBROUTINE EC_OUTPUT4

   END INTERFACE

   END MODULE EC_OUTPUT4_Interface

