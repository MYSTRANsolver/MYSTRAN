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

   MODULE STOKEN_Interface

   INTERFACE

      SUBROUTINE STOKEN ( CALLING_SUBR, TOKSTR, TOKEN_BEG, STRNG_END, NTOKEN, IERROR, TOKTYP, TOKEN, ERRTOK, THRU, EXCEPT )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  MAX_TOKEN_LEN, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  STOKEN_BEGEND
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG

      IMPLICIT NONE

      CHARACTER(LEN=*)          , INTENT(IN)   :: CALLING_SUBR! Character string to tokenize 
      CHARACTER(LEN=*)          , INTENT(IN)   :: TOKSTR      ! Character string to tokenize 
      CHARACTER( 3*BYTE)        , INTENT(INOUT):: EXCEPT      ! Flag indicating whether EXCEPT is "ON " or "OFF"
      CHARACTER( 3*BYTE)        , INTENT(INOUT):: THRU        ! Flag indicating whether THRU   is "ON " or "OFF"
      CHARACTER(LEN=LEN(TOKSTR)), INTENT(OUT)  :: ERRTOK      ! Char string with data for an error to be printed by calling subr
      CHARACTER( 8*BYTE), INTENT(OUT)          :: TOKEN(3)    ! Array of 3 char tokens (e.g. could contain I1, THRU, I2)
      CHARACTER( 8*BYTE), INTENT(OUT)          :: TOKTYP(3)   ! Array of 3 char indicators of what type of tokens are in TOKEN(1-3)

      INTEGER(LONG), INTENT(IN)                :: STRNG_END   ! Column of last character in TOKSTR
      INTEGER(LONG), INTENT(INOUT)             :: TOKEN_BEG   ! On entry, where to start to look for a token in TOKSTR
      INTEGER(LONG), INTENT(OUT)               :: IERROR      ! Integer error no. when an error occurs when processing tokens
      INTEGER(LONG), INTENT(OUT)               :: NTOKEN      ! The number of tokens found in this execution
      INTEGER(LONG), PARAMETER                 :: SUBR_BEGEND = STOKEN_BEGEND

      END SUBROUTINE STOKEN

   END INTERFACE

   END MODULE STOKEN_Interface

