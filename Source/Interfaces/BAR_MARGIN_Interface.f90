! ###############################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2022 Dr William R Case, Jr (mystransolver@gmail.com)                                              
                                                                                                         
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

   MODULE BAR_MARGIN_Interface

   INTERFACE

      SUBROUTINE BAR_MARGIN ( ICOL, S1, S2, S3, S4, S5, MS1, MS2, MS3, MSP1, MSP2, MSP3 )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BAR_MARGIN_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, ONEPM6, ONEPM15, ONEPP10
      USE PARAMS, ONLY                :  EPSIL 
      USE MODEL_STUF, ONLY            :  ULT_STRE

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(OUT)   :: MSP1              ! If '1',  print margins in F06 file. If '0', do not print.
      CHARACTER(LEN=*), INTENT(OUT)   :: MSP2              ! If '1',  print margins in F06 file. If '0', do not print.
      CHARACTER(LEN=*), INTENT(OUT)   :: MSP3              ! If '1',  print margins in F06 file. If '0', do not print.
 
      INTEGER(LONG), INTENT(IN)       :: ICOL              ! Column no. from ULT_STRE to get max allow. stresses
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BAR_MARGIN_BEGEND
 
      REAL(DOUBLE), INTENT(OUT)       :: MS1               ! Calculated margin of safety
      REAL(DOUBLE), INTENT(OUT)       :: MS2               ! Calculated margin of safety
      REAL(DOUBLE), INTENT(OUT)       :: MS3               ! Calculated margin of safety
      REAL(DOUBLE), INTENT(IN)        :: S1                ! An input stress for which margins are calculated
      REAL(DOUBLE), INTENT(IN)        :: S2                ! An input stress for which margins are calculated
      REAL(DOUBLE), INTENT(IN)        :: S3                ! An input stress for which margins are calculated
      REAL(DOUBLE), INTENT(IN)        :: S4                ! An input stress for which margins are calculated
      REAL(DOUBLE), INTENT(IN)        :: S5                ! An input stress for which margins are calculated
 
      END SUBROUTINE BAR_MARGIN

   END INTERFACE

   END MODULE BAR_MARGIN_Interface

