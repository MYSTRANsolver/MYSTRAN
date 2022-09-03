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

      SUBROUTINE PRINT_CONSTANTS_1

! Writes real constants defined in module CONSTANTS_1

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  PROG_NAME

      USE PRINT_CONSTANTS_1_USE_IFs

      IMPLICIT NONE
  
!***********************************************************************************************************************************
! Print real constants used in various subroutines
  
      WRITE(F06,1101)
      WRITE(F06,1102)
      WRITE(F06,*)

      WRITE(F06,*) '                                               ZERO         = ', ZERO
      WRITE(F06,*) '                                               ONEPM16      = ', ONEPM16
      WRITE(F06,*) '                                               ONEPM15      = ', ONEPM15
      WRITE(F06,*) '                                               ONEPM14      = ', ONEPM14
      WRITE(F06,*) '                                               ONEPM6       = ', ONEPM6
      WRITE(F06,*) '                                               ONEPM5       = ', ONEPM5
      WRITE(F06,*) '                                               ONEPM4       = ', ONEPM4

      WRITE(F06,*) '                                               TENTH        = ', TENTH
      WRITE(F06,*) '                                               SIXTH        = ', SIXTH
      WRITE(F06,*) '                                               QUARTER      = ', QUARTER
      WRITE(F06,*) '                                               THIRD        = ', THIRD
      WRITE(F06,*) '                                               HALF         = ', HALF

      WRITE(F06,*) '                                               ONE          = ', ONE
      WRITE(F06,*) '                                               TWO          = ', TWO
      WRITE(F06,*) '                                               THREE        = ', THREE
      WRITE(F06,*) '                                               THREEP6      = ', THREEP6
      WRITE(F06,*) '                                               FOUR         = ', FOUR
      WRITE(F06,*) '                                               FIVE         = ', FIVE
      WRITE(F06,*) '                                               SIX          = ', SIX
      WRITE(F06,*) '                                               SEVEN        = ', SEVEN
      WRITE(F06,*) '                                               EIGHT        = ', EIGHT
      WRITE(F06,*) '                                               NINE         = ', NINE
      WRITE(F06,*) '                                               TEN          = ', TEN
      WRITE(F06,*) '                                               ELEVEN       = ', ELEVEN
      WRITE(F06,*) '                                               TWELVE       = ', TWELVE
      WRITE(F06,*) '                                               FORTY5       = ', FORTY5
      WRITE(F06,*) '                                               ONE_HUNDRED  = ', ONE_HUNDRED
      WRITE(F06,*) '                                               ONE80        = ', ONE80
      WRITE(F06,*) '                                               ONE_THOUSAND = ', ONE_THOUSAND
      WRITE(F06,*) '                                               ONEPP6       = ', ONEPP6
      WRITE(F06,*) '                                               ONEPP7       = ', ONEPP7
      WRITE(F06,*) '                                               ONEPP10      = ', ONEPP10

      WRITE(F06,*) '                                               PI           = ', PI
      WRITE(F06,*) '                                               CONV_DEG_RAD = ', CONV_DEG_RAD
      WRITE(F06,*) '                                               CONV_RAD_DEG = ', CONV_RAD_DEG

      WRITE(F06,1106)

! **********************************************************************************************************************************
 1101 FORMAT(' __________________________________________________________________________________________________________________',&
             '_________________'                                                                                               ,//,&
             ' ::::::::::::::::::::::::::::::::::::::::::START DEBUG( 2) OUTPUT FROM SUBROUTINE LINK1::::::::::::::::::::::::::::',&
              ':::::::::::::::::',/)

 1102 FORMAT(45X,'Real constants used in MYSTRAN are:')

 1106 FORMAT(' :::::::::::::::::::::::::::::::::::::::::::END DEBUG( 2) OUTPUT FROM SUBROUTINE LINK1:::::::::::::::::::::::::::::',&
              ':::::::::::::::::'                                                                                               ,/,&
             ' __________________________________________________________________________________________________________________',&
             '_________________',/)

! **********************************************************************************************************************************

      END SUBROUTINE PRINT_CONSTANTS_1
