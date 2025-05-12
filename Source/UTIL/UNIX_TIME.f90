! ##################################################################################################################################
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

! Computes the current UNIX timestamp.

SUBROUTINE UNIX_TIME(T)

   USE PENTIUM_II_KIND, ONLY: LONG

   IMPLICIT NONE
   
   INTEGER(LONG), INTENT(OUT) :: T
   CHARACTER(LEN=8)     :: DATE_C
   CHARACTER(LEN=10)    :: TIME_C
   CHARACTER(LEN=5)     :: ZONE_C
   INTEGER              :: VALUES(8)
   INTEGER              :: Y, MO, DA, HH, MM, SS
   INTEGER              :: ZH, ZM, SIGN
   INTEGER(LONG)        :: TZ_MIN, Y0, M0, A, B, JDN, EPOCH

   CALL DATE_AND_TIME(DATE_C, TIME_C, ZONE_C, VALUES)

   READ(DATE_C(1:4), '(I4)') Y
   READ(DATE_C(5:6), '(I2)') MO
   READ(DATE_C(7:8), '(I2)') DA

   READ(TIME_C(1:2), '(I2)') HH
   READ(TIME_C(3:4), '(I2)') MM
   READ(TIME_C(5:6), '(I2)') SS

   ! Parse timezone “+hhmm”/“-hhmm” into minutes
   IF (ZONE_C(1:1) .EQ. '-') THEN
      SIGN = -1
   ELSE
      SIGN = 1
   END IF
   READ(ZONE_C(2:3), '(I2)') ZH
   READ(ZONE_C(4:5), '(I2)') ZM
   TZ_MIN = SIGN * (ZH * 60 + ZM)

   ! Compute Julian Day Number
   IF (MO .LE. 2) THEN
      Y0 = Y - 1
      M0 = MO + 12
   ELSE
      Y0 = Y
      M0 = MO
   END IF
   A   = Y0 / 100
   B   = 2 - A + A / 4
   JDN = INT(365.25D0 * (Y0 + 4716)) + INT(30.6001D0 * (M0 + 1)) &
         + DA + B - 1524

   EPOCH = 2440588       ! JDN of 1970-01-01

   T = (JDN - EPOCH) * 86400 &
     + HH * 3600 + MM * 60 + SS &
     - TZ_MIN * 60

END SUBROUTINE UNIX_TIME
