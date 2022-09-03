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
  
      SUBROUTINE EC_DEBUG ( CARD )
  
! Processes DEBUG in Exec Control. This is done to allow DEBUG to be read before some of the Bulk Data is input. Some of the DEBUG
! capability needs to be processed prior to the complete input data file being read 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F06
      USE SCONTR, ONLY                :  ECHO, IERRFL, JCARD_LEN, JF, WARN_ERR
      USE PARAMS, ONLY                :  SUPWARN
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG, NDEBUG
 
      USE EC_DEBUG_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: CHARFLD2          ! 
      CHARACTER(LEN=JCARD_LEN)        :: CHARFLD3          ! 
      CHARACTER( 8*BYTE)              :: JCARD_08(10)      ! The 10 fields of 8 characters making up CARD
 
      INTEGER(LONG)                   :: INDEX             ! An index into array DEBUG read on B.D. DEBUG card
      INTEGER(LONG), PARAMETER        :: LOWER    = 1      ! Lower allowable value for an integer parameter
      INTEGER(LONG)                   :: UPPER    = NDEBUG ! Upper allowable value for an integer parameter
      INTEGER(LONG)                   :: VALUE             ! Value for DEBUG(INDEX) read on B.D. DEBUG card
  
! **********************************************************************************************************************************
      CHARFLD2(1:) = ' '
      CHARFLD3(1:) = ' '
 
! Make JCARD_08 from CARD
 
      CALL MKJCARD_08 ( CARD, JCARD_08 )
      CHARFLD2(1:) = JCARD_08(2)(1:)
      CHARFLD3(1:) = JCARD_08(3)(1:)
 
! Read DEBUG index and DEBUG(INDEX) value

      CALL I4FLD ( CHARFLD2, JF(2), INDEX )
      IF (IERRFL(2) == 'N') THEN
         IF ((INDEX >= LOWER) .AND. (INDEX <= UPPER)) THEN
            CALL I4FLD ( CHARFLD3, JF(3), VALUE )
            IF (IERRFL(3) == 'N') THEN
               DEBUG(INDEX) = VALUE
            ENDIF
         ELSE
            WARN_ERR = WARN_ERR + 1
            WRITE(ERR,101) CARD
            WRITE(ERR,1120) LOWER,UPPER,INDEX
            IF (SUPWARN == 'N') THEN
               IF (ECHO == 'NONE  ') THEN
                  WRITE(F06,101) CARD
               ENDIF
               WRITE(F06,1120) LOWER,UPPER,INDEX
            ENDIF
         ENDIF
      ENDIF
  
! **********************************************************************************************************************************
  101 FORMAT(A)

 1120 FORMAT(' *WARNING    : DEBUG INDEX MUST BE >= ',I4,' AND <= ',I4,' BUT INPUT VALUE IS: ',I8,'. ENTRY IGNORED')
   
! **********************************************************************************************************************************
 
      END SUBROUTINE EC_DEBUG
