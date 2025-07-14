! #################################################################################################################################
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
      SUBROUTINE MITC_SHAPE_FUNCTIONS ( R, S, PSH, DPSHG )
 

      USE PENTIUM_II_KIND, ONLY       :  DOUBLE
      USE MODEL_STUF, ONLY            :  ELGP, TYPE
      USE IOUNT1, ONLY                :  ERR, F06
      USE SCONTR, ONLY                :  FATAL_ERR

      USE SHP2DQ_Interface
      USE OUTA_HERE_Interface

      IMPLICIT NONE 

      REAL(DOUBLE) , INTENT(IN)       :: R,S               ! Isoparametric coordinates
      REAL(DOUBLE) , INTENT(OUT)      :: PSH(ELGP)         ! Shape functions
      REAL(DOUBLE) , INTENT(OUT)      :: DPSHG(2,ELGP)     ! Derivatives of shape functions with respect to R and S.
      REAL(DOUBLE)                    :: DUM(2,ELGP)
      REAL(DOUBLE)                    :: DUM2(ELGP)

      
! **********************************************************************************************************************************

                                                           ! Shape function derivatives at R,S
      IF ((TYPE(1:5) == 'QUAD4') .OR. (TYPE(1:5) == 'QUAD8')) THEN

         CALL SHP2DQ ( 0, 0, ELGP, 'MITC_SHAPE_FUNCTIONS', '', 0, R, S, 'N', PSH, DPSHG )

                                                           ! Change node numbering to match Bathe's MITC4+ paper.
         IF (TYPE(1:5) == 'QUAD4') THEN
            DUM = DPSHG
            DPSHG(:,1) = DUM(:,3)
            DPSHG(:,2) = DUM(:,4)
            DPSHG(:,3) = DUM(:,1)
            DPSHG(:,4) = DUM(:,2)
            DUM2 = PSH
            PSH(1) = DUM2(3)
            PSH(2) = DUM2(4)
            PSH(3) = DUM2(1)
            PSH(4) = DUM2(2)
         ENDIF

      ELSE

        WRITE(ERR,*) ' *ERROR: INCORRECT ELEMENT TYPE ', TYPE
        WRITE(F06,*) ' *ERROR: INCORRECT ELEMENT TYPE ', TYPE
        FATAL_ERR = FATAL_ERR + 1
        CALL OUTA_HERE ( 'Y' )

      ENDIF



      RETURN


! **********************************************************************************************************************************
  
      END SUBROUTINE MITC_SHAPE_FUNCTIONS
