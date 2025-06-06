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
      SUBROUTINE MITC_CONTRAVARIANT_BASIS ( G_R, G_S, G_T, G_1, G_2, G_3 )
 
! Calculates the contravariant basis vectors g^1, g^2, g^3 to the specified covariant basis vectors
! G_R, G_S, G_T. This is the inverse Jacobian matrix where G_R/S/T are the columns of the Jacobian matrix.

      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE

      USE CROSS_Interface
      
      IMPLICIT NONE 
      
      INTEGER(LONG)                   :: I                 ! DO loop index

      REAL(DOUBLE) , INTENT(IN)       :: G_R(3)            ! Covariant basis vector g_r in basic coordinates
      REAL(DOUBLE) , INTENT(IN)       :: G_S(3)            ! Covariant basis vector g_s in basic coordinates
      REAL(DOUBLE) , INTENT(IN)       :: G_T(3)            ! Covariant basis vector g_t in basic coordinates
      REAL(DOUBLE) , INTENT(OUT)      :: G_1(3)            ! Contravariant basis vector g^1 in basic coordinates
      REAL(DOUBLE) , INTENT(OUT)      :: G_2(3)            ! Contravariant basis vector g^2 in basic coordinates
      REAL(DOUBLE) , INTENT(OUT)      :: G_3(3)            ! Contravariant basis vector g^3 in basic coordinates
      REAL(DOUBLE)                    :: DUM1(3)

      REAL(DOUBLE)                    :: DETJ


! **********************************************************************************************************************************

                                                           ! DET(J) = G_R . (G_S x G_T)
      CALL CROSS(G_S, G_T, DUM1)
      DETJ = G_R(1)*DUM1(1) + G_R(2)*DUM1(2) + G_R(3)*DUM1(3)

      CALL CROSS(G_S, G_T, G_1)
      CALL CROSS(G_T, G_R, G_2)
      CALL CROSS(G_R, G_S, G_3)

      DO I=1,3
        G_1(I) = G_1(I) / DETJ
        G_2(I) = G_2(I) / DETJ
        G_3(I) = G_3(I) / DETJ
      ENDDO

      RETURN


! **********************************************************************************************************************************
  
      END SUBROUTINE MITC_CONTRAVARIANT_BASIS
