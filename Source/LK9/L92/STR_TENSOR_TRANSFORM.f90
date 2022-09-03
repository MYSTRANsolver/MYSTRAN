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

      SUBROUTINE STR_TENSOR_TRANSFORM ( STRESS_TENSOR, STRESS_CORD_SYS )

! Transforms an input stress or strain tensor in input local elem coord sys to an output coord sys whose actual coord sys ID is
! STRESS_CORD_SYS

! The 3x3 transform matrix Tse which transforms a unit vector in elem coords (Ue) to a unit vector in stress output coords (Us) is:

!                                                  Us = Tse*Ue                                                         (1)

! Tse can be written as the product of two transformation matrices Ts0 (transform vector in basic to output s coord sys) and
! the transpose of Te0 (which transforms a vector in basic coords to local elem coords).
! That is:

!                                                 Tse = Ts0*T0e = Ts0*Te0'                                             (2)
!  and its transpose is Tes:
!                                                 Tes = Te0*Ts0'                                                       (3)


! where Te0 is output in subr ELMGMi as array TE.  With (2), eqn (1) becomes:

!                                                  Us = (Ts0*Te0')*Ue                                                  (4)

! The 2nd order stress tensor in element local coords is a 3x3 matrix of the 9 (6 independent) stresses:

!                                                       | Sxx  Sxy  Sxz |
!                                                 See = | Syx  Syy  Syz |                                              (5)
!                                                 ~     | Szx  Szy  Szz |

! where Sij = Sji and, for example Sxx is the normal stress in the x direction, Sxy is the shear stress in the xy plane, etc.
! Matrix Tse is used to transform this 2nd order stress tensor from element coords to output coords (Soo) via the equation:

!                                                 Soo = Tse*See*Tse' = Tse'*See*Tes                                    (6)
!                                                 ~         ~              ~

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NCORD
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE TIMDAT, ONLY                :  TSEC
      USE MODEL_STUF, ONLY            :  CORD, RCORD, TE
      USE SUBR_BEGEND_LEVELS, ONLY    :  STR_TENSOR_TRANSFORM_BEGEND

      USE STR_TENSOR_TRANSFORM_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'STR_TENSOR_TRANSFORM'

      INTEGER(LONG), INTENT(IN)       :: STRESS_CORD_SYS   ! Actual coord system ID for stress/strain/engr force output
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: ICORD             ! Internal coord system ID for STRESS_CORD_SYS
      INTEGER(LONG)                   :: K                 ! Counter
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = STR_TENSOR_TRANSFORM_BEGEND

      REAL(DOUBLE), INTENT(INOUT)     :: STRESS_TENSOR(3,3)! 2D stress tensor (eqn 6 above)
      REAL(DOUBLE)                    :: DUM33(3,3)        ! Intermediate array used in calc outputs
      REAL(DOUBLE)                    :: TS0(3,3)          ! Transform matrix from basic coords to stress output coords
      REAL(DOUBLE)                    :: T0S(3,3)          ! TS0'
      REAL(DOUBLE)                    :: TES(3,3)          ! Transform matrix from local elem coords to stress output coords
      REAL(DOUBLE)                    :: TSE(3,3)          ! TEO'

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Get transformation matrix TS0 from stress coord sys to basic if it exists

      IF (STRESS_CORD_SYS == 0) THEN                       ! STRESS_CORD_SYS was basic so set T0o to the identity matrix

         DO I=1,3
            DO J=1,3
               T0S(I,J) = ZERO
            ENDDO
            T0S(I,I) = ONE
         ENDDO

      ELSE                                                 ! Need to transform ES mat'l matrix to basic coords

         ICORD = 0
         DO I=1,NCORD                                      ! Get the internal coord system ID for STRESS_CORD_SYS
            IF (STRESS_CORD_SYS == CORD(I,2)) THEN
               ICORD = I
               EXIT
            ENDIF
         ENDDO

      ENDIF

      IF (ICORD > 0) THEN                                  ! STRESS_CORD_SYS was found so do transformation

         DO I=1,3                                          ! Get TO0 from RCORD array        
            DO J=1,3
               K = 3 + 3*(I-1) + J
               TS0(I,J) = RCORD(ICORD,K)
               T0S(J,I) = TS0(I,J)
            ENDDO
         ENDDO

         CALL MATMULT_FFF ( TE , T0S  , 3, 3, 3, TES )     ! Calc TES then transform to get TSE
         DO I=1,3
            DO J=1,3
               TSE(I,J) = TES(J,I)
            ENDDO
         ENDDO
                                                           ! Transform input stress tensor from e coords to o coords
         CALL MATMULT_FFF (STRESS_TENSOR, TES, 3, 3, 3, DUM33 )
         CALL MATMULT_FFF (TSE, DUM33, 3, 3, 3, STRESS_TENSOR )

      ELSE                                                 ! STRESS_CORD_SYS was not found leave tensor in local elem system

         WRITE(F06,9101) STRESS_CORD_SYS

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 9101 FORMAT(' *INFORMATION: COORD SYSTEM ',I8,' FOR STRESS TRANSFORMATION IS UNDEFINED. LOCAL ELEMENT COORD SYSTEM WILL BE USED')

! **********************************************************************************************************************************

      END SUBROUTINE STR_TENSOR_TRANSFORM

