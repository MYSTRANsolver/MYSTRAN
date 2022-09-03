! ##################################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2019 Dr William R Case, Jr (dbcase29@gmail,com)                                              
                                                                                                         
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
 
      SUBROUTINE GET_PCOMP_SECT_PROPS ( PCOMP_TM, PCOMP_IB, PCOMP_TS )
 
! Calculates section properties, PCOMP_TM, PCOMP_IB, PCOMP_TS, for shell elements that have PCOMP properties. 

      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MPCOMP_PLIES, MPCOMP0, MRPCOMP_PLIES, MRPCOMP0
      USE IOUNT1, ONLY                :  F04, WRT_LOG
      USE MODEL_STUF, ONLY            :  EPROP, INTL_PID, NUM_PLIES, RPCOMP, TPLY
      USE PARAMS, ONLY                :  PCMPTSTM 
      USE CONSTANTS_1, ONLY           :  ZERO, THIRD
      USE TIMDAT, ONLY                :  TSEC 
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_PCOMP_SECT_PROPS_BEGEND

      USE GET_PCOMP_SECT_PROPS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GET_PCOMP_SECT_PROPS'

      INTEGER(LONG)                   :: K                  ! DO loop index
      INTEGER(LONG)                   :: PLY_RPCOMP_INDEX   ! Index in array RPCOMP where data for ply K begins
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_PCOMP_SECT_PROPS_BEGEND

      REAL(DOUBLE), INTENT(OUT)       :: PCOMP_TM           ! Membrane thickness of PCOMP for equivalent PSHELL
      REAL(DOUBLE), INTENT(OUT)       :: PCOMP_IB           ! Bending MOI of PCOMP for equivalent PSHELL
      REAL(DOUBLE), INTENT(OUT)       :: PCOMP_TS           ! Transverse shear thickness of PCOMP for equivalent PSHELL
      REAL(DOUBLE)                    :: ZBK,ZTK            ! Coord from ref plane to bot and top of ply K
      REAL(DOUBLE)                    :: ZBK2,ZTK2          ! ZBK^2, ZTK^2
      REAL(DOUBLE)                    :: ZBK3,ZTK3          ! ZBK^3, ZTK^3
 
! *********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGIN',F10.3)
      ENDIF

! **********************************************************************************************************************************
      ZBK      = EPROP(4)
      PCOMP_TM = ZERO
      PCOMP_IB = ZERO
      PCOMP_TS = ZERO

      DO K=1,NUM_PLIES
                                                        ! Indices in PCOMP, RPCOMP arrays where ply K data begins
         PLY_RPCOMP_INDEX = MRPCOMP0 + MRPCOMP_PLIES*(K - 1) + 1

         TPLY = RPCOMP(INTL_PID,PLY_RPCOMP_INDEX)       ! Ply thickness

         ZTK = ZBK + TPLY    ;    ZTK2 = ZTK*ZTK    ;    ZTK3 = ZTK2*ZTK    ;    ZBK2 = ZBK*ZBK    ;    ZBK3 = ZBK2*ZBK

         PCOMP_TM = PCOMP_TM + (ZTK  - ZBK )
         PCOMP_IB = PCOMP_IB + (ZTK3 - ZBK3)*THIRD
         PCOMP_TS = PCOMP_TS + (ZTK  - ZBK )*PCMPTSTM   ! PCMPTSTM is a factor (< 1) for shear to total plate thickness for PCOMP

         ZBK = ZTK

      ENDDO


! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

      END SUBROUTINE GET_PCOMP_SECT_PROPS
