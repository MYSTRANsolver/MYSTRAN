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

      SUBROUTINE EXPAND_PHIXA_TO_PHIXG

! Expand array PHIXA (whose cols are stored in array UA_COL) to G-set columns (UG_COL). Each UG_COL is a column of matrix PHIXG.
! The Craig-Bampton mode array PHIXA is described in the MYSTRAN User's Reference Manual, Appendix D

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ONE
      USE IOUNT1, ONLY                :  ERR, F04, F06, L5B, SC1, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, LINKNO, NDOFA, NDOFF, NDOFG, NDOFM, NDOFN, NDOFO, NDOFR, NDOFS, NTERM_PHIXA,&
                                         NTERM_PHIXG, NVEC, SOL_NAME
      USE TIMDAT, ONLY                :  YEAR, MONTH, DAY, HOUR, MINUTE, SEC, SFRAC, STIME, TSEC
      USE COL_VECS, ONLY              :  UA_COL, UG_COL
      USE PARAMS, ONLY                :  EPSIL, TINY
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SPARSE_MATRICES, ONLY       :  I_PHIXA, J_PHIXA, PHIXA, I_PHIXG, J_PHIXG, PHIXG  
      USE SUBR_BEGEND_LEVELS, ONLY    :  EXPAND_PHIXA_TO_PHIXG_BEGEND
      USE EXPAND_PHIXA_TO_PHIXG_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'EXPAND_PHIXA_TO_PHIXG'
      CHARACTER(54*BYTE)              :: MODNAM            ! Name to write to screen to describe module being run
      CHARACTER( 1*BYTE)              :: NULL_COL          ! = 'Y' if col of PHIXA is null

      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = EXPAND_PHIXA_TO_PHIXG_BEGEND

      REAL(DOUBLE)                    :: PHIXG_FULL(NDOFG,NDOFR+NVEC)
!                                                          ! Full representation of matrix PHIXG before converting to sparse matrix

      REAL(DOUBLE)                    :: SMALL             ! A number used in filtering out small numbers from a full matrix

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Expand PHIXA (cols stored in UA_COL) to G-set columns (UG_COL). Each UG_COL is a column of matrix PHIXG

      DO J=1,NDOFR+NVEC
                                                           ! Put a col of PHIXA (A-set) into UA_COL
         CALL ALLOCATE_COL_VEC ( 'UA_COL', NDOFA, SUBR_NAME )
         CALL GET_SPARSE_CRS_COL ( 'PHIXA', J, NTERM_PHIXA, NDOFA, NDOFR+NVEC, I_PHIXA, J_PHIXA, PHIXA, ONE, UA_COL, NULL_COL )
                                                           ! Build F-set from A and O-set
         CALL ALLOCATE_COL_VEC ( 'UF_COL' , NDOFF, SUBR_NAME )
         CALL ALLOCATE_COL_VEC ( 'UO_COL' , NDOFO, SUBR_NAME )
         CALL OURTIM
         MODNAM = 'BUILD UF DISPLS FROM UA, UO:                      "'
         WRITE(SC1,5093) LINKNO,MODNAM,J,HOUR,MINUTE,SEC,SFRAC
         CALL BUILD_F_AO
         CALL DEALLOCATE_COL_VEC ( 'UA_COL' )
         CALL DEALLOCATE_COL_VEC ( 'UO_COL' )
                                                           ! Build N-set from F and S-set 
         CALL ALLOCATE_COL_VEC ( 'UN_COL', NDOFN, SUBR_NAME)
         CALL ALLOCATE_COL_VEC ( 'US_COL', NDOFS, SUBR_NAME )
         CALL OURTIM
         MODNAM = 'BUILD UN DISPLS FROM UF, US:                      "'
         WRITE(SC1,5093) LINKNO,MODNAM,J,HOUR,MINUTE,SEC,SFRAC
         CALL BUILD_N_FS
         CALL DEALLOCATE_COL_VEC ( 'UF_COL' )
         CALL DEALLOCATE_COL_VEC ( 'US_COL' )
                                                           ! Build G-set from N and M-set
         CALL ALLOCATE_COL_VEC ( 'UG_COL', NDOFG, SUBR_NAME )
         CALL ALLOCATE_COL_VEC ( 'UM_COL', NDOFM, SUBR_NAME )
         CALL OURTIM
         MODNAM = 'BUILD UG DISPLS FROM UN, UM:                      "'
         WRITE(SC1,5093) LINKNO,MODNAM,J,HOUR,MINUTE,SEC,SFRAC
         CALL BUILD_G_NM
         CALL DEALLOCATE_COL_VEC ( 'UN_COL' )
         CALL DEALLOCATE_COL_VEC ( 'UM_COL' )

         CALL OURTIM                                       ! Write UG displs for this subcase to file LINK5A
         MODNAM = 'WRITE PHIXG DISPLS TO FILE,                       "'
         WRITE(SC1,5093) LINKNO,MODNAM,J,HOUR,MINUTE,SEC,SFRAC
   !xx   WRITE(SC1, * )                                    ! Separator between UG_COL calcs
         DO I=1,NDOFG
            WRITE(L5B) UG_COL(I)                           ! For CB this is a col of PHIXG (which is never processed as an array)
         ENDDO

         DO I=1,NDOFG
            PHIXG_FULL(I,J) = UG_COL(I)
         ENDDO

         CALL DEALLOCATE_COL_VEC ( 'UG_COL' )

      ENDDO

! Convert full PHIXG to sparse

      CALL CNT_NONZ_IN_FULL_MAT ( 'PHIZG_FULL', PHIXG_FULL, NDOFG, NDOFR+NVEC, 'N', NTERM_PHIXG, SMALL )
      CALL ALLOCATE_SPARSE_MAT  ( 'PHIXG', NDOFG, NTERM_PHIXG, SUBR_NAME )
      CALL FULL_TO_SPARSE_CRS   ( 'PHIXG_FULL', NDOFG, NDOFR+NVEC, PHIXG_FULL, NTERM_PHIXG, SMALL, SUBR_NAME, 'N',                 &
                                   I_PHIXG, J_PHIXG, PHIXG )

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(' *INFORMATION: TERMS WHOSE ABS VALUE ARE < MACH_PREC =',1ES10.3,' ARE NOT INCLUDED IN MATRIX ',A,' IN SUBR ',A       &
                    ,/,14X,' AS THIS FULL MATRIX IS BEING CONVERTED TO A SPARSE MATRIX')

  102 FORMAT(' *INFORMATION: TERMS WHOSE ABS VALUE ARE < PARAM TINY =',1ES10.3,' ARE NOT INCLUDED IN MATRIX ',A,' IN SUBR ',A      &
                    ,/,14X,' AS THIS FULL MATRIX IS BEING CONVERTED TO A SPARSE MATRIX')

 5093 FORMAT(1X,I2,'/',A54,I8,2X,I2,':',I2,':',I2,'.',I3)

99885 FORMAT(82X,'MATRIX PHIXG',/,82X,'------------')                                                                             

99886 FORMAT(5X,32676(I14))                                                                                                       

99887 FORMAT(I8,'-',I1,32767(1ES14.6))                                                                                            

99888 FORMAT(8X,'-',I1,32767(1ES14.6))                                                                                            

! **********************************************************************************************************************************

      END SUBROUTINE EXPAND_PHIXA_TO_PHIXG


