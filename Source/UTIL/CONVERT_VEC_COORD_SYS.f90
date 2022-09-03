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

      SUBROUTINE CONVERT_VEC_COORD_SYS ( MESSAG, INPUT_VEC, OUTPUT_VEC, NCID )

! Convert coordinate system of a G-set vector. The input vector is in global coords and the output vector is in one system for all
! grids. The input vector is first converted to basic coords and then to the final output system, if that system is not the basic
! system.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NCORD, NDOFG, NGRID
      USE TIMDAT, ONLY                :  TSEC
      USE MODEL_STUF, ONLY            :  CORD, RCORD, GRID, GRID_ID, INV_GRID_SEQ
      USE SUBR_BEGEND_LEVELS, ONLY    :  CONVERT_VEC_COORD_SYS_BEGEND

      USE CONVERT_VEC_COORD_SYS_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'CONVERT_VEC_COORD_SYS'
      CHARACTER(LEN=*), INTENT(IN)    :: MESSAG            ! Text description of INPUT_VEC in case of undefined NCID
      CHARACTER( 1*BYTE)              :: CORD_FND          ! 'Y' if the internal coordinate system number for GCID, NCID is found

      INTEGER(LONG), INTENT(IN)       :: NCID              ! Actual coord system number. INPUT_VEC is to be transformed to this sys.
      INTEGER(LONG)                   :: AGRID             ! Actual grid number for the 6 components of vector being transformed
      INTEGER(LONG)                   :: GCID              ! Global actual coord system number for AGRID
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM   ! Row number in array GRID_ID where AGRID exists
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: JCORD             ! Internal coord system number for either GCID or NCID
      INTEGER(LONG)                   :: JFLD              ! Used in error message to indicate a coord sys ID undefined
      INTEGER(LONG)                   :: NUM_COMPS         ! No. displ components (1 for SPOINT, 6 for actual grid)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CONVERT_VEC_COORD_SYS_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: INPUT_VEC(NDOFG)  ! G-set input vector to be transformed from global to NCID
      REAL(DOUBLE), INTENT(OUT)       :: OUTPUT_VEC(NDOFG) ! Transformed output vector
      REAL(DOUBLE)                    :: DUM_GVEC_T(6)     ! Vector for the 3 translation components of 1 grid
      REAL(DOUBLE)                    :: DUM_GVEC_R(6)     ! Vector for the 3 rotation    components of 1 grid
      REAL(DOUBLE)                    :: INPUT_GVEC_T(6)   ! Vector for the 3 translation components of 1 grid
      REAL(DOUBLE)                    :: INPUT_GVEC_R(6)   ! Vector for the 3 rotation    components of 1 grid
      REAL(DOUBLE)                    :: OUTPUT_GVEC_T(6)  ! Vector for the 3 translation components of 1 grid
      REAL(DOUBLE)                    :: OUTPUT_GVEC_R(6)  ! Vector for the 3 rotation    components of 1 grid
      REAL(DOUBLE)                    :: PHID              ! Dummy arg for subr GEN_T0L that is not used here
      REAL(DOUBLE)                    :: THETAD            ! Dummy arg for subr GEN_T0L that is not used here
      REAL(DOUBLE)                    :: T_0_GCID(3,3)     ! Coord transformation matrix from basic to GCID system
      REAL(DOUBLE)                    :: T_0_NCID(3,3)     ! Coord transformation matrix from basic to NCID system

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Set OUTPUT_VEC = INPUT_VEC in case no transformation is done (e.g. all grids have basic global and transformed system is basic)


      DO I=1,NDOFG
         OUTPUT_VEC(I) = INPUT_VEC(I)
      ENDDO

! Transform each grid of INPUT_VEC to basic coords in OUTPUT_VEC (unless it is in basic already)

      DO K=1,NGRID

         AGRID = GRID_ID(INV_GRID_SEQ(K))
         CALL GET_GRID_NUM_COMPS ( AGRID, NUM_COMPS, SUBR_NAME )
         IF (NUM_COMPS == 6) THEN                          ! Only 6 comp grids need transforming

            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID, GRID_ID_ROW_NUM )
            GCID = GRID(GRID_ID_ROW_NUM,3)

            IF (GCID > 0) THEN

               CORD_FND = 'N'
j_do_1:        DO J=1,NCORD                                ! GCID should be a valid coord sys no. It was checked in subr GRID_PROC
                  IF (GCID == CORD(J,2)) THEN
                     JCORD = J
                     CORD_FND = 'Y'
                     EXIT j_do_1
                  ENDIF
               ENDDO j_do_1

               IF (CORD_FND == 'Y') THEN

                  DO J=1,3
                     INPUT_GVEC_T(J) = INPUT_VEC(6*(K-1)+J)
                     INPUT_GVEC_R(J) = INPUT_VEC(6*(K-1)+J+3)
                  ENDDO

                  CALL GEN_T0L ( GRID_ID_ROW_NUM, JCORD, THETAD, PHID, T_0_GCID )
 
                  CALL MATMULT_FFF ( T_0_GCID, INPUT_GVEC_T, 3, 3, 1, OUTPUT_GVEC_T )
                  CALL MATMULT_FFF ( T_0_GCID, INPUT_GVEC_R, 3, 3, 1, OUTPUT_GVEC_R )

                  DO J=1,3
                     OUTPUT_VEC(6*(K-1)+J)   = OUTPUT_GVEC_T(J)
                     OUTPUT_VEC(6*(K-1)+J+3) = OUTPUT_GVEC_R(J)
                  ENDDO


               ELSE                                        ! Should not get here (or pgm error) since GCID has been checked to exist

                  JFLD = 7
                  WRITE(ERR,910) GCID, JFLD, GRID(K,1), JFLD
                  WRITE(F06,910) GCID, JFLD, GRID(K,1), JFLD
                  CALL OUTA_HERE ( 'Y' )

               ENDIF

            ENDIF

         ENDIF

      ENDDO

! Transform OUTPUT_VEC to NCID if it is not basic

      IF (NCID /= 0) THEN
      
         CORD_FND = 'N'
j_do_2:  DO J=1,NCORD
           IF (NCID == CORD(J,2)) THEN
               JCORD = J
               CORD_FND = 'Y'
               EXIT j_do_2
            ENDIF
         ENDDO j_do_2

         IF (CORD_FND == 'Y') THEN

            DO K=1,NGRID

               AGRID = GRID_ID(INV_GRID_SEQ(K))
               CALL GET_GRID_NUM_COMPS ( AGRID, NUM_COMPS, SUBR_NAME )

               IF (NUM_COMPS == 6) THEN                          ! Only 6 comp grids need transforming

                  CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID, GRID_ID_ROW_NUM )


                  DO J=1,3
                     OUTPUT_GVEC_T(J) = OUTPUT_VEC(6*(K-1)+J)
                     OUTPUT_GVEC_R(J) = OUTPUT_VEC(6*(K-1)+J+3)
                  ENDDO

                  CALL GEN_T0L ( GRID_ID_ROW_NUM, JCORD, THETAD, PHID, T_0_NCID )
 
                  CALL MATMULT_FFF_T ( T_0_NCID, OUTPUT_GVEC_T, 3, 3, 1, DUM_GVEC_T )
                  CALL MATMULT_FFF_T ( T_0_NCID, OUTPUT_GVEC_R, 3, 3, 1, DUM_GVEC_R )

                  DO J=1,3
                     OUTPUT_VEC(6*(K-1)+J)   = DUM_GVEC_T(J)
                     OUTPUT_VEC(6*(K-1)+J+3) = DUM_GVEC_R(J)
                  ENDDO

               ENDIF

            ENDDO

         ELSE

            WRITE(ERR,912) NCID, MESSAG
            WRITE(F06,912) NCID, MESSAG
            CALL OUTA_HERE ( 'Y' )

         ENDIF

      ENDIF



! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  912 FORMAT(' *ERROR   912: COORDINATE SYSTEM ',I8,' DOES NOT EXIST. CANNOT TRANSFORM ',A                                         &
                    ,/,14X,' VECTOR FROM BASIC SYSTEM TO IT, SO VECTOR WILL BE LEFT IN BASIC COORD SYSTEM')

  910 FORMAT(' *ERROR   910: COORD SYSTEM',I8,' IN FIELD ',I2,' ON GRID ',I8,' (OR FROM GRDSET ENTRY FIELD ',I2,') IS UNDEFINED')

86954 format(' K, AGRID, J, INPUT_VEC(J), OUTPUT_VEC(J) = ',3i8,2(1es14.6))




! **********************************************************************************************************************************

      END SUBROUTINE CONVERT_VEC_COORD_SYS
