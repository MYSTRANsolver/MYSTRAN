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

      SUBROUTINE WRITE_USERIN_BD_CARDS ( NROWS, X_SET )

! Write out B.D. CUSERIN card images, created in MYSTRAN for use in INPUT4 for a substructure, if user has requested it via Bulk
! Data PARAM CUSERIN, 

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  ERR, F04, F06, F06FIL, MOU4, OU4, OU4FIL
      USE SCONTR, ONLY                :  JCARD_LEN, NCORD, NDOFG, NGRID, NVEC, WARN_ERR, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  START_YEAR, START_MONTH, START_DAY, START_HOUR, START_MINUTE, START_SEC, START_SFRAC
      USE DOF_TABLES, ONLY            :  TDOFI
      USE PARAMS, ONLY                :  CUSERIN_EID, CUSERIN_IN4, CUSERIN_PID, CUSERIN_SPNT_ID, CUSERIN_XSET,                     &
                                         CUSERIN_COMPTYP, SUPWARN
      USE MODEL_STUF, ONLY            :  CORD, RCORD, GRID_ID, GRID, RGRID 
      USE OUTPUT4_MATRICES, ONLY      :  ACT_OU4_OUTPUT_NAMES, NUM_OU4_REQUESTS, OU4_FILE_UNITS
      USE WRITE_USERIN_BD_CARDS_USE_IFs

      IMPLICIT NONE

      INTEGER(LONG), INTENT(IN)       :: NROWS                ! Size that is at least as large as any of the arrays below need

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_USERIN_BD_ENTRIES'
      CHARACTER(LEN=*), INTENT(IN)    :: X_SET                ! Displ set that the USERIN element is connected to
      CHARACTER( 1*BYTE)              :: COMPS(6)             ! Array of displ comps for 1 grid in the USERIN_GRIDS array
      CHARACTER( 1*BYTE)              :: CORD_FND             ! Indicator of whether we found a coord sys in array CORD

      CHARACTER( 8*BYTE)              :: CORD_MSG = '"CID0"  '! Field 6 of CUSERIN and field 3 of CORD2R to indicate to user that
!                                                               this should be replaced by act coord sys ID which defines USERIN
!                                                               element basic coord sys relative to the OA model basic coord sys

      CHARACTER(LEN=JCARD_LEN)        :: ICARD(NROWS,9)       ! Char array for fields 2-9 of CUSERIN or PUSERIN B.D. entries
      CHARACTER( 8*BYTE)              :: USERIN_COMPS(NROWS)  ! Char array of all of the displ comps for 1 USERIN_GRID

      INTEGER(LONG)                   :: CORD_ID              ! ID of a coord system on a grid for the global coord sys ID
      INTEGER(LONG)                   :: FIRST                ! First of the USERIN_CORDS that is nonzero
      INTEGER(LONG)                   :: I,J                  ! DO loop indices
      INTEGER(LONG)                   :: IROW                 ! Row number in array GRID_ID
      INTEGER(LONG)                   :: ICORD                ! Internal coord sys ID
      INTEGER(LONG)                   :: K                    ! Counter
      INTEGER(LONG)                   :: NUM_LEFT             ! Counter
      INTEGER(LONG)                   :: NUM_USERIN_GRIDS     ! Number of different R set grids
      INTEGER(LONG)                   :: X_SET_COMPS(NROWS)   ! Array of displ components in the R set
      INTEGER(LONG)                   :: X_SET_GRIDS(NROWS)   ! Array of grids in the R set
      INTEGER(LONG)                   :: X_SET_COL            ! Col in TDOF where R set exists
      INTEGER(LONG)                   :: USERIN_CORDS(NROWS,2)! Array of global coord ID's from X_SET_GRIDS (act, int ID's)
      INTEGER(LONG)                   :: USERIN_GRIDS(NROWS)  ! Array of different grid values from X_SET_GRIDS

! **********************************************************************************************************************************
      CALL TDOF_COL_NUM ( X_SET, X_SET_COL )

! Initialize

      DO I=1,NROWS
         X_SET_GRIDS(I)    = 0
         X_SET_COMPS(I)    = 0
         USERIN_COMPS(I)   = '        '
         USERIN_GRIDS(I)   = 0
         USERIN_CORDS(I,1) = 0   ;   USERIN_CORDS(I,2) = 0
      ENDDO

! Get array of X set grids and displ comps

      K = 0
      DO I=1, NDOFG
         IF (TDOFI(I,X_SET_COL) > 0) THEN
            K = K + 1
            X_SET_GRIDS(K) = TDOFI(I,1)
            X_SET_COMPS(K) = TDOFI(I,2)
         ENDIF
      ENDDO


! Determine number of different X_SET_GRIDS

      NUM_USERIN_GRIDS = 1
      USERIN_GRIDS(1) = X_SET_GRIDS(1)
      DO I=2,NROWS
         IF (X_SET_GRIDS(I) /= X_SET_GRIDS(I-1)) THEN
            NUM_USERIN_GRIDS = NUM_USERIN_GRIDS + 1
            USERIN_GRIDS(NUM_USERIN_GRIDS) = X_SET_GRIDS(I)
         ENDIF
      ENDDO


! Get list of displ comps for each of the different grids in USERIN_GRIDS

      DO I=1,NUM_USERIN_GRIDS

         USERIN_COMPS(I) = '        '

         DO J=1,6
            COMPS(J) = ' '
         ENDDO

         DO J=1,NROWS
            IF (X_SET_GRIDS(J) == USERIN_GRIDS(I)) THEN
               IF      (X_SET_COMPS(J) == 1) THEN
                  COMPS(1) = '1'
               ELSE IF (X_SET_COMPS(J) == 2) THEN
                  COMPS(2) = '2'
               ELSE IF (X_SET_COMPS(J) == 3) THEN
                  COMPS(3) = '3'
               ELSE IF (X_SET_COMPS(J) == 4) THEN
                  COMPS(4) = '4'
               ELSE IF (X_SET_COMPS(J) == 5) THEN
                  COMPS(5) = '5'
               ELSE IF (X_SET_COMPS(J) == 6) THEN
                  COMPS(6) = '6'
               ENDIF
            ENDIF
         ENDDO

         IF (CUSERIN_COMPTYP == 0) THEN
            K = 0
            DO J=1,6
               IF (COMPS(J) /= ' ') THEN
                  K = K + 1
                  USERIN_COMPS(I)(K:K) = COMPS(J)
               ENDIF
            ENDDO
         ELSE
            DO J=1,6
               USERIN_COMPS(I)(J:J) = COMPS(J)
            ENDDO
         ENDIF   

      ENDDO


! Get list of different global coord systems (NOTE: we don't need input coord systems since the GRID B.D. entries written out
! below have grid coords in basic system; which is the way RGRID is after subr GRID_PROC runs)

      DO I=1,NUM_USERIN_GRIDS
         CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, USERIN_GRIDS(I), IROW )
         CORD_ID = GRID(IROW,3)
         USERIN_CORDS(i,1) = CORD_ID
      ENDDO

! Now get the internal ID for the actual coord sys ID's in USERIN_CORDS(I,1)

      DO I=1,NUM_USERIN_GRIDS
         IF (USERIN_CORDS(I,1) /= 0) THEN
            CORD_FND = 'N'
               DO J=1,NCORD
               IF (USERIN_CORDS(I,1) == CORD(J,2)) THEN
                  CORD_FND = 'Y'
                  USERIN_CORDS(I,2) = J
                  EXIT
               ENDIF
            ENDDO
            IF (CORD_FND == 'N') THEN
               WARN_ERR = WARN_ERR + 1
               WRITE(ERR,9988) USERIN_CORDS(I,1), SUBR_NAME
               IF (SUPWARN == 'N') THEN 
                  WRITE(F06,9988) USERIN_CORDS(I,1), SUBR_NAME
               ENDIF
            ENDIF
         ENDIF
      ENDDO


      WRITE(F06,*)
      WRITE(F06,101)
      WRITE(F06,103) CUSERIN_EID, CUSERIN_XSET, F06FIL, START_MONTH, START_DAY, START_YEAR, START_HOUR, START_MINUTE,      &
                     START_SEC, START_SFRAC
      WRITE(F06,104)

      DO I=1,NROWS
         ICARD(I,1) = '        '
         ICARD(I,2) = '        '
      ENDDO

! Write grid entries

      WRITE(F06,*)
      DO I=1,NUM_USERIN_GRIDS

         CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, USERIN_GRIDS(I), IROW )
                                                           ! Grid ID
         WRITE(ICARD(I,2),201) USERIN_GRIDS(I)        ;   CALL LEFT_ADJ_BDFLD ( ICARD(I,2) )

         WRITE(ICARD(I,3),203) CORD_MSG

         DO J=1,3                                          ! Grid coordinates
            WRITE(ICARD(I,J+3),204) RGRID(I,J)        ;   CALL LEFT_ADJ_BDFLD ( ICARD(I,J+3) )
         ENDDO

         IF (GRID(I,3) /= 0) THEN                          ! Global coord sys
            WRITE(ICARD(I,7),201) GRID(I,3)           ;   CALL LEFT_ADJ_BDFLD ( ICARD(I,7) )
         ELSE
            WRITE(ICARD(I,7),203) '        '
         ENDIF

         WRITE(F06,205) (ICARD(I,J),J=2,7)

      ENDDO
      WRITE(F06,*)

! Write CORD entries (for grid global coord systems)

      FIRST = 1
      DO I=1,NUM_USERIN_GRIDS
         IF (USERIN_CORDS(I,1) /= 0) THEN
            FIRST = I
            EXIT
         ENDIF
      ENDDO

      IF (USERIN_CORDS(FIRST,1) /= 0) THEN

         WRITE(ICARD(FIRST,2),201) USERIN_CORDS(FIRST,1)      ;   CALL LEFT_ADJ_BDFLD ( ICARD(FIRST,2) )

         WRITE(ICARD(FIRST,3),203) CORD_MSG

         ICORD = USERIN_CORDS(FIRST,2)
         DO J=1,3
            WRITE(ICARD(FIRST,J+3),204) RCORD(ICORD,J)        ;   CALL LEFT_ADJ_BDFLD ( ICARD(FIRST,J+3) )
         ENDDO

         WRITE(ICARD(FIRST,7),204) RCORD(ICORD, 6)            ;   CALL LEFT_ADJ_BDFLD ( ICARD(FIRST,7) )   
         WRITE(ICARD(FIRST,8),204) RCORD(ICORD, 9)            ;   CALL LEFT_ADJ_BDFLD ( ICARD(FIRST,8) )   
         WRITE(ICARD(FIRST,9),204) RCORD(ICORD,12)            ;   CALL LEFT_ADJ_BDFLD ( ICARD(FIRST,9) )   

         WRITE(F06,206) (ICARD(FIRST,J),J=2,9)

         WRITE(ICARD(FIRST,2),204) RCORD(ICORD, 4)            ;   CALL LEFT_ADJ_BDFLD ( ICARD(FIRST,2) )   
         WRITE(ICARD(FIRST,3),204) RCORD(ICORD, 7)            ;   CALL LEFT_ADJ_BDFLD ( ICARD(FIRST,3) )   
         WRITE(ICARD(FIRST,4),204) RCORD(ICORD,10)            ;   CALL LEFT_ADJ_BDFLD ( ICARD(FIRST,4) )   

         WRITE(F06,207) (ICARD(FIRST,J),J=2,4)

      ENDIF

      DO I=FIRST+1,NUM_USERIN_GRIDS

         IF ((USERIN_CORDS(I,1) /= USERIN_CORDS(I-1,1)) .AND. (USERIN_CORDS(I,1) /= 0)) THEN

            WRITE(ICARD(I,2),201) USERIN_CORDS(I,1)        ;   CALL LEFT_ADJ_BDFLD ( ICARD(I,2) )

            WRITE(ICARD(I,3),203) CORD_MSG

            ICORD = USERIN_CORDS(I,2)
            DO J=1,3
               WRITE(ICARD(I,J+3),203) '0.      '
            ENDDO

            DO J=1,3
               WRITE(ICARD(I,J+6),204) RCORD(ICORD,J+9)    ;   CALL LEFT_ADJ_BDFLD ( ICARD(I,J+6) )   
            ENDDO

            WRITE(F06,206) (ICARD(I,J),J=2,9)

            DO J=1,3
               WRITE(ICARD(I,J+1),204) RCORD(ICORD,J+3)    ;   CALL LEFT_ADJ_BDFLD ( ICARD(I,J+3) )   
            ENDDO

            WRITE(F06,207) (ICARD(I,J),J=2,4)

         ENDIF

      ENDDO
      WRITE(F06,*)

! Write SPOINT entry

      IF (NVEC > 0) THEN
         WRITE(ICARD(2,1),201) CUSERIN_SPNT_ID             ;   CALL LEFT_ADJ_BDFLD ( ICARD(2,1) )
         WRITE(ICARD(4,1),201) CUSERIN_SPNT_ID+NVEC-1      ;   CALL LEFT_ADJ_BDFLD ( ICARD(4,1) )
         WRITE(F06,211) ICARD(2,1), ICARD(4,1)
      ENDIF

! Write CUSERIN entry

      WRITE(ICARD(2,1),201) CUSERIN_EID                    ;   CALL LEFT_ADJ_BDFLD ( ICARD(2,1) )
      WRITE(ICARD(3,1),201) CUSERIN_PID                    ;   CALL LEFT_ADJ_BDFLD ( ICARD(3,1) )
      WRITE(ICARD(4,1),201) NUM_USERIN_GRIDS               ;   CALL LEFT_ADJ_BDFLD ( ICARD(4,1) )
      WRITE(ICARD(5,1),201) NVEC                           ;   CALL LEFT_ADJ_BDFLD ( ICARD(5,1) )
      WRITE(ICARD(6,1),203) CORD_MSG
      WRITE(F06,202) (ICARD(I,1),I=2,6)
                   
      DO I=1,NUM_USERIN_GRIDS                              ! Write CUSERIN cont cards for grids to internal file (ICARD)
         WRITE(ICARD(I,1),201) USERIN_GRIDS(I)             ;   CALL LEFT_ADJ_BDFLD ( ICARD(I,1) )
         WRITE(ICARD(I,2),203) USERIN_COMPS(I)             ;   CALL LEFT_ADJ_BDFLD ( ICARD(I,2) )
      ENDDO

      NUM_LEFT = NUM_USERIN_GRIDS                          ! Write grids/comps (from internal file ICARD) to F06
      DO I=1,NUM_USERIN_GRIDS,4

         IF      (NUM_LEFT >=  4) THEN
            WRITE(F06,224) ICARD(I,1)  , ICARD(I,2)  , ICARD(I+1,1), ICARD(I+1,2),                                                 &
                           ICARD(I+2,1), ICARD(I+2,2), ICARD(I+3,1), ICARD(I+3,2)
            NUM_LEFT = NUM_LEFT - 4

         ELSE IF (NUM_LEFT == 3) THEN 
            WRITE(F06,223) ICARD(I,1)  , ICARD(I,2)  , ICARD(I+1,1), ICARD(I+1,2),                                                 &
                           ICARD(I+2,1), ICARD(I+2,2)

         ELSE IF (NUM_LEFT == 2) THEN 
            WRITE(F06,222) ICARD(I,1)  , ICARD(I,2)  , ICARD(I+1,1), ICARD(I+1,2)

         ELSE IF (NUM_LEFT == 1) THEN 
            WRITE(F06,222) ICARD(I,1)  , ICARD(I,2)

         ENDIF

      ENDDO

! Write PUSERIN entry

      WRITE(F06,*)
      WRITE(ICARD(2,1),201) CUSERIN_PID                    ;   CALL LEFT_ADJ_BDFLD ( ICARD(2,1) )
      WRITE(ICARD(3,1),201) CUSERIN_IN4                    ;   CALL LEFT_ADJ_BDFLD ( ICARD(3,1) )
      WRITE(F06,241) ICARD(2,1), ICARD(3,1)

! Write message regarding CORD_MSG

      WRITE(F06,298)
      WRITE(F06,299) CORD_MSG
      WRITE(F06,*)
      WRITE(F06,399)
      WRITE(F06,101)

! **********************************************************************************************************************************
  101 FORMAT('$*******************************************************************************')

  103 FORMAT('$ B U L K   D A T A   E N T R I E S   F O R   C U S E R I N   E L E M',I8      ,/,                                   &
             '$      (to be used to define a substructure in an overall systems model)'      ,//,                                  &
             '$ The GRID and CUSERIN entries below are for the ',A,'-set from file:',/,                                            &
             '$ ',A,/,                                                                                                             &
             '$ run on ',I2,'/',I2,'/',I4,' at ',I2,':',I2,':',I2,'.',I3)

  104 FORMAT('$--1---|---2---|---3---|---4---|---5---|---6---|---7---|---8---|---9---|--10---|')

  201 FORMAT(I8)

  202 FORMAT('CUSERIN ',5A8)

  203 FORMAT(A8)

  204 FORMAT(G8.1)

  205 FORMAT('GRID    ',6A8)

  206 FORMAT('CORD2R  ',8A8)

  207 FORMAT('        ',3A8)

  211 FORMAT('SPOINT  ',A8,'THRU    ',A8,/)

  221 FORMAT(8X,2A8)

  222 FORMAT(8X,4A8)

  223 FORMAT(8X,6A8)

  224 FORMAT(8X,8A8)

  241 FORMAT('PUSERIN ',2A8,'<-"mat 1 name"-><-"mat 2 name"-><-"mat 3 name"->',/)

  298 FORMAT('$ NOTES:',/,'$ -----')

  299 FORMAT('$ ',A,' is to be replaced with the coord sys ID that is used to define the'    ,/,                                   &
             '$ basic coord sys of this USERIN elem rel to the system model basic coord     ',/,                                   &
             '$ system in the system model run'                                              ,//,                                  &
             '$ If the above grid entries are used, and are different than the corresponding',/,                                   &
             '$ grids in the system model, RBE2''s should be included to connect them to the',/,                                   &
             '$ corresponding grids in the system model.'                                    ,/,                                   &
             '$ **NOTE: If RBE2''s are NOT used, it is imperative that the grids in the     ',/,                                   &
             '$ system model that this USERIN element is connected to have the same global  ',/,                                   &
             '$ coordinate system as was used in generating this substructure.')

  399 FORMAT('$ name 1 is to be replaced with the stiffness matrix name:'                    ,/,                                   &
             '$        For CB model generation, KXX or its alias, KRRGN'                     ,/,                                   &
             '$        For statics, KGG, KAA, etc'                                           ,//,                                  &
             '$ name 2 is to be replaced with the mass matrix name (if one is input):'       ,/,                                   &
             '$        For CB model generation, MXX or its alias, MRRGN'                     ,/,                                   &
             '$        For statics, MGG, MAA, etc'                                           ,//,                                  &
             '$ name 3 is to be replaced with:'                                              ,/,                                   &
             '$        For CB model generation, RBM0 (not required)'                         ,/,                                   &
             '$        For statics, load matrix PG, PA, etc'                                 ,//,                                  &
             '$ The matrices whose names are "name i" must have been requested to be'        ,/,                                   &
             '$ written to binary files via Exec Control OUTPUT4 statement(s) in this run'   ,//,                                  &
             '$ Finally, make sure that the real numbers above have enough decimal places to',/,                                   &
             '$ accurately represent the quantities. Otherwise replace them before using them')
             

 9988 FORMAT(' *WARNING    : CANNOT FIND COORD SYSTEM ',I8,' IN SUBR ',A)





! **********************************************************************************************************************************

      END SUBROUTINE WRITE_USERIN_BD_CARDS

