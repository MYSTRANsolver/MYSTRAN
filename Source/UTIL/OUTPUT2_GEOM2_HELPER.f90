!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM2()
      ! Writes the GEOM2 / elements table
      ! for some reason, we need to write the SPOINTs here too
      !
      USE PENTIUM_II_KIND, ONLY       :  LONG, BYTE
      USE IOUNT1, ONLY                :  ERR, OP2
      
      ! GEOM2 - elements
      USE SCONTR, ONLY : NCTETRA4, NCTETRA10, NCPENTA6, NCPENTA15, NCHEXA8, NCHEXA20
      USE SCONTR, ONLY : NCQUAD4, NCQUAD4K, NCSHEAR, NCTRIA3, NCTRIA3K
      USE SCONTR, ONLY : NCROD, NCBAR, NCBEAM, NCBUSH, NCELAS1, NCELAS2, NCELAS3, NCELAS4
      USE SCONTR, ONLY : NCMASS, NCONM2

      USE SCONTR, ONLY     : NELE, NEDAT
      USE MODEL_STUF, ONLY : ETYPE, EOFF, EPNT

      IMPLICIT NONE
      INTEGER(LONG)                    :: ITABLE                        ! the subtable counter
      INTEGER(LONG)                    :: I,J                           ! counter
      CHARACTER(LEN=8*BYTE)            :: TABLE_NAME = "GEOM2"          ! the current op2 table name
      LOGICAL                          :: IS_GEOM2                      ! do we need to write the table

      INTEGER(LONG)  :: NCTETRA, NCPENTA, NCHEXA
      INTEGER(LONG), DIMENSION(NCELAS1, 6)  :: CELAS1
      INTEGER(LONG), DIMENSION(NCELAS2, 6)  :: CELAS2
      INTEGER(LONG), DIMENSION(NCELAS3, 4)  :: CELAS3
      INTEGER(LONG), DIMENSION(NCELAS4, 4)  :: CELAS4
      INTEGER(LONG), DIMENSION(NCROD, 4)    :: CROD
      INTEGER(LONG), DIMENSION(NCTRIA3, 4)  :: CTRIA3
      INTEGER(LONG), DIMENSION(NCQUAD4, 4)  :: CQUAD4
      INTEGER(LONG), DIMENSION(NCSHEAR, 4)  :: CSHEAR
!      INTEGER(LONG), DIMENSION(:, :), allocatable :: CTETRA
!      INTEGER(LONG), DIMENSION(:, :), allocatable :: CPENTA
!      INTEGER(LONG), DIMENSION(:, :), allocatable  :: CHEXA
      INTEGER(LONG)                         :: NCROD_ACTUAL = 0
      INTEGER(LONG)                         :: NCONROD_ACTUAL = 0
      INTEGER(LONG), DIMENSION(NCROD)       :: CROD_INDEX, CONROD_INDEX
     
      !CHARACTER( 8*BYTE), ALLOCATABLE :: ETYPE(:)    ! NELE  x 1 array of elem types
      !CHARACTER( 1*BYTE), ALLOCATABLE :: EOFF(:)     ! NELE  x 1 array of 'Y' for elem offsets or 'N' if not

      !INTEGER(LONG)     , ALLOCATABLE :: EDAT(:)     ! NEDAT x 1 array of elem connection data
      !INTEGER(LONG)     , ALLOCATABLE :: EPNT(:)     ! NELE  x 1 array of pointers to EDAT where data begins for an elem
      
      !DO I=1,NEDAT
      !ENDDO

      NCTETRA = NCTETRA4 + NCTETRA10
      NCPENTA = NCPENTA6 + NCPENTA15
      NCHEXA = NCHEXA8 + NCHEXA20

      IF((NCELAS1 > 0) .OR. (NCELAS2 > 0) .OR. (NCELAS3 > 0) .OR. (NCELAS4 > 0) .OR.        &
         (NCROD > 0) .OR. (NCBAR > 0) .OR. (NCBEAM > 0) .OR. (NCBUSH > 0) .OR.              &
         (NCSHEAR > 0) .OR. (NCTRIA3 > 0) .OR. (NCQUAD4 > 0) .OR.                           &
         (NCTETRA > 0) .OR. (NCPENTA > 0) .OR. (NCHEXA > 0)) THEN
        IS_GEOM2 = .TRUE.
      ENDIF


      IS_GEOM2 = .FALSE.
      IF (IS_GEOM2) THEN
!        ALLOCATE ( CTETRA(NCTETRA,12) )      
!        ALLOCATE ( CPENTA(NCPENTA,17) )      
!        ALLOCATE ( CHEXA(NCHEXA,22) )      

        CALL GET_GEOM2(CELAS1, CELAS2, CELAS3, CELAS4, &
                       CROD,                           &
                       CTRIA3, CQUAD4, CSHEAR,         &
!                       CTETRA, CPENTA, CHEXA,          &
                       NCTETRA, NCPENTA, NCHEXA,       &
                       NCROD_ACTUAL, NCONROD_ACTUAL,   &
                       CROD_INDEX, CONROD_INDEX)

        CALL WRITE_OP2_GEOM_HEADER(TABLE_NAME, ITABLE)

        CALL WRITE_OP2_GEOM2_CROD(ITABLE, CROD,                 &
                                  NCROD_ACTUAL, NCONROD_ACTUAL, &
                                  CROD_INDEX, CONROD_INDEX)
        !CALL WRITE_OP2_GEOM2_CELAS1(ITABLE, CELAS1)
        !CALL WRITE_OP2_GEOM2_CELAS2(ITABLE, CELAS2)
        CALL WRITE_OP2_GEOM2_CELAS3(ITABLE, CELAS3)
        CALL WRITE_OP2_GEOM2_CELAS4(ITABLE, CELAS4)
        !CALL WRITE_OP2_GEOM2_CSHEAR(ITABLE, CSHEAR)
        !CALL WRITE_OP2_GEOM2_CTRIA3(ITABLE, CTRIA3)
        !CALL WRITE_OP2_GEOM2_CQUAD4(ITABLE, CQUAD4)
        !CALL WRITE_OP2_GEOM2_CTETRA(ITABLE, CTETRA, NCTETRA)
      
!       CORD2S_INDEX(1000000)   ! intentional crash
        CALL END_OP2_GEOM_TABLE(ITABLE)
!        DEALLOCATE (CTETRA)
!        DEALLOCATE (CPENTA)
!        DEALLOCATE (CHEXA)
      ENDIF
      END SUBROUTINE WRITE_OP2_GEOM2

!===================================================================================================================================
      SUBROUTINE GET_GEOM2(CELAS1, CELAS2, CELAS3, CELAS4, &
                     CROD,                                 &
                     CTRIA3, CQUAD4, CSHEAR,               &
!                     CTETRA, CPENTA, CHEXA,                &
                     NCTETRA, NCPENTA, NCHEXA,             &
                     NCROD_ACTUAL, NCONROD_ACTUAL,         &
                     CROD_INDEX, CONROD_INDEX)
      ! breaks EDAT into simpler to use arrays
      USE PENTIUM_II_KIND, ONLY       :  LONG, BYTE
      USE IOUNT1, ONLY                :  ERR, OP2
      USE SCONTR, ONLY     : NCTETRA4, NCTETRA10, NCPENTA6, NCPENTA15, NCHEXA8, NCHEXA20
      USE SCONTR, ONLY     : NCQUAD4, NCQUAD4K, NCSHEAR, NCTRIA3, NCTRIA3K
      USE SCONTR, ONLY     : NCROD, NCBAR, NCBEAM, NCBUSH, NCELAS1, NCELAS2, NCELAS3, NCELAS4
      USE SCONTR, ONLY     : NCMASS, NCONM2
      USE SCONTR, ONLY     : NELE, NEDAT
      USE SCONTR, ONLY : NPROD
      USE MODEL_STUF, ONLY : ETYPE, EOFF, EPNT, EDAT, PROD, RPROD

      IMPLICIT NONE
      INTEGER(LONG), INTENT(IN)  :: NCTETRA ! number of elements
      INTEGER(LONG), INTENT(IN)  :: NCPENTA ! number of elements
      INTEGER(LONG), INTENT(IN)  :: NCHEXA  ! number of elements
      INTEGER(LONG) :: I, J                 ! loop counters

      INTEGER(LONG) :: ICELAS1 = 1         ! loop counter
      INTEGER(LONG) :: ICELAS2 = 1         ! loop counter
      INTEGER(LONG) :: ICELAS3 = 1         ! loop counter
      INTEGER(LONG) :: ICELAS4 = 1         ! loop counter

      INTEGER(LONG) :: ICROD = 1           ! loop counter
      INTEGER(LONG) :: ICBAR = 1           ! loop counter
      INTEGER(LONG) :: ICBEAM = 1          ! loop counter

      INTEGER(LONG) :: ICQUAD4 = 1         ! loop counter
      INTEGER(LONG) :: ICTRIA3 = 1         ! loop counter
      INTEGER(LONG) :: ICSHEAR = 1         ! loop counter

      INTEGER(LONG) :: ICTETRA = 1         ! loop counter
      INTEGER(LONG) :: ICPENTA = 1         ! loop counter
      INTEGER(LONG) :: ICHEXA = 1          ! loop counter
      INTEGER(LONG) :: EPNTK
      INTEGER(LONG) :: NCROD_ACTUAL, NCONROD_ACTUAL

      INTEGER(LONG), INTENT(INOUT), DIMENSION(NCELAS1, 6)  :: CELAS1
      INTEGER(LONG), INTENT(INOUT), DIMENSION(NCELAS2, 6)  :: CELAS2
      INTEGER(LONG), INTENT(INOUT), DIMENSION(NCELAS3, 4)  :: CELAS3
      INTEGER(LONG), INTENT(INOUT), DIMENSION(NCELAS4, 4)  :: CELAS4
      INTEGER(LONG), INTENT(INOUT), DIMENSION(NCROD, 4)    :: CROD

      INTEGER(LONG), INTENT(INOUT), DIMENSION(NCTRIA3, 4)  :: CTRIA3
      INTEGER(LONG), INTENT(INOUT), DIMENSION(NCQUAD4, 4)  :: CQUAD4
      INTEGER(LONG), INTENT(INOUT), DIMENSION(NCSHEAR, 4)  :: CSHEAR
      INTEGER(LONG), INTENT(INOUT), DIMENSION(NCROD)       :: CROD_INDEX, CONROD_INDEX
!      INTEGER(LONG), INTENT(INOUT), DIMENSION(NCTETRA, 12) :: CTETRA
!      INTEGER(LONG), INTENT(INOUT), DIMENSION(NCPENTA, 17) :: CPENTA
!      INTEGER(LONG), INTENT(INOUT), DIMENSION(NCHEXA, 22)  :: CHEXA
      INTEGER(LONG) :: IPROD
      
 1    FORMAT(A)
      WRITE(ERR,1) "STARTING GET_GEOM2"
      FLUSH(ERR)
2     FORMAT("PROD: I=",i4, "; PID=", i8, "; MID=", i8, "; A=",f8.4,"; J=",f8.4)

 3    FORMAT(i8, "; ETYPE=", A, "; EOFF=",A, "; EPNT=", i8)
 4    FORMAT("  4 int fields: ", A, 4(" ", i4))
 6    FORMAT("  6 int fields: ", A, 6(" ", i4))
 8    FORMAT("  8 int fields: ", A, 8(" ", i4))
10    FORMAT(" 10 int fields: ", A, 10(" ", i4))
12    FORMAT(" 12 int fields: ", A, 12(" ", i4))
100   FORMAT(" ROD: ", A, " ", 3(A, i8, " "))
101   FORMAT(" ROD: ", A, i8)

      WRITE(1, 101) "NPROD=", NPROD
      DO I=1,NPROD
      !A,J,C,NMS
        WRITE(ERR,2) I, PROD(I,1), PROD(I,2), RPROD(I,1), RPROD(I,2)
        FLUSH(ERR)
      ENDDO
      WRITE(ERR,1) "FINISHED WRITING PROD"
      WRITE(ERR, 101) "NELE=", NELE

      DO I=1,NELE
        WRITE(ERR,3) I, ETYPE(I), EOFF(I), EPNT(I)
        EPNTK = EPNT(I)

        !EID  = EDAT(EPNTK)
        !PID  = EDAT(EPNTK+1)
        IF      (ETYPE(I)(1:5) .EQ. 'ELAS1') THEN
          WRITE(ERR,6) ETYPE(I)(1:5), (EDAT(EPNTK-1+J), J=1,6)
          FLUSH(ERR)
          ! 6 fields
          DO J=1,6
            CELAS1(ICELAS1, J) = EDAT(EPNTK-1+J)
          ENDDO
          ICELAS1 = ICELAS1 + 1
        ELSE IF (ETYPE(I)(1:5) .EQ. 'ELAS2') THEN
          ! 6 fields
          ! TODO: ge, s
          DO J=1,6
            CELAS2(ICELAS2, J) = EDAT(EPNTK-1+J)
          ENDDO
          !WRITE(ERR,6) ETYPE(I)(1:5), (EDAT(EPNTK-1+J), J=1,6)
          FLUSH(ERR)
          ICELAS2 = ICELAS2 + 1

        ELSE IF (ETYPE(I)(1:5) .EQ. 'ELAS3') THEN  
          ! 4 fields
          DO J=1,4
            CELAS3(ICELAS3, J) = EDAT(EPNTK-1+J)
          ENDDO
          !WRITE(ERR,4) ETYPE(I)(1:5), (EDAT(EPNTK-1+J), J=1,4)
          FLUSH(ERR)
          ICELAS3 = ICELAS3 + 1

        ELSE IF (ETYPE(I)(1:5) .EQ. 'ELAS4') THEN  
          ! 4 fields
          DO J=1,4
            CELAS4(ICELAS4, J) = EDAT(EPNTK-1+J)
          ENDDO
          !WRITE(ERR,4) ETYPE(I)(1:5), (EDAT(EPNTK-1+J), J=1,4)
          FLUSH(ERR)
          ICELAS4 = ICELAS4 + 1

        ELSE IF (ETYPE(I)(1:3) .EQ. 'ROD') THEN  
          !WRITE(ERR,4) "CROD?", (EDAT(EPNTK-1+J), J=1,4)
          ! 4 fields
          CROD(ICROD, J) = EDAT(EPNTK)   ! eid
          CROD(ICROD, J) = EDAT(EPNTK+1) ! pid
          CROD(ICROD, J) = EDAT(EPNTK+2) ! ga
          CROD(ICROD, J) = EDAT(EPNTK+3) ! gb
          IPROD = EDAT(EPNTK+1) ! CROD(ICROD,2)

          ! PROD: I=   1; PID=      20; MID=       1; A=  1.0000; J=  0.0000
          ! PROD: I=   2; PID=     -23; MID=       1; A=  2.0000; J=  0.1000
          IF (PROD(IPROD,1) > 0) THEN
            ! pid > 0
            ! ncrod_actual starts at 0
            ! noncrod_actual starts at 0
            ! icrod starts at 1
            WRITE(ERR,4) "CROD", (EDAT(EPNTK-1+J), J=1,4)
            FLUSH(ERR)
            NCROD_ACTUAL = NCROD_ACTUAL + 1
            WRITE(ERR,100) "CROD", "ICROD=",ICROD,"NCROD_ACTUAL=",NCROD_ACTUAL
            FLUSH(ERR)
            CROD_INDEX(NCROD_ACTUAL) = ICROD  
          ELSE
            WRITE(ERR,4) "CONROD", (EDAT(EPNTK-1+J), J=1,4)
            FLUSH(ERR)
            ! eid, pid, ga, gb
            NCONROD_ACTUAL = NCONROD_ACTUAL + 1
            WRITE(ERR,100) "CONROD", "ICROD=",ICROD,"NCONROD_ACTUAL=",NCONROD_ACTUAL
            FLUSH(ERR)
            CONROD_INDEX(NCONROD_ACTUAL) = ICROD
          ENDIF
          ICROD = ICROD + 1
          

        ELSE IF (ETYPE(I)(1:5) .EQ. 'SHEAR') THEN  
          ! 6 fields
          ICSHEAR = ICSHEAR + 1
          DO J=1,6
            CSHEAR(ICSHEAR, J) = EDAT(EPNTK-1+J)
          ENDDO
          !WRITE(ERR,6) ETYPE(I)(1:5), (EDAT(EPNTK-1+J), J=1,6)

!        ELSE IF (ETYPE(I)(1:5) .EQ. 'TETRA4') THEN  
!          ! 6 fields
!          DO J=1,6
!            CTETRA(ICTETRA, J) = EDAT(EPNTK-1+J)
!          ENDDO
!          DO J=7,12
!            CTETRA(ICTETRA, J) = 0
!          ENDDO
!          WRITE(ERR,6) ETYPE(I)(1:5), (EDAT(EPNTK-1+J), J=1,12)
!          ICTETRA = ICTETRA + 1
!        ELSE IF (ETYPE(I)(1:5) .EQ. 'TETRA10') THEN
!          ! 12 fields
!          DO J=1,12
!            CTETRA(ICTETRA, J) = EDAT(EPNTK-1+J)
!          ENDDO
!          WRITE(ERR,12) ETYPE(I)(1:5), (EDAT(EPNTK-1+J), J=1,12)  
!          ICTETRA = ICTETRA + 1
        ENDIF
        !EPNT(I)
      ENDDO
      WRITE(ERR,1) "FINISHED GET_GEOM2"
      FLUSH(ERR)
      END SUBROUTINE GET_GEOM2

!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM2_CROD(ITABLE, CROD,                 &
                                      NCROD_ACTUAL, NCONROD_ACTUAL, &
                                      CROD_INDEX, CONROD_INDEX)
      ! Writes the CROD and CONROD subtables
      USE PENTIUM_II_KIND, ONLY         :  LONG
      USE IOUNT1, ONLY                  :  ERR,OP2
      USE SCONTR, ONLY : NCROD
      USE MODEL_STUF, ONLY : PROD, RPROD
      IMPLICIT NONE
      INTEGER(LONG),INTENT(INOUT)                    :: ITABLE                        ! the op2 subtable counter
      INTEGER(LONG), INTENT(IN), DIMENSION(NCROD, 4) :: CROD                          ! stores eid, ipid, n1, n2 from EDAT
      INTEGER(LONG)                                  :: NUM_WIDE                      ! helper flag
      INTEGER(LONG)                                  :: NVALUES                       ! helper flag
      INTEGER(LONG)                                  :: I,J,IPID,MID                  ! counters
      INTEGER(LONG)                                  :: NCROD_ACTUAL, NCONROD_ACTUAL  ! counters
      INTEGER(LONG), INTENT(IN), DIMENSION(NCROD)    :: CROD_INDEX, CONROD_INDEX      ! flags where the CROD and CONRODs are in CROD
      LOGICAL                                        :: WRITE_OP2 = .FALSE.
      LOGICAL                                        :: WRITE_ERR = .TRUE.
      FLUSH(ERR)
 1    FORMAT(A)
 2    FORMAT(" **WRITE_OP2_GEOM2_CROD: ", 4(i8, " "))
      WRITE(ERR,1) "STARTING WRITE_OP2_GEOM_CROD"
      FLUSH(ERR)
      DO I=1,NCROD
        WRITE(ERR,2) CROD(I,1), CROD(I,2), CROD(I,3), CROD(I,4)
        FLUSH(ERR)
      ENDDO
      IF (NCROD_ACTUAL > 0) THEN
        NUM_WIDE = 4
        NVALUES = NUM_WIDE * NCROD_ACTUAL
      
        ! ROD  4 words: (all read by call to subr ELEPO from subr BD_ROD1)
        !   1) Elem ID
        !   2) Prop ID index
        !   3) Grid A
        !   4) Grid B
        IF (WRITE_ERR) THEN
 4        FORMAT(" **WRITE_OP2_GEOM2_CROD-B4: ", 5(i8, " "))
          FLUSH(ERR)
          DO I=1,NCROD_ACTUAL
            J = CROD_INDEX(I)
            WRITE(ERR,4) J, CROD(J,1), CROD(J,2), CROD(J,3), CROD(J,4)
            FLUSH(ERR)
          ENDDO

 5        FORMAT(" **WRITE_OP2_GEOM2_CROD-B5: ", 5(i8, " "))
          DO I=1,NCROD_ACTUAL
            J = CROD_INDEX(I)
            IPID = CROD(J,2)
            WRITE(ERR,5) J, CROD(J,1), PROD(IPID,1), CROD(J,3), CROD(J,4)
            FLUSH(ERR)
          ENDDO
        ENDIF

        IF(WRITE_OP2) THEN
          WRITE(OP2) NVALUES + 3
          ! eid, pid, n1, n2
          WRITE(OP2) 3001, 30, 48, (                   &
                            CROD(CROD_INDEX(I),1),     &
                       PROD(CROD(CROD_INDEX(I),2),1),  &
                            CROD(CROD_INDEX(I),3),     &
                            CROD(CROD_INDEX(I),4), I=1,NCROD_ACTUAL)
          CALL WRITE_OP2_SUBTABLE_INCREMENT(ITABLE)
          FLUSH(OP2)
        ENDIF
      ENDIF

      IF (NCONROD_ACTUAL > 0) THEN
        NUM_WIDE = 8
        NVALUES = NUM_WIDE * NCONROD_ACTUAL

 50       FORMAT(" **WRITE_OP2_GEOM2_CONROD: ", 4(i4, " "), 4(f8.4, " "))
        !IF (WRITE_ERR) THEN
        !  DO I=1,NCONROD_ACTUAL
        !    ! eid, ipid
        !    J = CONROD_INDEX(I)
        !    IPID = CROD(J, 2)
        !    MID = -PROD(IPID, 2)
        !    ! (eid, n1, n2, mid, a, j, c, nsm) = out
        !    WRITE(ERR,50) CROD(J,1), MID, CROD(J,3), CROD(J,4),                        &
        !                  RPROD(IPID,1), RPROD(IPID,2), RPROD(IPID,3), RPROD(IPID,4)
        !    FLUSH(ERR)
        !  ENDDO
        !ENDIF
        !WRITE(OP2) NVALUES + 3
        !
        !! (eid, n1, n2, mid, a, j, c, nsm) = out
        !CALL GET_PCONROD_INDEXS(CONROD_INDEX, NCONROD_ACTUAL)
        !! mid, a, j, c, nsm
        !WRITE(OP2) 1601, 16, 47, (CROD(CONROD_INDEX(I),1), CROD(CONROD_INDEX(I),3), CROD(CONROD_INDEX(I),4), &
        !                        PROD(PCONROD_INDEX(I),1),                                                  &
        !                        RPROD(PCONROD_INDEX(I),1), PROD(PCONROD_INDEX(I),2),                       &
        !                        RPROD(PCONROD_INDEX(I),3), PROD(PCONROD_INDEX(I),4),                       &
        !                        I=1,NCONROD_ACTUAL)
        !CALL WRITE_OP2_SUBTABLE_INCREMENT(ITABLE)
      ENDIF
      END SUBROUTINE WRITE_OP2_GEOM2_CROD


!===================================================================================================================================
      !SUBROUTINE GET_PCONROD_INDEXS(CONROD_INDEX, NCONROD_ACTUAL)
      !USE PENTIUM_II_KIND, ONLY         :  LONG
      !INTEGER(LONG), INTENT(INOUT), DIMENSION(NCONROD_ACTUAL) :: PCONROD_INDEX
      !INTEGER(LONG)                                           :: NCONROD_ACTUAL
      !END SUBROUTINE GET_PCONROD_INDEXS

!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM2_CELAS1(ITABLE, CELAS1)
      ! writes the CELAS1 subtable
      ! shockingly straightforward
      USE PENTIUM_II_KIND, ONLY         :  LONG
      USE IOUNT1, ONLY                  :  OP2
      USE SCONTR, ONLY : NCELAS1
      IMPLICIT NONE
      INTEGER(LONG), DIMENSION(NCELAS1, 6)           :: CELAS1
      INTEGER(LONG),INTENT(INOUT)                    :: ITABLE
      INTEGER(LONG)                                  :: NVALUES                      ! helper flag
      INTEGER(LONG), PARAMETER                       :: NUM_WIDE = 6                 ! helper flag
      INTEGER(LONG)                                  :: I,J                          ! counters
      IF (NCELAS1 > 0) THEN
        NVALUES = NUM_WIDE * NCELAS1
        WRITE(OP2) NVALUES + 3
      
        ! ELAS1  6 words: (all read by call to subr ELEPO from subr BD_CELAS1)
        !   1) Elem ID
        !   2) Prop ID
        !   3) Grid A
        !   4) Grid B
        !   5) Components at Grid A
        !   6) Components at Grid B
        !(eid, pid, g1, g2, c1, c2) = out
        WRITE(OP2) 601, 6, 73, ((CELAS1(I,J), J=1,6), I=1,NCELAS1)
        CALL WRITE_OP2_SUBTABLE_INCREMENT(ITABLE)
      ENDIF
      END SUBROUTINE WRITE_OP2_GEOM2_CELAS1
      
!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM2_CELAS2(ITABLE, CELAS2)
      ! writes the CELAS2 subtable
      ! TODO needs K, GE, S
      USE PENTIUM_II_KIND, ONLY         :  LONG, DOUBLE
      USE IOUNT1, ONLY                  :  ERR, OP2
      USE SCONTR, ONLY : NCELAS2
      IMPLICIT NONE
      INTEGER(LONG), DIMENSION(NCELAS2, 6)           :: CELAS2
      INTEGER(LONG),INTENT(INOUT)                    :: ITABLE
      INTEGER(LONG)                                  :: NVALUES                      ! helper flag
      INTEGER(LONG), PARAMETER                       :: NUM_WIDE = 8                 ! helper flag
      INTEGER(LONG)                                  :: I,J,IPID                     ! counters
      REAL(DOUBLE)                                   :: K = 1000.0
      REAL(DOUBLE), PARAMETER                        :: GE = 0.0
      REAL(DOUBLE), PARAMETER                        :: S = 0.0
      LOGICAL                                        :: WRITE_OP2 = .FALSE.
      LOGICAL                                        :: WRITE_ERR = .TRUE.
      IF (NCELAS2 > 0) THEN
        NVALUES = NUM_WIDE * NCELAS2
      
        ! ELAS2  6 words: (all read by call to subr ELEPO from subr BD_CELAS2)
        !   1) Elem ID
        !   2) Prop ID which is set to -EID since real props are on the CELAS2 entry
        !   3) Grid A
        !   4) Grid B
        !   5) Components at Grid A
        !   6) Components at Grid B
        IF (WRITE_ERR) THEN
 6        FORMAT(" **WRITE_OP2_GEOM2_CELAS2: ", i4, " ", f8.1, " ", 4(i4, " "))
          DO I=1,NCELAS2
            IPID = CELAS2(I,2)
            !K = RPELAS(IPID,1)
            !GE = RPELAS(IPID,1)
            !S = RPELAS(IPID,1)
            WRITE(ERR,6) CELAS2(I,1), K, CELAS2(I,3),                    &
                         CELAS2(I,4), CELAS2(I,5), CELAS2(I,6)
            FLUSH(ERR)
          ENDDO
        ENDIF

        ! [eid, k, g1, g2, c1, c2], ge, s
        IF (WRITE_OP2) THEN
          WRITE(OP2) NVALUES + 3
          WRITE(OP2) 701, 7, 74, (CELAS2(I,1), CELAS2(I,2), CELAS2(I,3), &
                                  CELAS2(I,4), CELAS2(I,5), CELAS2(I,6), &
                                  REAL(GE,4), REAL(S,4), I=1,NCELAS2)
          CALL WRITE_OP2_SUBTABLE_INCREMENT(ITABLE)
        ENDIF
      ENDIF
      END SUBROUTINE WRITE_OP2_GEOM2_CELAS2

!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM2_CELAS3(ITABLE, CELAS3)
      ! writes the CELAS3 subtable
      ! shockingly straightforward
      USE PENTIUM_II_KIND, ONLY         :  LONG
      USE IOUNT1, ONLY                  :  OP2
      USE SCONTR, ONLY : NCELAS3
      IMPLICIT NONE
      INTEGER(LONG), DIMENSION(NCELAS3, 4)           :: CELAS3
      INTEGER(LONG),INTENT(INOUT)                    :: ITABLE
      INTEGER(LONG)                                  :: NVALUES                      ! helper flag
      INTEGER(LONG)                                  :: I,J                          ! counters
      INTEGER(LONG), PARAMETER                       :: NUM_WIDE = 4                 ! helper flag
      IF (NCELAS3 > 0) THEN
        NVALUES = NUM_WIDE * NCELAS3
        WRITE(OP2) NVALUES + 3
      
        ! ELAS3  4 words: (all read by call to subr ELEPO from subr BD_CELAS3)
        !   1) Elem ID
        !   2) Prop ID
        !   3) Scalar point A
        !   4) Scalar point B
        ! (eid, pid, s1, s2) = out
        WRITE(OP2) 801, 8, 75, ((CELAS3(I,J), J=1,4), I=1,NCELAS3)
        CALL WRITE_OP2_SUBTABLE_INCREMENT(ITABLE)
      ENDIF
      END SUBROUTINE WRITE_OP2_GEOM2_CELAS3

!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM2_CELAS4(ITABLE, CELAS4)
      ! writes the CELAS4 subtable
      ! TODO needs K, not pid
      USE PENTIUM_II_KIND, ONLY         :  LONG
      USE IOUNT1, ONLY                  :  OP2
      USE SCONTR, ONLY : NCELAS4
      IMPLICIT NONE
      INTEGER(LONG), DIMENSION(NCELAS4, 4)           :: CELAS4
      INTEGER(LONG),INTENT(INOUT)                    :: ITABLE
      INTEGER(LONG)                                  :: NVALUES                      ! helper flag
      INTEGER(LONG), PARAMETER                       :: NUM_WIDE = 4                 ! helper flag
      INTEGER(LONG)                                  :: I,J                          ! counters
      IF (NCELAS4 > 0) THEN
        NVALUES = NUM_WIDE * NCELAS4
        WRITE(OP2) NVALUES + 3
      
        ! ELAS4  4 words: (all read by call to subr ELEPO from subr BD_CELAS4)
        !   1) Elem ID
        !   2) Prop ID which is set to -EID since real props are on the CELAS2 entry
        !   3) Scalar point A
        !   4) Scalar point B
        ! (eid, k, s1, s2) = out
        WRITE(OP2) 901, 9, 76, (CELAS4(I,1), CELAS4(I,2), CELAS4(I,3), CELAS4(I,4), I=1,NCELAS4)
        CALL WRITE_OP2_SUBTABLE_INCREMENT(ITABLE)
      ENDIF
      END SUBROUTINE WRITE_OP2_GEOM2_CELAS4

!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM2_CSHEAR(ITABLE, CSHEAR)
      ! writes the CSHEAR subtable
      ! shockingly straightforward
      USE PENTIUM_II_KIND, ONLY         :  LONG
      USE IOUNT1, ONLY                  :  OP2
      USE SCONTR, ONLY : NCSHEAR
      IMPLICIT NONE
      INTEGER(LONG), DIMENSION(NCSHEAR, 6)           :: CSHEAR
      INTEGER(LONG),INTENT(INOUT)                    :: ITABLE
      INTEGER(LONG)                                  :: NVALUES                      ! helper flag
      INTEGER(LONG), PARAMETER                       :: NUM_WIDE = 6                 ! helper flag
      INTEGER(LONG)                                  :: I,J                          ! counters
      IF (NCSHEAR > 0) THEN
        NVALUES = NUM_WIDE * NCSHEAR
        WRITE(OP2) NVALUES + 3
        ! eid, pid, n1, n2, n3, n4
        WRITE(OP2) 3101, 31, 61, ((CSHEAR(I,J), J=1,6), I=1,NCSHEAR)
        CALL WRITE_OP2_SUBTABLE_INCREMENT(ITABLE)
      ENDIF
      END SUBROUTINE WRITE_OP2_GEOM2_CSHEAR!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM2_CTETRA(ITABLE, CTETRA, NCTETRA)
      ! writes the CTETRA subtable
      ! CTETRA4 / CTETRA10 are the same kind of table despite being stored differently in MYSTRAn
      USE PENTIUM_II_KIND, ONLY         :  LONG
      USE IOUNT1, ONLY                  :  OP2
      IMPLICIT NONE
      INTEGER(LONG), DIMENSION(NCTETRA, 12)          :: CTETRA
      INTEGER(LONG),INTENT(INOUT)                    :: ITABLE
      INTEGER(LONG)                                  :: NCTETRA, NVALUES             ! helper flag
      INTEGER(LONG), PARAMETER                       :: NUM_WIDE = 12                ! helper flag
      INTEGER(LONG)                                  :: I,J                          ! counters
      IF (NCTETRA > 0) THEN
        NVALUES = NUM_WIDE * NCTETRA
        WRITE(OP2) NVALUES + 3
        
        ! TETRA4   6 words: (read by 1 or more calls to subr ELEPO from subr BD_TETRA)
        !   1) Elem ID
        !   2) Prop ID
        !   3) etc, Grids 1-4
      
        ! TETRA10  2 words: (read by 1 or more calls to subr ELEPO from subr BD_TETRA)
        !   1) Elem ID
        !   2) Prop ID
        !   3) etc, Grids 1-10
        WRITE(OP2) 5508, 55, 217, ((CTETRA(I,J), J=1,12), I=1,NCTETRA)
        !WRITE(OP2) 5508, 55, 217, & 
        !           ((TETRA4(I,J),  J=1,6), 0, 0, 0, 0, 0, 0, I=1,NCTETRA4), &
        !           ((TETRA10(I,J), J=1,12), I=1,NCTETRA10)
          CALL WRITE_OP2_SUBTABLE_INCREMENT(ITABLE)
      ENDIF
      END SUBROUTINE WRITE_OP2_GEOM2_CTETRA
!===================================================================================================================================
