      SUBROUTINE GET_ICD(CID, ICD)
      ! CD is the output coordinate system
      ! ICD is the index of the output coordinate system
      !
      ! TODO: ie CORD array sorted?  If it is, we can do a binary search
      !
      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE IOUNT1, ONLY                :  ERR
      USE SCONTR, ONLY                :  NCORD
      USE MODEL_STUF, ONLY            :  CORD

      IMPLICIT NONE

      INTEGER(LONG), INTENT(IN)       :: CID   ! coordinate system ID
      INTEGER(LONG), INTENT(INOUT)    :: ICD   ! index of coord id 
      INTEGER(LONG)                   :: I     ! counter

      DO I=1,NCORD
         IF (CID == CORD(I,2)) THEN
            ! CID global coord system exists. It was checked in CORDP_PROC
            ICD = I
            EXIT
         ENDIF
      ENDDO
      
      !WHILE (I<NCORD)
      END SUBROUTINE GET_ICD
