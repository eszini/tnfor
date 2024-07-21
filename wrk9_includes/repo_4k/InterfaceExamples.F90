!     ******************************************************************
!     InterfaceExamples.F90
!     Copyright(c)  2000
!
!     Created: 1/12/2011 4:28:35 PM
!     Author : MARK S GERBER
!     Last change: MSG 1/12/2011 4:28:35 PM
!     ******************************************************************

! Alan, there are three examples of user the interface structure.
! The first allocates arrays, the second does escalations and the third
! converts number to strings.
! Mark


!***********************************************************************
!A simple allocation for differnet type arrays that are sized 1:AllSize
! can be expanded to deal with arrays that have LowerBound:UpperBound
      MODULE ArrayAllocationInterface
        interface AllocateArray
          module procedure AllArrayI2,AllArrayI4,AllArrayR4,
     +                     AllD2ArrayI2,AllArrayChrI2,AllArrayI8
        end interface AllocateArray
        contains
c-----
         subroutine AllArrayI2(AllArray,AllSize)
         integer (kind=2), allocatable :: AllArray(:)
         integer (kind=2) :: AllSize
            if(allocated(AllArray)) then
               if(SIZE(AllArray) < AllSize) then
                  deallocate(AllArray)
                  allocate(AllArray(AllSize))
               endif
            else
               allocate(AllArray(AllSize))
            endif
         end subroutine
         subroutine AllArrayChrI2(AllArray,AllSize)
         character (len=*), allocatable :: AllArray(:)
         integer (kind=2) :: AllSize
            if(allocated(AllArray)) then
               if(SIZE(AllArray) < AllSize) then
                  deallocate(AllArray)
                  allocate(AllArray(AllSize))
               endif
            else
               allocate(AllArray(AllSize))
            endif
         end subroutine
         subroutine AllArrayI4(AllArray,AllSize)
         integer (kind=4), allocatable :: AllArray(:)
         integer (kind=2) :: AllSize
            if(allocated(AllArray)) then
               if(SIZE(AllArray) < AllSize) then
                  deallocate(AllArray)
                  allocate(AllArray(AllSize))
               endif
            else
               allocate(AllArray(AllSize))
            endif
         end subroutine
         subroutine AllArrayI8(AllArray,AllSize)
         integer (kind=8), allocatable :: AllArray(:)
         integer (kind=2) :: AllSize
            if(allocated(AllArray)) then
               if(SIZE(AllArray) < AllSize) then
                  deallocate(AllArray)
                  allocate(AllArray(AllSize))
               endif
            else
               allocate(AllArray(AllSize))
            endif
         end subroutine
         subroutine AllArrayR4(AllArray,AllSize)
         real (kind=4), allocatable :: AllArray(:)
         integer (kind=2) :: AllSize
            if(allocated(AllArray)) then
               if(SIZE(AllArray) < AllSize) then
                  deallocate(AllArray)
                  allocate(AllArray(AllSize))
               endif
            else
               allocate(AllArray(AllSize))
            endif
         end subroutine
         subroutine AllD2ArrayI2(AllArray,AllSize1,AllSize2)
         real (kind=4), allocatable :: AllArray(:,:)
         integer (kind=2) :: AllSize1,AllSize2
            if(allocated(AllArray)) then
               if(SIZE(AllArray,1) /= AllSize1 .OR.
     +                     SIZE(AllArray,2) /= AllSize2) then
                  deallocate(AllArray)
                  allocate(AllArray(AllSize1,AllSize2))
               endif
            else
               allocate(AllArray(AllSize1,AllSize2))
            endif
         end subroutine
      END MODULE ArrayAllocationInterface
! ***********************************************************************
      MODULE CoalModelVectorFileInterface
        interface SO2_ESCALATIONS
          module procedure SO2_ESCALATIONS_RATES_VALUES,
     +                     SO2_ESCALATIONS_VALUES_ONLY
        end interface SO2_ESCALATIONS
        contains
! ***********************************************************************
         SUBROUTINE SO2_ESCALATIONS_RATES_VALUES(EscalatedValue,
     +                                           EscalationRate)
! ***********************************************************************
            REAL (KIND=4) :: EscalatedValue(0:30),EscalationRate,
     +                       Vector_Values(1:30)
            INTEGER (KIND=2) :: VectorNum, iYr
            LOGICAL (KIND=4) :: RETURN_COAL_VECTOR
!          
            EscalatedValue(1:30) = EscalatedValue(0)
            IF(EscalationRate > 0.) THEN
               EscalationRate = 1. + EscalationRate/100.
               DO iYr = 1, 30
                  EscalatedValue(iYr) = EscalationRate *
     +                                             EscalatedValue(iYr-1)
               ENDDO
            ELSEIF(EscalationRate < 0.) THEN
               VectorNum = ABS(EscalationRate)
               IF(RETURN_COAL_VECTOR(Vector_Values,VectorNum)) THEN
                  EscalatedValue(1:30) = Vector_Values(1:30)
               ELSE
                  DO iYr = 1, 30
                     EscalatedValue(iYr)= (1.+Vector_Values(iYr)/100.) *
     +                                             EscalatedValue(iYr-1)
                  ENDDO
               ENDIF
            ENDIF
         RETURN
         END SUBROUTINE
! ***********************************************************************
         SUBROUTINE SO2_ESCALATIONS_VALUES_ONLY(EscalatedValue)
! ***********************************************************************
            REAL (KIND=4) :: EscalatedValue(0:30),
     +                       Vector_Values(1:30)
            INTEGER (KIND=2) :: VectorNum
            LOGICAL (KIND=4) :: RETURN_COAL_VECTOR
!          
            EscalatedValue(1:30) = EscalatedValue(0)
            IF(EscalatedValue(0) < 0.) THEN
               VectorNum = ABS(EscalatedValue(0))
               IF(RETURN_COAL_VECTOR(Vector_Values,VectorNum)) THEN
                  EscalatedValue(1:30) = Vector_Values(1:30)
               ENDIF
            ENDIF
            RETURN
         END SUBROUTINE
      END MODULE CoalModelVectorFileInterface
!
! ANOTHER EXAMPLE
!
      MODULE conversion_routines
         INTERFACE CONVERT_2_STR
            MODULE PROCEDURE INT2_CONVER_2_STR
            MODULE PROCEDURE STR_CONVER_2_STR
            MODULE PROCEDURE INT4_CONVER_2_STR
            MODULE PROCEDURE INT8_CONVER_2_STR
            MODULE PROCEDURE REAL_CONVER_2_STR
            MODULE PROCEDURE INT2_CONVER_2_STR_W_LEN
         END INTERFACE
      CONTAINS
!--------
         FUNCTION INT2_CONVER_2_STR(VALUE_IN) RESULT(VALUE_OUT)
         CHARACTER (LEN=20) :: VALUE_OUT,TEMP_STR
         INTEGER (KIND=2) :: VALUE_IN
            WRITE(TEMP_STR,'(I6)') VALUE_IN
            VALUE_OUT = ADJUSTL(TEMP_STR)
         END FUNCTION
!--------
         FUNCTION INT2_CONVER_2_STR_W_LEN(VALUE_IN,STR_LEN) RESULT(VALUE_OUT)
         CHARACTER (LEN=20) :: VALUE_OUT,TEMP_STR
         INTEGER (KIND=2) :: VALUE_IN
         INTEGER (KIND=4) :: STR_LEN
            WRITE(TEMP_STR,'(I8.8)') VALUE_IN
            IF(STR_LEN <= 8) THEN
               VALUE_OUT = TEMP_STR(8-STR_LEN+1:8)
            ELSE
               VALUE_OUT = ADJUSTL(TEMP_STR)
            ENDIF
         END FUNCTION
!--------
         FUNCTION STR_CONVER_2_STR(VALUE_IN) RESULT(VALUE_OUT)
         CHARACTER (LEN=256) :: VALUE_OUT
         CHARACTER (LEN=*) :: VALUE_IN
            IF(INDEX(TRIM(VALUE_IN),' ') == 0) THEN
                VALUE_OUT = VALUE_IN
            ELSE
                VALUE_OUT = '"'//TRIM(VALUE_IN)//'"'
            ENDIF
         END FUNCTION
!--------
         FUNCTION INT4_CONVER_2_STR(VALUE_IN) RESULT(VALUE_OUT)
         CHARACTER (LEN=20) :: VALUE_OUT,TEMP_STR
         INTEGER (KIND=4) :: VALUE_IN
            WRITE(TEMP_STR,'(I11)') VALUE_IN
            VALUE_OUT = ADJUSTL(TEMP_STR)
         END FUNCTION
!--------
         FUNCTION INT8_CONVER_2_STR(VALUE_IN) RESULT(VALUE_OUT)
         CHARACTER (LEN=30) :: VALUE_OUT,TEMP_STR
         INTEGER (KIND=8) :: VALUE_IN
            WRITE(TEMP_STR,'(I22)') VALUE_IN
            VALUE_OUT = ADJUSTL(TEMP_STR)
         END FUNCTION
!--------
         FUNCTION REAL_CONVER_2_STR(VALUE_IN) RESULT(VALUE_OUT)
         CHARACTER (LEN=20) :: VALUE_OUT,TEMP_STR,TEMP_STR2
         REAL (KIND=4) :: VALUE_IN
         INTEGER (KIND=2) :: STR_LEN,I
            IF(ABS(VALUE_IN) < .00001) THEN
               WRITE(TEMP_STR,'(F19.11)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < .0001) THEN
               WRITE(TEMP_STR,'(F19.10)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < .001) THEN
               WRITE(TEMP_STR,'(F19.9)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < .01) THEN
               WRITE(TEMP_STR,'(F19.8)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < .1) THEN
               WRITE(TEMP_STR,'(F19.7)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < 1.) THEN
               WRITE(TEMP_STR,'(F19.6)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < 10.) THEN
               WRITE(TEMP_STR,'(F19.5)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < 100.) THEN
               WRITE(TEMP_STR,'(F19.4)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < 1000.) THEN
               WRITE(TEMP_STR,'(F19.3)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < 10000.) THEN
               WRITE(TEMP_STR,'(F19.2)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < 100000.) THEN
               WRITE(TEMP_STR,'(F19.1)') VALUE_IN
            ELSE
               WRITE(TEMP_STR,'(F19.0)') VALUE_IN
            ENDIF
            TEMP_STR2 = ADJUSTL(TEMP_STR)
            STR_LEN = LEN_TRIM(TEMP_STR2)
            DO I = STR_LEN,1,-1
               IF(TEMP_STR2(I:I) /= '0') EXIT
            ENDDO
            IF(TEMP_STR2(I:I) == '.') THEN
               VALUE_OUT = TEMP_STR2(1:I-1)
            ELSE
               VALUE_OUT = TEMP_STR2(1:I)
            ENDIF
         END FUNCTION
      END MODULE conversion_routines

