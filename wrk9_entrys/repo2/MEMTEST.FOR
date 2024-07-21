!     ******************************************************************
!     MEMTEST.FOR
!     Copyright(c)  2000
!
!     Created: 6/16/2010 5:28:58 PM
!     Author : MARK S GERBER
!     Last change: MSG 6/17/2010 1:47:53 PM
!     ******************************************************************

      REAL (KIND=4), ALLOCATABLE :: TEST1(:,:,:),TEST2(:,:)
      REAL (KIND=4), ALLOCATABLE :: TEST3(:)
      INTEGER (KIND=4) :: DEM1,DEM2,DEM3,I,J,T1,T2,T3
      T1 = 5000000
      T2 = 100
      T3 = 1000
	  ! TODO: Get rid of this pause.
      pause
      J = 0
      DO I = 1,2
!
         J = J + 1
         DEM1 = T1 + J
         DEM2 = T2 + J
         DEM3 = T3 + J
!
         ALLOCATE(TEST1(DEM2,DEM2,DEM2))
         ALLOCATE(TEST2(DEM2,DEM3))
         ALLOCATE(TEST3(DEM1))
         TEST1= FLOAT(I) 
         TEST2= FLOAT(I) 
         TEST3= FLOAT(I) 
         DEALLOCATE(TEST1,TEST2,TEST3)
		 ! TODO: Get rid of this pause.
         IF(I ==1)PAUSE          
      ENDDO
	  ! TODO: Get rid of this pause.
      PAUSE
      END
