    ! these first few subroutines substitute for cinit_() and cmove()

      subroutine initb1(Dest,Count,Value)
      integer*2 Count,i
      logical*1 Dest(*),Value

      do i=1,Count
        Dest(i)=Value
      end do
      end


      subroutine initw1(Dest,Count,Value)
      integer*2 Count,i,Dest(*),Value

      do i=1,Count
        Dest(i)=Value
      end do
      end


      subroutine initd1(Dest,Count,Value)
      integer*2 Count,i
      real*4 Dest(*),Value

      do i=1,Count
        Dest(i)=Value
      end do
      end


      subroutine initd0(Dest,Count,Value)
      integer*2 Count,i
      real*4 Dest(0:*),Value

      do i=0,Count-1
        Dest(i)=Value
      end do
      end


      subroutine CopySqRMat0(Sour,Dest,nRowCol)
      integer*2 nRowCol,i,j
      real*4 Sour(0:nRowCol-1,0:*),Dest(0:nRowCol-1,0:*)

      do i=0,nRowCol-1
      do j=0,nRowCol-1
        Dest(j,i)=Sour(j,i)
      end do
      end do
      end


      subroutine CopyLinRVec0(Sour,Dest,Count)
      real*4 Sour(0:*),Dest(0:*)
      integer*2 Count,i

      do i=0,Count-1
        Dest(i)=Sour(i)
      end do
      end


      subroutine CopyLinRVec1(Sour,Dest,Count)
      real*4 Sour(*),Dest(*)
      integer*2 Count,i

      do i=1,Count
        Dest(i)=Sour(i)
      end do
      end


      subroutine CopyLinIVec1(Sour,Dest,Count)
      integer*2 Sour(*),Dest(*),Count,i

      do i=1,Count
        Dest(i)=Sour(i)
      end do
      end


      subroutine CopyLinLVec1(Sour,Dest,Count)
      logical*1 Sour(*),Dest(*)
      integer*2 Count,i

      do i=1,Count
        Dest(i)=Sour(i)
      end do
      end


!     function used for debugging below:

      character*4 function Char4Eqv(i)
        integer*2 i ! assumed on [0,9999]
        character*4 IOBuffer

        if    (i<10) then
          write(IOBuffer,'(i1)') i
        elseif(i<100) then
          write(IOBuffer,'(i2)') i
        elseif(i<1000) then
          write(IOBuffer,'(i3)') i
        else ! i<10000
          write(IOBuffer,'(i4)') i
        end if
        Char4Eqv=IOBuffer ! dubious whether result survives long enough on stack
      end


    ! various subroutines to solve a system of linear equations

      subroutine ShowSquare(title,A,n)
        include 'LPIntCom.mon' ! merely for OF1,PrtDetail,Fmtxaixaxi
        character*(*) title
        integer*2 n,i,j
        real*4 A(0:n,0:*)

        if(PrtDetail<2) return
        write(OF1,Fmtxaixaxi) title
        do i=0,n ! row index
          do j=0,n ! col index
            write(OF1,'(G10.2\)') A(j,i)
            if(mod(j,12)==11) write(OF1,Fmtxaixaxi)
          end do
          if(mod(n,12)/=11) write(OF1,Fmtxaixaxi)
        end do
      end


      subroutine CheckDiagonal(i,pivot,Singular)
        include 'LPIntCom.mon' ! merely for OF1,PrtDetail,SmallReal,Fmtxaixaxi
        logical*1 Singular
        integer*2 i
        real*4 pivot

        if(abs(pivot)<SmallReal) then
          if(PrtDetail>0) write(OF1,Fmtxaixaxi)
     +      'CMM LU factorization failed; 0 on diagonal of LU in row ',i
          Singular=.true.
        end if
      end


      subroutine LUFactorByCroutsMM(n,LU,Singular)
      ! Factors an (n+1)-square matrix (A) into lower (L) and upper (U)
      ! triangular matrices using a modification to Crout's Method.  Reference:
      ! Stoutemeyr, PL/I Programming for Engineering and Science, pp. 292-298.
      ! Advantage claimed over simple LU factorization:  Crout's method can
      ! accumulate inner-products in double-precision without having to
      ! store the result (in LU) in double-precision.  Here L and U are both
      ! stored in the space of A, with all on-diagonal terms of U implicitly 1.
      ! Following factorization, routine solves following equation for
      ! vector of unknowns (X), given (A) and the right-hand side (RHS):
      ! A X = RHS => L U X = RHS => L (U X) = RHS => L Y = RHS, with U X = Y.
      ! First, Y is obtained from Y = Linv RHS, then X from X = Uinv Y
        logical*1 Singular
        integer*2 n,i,j,k,ip1,kLim
        real*4 pivot,LU(0:n,0:*) ! LU will overwrite the values in the incoming array
        real*8 r8

        ! order of calculation of elements for a 4x4 example matrix:
        !   1  5  7  9
        !   2  6 11 13
        !   3  8 12 15
        !   4 10 14 16
        ! order of calculation for original Crout's Method:
        !   1  5  6  7
        !   2  8 11 12
        !   3  9 13 15
        !   4 10 14 16
          call ShowSquare('Input CMM:',LU,n)
        ! first column of LU remains equal to first column of the input array
          do i=0,n-1
            ip1=i+1
            pivot=LU(i,i)
            call CheckDiagonal(i,pivot,Singular)
            if(Singular) return
            do j=ip1,n
              r8=LU(j,i)
              do k=0,i-1
                r8=r8-LU(k,i)*LU(j,k)
              end do
              LU(j,i)=r8/pivot ! row i, col j
              r8=LU(ip1,j)
              do k=0,i
                r8=r8-LU(k,j)*LU(ip1,k)
              end do
              LU(ip1,j)=r8 ! row j, col ip1
            end do
          end do
          call ShowSquare('LU:',LU,n)
          call CheckDiagonal(n,LU(n,n),Singular)
        end ! subroutine LUFactorByCroutsMM


      subroutine SolveByCroutsMM(n,LU,X,RHS) ! solve LU*X=RHS for X
        integer*2 n,i,j
        real*4 X(0:*),RHS(0:*),LU(0:n,0:*)
        real*8 r8
      ! equivalence(X,Y) ! this precludes caller's X same as RHS
      ! since the compiler rejected the above, I use X as an alias for Y

        ! having L & U matrices as triangular matrices, first solve L Y = RHS for Y
          do i=0,n
            r8=RHS(i)
            do j=0,i-1
              r8=r8-LU(j,i)*X(j)
            end do
            X(i)=r8/LU(i,i)
          end do
        ! next solve U X = Y for X
          do i=n-1,0,-1
            r8=X(i)
            do j=i+1,n
              r8=r8-LU(j,i)*X(j)
            end do
            X(i)=r8
          end do
        end ! subroutine SolveByCroutsMM


      subroutine LUFactorByCroutsMMwPP(n,LU,Singular,PivotRow,
     +   SupMagInRow) ! ,SwapVector
      ! (the caller must allocate and dispose the work-vectors)
      ! Factors an (n+1)-square matrix (A) into lower (L) and upper (U)
      ! triangular matrices using a modification to Crout's Method.  Reference:
      ! Stoutemeyr, PL/I Programming for Engineering and Science, pp. 292-298.
      ! Advantage claimed over simple LU factorization:  Crout's method can
      ! accumulate inner-products in double-precision without having to
      ! store the result (in LU) in double-precision.  Here L and U are both
      ! stored in the space of A, with all on-diagonal terms of U implicitly 1.
      ! Following factorization, routine solves following equation for
      ! vector of unknowns (X), given (A) and the right-hand side (RHS):
      ! A X = RHS => L U X = RHS => L (U X) = RHS => L Y = RHS, with U X = Y.
      ! First, Y is obtained from Y = Linv RHS, then X from X = Uinv Y.

      ! This version uses 'scaled partial pivoting' to allow the input matrix
      ! to have elements of arbitrarily small magnitude on the diagonal; the
      ! simple Crout's Method uses these as divisors.  Permutation vector PivotRow
      ! is the set of indices of rows in the input matrix, in the order they are
      ! used in the output matrix.

        logical*1 Singular
        integer*2 n,i,j,k,ip1,iRow,jCol,PivotRow(0:*)
      ! integer*4 I4nBytes
        real*4 s,pivot,Ajip1,Mag,SupMagRatio,
     +    SupMagInRow(0:n),LU(0:n,0:*) ! ,SwapVector(0:n)
        ! LU will overwrite the values in the incoming array
        real*8 RowSum,ColSum,r8

        ! modified Crout's Method using permutation vector
        ! first column of LU remains equal to first column of input A
        ! order of calculation of elements for a 4x4 example matrix:
        !   1  5  7  9
        !   2  6 11 13
        !   3  8 12 15
        !   4 10 14 16
        ! order of calculation for original Crout's Method:
        !   1  5  6  7
        !   2  8 11 12
        !   3  9 13 15
        !   4 10 14 16
          call ShowSquare('Input wPP:',LU,n)
          SupMagRatio=0.0
          do i=0,n ! find row weights and first pivotal row
            PivotRow(i)=i
            s=0.0
            do j=n,0,-1
              Mag=abs(LU(j,i))
              if(s<Mag) s=Mag
            end do
            SupMagInRow(i)=s
            Mag=Mag/s ! finally, test ratio of abs(LU(0,i)) to sup magnitude
            if(SupMagRatio<Mag) then ! save index of row with largest ratio
              SupMagRatio=Mag
              iRow=i
            end if
          end do
          pivot=LU(0,iRow)
      !   I4nBytes=4*(n+1)
          do i=0,n-1  ! use PivotRow to track row interchanges
            if(iRow/=i) then ! swap rows i and iRow
              j             =PivotRow(i)
              PivotRow(i   )=PivotRow(iRow)
              PivotRow(iRow)=j
              s                =SupMagInRow(i)
              SupMagInRow(i   )=SupMagInRow(iRow)
              SupMagInRow(iRow)=s
            ! SwapVector=LU(:,i) ! call cmove(LU(0,i   ),SwapVector,I4nBytes)
            ! LU(:,i)=LU(:,iRow) ! call cmove(LU(0,iRow),LU(0,i)   ,I4nBytes)
            ! LU(:,iRow)=SwapVector ! call cmove(SwapVector,LU(0,iRow),I4nBytes)
              do j=0,n
                s         =LU(j,i)
                LU(j,i   )=LU(j,iRow)
                LU(j,iRow)=s
              end do
            end if
            ip1=i+1
            SupMagRatio=0.0
            call CheckDiagonal(i,pivot,Singular)
            if(Singular) return
            do j=ip1,n
              RowSum=LU(j,i)
              ColSum=0.0d0
              do k=0,i-1
                RowSum=RowSum-LU(k,i)*LU(j  ,k)
                ColSum=ColSum+LU(k,j)*LU(ip1,k)
              end do
              LU(j,i)=RowSum/pivot ! row i, col j
              Ajip1=LU(ip1,j) ! subscript reversal is proper
            ! r8=Ajip1-ColSum-LU(k,j)*LU(ip1,k) ! sic in source text
              r8=Ajip1-ColSum-LU(i,j)*LU(ip1,i)
              LU(ip1,j)=r8 ! row j, col ip1
              r8=dabs(r8)/SupMagInRow(j)
              if(SupMagRatio<r8) then
                SupMagRatio=r8
                iRow=j
              end if
            end do ! j loop
            pivot=LU(ip1,iRow)
          end do ! i loop
          call CheckDiagonal(n,LU(n,n),Singular)
        end ! subroutine LUFactorByCroutsMMwPP


      subroutine SolveByCroutsMMwPP(n,LU,X,RHS,PivotRow) ! solve LU*X=RHS for X
        integer*2 n,i,j,PivotRow(0:*) ! (the caller must allocate and dispose PivotRow)
        real*4 X(0:*),RHS(0:*),LU(0:n,0:*)
        real*8 r8
      ! equivalence(X,Y) ! this precludes caller's X same as RHS
      ! since the compiler rejected the above, I use X as an alias for Y

        ! having L & U matrices as triangular matrices, first solve L Y = RHS for Y
          do i=0,n
          ! r8=RHS(i); ! for method without partial pivoting
            r8=RHS(PivotRow(i))
            do j=0,i-1
              r8=r8-LU(j,i)*X(j)
            end do
            X(i)=r8/LU(i,i)
          end do
        ! next solve U X = Y for X
          do i=n-1,0,-1
            r8=X(i)
            do j=i+1,n
              r8=r8-LU(j,i)*X(j)
            end do
            X(i)=r8
          end do
        end ! subroutine SolveByCroutsMMwPP


      real*4 function ContInnerProduct(x,y,m) ! for contiguous elements in x & y
        real*4 x(0:*),y(0:*) !,s ! for 0-based vectors
        real*8 r8
        integer*2 i,m

        r8=0.0d0 ! use real*8 during the accumulation
        do i=0,m
          r8=r8+x(i)*y(i)
        end do
        ContInnerProduct=r8
      end


      real*4 function DiscInnerProduct(BInv,y,i,m)
        integer*2 i,m,j
        real*4 y(0:m),BInv(0:m,0:*)
        real*8 r8

        r8=0.0d0 ! use real*8 during the accumulation
        do j=0,m
          r8=r8+BInv(i,j)*y(j) ! discontiguous in BInv, whose first subscript is row
        end do
        DiscInnerProduct=r8
      end

