module SpinDriftLib

interface  LOCATEW
      subroutine LOCATEW_444(WindowHandle,Row,Col)
        integer*4 WindowHandle,Row,Col
      end

      subroutine LOCATEW_442(WindowHandle,Row,Col)
        integer*4 WindowHandle,Row
        integer*2 Col
      end

      subroutine LOCATEW_424(WindowHandle,Row,Col)
        integer*4 WindowHandle,Col
        integer*2 Row      
      end

      subroutine LOCATEW_422(WindowHandle,Row,Col)
        integer*4 WindowHandle
        integer*2 Row,Col 
      end

      subroutine LOCATEW_244(WindowHandle,Row,Col)
        integer*4 Row,Col
        integer*2 WindowHandle
      end

      subroutine LOCATEW_242(WindowHandle,Row,Col)
        integer*4 Row
        integer*2 WindowHandle,Col
      end

      subroutine LOCATEW_224(WindowHandle,Row,Col)
        integer*4 Col
        integer*2 WindowHandle,Row
      end

      subroutine LOCATEW_222(WindowHandle,Row,Col)
        integer*2 WindowHandle,Row,Col
      end
  end interface


interface locate
  
      subroutine locate_22(Row,Col) ! 0-based (Row,Col) to match Spindrift arguments
        integer*2 Row,Col
      end
      subroutine locate_24(Row,Col) ! 0-based (Row,Col) to match Spindrift arguments
        integer*2 Row
        integer*4 Col
      end
      subroutine locate_42(Row,Col) ! 0-based (Row,Col) to match Spindrift arguments
        integer*4 Row
        integer*2 Col
      end
      subroutine locate_44(Row,Col) ! 0-based (Row,Col) to match Spindrift arguments
        integer*4 Row,Col
      end
end interface

interface cls
  subroutine cls(ULRow,ULColumn,LRRow,LRColumn)
      integer, optional :: ULRow,ULColumn,LRRow,LRColumn
  end subroutine cls
end interface

!interface FindFrst
!      subroutine FindFrst(Pattern,PathAndName,AttrList)
!         character*(*) Pattern
!         character*(*) PathAndName
!         integer, dimension(4), optional :: AttrList
!      end
!end interface 

end module SpinDriftLib