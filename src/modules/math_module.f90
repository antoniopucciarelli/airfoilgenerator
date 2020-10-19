module math_module
    implicit none

    real(kind=8), parameter :: pi = 4 * atan(1.0)  

    contains 

    function cross(x, y)
    ! this function computes the cross product between x and y in 2D
        implicit none

        real(kind=8)              :: cross
        real(kind=8),dimension(2) :: x
        real(kind=8),dimension(2) :: y

        cross = x(1)*y(2) - x(2)*y(1)

    end function cross

end module math_module