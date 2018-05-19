program program_2_6
    implicit none
    integer :: i
    real :: x, f

    do i=-10, 10
        x = 0.1 * i
        write(6, 600) x, f(x), f(2*x + 1)
    end do

600 format("x=", e12.4, ", f(x)=", e11.4, ", f(2x+1)=", e11.4)
end program program_2_6

real function f(x)
    implicit none
    real, intent(in) :: x

    if (x < 0) then
        f = 0
    else
        f = sqrt(x)
    endif
end function f
