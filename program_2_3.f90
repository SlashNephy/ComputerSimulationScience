program program_2_3
    implicit none
    integer :: i
    real :: x, f

    do i=-10, 10
        x = 0.1 * i
        if (i < 0) then
            f = x + 1
        else
            f = -x + 1
        endif

        write(6, 600) x, f
    end do

600 format("x=", e9.2, ", f(x)=", e9.2)
end program program_2_3
