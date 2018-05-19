program program_2_4
    implicit none
    real, parameter :: r = 2.0
    real, parameter :: h = 3.0
    real :: V, S

    call volume(r, h, V)
    call surface(r, h, S)

    write(6, 600) V, S

600 format("V=", e11.4, ", S=", e11.4)
end program program_2_4

subroutine volume(r, h, V)
    implicit none
    real, intent(in) :: r, h
    real, intent(out) :: V
    real :: pi

    pi = 4 * atan(1.0)
    V = pi * r * r * h 
end subroutine volume

subroutine surface(r, h, S)
    implicit none
    real, intent(in) :: r, h
    real, intent(out) :: S
    real :: pi

    pi = 4 * atan(1.0)
    S = 2 * pi * r * (r + h)
end subroutine surface
