program program_5_3
    implicit none
    real(8), parameter :: pi = 4 * atan(1.0D0)
    real(8), parameter :: a = 0D0
    real(8), parameter :: b = 2 * pi
    real(8), parameter :: true = atan(2 * pi)
    real(8) :: h, simpson, trapezoid, error_simpson, error_trapezoid
    integer :: k, n
    integer, parameter :: k_min = 1
    integer, parameter :: k_max = 25

    do k = k_min, k_max
        n = 2 ** k
        h = (b - a) / n
        error_simpson = abs(true - simpson(h, n))
        error_trapezoid = abs(true - trapezoid(h, n))
        write(6, 600) log10(h), log10(error_simpson), log10(error_trapezoid)
    enddo
    600 format(e16.8, e16.8, e16.8)
end program program_5_3

real(8) function simpson(h, n)
    implicit none
    real(8), intent(in) :: h
    integer, intent(in) :: n

    real(8) :: x, y, S
    integer :: i
    S = 0D0

    do i = 0, n
        x = h * i
        y = 1 / (x ** 2 + 1)
        if (i == 0 .or. i == n) then
            S = S + y
        else if (mod(i, 2) == 1) then
            S = S + 4 * y
        else
            S = S + 2 * y
        endif
    enddo
    simpson = S * h / 3
end function simpson

real(8) function trapezoid(h, n)
    implicit none
    real(8), intent(in) :: h
    integer, intent(in) :: n

    real(8) :: x, y, S
    integer :: i
    S = 0D0

    do i = 0, n
        x = h * i
        y = 1 / (x ** 2 + 1)
        if (i == 0 .or. i == n) then
            S = S + y / 2
        else
            S = S + y
        endif
    end do
    trapezoid = S * h
end function trapezoid
