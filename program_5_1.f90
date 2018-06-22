program program_5_1
    implicit none
    real(8), parameter :: pi = 4 * atan(1.0D0)
    real(8), parameter :: a = 0D0
    real(8), parameter :: b = 2 * pi
    real(8), parameter :: true2 = 8 * pi ** 3 / 3
    real(8), parameter :: true3 = atan(2 * pi)
    real(8), parameter :: true4 = -pi / 5
    real(8) :: h, result, error, integrate2, integrate3, integrate4
    integer :: k, n
    integer, parameter :: k_min = 1
    integer, parameter :: k_max = 25

    write(6, 200)
    200 format("(2)")
    do k = k_min, k_max
        n = 2 ** k
        h = (b - a) / n
        result = integrate2(h, n)
        error = abs(true2 - result)
        write(6, 210) log10(h), log10(error)
        210 format(e16.8, e16.8)
    end do

    write(6, 300)
    300 format("(3)")
    do k = k_min, k_max
        n = 2 ** k
        h = (b - a) / n
        result = integrate3(h, n)
        error = abs(true3 - result)
        write(6, 310) log10(h), log10(error)
        310 format(e16.8, e16.8)
    end do

    write(6, 400)
    400 format("(4)")
    do k = k_min, k_max
        n = 2 ** k
        h = (b - a) / n
        result = integrate4(h, n)
        error = abs(true4 - result)
        write(6, 410) log10(h), log10(error)
        410 format(e16.8, e16.8)
    end do

100 format(e16.8)
end program program_5_1

real(8) function integrate2(h, n)
    implicit none
    real(8), intent(in) :: h
    integer, intent(in) :: n

    real(8) :: x, y, S
    integer :: i
    S = 0D0

    do i = 0, n
        x = h * i
        y = x ** 2
        if ((i == 0).or.(i == n)) then
            S = S + y / 2
        else
            S = S + y
        endif
    end do
    integrate2 = S * h
end function integrate2

real(8) function integrate3(h, n)
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
    integrate3 = S * h
end function integrate3

real(8) function integrate4(h, n)
    implicit none
    real(8), intent(in) :: h
    integer, intent(in) :: n

    real(8) :: x, y, S
    integer :: i
    S = 0D0

    do i = 0, n
        x = h * i
        y = x * sin(10 * x)
        if ((i == 0).or.(i == n)) then
            S = S + y / 2
        else
            S = S + y
        endif
    end do
    integrate4 = S * h
end function integrate4
