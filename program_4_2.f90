program program_4_2
    implicit none
    real, parameter :: pi = 4 * atan(1.0)
    real, parameter :: true = pi ** 2 / 6

    integer :: n, j
    real :: calc, calc_reverse, result

    write(6, 100) true
    100 format("True value =", e16.8)

    write(6, 110)
    110 format("(1) i=1, 2, ..., n")
    do n=1000, 10000, 1000
        result = calc(n)
        write(6, 600) n, result, result - true
    end do

    write(6, 120)
    120 format("(2) i=n, n-1, ..., 1")
    do n=1000, 10000, 1000
        result = calc_reverse(n)
        write(6, 600) n, result, result - true
    end do

    write(6, 130)
    130 format("(3)")
    do j = 5, 9
        n = 10 ** j
        result = calc_reverse(n)
        write(6, 600) n, result, result - true
    end do

600 format("n=", i11, " : Result =", e15.8, ", Error =", e16.8)
end program program_4_2

real function calc(n)
    implicit none
    integer, intent(in) :: n
    integer :: i
    real :: s
    
    s = 0
    do i=1, n
        s = s + 1.0 / i ** 2
    end do
    calc = s
end function calc

real function calc_reverse(n)
    implicit none
    integer, intent(in) :: n
    integer :: i
    real :: s

    s = 0
    do i=n, 1, -1
        s = s + 1.0 / (real(i) * i)
    end do
    calc_reverse = s
end function calc_reverse
