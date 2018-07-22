program program_6_2
    implicit none
    real(8), parameter :: a = 1
    integer, parameter :: n_max = 2 ** 10 - 1
    real(8), parameter :: pi = 4 * atan(1.0D0)

    character(1), parameter :: JOBZ = 'V'
    character(1), parameter :: RANGE = 'I'
    integer, parameter :: IL = 1
    integer, parameter :: IU = 4
    real(8), parameter :: ABSTOL = -1

    integer, parameter :: LDZ = n_max
    integer :: M, IWORK(5 * n_max), IFAIL(n_max), INFO, i, j, n
    real(8) :: D(n_max), E(n_max), W(n_max), VL, VU, Z(LDZ, 4), WORK(5 * n_max), delta, energy_eigenvalue, x, S, y, C, y_true

    ! (1)
    write(6, 600) (j, energy_eigenvalue(j), j = 1, 4)

    do i = 8, 10
        n = 2 ** i - 1
        write(6, 610) i

        delta = 2 * a / (n + 1)
        do j = 1, n
            D(j) = 2 / delta ** 2
            E(j) = - 1 / delta ** 2
        enddo

        call dstevx(JOBZ, RANGE, n, D, E, VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
        
        do j = 1, M
            write(6, 620) j
            write(6, 630) W(j)
        enddo
    enddo

    ! (2)
    do j = 1, M
        S = 0D0
        do i = 1, n
            y = abs(Z(i, j)) ** 2
            if (mod(i, 2) == 1) then
                S = S + 4 * y
            else
                S = S + 2 * y
            endif
        enddo
        S = S * (2D0 / (n + 1)) / 3
        C = sqrt(1 / S)

        write(6, 620) j
        do i = 1, n
            x = - a + i * delta
            if (mod(i, 2) == 1) then
                y_true = cos(i * pi * x / 2)
            else
                y_true = sin(i * pi * x / 2)
            endif
            write(6, 640) x, C * Z(i, j)
        enddo
    enddo

    600 format("true(l=", i1, ") = ", e16.8)
    610 format("n = 2 ** ", i2, " - 1")
    620 format("j = ", i2)
    630 format(e16.8)
    640 format(2e16.8)
end program program_6_2

real(8) function energy_eigenvalue(n)
    implicit none
    integer, intent(in) :: n
    real(8), parameter :: pi = 4 * atan(1.0D0)
    real(8), parameter :: a = 1

    energy_eigenvalue = (n * pi / (2 * a)) ** 2
end function energy_eigenvalue
