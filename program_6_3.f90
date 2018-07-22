program program_6_3
    implicit none
    real(8), parameter :: a = 1.0D0
    real(8), parameter :: b = 1 / 3.0D0
    integer, parameter :: n = 2 ** 10 - 1
    real(8), parameter :: V_0 = -20.0D0

    character(1), parameter :: JOBZ = 'V'
    character(1), parameter :: RANGE = 'I'
    integer, parameter :: IL = 1
    integer, parameter :: IU = 3
    real(8), parameter :: ABSTOL = -1
    integer, parameter :: LDZ = n
    integer :: M, IWORK(5 * n), IFAIL(n), INFO, i, j
    real(8) :: D(n), E(n), W(n), VL, VU, Z(LDZ, 4), WORK(5 * n), delta, x, y, C, S

    delta = 2 * a / (n + 1)
    do i = 1, n
        x = -a + delta * i
        D(i) = 2 / delta ** 2
        if (abs(x) < b) then
            D(i) = D(i) + V_0
        endif
        E(i) = - 1 / delta ** 2
    enddo

    call dstevx(JOBZ, RANGE, n, D, E, VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)

    do i = 1, M
        write(6, 600) i, W(i)
    enddo

    do j = 1, M
        write(6, 610) j

        S = 0D0
        do i = 1, n
            y = abs(Z(i, j)) ** 2
            if (mod(i, 2) == 1) then
                S = S + 4 * y
            else
                S = S + 2 * y
            endif
        enddo

        C = sqrt(1 / (S * (2D0 / (n + 1)) / 3))
        write(6, 620) C

        do i = 1, n
            x = - a + i * delta
            write(6, 630) x, C * Z(i, j)
        enddo
    enddo

    600 format("i = ", i2, ", eigenvalue = ", e16.8)
    610 format("j = ", i2)
    620 format("C = ", e16.8)
    630 format(2e16.8)
end program program_6_3
