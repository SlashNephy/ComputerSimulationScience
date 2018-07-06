program program_6_1
    implicit none
    character(1), parameter :: JOBZ = 'V'
    character(1), parameter :: RANGE = 'I'
    integer, parameter :: N = 5
    integer, parameter :: IL = 1
    integer, parameter :: IU = 3
    real(8), parameter :: ABSTOL = -1
    integer, parameter :: LDZ = N
    integer :: M, IWORK(5 * N), IFAIL(N), INFO, i, j, l, A(N, N)
    real(8) :: D(N), E(N), VL, VU, W(N), Z(LDZ, 3), WORK(5 * N), B(N), av(N)

    D(1) = 1; D(2) = 3; D(3) = 5; D(4) = 7; D(5) = 9
    E(1) = 2; E(2) = 4; E(3) = 6; E(4) = 8

    call dstevx(JOBZ, RANGE, N, D, E, VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
    
    do l = 1, M
        write(6, 600) l
        write(6, 610) W(l)
        write(6, 620) l
        write(6, 630) (Z(j, l), j = 1, N)
    enddo
    600 format("l=", i1, " eigenvalue")
    610 format(e16.8)
    620 format("l=", i1, " eigenvector")
    630 format(5e16.8)

    do i = 1, N
        do j = 1, N
            if (i == j) then
                A(i, j) = D(i)
            elseif (i == j - 1) then
                A(i, j) = E(i)
            elseif (i == j + 1) then
                A(i, j) = E(i - 1)
            else
                A(i, j) = 0
            endif
        enddo
    enddo
    700 format("A=")
    write(6, 700)
    710 format(5i2)
    do i = 1, N
        write(6, 710) (A(i, j), j = 1, N)
    enddo

    do l = 1, M
        write(6, 800) l
        do i = 1, N
            av(i) = W(l) * Z(i, l)
            B(i) = 0
            do j = 1, N
                B(i) = B(i) + A(i, j) * Z(j, l)
            enddo
        enddo
        write(6, 810) (abs(B(i) - av(i)), i = 1, N)
    enddo
    800 format("l=", i1)
    810 format("Av-av=", 5e16.8)
end program program_6_1
