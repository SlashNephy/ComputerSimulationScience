program program_2_2
    implicit none
    integer :: i, j, k
    integer, parameter :: n = 8
    real :: A(n, n), B(n, n), C(n, n), D(n, n)
    
    do i = 1, n
        do j = 1, n
            A(i, j) = 4 * i - j / 3.0
            B(i, j) = i / real(j)
            C(i, j) = A(i, j) + B(i, j)
        end do
    end do
    do i = 1, n
        do j = 1, n
            D(i, j) = 0
            do k = 1, n
                D(i, j) = D(i, j) + A(i, k) * B(k, j)
            end do
        end do
    end do

    write(6, 600)
    do i = 1, n
        write(6, 700) (A(i, j), j = 1, n)
    end do

    write(6, 610)
    do i = 1, n
        write(6, 700) (B(i, j), j = 1, n)
    end do

    write(6, 620)
    do i = 1, n
        write(6, 700) (C(i, j), j = 1, n)
    end do

    write(6, 630)
    do i = 1, n
        write(6, 700) (D(i, j), j = 1, n)
    end do

600 format("A=")
610 format("B=")
620 format("C=")
630 format("D=")
700 format(8e10.2)

end program program_2_2
