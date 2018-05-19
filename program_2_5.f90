program program_2_5
    implicit none
    integer, parameter :: n = 5

    integer :: i, j, d
    real :: A(n, n), B(n, n), C(n, n), X(n, n), Y(n, n)

    ! A, B, Cをn×nの単位行列とする
    write(6, 500)
    do i = 1, n
        do j = 1, n
            if (i == j) then
                d = 1
            else
                d = 0
            endif
            A(i, j) = d
            B(i, j) = d
            C(i, j) = d
        end do
    end do

    call product2(A, B, n, X)
    write(6, 600)
    do i = 1, n
        write(6, 700) (X(i, j), j = 1, n)
    end do

    call product3(A, B, C, n, Y)
    write(6, 610)
    do i = 1, n
        write(6, 700) (Y(i, j), j = 1, n)
    end do

    ! A, B, Cをn×nの全要素が1の行列とする
    write(6, 510)
    do i = 1, n
        do j = 1, n
            A(i, j) = 1
            B(i, j) = 1
            C(i, j) = 1
        end do
    end do

    call product2(A, B, n, X)
    write(6, 600)
    do i = 1, n
        write(6, 700) (X(i, j), j = 1, n)
    end do

    call product3(A, B, C, n, Y)
    write(6, 610)
    do i = 1, n
        write(6, 700) (Y(i, j), j = 1, n)
    end do
500 format("Test 1")
510 format("Test 2")
600 format("X=")
610 format("Y=")
700 format(5e12.4)
end program program_2_5

subroutine product2(A, B, n, X)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: A(n, n), B(n, n)
    real, intent(out) :: X(n, n)
    integer :: i, j, k

    do i = 1, n
        do j = 1, n
            X(i, j) = 0
            do k = 1, n
                X(i, j) = X(i, j) + A(i, k) * B(k, j)
            end do
        end do
    end do
end subroutine product2



subroutine product3(A, B, C, n, Y)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: A(n, n), B(n, n), C(n, n)
    real, intent(out) :: Y(n, n)
    real :: X(n, n)

    call product2(A, B, n, X)
    call product2(X, C, n, Y)
end subroutine product3
