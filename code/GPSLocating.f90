program GPSLocating
    implicit none
    integer, parameter :: dp = selected_real_kind(15, 307)
    integer :: N, i, j
    real(dp), allocatable :: xi(:), yi(:), zi(:), Di(:)
    real(dp), allocatable :: A(:,:), L(:), At(:,:), AtA(:,:), AtL(:)
    real(dp), allocatable :: X(:), solution(:)
    real(dp) :: D1, x1, y1, z1
    real(dp) :: det
    ! For LAPACK routines
    integer :: info, nrhs, lda, ldb

    ! Prompt user for the number of stations
    print *, 'Enter the number of stations (N >= 4):'
    read *, N
    if (N < 4) then
        print *, 'At least 4 stations are required.'
        stop
    endif

    ! Allocate arrays
    allocate(xi(N), yi(N), zi(N), Di(N))
    allocate(A(N-1, 3), L(N-1))
    allocate(At(3, N-1), AtA(3, 3), AtL(3))
    allocate(X(3), solution(3))

    ! Input station coordinates and distances
    print *, 'Enter the coordinates (xi, yi, zi) and distance Di for each station:'
    do i = 1, N
        print *, 'Station ', i, ':'
        read *, xi(i), yi(i), zi(i), Di(i)
    end do

    ! Use the first station as the reference
    x1 = xi(1)
    y1 = yi(1)
    z1 = zi(1)
    D1 = Di(1)

    ! Construct matrix A and vector L
    do i = 2, N
        A(i-1, 1) = -2.0_dp * (xi(i) - x1)
        A(i-1, 2) = -2.0_dp * (yi(i) - y1)
        A(i-1, 3) = -2.0_dp * (zi(i) - z1)
        L(i-1) = (Di(i)**2 - D1**2) - (xi(i)**2 + yi(i)**2 + zi(i)**2) + (x1**2 + y1**2 + z1**2)
    end do

    ! Transpose of A
    At = transpose(A)

    ! Compute AtA and AtL
    AtA = matmul(At, A)
    AtL = matmul(At, L)

    ! Solve the normal equations AtA * X = AtL
    ! For simplicity, we'll use Cramer's rule for 3x3 system
    call solve_3x3(AtA, AtL, solution)

    ! The coordinates of the target point
    X = solution

    ! Output the result
    print *, 'The estimated coordinates of the target point are:'
    print *, 'X = ', X(1)
    print *, 'Y = ', X(2)
    print *, 'Z = ', X(3)

    contains

    subroutine solve_3x3(A, B, X)
    real(dp), intent(in) :: A(3,3), B(3)
    real(dp), intent(out) :: X(3)
    real(dp) :: detA, detA1, detA2, detA3
    real(dp), dimension(3,3) :: A1, A2, A3

    ! Compute determinant of A
    detA = determinant_3x3(A)
    if (abs(detA) < 1.0e-12_dp) then
        print *, 'Matrix is singular or ill-conditioned.'
        stop
    endif

    ! Replace columns of A with B to compute determinants
    A1 = A
    A1(:,1) = B
    detA1 = determinant_3x3(A1)

    A2 = A
    A2(:,2) = B
    detA2 = determinant_3x3(A2)

    A3 = A
    A3(:,3) = B
    detA3 = determinant_3x3(A3)

    ! Compute solutions
    X(1) = detA1 / detA
    X(2) = detA2 / detA
    X(3) = detA3 / detA
    end subroutine solve_3x3



    function determinant_3x3(M) result(det)
    real(dp), intent(in) :: M(3,3)
    real(dp) :: det
    det = M(1,1)*(M(2,2)*M(3,3) - M(2,3)*M(3,2)) &
        - M(1,2)*(M(2,1)*M(3,3) - M(2,3)*M(3,1)) &
        + M(1,3)*(M(2,1)*M(3,2) - M(2,2)*M(3,1))
    end function determinant_3x3

end program GPSLocating