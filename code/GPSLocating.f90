program GPSLocating
    use, intrinsic :: iso_fortran_env
    implicit none

    integer, parameter :: dp = real64
    integer :: N, i, j, k
    real(dp), allocatable :: v_QC1(:), v_QC2(:), v_QC3(:), v_d(:), v_m(:)
    real(dp), allocatable :: m_Q(:,:), m_QT(:,:)

    real(dp) :: m_LHS(3, 3)
    real(dp) :: v_RHS()

    ! For LAPACK routine DGELS
    external :: DGELS

    ! Prompt user for the number of stations
    print *, 'Enter the number of stations (N >= 4):'
    read *, N
    if (N < 4) then
        print *, 'At least 4 stations are required.'
        stop
    end if

    ! Allocate arrays
    allocate(v_QC1(N), v_QC2(N), v_QC3(N), v_d(N), v_m(N))

    ! Input station coorv_dnates and v_dstances
    print *, 'Enter the coorv_dnates (v_QC1, v_QC2, v_QC3) and v_dstance v_d for each station:'
    do i = 1, N
        print *, 'Station ', i, ':'
        read *, v_QC1(i), v_QC2(i), v_QC3(i), v_d(i)
    end do

    allocate(m_Q(N, 3), m_QT(3, N))
    do j = 1, N
        m_Q(j, 1) = v_QC1(j)
        m_Q(j, 2) = v_QC2(j)
        m_Q(j, 3) = v_QC3(j)
    end do

    ! Print the matrix to check
    print *, 'Matrix Q:'
    do k = 1, N
        print *, m_Q(k, 1), m_Q(k, 2), m_Q(k, 3)
    end do

    m_QT = transpose(m_Q)
    v_m = matmul(matmul(matmul(m_QT, m_Q)**(-1), m_QT), v_d) ! Complete this part

    m_LHS = matmul(m_QT, m_Q)

    call solve_3x3()


    ! Output the result
    print *, 'The estimated coordinates of the target point are:'
    print *, 'X = ', v_m(1)
    print *, 'Y = ', v_m(2)
    print *, 'Z = ', v_m(3)



    deallocate(v_QC1, v_QC2, v_QC3, v_d, v_m)
    deallocate(m_Q)

end program GPSLocating