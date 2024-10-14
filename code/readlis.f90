 program readlis
    implicit none
    character(len = 100) :: line
    
    integer :: n, i
    integer :: io_status, unit_num
    real :: x, y, z, w

    real, allocatable :: EQ_X(:), EQ_Y(:), EQ_Z(:), EQ_M(:)

    ! Get earthquake data ----------------------------------------------------------------
    open(newunit = unit_num, file = "../data/1999.lis", status = "old", action = "read")
    n = 0
    do
        read(unit_num, '(A)', iostat=io_status) line
        if (io_status /= 0) exit
        n = n + 1
    end do
    close(unit_num)
    allocate(EQ_X(n), EQ_Y(n), EQ_Z(n), EQ_M(n))

    open(newunit=unit_num, file = "../data/1999.lis", status = "old", action = "read")
    do i = 1, n
        read(unit_num, '(18X, F2.0, F5.2, F3.0, F5.2, F5.2)', iostat=io_status) x, y, z, w, EQ_Z(i)
        EQ_X(i) = (z + w / 60)
        EQ_Y(i) = (x + y / 60)


        if (io_status /= 0) exit
    end do
    close(unit_num)
    ! Get earthquake data ----------------------------------------------------------------


    print *, EQ_Z


    deallocate(EQ_X, EQ_Y, EQ_Z, EQ_M)
end program readlis