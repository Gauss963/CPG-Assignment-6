program OnLands
    implicit none

    character(len = 100) :: line
    
    integer :: n, m, i
    integer :: io_status, unit_num

    real, allocatable :: CONTOUR_X(:), CONTOUR_Y(:)
    real :: x, y, z, w
    real, allocatable :: EQ_X(:), EQ_Y(:), EQ_Z(:), EQ_M(:)

    ! Get contour data -------------------------------------------------------------------
    open(newunit = unit_num, file = "../data/Taiwan.txt", status = "old", action = "read")
    n = 0
    do
        read(unit_num, '(A)', iostat=io_status) line
        if (io_status /= 0) exit
        n = n + 1
    end do
    close(unit_num)
    allocate(CONTOUR_X(n), CONTOUR_Y(n))
    open(newunit=unit_num, file = "../data/Taiwan.txt", status = "old", action = "read")
    do i = 1, n
        read(unit_num, *, iostat=io_status) CONTOUR_X(i), CONTOUR_Y(i)
        if (io_status /= 0) exit
    end do
    close(unit_num)


    ! Get earthquake data ----------------------------------------------------------------
    open(newunit = unit_num, file = "../data/1999.lis", status = "old", action = "read")
    m = 0
    do
        read(unit_num, '(A)', iostat=io_status) line
        if (io_status /= 0) exit
        m = m + 1
    end do
    close(unit_num)
    allocate(EQ_X(m), EQ_Y(m), EQ_Z(m), EQ_M(m))

    open(newunit=unit_num, file = "../data/1999.lis", status = "old", action = "read")
    do i = 1, m
        read(unit_num, '(18X, F2.0, F5.2, F3.0, F5.2, F6.2, F5.2)', iostat=io_status) x, y, z, w, EQ_Z(i), EQ_M(i)
        EQ_X(i) = (z + w / 60)
        EQ_Y(i) = (x + y / 60)


        if (io_status /= 0) exit
    end do
    close(unit_num)

    do i = 1, 3
        print *, EQ_X(i), EQ_Y(i)
    end do

    deallocate(CONTOUR_X, CONTOUR_Y)
    deallocate(EQ_X, EQ_Y, EQ_Z, EQ_M)

end program OnLands