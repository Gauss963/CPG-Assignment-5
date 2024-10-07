 program epicentral
    implicit none
    character(len = 100) :: line
    
    integer :: n, m, i
    integer :: io_status, unit_num

    real, allocatable :: CONTOUR_X(:), CONTOUR_Y(:)
    real :: x, y, z, w
    real, allocatable :: EQ_X(:), EQ_Y(:), EQ_Z(:), EQ_M(:)
    
    real :: ax_x_max, ax_x_min, ax_y_max, ax_y_min, margen

    real :: EQ_M_min, EQ_M_max
    real :: symbol_size, size_min, size_max

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
    ! Get contour data -------------------------------------------------------------------

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
        read(unit_num, '(18X, F2.0, F5.2, F3.0, F5.2, F5.2)', iostat=io_status) x, y, z, w, EQ_Z(i)
        EQ_X(i) = (z + w / 60)
        EQ_Y(i) = (x + y / 60)


        if (io_status /= 0) exit
    end do
    close(unit_num)
    ! Get earthquake data ----------------------------------------------------------------


    ! Set x, y lims
    margen = 0.25  ! 25% larger
    ax_x_min = minval(CONTOUR_X) - margen
    ax_x_max = maxval(CONTOUR_X) + margen
    ax_y_min = minval(CONTOUR_Y) - margen
    ax_y_max = maxval(CONTOUR_Y) + margen

    ! Set scatter size lims
    EQ_M_min = minval(EQ_Y)
    EQ_M_max = maxval(EQ_Y)
    size_min = 0.5
    size_max = 2.0


    ! Plot Contour
    call pgopen('1999_event_distribution.ps/VCPS')
    call pgsci(1)
    call pgenv(ax_x_min, ax_x_max, ax_y_min, ax_y_max, 0, 1)
    call pgscf(2)
    call pglab('Longitude (E)', 'Latitude (N)', '1999 Event Distribution')
    call pgline(n, CONTOUR_X, CONTOUR_Y)
    
    ! Plot symbol size with "EQ_Y", chahge to EQ_M later
    call pgsci(2)
    do i = 1, n
        symbol_size = size_min + (EQ_Y(i) - EQ_M_min) * (size_max - size_min) / (EQ_M_max - EQ_M_min)
        
        call pgsch(symbol_size)
        
        call pgpt1(EQ_X(i), EQ_Y(i), 22)
    end do
    call pgsch(1.0)
    
    
    
    call pgclos()


    deallocate(CONTOUR_X, CONTOUR_Y)
    deallocate(EQ_X, EQ_Y, EQ_Z, EQ_M)
end program epicentral
