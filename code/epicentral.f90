 program waveform
    implicit none
    character(len = 100) :: line
    
    integer :: n, i
    integer :: io_status, unit_num

    real, allocatable :: CONTOUR_X(:), CONTOUR_Y(:)
    real, allocatable :: EQ_X(:), EQ_Y(:), EQ_Z(:), EQ_M(:)
    real :: ax_x_max, ax_x_min, ax_y_max, ax_y_min, margen

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
    ! open(newunit = unit_num, file = "../data/1999.lis", status = "old", action = "read")
    ! n = 0
    ! do
    !     read(unit_num, '(A)', iostat=io_status) line
    !     if (io_status /= 0) exit
    !     n = n + 1
    ! end do
    ! close(unit_num)
    ! allocate(EARTHQUAKE_X(n), EARTHQUAKE_Y(n))

    ! open(newunit=unit_num, file = "../data/1999.lis", status = "old", action = "read")
    ! do i = 1, n
    !     read(unit_num, *, iostat=io_status) EARTHQUAKE_X(i), EARTHQUAKE_Y(i), EARTHQUAKE_Z(i), EARTHQUAKE_M(i)
    !     if (io_status /= 0) exit
    ! end do
    ! close(unit_num)
    ! Get earthquake data ----------------------------------------------------------------


    ! Set x, y lims
    margen = 0.25  ! 25% larger
    ax_x_min = minval(CONTOUR_X) - margen
    ax_x_max = maxval(CONTOUR_X) + margen
    ax_y_min = minval(CONTOUR_Y) - margen
    ax_y_max = maxval(CONTOUR_Y) + margen

    ! Plot Contour
    call pgopen('1999_event_distribution.ps/VCPS')
    call pgsci(1)
    call pgenv(ax_x_min, ax_x_max, ax_y_min, ax_y_max, 0, 1)
    call pgscf(2)
    call pglab('Longitude (E)', 'Latitude (N)', '1999 Event Distribution')
    call pgline(n, CONTOUR_X, CONTOUR_Y)
    
    ! Plot Contour
    call pgmove(0, 0)
    call pgsci(2)
    
    
    
    call pgclos()

    deallocate(CONTOUR_X, CONTOUR_Y)
    ! deallocate(EARTHQUAKE_X, EARTHQUAKE_Y, EARTHQUAKE_Z, EARTHQUAKE_M)
end program waveform
