 program waveform
    implicit none
    integer :: n, i
    integer :: io_status, unit_num
    character(len = 100) :: line

    real, allocatable :: CONTOUR_X(:), CONTOUR_Y(:)
    real :: ax_x_max, ax_x_min, ax_y_max, ax_y_min, scale_ratio

    ! open(newunit = unit_num, file = "../data/seisdata.txt", status = "old", action = "read")
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

    print *, CONTOUR_X, CONTOUR_Y


    call pgopen('1999_event_distribution.ps/VCPS')
    ! call pgsubp(1, 3)

    call pgsci(1) 
    call pgslw(4)
    call pgsch(1.5)
    call pgscf(2)

    ! Plot E1

    scale_ratio = 1.25
    ax_x_min = minval(CONTOUR_X) * scale_ratio
    ax_x_max = maxval(CONTOUR_X) * scale_ratio
    ax_y_min = minval(CONTOUR_Y) * scale_ratio
    ax_y_max = maxval(CONTOUR_Y) * scale_ratio

    call pgsci(1)
    call pgenv(ax_x_min, ax_x_max, ax_y_min, ax_y_max, 0, 1)
    call pglab('Longitude (E)', 'Latitude (N)', '1999 Event Distribution')
    call pgsci(2)
    call pgline(n, CONTOUR_X, CONTOUR_Y)

    call pgclos()

    deallocate(CONTOUR_X, CONTOUR_Y)
end program waveform
