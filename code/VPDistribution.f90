program VPDistribution

    implicit none
    
    character(len = 100) :: line
    
    integer :: n, i
    integer :: io_status, unit_num
    real :: x, y, z, w

    real, allocatable :: Distance(:), Depth(:)
    real, allocatable :: Vp(:), Vp_perturbation(:)
    real, allocatable :: Long(:), Lat(:)

    ! Get earthquake data ----------------------------------------------------------------
    open(newunit = unit_num, file = "../data/Vp_prof.dat", status = "old", action = "read")
    n = 0
    do
        read(unit_num, '(A)', iostat=io_status) line
        if (io_status /= 0) exit
        n = n + 1
    end do
    close(unit_num)
    allocate(Distance(n), Depth(n))
    allocate(Vp(n), Vp_perturbation(n))
    allocate(Long(n), Lat(n))

    open(newunit=unit_num, file = "../data/Vp_prof.dat", status = "old", action = "read")
    read(unit_num, *, iostat=io_status)
    do i = 1, n
        read(unit_num, '(2X, F8.4, 2X, F8.4, 2X, F8.4, 2X, F10.5, F8.4, 2X, F8.4)', iostat=io_status) &
        Distance(i), Depth(i), Vp(i), Vp_perturbation(i), Long(i), Lat(i)


        if (io_status /= 0) exit
    end do
    close(unit_num)



    print *, Distance(1), Depth(1), Vp(1), Vp_perturbation(1), Long(1), Lat(1)

    ! Finish data reading
    
    deallocate(Distance, Depth)
    deallocate(Vp, Vp_perturbation)
    deallocate(Long, Lat)

end program VPDistribution
