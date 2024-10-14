program VPDistribution

    implicit none
    
    character(len = 100) :: line
    
    integer :: n, i
    integer :: io_status, unit_num
    real :: x, y, z, w

    real, allocatable :: Distance(:), Depth(:)
    real, allocatable :: Vp(:), Vp_perturbation(:)
    real, allocatable :: Long(:), Lat(:)

    real :: symbol_color_R, symbol_color_G, symbol_color_B

    real :: Vp_dummy
    real :: Vp_perturbation_dummy

    ! Get earthquake data ----------------------------------------------------------------
    open(newunit = unit_num, file = "../data/Vp_prof.dat", status = "old", action = "read")
    read(unit_num, *, iostat=io_status)
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


    Depth = Depth * -1
    


    call pgopen('Vp_Profile.ps/VCPS')
    call pgsubp(1, 2)
    call pgsci(1)
    call pgenv(minval(Distance), maxval(Distance), minval(Depth) - 20, maxval(Depth), 0, 1)
    call pgscf(1)
    call pglab('Distance [km]', 'Depth [km]', 'Vp Profile')
    
    
    do i = 1, n - 1

        Vp_dummy = Vp(i)
        if (Vp_dummy < 3.0) then
            Vp_dummy = 3.0
        end if
        if (Vp_dummy > 8.0) then
            Vp_dummy = 8.0
        end if

        symbol_color_R = get_color_R(Vp_dummy)
        symbol_color_G = get_color_G(Vp_dummy)
        symbol_color_B = get_color_B(Vp_dummy)

        call pgscr(42, symbol_color_R, symbol_color_G, symbol_color_B)
        call pgsci(42)
        call pgrect(Distance(i) - 1.0, Distance(i) + 1.0, Depth(i) - 1.0, Depth(i) + 1.0)

    end do
    


    ! Second Plot

    call pgsci(1)
    call pgenv(minval(Distance), maxval(Distance), minval(Depth) - 20, maxval(Depth), 0, 1)
    call pgscf(1)
    call pglab('Distance [km]', 'Depth [km]', 'Vp Perturbation Profile')
    
    
    do i = 1, n - 1

        Vp_perturbation_dummy = Vp_perturbation(i) / 100
        
        if (Vp_perturbation_dummy <= -0.1) then
            Vp_perturbation_dummy = -0.1
        end if

        if (Vp_perturbation_dummy >= 0.1) then
            Vp_perturbation_dummy = 0.1
        end if

        symbol_color_R = get_RB_R(Vp_perturbation_dummy)
        symbol_color_G = get_RB_G(Vp_perturbation_dummy)
        symbol_color_B = get_RB_B(Vp_perturbation_dummy)

        call pgscr(43, symbol_color_R, symbol_color_G, symbol_color_B)
        call pgsci(43)
        call pgrect(Distance(i) - 1.0, Distance(i) + 1.0, Depth(i) - 1.0, Depth(i) + 1.0)

        ! if (i < 40) then
        !     print *, Vp_perturbation(i)
        ! end if

    end do
    
    call pgclos()
    
    
    
    
    deallocate(Distance, Depth)
    deallocate(Vp, Vp_perturbation)
    deallocate(Long, Lat)

    contains

    function get_color_R(velocity) result(color_R)
    real :: velocity
    real :: color_R

    if ( velocity > 7.0 .AND. velocity <= 9.0) then
        color_R = 0.0
    end if

    if (velocity >= 5.0 .AND. velocity <= 7.0) then
        color_R = 1 - ((velocity - 5.0) / 2.0)
    end if

    if ( velocity >= 3.0 .AND. velocity < 5.0) then
        color_R = 1.0
    end if
    
    end function get_color_R



    function get_color_G(velocity) result(color_G)
    real :: velocity
    real :: color_G

    if (velocity > 7.0 .AND. velocity <= 9.0) then
        color_G = 1 - ((velocity - 7.0) / 2.0)
    end if

    if (velocity >= 5.0 .AND. velocity <= 7.0) then
        color_G = 1.0
    end if

    if (velocity >= 3.0 .AND. velocity < 5.0) then
        color_G = (velocity - 3.0) / 2
    end if
    
    end function get_color_G


    function get_color_B(velocity) result(color_B)
    real :: velocity
    real :: color_B

    if (velocity > 7.0 .AND. velocity <= 9.0) then
        color_B = 1.0
    end if

    if (velocity >= 5.0 .AND. velocity <= 7.0) then
        color_B = (velocity - 5.0) / 2
    end if

    if (velocity >= 3.0 .AND. velocity < 5.0) then
        color_B = 0.0
    end if
    
    end function get_color_B





    function get_RB_R(velocity_pertub) result(RB_R)
    real :: velocity_pertub
    real :: RB_R

    if (velocity_pertub >= 0.0 .AND. velocity_pertub <= 0.1) then
        RB_R = 1.0 - (velocity_pertub / 0.1)
    end if

    if (velocity_pertub >= -0.1 .AND. velocity_pertub <= 0.0) then
        RB_R = 1.0
    end if
    
    end function get_RB_R





    function get_RB_G(velocity_pertub) result(RB_G)
    real :: velocity_pertub
    real :: RB_G

    if (velocity_pertub >= 0.0 .AND. velocity_pertub <= 0.1) then
        RB_G = 1.0 - (velocity_pertub / 0.1)
    end if

    if (velocity_pertub >= -0.1 .AND. velocity_pertub <= 0.0) then
        RB_G = 1.0 - (velocity_pertub * -1 / 0.1)
    end if
    
    end function get_RB_G




    function get_RB_B(velocity_pertub) result(RB_B)
    real :: velocity_pertub
    real :: RB_B

    if (velocity_pertub >= 0.0 .AND. velocity_pertub <= 0.1) then
        RB_B = 1.0
    end if

    if (velocity_pertub >= -0.1 .AND. velocity_pertub <= 0.0) then
        RB_B = 1.0 - (velocity_pertub * -1 / 0.1)
    end if
    
    end function get_RB_B

end program VPDistribution
