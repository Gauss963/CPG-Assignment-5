program LinearRegressionPlotting
    implicit none
    integer :: n
    real, allocatable :: Xi(:), Yi(:)
    real :: a, b, sdv, R, std_a, std_b

    ! real :: longitude, origin_longitude_2
    ! real :: origin_longitude_1, origin_longitude_2
    ! real :: origin_latitude_1, origin_latitude_2

    character(len = 100) :: line
    integer :: i, unit_num

    real :: xsec
    integer iy, im, id, ih, mm

    ! Set the number of points
    n = 30


    ! Allocate Xi and Yi
    allocate(Xi(n), Yi(n))


    open(newunit=unit_num, file = "../data/ppfile.txt", status = "old", action = "read")
    
    
    ! Get the number of lines-----------------
    ! Skipping header
    read(unit_num, '(A)') ! Do nothing
    ! Get the number of lines
    n = 0
    do while (.true.)
        read(unit_num, '(A)', iostat=i) line
        if (i /= 0) exit
        n = n + 1
    end do
    ! Get the number of lines-----------------


    ! Read the datas and skipping header------
    rewind(unit_num)
    read(unit_num,'(1x, i4, 4i2, f6.2)') iy, im, id, ih, mm, xsec
    xsec = mm * 60.0 + xsec


    ! Read data and fill into Xi and Yi
    do i = 1, n
        ! read(unit_num, '(6X, F5.1, F4.0)') Xi(i), Yi(i)
        read(unit_num,'(5x, f6.1, 9x, i3, f6.2)') Xi(i), mm, Yi(i)
        Yi(i) = mm * 60.0 + Yi(i) - xsec
    end do
    ! Read the datas and skipping header------

    close(unit_num)
    
    call LinearRegressionFitting(Xi, Yi, n, a, b, sdv, R, std_a, std_b)

    print *, 'Constant a: ', a
    print *, 'Constant b: ', b
    print *, 'Standard deviation of fit (sdv): ', sdv
    print *, 'Linear correlation coefficient (R): ', R
    print *, 'Standard deviation of a: ', std_a
    print *, 'Standard deviation of b: ', std_b


    deallocate(Xi, Yi)

end program LinearRegressionPlotting