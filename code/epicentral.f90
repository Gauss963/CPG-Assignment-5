 program waveform
    implicit none
    integer :: n, i
    integer :: io_status, unit_num
    character(len = 100) :: line
    real, allocatable :: T(:), E1(:), E2(:), E3(:)
    real :: peak_value1, peak_value2, peak_value3
    integer :: peak_idx1, peak_idx2, peak_idx3
    character(len=20) :: peak_str1, peak_str2, peak_str3

    real :: avg1, avg2, avg3

    open(newunit=unit_num, file = "../data/seisdata.txt", status = "old", action = "read")

    n = 0
    do
        read(unit_num, '(A)', iostat=io_status) line
        if (io_status /= 0) exit
        n = n + 1
    end do
    close(unit_num)
    allocate(T(n), E1(n), E2(n), E3(n))

    open(newunit=unit_num, file = "../data/seisdata.txt", status = "old", action = "read")
    do i = 1, n
        read(unit_num, *, iostat=io_status) T(i), E1(i), E2(i), E3(i)
        if (io_status /= 0) exit
    end do
    close(unit_num)

    avg1 = sum(E1)/size(E1)
    avg2 = sum(E2)/size(E2)
    avg3 = sum(E3)/size(E3)

    E1 = E1 - avg1
    E2 = E2 - avg2
    E3 = E3 - avg3

    peak_value1 = maxval(E1)
    peak_value2 = maxval(E2)
    peak_value3 = maxval(E3)

    peak_idx1 = maxloc(E1, dim=1)
    peak_idx2 = maxloc(E2, dim=1)
    peak_idx3 = maxloc(E3, dim=1)

 
    write(peak_str1, '(F8.2)') peak_value1
    write(peak_str2, '(F8.2)') peak_value2
    write(peak_str3, '(F8.2)') peak_value3

    call pgopen('waveform_plot.ps/VCPS')
    call pgsubp(1, 3)

    call pgsci(1) 
    call pgslw(4)
    call pgsch(1.5)
    call pgscf(2)

    ! Plot E1
    call pgsci(1)
    call pgenv(minval(T), maxval(T), minval(E1), maxval(E1), 0, 1)
    call pglab('Time (sec)', 'Amplitude', 'Waveform E1')
    call pgsci(2)
    call pgline(n, T, E1)

    ! Label E1 peak
    call pgpt(1, [T(peak_idx1)], [peak_value1], 17)
    call pgtext(T(peak_idx1), peak_value1 - 2, 'Peak='//trim(adjustl(peak_str1)))

    ! Plot E2
    call pgsci(1)
    call pgenv(minval(T), maxval(T), minval(E2), maxval(E2), 0, 1)
    call pglab('Time (sec)', 'Amplitude', 'Waveform E2')
    call pgsci(3)
    call pgline(n, T, E2)

    ! Label E2 peak
    call pgpt(1, [T(peak_idx2)], [peak_value2], 17)
    call pgtext(T(peak_idx2), peak_value2 - 2, 'Peak='//trim(adjustl(peak_str2)))

    ! Plot E3
    call pgsci(1)
    call pgenv(minval(T), maxval(T), minval(E3), maxval(E3), 0, 1)
    call pglab('Time (sec)', 'Amplitude', 'Waveform E3')
    call pgsci(4)
    call pgline(n, T, E3)

    ! Label E3 peak
    call pgpt(1, [T(peak_idx3)], [peak_value3], 17)
    call pgtext(T(peak_idx3), peak_value3 - 2, 'Peak='//trim(adjustl(peak_str3)))

    call pgclos()

    deallocate(T, E1, E2, E3)
end program waveform
