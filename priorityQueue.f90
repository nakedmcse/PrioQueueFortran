! Main Program
program priority_queue_example
    use priority_queue
    implicit none
    
    integer, parameter :: range = 10000000
    type (queue) :: simple_prio
    type (node) :: x
    real :: start_time, end_time, elapsed
    character(len=32) :: buffer
    character(len=256) :: test_out
    integer :: count_start, count_end, count_rate, count, i, rate

    ! Get clock rate
    call system_clock(count_rate=rate)

    call simple_prio%enqueue(1, 1)
    call simple_prio%enqueue(1, 2)
    call simple_prio%enqueue(1, 3)
    call simple_prio%enqueue(5, 4)
    call simple_prio%enqueue(9, 5)

    ! Dequeue and check order
    test_out = ''
    do while(simple_prio%size > 0)
        x = simple_prio%top()
        write(buffer, '(I2)') x%value
        test_out = trim(test_out) // ' ' // trim(buffer)
    end do
    print *, "Simple Priority Queue Dequeued:", test_out

    ! Speed test for enqueue
    call system_clock(count_start)
    do i = 1, range
        call simple_prio%enqueue(int(rand() * 10), int(rand() * range))
    end do
    call system_clock(count_end)
    elapsed = real(count_end - count_start) / real(rate)
    print *, "Simple priority queue enqueue time:", elapsed

    ! Speed test for dequeue
    count = 0
    count_start = 0
    count_end = 0
    call system_clock(count_start)
    do while(simple_prio%size > 0)
        x = simple_prio%top()
        count = count + 1
    end do
    call system_clock(count_end)
    elapsed = real(count_end - count_start) / real(rate)
    print *, "Simple priority queue dequeued items:", count
    print *, "Simple priority queue dequeue time:", elapsed
end program priority_queue_example