! Priority queue module
module priority_queue
    implicit none

    type node
        integer :: value
        integer :: priority
    end type

    type queue
        type(node), allocatable :: buf(:)
        integer                 :: size = 0
    contains
        procedure :: top
        procedure :: enqueue
        procedure :: siftdown
    end type

    contains

    ! Insert into heap
    subroutine siftdown(this, a)
        class (queue) :: this
        integer :: a, parent, child
        associate (x => this%buf)
        parent = a
        do while(parent*2 <= this%size)
            child = parent*2
            if (child + 1 <= this%size) then 
                if (x(child+1)%priority > x(child)%priority ) then
                    child = child +1 
                end if
            end if
            if (x(parent)%priority < x(child)%priority) then
                x([child, parent]) = x([parent, child])
                parent = child
            else
                exit
            end if  
        end do      
        end associate
    end subroutine

    ! Return top of heap
    function top(this) result (res)
        class(queue) :: this
        type(node)   :: res
        res = this%buf(1)
        this%buf(1) = this%buf(this%size)
        this%size = this%size - 1
        call this%siftdown(1)
    end function

    ! Enqueue
    subroutine enqueue(this, priority, value)
        class(queue), intent(inout) :: this
        integer                     :: priority
        integer                     :: value
        type(node)                  :: x
        type(node), allocatable     :: tmp(:)
        integer                     :: i
        x%priority = priority
        x%value = value
        this%size = this%size +1  
        if (.not.allocated(this%buf)) allocate(this%buf(1))
        if (size(this%buf)<this%size) then
            allocate(tmp(2*size(this%buf)))
            tmp(1:this%size-1) = this%buf
            call move_alloc(tmp, this%buf)
        end if
        this%buf(this%size) = x
        i = this%size
        do 
            i = i / 2
            if (i==0) exit
            call this%siftdown(i)
        end do
    end subroutine
end module