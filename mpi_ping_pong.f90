program pingpong

    implicit none
    include "mpif.h"

    integer ball, max_count, counter
    logical bored
    integer rank, neighbour, ierr
    integer status(MPI_STATUS_SIZE)

    ball = 1
    max_count = 1000000

    call MPI_Init(ierr)

    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

    if (rank == 0) then
        neighbour = 1
    else 
        neighbour = 0
    end if

    if (rank == 0) then
        call MPI_Send(ball, 1, MPI_INTEGER, neighbour, 0, MPI_COMM_WORLD, ierr)
    end if

    counter = 0
    bored = .false.

    do while(.NOT. bored)
        call MPI_Recv(ball, 1, MPI_INTEGER, neighbour, 0, MPI_COMM_WORLD, status, ierr)

        counter = counter + 1
        call MPI_Send(ball, 1, MPI_INTEGER, neighbour, 0 MPI_COMM_WORLD, ierr)

        bored = counter >= max_count
    end do
    
    write(6,*) "Rank ", rank, "is bored and giving up"

    call MPI_Finalize(ierr)
end