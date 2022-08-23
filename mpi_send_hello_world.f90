program hello
    
    implicit none
    include "mpif.h"

    integer rank, n_ranks, ierr
    integer sender
    integer status(MPI_STATUS_SIZE)
    charachter (len=40) message

    call MPI_Init(ierr)

    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks, ierr)

    if(rank .NE. 0) then
        
        write(message,*) "Hello World, I'm rank", rank
        call MPI_Send(message, 40, MPI_CHARACHTER, 0, 0, MPI_COMM_WORLD, ierr)

    else

        do sender = 1, n_ranks-1
            call MPI__Recv(message, 40, MPI_CHARACHTER, sender, 0, MPI_COMM_WORLD, status, ierr)
            write(6,*) message
        end do
    end if
    
    call MPI_Finalize(ierr)
end