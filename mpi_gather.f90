program Hello

    implicit none
    include "mpif.h"

    integer n_ranks, rank, sender, ierr
    charachter(len=40) send_message
    charachter, dimension(:), alloatable :: receive_message

    call MPI_Init(ierr)

    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks, ierr)

    allocate (receive_messages(n_ranks*40))

    write(send_message,*) "Hello World, I'm rank", rank
    call MPI_Gather(send_message, 40, MPI_CHARACHTER, receive_message, 40, MPI_CHARACHTER, 0, MPI_COMM_WORLD, ierr)

    if(rank == 0) then
        do sender = 1, n_ranks-1
            write(6,*) receive_message(40*sender: 40*(sender+1))
        end do
    end if

    deallocate(receive_message)
    call MPI_Finalize(ierr)
end