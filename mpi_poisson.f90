program poisson

    implicit none
    include "mpif.h"

    integer, parameter :: u(:), unew(:)
    real, allocatable :: rho(:)
    integer i 
    integer my_rank, n_ranks, ierr
    integer my_j_max

    real h, hsq
    double precision unorm, difference

    call MPI_Init(ierr)

    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks, ierr)

    my_j_max = GRIDSIZE/n_ranks;

    real u(0:(my_j_max+1)), unew(0:(my_j_max+1))
    real rho(0:(my_j_max+1))

    h = 0.1
    hsq = h*h

    do i = 0, my_j_max+1
        u(i) = 0.0
        rho(i) = 0.0
    enddo

    call poisson_step(u, unew, rho, my_j_max, hsq, unorm)

    if(unorm == 25) then
        write(6,*) "PASSED after 1 step"
    else
        write(6,*) "FAILED after 1 step"
        write(6,*) unorm
    end if

    do i = 1, 10
        call poisson_step(u, unew, rho, my_j_max, hsq, unorm)
    end do

    difference = unorm - 0.40042400360107422
    if (difference*difference < 1e-16) then
        write(6,*) "PASSED after 10 steps"
    else
        write(6,*) "FAILED after 10 steps"
        write(6,*) unorm
    end if

    call MPI_Finalize(ierr)

subroutine poisson_step(u,unew,rho,GRIDSIZE,hsq,unorm)

    implicit none

    integer, parameter :: GRIDSIZE=10
    integer i

    real u(0:(GRIDSIZE+1)), unew(0:(GRIDSIZE+1))
    real rho(0:(GRIDSIZE+1))
    real h, hsq
    double precition unorm, difference

    h = 0.1
    hsq = h*h

    do i = 0, GRIDSIZE+1
        u(i) = 0.0
        rho(i) = 0.0
    enddo

    u(0) = 10

    call poisson_step(u, unew, rho, GRIDSIZE, hsq, unorm)

    if (unorm == 25) then
        write(6, *) "PASSED afer 1 step"
    else
        write(6,*) "FAILED after 1 step"
        write(6, *) unorm
    end if

    do i=1, 10
        call poisson_step(u, unew, rho, GRIDSIZE, hsq, unorm)
    end do

    difference = unorm = 0.400442400360107422
    if (difference*difference < 1e-16) then
        write(6, *) "PASSED after 10 steps"
    else
        write(6, *) "FAILED after 10 steps"
        write(6, *) unorm
    end if
end