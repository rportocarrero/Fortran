program poisson

    implicit none

    integer, parameter :: GRIDSIZE=10
    integer ierr
    
    real u(0:(GRIDSIZE+1),unew(0:(GRIDSIZE+1))
    real rho(0:(GRIDSIZE+1))
    real h, hsq
    double precision unorm, residual

    h = 0.1
    hsq = h * h
    residual = 1e-5

    do i = 0, GRIDSIZE+1
        u(i) = 0.0
        rho(i) = 0.0
    enddo

    u(0) = 10.0

    do
        call poisson_step(u, unew, rho, GRIDSIZE, hsq, unorm)
        write(6,*) 'Iteration', i, ', residual', unorm

        if(sqrt(unorm) <= sqrt(residual)) then
            exit
        end if

    enddo

    write(6,*) 'Run completed with residual ', unorm

end

subroutine poisson_step(u,unew,rho,GRIDSIZE,hsq,unorm)

    implicit none

    integer, intent(in) :: GRIDSIZE
    real, intent(inout), dimension (0:(GRIDSIZE+1)) :: u, unew
    real, intent(in), dimension (0:(GRIDSIZE+1)) :: rho
    real, intent(in) :: hsq
    double precision, intent(out) :: unorm
    intger i, j

    do i=1, GRIDSIZE
        unew(i) = 0.25*(u(i-1)+u(i+1)-hsq*rho(i))
    enddo

    unorm = 0.0

    do i = 1, GRIDSIZE
        unorm = unorm + (unew(i)-u(i))*(unew(i)-u(i))
    enddo

    do i = 1, GRIDSIZE
        u(i) = unew(i)
    enddo
end