C	tarefa-1-11236289.f
        implicit real*8 (a-h,o-z)
        parameter (pi=acos(-1.d0))
        open(unit=2,file='saida-2-11236289.f')
        open(unit=1,file='saida-1-11236289.f')
        dt = 0.01d0
        theta = pi/4.d0
        omega = 0.d0
        do i = 1,10000
            t = t + dt
            omega_arm = omega
            omega=omega-theta*dt
            theta = theta + omega_arm*dt
            if(theta + omega*dt.gt.2*pi) then
                theta = mod(theta,2*pi)
            end if
            write(1,*) t,theta
        end do
        do j = 1,10000
            t = t + dt
            omega=omega-theta*dt
            theta = theta + omega*dt
            if(theta + omega*dt.gt.2*pi) then
                theta = mod(theta,2*pi)
            end if
            write(2,*) t,theta
        end do
        close(2)
        close(1)
        write(*,*) 'Codigo executado'
        end