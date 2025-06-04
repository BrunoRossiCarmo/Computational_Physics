C	tarefa-2-2-11236289.f
	implicit real*8 (a-h,o-z)
	parameter (pi=acos(-1.d0))
	open(unit=1,file='saida-2b-11236289.f')
	dt = 0.01d0
	theta = pi/3.d0
	omega = 0.d0
	do i = 1,10000
		omega=omega-(dsin(theta))*dt-(1.d0/2.d0)*omega*dt
		theta = theta + omega*dt
		t = t + dt
		if(theta + omega*dt.gt.2*pi) then
			theta = mod(theta,2*pi)
		end if
		write(1,*) t,theta
	end do
	write(*,*) 'Codigo executado'
	end
			
