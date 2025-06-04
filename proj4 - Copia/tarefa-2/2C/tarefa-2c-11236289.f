C	tarefa-2c-11236289.f
	implicit real*8 (a-h,o-z)
	parameter (pi=acos(-1.d0))
	open(unit=1,file='saida-2c-11236289.dat')
C	Constantes:
	theta = pi/6.d0 !Theta inicial.
	omega = 0.d0
	g = 0.05d0 !Gamma.
	om = 2.d0/3.d0 !Omega.
	F = 0.5d0 !Forca.
	dt = 0.04d0 !Intervalo.
	t = 0.d0
	write(1,*) t,theta,omega
C	Iteracao:
	do i = 1,10000
		a = -(dsin(theta))-(g*omega)+(F*dsin(t*om))
		t = t + dt
		omega=omega + a*dt
		theta=theta+omega*dt
		if(abs(theta).gt.2*pi) then
			theta = mod(theta,2*pi)
		end if
		write(1,*) t,theta,omega
	end do
	close(1)
	end  
