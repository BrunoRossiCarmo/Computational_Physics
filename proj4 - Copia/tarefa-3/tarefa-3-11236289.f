C	tarefa-2c-11236289.f
	implicit real*8 (a-h,o-z)
	parameter (pi=acos(-1.d0))
	open(unit=1,file='saida-2-11236289.dat')
C	Constantes:
	theta = pi/40.d0 !Theta inicial.
	theta2 = theta - 0.001e0
	omega = 0.d0
	omega2 = omega
	g = 0.05d0 !Gamma.
	om = 2.d0/3.d0 !Omega.
	F = 1.2d0 !Forca.
	dt = 0.04d0 !Intervalo.
	t = 0.d0
	write(1,*) t, abs(theta-theta2)
C	Iteracao:
	do i = 1,10000
		a1 = -(dsin(theta))-(g*omega)+(F*dsin(t*om))
		a2 = -(dsin(theta2))-(g*omega2)+(F*dsin(t*om))
		t = t + dt
		omega=omega + a1*dt
		omega2=omega2 + a2*dt
		theta=theta+omega*dt
		theta2=theta2+omega2*dt
		if(abs(theta).gt.2*pi) then
			theta = mod(theta,2*pi)
		end if
		if(abs(theta2).gt.2*pi) then
			theta2 = mod(theta2,2*pi)
		end if
		write(1,*) t, abs(theta-theta2)
	end do
	close(1)
	end  
