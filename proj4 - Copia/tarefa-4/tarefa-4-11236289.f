C	tarefa-2c-11236289.f
	implicit real*8 (a-h,o-z)
	parameter (pi=acos(-1.d0))
	open(unit=1,file='saida-1-11236289.dat')
C	Constantes:
	g = 0.05d0 !Gamma.
	om = 2.d0/3.d0 !Omega.
	F = 0.5d0 !Forca.
	i_con = 0
	theta0 = 0.8d0
C	Iteracao:
	do while(theta0.gt.0.5d0)
		theta0 = 0.8d0-(i_con)*0.1d0
		theta = theta0
		dt = 0.04d0 !Intervalo.
		t = 0.d0
		omega = 0.d0
		do i = 1,5000
			a1 = -(dsin(theta))-(g*omega)+(F*dsin(t*om))
			t = t + dt
			omega=omega + a1*dt
			theta=theta+omega*dt
			if(abs(theta).gt.2*pi) then
				theta = mod(theta,2*pi)
			end if
			write(1,*) theta,omega
		end do
		i_con = i_con + 1
	end do
	close(1)
	write(*,*) 'Codigo executado'
	end  
