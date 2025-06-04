C	tarefa-2a-11236289.f
	implicit real*8 (a-h,o-z)
	parameter (pi=acos(-1.d0))
	parameter (N=100000)
	open(unit=1,file='saida-2a-11236289.dat')
	open(unit=2,file='saida-2b-11236289.dat')
C	Definir metodologia:
	do j = 4,12,4
C		Condicoes iniciais:
		omega = 0.d0
		theta = pi/(real(j,8))
		theta0 = theta !Para calcular o metodo de Boole.
		dt = 0.01d0
		t=0
C		Contadores:
		write(2,*) '------- Angulo:',theta0,'-------'
		i_cont = 0
		period_ant = 0 !Utilizar para encontrar o periodo anterior.
		period = 0 !Utilizar para encontrar o periodo.
C		Itera os valores angulares:
		do i = 1,1000 !Encontrar com melhor precisao os periodos.
			t = t + dt	
			omega=omega-(dsin(theta))*dt
			theta = theta + omega*dt
			if(theta + omega*dt.gt.2*pi) then
				theta = mod(theta,2*pi)
			end if
			!Calcular o periodo:
			period_ant = period
			if(((theta+omega*dt)*theta.lt.0.d0).and.(t.gt.4)) then
				period=t-period_ant !Encontra os periodos.
			end if
			write(1,*) t, theta
		end do
C		Itegral Eliptica utilizando metodo de Boole:
		h = 2*theta0/N
		bool = 0
		e = 1d-5
		do i = 0,N,4
			thet = (-theta0 +e) + i*h/2 !O intermediario se torna o ponto mais baixo. 
			p0=fun(thet,theta0)
			p1=fun(thet+h,theta0)
			p2=fun(thet+2*h,theta0)
			p3=fun(thet+3*h,theta0)
			p4=fun(thet+4*h,theta0)
			bool=bool+(7*p0+32*p1+12*p2+32*p3+7*p4)*((2*h)/45)
		end do
		period2 = (sqrt(2.d0)*bool) + (2*sqrt(2*e/dsin(theta0)))
		write(2,*)  2*period,period2
	end do
	close(1)
	close(2)
	write(*,*) 'Codigo executado!'
	end

C	Funcao calculada:
	function fun(x,theta0)
	implicit real*8 (a-h,o-z)
	fun = 1.d0/(sqrt(dcos(x)-dcos(theta0)))
	return
	end function