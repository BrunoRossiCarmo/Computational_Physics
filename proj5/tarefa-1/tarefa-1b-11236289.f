C	tarefa-1b-11236289.f
	implicit real*8 (a-h,o-z)
	parameter (pi = dacos(-1.d0))
	parameter (piq = 4*(pi**2))
	dimension r(9)
	open(unit=1,file='saida-1b-11236289.dat')
	open(unit=2,file='entrada-1-11236289.dat')
	open(unit=3,file='saida-2b-11236289.dat')
	open(unit=4,file='saida-3b-11236289.dat')
	do i = 1,9
		read(2,*) r(i) !Le os valores de raio.
	end do
	do j = 1,9 !Fazendo apenas com a Terra.
C	Parametros:
		dt = 1.d-3 
		t = 0.d0
		x_old = r(j) !Raio.
		y_old = 0.d0
		vy = (pi*2.2d0) / sqrt(r(j)) !Vel. para n. circ.
		!Basta estar entre vel. circula < v < velocidade de escape
		!1/2*V^2*m - GmM/R (potencial) = 0 => V = sqrt(2GmM/R*m)
		x1 = x_old
		y1 = y_old + vy*dt
		r_max = 0 !Semi-eixo maior.
		summ = 0
		summ2 = 0
		i_cond = 0 !Termo que reflete o periodo.
C	Iteracao sobre movimento:
		do while(i_cond.ne.1)
			t = dt + t
			r_t=sqrt(x1**2 + y1**2)
			!Constancia do periodo^2/(semi-eixo maior)^3
			if(r_t.gt.r_max) then
				r_max = r_t
			end if
			ax = -(piq*x1/(r_t**3))
			ay = -(piq*y1/(r_t**3))
			x = (2.d0*x1) - x_old + (ax*(dt**2))
			y = (2.d0*y1) - y_old + (ay*(dt**2))
			!Areas iguais percorridas:
			if((y1.gt.0.d0)) then
				summ = r_t*dt + summ
			end if
			if((y1.lt.0.d0)) then
				summ2 = r_t*dt + summ2
			end if
			write(1,*) x,y,t
			x_old = x1
			y_old = y1
			x1 = x
			y1 = y
C	Nesta condicao, colocamos na troca de sinal no fim da circ.
			if((y_old*y1.lt.0).and.(y_old.lt.y1)) then
				i_cond = 1
			end if
		end do
		write(*,*) t**2/r(j)**3
	end do
	close(1)
	close(2)
	end	