C	tarefa-1a-11236289.f
	implicit real*8 (a-h,o-z)
	parameter (pi = dacos(-1.d0))
	parameter (piq = 4*(pi**2))
	dimension r(9)
	open(unit=1,file='saida-1a-11236289.dat')
	open(unit=2,file='entrada-1-11236289.dat')
	do i = 1,9
		read(2,*) r(i) !Le os valores de raio.
	end do
	write(*,*) '----------Valores:----------'
	do j = 1,9
C	Parametros:
		dt = 1.d-3 
		t = 0.d0
		x_old = r(j) !Raio.
		y_old = 0.d0
		vy = (pi*4.2d0) / sqrt(r(j)) !Vel. para circ.
		x1 = x_old
		y1 = y_old + vy*dt
		i_cond = 0 !Termo que reflete o periodo.
C	Iteracao sobre movimento:
		do i = 1,50000
			t = dt + t
			r_t=sqrt(x1**2 + y1**2)
			ax = -(piq*x1/(r_t**3))
			ay = -(piq*y1/(r_t**3))
			x = (2.d0*x1) - x_old + (ax*(dt**2))
			y = (2.d0*y1) - y_old + (ay*(dt**2))
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
		!write(*,*) (t**2)/(r(j)**3)
	end do
	close(1)
	close(2)
	end	
