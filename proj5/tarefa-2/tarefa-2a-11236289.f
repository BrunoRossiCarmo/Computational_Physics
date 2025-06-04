C	tarefa-2a-11236289.f
	implicit real*8 (a-h,o-z)
	parameter (pi = dacos(-1.d0))
	parameter (piq = 4*(pi**2))
	open(unit=1,file='saida-2a-1-11236289.dat')
	open(unit=2,file='saida-2a-2-11236289.dat')
C	Parametros:
	dt = 1.d-3 
	t = 0.d0
C	-------Terra:-------
	x_old = 1.d0 !Raio Terra.
	y_old = 0.d0
	vy = (pi*2.d0) / sqrt(x_old) !Supostamente Vel. da Terra circular.
	x1 = x_old
	y1 = y_old + vy*dt
C	-------Jupiter:-------
	x2_old =  5.2d0 !Raio Jupiter.
	y2_old = 0.d0
	vy2 = (pi*2.d0) / sqrt(x2_old)  !Vel. para circ em Jupiter.
	x2 = x2_old
	y2 = y2_old + vy2*dt
C	Iteracao sobre movimento:
	do i = 1,20000
		t = dt + t
	!Terra:-------
		r_t=sqrt(x1**2 + y1**2)
		r_j=sqrt((x1-x2)**2 + (y1-y2)**2)
		ax =-(piq*x1/(r_t**3))-((piq*(x1-x2))/((r_j**3)*1.d3)) !Alterar aqui massa de jupiter.
		ay =-(piq*y1/(r_t**3))-((piq*(y1-y2))/((r_j**3)*1.d3)) !Alterar aqui massa de jupiter.
		x = (2.d0*x1) - x_old + (ax*(dt**2))
		y = (2.d0*y1) - y_old + (ay*(dt**2))
		x_old = x1
		y_old = y1
		x1 = x
		y1 = y
		write(1,*) x,y,t,r_t !Para tomar distancia terra-sol ao longo do tempo.
	!Jupiter:-------
		r_t2=sqrt(x2**2 + y2**2)
		r_j2=sqrt((x2-x1)**2 + (y2-y1)**2)
		ax2 =-(piq*x2/(r_t2**3))-(piq*(x2-x1)/((r_j2**3)*3.d5))
		ay2 =-(piq*y2/(r_t2**3))-(piq*(y2-y1)/((r_j2**3)*3.d5))
		x_2 = (2.d0*x2) - x2_old + (ax2*(dt**2))
		y_2 = (2.d0*y2) - y2_old + (ay2*(dt**2))
		x2_old = x2
		y2_old = y2
		x2 = x_2
		y2 = y_2
		write(2,*) x_2,y_2,t
C	Nesta condicao, colocamos na troca de sinal no fim da circ.
	end do
	close(1)
	close(2)
	end	