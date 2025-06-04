C	tarefa-1-11236289.f
	implicit real*8 (a-h,o-z)
	parameter (x=1.d0/2.d0)
	open(unit=1,file='saida-1-11236289.dat')
	open(unit=2,file='saida-2-11236289.dat')
C	Definicoes:
	ex = exp(x**2)
	cot = tan(2*x)**(-1)
	csc2 = (sin(2*x)**(-2))
C	Valores reais - derivadas:
	der1 = 2*ex*x*(cot) - 2*ex*csc2 !Derivada primeira.
	der2 = 2*ex*(cot*((2*(x**2))+(4*csc2)+1))
	der2 = der2 - 2*ex*4*x*csc2 !Derivada segunda.
C	Definiremos as funcoes:
	do i = 1,12
		h = (5.d0**(-i))
		f2t = (fx(x+h) - fx(x))/h
		f2s = (fx(x) - fx(x-h))/h
		f3s = (fx(x+h) - fx(x-h))/(2*h)
		f5s = (fx(x-2*h)-8*fx(x-h)+8*fx(x+h))
		f5s = f5s -fx(x+2*h)
		f5s = f5s/(12*h)
		f23s = (fx(x+h)-(2*fx(x))+(fx(x-h)))/(h**2)
		f25s = -fx(x-2*h) + 16*fx(x-h) - 30*fx(x)
		f25s = f25s + 16*fx(x+h) - fx(x+2*h)
		f25s = f25s/(12*(h**2))
		write(1,*) h,f2t,f2s, f3s,f5s,f23s,f25s
		write(2,*) h,abs(f2t-der1),abs(f3s-der1),abs(f5s-der1) !f5s eh o melhor para iteracao 5.
	end do
	write(1,'(A,F15.11)') 'Valor exato =', der1
	write(1,'(A,F15.11)') 'Valor exato derivada segunda =', der2
C	Valor exato:
C	Fim do programa.
	write(*,*) 'Fim do programa, saidas geradas!'
	close(1)
	close(2)
	end

	function fx(x)
	implicit real*8 (a-h,o-z)
	fx = (dexp((x)**2))/dtan(2*(x))
	return
	end function
