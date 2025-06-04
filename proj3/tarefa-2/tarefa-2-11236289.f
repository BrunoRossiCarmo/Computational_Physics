C	tarefa-2-11236289.f
	implicit real*8 (a-h,o-z)
	parameter (pi=acos(-1.d0))
	open(unit=1,file='saida-2-1-11236289.dat') !Tabela.
	open(unit=2,file='saida-2-2-11236289.dat') !Erros.
	write(1,'(A15,A31,A28,A29)') '|H|', '|Trap|', '|Sims|','|Bool|'
C	Integral real:
	real_int = (1.d0/2.d0)*(-dexp(-(2*pi)) + 1) !Integral definida calculada.
C	Definindo numero de intervalos:
	do i = 1,12
		i_N = 2**(i+1)
		h = (2*pi)/i_N
C		trapezio:
		trp = 0
		err1=0
		do j = 0, i_N
			trp = trp + (h/2.d0)*(fun((j+1)*h)+fun(j*h))
			err1= abs(real_int-trp) 
		end do
C		Simpson:
		smp = 0
		err2=0
		do j = 1, i_N,2
			smp = smp + (h/3.d0)*(fun((j+1)*h)+4*fun(j*h)+fun((j-1)*h))
			err2= abs(real_int-smp) 
		end do
C		Boole:
		bool = 0
		err3=0
		do j = 2, i_N,4
			bool = bool + ((2*h)/45.d0)*(7*fun((j-2)*h))
			bool = bool + ((2*h)/45.d0)*(32*fun((j-1)*h))
			bool = bool + ((2*h)/45.d0)*(12*fun(j*h))
			bool = bool + ((2*h)/45.d0)*(32*fun((j+1)*h))
			bool = bool + ((2*h)/45.d0)*(7*fun((j+2)*h))
			err3= abs(real_int-bool) 
		end do
C		Escrever nos documentos:
		write(1,*) '|',h,'|', trp,'|', smp,'|', bool,'|'
		write(2,*) h,err1,err2,err3
	end do
	write(1,'(A,F15.11)') 'Valor exato =', real_int
	write(*,*) 'Fim do programa, integrais calculadas!'
	close(1)
	close(2)
	end
	
	function fun(x)
	implicit real*8 (a-h,o-z)
	fun = dexp(-x)*dcos(x)
	return
	end function