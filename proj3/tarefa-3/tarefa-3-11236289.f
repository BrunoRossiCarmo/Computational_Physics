C	tarefa-3-11236289.f
	implicit real*8 (a-h,o-z)
	open(unit=1,file='saida-3-11236289.f')
C	Encontrar onde variacao de sinal:
	do i = -100,100,1
		val = fun(real(i,8)*0.1d0)*fun(real(i-1,8)*0.1d0)
		if(val.lt.0) then
			raiz = val
		end if
	end do
C	Bissec:

C	Raphson:
	x = 0
	err = 1.d0
	do while (err.gt.10.d0**(-6))
		err = abs(fun(x)/fun_der(x))
		x = x - fun(x)/fun_der(x)
	end do
	write(*,*) x
	
C	Outro metodo:
	end
	
	function fun(x)
	implicit real*8 (a-h,o-z)
	fun = 27*(x**(3)) - 522*(x**(2)) + 3003*x - 4508
	return
	end function

	function fun_der(x)
	implicit real*8 (a-h,o-z)
	fun_der = 27*(x**(2))*3 - 522*(x**(1))*2 + 3003
	return
	end function
		
