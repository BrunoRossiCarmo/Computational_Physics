C	tarefa-5b-11236289.f
C	########################
	implicit real*8 (a-h,o-z)
	parameter (idim=1000)
	dimension i_val(idim,idim), a_val(idim,idim)
	open(unit=5,file='saida-5a-11236289.in')
	read(5,*) i_N
	k = 1
	do i = 1, i_N
		k = k*i
	end do
	do i= 1,k
		read(5,*)(i_val(i,j),j=1,i_N+1)
	end do
C	Criar a matriz:
	do i = 1, i_N !Linha
		do j = 1, i_N !Coluna
			a_val(i,j) = 0
			if(i.eq.j) then
				a_val(i,j)=i
			end if
		end do
	end do
C	Imprime a matriz:
	do i = 1, i_N
		write(*,*)(a_val(i,j),j = 1,i_N)
	end do
	a_det = det(a_val,i_val,i_N,k)
	close(5)
	end
C	########################

C	########################
	function det(a_val,i_val,i_N,k)
	implicit real*8 (a-h,o-z)
	parameter (idim=1000)
	dimension a_val(idim,idim), i_val(idim,idim)
	a_sum = 0
	do i = 1, k
		a = 1
		do j = 1,i_N
		      a = a*(a_val(j,i_val(i,j)))
		end do
		a_sum = a_sum + i_val(i,i_N+1)*a
	end do
	write (*,*) 'O resultado = ', a_sum
	end
C	########################
