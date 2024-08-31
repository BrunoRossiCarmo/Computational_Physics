C       tarefa-5c-11236289.f
C       #######################
        implicit real*8 (a-h,o-z)
        dimension a_val(10,10), a_vec(16), a_res(16) 
        dimension i_val(1000,7),a_cpy(10,10)
C       Escolhe qual modelo fazer:
        write(*,*) 'Qual N quer escolher? (4,5,6)'
        read(*,*) N
        if (N.eq.4) then
                open(unit=40,file='saida-5a-11236289.in')
                open(unit=1,file='entrada-5c-1-11236289.in')
                i_N = 4 !Alterar aqui N
                i_fator = 24 !Alterar aqui N!
        else if (N.eq.5) then
                open(unit=40,file='entrada-5c-4-11236289.in')
                open(unit=1,file='entrada-5c-2-11236289.in')
                i_N = 5 !Alterar aqui N
                i_fator = 120 !Alterar aqui N!
        else if (N.eq.6) then
                open(unit=40,file='entrada-5c-5-11236289.in')
                open(unit=1,file='entrada-5c-3-11236289.in')
                i_N = 6 !Alterar aqui N
                i_fator = 720 !Alterar aqui N!
        else 
                write(*,*) 'Erro!'
                stop
        end if
C	Imprime a matriz e vetor:
        call matr_vec(i_N,a_val,a_vec)
        write(*,*) 'Matriz A:'
	do i = 1, i_N 
		write(*,'(100F8.2)')(a_val(i,j),j = 1,i_N )
	end do
        write(*,*) 'Vetor y:'
        write(*,'(100F8.2)')(a_vec(i), i = 1,i_N )
C       Le permutacao:
        read(40,*) i_N
        do i = 1, i_fator
                read(40,*)(i_val(i,j),j=1,i_N+1)
        end do
C       Resolver equacoes lineares:
        a_det = det(a_val,i_val,i_N,i_fator)
        do i = 1, i_N
                a_dx = d_m(a_val,i_val,a_vec,i,i_N,i_fator)
                a_res(i) = a_dx/a_det
        end do
C       Imprime o resultado:
        write(*,*) 'Vetor x:'
        write(*,'(1000F8.2)') (a_res(i), i = 1, i_N)
        write(*,*)'-------------------------------------'
C       Fechar arquivos:
        close(40)
        end
C       #######################

C       #######################
        subroutine matr_vec(i_N,a_val,a_vec)
        implicit real*8 (a-h,o-z)
        dimension a_val(10,10), a_vec(16)
        do i = 1, i_N 
                read(1,*)(a_val(i,j),j=1,i_N)
        end do
        do i = 1,i_N
                read(1,*) a_vec(i)
        end do
        close(1)
        end subroutine matr_vec
C       #######################

C	########################
	function det(a_val,i_val,i_N,k)
	implicit real*8 (a-h,o-z)
	dimension a_val(10,10)
        dimension i_val(1000,7)
	det = 0
	do i = 1, k
		a = 1
		do j = 1,i_N
		      a = a*(a_val(j,i_val(i,j)))
		end do
		det = det + i_val(i,i_N+1)*a
	end do
	end
C	########################

C	########################
	function d_m(a_val,i_val,a_vec,i_cont,i_N,k)
        implicit real*8 (a-h,o-z)
        dimension i_val(1000,7), a_cpy(10,10)
        dimension a_val(10,10), a_vec(16)
C       Utiliza a copia:
        a_cpy = a_val
        !Substitui
        do i = 1, i_N
                a_cpy(i,i_cont) = a_vec(i)
        end do
        !Calcula determinante:
        d_m= 0
	do i = 1, k
		a = 1
		do j = 1,i_N
		      a = a*(a_cpy(j,i_val(i,j)))
		end do
		d_m = d_m + i_val(i,i_N+1)*a
	end do
	end function d_m
C	########################
