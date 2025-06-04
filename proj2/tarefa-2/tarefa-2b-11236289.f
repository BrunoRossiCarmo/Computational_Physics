C   tarefa-2b-11236289.f
        implicit real (a-h,o-z)
        open(unit=2,file='saida-2b-1-11236289.dat')
        write(*,*) 'Quantos andarilhos? ' 
        read(*,*) M 
C       Variaveis de contagem:
        N = 0
        a_med = 0
        a_sd = 0
        prob = 1.e0/3.e0
C       Inicio do loop:
1       continue
        cont = 0
        N = N + 1
        do i = 1, 1000
            rr = rand()
            if (rr.le.prob) then
                cont = cont + 1
            else if (rr.gt.prob) then
                cont = cont - 1
            end if
        end do
        write(2,*) N, cont
        a_med = a_med + cont
        a_sd = a_sd + (cont**(2))
C       Condicao para fim do loop:
        if(N.ne.M) then
            goto 1
        end if
        write(*,*) '<x> = ', a_med/M
        write(*,*) '<x^2> =', a_sd/M
        close(2)
        end