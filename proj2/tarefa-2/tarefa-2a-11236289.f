C   tarefa-2a-11236289.f
        implicit real (a-h,o-z)
C       Arquivo com os dados de saída:
        open(unit=2,file='saida-2a-11236289.dat')
        write(*,*) 'Quantos andarilhos? '
        read(*,*) M !Lê a quantidade de andarilhos.
        N = 0 !Passos.
        a_med = 0 !Media.
        a_sd = 0 !Media quadratica para variancia.
1       continue
        cont = 0
        N = N + 1
        do i = 1, 1000
            rr = rand()
            if (rr.le.0.5) then
                cont = cont - 1
            else if (rr.gt.0.5) then
                cont = cont + 1
            end if
        end do
        a_med = a_med + cont
        a_sd = a_sd + (cont**(2))
        write(2,*) N,cont
        if(N.ne.M) then
            goto 1
        end if
        close(2)
        write(*,*) '<x> = ', a_med/M
        write(*,*) '<x^2> =', a_sd/M
        end