C   tarefa-3-11236289.f
        implicit real (a-h,o-z)
        parameter (M=1000) !Definido 1000 andarilhos.
        open(unit=1,file='saida-6-11236289.dat')
        write(*,*) 'Quantos passos ' 
        read(*,*) i_passo 
C       Variaveis de contagem:
        N = 0
        a_medx = 0
        a_medy = 0
        a_sdx = 0
        a_sdy = 0
C       Inicio do loop:
1       continue
        a_y = 0 !Norte|Sul
        a_x = 0 !Leste|Oeste
        N = N + 1
        do i = 1, i_passo 
            rr = rand()
            if (rr.le.1.e0/4.e0) then
                a_y = a_y + 1
            else if ((rr.gt.1.e0/4.e0).and.(rr.le.2.e0/4.e0)) then
                a_y = a_y -1
            else if ((rr.gt.2.e0/4.e0).and.(rr.le.3.e0/4.e0)) then
                a_x = a_x + 1
            else 
                a_x = a_x - 1
            end if
        end do
        a_sdx = a_sdx + a_x**2
        a_sdy = a_sdy + a_y**2
        a_medx = a_medx + a_x
        a_medy = a_medy + a_y
        write(1,*) a_x,a_y
C       Condicao para fim do loop:
        if(N.ne.M) then
            goto 1
        end if
        a_sd = sqrt((a_sdx**2) + (a_sdy**2))/M
        a_mean = sqrt((a_medx**2) + (a_medy**2))/M
        write(*,*) '<r> =', a_mean
        write(*,*) 'variancia =', (a_sd) - (a_mean**2)
        end