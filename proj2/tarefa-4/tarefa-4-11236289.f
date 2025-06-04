C   tarefa-3-11236289.f
        implicit real (a-h,o-z)
        parameter (M=1000) !Definido 1000 andarilhos.
        dimension cont(6,6),x(M),y(M)
        open(unit=1,file='saida-1-11236289.dat')
        i_passo = 100
C       Variaveis de contagem:
        N = 0
        do i = 1,M
            x(M) = 0
            y(M) = 0
        end do
C       Inicio do loop:
1       continue
        do i = 1,6
            do j = 1,6
                cont(i,j) = 0
            end do
        end do
        ent = 0 !Entropia.
        N = N + 1
        do i = 1, M
            rr = rand()
            if (rr.le.1.e0/4.e0) then
                y(i) = y(i) + 1
            else if ((rr.gt.1.e0/4.e0).and.(rr.le.2.e0/4.e0)) then
                y(i) = y(i) -1
            else if ((rr.gt.2.e0/4.e0).and.(rr.le.3.e0/4.e0)) then
                x(i) = x(i) + 1
            else 
                x(i) = x(i) - 1
            end if
        end do
        do k = 1, M
            do i = 1, 6
                do j = 1, 6
c               Define os intervalos:
                    x_low = (-30 + 10*(i-1))
                    x_up= (-30 + 10*i)
                    y_low = (-30 + 10*(j-1))
                    y_up = (-30 + 10*j)
                        if (x(k).ge.x_low .and. x(k).lt.x_up) then
                            if(y(k).ge.y_low .and. y(k).lt.y_up) then
                                cont(i,j) = cont(i,j) + 1
                            end if
                        end if
                end do
            end do
        end do
        do i = 1,6
            do j = 1,6
                    if(cont(i,j).ne.0d0) then
                        ent = ent - ((cont(i,j)/M))*log((cont(i,j)/M))
                    end if
            end do
        end do
        write(1,*) ent,N
C       Condicao para fim do loop:
        if(N.ne.i_passo) then
            goto 1
        end if
        close(1)
        end