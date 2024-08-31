C       tarefa-7-11236289.f
C       ##################-main-#################
        implicit real*8 (a-h,o-z)
        parameter (idim=1.d+2)
        dimension a_volumes(idim)
C       Lendo dados:
        write(*,*) 'Insira a dimensao: '; read(*,*) i_D
        a_R = 0.8d0 ! Valor inicial do intervalo do raio
C       Calculando o volume:
        open(unit=7,file='saida-7-11236289.dat')
1       continue
        a_R = a_R + 0.1d0
        do i = 1,i_D  
            write(7,*) i,a_Vd(i,a_R), a_R
        end do
        if (a_R.ne.1.1d0) then
                goto 1
        end if
        close(7)
        end
C       ##################-main-#################

C       #####################################
C       Calcula volume via equacao:
        function a_Vd(i_D,a_R)
        implicit real*8 (a-h,o-z)
        a_cont = i_D/2.d0
        a_parameter = 1
C       Realiza a funcao gamma:
        do while (a_cont.gt.1.d0 .or. a_cont.gt.0.5d0)
                a_parameter = a_parameter*(a_cont)
                a_cont = a_cont - 1
        end do
        if (mod(i_D,2).ne.0) then
                a_parameter = a_parameter*sqrt(acos(-1.d0))
                a_parameter = a_parameter*(a_cont)
        end if
C       Calcula o volume:
        a_Vd = ((acos(-1.d0)**(i_D/2.d0))*(a_R**(i_D)))/a_parameter 
        return
        end function a_Vd
C       #####################################
