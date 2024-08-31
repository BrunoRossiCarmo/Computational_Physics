C       tarefa-6-11236289.f
C       ##################-main-#################
        implicit real*8 (a-h,o-z)
        parameter (idim = 1.d+8)
        dimension a_ram(idim)
        a_R = 1.d0 !Definindo o raio sendo equivalente a 1.
C       Lendo dados:
        write(*,*) 'Insira numero de pontos: '; read(*,*) i_M
        write(*,*) 'Insira a dimensao: '; read(*,*) i_D
C       Criando pontos:
        do i=1,i_M*i_D
                a_ram(i) = rand()
        end do
        call distance(a_ram,i_D,i_M,a_R)
        call V(i_D, a_R)
        end
C       ##################-main-#################

C       #####################################
        subroutine distance(a_ram,i_D,i_M,a_R)
        implicit real*8 (a-h,o-z)
        parameter (idim = 1.d+8)
        dimension a_ram(idim)
C       Verifica quantos pontos estao dentro da esfera:
        a_dentro = 0
        do i=0,i_M
            a_dist = 0
            do j = 1,i_D
                a_dist = (a_ram(j+3*i)**2) + a_dist    
            end do
            if(a_dist.lt.(a_R**2)) then
                a_dentro = a_dentro + 1
            end if
        end do
        a_prop = a_dentro/i_M !Proporcao de pontos dentro da esfera.
C       Calculando volume da esfera:
        a_volume = ((a_R*2.d0)**(i_D))*a_prop
        write(*,*) 'O volume aproximado = ', a_volume        
        return
        end subroutine distance
C       #####################################

C       #####################################
C       Calcula volume via equacao:
        subroutine V(i_D,a_R)
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
C       Printa o volume calculado:
        a_Vd = ((acos(-1.d0)**(i_D/2.d0))*(a_R**(i_D)))/a_parameter 
        write(*,*) 'Volume calculado =', a_Vd
        end subroutine V
C       #####################################
