C       tarefa-4b-11236289.f
C     ############### main ###############
        implicit real*8 (a-h,o-z)
        write(*,*) 'Insira o valor de x =' ; read(*,*) a_x
C       Permite regularizacao periodica:
        if(a_x.gt.(2*acos(-1.d0))) then
            a_x = modulo(a_x, 2.0d0 * acos(-1.0d0))
        end if
C       Calcula o cosseno e compara:
        call cos_bruno(a_x)
        end
C     ########################################        

C     ########################################  
        subroutine cos_bruno(a_x)
        implicit real*8 (a-h,o-z)
        a_termo = 1
        a_fator = 2
        i = 1
        a_cos = 1
C       Calcula a serie na precisao indicada:
        do while(abs(a_termo).gt.1.0d-12)
            a_termo = (((-1.d0)**(i))*(a_x**(2.d0*i)))/a_fator
            a_cos = a_cos + a_termo
            a_fator = a_fator*(2*i + 1)*(2*i + 2)
            i = i + 1
        end do
C       Printa os valores:
        write(*,*) 'Encontrou-se no codigo cos(x) =', a_cos
        write(*,*) 'O cosseno da linguagem cosf77(x) =', dcos(a_x)
        end subroutine cos_bruno
C     ########################################  

C     ########################################
      function modulo(a, b)
      real a, b, modulo
      integer n
      n = a / b           
      modulo = a - n * b   
      if (modulo .lt. 0.0) then
         modulo = modulo + b
      endif
      end function
C     ########################################
