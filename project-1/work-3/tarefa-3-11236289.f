C     tarefa-3-11236289.f
C     ############### main ###############
      implicit real*8 (a-h,o-z)
      parameter (idim = 10000)
      dimension a_values(idim)
C     Abre arquivo:
      open(unit=3,file='entrada-3-11236289.in')
C     Contabiliza elementos:
      i_cont = 0
      do i=1,idim
            read(3,*,end=1) a_values(i)
            i_cont = i_cont + 1
      end do
1     write(*,'(A,I0)') 'O documento possui N = ', i_cont
C     Recebe valor para ordenacao:
2     write(*,*) 'Insira quantidade para ordenar: '; read(*,*) i_M
      if(i_M.gt.i_cont) then
            write(*,'(A,I0)') 'Insira um valor menor do que N = ', i_cont
            goto 2
      end if
      call ordem(i_cont,i_M,a_values)
C     Realiza ordenacao e escreve documento:
      close(3)
      end
C     ########################################
C
C     ############### subrot1 ###############
      subroutine ordem(i_cont,i_M,a_values)
      implicit real*8 (a-h,o-z)
      dimension a_values(i_cont), a_ordenado(i_M)
      j = 1
      k = 1
C     Inicia a ordenar os valores:
3     a_ordenado(j) = a_values(1)
      do i = 1,i_cont-1
            if(a_values(i+1).lt.a_ordenado(j)) then
                  a_ordenado(j) = a_values(i+1)
                  k = i + 1
            end if
      end do
      a_values(k) = 1.0E+8
      do while (j.ne.i_M)
            j = j + 1
            goto 3
      end do
C     Escreve a saida ordenada:
      open(unit = 2, file = 'saida-3-11236289.in')
      write(2,*) 'Ordenando em M =', i_M
      do i = 1,i_M
            write(2,*) a_ordenado(i)
      end do
      close(2)
      return 
      end subroutine ordem
C     ########################################
