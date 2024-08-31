C     tarefa-5a-11236289.f
      implicit real*8 (a-h,o-z)
      parameter (idim = 100000)
      dimension i_alt(idim)
C     Le arquivo:
      open(unit=5,file='entrada-5a-11236289.in')
      j = 0
      do i = 1, idim
            read(5,*,end=1) i_alt(i)
            j = j + 1
      end do
1     continue
C     Encontra o maior valor => N.
      j_cont = 1
      do i = 1, j
            if(i_alt(i).gt.j_cont) then     
                  j_cont = i_alt(i)
            end if
      end do
C     Quantidade de permutacao (N+1)! :
      k = j*(j_cont+1)
      write(*,"(A,I0)") 'O arquivo possui N = ', j_cont
      write(*,"(A,I0)") 'O proximo tera N = ', j_cont + 1
      write(*,"(A,I0)") 'A quantidade de permutacao sera = ', k
      call create_doc(k,j_cont)
      close(5)
      end
      
      subroutine create_doc(k,j_cont)
      implicit real*8 (a-h,o-z)
      dimension i_values(k,j_cont + 2), i_par(k), armazenador(j_cont+1)
      rewind(5)
      a_count = j_cont !Para fazer a comparacao.
      open(unit = 10, file = 'saida-5a-11236289.in')
      write(10,*) j_cont + 1
      do i = 1, k
            i_values(i,1) = j_cont+1
      end do
      do i = 1, k
            read(5,*,end=2) (i_values(i,j+1), j = 1,j_cont+1)
C           N+1 ser par ou nao importa para ordenacao.
            if(mod(a_count+1.d0,2.d0).eq.0) then
                  i_values(i,j_cont+2) = i_values(i,j_cont+2)*(-1)
            end if
            write(10,'(I0,100I5)') (i_values(i,j), j = 1,j_cont+2)
      end do
2     continue
      do i = 1,k/(j_cont+1)
            do j = 1,j_cont
                  armazenador(j) = i_values(i,j)
                  i_values(i,j) = i_values(i,j+1)
                  i_values(i,j+1) = armazenador(j)
                  i_values(i,j_cont+2) = -1*(i_values(i,j_cont+2))
                  write(10,'(I0,100I5)') (i_values(i,m),m=1,j_cont+2)
            end do
      end do
      close(10)
      return
      end subroutine create_doc
