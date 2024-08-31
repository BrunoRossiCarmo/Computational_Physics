C     tarefa-1-11236289.f
      implicit real*8 (a-h,o-z)                          !identifica variaveis reais e inteiras.

C     ############### main ###############
C     Leitura dos raios:
1     write(*,*) 'Insira raio menor e raio maior (em qualquer ordem): '
      read(*,*) a_raio1, a_raio2
      if(a_raio1.eq.a_raio2) then                        !Garante que os raios sao diferentes.
            write(*,*) 'Insira valores diferentes de raio'
            goto 1
      end if
      call subrot1(a_raio1, a_raio2, area, volume)       !Faz o calculo da area e volume.
      write(*,*) 'A area equivale a = ', area
      write(*,*) 'O volume equivale a = ', volume
      end
C     ########################################

C     ############### subrot1 ###############
C     Area e Volume:  
      subroutine subrot1(a_raio1,a_raio2,area,volume)
      implicit real*8 (a-h,o-z)   
      area = 4.d0*((acos(-1.d0))**(2.d0))*a_raio1*a_raio2
C     Independente da ordem inserida dos raios, será corretamente calculado o volume:
      if(a_raio1.gt.a_raio2) then
            volume = (area*a_raio2)/2
      else      
            volume = (area*a_raio1)/2
      end if
      return
      end subroutine subrot1
C     ########################################
