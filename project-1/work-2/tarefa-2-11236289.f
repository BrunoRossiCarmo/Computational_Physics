C     tarefa-2-11236289.f
C     ############### main ###############
      implicit real*8 (a-h,o-z)                   ! Identifica variaveis reais e inteiras
      parameter (idim=3)                          ! Define a quantidade de elementos contidos no vetor
      dimension v1(idim), v2(idim), v3(idim), v_res(idim), v21(idim)
C     Lendo vetores:
1     write(*,*) 'Indique o vetor 1 = ' ; read(*,*)(v1(i),i=1,idim)
      write(*,*) 'Indique o vetor 2 = ' ; read(*,*)(v2(i),i=1,idim)
C     Acusação caso seja paralelo:
      call produt_vet(v1 ,v2 ,v_res,v_modul)      ! Invoca a rotina para calcular o produto vetorial e o modulo resultante
      if(v_modul.eq.0d0) then                     ! Analisa se o modulo eh nulo ou nao
            write(*,*) 'O vetor 1 e vetor 2 sao paralelos, repita:'
            goto  1
      end if
C     Acusação caso v3 esteja mesmo plano de base triangular:
2     write(*,*) 'Indique o vetor 3 = '; read(*,*)(v3(i),i=1,idim)
      call produt_esc(v3,v_res,res) !Ao mesmo tempo que verifica coplanaridade, realiza produto misto
      if(res.eq.0d0) then           !A variavel res ja carrega o resultado da operacao produto misto
            write(*,*) 'O vetor 3 esta no plano da base triangular'
            goto 2
      end if
C     Area e Volume:
      write(*,*) ' O volume do paralelepipedo = ', abs(res)
      call produt_vet(v3,v1,v_res,v_modul)
      call produt_vet(v3,v2 - v1,v_res,v_modul2)
      call produt_vet(v3,v2,v_res,v_modul3)
      area = abs(v_modul) + abs(v_modul2) + abs(v_modul3) !Soma as areas laterais
      write(*,*) 'O valor da area lateral = ', area
      end
C     ########################################

C     ############### subrot1 ###############
C     ### Produto vetorial | Módulo do Vetor ###
      subroutine produt_vet(v1 ,v2 ,v_res,v_modul)
      implicit real*8 (a-h,o-z)
      parameter (idim=3)
      dimension v_res(idim), v1(idim), v2(idim)
C     Realiza produto vetorial:
      v_res(1) = v1(2)*v2(3) - v1(3)*v2(2)
      v_res(2) = v1(3)*v2(1) - v1(1)*v2(3)
      v_res(3) = v1(1)*v2(2) - v1(2)*v2(1)
      v_modul = sqrt((v_res(1)**2)+(v_res(2)**2)+(v_res(3)**2)) !Calcula o modulo
      return
      end subroutine produt_vet
C     ########################################

C     ############### subrot2 ###############
C     ### Produto escalar###
      subroutine produt_esc(v1 ,v2 ,res)
      implicit real*8 (a-h,o-z)
      parameter (idim=3)
      dimension v1(idim), v2(idim)
      res = 0
C     Realiza produto escalar:
      do i=1,idim
            res = res + v1(i)*v2(i)
      end do
      return
      end subroutine produt_esc
C     ########################################
