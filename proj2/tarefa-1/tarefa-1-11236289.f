C   tarefa-1-11236289.f
        implicit real (a-h,o-z)
        parameter (idim=10000)
        sum = 0
        sum_2 = 0
        sum_3 = 0
        sum_4 = 0
        do i = 1, idim
            sum = sum + rand()
            sum_2 = sum_2 + (rand()**(2))
            sum_3 = sum_3 + (rand()**(3))
            sum_4 = sum_4 + (rand()**(4))
        end do
        write(*,*) '<x> = ', sum/idim
        write(*,*) '<x**2> = ', sum_2/idim
        write(*,*) '<x**3> = ', sum_3/idim
        write(*,*) '<x**4> = ', sum_4/idim
        end