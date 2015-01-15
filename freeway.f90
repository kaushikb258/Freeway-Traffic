                program interstate

       implicit none


          integer, parameter :: L = 1000
          integer, parameter :: N = 200 
          integer, parameter :: vmax = 5   
          integer, parameter :: ng = 10 ! ghost cells
          real(kind=8), parameter :: prand = 0.15d0
          real(kind=8), parameter :: tmax = 5.0d2


          integer, dimension(1:L) :: iamhere
          integer, dimension(1-ng:L+ng) :: location
          integer, dimension(1:N) :: car_cell
          integer, dimension(1:N) :: speed, dist_ahead

          integer :: i, j
          real(kind=8) :: t, r
          

          print*, 'mean car density ', dble(N)/dble(L)



!--------------------------
! initialize car locations and velocities
  
           location = 0
           car_cell = 0
           speed = 0 
           dist_ahead = 0


           do i = 1, N
102          continue
             call get_rand_int(1,L,j)
             if(location(j).ne.0) then
               goto 102
             endif     
             location(j) = i       
             car_cell(i) = j  

             call get_rand_int(0,vmax,speed(i))
           enddo

!-------------------------
! start the iteration

             t = 0.0d0

103          continue 

              t = t + 1.0d0


! assign values in the ghost cells
         location(1-ng:0) = location(L-ng+1:L)         
         location(L+1:L+ng) = location(1:ng)


! for each car find the distance ahead to the next car

          dist_ahead = 0 
          do i = 1, N

           j = car_cell(i)+1           
104        continue
             
            if(location(j).eq.0) then
             dist_ahead(i) = dist_ahead(i) + 1
            else
             goto 105
            endif 

            if(j.lt.L+ng) then
              j = j + 1
              goto 104
            endif

105       continue 

          enddo    
 

!---------------------------
! acceleration

       do i = 1, N
         if(speed(i).lt.vmax.and.dist_ahead(i).gt.speed(i)+1) then
           speed(i) = speed(i) + 1 
         endif
       enddo 

!---------------------------
! deceleration
 
       do i = 1, N
         if(dist_ahead(i).le.speed(i)) then
           speed(i) = dist_ahead(i)-1
         endif
       enddo

!----------------------------
! randomization

       do i = 1, N
         call random_number(r)  
         if(r.le.prand.and.speed(i).gt.0) then
          speed(i) = speed(i) - 1
         endif 
       enddo 


!----------------------------
! advance each vehicle v sites

        do i = 1, N
         car_cell(i) = car_cell(i) + speed(i)
        enddo


!---------------------------
! apply boundary condition: periodic

       do i = 1, N
        if(car_cell(i).gt.L) then
          car_cell(i) = car_cell(i) - L
        endif
        if(car_cell(i).lt.1) then
         print*, 'bug ', car_cell(i), i
         stop
        endif  
       enddo

!---------------------------
! recompute location

        location = 0
        do i = 1, N
         location(car_cell(i)) = i 
        enddo  

 
!-----------------------------

      iamhere = 0
      do i = 1, L
       if(location(i).ne.0) then
         iamhere(i) = 1
       endif 
      enddo  


      open(3,file='hwy',form='formatted',access='append')
        do i = 1, L
         if(location(i).ne.0) then 
          write(3,*) i, t
         endif
        enddo
      close(3)

!---------------------------

          if(t.le.tmax) then
            goto 103
          endif

!--------------------------

! output data

 
!       open(4,file='loc',form='formatted')
!        do i = 1, L
!          write(4,*) i, iamhere(i)
!        enddo
!       close(4)


       j = 0
       do i = 1, L
        if(location(i).ne.0) then
         j = j + 1
        endif
       enddo 

       print*, 'number of occupied sites ', j
       print*, 'total number of cars ', N



!       open(5,file='car',form='formatted')
!         do i = 1, N
!          write(5,*) i, car_cell(i), speed(i)
!         enddo
!       close(5)



                end program

!--------------------------------------

     subroutine get_rand_int(low,high,j)

    implicit none

      integer :: low, high, j
      real(kind=8) :: u

      call random_number(u)
      j = low + floor((high+1-low)*u)

     return
     end subroutine

!--------------------------------------

