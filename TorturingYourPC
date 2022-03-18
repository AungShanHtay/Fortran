!  Estimator.f90 
!
!  FUNCTIONS:
!  Estimator - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: Estimator
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program Estimator

    implicit none
    
    real::gen_rand,est_rand
    integer:: gen,est,range,total_count,wrong_count
    print*,"Enter range"
    read*,range
    call RANDOM_SEED()
    call RANDOM_NUMBER(gen_rand)
    gen=int(MOD(gen_rand*gen_rand,real(range))*range)
    total_count=0
    wrong_count=0
    
    do
        total_count=total_count+1
        call RANDOM_SEED()
        call RANDOM_NUMBER(est_rand)
        est=int(MOD(est_rand,real(range))*range)
        if(est/=gen) then
            print*,"Random number:",gen,"|","Estimator:",est,"|","Deviate %:",(real(gen)/real(est))*100,"%"
            wrong_count=wrong_count+1
        else
            print*,"Random number:",gen,"|","Estimator:",est,"|","Deviate %:",(real(gen)/real(est))*100,"%"
            print*,"MATCH!"
            print*,"Total=",total_count,",Wrong=",wrong_count
            exit
        end if
        end do
    end program Estimator
