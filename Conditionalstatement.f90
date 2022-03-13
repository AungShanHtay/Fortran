!  Conditionalstatement.f90 
!
!  FUNCTIONS:
!  Conditionalstatement - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: Conditionalstatement
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program Conditionalstatement
    implicit none

    Integer::a,b,count
    Real::x
    
    !Enter initial guess
    print*,'Initial guess'
    read*,a
    
    !Generating random number 1 to 10
    call RANDOM_SEED()
    call RANDOM_NUMBER(x)
    b=int(MOD(x,10.0)*10) 
    
    !Looping start
    if(a==b) then
        count=1
        print*,"Guess",count,"times: Correct!"
    else
        count=0
        do while(a/=b)
            count=count+1
            call RANDOM_SEED()
            call RANDOM_NUMBER(x)
            b=int(MOD(x,10.0)*10)
            if(a/=b) then
                print*,"Guess ",count,"times: Wrong guess but it close to ",REAL(MIN(a,b))/REAL(MAX(a,b))*100,"%."
                read*,a
            else if(a==b) then
                print*,"Guess ",count,"times: Correct!"
            end if
        end do
    end if
    end program Conditionalstatement

