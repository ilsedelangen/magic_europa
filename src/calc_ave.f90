module calc_ave 

    implicit none

    private

    type, public :: ave         
        real, allocatable :: f_ave(:,:,:)    ! 3dim array to store average field in
        real :: time_last     ! Last time at which the routine is called ? 
        real :: time_ave      ! Time that has passed since last averaging??  
        integer :: nTimes     ! Counter for how many times the subroutine (?) is called
        integer :: nr, nt, np
    contains
        procedure :: init
        procedure :: add
        procedure :: add_r
    end type ave

contains 
  
    subroutine init(this,nr_I,nt_I,np_I)
        class(ave) :: this
        integer :: nr_I, nt_I, np_I               ! Define grid that we want, that we expect
        allocate(this%f_ave(nr_I,nt_I,np_I))
        this%nr = nr_I
        this%nt = nt_I
        this%np = np_I
        this%nTimes=0         ! Set counter to zero
        
    end subroutine init

    subroutine add(this,f_in,time_in)
        class(ave) :: this 
        real, intent(in) :: f_in(:,:,:)    ! field to be added
        real, intent(in) :: time_in      ! time corresponding to field to be added  
        real :: dt                       ! time since last call
        integer, allocatable :: nshape(:)
        integer :: nsize

        ! Check if input field has correct dimension
        nshape = shape(f_in)
        nsize = size(nshape)
        if ( nsize /= 3 ) then
           stop "Wrong input size"
        else
           if (nshape(1) /= this%nr .or. nshape(2) /= this%nt .or. nshape(3) /= this%np ) then
               stop "Wrong input dimension" 
           end if
        end if        

        this%nTimes=this%nTimes+1             
        if ( this%nTimes==1 ) then                 
            this%f_ave = f_in                 ! First call: average field == input field 
        else
            dt = time_in-this%time_last       ! Calculate dt for all other calls    

            if ( this%nTimes==2 ) then
                this%f_ave=dt*(this%f_ave+f_in)    ! Second call: we sum and divide by 2. Don't get what we are doing with dt??? 
                this%time_ave=2*dt
            else
                this%f_ave = this%f_ave*this%time_ave + dt*f_in    ! We do the averaged field times the passed time over averaging? and the input field times the new dt
                this%time_ave=this%time_ave+dt                     ! Calculate new time over averaging 
            end if

            this%f_ave = this%f_ave/this%time_ave         ! To get average we divide by total time
        end if

        this%time_last=time_in    ! okay i dont get it anymore but it works


    end subroutine add

    subroutine add_r(this,f_in,time_in,nR)
        class(ave) :: this 
        real, intent(in) :: f_in(:,:)    ! field to be added
        real, intent(in) :: time_in      ! time corresponding to field to be added 
        integer, intent(in) :: nR        ! Radial grid level 
        real :: dt                       ! time since last call
        integer, allocatable :: nshape(:)
        integer :: nsize

        ! Check if input field has correct dimension
        nshape = shape(f_in)
        nsize = size(nshape)
        if ( nsize /= 2 ) then
           stop "Wrong input size"
        else
           if ( nshape(1) /= this%nt .or. nshape(2) /= this%np ) then
               stop "Wrong input dimension" 
           end if
        end if        

        this%nTimes=this%nTimes+1   
           
        if ( this%nTimes==1 ) then                 
            this%f_ave(nR,:,:) = f_in                 ! First call: average field == input field 
        else
            dt = time_in-this%time_last       ! Calculate dt for all other calls    

            if ( this%nTimes==2 ) then
                this%f_ave(nR,:,:)=dt*(this%f_ave(nR,:,:)+f_in)    ! Second call: we sum and divide by 2. Don't get what we are doing with dt??? 
                this%time_ave=2*dt
            else
                this%f_ave(nR,:,:) = this%f_ave(nR,:,:)*this%time_ave + dt*f_in    ! We do the averaged field times the passed time over averaging? and the input field times the new dt
                this%time_ave=this%time_ave+dt                     ! Calculate new time over averaging 
            end if

            this%f_ave(nR,:,:) = this%f_ave(nR,:,:)/this%time_ave         ! To get average we divide by total time
        end if

        this%time_last=time_in    ! okay i dont get it anymore but it works


    end subroutine add_r


end module calc_ave  










