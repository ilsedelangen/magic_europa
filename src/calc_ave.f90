module calc_ave
    ! 
    ! This module calculates the time-averaged field quantities such as the Lorentz force.
    !
    use precision_mod

    implicit none

    private

    type, public :: ave         
        real(cp), allocatable :: f_ave(:,:,:)    ! 3dim array to store average field in
        real(cp) :: time_last     ! Last time at which the routine is called 
        real(cp) :: time_ave      ! Time that has passed since last averaging 
        integer :: nTimes     ! Counter for how many times the subroutine is called
        integer :: nr, nt, np
    contains
        procedure :: init
        procedure :: add_r
    end type ave

contains 
  
    subroutine init(this,nr_I,nt_I,np_I)
        ! 
        ! Initialize the array containing the averaged field
        ! 
        class(ave) :: this
        integer :: nr_I, nt_I, np_I               ! Define grid that is expected
        allocate(this%f_ave(nr_I,nt_I,np_I))
        this%nr = nr_I
        this%nt = nt_I
        this%np = np_I
        this%nTimes=0         ! Set counter to zero
        
    end subroutine init

    subroutine add_r(this,f_in,time_in,nR)
        !
        ! For a given radial level, add the updated field and compute new average
        !
        class(ave) :: this 
        real(cp), intent(in) :: f_in(:,:)    ! field to be added
        real(cp), intent(in) :: time_in      ! time corresponding to field to be added 
        integer, intent(in) :: nR        ! Radial grid level 

        real(cp) :: dt                       ! time since last call
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
                this%f_ave(nR,:,:)=dt*(this%f_ave(nR,:,:)+f_in)    ! Second call: we sum and divide by 2. 
                this%time_ave=2*dt
            else
                this%f_ave(nR,:,:) = this%f_ave(nR,:,:)*this%time_ave + dt*f_in    ! We do the averaged field times the passed time over averaging? and the input field times the new dt
                this%time_ave=this%time_ave+dt                     ! Calculate new time over averaging 
            end if

            this%f_ave(nR,:,:) = this%f_ave(nR,:,:)/this%time_ave         ! To get average we divide by total time
        end if

        this%time_last=time_in   


    end subroutine add_r


end module calc_ave  










