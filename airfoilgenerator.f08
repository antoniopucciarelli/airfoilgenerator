!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! the aim of the program is to calculate the geometry of a NACA**** airfoil !
!   -> the airfoil must be NACA 4-digits                                    !
!       -> there are different options for saving and displaying data       !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program airfoilgenerator

    ! module declaration
    use AIRFOIL_object
    use PANEL_object
    use MEANline_object
    use discretisation_module

    implicit none

    ! variable declaration
    type(NACA_airfoil)                      :: airfoil       ! airfoil object
    type(panel),allocatable,dimension(:)    :: PANELarray    ! panel object array
    type(MEANline),allocatable,dimension(:) :: MEANLINEarray ! mean line point object array
    real(kind=8),allocatable,dimension(:)   :: coordy        ! x-coords array -> describes airfoil's geometry
    real(kind=8),allocatable,dimension(:)   :: coordx        ! y-coords array -> describes airfoil's geometry
    integer(kind=4)                         :: counter = 1   ! # of airfoil studied counter
    integer(kind=4)                         :: x = 1         ! checking variable in while loop
    integer(kind=4)                         :: dim           ! auxiliary variable -> # of discretisation points for the airfoil
    integer(kind=4)                         :: k             ! auxiliary variable
    integer(kind=4)                         :: j             ! auxiliary variable
    integer(kind=4)                         :: i             ! auxiliary variable
    real(kind=8)                            :: airfoil_data1 ! easy-access variable -> 1st number of airfoil%data
    real(kind=8)                            :: airfoil_data2 ! easy-access variable -> 2nd number of arifoil%data


    do while(x==1)

        print*, 'AIRFOIL # ', counter

        call airfoil%set_AIRFOILname() ! setting airfoil name

        call airfoil%set_npoints()     ! setting airfoil # of discretisation points

        dim = airfoil%get_npoints()

        ! data allocation
        allocate(coordx(1:2*dim-1)) 
        allocate(coordy(1:2*dim-1))
        allocate(PANELarray(1:2*dim-2)) 
        allocate(MEANLINEarray(1:dim))

        ! calculate airfoil mean line point discretisation along x-axis
        do k=1,dim
            call MEANLINEarray(k)%set_id(k)
            call MEANLINEarray(k)%set_coordx(dim,k)
        end do

        ! initialising data for the loop
        k             = 1 
        airfoil_data1 = real(airfoil%data(1),8)/100.0
        airfoil_data2 = real(airfoil%data(2),8)/10.0

        ! calculate airfoil mean line point discretisation along y-axis
        !     y coord
        !     gradient [dy] => theta
        do while(MEANLINEarray(k)%coords(1)<airfoil_data2)
            call MEANLINEarray(k)%set_coordy_leading(airfoil_data1,airfoil_data2)
            call MEANLINEarray(k)%set_gradient_leading(airfoil_data1,airfoil_data2)
            k = k + 1
        end do
        do i=k,dim ! loop starts counting from k
            call MEANLINEarray(i)%set_coordy_trailing(airfoil_data1,airfoil_data2)
            call MEANLINEarray(i)%set_gradient_trailing(airfoil_data1,airfoil_data2)
        end do

        MEANLINEarray(dim)%coords = (/1.0, 0.0/) ! mean-line end point coords

        ! thickness
        do k=1,dim
            call MEANLINEarray(k)%set_thickness(airfoil%data(3)) 
        end do

        ! coordx|coordy
        do k=1,dim
            if(k/=1)then
                call MEANLINEarray(k)%compute_UPcoords(coordx(dim+k-1),coordy(dim+k-1))    
            end if
            call MEANLINEarray(k)%compute_DOWNcoords(coordx(dim-k+1),coordy(dim-k+1))
        end do

        ! closure coords
        coordx(1)                    = 1
        coordy(1)                    = 0
        coordx(2*dim-1)              = 1
        coordy(2*dim-1)              = 0
        MEANLINEarray(dim)%coords(1) = 1
        MEANLINEarray(dim)%coords(2) = 0

        call airfoil%set_scaling() ! setting scale factor for airfoil dimension

        if(airfoil%get_scaling() /= 1.0) then
            ! airfoil coordinates and airfoil mean coordinates scaling procedures
            do j=1,2*dim-1
                call SCALINGfunction(coordx(j),coordy(j),airfoil%get_scaling())
            end do
            do j=1,dim
                call SCALINGfunction(MEANLINEarray(j)%coords(1),MEANLINEarray(j)%coords(2),airfoil%get_scaling())
            end do
        end if

        call airfoil%set_AOA() ! setting wing rotation -> airfoil AOA

        ! rotation procedure for panel and mean-line objects
        if(airfoil%get_AOA() /= 0) then
            
            ! geometry points rotation procedure
            do j=1,2*dim-1
                call rot(coordx(j),coordy(j),airfoil%get_AOA())
            end do

            ! panel array allocation data
            do k=1,2*dim-2
                call PANELarray(k)%set_id(k)
                call PANELarray(k)%set_coords(coordx(k),coordy(k),coordx(k+1),coordy(k+1))
            end do

            ! compute segments length -> length doesn't vary with rotation and translation 
            do k=1,2*dim-2
                call PANELarray(k)%compute_length()
            end do

            ! compute segments tangent and normal versors
            !  -> the easiest way is to calculate before rotation
            do j=1,2*dim-2
                call PANELarray(j)%compute_tangent_and_normal() 
            end do

            do j=1,dim
                call rot(MEANLINEarray(j)%coords(1),MEANLINEarray(j)%coords(2),airfoil%get_AOA())
            end do

        else 

            ! panel array allocation data
            do k=1,2*dim-2
                call PANELarray(k)%set_id(k)
                call PANELarray(k)%set_coords(coordx(k),coordy(k),coordx(k+1),coordy(k+1))
            end do

            ! compute segments length -> length doesn't vary with rotation and translation 
            do k=1,2*dim-2
                call PANELarray(k)%compute_length()
            end do

            ! compute segments tangent and normal versors
            !     -> the easiest way is to calculate before rotation
            do j=1,2*dim-2
                call PANELarray(j)%compute_tangent_and_normal() 
            end do

        end if

        call airfoil%set_transl() ! setting wing translation

        do j=1,2*dim-2
            call PANELarray(j)%compute_transl(airfoil)    ! panel coords translation function 
        end do

        do j=1,dim
            call MEANLINEarray(j)%compute_transl(airfoil) ! mean-line coords translation function 
        end do

        ! clear screen & print airfoil data
        print *, achar(27)//"[2J"
        call airfoil%print_data()

        ! compute segments middle points 
        !  -> middle-points depend on actual wing-points coordinates
        do j=1,2*dim-2
            call PANELarray(j)%compute_midpoint()
        end do

        ! storing data
        call ask_and_save(airfoil,PANELarray,MEANLINEarray)

        ! printing data
        call GNUplot_print(airfoil,PANELarray,MEANLINEarray)

        ! new airfoil generator option
        call ask_to_continue(x)

        counter = counter + 1 
        
        deallocate(coordx) 
        deallocate(coordy)
        deallocate(PANELarray) 
        deallocate(MEANLINEarray)
    end do

end program airfoilgenerator