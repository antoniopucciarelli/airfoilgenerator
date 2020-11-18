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
    use discretization_module

    implicit none

    ! variable declaration
    type(NACA_airfoil)                      :: airfoil       ! airfoil object
    type(panel),allocatable,dimension(:)    :: PANELarray    ! panel object array
    type(MEANline),allocatable,dimension(:) :: MEANLINEarray ! mean line point object array
    real(kind=8),allocatable,dimension(:)   :: coordyUP      ! x-coords array -> describes the UPPER airfoil geometry
    real(kind=8),allocatable,dimension(:)   :: coordxUP      ! y-coords array -> describes the UPPER airfoil geometry
    real(kind=8),allocatable,dimension(:)   :: coordyDW      ! x-coords array -> describes the LOWER airfoil geometry
    real(kind=8),allocatable,dimension(:)   :: coordxDW      ! y-coords array -> describes the LOWER airfoil geometry
    integer(kind=4)                         :: counter = 1   ! # of airfoil studied counter
    integer(kind=4)                         :: x = 1         ! checking variable in while loop
    integer(kind=4)                         :: dim           ! auxiliary variable -> # of discretisation points for the airfoil
    integer(kind=4)                         :: k             ! auxiliary variable
    integer(kind=4)                         :: j             ! auxiliary variable
    integer(kind=4)                         :: i             ! auxiliary variable
    real(kind=8)                            :: airfoil_data1 ! easy-access variable -> 1st number of airfoil%data
    real(kind=8)                            :: airfoil_data2 ! easy-access variable -> 2nd number of arifoil%data

    ! this program allows you to create multiple NACA**** profile each run 
    do while(x==1)

        print*, 'AIRFOIL # ', counter

        call airfoil%set_AIRFOILname() ! setting airfoil name

        call airfoil%set_npoints()     ! setting airfoil # of discretisation points

        dim = airfoil%get_npoints()

        ! data allocation procedure
        allocate(coordxUP(1:dim)) 
        allocate(coordyUP(1:dim))
        allocate(coordxDW(1:dim)) 
        allocate(coordyDW(1:dim))
        allocate(PANELarray(1:2*dim-2)) 
        allocate(MEANLINEarray(1:dim))


        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MEAN LINE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! calculate airfoil mean line point discretisation along x-axis
            do k=1,dim
                call MEANLINEarray(k)%set_id(k)
                call MEANLINEarray(k)%set_coordx(dim,k)
            end do

            ! extracting information from NACA digits 
            airfoil_data1 = real(airfoil%data(1),8)/100.0
            airfoil_data2 = real(airfoil%data(2),8)/10.0
            
            ! calculate airfoil mean line point discretisation along y-axis
            !     y coord
            !     gradient [dy] => theta
            k = 1 ! initialising data for the loop
            do while(MEANLINEarray(k)%coords(1)<airfoil_data2)
                call MEANLINEarray(k)%set_coordy_leading(airfoil_data1,airfoil_data2)
                call MEANLINEarray(k)%set_gradient_leading(airfoil_data1,airfoil_data2)
                k = k + 1
            end do
            do i=k,dim ! loop starts counting from k
                call MEANLINEarray(i)%set_coordy_trailing(airfoil_data1,airfoil_data2)
                call MEANLINEarray(i)%set_gradient_trailing(airfoil_data1,airfoil_data2)
            end do
            
            MEANLINEarray(dim)%coords = (/ 1.0, 0.0 /) ! mean-line end point coords

            ! thickness
            do k=1,dim
                call MEANLINEarray(k)%set_thickness(airfoil%data(3)) 
            end do

            ! coordx|coordy
            do k=1,dim
                call MEANLINEarray(k)%compute_UPcoords(coordxUP(k),coordyUP(k))
            end do
            do k=1,dim
                call MEANLINEarray(k)%compute_DOWNcoords(coordxDW(k),coordyDW(k))
            end do

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MEAN LINE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ! closure coords
        coordxUP(dim)                = 1
        coordyUP(dim)                = 0
        coordxDW(dim)                = 1
        coordyDW(dim)                = 0
        MEANLINEarray(dim)%coords(1) = 1
        MEANLINEarray(dim)%coords(2) = 0

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! PANEL !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! SCALING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                call airfoil%set_scaling() ! setting scale factor for airfoil dimension

                if(airfoil%get_scaling() /= 1.0) then
                ! airfoil coordinates and airfoil mean coordinates scaling procedures
                ! scaling panel points
                    ! UPPER SURFACE
                    do j=1,dim
                        call SCALINGfunction(coordxUP(j),coordyUP(j),airfoil%get_scaling())
                    end do
                    ! LOWER SURFACE
                    do j=1,dim
                        call SCALINGfunction(coordxDW(j),coordyDW(j),airfoil%get_scaling())
                    end do
                    ! scaling MEANline points
                    do j=1,dim
                        call SCALINGfunction(MEANLINEarray(j)%coords(1),& 
                                             MEANLINEarray(j)%coords(2),&
                                             airfoil%get_scaling())
                    end do
                end if
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! SCALING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                
            !!!!!!!!!!!!!!!!!!!!!!!!! PANEL DATA ALLOCATION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                ! panel array allocation data -> LOWER AIRFOIL PART
                do k=1,dim-1
                    call PANELarray(k)%set_id(k)
                    call PANELarray(k)%set_coords(coordxDW(dim-k+1),coordyDW(dim-k+1),&
                                                  coordxDW(dim-k),coordyDW(dim-k))
                    call PANELarray(k)%set_position('DW')
                end do
                ! panel array allocation data -> UPPER AIRFOIL PART
                do k=1,dim-1
                    call PANELarray(dim+k-1)%set_id(dim+k-1)
                    call PANELarray(dim+k-1)%set_coords(coordxUP(k),coordyUP(k),&
                                                  coordxUP(k+1),coordyUP(k+1))
                    call PANELarray(dim+k-1)%set_position('UP')
                end do
            !!!!!!!!!!!!!!!!!!!!!!!!! PANEL DATA ALLOCATION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!! PANEL PROPERTIES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                ! compute segments length -> length doesn't vary with rotation and translation 
                do k=1,2*dim-2
                    call PANELarray(k)%compute_length()
                end do

                ! compute panel angle between x-axis and the tangent vector direction
                do k=1,2*dim-2
                    call PANELarray(k)%set_angle()
                end do

                ! compute segments tangent and normal versors
                do j=1,2*dim-2
                    call PANELarray(j)%compute_tangent_and_normal()  
                end do

                ! check on leading edge panels
                call check_LE_panels(PANELarray,dim)
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!! PANEL PROPERTIES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ROTATION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                call airfoil%set_AOA() ! setting wing rotation -> airfoil AOA

                ! rotation procedure for panel and mean-line objects
                if(airfoil%get_AOA() /= 0) then
                !!! WARNING:   this choice to rotate the panel points and the vector associated to it
                !              after allocated all the points in the panel are made because of it can 
                !              be some tangent and normal vector could present errors in direction 
                !              computing them after node rotation and allocation into PANEL objects
                !              THIS IS CLOSELY RELATED TO AOA AND THE GEOMETRY OF THE AIRFOIL (EXPECIALLY
                !              AT ITS LEADING EDGE) 

                    ! geometry points rotation procedure
                    ! PANELS COORDS
                    do j=1,2*dim-2
                        call rot(PANELarray(j)%coords1(1),PANELarray(j)%coords1(2),airfoil%get_AOA())
                    end do
                    do j=1,2*dim-2
                        call rot(PANELarray(j)%coords2(1),PANELarray(j)%coords2(2),airfoil%get_AOA())
                    end do
                    ! PANELS TANGENT VECTOR
                    do j=1,2*dim-2
                        call rot(PANELarray(j)%tangent(1),PANELarray(j)%tangent(2),airfoil%get_AOA())
                    end do
                    ! PANELS NORMAL VECTOR
                    do j=1,2*dim-2
                        call rot(PANELarray(j)%normal(1),PANELarray(j)%normal(2),airfoil%get_AOA())
                    end do
                    ! MEAN LINE COORDS
                    do j=1,dim
                        call rot(MEANLINEarray(j)%coords(1),MEANLINEarray(j)%coords(2),airfoil%get_AOA())
                    end do

                end if
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ROTATION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! PANEL !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!! TRANSLATION !!!!!!!!!!!!!!!!!!!!!!!!!!!
            call airfoil%set_transl() ! setting wing translation
            
            do j=1,2*dim-2
                call PANELarray(j)%compute_transl(airfoil)    ! panel coords translation function 
            end do

            do j=1,dim
                call MEANLINEarray(j)%compute_transl(airfoil) ! mean-line coords translation function 
            end do
        !!!!!!!!!!!!!!!!!!!!!!!!!!! TRANSLATION !!!!!!!!!!!!!!!!!!!!!!!!!!!
        
        ! clear screen & print airfoil data -> more clearness in the process
        print *, achar(27)//"[2J"
        call airfoil%print_data()
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!! MIDPOINT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! compute segments middle points 
            !  -> middle-points depend on actual wing-points coordinates
            do j=1,2*dim-2
                call PANELarray(j)%compute_midpoint('noprt')
            end do
        !!!!!!!!!!!!!!!!!!!!!!!!!!!! MIDPOINT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
        !!!!!!!!!!!!!!!!!!!!!!! SAVING & GRAPHICS !!!!!!!!!!!!!!!!!!!!!!!!!
            ! storing data
            call ask_and_save(airfoil,PANELarray,MEANLINEarray)
            
            ! printing data
            call GNUplot_print(airfoil,PANELarray,MEANLINEarray)
        !!!!!!!!!!!!!!!!!!!!!!! SAVING & GRAPHICS !!!!!!!!!!!!!!!!!!!!!!!!!

        ! new airfoil generator option
        call ask_to_continue(x)
        
        counter = counter + 1 
        
        ! data deallocation procedure
        deallocate(coordxUP) 
        deallocate(coordyUP)
        deallocate(coordxDW)
        deallocate(coordyDW)
        deallocate(PANELarray) 
        deallocate(MEANLINEarray)

    end do

end program airfoilgenerator
