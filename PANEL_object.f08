module PANEL_object

    type panel
        !
        ! panel object
        ! id                                = panel identification number
        ! midpoint = [midpointx, midpointy] = panel middle-point position vector 
        ! tangent  = [tangentx, tangenty]   = panel tangent vector coordinates
        ! normal   = [normalx, normaly]     = panel normal vector coordinates
        ! length                            = panel length
        ! coords1  = [coodrs1_x, coords1_y] = panel starting point
        ! coords2  = [coodrs2_x, coords2_y] = panel ending point
        !
        integer(kind=4)           :: id       = 0
        real(kind=8),dimension(2) :: midpoint = (/0.0, 0.0/)
        real(kind=8),dimension(2) :: tangent  = (/0.0, 0.0/)
        real(kind=8),dimension(2) :: normal   = (/0.0, 0.0/)
        real(kind=8)              :: length   = 0.0
        real(kind=8),dimension(2) :: coords1  = (/0.0, 0.0/)
        real(kind=8),dimension(2) :: coords2  = (/0.0, 0.0/)

        contains

        procedure, pass(this) :: get_id
        procedure, pass(this) :: get_midpointx
        procedure, pass(this) :: get_midpointy
        procedure, pass(this) :: get_tangentx
        procedure, pass(this) :: get_tangenty
        procedure, pass(this) :: get_normalx
        procedure, pass(this) :: get_normaly
        procedure, pass(this) :: get_length
        procedure, pass(this) :: get_coords1
        procedure, pass(this) :: get_coords2
        procedure, pass(this) :: set_coords
        procedure, pass(this) :: set_id
        procedure, pass(this) :: SCALINGfunc
        procedure, pass(this) :: compute_length
        procedure, pass(this) :: compute_tangent_and_normal
        procedure, pass(this) :: compute_transl
        procedure, pass(this) :: compute_midpoint
        procedure, pass(this) :: saving

    end type panel

    contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!! GET functions !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    integer(kind=4) function get_id(this)
        implicit none
        class(panel), intent(in) :: this
        get_id = this%id
    end function get_id

    real(kind=8) function get_midpointx(this)
        implicit none
        class(panel), intent(in) :: this

        get_midpointx = this%midpoint(1)
    end function get_midpointx

    real(kind=8) function get_midpointy(this)
        implicit none
        class(panel), intent(in) :: this

        get_midpointy = this%midpoint(2)
    end function get_midpointy

    real(kind=8) function get_tangentx(this)
        implicit none 
        class(panel), intent(in) :: this

        get_tangentx = this%tangent(1)
    end function get_tangentx

    real(kind=8) function get_tangenty(this)
        implicit none
        class(panel), intent(in) :: this

        get_tangenty = this%tangent(2)
    end function get_tangenty

    real(kind=8) function get_normalx(this)
        implicit none
        class(panel), intent(in) :: this

        get_normalx = this%normal(1)
    end function get_normalx

    real(kind=8) function get_normaly(this)
        implicit none 
        class(panel), intent(in) :: this
         
        get_normaly = this%normal(2)
    end function get_normaly

    real(kind=8) function get_length(this)
        implicit none
        class(panel), intent(in) :: this

        get_length = this%length
    end function get_length

    function get_coords1(this)
        implicit none
        real(kind=8),dimension(2) :: get_coords1
        class(panel), intent(in) :: this

        get_coords1 = this%coords1
    end function get_coords1

    function get_coords2(this)
        implicit none
        real(kind=8),dimension(2) :: get_coords2
        class(panel), intent(in) :: this

        get_coords2 = this%coords2
    end function get_coords2
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!! GET functions !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine set_coords(this,coordx1,coordy1,coordx2,coordy2)
        implicit none
        class(panel), intent(inout) :: this
        real(kind=8), intent(in)    :: coordx1, coordx2, coordy1, coordy2

        this%coords1(1) = coordx1
        this%coords1(2) = coordy1
        this%coords2(1) = coordx2
        this%coords2(2) = coordy2
    end subroutine set_coords

    subroutine set_id(this,ID)
        implicit none
        class(panel),intent(inout) :: this
        integer(kind=4),intent(in) :: ID  
        this%id = ID
    end subroutine set_id

    subroutine SCALINGfunc(this,scale)
        implicit none
        class(panel),intent(inout) :: this
        real(kind=4),intent(in)    :: scale

        ! scaling 
        this%coords1(:) = this%coords1(:) * scale
        this%coords2(:) = this%coords2(:) * scale
    end subroutine SCALINGfunc

    subroutine compute_length(this)
        implicit none
        class(panel), intent(inout) :: this
        real(kind=8)                :: coordx1, coordx2, coordy1, coordy2
        
        coordx1 = this%coords1(1)
        coordy1 = this%coords1(2)
        coordx2 = this%coords2(1)
        coordy2 = this%coords2(2)
        
        this%length = sqrt((coordx1 - coordx2)**2 + (coordy1**2 - coordy2)**2)        
    end subroutine compute_length

    subroutine compute_tangent_and_normal(this)
        use math_module
        implicit none
        class(panel), intent(inout) :: this
        real(kind=8) :: coordx1, coordx2, coordy1, coordy2, dx, dy, alpha
        
        coordx1 = this%coords1(1)
        coordy1 = this%coords1(2)
        coordx2 = this%coords2(1)
        coordy2 = this%coords2(2)

        if (this%length /= 0.0) then 
            dx = coordx2 - coordx1
            dy = coordy2 - coordy1
            if (dx<0) then
                alpha = atan(dy/dx) + pi
            else
                alpha = atan(dy/dx)
            end if 
            this%tangent(1) =   cos(alpha)
            this%tangent(2) =   sin(alpha)
            this%normal(1)  = - sin(alpha)
            this%normal(2)  =   cos(alpha)
        end if
    end subroutine compute_tangent_and_normal

    subroutine compute_midpoint(this)
        implicit none
        class(panel),intent(inout) :: this

        this%midpoint = (this%coords1 + this%coords2)/2
    end subroutine compute_midpoint

    subroutine compute_transl(this,airfoil)
        use AIRFOIL_object
        implicit none
        class(panel),intent(inout)     :: this
        class(NACA_airfoil),intent(in) :: airfoil

        this%coords1  = this%coords1  + airfoil%transl
        this%coords2  = this%coords2  + airfoil%transl
    end subroutine compute_transl

    subroutine saving(this,writing_file)
        implicit none 
        class(panel),intent(in) :: this
        integer,intent(in)      :: writing_file

        write(writing_file,*) 'PANEL OBJECT'
        write(writing_file,*) '    id:                   ', this%get_id()
        write(writing_file,*) '    length:               ', this%get_length()
        write(writing_file,*) '    midpoint [x,y]:       ', this%get_midpointx(), this%get_midpointy() 
        write(writing_file,*) '    starting point [x,y]: ', this%get_coords1()
        write(writing_file,*) '    ending point [x,y]:   ', this%get_coords2()
        write(writing_file,*) '    tangent vector:       ', this%get_tangentx(), this%get_tangenty()
        write(writing_file,*) '    normal vector:        ', this%get_normalx(), this%get_normaly()
        write(writing_file,*) new_line('A')  

    end subroutine saving

end module PANEL_object 