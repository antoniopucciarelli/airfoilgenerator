module math_module
    implicit none

    real(kind=8), parameter :: pi = 4 * atan(1.0)  

end module math_module

module discretization_module

    contains

    recursive subroutine SCALINGfunction(x,y,scale)
        implicit none
        real(kind=8),intent(in)    :: scale
        real(kind=8),intent(inout) :: x
        real(kind=8),intent(inout) :: y

        ! scaling 
        x = x * scale
        y = y * scale
    end subroutine SCALINGfunction

    recursive subroutine rot(coordx,coordy,alpha)
        use math_module
        implicit none
        real(kind=8),intent(inout) :: coordx, coordy
        real(kind=8)               :: angle, radius
        real(kind=8),intent(in)    :: alpha

        if(coordx<0) then 
            angle = pi + atan(coordy/coordx)
        else if(coordx>0) then
            angle = atan(coordy/coordx)
        else
            angle = pi/2
        end if
    
        angle  = angle - alpha * pi /180.0
        radius = abs(sqrt(coordx**2 + coordy**2))
        coordx = radius * cos(angle)
        coordy = radius * sin(angle)
    end subroutine rot

    subroutine ask_to_continue(i)
        implicit none
        character(len=1)              :: resp
        integer(kind=4),intent(inout) :: i 
        
        print*, 'do you want to create a new geometry? [Y\n]'
        read*, resp

        if(resp=='Y' .or. resp=='y')then
            i = 1
        else 
            i = 0
        end if
    end subroutine ask_to_continue

    subroutine ask_and_save(airfoil,PANELarray,MEANLINEarray)
        use AIRFOIL_object
        use PANEL_object
        use MEANline_object

        implicit none
        class(NACA_airfoil),intent(in)          :: airfoil    
        class(panel),intent(in),dimension(:)    :: PANELarray
        class(MEANline),intent(in),dimension(:) :: MEANLINEarray
        character(len=1)                        :: resp
        character(len=30)                       :: filename
        integer(kind=4)                         :: dim, k, writing_file

        print*, 'do you want to store data? [Y\n]'
        read*, resp

        if(resp=='Y' .or. resp=='y')then
            call airfoil%saving()

            dim = airfoil%get_npoints()
            read(airfoil%airfoilname,*) filename
            filename(9:30) = '_airfoil_MEAN.dat'

            writing_file = 1

            open(unit=writing_file, file=filename, status='replace')
            do k=1,dim
                call MEANLINEarray(k)%saving(writing_file)
            end do

            close(writing_file)

            filename(9:30) = '_airfoil_PANEL.dat'
            writing_file = 1
            open(unit=writing_file, file=filename, status='replace')
            do k=1,2*dim-2 
                call PANELarray(k)%saving(writing_file)
            end do

            close(writing_file)
        end if

    end subroutine ask_and_save

    subroutine GNUplot_print(airfoil,PANELarray,MEANLINEarray)
        use AIRFOIL_object
        use MEANline_object
        use PANEL_object
        implicit none   
        class(NACA_airfoil),intent(in)          :: airfoil
        class(MEANline),intent(in),dimension(:) :: MEANLINEarray
        class(panel),intent(in),dimension(:)    :: PANELarray
        character(len=1)                        :: resp

        ! printing option via gnuplot
        print*, 'do you want to print ',airfoil%get_airfoilname(),' ? [Y\n]'
        read*, resp
        if(resp=='Y' .or. resp=='y')then
            call GNUplot_saving(PANELarray,MEANLINEarray,airfoil%get_npoints())
            call system('gnuplot -p AIRFOILgnuplot.plt')
        end if

    end subroutine GNUplot_print

    subroutine GNUplot_saving(PANELarray,MEANLINEarray,dim)
        use MEANline_object
        use PANEL_object
        implicit none
        class(MEANline),intent(in),dimension(:) :: MEANLINEarray
        class(panel),intent(in),dimension(:)    :: PANELarray
        integer(kind=4),intent(in)              :: dim
        integer(kind=4)                         :: k
    
        open(unit=1, file='GNUplot_coord_data.dat', status='replace')
        open(unit=2, file='GNUplot_mean_data.dat', status='replace')
        
        do k=1,dim
            write(1,*) MEANLINEarray(k)%get_coords()
        end do

        do k=1,2*dim-2
            write(2,*) PANELarray(k)%get_coords1()
        end do

        write(2,*) PANELarray(1)%get_coords1()

        close(1)
        close(2)
    end subroutine GNUplot_saving

end module discretization_module