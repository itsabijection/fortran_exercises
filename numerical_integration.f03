program numerical_integration
    implicit none
    real :: a, b, result
    integer :: n
    a = 0.0
    b = 1.0
    n = 100
    result = integrate(a, b, n, integrand)
    print *, result
    result = integrate(a, b, n, integrand2)
    print *, result

contains

    ! Numerical integration function using the trapezoidal rule
    function integrate(a, b, n, f) result(integral)
        implicit none
        real :: a, b, integral
        integer :: n, i
        real :: h, x
        real, external :: f

        h = (b - a) / n
        integral = 0
        do i = 1, n
            x = a + i * h
            integral = integral + f(x)
        end do
        integral = integral * h
    end function integrate

    function integrand(x)
        implicit none
        real :: integrand
        real, intent(in) :: x
        integrand = x**2
    end function integrand
    
    function integrand2(x)
        implicit none
        real :: integrand2
        real, intent(in) :: x
        integrand2 = x**3
    end function integrand2

end program numerical_integration
