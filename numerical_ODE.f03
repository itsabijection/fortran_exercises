program eulers_method
        implicit none
        real :: x, y, targ, delta
        real, allocatable :: r(:)
        integer :: i
        x = 0
        y = 1
        targ = 10
        r = solve(x, y, targ, 0.01, integrand)
        do i = 1, size(r), 10 
            print *, r(i)
        end do
contains
        function solve(x, y, targ, abs_delta, df) result(vals)
        implicit none
        real, intent(in) :: x, y, targ, abs_delta
        real, external :: df
        real :: cur_x, delta
        real, allocatable :: vals(:)
        integer :: c, points
        points = int(abs(targ - x)/abs_delta) + 1
        allocate(vals(1:points))
        if (x .GT. targ) then
            delta = -1 * abs_delta
        else
            delta = abs_delta
        end if
        vals(1) = y
        cur_x = x
        do c = 2, points
        cur_x = cur_x + delta
        vals(c) = vals(c - 1) + delta * df(cur_x - delta, vals(c - 1))
        end do
        end function solve
        
        function integrand(x, y)
        implicit none
        real :: integrand
        real, intent(in) :: x, y
        integrand = y
        end function integrand
end program eulers_method
