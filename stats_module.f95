! Modern Fortran statistical computations module
module stats_module
contains
   ! function returns real mean of all array elements
   function mean(x, n) result(m)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function result location
      real :: m
      ! processing
      m = sum(x) / n
   end function mean

   ! function returns array of deviations about central tendency m
   function compute_dev(x, m, n) result(dev)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in) :: m
      real, dimension(n), intent(in) :: x
      ! function result location
      real, dimension(n) :: dev
      ! processing
      dev = x - m
   end function compute_dev

   ! function returns array of absolute deviations about central tendency m
   function compute_adev(x, m, n) result(dev)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in) :: m
      real, dimension(n), intent(in) :: x
      ! function result location
      real, dimension(n) :: dev
      ! processing
      dev = abs(x - m)
   end function compute_adev

   ! function returns real population variance of x
   function pop_var(x, n) result(var)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: var
      ! processing
      var = sum(compute_dev(x, mean(x, n), n)**2) / n
   end function pop_var

   ! function returns real sample variance of x
   function sam_var(x, n) result(var)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: var
      ! processing
      var = sum(compute_dev(x, mean(x, n), n)**2) / (n - 1)
   end function sam_var

   ! function returns real population standard deviation of x
   function pop_std(x, n) result(std)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: std
      ! processing
      std = sqrt(pop_var(x, n))
   end function pop_std

   ! function returns real sample standard deviation of x
   function sam_std(x, n) result(std)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: std
      ! processing
      std = sqrt(sam_var(x, n))
   end function sam_std

   ! functions returns index of kth element of x
   ! C. A. R. Hoare's algorithm
   ! implementation works on index array only - preserves data set array
   function quick_select(k, n, x) result(v)
      implicit none
      ! dummy arguments
      integer, intent(in) :: k, n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: v
      ! local variables
      integer :: i, j, left, right, tmp
      integer, dimension(n) :: idx
      real :: pivot
      ! processing
      do i=1,n
         idx(i)=i
      end do
      left=1
      right=n
      do while (left < right)
         pivot=x(idx(k))
         i=left
         j=right
         do
            do while (x(idx(i)) < pivot)
               i=i+1
            end do
            do while (pivot < x(idx(j)))
               j=j-1
            end do
            if (i <= j) then
               tmp=idx(i)
               idx(i)=idx(j)
               idx(j)=tmp
               i=i+1
               j=j-1
            end if
            if (i > j) exit
         end do
         if (j < k) left=i
         if (k < i) right=j
      end do
      v=x(idx(k))
   end function quick_select

   ! function returns median of x
   function median(x, n) result(mdn)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: mdn
      ! processing
      if (mod(n, 2) /= 0) then
         mdn=quick_select(n/2+1, n, x)
      else
         mdn=(quick_select(n/2, n, x) + quick_select(n/2+1, n, x))/2
      end if
   end function median

   !funcition returns median absolute deviation of x
   function median_deviation(x, n) result(mad)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: mad
      ! processing
      mad = median( compute_adev(x, median(x, n), n), n )
   end function median_deviation

   ! function computes the mean absolute deviation of x
   function mean_deviation(x, n) result(aad)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: aad
      ! processing
      aad = mean( compute_adev(x, mean(x, n), n), n )
   end function mean_deviation

   ! function computes the skewness of x (expected value 1/N)
   function skewness(x, n) result(skw)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: skw
      ! processing
      skw = sum(compute_dev(x, mean(x, n), n)**3) / ((n - 1) * sam_std(x, n)**3)
   end function skewness

   ! function computes the sample covariance of data sets x and y
   function covar(x, y, n) result(cov)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, dimension(n), intent(in) :: x, y
      ! function return location
      real :: cov
      ! processing
      cov = sum(compute_dev(x,mean(x,n),n)*compute_dev(y,mean(y,n),n))/(n-1)
   end function covar

   ! correlation coefficient of data sets x and y
   function p(x, y, n) result(corr)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, dimension(n), intent(in) :: x, y
      ! function return location
      real :: corr
      ! processing
      corr = covar(x, y, n) / (sam_std(x, n) * sam_std(y, n))
   end function p
end module stats_module
