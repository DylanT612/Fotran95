! I certify, that this computer program submitted by me is all of my own work.
! Signed: Dylan Theis 5/17/2024
 
! Author: Dylan Theis
! Date: Summer 2024
! Class: CSC330
! Project: Calculating Fortran 95 Mersenne Primes
! Description: Calculates the first eight Fortran 95 Mersenne primes and the value of N that generates it.

! Create program called Lesson3 
program Lesson3
  ! Implicit none makes sure all variables are declared
  implicit none

  ! declare integer variables count, N, and MersennePrime
  integer :: count, N, MersennePrime
  ! MersennePrimes will have eight elements maxxing at 4 bytes long
  integer*4, dimension(8) :: MersennePrimes
  ! NValues will have 8 elements
  integer, dimension(8) :: NValues

  ! Initialize count to 0 and N at 2
  count = 0
  N = 2

  ! Loop for 8 Mersenne primes
  do while (count < 8)
    ! If N is prime then execute Mersenne prime formula
    if (is_prime(N)) then
      MersennePrime = 2**N - 1
      ! Checks if the newly calculated number is prime
      if (is_prime(MersennePrime)) then
        ! Increment count, store the MersennePrime and its N value
        count = count + 1
        MersennePrimes(count) = MersennePrime
        NValues(count) = N
      end if
    end if
    ! Increments N
    N = N + 1
  end do

  ! Print blank line
  print *
  ! Writes following string
  write(*, '(A)') "First 8 Mersenne Primes and their corresponding N values:"
  ! Loop to write MersennePrime nad its N value
  do count = 1, 8
    ! Writes Mersenne Prime and N value
    write(*, '(A, i0, A, i0, A)') "The Mersenne prime of ", MersennePrimes(count), " has an N value of ", NValues(count), "."
  end do

! Section with internal function is_prime
contains

  ! is_prime function
  logical function is_prime(num)
    ! declares parameter num and i
    integer, intent(IN) :: num
    integer :: i

    ! is_prime defaults to true
    is_prime = .TRUE.
    ! If number is less than or equal to 1 it is not prime
    if (num <= 1) then
      is_prime = .FALSE.
    ! If the number is 2 it is prime
    else if (num == 2) then
      is_prime = .TRUE.
    ! if the number is even and greater than 2 it is not prime  
    else if (mod(num, 2) == 0) then
      is_prime = .FALSE.
    else
      ! Loop from 3 to sqrt of number checking odd only
      do i = 3, int(sqrt(real(num))), 2
        ! If i is a factor of the number then it is not prime
        if (mod(num, i) == 0) then
          is_prime = .FALSE.
          exit
        end if
      end do
    end if
  end function is_prime

end program Lesson3
