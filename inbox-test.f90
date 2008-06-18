!*******************************************************************************
      PROGRAM test_inbox
      IMPLICIT NONE
! A quick program to test the point-in-box functions
!*******************************************************************************

      ! Functions ------------
      logical :: inbox, inbox2
      !-----------------------

      real :: i = 5.01
      real :: j = 5.51
      real, dimension(4,2) :: ptz
      logical :: yesno

      ptz(1,1) = 0.0
      ptz(1,2) = 1.0
      ptz(2,1) = 10.0
      ptz(2,2) = 2.0
      ptz(3,1) = 0.0
      ptz(3,2) = 3.0
      ptz(4,1) = -10.0
      ptz(4,2) = 2.0
      yesno = inbox( i, j, ptz )
      write(*,*) "First test:", yesno .EQV. .FALSE.

      i = 5.01
      j = 5.51
      ptz(1,1) = 5.0
      ptz(1,2) = 5.0
      ptz(2,1) = 5.0
      ptz(2,2) = 6.0
      ptz(3,1) = 4.5
      ptz(3,2) = 5.5
      ptz(4,1) = 5.5 
      ptz(4,2) = 5.5
      yesno = inbox( i, j, ptz )
      write(*,*) "Second test:", yesno .EQV. .TRUE.

      i = 0
      j = 0.23411
      ptz(1,1) = -5.0
      ptz(1,2) = -50.0
      ptz(2,1) = -5.0
      ptz(2,2) = 50.0
      ptz(3,1) = 34.251
      ptz(3,2) = -5.0
      ptz(4,1) = 34.251
      ptz(4,2) = 5.0
      yesno = inbox( i, j, ptz )
      write(*,*) "Third test:", yesno .EQV. .TRUE.

      i = 20.8
      j = 6.4
      ptz(1,1) = 16.0
      ptz(1,2) = 8.7
      ptz(2,1) = 23.9
      ptz(2,2) = 6.3
      ptz(3,1) = 19.0
      ptz(3,2) = 8.7
      ptz(4,1) = 20.9 
      ptz(4,2) = 6.3
      yesno = inbox( i, j, ptz )
      write(*,*) "Fourth test:", yesno .EQV. .TRUE.

      i = 20.8
      j = 6.4
      ptz(1,1) = 5.0
      ptz(1,2) = 5.0
      ptz(2,1) = 5.0
      ptz(2,2) = 6.0
      ptz(3,1) = 4.5
      ptz(3,2) = 5.5
      ptz(4,1) = 5.5 
      ptz(4,2) = 5.5
      yesno = inbox( i, j, ptz )
      write(*,*) "Fifth test:", yesno .EQV. .FALSE. ! false

      i = 20.8
      j = 6.4
      ptz(1,1) = 100.0
      ptz(1,2) = 0.0
      ptz(2,1) = 0.0
      ptz(2,2) = 100.0
      ptz(3,1) = -100.0
      ptz(3,2) = 0.0
      ptz(4,1) = 0.0 
      ptz(4,2) = -100.0
      yesno = inbox( i, j, ptz )
      write(*,*) "Sixth test:", yesno .EQV. .TRUE. ! true


      i = 20.8
      j = 6.4
      ptz(1,1) = 5.0
      ptz(1,2) = 5.0
      ptz(2,1) = 5.0
      ptz(2,2) = 6.0
      ptz(3,1) = 4.5
      ptz(3,2) = 5.5
      ptz(4,1) = 5.5 
      ptz(4,2) = 5.5
      yesno = inbox( i, j, ptz )
      write(*,*) "Seventh test:", yesno .EQV. .FALSE. ! false

      i = 9.0
      j = 7.0
      ptz(1,1) = 1.0
      ptz(1,2) = 5.0
      ptz(2,1) = 5.0
      ptz(2,2) = 2.0
      ptz(3,1) = 6.0
      ptz(3,2) = 10.0
      ptz(4,1) = 10.0
      ptz(4,2) = 7.0
      yesno = inbox( i, j, ptz )
      write(*,*) "Eigth test:", yesno .EQV. .TRUE. ! true

      i = 5.0
      j = 9.0
      ptz(1,1) = 1.0
      ptz(1,2) = 5.0
      ptz(2,1) = 5.0
      ptz(2,2) = 2.0
      ptz(3,1) = 6.0
      ptz(3,2) = 10.0
      ptz(4,1) = 10.0
      ptz(4,2) = 7.0
      yesno = inbox( i, j, ptz )
      write(*,*) "Ninth test:", yesno .EQV. .TRUE. ! true
      
      i = 3.0
      j = 3.0
      ptz(1,1) = 1.0
      ptz(1,2) = 5.0
      ptz(2,1) = 5.0
      ptz(2,2) = 2.0
      ptz(3,1) = 6.0
      ptz(3,2) = 10.0
      ptz(4,1) = 10.0
      ptz(4,2) = 7.0
      yesno = inbox( i, j, ptz )
      write(*,*) "Tenth test:", yesno .EQV. .FALSE. ! false

!      yesno = inbox2( i, j, ptz )
!      write(*,*) "The value returned by in-box2 is: ", yesno

      END PROGRAM test_inbox
