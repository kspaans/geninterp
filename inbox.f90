! Interfaces for the Functions
!      INTERFACE
!        logical FUNCTION inbox( x, y, pts )
!          real, INTENT(IN) :: x, y
!          real, dimension(1:2,1:4), INTENT(IN) :: pts
!        END FUNCTION
!      END INTERFACE
!
!      INTERFACE
!        logical FUNCTION inbox2( x, y, pts )
!          real, INTENT(IN) :: x, y
!          real, dimension(1:2,1:4), INTENT(IN) :: pts
!        END FUNCTION
!      END INTERFACE

!===============================================================================

!*******************************************************************************
      logical FUNCTION inbox( x, y, pts )
      IMPLICIT NONE
! A subroutine to solve my old "is the point inside the box? conundrum.
! Find the arrangement of the points, the calculate there slope of each side.
! Then check that the point is on the correct side of each line (line == side).
!---------NOTE: this program assumes that points on the line are *IN* the box.
!*******************************************************************************

      real, INTENT(IN) :: x, y
      real, dimension(1:4,1:2), INTENT(IN) :: pts
      ! FUNCTIONS---------------------------------------
      real :: line_y
      real :: slope
      ! LOCAL VARIABLES---------------------------------
      real :: slope1, slope2, slope3, slope4, val1, val2, val3, val4
      real, dimension(1:2) :: p1, p2, p3, p4
      real, dimension(4) :: maxarrx, maxarry, dist
      integer :: i, negcount, poscount
      integer, dimension(1) :: closest
      logical :: onetwo, onethree, onefour

      p1(1) = pts(1,1)
      p1(2) = pts(1,2)
      p2(1) = pts(2,1)
      p2(2) = pts(2,2)
      p3(1) = pts(3,1)
      p3(2) = pts(3,2)
      p4(1) = pts(4,1)
      p4(2) = pts(4,2)

      !!!! Sides can be built sequentially this way, 1 is adjacent to 2, which
      !!!! is adjacent to 3, which is to 4, which is to 1 again. So slope 1 and
      !!!! 3 and 2 and 4 will always be the opposite sides. This makes point-in
      !!!! -box decision easier later.
      IF ( slope( p1, p2 ) /= slope( p3, p4 ) ) THEN
      !   then segment 12 should be a diagonal of the box,
      !   and thus segment 34 will be the other diagonal
        slope1 = slope( p1, p3 )
        slope2 = slope( p3, p2 )
        slope3 = slope( p2, p4 )
        slope4 = slope( p4, p1 )
        val1 = y - line_y( slope1, p1(1), p1(2), x )
        val2 = y - line_y( slope2, p2(1), p2(2), x )
        val3 = y - line_y( slope3, p2(1), p2(2), x )
        val4 = y - line_y( slope4, p1(1), p1(2), x )
        ! Now that we have the above/below-line values, they should be of
        ! opposite signs for opposite sides
        IF ( ((val1 > 0).AND.(val3 < 0) .OR. (val1 < 0).AND.(val3 > 0)) .AND. &
             ((val2 > 0).AND.(val4 < 0) .OR. (val2 < 0).AND.(val4 > 0)) ) THEN
          inbox = .TRUE.
        ELSE
          inbox = .FALSE.
        ENDIF
      ELSEIF ( slope( p1, p3 ) /= slope( p2, p4 ) ) THEN
      !   then segment 13 should be a diagonal of the box,
      !   ...
        slope1 = slope( p1, p2 )
        slope2 = slope( p2, p3 )
        slope3 = slope( p3, p4 )
        slope4 = slope( p4, p1 )
        val1 = y - line_y( slope1, p1(1), p1(2), x )
        val2 = y - line_y( slope2, p3(1), p3(2), x )
        val3 = y - line_y( slope3, p3(1), p3(2), x )
        val4 = y - line_y( slope4, p1(1), p1(2), x )
        IF ( ((val1 >= 0).AND.(val3 <= 0) .OR. (val1 <= 0).AND.(val3 >= 0)) .AND. &
             ((val2 >= 0).AND.(val4 <= 0) .OR. (val2 <= 0).AND.(val4 >= 0)) ) THEN
          inbox = .TRUE.
        ELSE
          inbox = .FALSE.
        ENDIF
      ELSEIF ( slope( p1, p4 ) /= slope( p2, p3 ) ) THEN
      !   then segment 14 should be a diagonal of the box,
      !   ...
        slope1 = slope( p1, p2 )
        slope2 = slope( p2, p4 )
        slope3 = slope( p4, p3 )
        slope4 = slope( p3, p1 )
        val1 = y - line_y( slope1, p1(1), p1(2), x )
        val2 = y - line_y( slope2, p4(1), p4(2), x )
        val3 = y - line_y( slope3, p4(1), p4(2), x )
        val4 = y - line_y( slope4, p1(1), p1(2), x )
        IF ( ((val1 >= 0).AND.(val3 <= 0) .OR. (val1 <= 0).AND.(val3 >= 0)) .AND. &
             ((val2 >= 0).AND.(val4 <= 0) .OR. (val2 <= 0).AND.(val4 >= 0)) ) THEN
          inbox = .TRUE.
        ELSE
          inbox = .FALSE.
        ENDIF
      ENDIF

      END FUNCTION inbox

!===============================================================================
!*******************************************************************************

      logical FUNCTION inbox2( x, y, pts )
      IMPLICIT NONE
! Same as above, put a different implementation idea I found on the internet.
! Extend a ray along the positive X-axis, and see if it intersects with any of
! vertices of the box.
! See http://tog.acm.org/editors/erich/ptinpoly/
!*******************************************************************************
      

      real, INTENT(IN) :: x, y
      real, dimension(1:2,1:4), INTENT(IN) :: pts

      inbox2 = .TRUE.

      END FUNCTION inbox2

!===============================================================================
!*******************************************************************************
      real FUNCTION line_y( m, x1, y1, px )
      IMPLICIT NONE
! A quick function that will return the corresponding y-value of the x-value px
! where the point px is on the line made by the point (x1,y1) and the slope m.
!*******************************************************************************

      real, INTENT(IN) :: m, x1, y1, px

      line_y = m * (px - x1) + y1

      END FUNCTION line_y

!===============================================================================
!*******************************************************************************
      real FUNCTION slope( p1, p2 )
      IMPLICIT NONE
! Calculate the slope between two points in 2-dimensional space
!*******************************************************************************
      real, dimension(1:2), INTENT(IN) :: p1, p2

      slope = (p2(2)-p1(2)) / (p2(1)-p1(1))

      END FUNCTION slope
