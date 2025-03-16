MODULE error_handling
    IMPLICIT NONE
    CHARACTER(LEN=256) :: error_message
    INTEGER :: error_code = 0
    LOGICAL :: has_error = .FALSE.
  
  CONTAINS
  
    SUBROUTINE set_error(code, message)
      INTEGER, INTENT(IN) :: code
      CHARACTER(LEN=*), INTENT(IN) :: message
  
      error_code = code
      error_message = message
      has_error = .TRUE.
    END SUBROUTINE set_error
  
    SUBROUTINE get_error(code, message)
      INTEGER, INTENT(OUT) :: code
      CHARACTER(LEN=*), INTENT(OUT) :: message
  
       if(has_error) then
         code = error_code
         message = error_message
       else
         code = 0
         message = 'No error'
       end if
    END SUBROUTINE get_error
  
  END MODULE error_handling