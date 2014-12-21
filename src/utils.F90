 SUBROUTINE QUIT(TEXT)
#include "priunit.h"

 CHARACTER TEXT*(*)

 WRITE (LUPRI,'(/A)') STRING
 
 WRITE (LUPRI,'(1X,A)') TEXT

 WRITE (0,'(/A/1X,A)') STRING, TEXT

 END SUBROUTINE QUIT

 SUBROUTINE FLSHFO (IUNIT)
 CALL FLUSH(IUNIT)
 END SUBROUTINE FLSHFO

 SUBROUTINE HEADER(HEAD,IN)
      CHARACTER HEAD*(*)
#include "priunit.h"
      integer, intent(in) :: in
      LHEAD = LNBLNK(HEAD)
      IF (IN .GE. 0) THEN
         INDENT = IN + 1
      ELSE
         INDENT = MAX(1,(80 - LHEAD)/2 + 1)
      END IF
      WRITE (LUPRI, '(//,150A)') (' ',I=1,INDENT), HEAD(1:LHEAD)
      WRITE (LUPRI, '(   150A)') (' ',I=1,INDENT), ('-',I=1,LHEAD)
      WRITE (LUPRI, '()')
 END SUBROUTINE HEADER
