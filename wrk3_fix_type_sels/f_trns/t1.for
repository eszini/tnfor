      INCLUDE 'LAMCOM.MON'
      CHARACTER*20 MONTH_NAME
C    +(12)/'January','February','March','April',
C    +         'May','June','July','August','September','October',
C    +         'November','December'/
      CHARACTER*9 DAY_NAME(3)/' weekdays',' weekends',' peakdays'/
      CHARACTER*11 CLASS_NAME(7)/'Commerical','Residential',
     +          'Industrial','Other 1','Other 2','Other 3','System'/
      INTEGER*2 SEASON,DAY,HOUR,LRECL,
     +          YEAR,CLASS,END_POINT,THREE_DAYS,TOTAL_DAYS
