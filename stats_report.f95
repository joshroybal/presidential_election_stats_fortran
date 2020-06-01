program stats_report
   use stats_module
   implicit none
   ! interface declaration
   interface
      function csv_record(state, party, fields, NOSTATS) result(record)
         character (len=*), intent(in) :: state
         character, intent(in) :: party
         real, intent(in), dimension(NOSTATS) :: fields
         integer, intent(in) :: NOSTATS
         character (len=256) :: record
      end function csv_record
      function html_record(state, party, fields, NOSTATS) result(record)
         character (len=*), intent(in) :: state
         character, intent(in) :: party
         real, intent(in), dimension(NOSTATS) :: fields
         integer, intent(in) :: NOSTATS
         character (len=256) :: record
      end function html_record
   end interface
   ! core data
   integer, parameter :: NP = 3, NR = 2000, NS = 52, NE = 41, NOSTATS = 11
   integer :: eof, n, i, j, jj, k, q
   integer, dimension(NS) :: counts = (/(0,i=1,NS)/)
   real, dimension(NE) :: x, y
   real, dimension(NOSTATS,NP,NS) :: otab
   real, dimension(NP,NR) :: itab
   real, dimension(NE,NP,NS) :: swings
   real, dimension(NOSTATS,NP,NS) :: swing_stats
   character (len=4) :: year
   character (len=14) :: key, tag = ''
   character (len=50) :: arg0, arg1, outfile
   character (len=256) :: record
   character (len=14), dimension(NS) :: labels
   character, dimension(NP) :: parties = (/'D','R','I'/)

   ! processing
   if (command_argument_count() < 1) then
      call get_command_argument(0, arg0)
      print *, 'Usage: ', trim(arg0), ' csv|flat|html'
      stop
   end if
   call get_command_argument(1, arg1)
   if (arg1 /= 'csv' .and. arg1 /= 'flat' .and. arg1 /= 'html') then
      call get_command_argument(0, arg0)
      print *, 'Usage: ', trim(arg0), ' csv|flat|html'
      stop
   end if
   k = 0
   n = 1
   open (7,file='results.dat',status='old',action='read')
   do
      read (7,*,iostat=eof) key, year, itab(1:3,n)
      if (eof /= 0) exit
      if (key /= tag) then
         k = k + 1
         labels(k) = key
         counts(k) = 1
         tag = key
      else
         counts(k) = counts(k) + 1
      end if
      n = n + 1
   end do
   close (7)
   n = n - 1

   ! compute output tables

   ! compute results table data
   q = 1
   do i = 1, k
      do j = 1, NP
         x(1:counts(i)) = 100.*itab(j,q:q+counts(i)-1)
         otab(1,j,i) = MINVAL (x(1:counts(i)))
         otab(2,j,i) = MAXVAL (x(1:counts(i)))
         otab(3,j,i) = mean(x, counts(i))
         otab(4,j,i) = sam_std(x, counts(i))
         otab(5,j,i) = mean_deviation(x, counts(i))
         otab(6,j,i) = median(x, counts(i))
         otab(7,j,i) = median_deviation(x, counts(i))
         otab(8,j,i) = skewness(x, counts(i))
         do jj = 1, NP
            y(1:counts(i)) = 100.*itab(jj,q:q+counts(i)-1)
            otab(8+jj,j,i) = p(x, y, counts(i))
         end do
      end do
      q = q + counts(i)
   end do


   ! compute swings table data
   jj = 1
   do i = 1, k
      do j = 1, NP
         swings(1:counts(i)-1,j,i) = &
         100.*itab(j,jj+1:jj+counts(i)-2) - 100.*itab(j,jj:jj+counts(i)-2)
      end do
      jj = jj + counts(i)
   end do

   do i = 1, k
      n = counts(i)-1
      do j = 1, NP
         swing_stats(1,j,i) = MINVAL (swings(1:n,j,i))
         swing_stats(2,j,i) = MAXVAL (swings(1:n,j,i))
         swing_stats(3,j,i) = mean(swings(1,j,i), n)
         swing_stats(4,j,i) = sam_std(swings(1,j,i), n)
         swing_stats(5,j,i) = mean_deviation(swings(1,j,i), n)
         swing_stats(6,j,i) = median(swings(1,j,i), n)
         swing_stats(7,j,i) = median_deviation(swings(1,j,i), n)
         swing_stats(8,j,i) = skewness(swings(1,j,i), n)
         do jj = 1, NP
            swing_stats(8+jj,j,i) = p(swings(1:n,j,i), swings(1:n,jj,i), n)
         end do
      end do
   end do

   ! output
   if (arg1 == 'flat') then
      outfile = 'report.txt'
   else if (arg1 == 'csv') then
      outfile = 'report.csv'
   else if (arg1 == 'html') then
      outfile = 'report.html'
   end if

   open (8,file=outfile,status='unknown')

   ! results stats table
   if (arg1 == 'flat') then
      write (8,1000) 'STATE','P','MIN','MAX','AVG','STD','AAD','MDN','MAD',&
      'SKW','P(1,j)','P(2,j)','P(3,j)'
   else if (arg1 == 'csv') then
      write (8,'(A)') 'STATE,P,MIN,MAX,AVG,STD,AAD,MDN,MAD,SKW,"P(1,J)","P(2,J)",&
      "P(3,J)"'
   else if (arg1 == 'html') then
      write (8,'(A)') '<!DOCTYPE html>'
      write (8,'(A)') '<html>'
      write (8,'(A)') '<head>'
      write (8,'(A)') '<link id="styleinfo" media="all">'
      write (8,'(A)') '<script type="text/javascript" src="style.js" defer>&
         </script>'
      write (8,'(A)') '</head>'
      write (8,'(A)') '<body>'
      write (8,'(A)') '<h1>results table</h1>'
      write (8,'(A)') '<table id="stats_table">'
      write (8,'(A)') '<tr><th>STATE</th><th>P</th><th>MIN</th><th>MAX</th>&
         <th>AVG</th><th>STD</th><th>AAD</th><th>MDN</th><th>MAD</th>&
         <th>SKW</th><th>P(1,J)</th><th>P(2,J)</th><th>P(3,J)</th></tr>'
   end if
   do i = 1, NS
      do j = 1, NP
         if (labels(i) == 'U. S. Total') then
            jj = i
            cycle
         end if
         if (arg1 == 'flat') then
            write (8,2000) labels(i), parties(j), otab(1:NOSTATS,j,i)
         else if (arg1 == 'csv') then
            record=csv_record(labels(i),parties(j),otab(1:NOSTATS,j,i),NOSTATS)
            write (8,'(A)') trim(record)
         else if (arg1 == 'html') then
            record=html_record(labels(i),parties(j),otab(1:NOSTATS,j,i),NOSTATS)
            write (8,'(A)') trim(record)
         end if
      end do
   end do
   do j = 1, NP
      if (arg1 == 'flat') then
         write (8,2000) labels(jj), parties(j), otab(1:NOSTATS,j,jj)
      else if (arg1 == 'csv') then
         record=csv_record(labels(jj),parties(j),otab(1:NOSTATS,j,jj),NOSTATS)
         write (8,'(A)') trim(record)
      else if (arg1 == 'html') then
         record=html_record(labels(jj),parties(j),otab(1:NOSTATS,j,jj),NOSTATS)
         write (8,'(A)') trim(record)
      end if
   end do
   ! swings stats table
   if (arg1 == 'flat') then
      write (8,1000) 'STATE','P','MIN','MAX','AVG','STD','AAD','MDN','MAD',&
      'SKW','P(1,j)','P(2,j)','P(3,j)'
   else if (arg1 == 'csv') then
      write (8,'(A)') 'STATE,P,MIN,MAX,AVG,STD,AAD,MDN,MAD,SKW,"P(1,J)","P(2,J)",&
      "P(3,J)"'
   else if (arg1 == 'html') then
      write (8,'(A)') '</table>'
      write (8,'(A)') '<h1>swings table</h1>'
      write (8,'(A)') '<table id="swings_table">'
      write (8,'(A)') '<tr><th>STATE</th><th>P</th><th>MIN</th><th>MAX</th>&
         <th>AVG</th><th>STD</th><th>AAD</th><th>MDN</th><th>MAD</th>&
         <th>SKW</th><th>P(1,J)</th><th>P(2,J)</th><th>P(3,J)</th></tr>'
   end if
   do i = 1, NS
      do j = 1, NP
         if (labels(i) == 'U. S. Total') then
            jj = i
            cycle
         end if
         if (arg1 == 'flat') then
            write (8,2000) labels(i), parties(j), swing_stats(1:NOSTATS,j,i)
         else if (arg1 == 'csv') then
            record=csv_record(labels(i),parties(j),swing_stats(1:NOSTATS,j,i),NOSTATS)
            write (8,'(A)') trim(record)
         else if (arg1 == 'html') then
            record=html_record(labels(i),parties(j),swing_stats(1:NOSTATS,j,i),NOSTATS)
            write (8,'(A)') trim(record)
         end if
      end do
   end do
   do j = 1, NP
      if (arg1 == 'flat') then
         write (8,2000) labels(jj), parties(j), swing_stats(1:NOSTATS,j,jj)
      else if (arg1 == 'csv') then
         record=csv_record(labels(jj),parties(j),swing_stats(1:NOSTATS,j,jj),NOSTATS)
         write (8,'(A)') trim(record)
      else if (arg1 == 'html') then
         record=html_record(labels(jj),parties(j),swing_stats(1:NOSTATS,j,jj),NOSTATS)
         write (8,'(A)') trim(record)
      end if
   end do
   if (arg1 == 'html') then
      write (8,'(A)') '</table>'
      write (8,'(A)') '</body>'
      write (8,'(A)') '</html>'
   end if

   close (8)
   print *, 'output written to file ', trim(outfile)
   stop
1000  format (/,X,A5,10X,A,X,7A8,4A7)
2000  format (X,A14,X,A,X,7F8.2,4F7.3)
end program stats_report

! construct string location as csv record
function csv_record(state, party, fields, NOSTATS) result(record)
   implicit none
   ! dummy arguments
   character (len=*), intent(in) :: state
   character, intent(in) :: party
   real, intent(in), dimension(NOSTATS) :: fields
   integer, intent(in) :: NOSTATS
   ! function result location
   character (len=256) :: record
   ! local data
   integer :: i
   character (len=25) :: str
   ! processing
   record=trim(state)//','//trim(party)
   do i = 1, NOSTATS
      write (str,*) fields(i)
      record=trim(record)//','//trim(adjustl(str))
   end do
end function csv_record

! construct string location as html data row
function html_record(state, party, fields, NOSTATS) result(record)
   implicit none
   ! dummy arguments
   character (len=*), intent(in) :: state
   character, intent(in) :: party
   real, intent(in), dimension(NOSTATS) :: fields
   integer, intent(in) :: NOSTATS
   ! function result location
   character (len=256) :: record
   ! local data
   integer :: i
   character (len=25) :: str
   ! processing
   record='<tr><td>'//trim(state)//'</td><td>'//trim(party)//'</td>'
   do i = 1, 7
      write (str,'(F7.2)') fields(i)
      record=trim(record)//'<td>'//trim(adjustl(str))//'</td>'
   end do
   do i = 8, NOSTATS
      write (str,'(F6.3)') fields(i)
      record=trim(record)//'<td>'//trim(adjustl(str))//'</td>'
   end do
   record=trim(record)//'</tr>'
end function html_record
