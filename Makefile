fc = gfortran
cflags = -O2
lflags = -static -s

stats_report: stats_report.o stats_module.o
	$(fc) $^ $(lflags) -o $@

stats_report.o: stats_report.f95 stats_module.o
	$(fc) $(cflags) -c $<

stats_module.o: stats_module.f95
	$(fc) $(cflags) -c $<

clean:
	$(RM) stats_report *.o *.mod
