
full: ss3.ctl ss3.dat
	ss3

om: ss3.ctl ss3.dat
	ss3 -nohess

.PHONY: clean

clean:
	find . -type f -not \( -name 'ss3.ctl' -or -name 'ss3.dat' -or -name 'forecast.ss' -or -name 'starter.ss' \) -delete
