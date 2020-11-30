CFLAGS=-O3
read-a-file-by-chars: read-a-file-by-chars.c

read-a-file-by-line-getline: read-a-file-by-line-getline.c

read-a-file-by-line-mmap: read-a-file-by-line-mmap.c

getline: read-a-file-by-line-mmap read-a-file-by-line-getline 
	sudo likwid-powermeter ./read-a-file-by-line-getline | tail -n 13
	sudo likwid-powermeter ./read-a-file-by-line-mmap | tail -n 13

likwid-tests: read-a-file-by-chars read-a-file-by-line-getline read-a-file-by-line-mmap read-a-file-by-chars.pl  read-a-file-by-chars.py
	cat input.txt > /dev/null
	sudo likwid-powermeter ./read-a-file-by-chars | tail -n 13
	sudo likwid-powermeter python3 ./read-a-file-by-chars.py | tail -n 13
	sudo likwid-powermeter perl ./read-a-file-by-chars.pl | tail -n 13
	sudo likwid-powermeter ./read-a-file-by-line-getline | tail -n 13
	sudo likwid-powermeter ./read-a-file-by-line-mmap | tail -n 13

perf-tests: read-a-file-by-chars read-a-file-by-line-getline read-a-file-by-line-mmap read-a-file-by-chars.pl  read-a-file-by-chars.py
	cat input.txt > /dev/null
	sudo perf stat -a -e "power/energy-cores/" ./read-a-file-by-chars             2>&1 > /dev/null               
	sudo perf stat -a -e "power/energy-cores/" python3 ./read-a-file-by-chars.py  2>&1 > /dev/null              
	sudo perf stat -a -e "power/energy-cores/" perl ./read-a-file-by-chars.pl     2>&1 > /dev/null           
	sudo perf stat -a -e "power/energy-cores/" ./read-a-file-by-line-getline      2>&1 > /dev/null         
	sudo perf stat -a -e "power/energy-cores/" ./read-a-file-by-line-mmap         2>&1 > /dev/null      

time-tests: read-a-file-by-chars read-a-file-by-line-getline read-a-file-by-line-mmap read-a-file-by-chars.pl  read-a-file-by-chars.py
	cat input.txt > /dev/null
	time ./read-a-file-by-chars             2>&1 > /dev/null               
	time python3 ./read-a-file-by-chars.py  2>&1 > /dev/null              
	time perl ./read-a-file-by-chars.pl     2>&1 > /dev/null           
	time ./read-a-file-by-line-getline      2>&1 > /dev/null         
	time ./read-a-file-by-line-mmap         2>&1 > /dev/null      

all: time-tests perf-tests likwid-tests
