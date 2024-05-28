# Author: Jan Kalenda (xkalen07)
# Description: Makefile for FLP2 project
# Year: 2024

flp23-log: *.pl
	swipl -q --goal=run_sim --stand_alone=true -o $@ -c $^

run: flp23-log
	./flp23-log < input > output
	./flp23-log < input2 > output2
	./flp23-log < input3 > output3
	./flp23-log < input4 > output4

clean:
	rm -f flp23-log output* flp-log-xkalen07.zip

pack: *.pl Makefile README.md input input2 input3 input4
	zip flp-log-xkalen07.zip $^

