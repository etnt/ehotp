
compile: 
	erl -pz ./ebin -make
	
clean:
	rm -rf ./ebin/*.beam
