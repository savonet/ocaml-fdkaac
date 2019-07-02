SOURCES = wav2aac.ml
RESULT = wav2aac
ANNOTATE = true
LIBS = unix fdkaac
INCDIRS = ../src

all: dnc

test: all
	if [ ! -e test.wav ] ; then ffmpeg -f lavfi -i "sine=frequency=440:duration=5" -ac 2 test.wav ; fi
	./wav2aac test.wav test.aac

include OCamlMakefile
