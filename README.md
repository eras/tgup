# TinyG Uploader #

tgup is a tool for uploading CNC programs to
TinyG (https://github.com/synthetos/TinyG) from Linux.

It also incorporates a tool for aligning a camera and the tool, so
that the camera can be used for finding exact coordinates of some
place you see; see a mark in the object, and you can the write Gcode
to drill a hole at that exact point. It also wants to perform
automatical Gcode realignment from given reference points, but that
part isn't quite complete yet..

# Compiling #

You need Objective Caml to build tgup. On Debian Sid the following
might be sufficient:

	% sudo apt-get install ocaml-nox opam ocaml-findlib
	  # if you already used opam, this is not necessary. You need at least version  1.1.
	% opam init
	  # this step is maybe not necessary:
	% opam update
	% opam install batteries cmdliner yojson pcre ANSITerminal v4l2 gg
	% ./build.sh

# Using #

Usage:
	% ./main.native --help
	% ./main.native -d /dev/ttyUSB0 -b 115200 upload work.gcode

Ctrl-c sends '!' (the TinyG feed hold symbol) to the machine so it
should be instantenous. If you use a terminal program you need to
manually release it with the character % (TODO: or was it ~?).

'upload' also knows the switch -# nnn, where it starts reading the
file from the given line. This can be useful if the job gets
interrupted. The upload tool also keeps track of moves so that is is
able to tell which was the last line with a positive z value. This is
useful for determining a nice resume point, assuming you have places
your work object below z=0.

Good luck!

