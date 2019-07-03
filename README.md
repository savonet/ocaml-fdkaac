ocaml-fdkaac

This package contains an OCaml interface for 
the fdk=aac library.

Please read the COPYING file before using this software.

Prerequisites:
==============

- ocaml >= 4.00.1 (haven't tried earlier versions)

- fdk-aac >= 0.1.1

- findlib >= 0.8.1 (haven't tried earlier versions)

Compilation:
============

	$ make all

This should build both the native and the byte-code version of the
extension library.

Installation:
=============

	$ make install

This should install the library file (using ocamlfind) in the
appropriate place.

Author:
=======

This author of this software may be contacted by electronic mail
at the following address: savonet-users@lists.sourceforge.net.
