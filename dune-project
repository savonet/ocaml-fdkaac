(lang dune 2.9)
(version 0.3.2)
(name fdkaac)
(source (github savonet/ocaml-fdkaac))
(license GPL-2.0)
(authors "The Savonet Team <savonet-users@lists.sourceforge.net>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)
(use_standard_c_and_cxx_flags false)

(package
 (name fdkaac)
 (synopsis "Fraunhofer FDK AAC Codec Library")
 (description "The FDK AAC Codec Library For Android contains an encoder implementation of the Advanced Audio Coding (AAC) audio codec.")
 (depends
  conf-fdkaac
  dune
  dune-configurator)
)
