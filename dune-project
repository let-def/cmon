(lang dune 2.9)
(name cmon)
(generate_opam_files true)

(source (github let-def/cmon))
(license MIT)
(authors "Frédéric Bour")
(maintainers "frederic.bour@lakaban.net")

(package
  (name cmon)
  (synopsis "A library for printing OCaml values with sharing")
  (depends (pprint (>= 20171003))
           (grenier (>= 0.14))))
