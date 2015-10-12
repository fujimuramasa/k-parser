# karyotype-parser


this is a brief introduction for using k-parser.
1. create a txt file and write each line with format like "[RegID]@@[Karyotype string]". (eg. "1001@@46,XY")
2. change file name to "import.txt" and put it in the parser folder.
3. run clojure repl and type the following commands.
	>(use 'karyotype-parser.core)
	>(ns karyotype-parser.core)
	>(full-pipeline)
	>(save-karontology)
4. there will be a owl file called ontology.owl generated in the parser folder.

*two datasets mentioned in the dissertation are stored in parser folder called MD.txt (Mitelman database) and LRCG.txt (LRCG database) respectively.
**main codes are stored in .../src/karyotype_parser folder due to leiningen project template. 

## License

Copyright © 2015 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
