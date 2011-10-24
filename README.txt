introduce new field "kindle-file" in bibtex
add metadata (title + author) to pdf when added to bibtex
creates collections.json from a bibtex file (using journal/booktitle and keywords)

Usage:
required is pdftk - please install that separately!
and set the bibtex-kindle-pdftk variable to the binary!

then set the bibtex-kindle-prefix variable to point to some path in
your file system which you want to sync with your kindle (the pdfs
are put there)

put (require 'bibtex-kindle) in your .emacs, after it is in your load-path

In any bibtex buffer, just press C-c f in a bibtex-entry, and enter
the path to the pdf of that entry. This pdf is then copied to
bibtex-kindle-prefix/year/title.pdf and metadata from the bibtex-entry
is added (author, title).

Once finished with enough pdfs, press C-c e to generate a
collections.json (also in bibtex-kindle-prefix directory). This will
contain journal-issue and bootitle-year collections for @article and
@inproceedings bibtex entries.

If you specified any keywords (extremely helpful add-on:
http://www.emacswiki.org/emacs/bibtex-utils.el), these are also used
for creating a collection each.

Finally, to sync with your kindle, just copy over bibtex-kindle-prefix
to <kindle>/documents/bib and collections.json to <kindle>/sys (no
merge of collections yet - so if you use collections on your own,
better merge manually)

