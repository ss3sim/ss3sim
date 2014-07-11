====================
The APPENDIX package

    The appendix package provides various ways of formatting the titles
of appendices. Also (sub)appendices environments are provided that can be
used, for example, for per chapter/section appendices.

Changes v1.2b (2009/09/02)
- New maintainer (Will Robertson)

Changes v1.2a (2004/04/16)
- Updated contact details 
- Changed license from LPPL v1.0 to LPPL v1.3

Changes in version 1.2 (2002/08/06)
- The ifthen package is not required.
- Supports hyperref bookmarks

Changes in version 1.1a (2001/03/15)
- Fixed unfortunate interaction between \addappheadtotoc and \appendixpage

Changes in version 1.1 (2000/02/29)
- Addition of a subappendices environment for per chapter/section appendices
- Appendices environment now restores chapter/section numbering at the end

----------------------------------------------------------------- 
  Author: Peter Wilson, Herries Press
  Maintainer: Will Robertson (will dot robertson at latex-project dot org)
  Copyright 1998--2004 Peter R. Wilson
 
  This work may be distributed and/or modified under the
  conditions of the LaTeX Project Public License, either
  version 1.3c of this license or (at your option) any 
  later version: <http://www.latex-project.org/lppl.txt>
 
  This work has the LPPL maintenance status "maintained".
  The Current Maintainer of this work is Will Robertson.
 
  This work consists of the files:
README (this file)
appendix.dtx
appendix.ins
appendix.pdf (the user manual)
  and the derived files:
appendix.sty

----------------------------------------------------------------- 

    To install the package:
- run: latex appendix.ins (which will generate appendix.sty)
- Move appendix.sty to a location where LaTeX will find it
  (typically in a local texmf tree at tex/latex/appendix) and refresh
  the file database. See the FAQ on CTAN at help/uk-tex-faq or
  http://www.tug.ac.uk/faq for more information on this.
- To obtain the user manual:
  o run: latex appendix.dtx
  o run: makeindex -s gind.ist appendix
  o run: latex appendix.dtx
  o Print appendix.dvi for a hardcopy of the package manual 

