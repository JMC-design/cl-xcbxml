# cl-xcbxml
Simple parsing of Xorg xml protocol descriptions for xcb
Was designed for extensions, so the struct to hold parsed information is called extension.

(LOAD-EXTENSION xml-file) returns a parsed xml file stuffed into an extension struct.

(CLX-LOAD xml-file) returns a struct filled with clx ready stuff.

(COMPARE x-accessor &optional (item-index)) Prints out a specific item, or all if no index given, showing unparsed, parsed, and clx if present.

FIND-REQUEST, FIND-EVENT, FIND-ERROR all take a string and return either parsed or clx depending on type of extension.

(PRINT-CLX-EXTENSION extension) prints out a clx ready file.

Caveats:
* Nothing done with docs
* switches not implemented

TODO:
* fix readme to show intermediate format e.g. (:request name (send-list)(receive-list))
* fix clx generators for sequence-get and -put, and whatever ;fixme's are in the code.

parse.lisp contains all the intermediate format stuff besides utils in utils.lisp.
generate.lisp and print.lisp are for clx format.
