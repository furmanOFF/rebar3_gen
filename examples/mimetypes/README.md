# MIME type mapping module generator

Generating hardcoded `<<"ext">>` -> `{<<"type">>, <<"subtype">>, []}` mappings for MIME types

This example fetches MIME type list from http://svn.apache.org/repos/asf/httpd/httpd/trunk/docs/conf/mime.types.
- URL is declared at the start of [mimetypes.erl.gen](/examples/mimetypes/src/mimetypes.erl.gen#L1)
- Response is processed in [mimetypes.erl.script](/examples/mimetypes/src/mimetypes.erl.script) (binded to **Data** variable)
- Script exports a map with **mimetypes** variable
- It is passed further to mustache template in [mimetypes.erl.gen](/examples/mimetypes/src/mimetypes.erl.gen)
