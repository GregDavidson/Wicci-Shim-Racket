-- * Header  -*-Mode: sql;-*-
-- Code to Pretend to Serve a Wicci Page
-- Actually returns a page showing HTTP Request

-- ** Copyright

-- Copyright (c) 2019, J. Greg Davidson.
-- You may use this file under the terms of the
-- GNU AFFERO GENERAL PUBLIC LICENSE 3.0
-- as specified in the file LICENSE.md included with this distribution.
-- All other use requires my permission in writing.

-- * Round-Trip Testing

CREATE OR REPLACE
FUNCTION wicci_test_hdrs_html(hdrs text)
RETURNS text AS $$
  SELECT array_to_string(ARRAY(SELECT (
		SELECT format('<dt>%s</dt> <dd>%s</dd>', hv[1], hv[2]) FROM
			regexp_split_to_array(line, E':[[:space:]]*') hv
	) FROM regexp_split_to_table(hdrs, E'\r\n') line), E'\n');
$$ LANGUAGE sql SET search_path FROM CURRENT;

-- select wicci_test_hdrs_html(E'this: that\r\nstuff: nonsense\r\nyin:yang');
--        wicci_test_hdrs_html       
-- ----------------------------------
--  <dt>this</dt> <dd>that</dd>     +
--  <dt>stuff</dt> <dd>nonsense</dd>+
--  <dt>yin</dt> <dd>yang</dd>

CREATE OR REPLACE
FUNCTION wicci_test_body_html(hdrs text, body text)
RETURNS text AS $$
	SELECT format('
<!DOCTYPE HTML>
<html>
	<head>
		<title>
			Your Request echoed back from the Database Wicci Test Frame
		</title>
	</head>
	<body>
    <dl>
%s
		<dt> _body </dt>
		<dd>
%s
		</dd>
    </dl>
	</body>
</html>
',
  wicci_test_hdrs_html(hdrs),
  body);
$$ LANGUAGE sql SET search_path FROM CURRENT;


-- select wicci_test_body_html(E'this: that\r\nstuff: nonsense', 'hello world');
--        wicci_test_body_html       
-- ----------------------------------
--                                  +
--  <!DOCTYPE HTML>                 +
--  <html>                          +
--  <head>                          +
--  <title>                         +
--  Wicci HTML Echo Test            +
--  </title>                        +
--  </head>                         +
--  <body>                          +
--      <dl>                        +
--  <dt>this</dt> <dd>that</dd>     +
--  <dt>stuff</dt> <dd>nonsense</dd>+
--  <dt> _body </dt>                +
--  <dd>                            +
--  hello world                     +
--  </dd>                           +
--      </dl>                       +
--  </body>                         +
--  </html>                         +
 
-- (1 row)

-- We have to use arrays of rows rather than just return
-- tables to avoid PostgreSQL reordering the rows!!

CREATE TYPE http_row_return AS (
  "name" text,
   text_value text,
   binary_value bytea
);

-- Probably should take an argument for the Content-Type so
-- that when _body_bin is requested, we can say we're
-- returning LATIN1 since that's what we're returning!
CREATE OR REPLACE
FUNCTION wicci_test_headers_out(length INTEGER, content_type text)
RETURNS http_row_return[] AS $$
	SELECT ARRAY[
		ROW('_status', 'HTTP/1.1 200 OK', ''::bytea)::http_row_return,
		ROW('Server', 'Wicci', ''),
		ROW('Content-Length', length::text, ''),
		ROW('Content-Type', content_type, '') ]::http_row_return[];
$$ LANGUAGE sql SET search_path FROM CURRENT;

-- Other than for testing, why shoud the Shim be requesting
-- whether the body is being returned as text or binary??
-- Shouldn't the Wicci just return what it wants and the
-- Shim simply have to deal with it??  And if so, why not
-- return everything binary encoded and get rid of the text
-- column??

-- Why not just have the Shim compute Content-Length??

CREATE OR REPLACE
FUNCTION wicci_serve_text(body text)
RETURNS http_row_return[] AS $$
	SELECT wicci_test_headers_out(
		octet_length(body), 'text/html; charset=utf-8'
	) || ROW('_body', body, '')::http_row_return
$$ LANGUAGE sql SET search_path FROM CURRENT;

CREATE OR REPLACE
FUNCTION wicci_serve_bytes(body bytea, content_type text)
RETURNS http_row_return[] AS $$
	SELECT wicci_test_headers_out(
		octet_length(body), content_type
	) || ROW('_body_bin', body, '')::http_row_return
$$ LANGUAGE sql SET search_path FROM CURRENT;

CREATE OR REPLACE
FUNCTION public.wicci_serve_echo(bytea, bytea, _bin_ text = '_body_bin')
RETURNS TABLE("name" text, text_value text, binary_value bytea)  AS $$
	SELECT unnest( CASE _bin_
	WHEN '_body' THEN wicci_serve_text(body_html)
	WHEN '_body_bin' THEN wicci_serve_bytes(
			convert_to(body_html, 'latin1'), 'text/html; charset=iso8859-1'
	) END	) FROM wicci_test_body_html(
			convert_from($1, 'latin1'), convert_From($2, 'latin1')
	) body_html;
$$ LANGUAGE sql SET search_path FROM CURRENT;

-- wicci_serve_echo(E'this: that\r\nstuff: nonsense', 'hello world');
--                 wicci_serve_echo                 
-- -------------------------------------------------
--  (_status,"HTTP/1.1 200 OK","\\x")
--  (Server,Wicci,"\\x")
--  (Content-Length,220,"\\x")
--  (Content-Type,"text/html; charset=utf-8","\\x")
--  (_body,"                                       +
--  <!DOCTYPE HTML>                                +
--  <html>                                         +
--  <head>                                         +
--  <title>                                        +
--  Wicci HTML Echo Test                           +
--  </title>                                       +
--  </head>                                        +
--  <body>                                         +
--      <dl>                                       +
--  <dt>this</dt> <dd>that</dd>                    +
--  <dt>stuff</dt> <dd>nonsense</dd>               +
--  <dt> _body </dt>                               +
--  <dd>                                           +
--  hello world                                    +
--  </dd>                                          +
--      </dl>                                      +
--  </body>                                        +
--  </html>                                        +
--  ","\\x")
-- (5 rows)

-- * wicci_serve

CREATE OR REPLACE
FUNCTION public.wicci_serve(bytea, bytea, _bin_ text = '_body_bin')
RETURNS TABLE("name" text, text_value text, binary_value bytea)  AS $$
	SELECT wicci_serve_echo($1, $2, $3)
$$ LANGUAGE sql SET search_path FROM CURRENT;

-- * wicci_ready

CREATE OR REPLACE
FUNCTION public.wicci_ready()
RETURNS text AS $$
  SELECT 'Thunderbirds are GO!'
$$ LANGUAGE sql SET search_path FROM CURRENT;
