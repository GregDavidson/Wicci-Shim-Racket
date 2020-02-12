-- * Media Repository Project

-- ** Examples

-- *** Names should be XML
/*
Senor Jose Garcia de la Vega
<pn> <h> Senor</h> <fn>Jose</fn> <sn>Garcia</sn> <s2>de la Vega</s2></pn>
The Honorable Judge Kittering, Jr.
Greg "Touch Pu'uhonua" Davidson
*/

-- ** Media Entities

UniqueID
MediaType
Title
Artist/Producer/Group
IMDB Link
Web Link
Local Link
UPC
Comments/Notes
  Viewers
Location -- (Box A; sleeve 5)
  On Loan To
Bitset

-- ** Attributes
Release Date
Entry Date
Last Access Date

-- using timestamp exclusively for date and time recording; due to DATETIME not being indexable...
-- check that timestamo is the right thig for timezone recording

-- ** Indicies

-- table of things we're keeping track of
DROP TABLE IF EXISTS TrackedItems;
CREATE TABLE TrackedItems (
       id serial PRIMARY KEY,
       entry timestamp,
       access timestamp,
       release timestamp
  --        itemtype : CD, DVD, VHS, Cassette, 8Trak, etc.
  --        imdb, Webpresence, Local : httplink
  --        upc : digits? barcode? WTF?
);

-- table of remarkable play events
--       "when_" to be clearly distinct from WHEN
DROP TABLE IF EXISTS ItemPlays;
CREATE TABLE ItemPlays (
       item serial REFERENCES TrackedItems,
       when_ timestamp,
       viewer person,
       PRIMARY KEY (item,play,viewer)
);

-- retain check-outs and check-ins for tracked items
DROP TABLE IF EXISTS ItemLoans;
CREATE TABLE ItemLoans (
       returned timestamp, -- use range?
       PRIMARY KEY (item,play,viewer) -- is this actually automatically inherited?
) INHERITS (ItemPlays);

-- notes; we should always take notes; or, at least put notes
DROP TABLE IF EXISTS Notes;
CREATE TABLE Notes (
       id serial PRIMARY KEY,
       author person NOT NULL,
       when_ timestamp NOT NULL,
       note text NOT NULL,
       codes bitset NOT NULL
);

-- good to know what the note is a note for
DROP TABLE IF EXISTS Items_Notes;
CREATE TABLE Items_Notes (
       item serial NOT NULL REFERENCES TrackedItems(id),
       note serial NOT NULL REFERENCES Notes(id)
);
